#' Resolve the context ceiling for a run
#'
#' @description
#' Turns the `num_ctx` argument of [measure()] into an integer or `NULL`.
#' Called once per run, before any document is scored.
#'
#' @section Truncation and cache stability:
#' A prompt that overruns the context window is truncated with no error, and
#' llama.cpp keeps the tail. With document-first prompts the opening of the
#' document is what disappears. The truncation flag on each call is the only
#' downstream signal.
#'
#' The ceiling must stay fixed for the whole run. Ollama treats a change as a
#' different runner configuration, reloads the model, and discards the prefix
#' cache.
#'
#' @section The four cases:
#' \describe{
#'   \item{`"auto"` (default)}{Measure it. The longest document by word count is
#'     assembled into a real prompt with the longest item, sent once with no
#'     ceiling, and `prompt_eval_count` read back. That is the token length
#'     under the model's own tokenizer, including template and cue. The ceiling
#'     is then that measurement plus headroom, rounded up to a power of two.}
#'   \item{an integer}{Used as given. No measurement call.}
#'   \item{`NULL` or `NA`}{No ceiling is sent and Ollama chooses.}
#'   \item{a resumed run}{Whatever the existing checkpoints used, overriding all
#'     of the above.}
#' }
#'
#' @section Document selection:
#' Token count correlates better with words than with characters, because BPE
#' splits within words rather than across them. The correlation is imperfect. A
#' document dense in numbers or proper nouns tokenizes well above its word rank.
#' The headroom absorbs that, and [measure()] flags any call landing within 64
#' tokens of the ceiling.
#'
#' @section A single document:
#' Measuring one document to set a ceiling for that same document is circular. A
#' single string runs with no ceiling.
#'
#' @section Resumed runs:
#' Mixing ceilings within one corpus means some documents were truncated and
#' others were not, and nothing in the results distinguishes them. A resumed run
#' uses the ceiling its checkpoints were written under, and says so. To change
#' it, delete the checkpoint directory and re-extract.
#'
#' @param num_ctx A value that is one of `"auto"`, an integer, `NULL`, or `NA`.
#' @param texts A character vector of the documents in this run.
#' @param instrument A `fathom_instrument`.
#' @param backend A `fathom_backend`.
#' @param ids A character vector of document identifiers aligned with `texts`,
#'   used to name the measured document in the report. Falls back to the
#'   position when `NULL`.
#' @param checkpoint A string giving the checkpoint directory, or `NULL`.
#' @param images A list of character vectors, one per document, each holding
#'   that document's media, or `NULL`. Switches the probe document from the
#'   longest to the one carrying the most bytes, and sends its media into the
#'   measuring call so `prompt_eval_count` covers them.
#' @param quiet A logical value indicating whether to suppress its notes.
#' @param report An environment to write what was decided into, or `NULL`.
#'   [measure()] reads it for its header.
#' @return An integer ceiling, or `NULL` for no ceiling. With `report`, it
#'   also writes `context`, the ceiling as the header prints it, and,
#'   when the ceiling was measured, `measured_doc`, a field naming the document
#'   the measurement read.
#' @keywords internal
#' @noRd
resolve_num_ctx <- function(
  num_ctx,
  texts,
  instrument,
  backend,
  ids = NULL,
  checkpoint = NULL,
  images = NULL,
  quiet = FALSE,
  report = NULL
) {
  # What measure() prints in its header, written here because this is where
  # the ceiling is decided.
  tell <- function(context, measured_doc = NULL) {
    if (!is.null(report)) {
      report$context <- context
      report$measured_doc <- measured_doc
    }
  }

  # ---- a resumed run uses whatever its checkpoints were written under -------
  if (!is.null(checkpoint) && dir.exists(checkpoint)) {
    prior <- ckpt_num_ctx(checkpoint)
    if (!is.null(prior)) {
      asked <- if (identical(num_ctx, "auto")) {
        "auto"
      } else if (is.null(num_ctx) || is.na(num_ctx)) {
        "none"
      } else {
        format(as.integer(num_ctx), big.mark = ",")
      }
      resolved <- if (is.na(prior)) NULL else as.integer(prior)
      # The one ceiling the header explains, since the caller did not choose
      # it this run.
      tell(paste(
        if (is.null(resolved)) "none" else paste(fmt_ctx(resolved), "tokens"),
        "(from the saved run)"
      ))

      # The default asks for no ceiling in particular, so deferring to the
      # checkpoints overrides nothing, and the header's Context Ceiling line
      # already says where the ceiling came from. Only a value the caller passed
      # gets a note.
      if (
        !quiet &&
          !identical(asked, "auto") &&
          !identical(asked, fmt_ctx(resolved))
      ) {
        message(fmt_note(
          "Note",
          sprintf(
            "num_ctx = %s, from existing checkpoints. Ignoring the supplied %s.",
            fmt_ctx(resolved),
            asked
          )
        ))
      }
      return(resolved)
    }
  }

  # ---- explicit NULL or NA, so no ceiling -----------------------------------
  if (is.null(num_ctx) || (length(num_ctx) == 1L && is.na(num_ctx))) {
    tell("none")
    return(NULL)
  }

  # ---- an explicit integer --------------------------------------------------
  if (is.numeric(num_ctx)) {
    n <- as.integer(num_ctx)
    if (n < 512L) {
      stop("`num_ctx` of ", n, " is too small to hold a prompt", call. = FALSE)
    }
    tell(paste(fmt_ctx(n), "tokens"))
    return(n)
  }

  if (!identical(num_ctx, "auto")) {
    stop('`num_ctx` must be "auto", an integer, NULL, or NA', call. = FALSE)
  }

  # ---- "auto" on a single document, nothing to generalize from --------------
  # An image corpus is exempt. With text, one document gives nothing to
  # generalize from because a word count is standing in for a token count. With
  # an image the probe returns a real `prompt_eval_count` covering the picture,
  # the question and the scaffolding together, so refusing here would discard a
  # measurement already taken.
  if (length(texts) < 2L && is.null(images)) {
    tell("none")
    return(NULL)
  }

  # ---- "auto", so measure ---------------------------------------------------
  # Which document to probe with. Words for text. File size for images, which
  # ranks rather than predicts, since within a corpus of comparable pictures the
  # largest file is the one most likely to tile into the most patches. What
  # comes back is measured either way, so the proxy only has to pick.
  i <- if (is.null(images)) {
    which.max(n_words(texts))
  } else {
    # Total bytes for the document, not the largest single file. A window of
    # twelve frames costs twelve encodes, so the widest prompt is the row with
    # the most media rather than the one holding the biggest picture.
    sz <- vapply(
      images,
      function(v) sum(suppressWarnings(file.size(v)), na.rm = TRUE),
      numeric(1)
    )
    if (all(sz == 0)) 1L else which.max(sz)
  }
  item <- names(instrument$items)[which.max(nchar(unlist(instrument$items)))]
  doc <- if (length(texts) >= i) texts[i] else ""
  prompt <- build_prompt(doc, instrument$items[[item]], instrument)

  # `prompt_eval_count` counts the image tokens too, because PostTokenize
  # interleaves the embeddings into the stream before the prompt is evaluated.
  # So one call measures picture, question and scaffolding together and nothing
  # is estimated.
  probe <- tryCatch(
    ollama_generate(
      prompt,
      backend,
      num_ctx = NULL,
      image = if (is.null(images)) NULL else images[[i]]
    ),
    error = function(e) e
  )

  if (inherits(probe, "error")) {
    tell("none")
    warning(
      fmt_warn(
        paste0(
          "could not measure the context ceiling. ",
          conditionMessage(probe)
        ),
        "Proceeding with no ceiling. Supply `num_ctx` explicitly to avoid this."
      ),
      call. = FALSE
    )
    return(NULL)
  }

  measured <- suppressWarnings(as.integer(probe$prompt_eval_count %||% NA))
  if (is.na(measured)) {
    tell("none")
    warning(
      fmt_warn(paste0(
        "the model did not report a prompt token count, so the ceiling could ",
        "not be measured. Proceeding with no ceiling."
      )),
      call. = FALSE
    )
    return(NULL)
  }

  # Headroom, then round up to a power of two. The longest document by words
  # need not be the longest by tokens, and KV cache blocks are usually allocated
  # in powers of two.
  target <- measured * 1.25
  raw <- as.integer(2^ceiling(log2(target)))
  ceiling_val <- max(2048L, raw)

  # If the measurement came back close to a round number, Ollama's own default
  # may have truncated the probe, in which case the real prompt is longer than
  # measured.
  suspect <- measured %% 1024L <= 8L && measured >= 2048L

  size <- if (is.null(images)) {
    paste(format(n_words(texts)[i], big.mark = ","), "words")
  } else {
    sprintf(
      "%s bytes over %d file(s)",
      format(
        sum(suppressWarnings(file.size(images[[i]])), na.rm = TRUE),
        big.mark = ","
      ),
      length(images[[i]])
    )
  }
  # Named by id alone, since the Documents line already gives the total.
  measured_doc <- list(sprintf(
    "'%s', %s, %s tokens",
    if (is.null(ids)) as.character(i) else as.character(ids[i]),
    size,
    format(measured, big.mark = ",")
  ))
  names(measured_doc) <- if (is.null(images)) "Longest doc" else "Largest doc"
  tell(paste(fmt_ctx(ceiling_val), "tokens"), measured_doc)

  if (suspect && !quiet) {
    message(fmt_note(
      "Note",
      paste0(
        "The measurement landed on a round number, which can mean ",
        "Ollama's own default truncated the probe. If truncation warnings ",
        "appear during the run, set `num_ctx` explicitly."
      )
    ))
  }

  ceiling_val
}

#' The ceiling a checkpoint directory was written under
#'
#' Reads the first readable checkpoint and returns its stored ceiling. `NA`
#' means the run used no ceiling, which is different from not knowing.
#'
#' @param dir A string giving the checkpoint directory.
#' @return An integer, `NA` for no ceiling, or `NULL` if nothing readable.
#' @keywords internal
#' @noRd
ckpt_num_ctx <- function(dir) {
  fps <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(fps) == 0L) {
    return(NULL)
  }
  for (fp in fps) {
    p <- tryCatch(readRDS(fp), error = function(e) NULL)
    if (!is.null(p) && "num_ctx" %in% names(p)) {
      return(p$num_ctx %||% NA_integer_)
    }
  }
  NULL
}
