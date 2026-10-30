#' One failed record per label
#'
#' The shape a call takes when it could not be scored, and the definition of
#' the results schema. Every column measure_one() returns on success appears
#' here too, in the same order, since bind_rows() over a corpus mixing failed
#' and successful documents would otherwise give a ragged frame. That
#' correspondence is already load-bearing, which is what lets reserved_cols()
#' read the schema off this function rather than restate it.
#'
#' Hoisted out of measure_one(), where it was a closure over `instrument`. The
#' local `fail()` there is now a thin wrapper, so the call sites are unchanged.
#'
#' @param instrument A `fathom_instrument`.
#' @param msg A string giving the error text. Defaults to `NA`, for a caller
#'   that wants the frame for its column names alone.
#' @return Tibble, one row per label, with `call_ok = FALSE`.
#' @keywords internal
#' @noRd
fail_rows <- function(instrument, msg = NA_character_) {
  tibble::tibble(
    label = names(instrument$labels),
    logprob = NA_real_,
    prob = NA_real_,
    prob_renorm = NA_real_,
    present = NA,
    top1_token = NA_character_,
    top1_prob = NA_real_,
    prob_other = NA_real_,
    entropy = NA_real_,
    margin = NA_real_,
    expected_score = NA_real_,
    winner = NA_character_,
    call_ok = FALSE,
    error = msg,
    prompt_tokens = NA_integer_,
    prefill_ms = NA_real_,
    wall_ms = NA_real_,
    truncated = NA,
    all_absent = NA
  )
}

#' Measure one document against one question
#'
#' Assembles the prompt, generates a single token, reads the label distribution
#' at position 1. Never throws. On failure it returns one row per label with
#' `call_ok = FALSE`, so a dead server during a long run marks rows for retry
#' rather than aborting the loop.
#'
#' The generated string is never parsed. The measurement is the logit
#' distribution, so the model cannot answer off-vocabulary in a way that
#' corrupts the result.
#'
#' @param text A string giving the document text, or `""` for a corpus that has
#'   none.
#' @param question A string giving the item question.
#' @param instrument A `fathom_instrument`.
#' @param backend A `fathom_backend` object.
#' @param num_ctx As in `ollama_generate()`.
#' @param image A character vector of media paths sent with the prompt, or
#'   `NULL`.
#' @return A data frame of one row per label, scored at generated position 1,
#'   with `call_ok` `FALSE` and the reason in `error` when the call failed.
#' @keywords internal
#' @noRd
measure_one <- function(
  text,
  question,
  instrument,
  backend,
  num_ctx = NULL,
  image = NULL
) {
  fail <- function(msg) fail_rows(instrument, msg)

  resp <- tryCatch(
    ollama_generate(
      build_prompt(text, question, instrument),
      backend,
      num_ctx,
      image = image
    ),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(fail(paste("request error.", conditionMessage(resp))))
  }

  lp <- tryCatch(resp$logprobs[[1]]$top_logprobs, error = function(e) NULL)
  if (is.null(lp) || length(lp) == 0L) {
    return(fail(
      "no log probabilities at position 1, needs Ollama >= 0.12.11"
    ))
  }

  scored <- tryCatch(score_labels(lp, instrument), error = function(e) e)
  if (inherits(scored, "error")) {
    return(fail(paste("scoring error.", conditionMessage(scored))))
  }

  pe <- suppressWarnings(as.integer(resp$prompt_eval_count %||% NA))

  scored$call_ok <- TRUE
  scored$error <- NA_character_
  scored$prompt_tokens <- pe
  scored$prefill_ms <- (resp$prompt_eval_duration %||% NA_real_) / 1e6
  scored$wall_ms <- resp$wall_ms %||% NA_real_
  # Within 64 tokens of the ceiling. llama.cpp keeps the tail, so with
  # document-first prompts the opening of the document was dropped. With no
  # ceiling, truncation can be neither ruled out nor confirmed.
  scored$truncated <- if (is.null(num_ctx)) {
    NA
  } else {
    !is.na(pe) && pe >= as.integer(num_ctx) - 64L
  }
  # No label at all in the top-k. Unlike one label missing, which is normal on
  # saturated items, this means the prompt or template is broken.
  scored$all_absent <- !any(scored$present)

  scored
}

#' Restrict a results frame to a set of item names
#'
#' A checkpoint accumulates every item a document has ever been scored on, which
#' is what makes a resume cheap. `items` asks a narrower question than that, and
#' a caller who gets extra items back from the cached documents but not from the
#' fresh ones has a ragged frame that `as_wide(drop_incomplete = TRUE)` will
#' quietly thin.
#'
#' @param res A data frame of result rows, or `NULL`.
#' @param wanted A character vector of item names to keep.
#' @return `res` with only the rows whose `item` is in `wanted`, or `res`
#'   unchanged when it is `NULL` or empty.
#' @keywords internal
#' @noRd
restrict_items <- function(res, wanted) {
  if (is.null(res) || nrow(res) == 0L) {
    return(res)
  }
  res[res$item %in% wanted, , drop = FALSE]
}

# The three columns measure_doc() prepends to each scored block, naming the
# call that produced the row. Which question, under which wording, on which
# model. Held here rather than written twice inside the reorder below, so the
# set has one definition and reserved_cols() can read it rather than restate it.
ITEM_COLS <- c("item", "prompt_hash", "model")


#' The header a run prints before its first call
#'
#' @param backend A `fathom_backend` object.
#' @param pre An environment holding what `resolve_num_ctx()` and
#'   `check_backend()` reported.
#' @param n_docs An integer giving the documents in the run.
#' @param items A character vector of the item names the run asks.
#' @param instrument A `fathom_instrument`.
#' @param checkpoint A string giving the checkpoint directory, or `NULL`.
#' @return A single string, a title over a block of aligned fields, ending in a
#'   newline so a blank line follows it.
#' @keywords internal
#' @noRd
run_header <- function(backend, pre, n_docs, items, instrument, checkpoint) {
  labs <- names(instrument$labels)
  first <- if (!is.null(pre$top1)) {
    sprintf(
      "'%s', a label, from %d log probabilities",
      pre$top1,
      pre$n_logprobs
    )
  }
  paste0(
    "Constrained-label measurement\n\n",
    fmt_fields(
      c(
        list(Model = backend$model, `Context Ceiling` = pre$context),
        pre$measured_doc,
        list(
          `First token` = first,
          Documents = format(n_docs, big.mark = ","),
          Items = sprintf("%d (%s)", length(items), fmt_names(items)),
          Labels = sprintf("%d (%s)", length(labs), fmt_names(labs)),
          Checkpoints = checkpoint %||% "off"
        )
      ),
      name_width = HEADER_WIDTH
    ),
    "\n"
  )
}

#' Whether a run draws its progress as a bar
#'
#' A bar is redrawn in place, which only an interactive session shows as one
#' line. A knitted document or a log keeps every frame as text, so those get a
#' line every ten documents instead. Verbose output prints a line per item,
#' which a bar would draw over, so it takes the lines too. A function rather
#' than an inline test, so a test can pin the choice whatever session runs it.
#'
#' @param verbose A logical value indicating whether the run prints a line per
#'   item.
#' @return A logical value.
#' @keywords internal
#' @noRd
use_bar <- function(verbose) {
  !verbose && interactive() && !isTRUE(getOption("knitr.in.progress"))
}

#' One line of run progress
#'
#' @param i An integer giving the documents done so far.
#' @param n An integer giving the documents in the run.
#' @param per A number giving seconds per measured document, or `NA` before the
#'   first one.
#' @param bar A logical value indicating whether to draw the bar.
#' @param verb A string naming the work, which opens the line.
#' @param unit A string naming one unit of the work, for the rate.
#' @param pad An integer giving the column the bar, or the count, starts in.
#'   Default is the width of `verb` and two spaces.
#' @param cells An integer giving the bar's width in cells.
#' @return A string.
#' @keywords internal
#' @noRd
progress_line <- function(
  i,
  n,
  per,
  bar = FALSE,
  verb = "Measuring",
  unit = "doc",
  pad = nchar(verb) + 2L,
  cells = 24L
) {
  width <- cells
  filled <- floor(width * i / n)
  drawn <- if (!bar) {
    ""
  } else if (filled >= width) {
    paste0("[", strrep("=", width), "]  ")
  } else if (filled == 0L) {
    paste0("[", strrep(" ", width), "]  ")
  } else {
    paste0(
      "[",
      strrep("=", filled - 1L),
      ">",
      strrep(" ", width - filled),
      "]  "
    )
  }
  rate <- if (is.finite(per)) {
    sprintf("  %s/%s  ETA %s", fmt_dur(per), unit, fmt_dur((n - i) * per))
  } else {
    ""
  }
  # The count and the percentage are padded to their widest, so a bar redrawn
  # in place holds still and logged lines line up.
  sprintf(
    "%s%s%*d/%d  %3d%%%s",
    formatC(verb, width = -pad),
    drawn,
    nchar(n),
    i,
    n,
    as.integer(floor(100 * i / n)),
    rate
  )
}

#' Run an instrument against one document
#'
#' The loop stays here rather than being flattened into the corpus loop. Every
#' item for a document is called contiguously. The first pays a full prefill and
#' the rest match the document as a cached prefix. Sweeping item-outer across a
#' corpus issues the same number of calls and gets no cache hits.
#'
#' @param text,instrument,backend,num_ctx,image As in `measure_one()`.
#' @param only A character vector of item names to run, or `NULL` for all of
#'   them.
#' @param hashes A named character vector of prompt hashes by item, or `NULL` to
#'   compute them.
#' @param verbose A logical value indicating whether to print one line per item.
#' @return A data frame of one row per item and label, with no rows when no item
#'   is left to run.
#' @keywords internal
#' @noRd
measure_doc <- function(
  text,
  instrument,
  backend,
  num_ctx = NULL,
  only = NULL,
  hashes = NULL,
  image = NULL,
  verbose = FALSE
) {
  items <- instrument$items
  if (!is.null(only)) {
    items <- items[names(items) %in% only]
  }
  if (length(items) == 0L) {
    return(tibble::tibble())
  }
  if (is.null(hashes)) {
    hashes <- item_hashes(instrument, backend$model, nzchar(text))
  }

  rows <- vector("list", length(items))
  for (j in seq_along(items)) {
    nm <- names(items)[j]
    r <- measure_one(text, items[[j]], instrument, backend, num_ctx, image)
    r$item <- nm
    r$prompt_hash <- unname(hashes[[nm]])
    r$model <- backend$model
    rows[[j]] <- r[, c(ITEM_COLS, setdiff(names(r), ITEM_COLS))]
    if (verbose) {
      message(sprintf(
        "  %-8s %-6s p=%.4f other=%.2e %6.0f ms%s",
        nm,
        r$winner[1] %||% "??",
        # A failed call has no probabilities, and max() over none warns.
        if (isTRUE(r$call_ok[1])) max(r$prob) else NA_real_,
        r$prob_other[1] %||% NA_real_,
        r$wall_ms[1] %||% NA_real_,
        if (isTRUE(r$call_ok[1])) "" else paste0("  FAILED. ", r$error[1])
      ))
    }
  }
  dplyr::bind_rows(rows)
}

#' Column names a results frame already carries
#'
#' A `keep` column naming one of these arrives at bind_cols() beside the column
#' it collides with, both come back suffixed, and every later read of that name
#' returns NULL. The failure surfaces a function or two downstream, in as_wide()
#' or an audit join, with nothing pointing back at the argument that caused it.
#'
#' Read off the schema rather than listed, so a column added to fail_rows() or
#' to ITEM_COLS is reserved with nothing here to remember. A hand-kept list is
#' the same class of defect this check exists to catch, silent and one edit
#' away.
#'
#' `id` is the one name still written literally. The meta frame in
#' measure.data.frame() is its only source, and reaching it from here would
#' cost more than it saves.
#'
#' Costs one K-row tibble per measure() call, not per document.
#'
#' @param instrument A `fathom_instrument`.
#' @return Character vector of reserved column names.
#' @keywords internal
#' @noRd
reserved_cols <- function(instrument) {
  c("id", ITEM_COLS, names(fail_rows(instrument)))
}

#' Measure documents against an instrument
#'
#' @description
#' Runs every item in the instrument against every document and returns the
#' probabilities in `$probs`, one row per document, item and label, beside
#' `$calls`, one row per document and item.
#'
#' @section Prompt ordering:
#' The document goes before the question, and items for a document run
#' contiguously, so Ollama's prefix cache prefills the document once per
#' document rather than once per item.
#'
#' @section Checkpointing:
#' Checkpointing is off by default. Supply `checkpoint` to enable it. One `.rds`
#' per document holds the full record plus provenance, and a document whose
#' items are all current is skipped without a model call. There is no separate
#' resume function. Calling this twice against the same directory does only what
#' is outstanding.
#'
#' Invalidation is per item, keyed on a hash of the question, cue, labels,
#' scaffolding, and model. Rewording one item re-extracts that item across the
#' corpus at question-only cost, since the first call still warms the document
#' cache. This is not proportionally cheap. A partial rerun still pays the full
#' document prefill for every row.
#'
#' `scores` are the exception, and they are outside the hash on purpose.
#' Rescaling an ordinal instrument changes no token the model sees, so
#' re-extracting would be waste. The expected score is instead recomputed from
#' the stored probabilities against whatever instrument is in hand, on this path
#' and in [read_checkpoints()], so a rescaled resume returns the new arithmetic
#' without spending a call.
#'
#' There is no schema tag. A change to scoring code that leaves prompts
#' unchanged will not invalidate anything. Delete the directory by hand.
#'
#' @section Cost of `num_ctx = "auto"`:
#' One extra call per `measure()` invocation, not per document. Running several
#' instruments over the same corpus measures once each, because a different
#' instrument means a different prompt and therefore a different token count.
#' Passing an integer once the corpus is known skips it.
#'
#' @section Retrying failures:
#' A failed item is stored with `call_ok = FALSE` and is therefore stale on the
#' next pass, so re-running retries exactly those. There is no separate retry
#' sweep, which would run against a cold cache.
#'
#' @param x A character vector of documents, or a data frame.
#' @param backend An [ollama_backend()] object.
#' @param instrument An [instrument()].
#' @param ... Passed to methods by the generic. On a method it is not used,
#'   and is refused if anything reaches it, since every argument after it is
#'   name-only and a misspelling would otherwise be swallowed.
#' @return A `fathom_results` object. A list of two frames, with the run's
#' provenance on its attributes. `$probs` holds one row per document, item and
#' label. `$calls` holds one row per document and item, carrying everything that
#' describes the answer rather than any one label, so it is stored once instead
#' of repeating down the label rows.
#'
#' ## Keys, in both frames, and what a join is on
#'
#' - `id` is the document identifier, from the `id` column or from the names of
#'   a character vector.
#' - `item` names which instrument question produced the row.
#'
#' ## `$probs`, differing down the labels of one call
#'
#' - `label` is the label this row scores, in the order the instrument declares
#'   them.
#' - `prob_renorm` is `prob` rescaled so the labels sum to 1. The measure. `NA`
#'   when no label appeared at all, since there is nothing to condition on.
#' - `prob` is the raw probability, against every token the model could have
#'   written. Does not sum to 1 down the labels.
#' - `present` says whether any token counting as this label appeared in the
#'   returned distribution. A `prob` of zero means the label was measured at
#'   zero when this is `TRUE`, and fell below `top_logprobs` when it is `FALSE`.
#' - `call_ok` is `FALSE` when the call failed. One fact per call, kept here
#'   rather than only in `$calls` because it guards almost every filter over
#'   these rows.
#'
#' ## `$calls`, one row per document and item
#'
#' Anything named in `keep` rides here too, since a carried column describes the
#' document rather than any one label.
#'
#' - `error` is the message when the call failed, otherwise `NA`. Non-`NA`
#'   exactly when `call_ok` is `FALSE`.
#' - `truncated` is `TRUE` when the prompt came within 64 tokens of `num_ctx`,
#'   so the document may have lost its opening.
#' - `top1_token` is the token the model actually generated, whether or not it
#'   counts as a label.
#' - `top1_prob` is the probability of that token. When it is not a label, this
#'   is the only record of how much mass the winning non-label held.
#' - `prob_other` is the mass on tokens counting as no label. The diagnostic
#'   that says whether the question landed.
#' - `all_absent` is `TRUE` when no label appeared anywhere in the returned
#'   distribution.
#' - `margin` is the gap between the top two `prob_renorm` values. What
#'   [inspect_cases()] sorts on to find the closest calls above two labels.
#' - `expected_score` is `prob_renorm` weighted by the instrument's `scores`.
#'   `NA` for an instrument built without them.
#' - `prompt_hash` is the hash of the assembled prompt. What checkpoint
#'   invalidation matches on, and not for reading.
#'
#' ## On the attributes, constant for the run
#'
#' `instrument` and `backend` as given, `model`, `num_ctx` as resolved,
#' `exclusive`, and `run_at`.
#'
#' @seealso [ollama_backend()] and [instrument()] for its two inputs,
#'   [preview()] for the prompt it sends, [read_checkpoints()] for a run cut
#'   short, and [item_report()] and [inspect_cases()] for reading what comes
#'   back.
#' @examples
#' \dontrun{
#' # Needs a running Ollama server, so it is not run here.
#' oll <- ollama_backend("gemma4:31b")
#' b <- instrument(
#'   items = "Where does this sentence fall on the six-point sentiment scale?",
#'   labels = c("1", "2", "3", "4", "5", "6"),
#'   exclusive = TRUE
#' )
#' res <- measure(parlasent, instrument = b, backend = oll, text = "sentence",
#'   id = "id")
#' res$probs
#' }
#' @export
measure <- function(x, ...) {
  UseMethod("measure")
}

#' @rdname measure
#' @section Character input:
#' A character vector stores the documents on the results in a `docs`
#' attribute, so the audit functions find them without a `docs` argument. Ids
#' are the vector's names when it has them, and its positions otherwise.
#'
#' A data frame does not, and `measure()` never writes the document into a
#' column. It reads the one you name and keeps a hash of the assembled prompt.
#' Carrying the text instead would put a copy of the corpus in `$calls`, once
#' per item, and the results are the object that gets saved.
#'
#' Naming that column in `keep` carries it deliberately, and then it is a kept
#' column like any other rather than something the schema promises.
#'
#' @export
measure.character <- function(x, backend, instrument, ...) {
  ids <- names(x) %||% as.character(seq_along(x))
  res <- measure(
    tibble::tibble(.id = ids, .text = as.character(x)),
    backend = backend,
    instrument = instrument,
    text = ".text",
    id = ".id",
    ...
  )

  # Stashed so the audit functions need no `docs` argument, and so a subset or
  # a reordering between calls cannot misalign ids. The data frame method does
  # not stash.
  #
  # Named for the role rather than for what happens to be in it. as_docs()
  # accepts a character vector of documents under the name `docs` already, so
  # calling the same vector `texts` here made one function use two words for
  # one thing.
  attr(res, "docs") <- stats::setNames(as.character(x), ids)
  res
}

#' Pick the document a backend check should probe with
#'
#' @description
#' The one nearest the median word count among the non-empty documents. `NULL`
#' when nothing qualifies, which sends `check_backend()` down its generic path
#' rather than probing with an empty string.
#'
#' @param x A character vector of documents.
#' @return A character scalar, or `NULL`.
#' @keywords internal
#' @noRd
probe_doc <- function(x) {
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) {
    return(NULL)
  }
  n <- lengths(strsplit(trimws(x), "\\s+"))
  x[[which.min(abs(n - stats::median(n)))]]
}

#' @rdname measure
#' @param text A string naming the column holding document text. Optional when
#'   `image` names one, so a picture with no caption is a corpus. Default is
#'   `NULL`.
#' @param image A string naming a column holding media sent alongside the prompt
#'   rather than inside it. A character column is one picture per document. A
#'   list column is several, one element per document holding its paths in
#'   order, which is how a video window arrives. Nothing is shared between
#'   calls, so a run costs documents times items at close to full price, and a
#'   window of K frames costs K encodes on every call. `prepare_image()` builds
#'   a corpus in the character shape. Default is `NULL`.
#' @param id A string naming an identifier column. Defaults to the integer row
#'   index.
#' @param num_ctx An integer giving the context window ceiling, or `"auto"`.
#'   Left unset, Ollama allocates the model's full window and the cache that
#'   goes with it whether the prompts need it or not, which on a local machine
#'   is memory taken from elsewhere. Setting the ceiling to what the corpus
#'   needs returns it. Setting it too low truncates silently, since llama.cpp
#'   keeps the tail and the answer looks ordinary, and the size needed is not
#'   knowable by inspection because documents and questions both vary and the
#'   answer needs room after both.
#'
#'   So `"auto"` (the default) measures it. The longest document by word count
#'   is sent once with no ceiling and its token count read back, then rounded up
#'   with headroom. An integer is used as given, for a corpus already known.
#'   `NULL` or `NA` sends no ceiling and lets Ollama choose, in which case the
#'   `truncated` column is `NA` for every row. A resumed run uses whatever its
#'   checkpoints were written under, overriding all of these.
#' @param keep A character vector of additional columns to carry into the
#'   results. Names must be unique, and a column whose name the results already
#'   use is refused, since both copies would come back suffixed and the name
#'   would then read as `NULL`. Rename it in `x` first.
#' @param items A character vector of item names to restrict the run to, or
#'   `NULL` (the default) for every item.
#' @param checkpoint A string giving the directory for per-document `.rds`
#'   files, or `NULL` (the default) to hold everything in memory.
#' @param verbosity A string setting how much to report. Each level is the one
#'   before it plus more.
#'
#'   - `"quiet"` reports progress and the run summary only.
#'   - `"normal"` is the default. It adds the header, meaning a title over one
#'     line each for the model, the context ceiling, the first token the
#'     backend check read, and the size of the job.
#'   - `"verbose"` adds one line per item, with its winning label, probability,
#'     leaked mass, and wall time.
#'
#'   Every check runs at every level. Only the reporting changes. `"quiet"` is
#'   for loops over several instruments against one corpus, where the caller
#'   states the configuration once rather than once per call. For no output at
#'   all, wrap the call in [suppressMessages()]. Progress is a bar redrawn in
#'   place in an interactive session, and a line every ten documents anywhere
#'   else, such as a knitted document or a log.
#' @export
measure.data.frame <- function(
  x,
  backend,
  instrument,
  ...,
  text = NULL,
  id = NULL,
  image = NULL,
  keep = character(0),
  num_ctx = "auto",
  items = NULL,
  checkpoint = NULL,
  verbosity = c("normal", "quiet", "verbose")
) {
  no_dots(..., .fn = "measure")
  # Resolved to the two booleans the body already used, so nothing downstream
  # of here changes. match.arg() supplies the default and rejects anything
  # outside the three levels.
  verbosity <- match.arg(verbosity)
  quiet <- identical(verbosity, "quiet")
  verbose <- identical(verbosity, "verbose")

  # Both are bundles of settings and nothing tells them apart by position, so
  # a call passing the two the other way round is named here rather than left
  # to the class check below.
  if (
    inherits(backend, "fathom_instrument") &&
      inherits(instrument, "fathom_backend")
  ) {
    stop(
      fmt_err(paste0(
        "`backend` and `instrument` arrived the other way round. The backend ",
        "comes second and the instrument third, so pass them in that order ",
        "or name both."
      )),
      call. = FALSE
    )
  }
  stopifnot(
    inherits(instrument, "fathom_instrument"),
    inherits(backend, "fathom_backend")
  )
  # One of the two, or both. A row can carry a picture and its caption, and a
  # row can carry either alone. Neither is a call with no document in it.
  if (is.null(text) && is.null(image)) {
    stop(
      fmt_err(paste0(
        "name `text`, `image`, or both. Available columns are ",
        paste(names(x), collapse = ", ")
      )),
      call. = FALSE
    )
  }
  bad <- setdiff(c(text, image), names(x))
  if (length(bad)) {
    stop(
      fmt_err(paste0(
        "column(s) not found in `x`. ",
        paste(bad, collapse = ", "),
        ". Available columns are ",
        paste(names(x), collapse = ", ")
      )),
      call. = FALSE
    )
  }
  # A declaration the server made about itself, checked before anything is
  # spent. Ollama returns HTTP 200 for a field it does not recognize, so an
  # undeclared column would otherwise be discarded in silence.
  have <- backend$modalities %||% "text"
  want <- c(if (!is.null(text)) "text", if (!is.null(image)) "image")
  short <- setdiff(want, have)
  if (length(short)) {
    stop(
      fmt_err(
        paste0(
          "this backend does not take ",
          paste(short, collapse = " or "),
          ". It declares ",
          paste(have, collapse = ", "),
          "."
        ),
        paste0(
          "Sending it anyway would return a full results frame built from ",
          "the prompt alone, since the server accepts a field it does not ",
          "recognize without complaint."
        )
      ),
      call. = FALSE
    )
  }
  missing_keep <- setdiff(c(id, keep), names(x))
  if (length(missing_keep)) {
    stop(
      fmt_err(paste0(
        "column(s) not found in `x`. ",
        paste(missing_keep, collapse = ", ")
      )),
      call. = FALSE
    )
  }
  # After the existence check, so a typo is reported as a missing column rather
  # than as a name that happens not to be reserved. Only `keep` is checked. The
  # `id` column is read for its values and written back under the name `id`, so
  # `id = "prob"` carries nothing called prob into the results.
  clash <- intersect(keep, reserved_cols(instrument))
  if (length(clash)) {
    stop(
      fmt_err(
        paste0(
          "`keep` names column(s) the results already carry. ",
          paste(clash, collapse = ", "),
          "."
        ),
        paste0(
          "Both copies would come back suffixed and every later read of that ",
          "name would return NULL, one or two functions downstream of here. ",
          "Rename the column in `x` first."
        )
      ),
      call. = FALSE
    )
  }
  # After the reserved check, so a repeated reserved name is reported as one
  # that cannot be carried at all rather than as one carried twice. Fixing it
  # the other way round costs two passes. Within `keep` only. Naming the same
  # column as both `id` and `keep` is redundant rather than broken, since the
  # identifier is written back under the name `id` and the kept copy keeps its
  # own.
  if (anyDuplicated(keep)) {
    stop(
      fmt_err(paste0(
        "`keep` names the same column more than once. Repeated names are ",
        paste(unique(keep[duplicated(keep)]), collapse = ", "),
        ". Each column is carried once, so name it once."
      )),
      call. = FALSE
    )
  }

  if (!is.null(items)) {
    unknown <- setdiff(items, names(instrument$items))
    if (length(unknown)) {
      stop(
        fmt_err(paste0(
          "`items` names item(s) the instrument does not carry. ",
          paste(unknown, collapse = ", "),
          ". Items are ",
          paste(names(instrument$items), collapse = ", "),
          "."
        )),
        call. = FALSE
      )
    }
  }

  dat <- x
  ids <- if (is.null(id)) {
    as.character(seq_len(nrow(dat)))
  } else {
    as.character(dat[[id]])
  }
  # Ids key the checkpoints and the audit joins, so a repeat silently doubles
  # one document and drops another.
  if (anyDuplicated(ids)) {
    stop(
      fmt_err(paste0(
        "document ids must be unique. Repeated ids are ",
        paste(utils::head(unique(ids[duplicated(ids)]), 5), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }

  hashes <- item_hashes(instrument, backend$model, !is.null(text))
  wanted <- if (is.null(items)) names(instrument$items) else items
  hashes <- hashes[names(hashes) %in% wanted]
  use_ck <- !is.null(checkpoint)

  # Resolved per run rather than on the backend, since "auto" measures it from
  # this corpus.
  docs_text <- if (is.null(text)) {
    rep("", nrow(dat))
  } else {
    as.character(dat[[text]])
  }
  # A character column is a corpus of single pictures. A list column is a
  # corpus of sets, one element per document. as_media() returns the same shape
  # for both, so everything below indexes with [[ and gets a vector.
  docs_image <- if (is.null(image)) NULL else as_media(dat[[image]])

  # Both checks write what they found into `pre` rather than printing it, so
  # the header can show it in one block. Notes they raise are held and printed
  # under the header, or before the error when a check fails or stops.
  pre <- new.env()
  held <- character(0)
  release <- function() {
    any <- length(held) > 0L
    for (m in held) {
      message(m, appendLF = FALSE)
    }
    held <<- character(0)
    invisible(any)
  }
  holding <- function(expr) {
    tryCatch(
      withCallingHandlers(
        expr,
        message = function(m) {
          held <<- c(held, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        release()
        stop(e)
      }
    )
  }

  ctx <- holding(resolve_num_ctx(
    num_ctx,
    docs_text,
    instrument,
    backend,
    ids = ids,
    checkpoint = checkpoint,
    images = docs_image,
    quiet = quiet,
    report = pre
  ))

  if (!use_ck && nrow(dat) * length(hashes) > 2000) {
    warning(
      fmt_warn(paste0(
        "checkpointing is OFF for an estimated ",
        nrow(dat) * length(hashes),
        " model calls. A crash loses everything. Supply `checkpoint`."
      )),
      call. = FALSE
    )
  }
  # The check runs at every verbosity. A failure prints its notes and stops the
  # run.
  #
  # Probed with a real document rather than an invented one, so the prompt the
  # check sends is the prompt the run is about to send. Median length rather
  # than the first row, because a first row can be a header, an empty string, or
  # in an ATI-style corpus a pointer to an attachment, none of which exercise
  # anything. Nearest the median rather than the median itself, so the pick is
  # always a document that exists. Ties go to the first, so a rerun on the same
  # corpus checks the same document.
  ok <- holding(check_backend(
    backend,
    instrument,
    num_ctx = ctx,
    text = if (is.null(text)) NULL else probe_doc(docs_text),
    images = !is.null(docs_image),
    report = pre
  ))
  if (!ok) {
    release()
    stop("backend check failed. Not starting.", call. = FALSE)
  }

  if (!quiet) {
    message(run_header(
      backend,
      pre,
      nrow(dat),
      names(hashes),
      instrument,
      if (use_ck) checkpoint else NULL
    ))
  }
  # Whether anything has printed under the header. The summary opens with a
  # blank line only when something has, so a run with nothing to report between
  # the two, such as a resume that skips every document, is not left with two.
  shown <- release()

  # `keep` describes the document, so it is read from `x` on every call rather
  # than off the checkpoint. A stored row carries whatever was kept when it was
  # written, so returning it as stored gave a resume under another `keep`, or
  # after a kept column was corrected, the old columns on cached documents and
  # the new ones on measured documents.
  schema <- reserved_cols(instrument)
  rekeep <- function(rows, i) {
    if (is.null(rows) || nrow(rows) == 0L) {
      return(rows)
    }
    rows <- rows[, intersect(names(rows), schema), drop = FALSE]
    if (!length(keep)) {
      return(rows)
    }
    dplyr::bind_cols(
      rows[, "id", drop = FALSE],
      dat[rep(i, nrow(rows)), keep, drop = FALSE],
      rows[, setdiff(names(rows), "id"), drop = FALSE]
    )
  }

  t0 <- Sys.time()
  out <- vector("list", nrow(dat))
  n_called <- 0L

  # A bar where use_bar() says one can be shown, and a line every ten
  # documents measured anywhere else, both starting in the header's value
  # column.
  run_env <- environment()
  pb <- NULL
  pb_line <- ""
  if (use_bar(verbose)) {
    pb <- cli::cli_progress_bar(
      total = nrow(dat),
      format = "{pb_line}",
      clear = FALSE,
      .envir = run_env
    )
  }
  tick <- function(i, called) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    per <- if (n_called > 0L) el / n_called else NA_real_
    if (!is.null(pb)) {
      pb_line <<- progress_line(
        i,
        nrow(dat),
        per,
        bar = TRUE,
        pad = HEADER_WIDTH,
        cells = BAR_CELLS
      )
      cli::cli_progress_update(id = pb, set = i, .envir = run_env)
      # cli draws the bar only once a run has lasted this long.
      if (el >= getOption("cli.progress_show_after", 2)) shown <<- TRUE
    } else if (called && n_called %% 10L == 0L && i < nrow(dat)) {
      # Not on the final document. n_called lands on a multiple of ten there
      # whenever the corpus size is divisible by ten, and the summary follows
      # immediately with better information.
      progress_message(progress_line(i, nrow(dat), per, pad = HEADER_WIDTH))
      shown <<- TRUE
    }
  }

  for (i in seq_len(nrow(dat))) {
    doc_id <- ids[i]
    prev <- if (use_ck) ckpt_read(doc_id, checkpoint) else NULL
    todo <- ckpt_stale(prev, hashes, backend$model)

    if (length(todo) == 0L) {
      # Restricted to the requested items, so the return shape does not depend
      # on how much of the corpus happened to be cached. The checkpoint keeps
      # every item it has ever held, so calling without `items` still returns
      # the full record and costs nothing on a cached corpus.
      out[[i]] <- rekeep(restrict_items(prev$results, wanted), i)

      # Nothing to extract, but the stored instrument may still be out of date.
      # `scores` are the one field that can change without changing the hash, so
      # a rescaled run skips every document and leaves the old values on disk.
      # read_checkpoints() has no instrument but the stored one, so it would
      # read the corpus back under a scale nobody is using any more.
      #
      # Rewritten only when they actually differ, so an ordinary resume still
      # touches no files.
      if (use_ck && !identical(prev$instrument$scores, instrument$scores)) {
        prev$instrument <- instrument
        prev$results <- apply_scores(prev$results, instrument)
        ckpt_write(prev, doc_id, checkpoint)
      }
      tick(i, called = FALSE)
      next
    }

    ti <- Sys.time()
    new <- measure_doc(
      docs_text[i],
      instrument,
      backend,
      num_ctx = ctx,
      only = todo,
      hashes = hashes,
      image = if (is.null(docs_image)) NULL else docs_image[[i]],
      verbose = verbose
    )
    if (verbose) {
      shown <- TRUE
    }

    keep_rows <- if (!is.null(prev) && nrow(prev$results) > 0L) {
      rekeep(prev$results[!prev$results$item %in% todo, , drop = FALSE], i)
    } else {
      NULL
    }

    meta <- tibble::tibble(id = doc_id)
    if (length(keep)) {
      meta <- dplyr::bind_cols(meta, dat[i, keep, drop = FALSE])
    }
    new <- dplyr::bind_cols(meta[rep(1L, nrow(new)), , drop = FALSE], new)

    merged <- dplyr::bind_rows(keep_rows, new)
    merged$item <- factor(merged$item, levels = names(instrument$items))
    merged <- merged[order(merged$item), , drop = FALSE]
    merged$item <- as.character(merged$item)

    if (use_ck) {
      ckpt_write(
        list(
          id = doc_id,
          instrument = instrument,
          backend = backend,
          # NA rather than NULL, so a run with no ceiling is distinguishable
          # from a checkpoint that predates this field.
          num_ctx = if (is.null(ctx)) NA_integer_ else as.integer(ctx),
          run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
          results = merged
        ),
        doc_id,
        checkpoint
      )
    }

    # After the checkpoint write, which stores every item this document has.
    out[[i]] <- restrict_items(merged, wanted)
    n_called <- n_called + 1L

    tick(i, called = TRUE)
  }

  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb, .envir = run_env)
  }

  res <- dplyr::bind_rows(out)

  # Restored rows carry the expected score computed under whatever `scores`
  # were in force when the checkpoint was written. Recomputing here makes the
  # column a function of the instrument in hand, and costs nothing on rows that
  # were just scored.
  res <- apply_scores(res, instrument)

  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  nf <- sum(!res$call_ok, na.rm = TRUE) / instrument$k
  # The rate lives here rather than on a final progress tick, which would only
  # repeat what this line says. Omitted below two documents, where it is either
  # undefined or the same number as the elapsed time.
  rate <- if (n_called > 1L) {
    sprintf(", %s per document", fmt_dur(el / n_called))
  } else {
    ""
  }
  message(
    if (shown) "\n" else "",
    fmt_fields(
      list(
        Done = sprintf(
          "%d processed, %d skipped, %d item failures",
          n_called,
          nrow(dat) - n_called,
          round(nf)
        ),
        Elapsed = paste0(fmt_dur(el), rate)
      ),
      name_width = HEADER_WIDTH
    )
  )
  # Only a checkpointed run can retry its failures alone. Without checkpoints a
  # re-run starts again from the first document. Laid out as a field rather
  # than a note, so it lines up with the summary above it.
  if (nf > 0 && use_ck) {
    message(fmt_fields(
      list(Tip = "Re-run to retry failures only."),
      name_width = HEADER_WIDTH
    ))
  }

  new_results(res, instrument, backend, num_ctx = ctx)
}

# The two grains a run produces. Everything above this point works on one flat
# frame, which is what the scoring loop, the progress line and the checkpoint
# payload all need, so the split happens once, here, at the only place a
# results object is built.
PROB_COLS <- c(
  "id",
  "item",
  "label",
  "prob_renorm",
  "prob",
  "present",
  "call_ok"
)

# Computable from what is kept, or telemetry nothing reads. `winner` is
# `slice_max(prob)` within a call, which is how every internal consumer already
# derives it. `logprob` is `log(prob)`, and `present` carries the one thing it
# said that `prob` cannot, which is whether a zero was measured or unseen.
#
# Every one is still computed and checkpointed. The split happens here and
# nowhere earlier so that scoring, the loop and the checkpoint directories
# already on disk stay as they were. `winner` and `wall_ms` feed the verbose
# line before they go, and `model` cannot leave the flat frame at all, since
# ckpt_stale() reads it there. On the results it is an attribute.
DROP_COLS <- c(
  "entropy",
  "logprob",
  "prefill_ms",
  "prompt_tokens",
  "wall_ms",
  "winner",
  "model"
)

# Ordered rather than derived, so the frame reads the way the article walks it.
# Anything else the run carried, which is whatever `keep` asked for, follows.
CALL_COLS <- c(
  "id",
  "item",
  "error",
  "truncated",
  "top1_token",
  "top1_prob",
  "prob_other",
  "all_absent",
  "margin",
  "expected_score",
  "prompt_hash"
)

#' Build a results object from the flat frame of a run
#'
#' Splits the flat frame a run produces into the two tables of a
#' `fathom_results` object and puts the run's provenance on its attributes.
#' It is the only constructor, called by [measure()] and [read_checkpoints()].
#'
#' `$probs` holds one row per document, item and label, and `$calls` one row
#' per document and item. `PROB_COLS` and `CALL_COLS` decide which columns go
#' where, `DROP_COLS` lists those kept in neither, and any other column the
#' frame carries, which is whatever `keep` asked for, joins `$calls`.
#'
#' @param x A data frame of one row per document, item and label, as the
#'   measurement loop and the checkpoint reader produce it.
#' @param instrument A `fathom_instrument` object, kept as an attribute.
#' @param backend A `fathom_backend` object, kept as an attribute along with
#'   its model name.
#' @param num_ctx An integer giving the context ceiling the run used, or
#'   `NULL` or `NA` for a run without one.
#'
#' @return An object of class `fathom_results`, a list of the tibbles `probs`
#'   and `calls` carrying the attributes `instrument`, `backend`, `model`,
#'   `num_ctx`, `exclusive` and `run_at`. [measure()] adds `docs` for
#'   character input, and [read_checkpoints()] overwrites `run_at`, the time
#'   of construction, with the time of the latest write.
#' @keywords internal
#' @noRd
new_results <- function(x, instrument, backend, num_ctx) {
  x <- tibble::as_tibble(x)
  keys <- c("id", "item")

  probs <- x[, intersect(PROB_COLS, names(x)), drop = FALSE]

  # Per-call columns in the stated order, then anything `keep` carried through,
  # which is per document and therefore per call as well.
  named <- intersect(CALL_COLS, names(x))
  carried <- setdiff(names(x), c(PROB_COLS, DROP_COLS, named))
  rows <- if (nrow(x)) !duplicated(x[, keys, drop = FALSE]) else logical(0)
  calls <- x[rows, c(named, carried), drop = FALSE]

  structure(
    list(probs = probs, calls = calls),
    instrument = instrument,
    backend = backend,
    model = backend$model,
    # Passed in and required, since the object built here is new and nothing
    # set on `x` reaches it. NA is a run with no ceiling.
    num_ctx = if (is.null(num_ctx)) NA_integer_ else as.integer(num_ctx),
    # An exclusive instrument's labels share one softmax at one position, so
    # its probabilities are a distribution. as_wide() and the print method read
    # this, since row sums are meaningful there and meaningless for independent
    # binary items.
    exclusive = isTRUE(instrument$exclusive),
    run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    class = "fathom_results"
  )
}

#' @export
print.fathom_results <- function(x, ...) {
  # The object is a list of two frames now, so a dplyr verb applied to it errors
  # rather than returning a classed thing with the columns missing. The guard is
  # kept for an object built before the split, or one taken apart by hand.
  if (!all(c("probs", "calls") %in% names(x))) {
    return(print(unclass(x), ...))
  }
  k <- attr(x, "backend")
  ctx <- attr(x, "num_ctx")
  cat(
    "Measurement results\n\n",
    fmt_fields(list(
      Documents = format(dplyr::n_distinct(x$probs$id), big.mark = ","),
      Items = as.character(dplyr::n_distinct(x$probs$item)),
      Labels = as.character(dplyr::n_distinct(x$probs$label)),
      Model = if (!is.null(k)) attr(x, "model") %||% k$model,
      Context = if (!is.null(k)) {
        if (is.null(ctx) || is.na(ctx)) {
          "none"
        } else {
          paste(fmt_ctx(ctx), "tokens")
        }
      },
      Run = if (!is.null(k)) attr(x, "run_at"),
      Tables = sprintf(
        "$probs %d x %d, $calls %d x %d",
        nrow(x$probs),
        ncol(x$probs),
        nrow(x$calls),
        ncol(x$calls)
      )
    )),
    "\n",
    sep = ""
  )
  # Counted in `$calls`, one row per call, where `error` is set exactly when
  # the call failed. Counting `$probs` would count each failure once per label.
  nf <- sum(!is.na(x$calls$error))
  nt <- sum(x$calls$truncated, na.rm = TRUE)
  na <- sum(x$calls$all_absent, na.rm = TRUE)
  calls <- function(n) sprintf("%d call%s", n, if (n == 1L) "" else "s")
  if (nf) {
    cat_note("Warning", paste(calls(nf), "failed."))
  }
  if (nt) {
    cat_note(
      "Warning",
      paste(
        calls(nt),
        "truncated, so the opening of the document was dropped."
      )
    )
  }
  if (na) {
    cat_note(
      "Warning",
      paste(
        calls(na),
        "returned no label anywhere in the distribution, which points to a",
        "template problem."
      )
    )
  }
  if (isTRUE(attr(x, "exclusive"))) {
    cat_note(
      "Note",
      "The labels share one softmax, so each row sums to a distribution."
    )
  }
  cat_note("Tip", "Next, item_report(), then inspect_cases().")
  cat("\n")
  print(x$probs, ...)
  invisible(x)
}
