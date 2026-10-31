# Timeout for calls to /api/tags, used by the discovery branch of
# ollama_backend() and by check_backend(). Separate from backend$timeout, which
# governs generation.
TAGS_TIMEOUT <- 15L

#' Configure an Ollama backend
#'
#' @description
#' Bundles connection and sampling settings into one object, passed down whole
#' so parameters cannot drift between call sites. Drift can create context size
#' differences between calls, defeating the prefix cache and forcing Ollama to
#' reload the model.
#'
#' Scoring reads log probabilities, which need an Ollama server at version
#' 0.12.11 or later. [measure()] checks for them before a run and stops when
#' none come back.
#'
#' @section Omitting `model`:
#' The call contacts the server, prints the models installed there, and errors.
#' It does not return `NULL` or a partial object, so [instrument()] and
#' [measure()] never receive a backend with no model attached.
#'
#' @section Temperature and seed:
#' Neither is exposed. Temperature is fixed at 0, where the returned
#' probabilities are the model's unmodified posterior. Scoring reads the
#' distribution at generated position 1 and discards the token that would have
#' been drawn from it, so temperature rescales the logits being measured without
#' changing what is read.
#'
#' At temperature 0 there is no sampling, and identical calls return
#' byte-identical token sequences. A seed has nothing to control.
#'
#' @param model A string giving the model name as Ollama knows it, e.g.
#'   `"gemma3:27b"`. The full tag is required. Shorthand is refused. Omit it
#'   entirely to list what the server has installed.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param keep_alive A string setting how long Ollama holds the model after a
#'   call. The default, `"24h"`, is long because a corpus run over one model
#'   should not pay a reload between documents. A loop over several models on
#'   one machine wants a short value instead, since each one called stays
#'   resident for the whole window and the cost arrives as memory pressure
#'   rather than as an error.
#' @param timeout A number giving the request timeout in seconds for generation
#'   calls. A cold prefill on a very long document can run minutes. Discovery
#'   and validation use their own shorter timeout. Default is 600.
#' @param top_logprobs An integer giving the number of alternatives to return at
#'   each position. Ollama caps this at 20. Lower it only if every label
#'   reliably appears in the top-k. Default is 20.
#' @param host A string giving the server URL. Defaults to `OLLAMA_HOST` or
#'   localhost. A host given with no scheme and no port takes port 11434, the
#'   way Ollama's own client reads `OLLAMA_HOST`.
#' @param modalities A character vector of what the server accepts, which is
#'   `text`, `image` or both. A fact about the server rather than the model.
#'   [measure()] refuses a column naming something absent here, locally and
#'   before any call, because Ollama returns HTTP 200 for a field it does not
#'   recognize. Audio and video are not among them. [prepare_audio()] turns a
#'   recording into a transcript, and [prepare_video()] turns a video into
#'   frames plus a transcript when it has sound, which reach the model as `text`
#'   and `image`. Default is `c("text", "image")`.
#' @param validate A logical value indicating whether to contact the server and
#'   confirm `model` is installed. `FALSE` constructs blind with no network
#'   call, which is what examples, documentation builds, and constructor tests
#'   use. Default is `TRUE`.
#' @return An object of class `fathom_backend`, a list of eight settings. It
#' holds no connection, so it is cheap to build and safe to keep.
#'
#' - `model` is the tag calls are sent to. Recorded on the results as the
#'   `model` attribute, so a corpus says which model produced it.
#' - `host` is the server address, normalized to include a scheme.
#' - `modalities` holds what this server accepts. [measure()] refuses a column
#'   naming something absent here, before any call, because Ollama answers HTTP
#'   200 to a field it does not recognize and an undeclared column would
#'   otherwise be discarded in silence.
#' - `top_logprobs` says how many alternatives are requested at the answer
#'   position. Ollama caps this at 20. A label whose token falls outside the
#'   returned set is scored as absent, with a `prob_renorm` of exactly 0, so
#'   `prob_other` absorbs its mass, and [item_report()] reports how often that
#'   happened as `pct_label_absent`.
#' - `think` is always `FALSE`, and not an argument. Reasoning is disabled on
#'   every call, because scoring reads generated position 1 and a model that
#'   reasons first puts something else there.
#' - `keep_alive` says how long the server holds the model in memory between
#'   calls. Long enough that a corpus run does not pay reload cost.
#' - `timeout` is the number of seconds before a single call is abandoned. A
#'   call that times out returns a failed row rather than ending the run.
#' - `validated` says whether the constructor confirmed the model is installed.
#'   `FALSE` when built with `validate = FALSE`, which skips the check rather
#'   than failing it.
#'
#' Called with no model name it prints what is installed and stops, which
#' doubles as a way to confirm the server is running.
#' @seealso [measure()], which takes the backend, and [instrument()], which can
#'   probe labels against it.
#' @examples
#' ollama_backend("gemma3:27b", validate = FALSE)
#' @export
ollama_backend <- function(
  model = NULL,
  ...,
  keep_alive = "24h",
  timeout = 600,
  top_logprobs = 20L,
  host = NULL,
  modalities = c("text", "image"),
  validate = TRUE
) {
  no_dots(..., .fn = "ollama_backend")

  # ---- local checks first, so a bad argument fails before a network call ----
  if (!is.null(model) && (!is.character(model) || length(model) != 1L)) {
    stop("`model` must be a single string", call. = FALSE)
  }
  if (!is.numeric(top_logprobs) || length(top_logprobs) != 1L) {
    stop("`top_logprobs` must be a single number", call. = FALSE)
  }
  # A fact about the server rather than the model. Ollama returned HTTP 200 for
  # a body carrying a field named `zzzz`, so it discards what it does not
  # recognize without saying so. measure() therefore refuses an undeclared
  # column here rather than sending it and waiting to be told.
  #
  # Only what measure() can send is accepted. Audio and video reach the model
  # as text and frames through prepare_audio() and prepare_video(), so neither
  # belongs here until a native path sends one.
  known <- c("text", "image")
  if (!is.character(modalities) || length(modalities) == 0L) {
    stop("`modalities` must be a character vector", call. = FALSE)
  }
  unknown <- setdiff(modalities, known)
  if (length(unknown)) {
    stop(
      fmt_err(
        paste0(
          "`modalities` names something measure() cannot send. ",
          paste(unknown, collapse = ", "),
          ". It sends text and image."
        ),
        if (any(unknown %in% c("audio", "video"))) {
          paste0(
            "A recording reaches the model as a transcript through ",
            "prepare_audio(), and a video as frames, plus a transcript when ",
            "it has sound, through prepare_video()."
          )
        }
      ),
      call. = FALSE
    )
  }
  if (top_logprobs > 20L) {
    warning("Ollama caps top_logprobs at 20. Using 20.", call. = FALSE)
    top_logprobs <- 20L
  }

  # Resolved before the branching, since the discovery call needs the host and
  # no object exists yet to read it from.
  # An empty OLLAMA_HOST counts as unset, as it does for Ollama itself.
  env <- Sys.getenv("OLLAMA_HOST")
  host <- normalize_host(
    host %||% if (nzchar(env)) env else "http://localhost:11434"
  )

  build <- function(m, validated) {
    structure(
      list(
        model = m,
        keep_alive = keep_alive,
        timeout = timeout,
        # Fixed rather than settable, since scoring reads generated position 1
        # and a model that reasons first puts something else there. Kept as a
        # field so the request body and every checkpointed backend say so.
        think = FALSE,
        top_logprobs = as.integer(top_logprobs),
        host = host,
        modalities = modalities,
        validated = validated
      ),
      class = "fathom_backend"
    )
  }

  # ---- validate = FALSE, so construct blind --------------------------------
  if (!isTRUE(validate)) {
    if (is.null(model)) {
      stop(
        fmt_err(
          "`validate = FALSE` makes no network call, so `model` is required.",
          "Either name the model, or call ollama_backend() with validate = TRUE to see what is installed."
        ),
        call. = FALSE
      )
    }
    return(build(model, FALSE))
  }

  # ---- one call serves both remaining branches ------------------------------
  tags <- ollama_tags(host)

  if (inherits(tags, "fathom_tags_failure")) {
    # "unreachable" is a connection-level failure. "bad_response" covers a bad
    # status or a body that will not parse, meaning something answered but was
    # not Ollama.
    if (identical(tags$kind, "unreachable")) {
      stop(
        fmt_err(
          paste0("cannot reach Ollama at ", host, "."),
          "Check that Ollama is installed and running (`ollama serve`).",
          "If that host looks wrong, it came from the `host` argument or the OLLAMA_HOST variable."
        ),
        call. = FALSE
      )
    }
    stop(
      fmt_err(
        paste0(
          "reached ",
          host,
          " but the reply was not an Ollama model list."
        ),
        tags$message,
        "Something other than Ollama may be listening on that port."
      ),
      call. = FALSE
    )
  }

  if (nrow(tags) == 0L) {
    stop(
      fmt_err(
        paste0("Ollama is running at ", host, " but has no models installed."),
        "Install one with `ollama pull gemma3:27b` in a terminal shell.",
        "Browse what is available at https://ollama.com/library"
      ),
      call. = FALSE
    )
  }

  # ---- no model, so this call is a lookup rather than a constructor --------
  if (is.null(model)) {
    show_models(tags, host)
    stop(
      fmt_err(
        "`model` is required. Paste one of the tags above, e.g.",
        paste0('ollama_backend("', tags$name[1], '")')
      ),
      call. = FALSE
    )
  }

  # ---- model supplied, so it must be an exact tag --------------------------
  if (!model %in% tags$name) {
    show_models(tags, host)
    stop(
      fmt_err(paste0(
        '"',
        model,
        '" is not an installed tag on ',
        host,
        ". Choose one of the tags above."
      )),
      call. = FALSE
    )
  }

  build(model, TRUE)
}

#' @export
print.fathom_backend <- function(x, ...) {
  cat(
    "Ollama backend\n\n",
    fmt_fields(list(
      Model = x$model,
      Host = x$host,
      Logprobs = paste("top", x$top_logprobs),
      # NULL on a backend restored from a checkpoint written before this field.
      Modalities = paste(x$modalities %||% "text", collapse = ", ")
    )),
    "\n",
    sep = ""
  )
  # Printed only when `validated` is explicitly FALSE. A backend restored from
  # a checkpoint written before this field existed has no such field.
  if (isFALSE(x$validated)) {
    cat_note(
      "Note",
      paste0(
        "Not validated. Built with validate = FALSE, so the model name has ",
        "not been checked against the server."
      )
    )
  }
  invisible(x)
}

#' Read an image file as base64, for the `images` array
#'
#' Ollama takes base64 with no data-URI prefix. The file is read as raw bytes
#' rather than as text, so a PNG is not mangled by an encoding guess.
#'
#' @param path A file path.
#' @return A single base64 string.
#' @keywords internal
#' @noRd
image_b64 <- function(path) {
  if (!file.exists(path)) {
    stop(
      fmt_err(paste0("image file not found. ", path)),
      call. = FALSE
    )
  }
  jsonlite::base64_enc(readBin(path, "raw", file.size(path)))
}

#' Normalize a host string into a scheme-qualified URL with no trailing slash
#'
#' Read the way Ollama's own client reads `OLLAMA_HOST`. A host given with no
#' scheme takes port 11434 when it names no port, so `0.0.0.0` reaches the
#' server rather than port 80. A host given with a scheme keeps that scheme's
#' own default, which is how a server behind a proxy on 443 stays reachable.
#'
#' @param h A string giving a host, with or without a scheme or a port.
#' @return A string giving the host with `http://` added where no scheme was
#'   given, port 11434 added where neither a scheme nor a port was, and any
#'   trailing slash removed.
#' @keywords internal
#' @noRd
normalize_host <- function(h) {
  h <- sub("/$", "", trimws(h))
  if (!grepl("^https?://", h)) {
    if (!grepl(":[0-9]+$", h)) {
      h <- paste0(h, ":11434")
    }
    h <- paste0("http://", h)
  }
  h
}

#' Format a context ceiling for messages, including its absence
#'
#' @param x An integer giving a context ceiling, or `NULL` or `NA`.
#' @return A string, the ceiling with its thousands separated by commas, or
#'   `"none"`.
#' @keywords internal
#' @noRd
fmt_ctx <- function(x) {
  if (is.null(x) || is.na(x)) "none" else format(as.integer(x), big.mark = ",")
}

#' Build an endpoint URL
#'
#' @param backend A `fathom_backend` object.
#' @param path A string giving the endpoint path.
#' @return A string giving the full URL.
#' @keywords internal
#' @noRd
backend_url <- function(backend, path = "/api/generate") {
  paste0(normalize_host(backend$host), path)
}

#' What models the server has, or a classified failure
#'
#' A refused connection means Ollama is not running. Anything else, including a
#' bad status or a body that will not parse as JSON, means something answered
#' but was not Ollama.
#'
#' @param host A normalized host URL.
#' @param timeout A number giving the timeout in seconds. See `TAGS_TIMEOUT`.
#'   Modalities are not fetched here. This is called by the listing branch, by
#'   the model-validation branch, and by `check_backend()`, which [measure()]
#'   runs before every corpus, so a per-model call added here would be paid on
#'   runs that print nothing.
#'
#' @return A tibble with `name`, `size`, `params`, `quant`, sorted by name, or
#'   an object of class `fathom_tags_failure`.
#' @keywords internal
#' @noRd
ollama_tags <- function(host, timeout = TAGS_TIMEOUT) {
  out <- tryCatch(
    httr2::request(paste0(host, "/api/tags")) %>%
      httr2::req_timeout(timeout) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json(),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    return(structure(
      list(
        kind = if (inherits(out, "httr2_failure")) {
          "unreachable"
        } else {
          "bad_response"
        },
        message = conditionMessage(out)
      ),
      class = "fathom_tags_failure"
    ))
  }

  models <- out$models %||% list()
  if (length(models) == 0L) {
    return(tibble::tibble(
      name = character(0),
      size = numeric(0),
      params = character(0),
      quant = character(0)
    ))
  }

  tb <- tibble::tibble(
    name = vapply(
      models,
      function(m) as.character(m$name %||% m$model %||% ""),
      character(1)
    ),
    size = vapply(
      models,
      function(m) as.numeric(m$size %||% NA_real_),
      numeric(1)
    ),
    params = vapply(
      models,
      function(m) as.character(m$details$parameter_size %||% ""),
      character(1)
    ),
    quant = vapply(
      models,
      function(m) as.character(m$details$quantization_level %||% ""),
      character(1)
    )
  )
  tb <- tb[nzchar(tb$name), , drop = FALSE]
  tb[order(tb$name), , drop = FALSE]
}

#' The server's capability words, in the ones `modalities` takes
#'
#' The server says `vision` where [measure()] says `image`, and also declares
#' `completion`, `tools`, `thinking` and sometimes `audio`. Only the two the
#' package can act on are reported, in the spelling a caller would type into
#' `modalities`.
#'
#' @param cap A character vector from `/api/show`.
#' @return One string.
#' @keywords internal
#' @noRd
modality_words <- function(cap) {
  paste(c("text", if ("vision" %in% cap) "image"), collapse = ", ")
}

#' What each installed model accepts, one call per model
#'
#' `/api/tags` carries the name, size and quantization but not the
#' capabilities, so this is a second round trip per model.
#'
#' It is called from `show_models()` rather than from `ollama_tags()`, because
#' that is the only path that prints. `ollama_tags()` also serves the
#' model-validation branch and `check_backend()`, and [measure()] runs the
#' second before every corpus, so a fetch there would be paid on runs that
#' never show a listing.
#'
#' Only `text` and `image` are reported, because those are the two
#' [measure()] can act on. The server also declares `audio`, `tools` and
#' `thinking`, and naming a modality the package cannot send would invite a
#' call that fails later rather than here.
#'
#' A model whose call fails comes back empty rather than failing the listing,
#' and a first failure abandons the rest, so a server that answers `/api/tags`
#' and not `/api/show` costs one timeout instead of one per model.
#'
#' @param host A normalized host URL.
#' @param names A character vector of model tags, in the order the listing will
#'   print them.
#' @param timeout A number giving the seconds per call, shorter than
#'   `TAGS_TIMEOUT` because this runs once per model.
#' @return A character vector, one per name.
#' @keywords internal
#' @noRd
ollama_modalities <- function(host, names, timeout = 5L) {
  out <- rep(NA_character_, length(names))
  for (i in seq_along(names)) {
    got <- tryCatch(
      httr2::request(paste0(host, "/api/show")) %>%
        httr2::req_body_json(list(model = names[i])) %>%
        httr2::req_timeout(timeout) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json(),
      error = function(e) NULL
    )
    if (is.null(got)) {
      if (i == 1L) {
        break
      }
      next
    }
    out[i] <- modality_words(unlist(got$capabilities %||% list()))
  }
  ifelse(is.na(out), "", out)
}

#' Human-readable byte size
#'
#' @param bytes A numeric vector of sizes in bytes.
#' @return A character vector of sizes, in GB to one decimal from a billion
#'   bytes up and in MB below that, and `""` where `bytes` is `NA`.
#' @keywords internal
#' @noRd
fmt_size <- function(bytes) {
  ifelse(
    is.na(bytes),
    "",
    ifelse(
      bytes >= 1e9,
      sprintf("%.1f GB", bytes / 1e9),
      sprintf("%.0f MB", bytes / 1e6)
    )
  )
}

#' One line per installed model
#'
#' The name column is sized by the longest name that fits under a cap, not by
#' the longest name outright. A single Hugging Face path can run past forty
#' characters, which would pad every other row out to match it. A name over the
#' cap overflows its own row and leaves the rest aligned.
#'
#' @param tb A data frame of installed models from `ollama_tags()`, carrying
#'   `name`, `size`, `params` and `quant`, and `modalities` once fetched.
#' @return A character vector, one formatted line per model.
#' @keywords internal
#' @noRd
format_model_table <- function(tb) {
  sz <- fmt_size(tb$size)
  # Asked for by name rather than with `$`, because a tibble warns on a column
  # that is not there where a data frame returns NULL in silence. The column is
  # absent on a tibble from an older mock or from a server that answered
  # `/api/tags` and nothing else.
  mod <- if ("modalities" %in% names(tb)) {
    tb$modalities
  } else {
    rep("", nrow(tb))
  }
  extra <- trimws(paste(tb$params, tb$quant))
  widths <- nchar(tb$name)
  fits <- widths[widths <= 32L]
  line <- sprintf(
    "  %-*s   %*s   %-*s   %s",
    if (length(fits)) max(fits) else 32L,
    tb$name,
    max(nchar(sz)),
    sz,
    max(nchar(extra)),
    extra,
    mod
  )
  sub("\\s+$", "", line)
}

#' Print the model listing to stderr
#'
#' @param tb As in `format_model_table()`.
#' @param host A string giving the normalized host URL.
#' @return `NULL`, invisibly. The listing is printed as a message.
#' @keywords internal
#' @noRd
show_models <- function(tb, host) {
  # Fetched here rather than in ollama_tags(), since this is the only branch
  # that prints and the cost is one call per installed model.
  if (nrow(tb)) {
    tb$modalities <- ollama_modalities(host, tb$name)
  }
  # A title naming the server, then the table under it.
  message(
    "\nModels on ",
    host,
    "\n\n",
    paste(format_model_table(tb), collapse = "\n"),
    "\n"
  )
}

#' One-token generation with top-k log probabilities
#'
#' Posts to Ollama's native `/api/generate`. `think` is a top-level field rather
#' than a sampling option, which is why this is a direct POST. `num_predict` is
#' 1 because the measurement is the distribution at generated position 1. No
#' `format` field is sent, since Ollama returns unconstrained log probabilities
#' at each position regardless of a grammar.
#'
#' @param prompt A string giving the assembled prompt.
#' @param backend A `fathom_backend` object.
#' @param num_ctx An integer giving the context ceiling for this call, or `NULL`
#'   to omit the option and let Ollama choose. With no ceiling the prompt is
#'   sent with nothing to truncate against, so `prompt_eval_count` reports its
#'   true length.
#' @param image A character vector of media paths sent with the prompt, or
#'   `NULL`.
#' @return A list, the parsed JSON response from `/api/generate`, with `wall_ms`
#'   added.
#' @keywords internal
#' @noRd
ollama_generate <- function(prompt, backend, num_ctx = NULL, image = NULL) {
  t0 <- Sys.time()
  opts <- list(num_predict = 1L, temperature = 0)
  if (!is.null(num_ctx) && !is.na(num_ctx)) {
    opts$num_ctx <- as.integer(num_ctx)
  }
  body <- list(
    model = backend$model,
    prompt = prompt,
    think = backend$think,
    stream = FALSE,
    logprobs = TRUE,
    top_logprobs = backend$top_logprobs,
    keep_alive = backend$keep_alive,
    options = opts
  )
  # Added only when there is media, so a text run sends the body it always sent.
  # `image` is a character vector, one path per picture, and a video window is
  # several. A list rather than a vector, because auto_unbox would send a
  # length-one vector as a scalar where the API wants an array of one.
  if (!is.null(image)) {
    keep <- !is.na(image) & nzchar(image)
    if (any(keep)) {
      body$images <- lapply(image[keep], image_b64)
    }
  }
  resp <- httr2::request(backend_url(backend)) %>%
    httr2::req_body_json(body, auto_unbox = TRUE) %>%
    httr2::req_timeout(backend$timeout) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  resp$wall_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  resp
}

#' Check a backend before a long run, internal
#'
#' @description
#' Confirms the server answers, the model is present, log probabilities come
#' back in the expected shape, and generated position 1 is a label token rather
#' than something structural.
#'
#' If a chat template opens a reasoning block despite `think = FALSE`, position
#' 1 is a structural token, no label appears in the top-k, and every probability
#' comes back as zero without an error being raised.
#'
#' Called automatically by [measure()] before a corpus is started, which is
#' what makes it internal. That call passes the real instrument and a real
#' document, where a hand call without both probes a question the package
#' invented, so the automatic check is the stronger of the two and there is
#' nothing left for the manual one to buy.
#'
#' @section Model presence:
#' [ollama_backend()] confirms the model against the server when `validate` is
#' left at its default. Two paths reach here without that check. One is a
#' backend built with `validate = FALSE`. The other is a backend restored from a
#' checkpoint by [read_checkpoints()], which may have been written by an older
#' version.
#'
#' @section Two modes:
#' With `text`, the probe is your document, your instrument's first item, and
#' your cue. That is the prompt the run is about to send, so nothing about it
#' is artificial and the check is exact. [measure()] takes this path, passing
#' the document of median length.
#'
#' Without `text`, the instrument is ignored and a built-in `YES`/`NO` question
#' is asked of a built-in sentence. What that tests is whether the chat
#' template puts an answer at generated position 1, which is a property of the
#' template and not of your labels. A model opening a reasoning block does so
#' whatever it is asked. Pairing your instrument with a document it was not
#' written for would test nothing extra and could refuse a working setup, so
#' the two are never mixed. Use [preview()] to read your own assembled prompt,
#' which needs no call at all.
#'
#' @param backend An [ollama_backend()] object.
#' @param num_ctx An integer giving the context ceiling for the probe call.
#'   `NULL` omits it. Passed through so the reported configuration matches what
#'   will run.
#' @param instrument A `fathom_instrument`, or `NULL`. Used only when `text` is
#'   also supplied. See Two modes.
#' @param text A string giving a single document to probe with, or `NULL`.
#'   Supplying it switches the check from the generic probe to your own prompt.
#' @param images A logical value indicating whether to check that images reach
#'   the model, at the cost of two extra calls. Set by [measure()] when an
#'   `image` column was named. A backend not declaring `image` skips it whatever
#'   this says.
#' @param report An environment to write what the probe read into, or `NULL`.
#'   [measure()] reads it for its header. A passing check prints nothing,
#'   and a failing one prints its note either way.
#' @return A single logical, and the report it prints on the way is the point
#'   rather than a side effect.
#'
#'   `TRUE` means the server answered, the model is installed, log
#'   probabilities came back, and the first generated token was a label rather
#'   than the opening of a reasoning block or a preamble.
#'
#'   `FALSE` covers nine distinct failures, each named in the message rather
#'   than collapsed into one. They are listed in the order they are tested, so
#'   the first costs nothing and the rest reach the network in turn.
#'   \describe{
#'     \item{Exact scoring with no discovered tokens}{The instrument was built
#'     with `exact = TRUE` and its label probe did not come back, so scoring
#'     compares against the label strings as written. Any label the tokenizer
#'     splits then scores zero on every document. Read from the instrument
#'     rather than probed for, so this is settled before the first call.}
#'     \item{Host unreachable}{No answer from `backend$host`.}
#'     \item{No models installed}{The server answered and has nothing to run.}
#'     \item{Model not installed}{The tag in `backend$model` is not among the
#'     installed ones, which are listed.}
#'     \item{Probe call failed}{The request reached the server and errored.}
#'     \item{No log probabilities}{Ollama returned text but no distribution,
#'     which means a version below 0.12.11.}
#'     \item{First token is not a label}{The answer is not at position 1, which
#'     is what a chat template opening a reasoning block does, and what a model
#'     opening with prose does. Left unchecked this returns zeros for an entire
#'     corpus with no error anywhere.}
#'     \item{Image probe call failed}{One of the two image calls did not come
#'     back, so whether pictures reach the model is unknown rather than known
#'     to be broken. Distinguished from the text probe failing because the
#'     remedy differs, and a vision model may be absent where a text one is
#'     not.}
#'     \item{Images are accepted and ignored}{Two calls carry the same question
#'     with different pictures, and identical distributions mean the image
#'     never reached the model. Ollama returns HTTP 200 for a body carrying a
#'     field it does not recognize, so a declaration alone proves nothing. Both
#'     run only when `images = TRUE` and the backend declares `image`, and cost
#'     two calls between them.}
#'   }
#'
#'   The first-token failure is the reason to run this before a corpus rather
#'   than after. Every other failure stops a run visibly. That one does not. A
#'   model writing a reasoning token at position 1 still returns a full results
#'   frame, because the labels sit further down the same top-k at trace mass
#'   and `prob_renorm` divides that remainder by itself into something that
#'   reads as an answer. Matching is therefore on the token at position 1
#'   rather than on whether a label appeared anywhere in the distribution,
#'   which a `prob_other` below 1 reports and which is true of a broken template
#'   too.
#'
#'   Exclusive instruments are checked like any other. They were skipped while
#'   the question was generic, because sorting an unrelated sentence into a
#'   user's categories had no answer. With `text` the question is the
#'   instrument's own, and without it the instrument is not used, so neither
#'   mode reaches that problem.
#' @keywords internal
#' @noRd
check_backend <- function(
  backend,
  instrument = NULL,
  num_ctx = NULL,
  text = NULL,
  images = FALSE,
  report = NULL
) {
  stopifnot(inherits(backend, "fathom_backend"))

  # Free, so it runs before anything reaches the network. `exact_match` is set
  # only by `exact = TRUE`, which instrument() refuses without a backend, and
  # `firsts` is written only where probe_labels() got every label back. The
  # pair therefore identifies one state and not a legitimate configuration.
  # The warning at construction already said what it costs. This is where the
  # cost is refused, because it is the same silent failure the position-1 check
  # exists for, a full results frame with no error and zeros for a label.
  if (
    !is.null(instrument) &&
      isTRUE(instrument$exact_match) &&
      is.null(instrument$firsts)
  ) {
    message(fmt_note(
      "Warning",
      paste0(
        "This instrument scores by exact match and carries no discovered ",
        "tokens, so its label probe did not come back."
      ),
      "Any label the tokenizer splits scores zero on every document.",
      paste0(
        "Rebuild it with exact = TRUE and a backend once the server answers, ",
        "or drop exact and let prefix matching handle the split."
      )
    ))
    return(FALSE)
  }

  tags <- ollama_tags(backend$host)
  if (inherits(tags, "fathom_tags_failure")) {
    message(fmt_note(
      "Warning",
      paste0(
        "Cannot reach ",
        backend$host,
        ". ",
        tags$message
      )
    ))
    return(FALSE)
  }
  if (nrow(tags) == 0L) {
    message(fmt_note(
      "Warning",
      paste0(backend$host, " has no models installed.")
    ))
    return(FALSE)
  }

  if (!backend$model %in% tags$name) {
    message(fmt_note(
      "Warning",
      paste0(
        "Model '",
        backend$model,
        "' is not an installed tag. Installed tags ",
        "are ",
        paste(tags$name, collapse = ", ")
      )
    ))
    return(FALSE)
  }

  # Two modes, never mixed. With a document the instrument is used whole, so the
  # matching rule, the variants probing discovered, and the exemption a
  # colliding label set was granted by `exact = TRUE` all survive, none of which
  # a rebuild from parts would carry. Without one the instrument is not used at
  # all, because a question written for a corpus has no answer about a sentence
  # invented here, and a refusal on that pairing would block a working run.
  use_own <- !is.null(text) && !is.null(instrument)
  if (use_own) {
    b <- instrument
    doc <- as.character(text)[1]
  } else {
    b <- instrument(
      list(probe = "Is the text about weather?"),
      labels = c("YES", "NO")
    )
    doc <- "It rained heavily all afternoon."
  }
  prompt <- build_prompt(doc, b$items[[1]], b)

  probe <- tryCatch(
    ollama_generate(prompt, backend, num_ctx),
    error = function(e) e
  )
  if (inherits(probe, "error")) {
    message(fmt_note(
      "Warning",
      paste0("Probe call failed. ", conditionMessage(probe))
    ))
    return(FALSE)
  }
  # Read the way measure_one() reads it. `%||%` never fires here, because the
  # shapes it would have to catch all raise before returning. An empty list, or
  # the field absent, makes `[[1]]` a subscript error rather than NULL, so the
  # documented "No log probabilities" failure was reachable only when Ollama
  # returned a one-element list whose element lacked the field. Any other shape
  # threw a bare subscript error out of a function contracted to return a
  # logical.
  lp <- tryCatch(probe$logprobs[[1]]$top_logprobs, error = function(e) NULL)
  if (is.null(lp) || length(lp) == 0L) {
    message(fmt_note(
      "Warning",
      "No log probabilities returned. Ollama >= 0.12.11 required."
    ))
    return(FALSE)
  }

  top1 <- stringi::stri_trans_toupper(trimws(lp[[1]]$token %||% ""))

  # Asked the way the run will ask it, so an instrument matching by prefix
  # counts a fragment here too. Checking membership in the variant list would
  # flag BU as unrecognized on an instrument that scores it as BUDGET.
  hits <- vapply(
    seq_along(b$normalized),
    function(i) any(match_token(top1, b, i)),
    logical(1)
  )

  if (!any(hits)) {
    message(fmt_note(
      "Warning",
      paste0(
        "Position 1 is '",
        top1,
        "', which counts as none of ",
        paste(names(b$labels), collapse = "/"),
        "."
      ),
      "Three known causes.",
      "1. The cue does not name the labels clearly enough. Run preview() and read the assembled prompt.",
      "2. The chat template opened a reasoning block despite think = FALSE. The run would return a full results frame built from trace mass, with no error.",
      "3. The model opens with prose, a word such as The or Okay before its answer. Worked examples in the question, each a short document and its one-word answer, have fixed this for at least one model. See ?measure."
    ))
    return(FALSE)
  }

  # ---- eighth, and only when images are in play ----------------------------
  # A declaration is not evidence. Ollama returned HTTP 200 for a body carrying
  # a field named `zzzz`, so an ignored `images` array looks exactly like an
  # honored one from the outside. Two pictures, one question. If the answer is
  # the same distribution both times the image is not reaching the model, and a
  # corpus run would return a full results frame built from the prompt alone.
  if (isTRUE(images) && "image" %in% (backend$modalities %||% "text")) {
    # A path, because that is what ollama_generate() takes. Returning base64
    # here made it encode an already-encoded string, and file.exists() then
    # reported the probe as having failed rather than as having been misused.
    #
    # Written once, up front, and removed when check_backend() returns rather
    # than when the helper does, since the file has to outlive the call that
    # reads it.
    swatch <- function(col) {
      p <- tempfile(fileext = ".png")
      grDevices::png(p, width = 64, height = 64)
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::rect(0, 0, 1, 1, col = col, border = NA)
      grDevices::dev.off()
      p
    }
    pics <- vapply(c("red", "blue"), swatch, character(1))
    on.exit(unlink(pics), add = TRUE)

    ask <- build_prompt(
      "",
      "Is this image mostly red?",
      instrument(list(q = "Is this image mostly red?"), labels = c("YES", "NO"))
    )
    two <- lapply(pics, function(p) {
      r <- tryCatch(
        ollama_generate(ask, backend, num_ctx, image = p),
        error = function(e) e
      )
      if (inherits(r, "error")) NULL else r$logprobs[[1]]$top_logprobs
    })
    if (any(vapply(two, is.null, logical(1)))) {
      message(fmt_note(
        "Warning",
        "The image probe call failed, so whether images reach the model is unknown.",
        paste0(
          "Check that the model takes images, which ollama_backend() lists ",
          "when called without a model, or run measure() without `image`."
        )
      ))
      return(FALSE)
    }
    if (identical(two[[1]], two[[2]])) {
      message(fmt_note(
        "Warning",
        "Two different images produced an identical distribution, so the image is not reaching the model.",
        "The server accepts an unknown field without complaint, so a run would return a full results frame built from the prompt alone.",
        "Check the model is a vision model and that this Ollama version takes the `images` array."
      ))
      return(FALSE)
    }
  }

  if (!is.null(report)) {
    report$top1 <- top1
    report$n_logprobs <- length(lp)
  }
  TRUE
}
