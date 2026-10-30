#' Word count without a tokenizer
#'
#' @param x A character vector.
#' @return An integer vector of word counts, one per element, splitting on
#'   whitespace.
#' @keywords internal
#' @noRd
n_words <- function(x) {
  vapply(strsplit(trimws(x), "\\s+"), function(z) sum(nzchar(z)), integer(1))
}

#' Wrap a console message to a fixed width with a hanging indent
#'
#' Each argument is wrapped as its own block, so a headline and the paragraphs
#' under it keep their separation. The first line carries `prefix` and every
#' later line is indented to sit under it.
#'
#' Only for prose. Blocks that rely on internal spacing to align, such as the
#' num_ctx report or the model listing, must not go through this, since
#' `strwrap()` collapses runs of whitespace.
#'
#' @param ... One or more strings, each wrapped separately.
#' @param prefix A string prepended to the first line, empty by default.
#' @param indent A string prepended to every line after the first. Defaults to
#'   enough spaces to align under `prefix`.
#' @param width An integer giving the target column, counting the prefix.
#' @return A single string with embedded newlines.
#' @keywords internal
#' @noRd
fmt_lines <- function(..., prefix = "", indent = NULL, width = 80L) {
  parts <- c(...)
  cont <- indent %||% strrep(" ", nchar(prefix))
  out <- character(0)
  for (k in seq_along(parts)) {
    # A numbered step hangs its wrapped lines under its own text rather than
    # under its number.
    num <- regmatches(parts[[k]], regexpr("^[0-9]+\\. ", parts[[k]]))
    hang <- if (length(num)) strrep(" ", nchar(num)) else ""
    out <- c(
      out,
      strwrap(
        parts[[k]],
        width = width,
        prefix = paste0(cont, hang),
        initial = if (k == 1L) prefix else cont
      )
    )
  }
  paste(out, collapse = "\n")
}

#' Wrap the text of a stop() call
#'
#' R prefixes an error with `Error : `, which is eight characters, and prints
#' embedded newlines verbatim rather than rewrapping. Reserving those eight
#' columns on every line and stripping them from the first leaves the whole
#' message aligned under its opening text.
#'
#' @inheritParams fmt_lines
#' @return A single string with embedded newlines, every line after the first
#'   indented to sit under the first once R adds its prefix.
#' @keywords internal
#' @noRd
fmt_err <- function(..., width = 80L) {
  pad <- strrep(" ", 8L)
  sub(
    paste0("^", pad),
    "",
    fmt_lines(..., prefix = pad, indent = pad, width = width)
  )
}

#' Wrap the text of a warning() call
#'
#' A flat block with no hanging indent. R prints "Warning message:" on a line of
#' its own and puts the text underneath, so there is no prefix to align under
#' and an indent only makes the first line look out of place. Under
#' `options(warn = 1)` R prints "Warning: " inline instead, and a flat block
#' still reads.
#'
#' @inheritParams fmt_lines
#' @return A single string with embedded newlines and no indent.
#' @keywords internal
#' @noRd
fmt_warn <- function(..., width = 78L) {
  fmt_lines(..., prefix = "", indent = "", width = width)
}

# Wide enough for the longest field name a header uses and three spaces, so
# every value starts in the same column.
FIELD_WIDTH <- 14L

# The run header's own column, wider than FIELD_WIDTH. Its longest name,
# Context Ceiling, keeps five spaces before its value, and every other value in
# the block starts in the same column.
HEADER_WIDTH <- 20L

# The cells in measure()'s progress bar, against 24 by default. The bar starts
# at HEADER_WIDTH rather than after its verb, so it is shorter by the same nine
# columns, and the line keeps the length it had inside an 80-column console.
BAR_CELLS <- 15L

#' Format a block of named fields for the console
#'
#' Each name sits in a column of its own and its value beside it. A value that
#' fits on its line is kept as written, inner spacing included, and a longer
#' one wraps with every later line hanging under the value. A character vector
#' puts each element on a line of its own in the value column. A field whose
#' value is `NULL` is left out, so a header can list a field that applies only
#' sometimes.
#'
#' @param fields A named list of strings or character vectors, one entry per
#'   field, in display order.
#' @param width An integer giving the target column, counting the name.
#' @param name_width An integer giving the width of the name column. Default is
#'   `FIELD_WIDTH`. Wider for a table whose names are data, such as labels.
#' @return A single string with embedded newlines.
#' @keywords internal
#' @noRd
fmt_fields <- function(fields, width = 80L, name_width = FIELD_WIDTH) {
  fields <- Filter(Negate(is.null), fields)
  pad <- strrep(" ", name_width)
  one <- function(lead, value) {
    if (nchar(lead) + nchar(value) < width) {
      return(paste0(lead, value))
    }
    strwrap(value, width = width, initial = lead, prefix = pad)
  }
  lines <- unlist(
    Map(
      function(name, value) {
        lead <- c(
          formatC(name, width = -name_width),
          rep(pad, length(value) - 1L)
        )
        unlist(Map(one, lead, value), use.names = FALSE)
      },
      names(fields),
      fields
    ),
    use.names = FALSE
  )
  paste(lines, collapse = "\n")
}

#' Write a labeled note to standard output
#'
#' The print methods write with `cat()`, so a note inside a printed object goes
#' the same way rather than through `message()`.
#'
#' @param label,... As in `fmt_note()`.
#' @return `NULL`, invisibly. Called for the output.
#' @keywords internal
#' @noRd
cat_note <- function(label, ...) {
  cat(fmt_note(label, ...), "\n", sep = "")
  invisible(NULL)
}

#' Print a line of progress
#'
#' A message like any other, so `suppressMessages()` still silences it, with a
#' class of its own on top, so a caller running another function inside its
#' own progress can drop that function's lines and keep its notes.
#'
#' @param text A string, the line to print.
#' @return `NULL`, invisibly. Called for the message.
#' @keywords internal
#' @noRd
progress_message <- function(text) {
  message(structure(
    class = c("fathom_progress", "message", "condition"),
    list(message = paste0(text, "\n"), call = NULL)
  ))
}

#' Name the first few of a set and count the rest
#'
#' @param x A character vector of names.
#' @param max An integer giving how many to name before counting the rest.
#' @return A string, the names joined with commas, ending in a count of the rest
#'   when there are more than `max`.
#' @keywords internal
#' @noRd
fmt_names <- function(x, max = 6L) {
  if (length(x) <= max) {
    return(paste(x, collapse = ", "))
  }
  sprintf(
    "%s, and %d more",
    paste(x[seq_len(max)], collapse = ", "),
    length(x) - max
  )
}

# Wide enough for "Warning" and two spaces, so the text of every label starts in
# the same column.
NOTE_WIDTH <- 9L

#' Format a labeled note for the console
#'
#' The label sits in a column of its own and every wrapped line hangs under the
#' text beside it, with a blank line above so consecutive notes stay apart. A
#' `"Note"` reports something worth knowing about what happened, a `"Warning"`
#' a problem to fix before trusting the results, and a `"Tip"` a next step. A
#' `"Warning"` here is a message, shown when it happens. A condition a caller
#' may need to catch goes through `warning()` instead.
#'
#' @param label A string naming the kind of note, `"Note"` (the default),
#'   `"Warning"` or `"Tip"`.
#' @param ... Strings, one per paragraph, as in `fmt_lines()`.
#' @param width An integer giving the target column, counting the label.
#' @return A single string with embedded newlines, opening with a blank line.
#' @keywords internal
#' @noRd
fmt_note <- function(label = c("Note", "Warning", "Tip"), ..., width = 80L) {
  label <- match.arg(label)
  paste0(
    "\n",
    fmt_lines(
      ...,
      prefix = formatC(label, width = -NOTE_WIDTH),
      indent = strrep(" ", NOTE_WIDTH),
      width = width
    )
  )
}

#' Format a duration in whichever unit reads at that scale
#'
#' Seconds below a minute, minutes below an hour, hours above. Used for elapsed
#' time, remaining time, and per-document rates, which span several orders of
#' magnitude between a ten-document trial and a corpus run.
#'
#' @param seconds A duration in seconds.
#' @return A single string, including its unit.
#' @keywords internal
#' @noRd
fmt_dur <- function(seconds) {
  if (!is.finite(seconds)) {
    return("?")
  }
  if (seconds < 60) {
    return(sprintf("%.1f s", seconds))
  }
  if (seconds < 3600) {
    return(sprintf("%.1f min", seconds / 60))
  }
  sprintf("%.1f h", seconds / 3600)
}

#' Resolve a text source to a data frame of id and text
#'
#' Audit functions need the documents. Results do not carry them in the general
#' case, since the long format repeats every row-level column once per
#' item-label pair.
#'
#' `measure()` given a character vector stashes it on the results. This reads
#' that stash when `docs` is not supplied. An explicit `docs` argument always
#' wins.
#'
#' @param docs A character vector, a data frame, or NULL to use the stash.
#' @param text,id Strings naming the text and identifier columns, if `docs` is a
#'   data frame.
#' @param results A results object carrying a `docs` attribute, which only the
#'   character method sets, or `NULL`.
#' @return A tibble with `id` and `.fathom_text`. The text column is namespaced
#'   because the audit functions join this onto results, and a results column
#'   named `text`, carried through `keep`, would otherwise suffix both sides and
#'   leave the read returning `NULL`.
#' @keywords internal
#' @noRd
as_docs <- function(docs, text = NULL, id = NULL, results = NULL) {
  if (is.null(docs) && !is.null(results)) {
    stash <- attr(results, "docs")
    if (is.null(stash)) {
      stop(
        fmt_err(
          paste0(
            "`docs` is required here. The documents are not stored on the ",
            "results because measure() was given a data frame. Pass that ",
            "same data frame and name its text column, e.g."
          ),
          "inspect_cases(res, \"item\", docs, text = \"body\")"
        ),
        call. = FALSE
      )
    }
    return(tibble::tibble(
      id = names(stash) %||% as.character(seq_along(stash)),
      .fathom_text = as.character(stash)
    ))
  }

  if (is.character(docs)) {
    return(tibble::tibble(
      id = names(docs) %||% as.character(seq_along(docs)),
      .fathom_text = as.character(docs)
    ))
  }
  if (is.data.frame(docs)) {
    if (is.null(text) || !text %in% names(docs)) {
      stop("`text` must name the text column in `docs`", call. = FALSE)
    }
    return(tibble::tibble(
      id = if (is.null(id)) {
        as.character(seq_len(nrow(docs)))
      } else {
        as.character(docs[[id]])
      },
      .fathom_text = as.character(docs[[text]])
    ))
  }
  stop("`docs` must be a character vector or a data frame", call. = FALSE)
}

#' Stamp an object with what produced it
#'
#' `fathom_results` already records this at the point of measurement. The
#' validation objects carry the numbers that reach a paper and record nothing,
#' so a figure queried a year later cannot be traced to the call that made it.
#'
#' The package version is not among what is recorded, here or anywhere else.
#' What decides a number is the model, the instrument and the call, and the
#' first is stamped on the results while the last two are stamped here.
#'
#' @param x An object of any class.
#' @param call A call object giving the call that produced it, or `NULL`.
#' @return `x`, with `run_at` and, when given, `call` attached.
#' @keywords internal
#' @noRd
stamp_provenance <- function(x, call = NULL) {
  attr(x, "run_at") <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  if (!is.null(call)) {
    attr(x, "call") <- call
  }
  x
}

#' One row's media, as a list of character vectors
#'
#' A corpus of single pictures is a character column. A corpus of video windows
#' is a list column, one element per document holding that document's frames in
#' order. Both arrive here and leave in one shape, so nothing downstream has to
#' ask which it was handed.
#'
#' `as.character()` on a list column pastes each element into one string and
#' every path after the first is lost, so the branch is not decoration.
#'
#' @param x A character vector, or a list of character vectors.
#' @return A list of character vectors, one per document.
#' @keywords internal
#' @noRd
as_media <- function(x) {
  if (is.list(x)) {
    return(lapply(x, as.character))
  }
  lapply(as.character(x), identity)
}

#' The winning label of each call, or none
#'
#' The row holding the most `prob` within each document and item. A call whose
#' distribution held none of the labels has every `prob` at zero, and
#' slice_max() would hand the win to whichever label it met first, so such a
#' call keeps no row here. score_labels() writes `NA` for the same case, for
#' the same reason.
#'
#' The test is on `prob` rather than `present`, since a label is present
#' exactly when its `prob` is above zero, and a frame built by hand may carry
#' `prob` alone.
#'
#' @param x A data frame of label rows carrying `id`, `item`, `label` and
#'   `prob`.
#' @return The winning row of each call that had one.
#' @keywords internal
#' @noRd
winning_rows <- function(x) {
  x %>%
    dplyr::group_by(.data$id, .data$item) %>%
    dplyr::filter(any(.data$prob > 0)) %>%
    dplyr::slice_max(.data$prob, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()
}

#' Refuse a coverage that is not a probability
#'
#' @param conf_level A number giving the coverage an interval was asked for.
#' @return `conf_level`, invisibly.
#' @keywords internal
#' @noRd
check_conf_level <- function(conf_level) {
  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    stop(
      fmt_err(
        "`conf_level` must be a single number between 0 and 1, such as 0.95."
      ),
      call. = FALSE
    )
  }
  invisible(conf_level)
}

#' Refuse a target that names no label
#'
#' A `target` matching no label filters every row away, so a report came back
#' empty and a sweep said there was nothing to threshold, and neither pointed
#' at the argument. Matched exactly, since the labels are the strings the
#' instrument declared.
#'
#' @param target A string naming the label counted as endorsement.
#' @param labels A character vector of the labels the results carry.
#' @return `target`, invisibly.
#' @keywords internal
#' @noRd
check_target <- function(target, labels) {
  if (!is.character(target) || length(target) != 1L || !target %in% labels) {
    stop(
      fmt_err(paste0(
        "target '",
        paste(target, collapse = ", "),
        "' is not a label. Available labels are ",
        paste(labels, collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }
  invisible(target)
}

#' Seed the random stream for the calling function, then put it back
#'
#' set.seed() changes the stream for everything the session runs afterwards,
#' so a seed handed to one call would decide the draws of the next. This seeds
#' for the length of the caller and, when the caller exits, restores the stream
#' the session had, or removes the one this created if there was none before.
#' `NULL` leaves the stream alone, so the call draws from it like any other.
#'
#' @param seed A single number, or `NULL`.
#' @param env An environment giving the frame whose exit restores the stream,
#'   the caller's by default.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
local_seed <- function(seed, env = parent.frame()) {
  if (is.null(seed)) {
    return(invisible(NULL))
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    stop(
      fmt_err(paste0(
        "`seed` must be a single number, or NULL to leave R's random stream ",
        "alone."
      )),
      call. = FALSE
    )
  }
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  restore <- function() {
    if (had) {
      assign(".Random.seed", old, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }
  # Registered on the caller's frame, since on.exit() called here would fire
  # when this function returns. The function object itself goes into the call,
  # so nothing has to be found by name in the caller's frame.
  do.call(on.exit, list(as.call(list(restore)), add = TRUE), envir = env)
  set.seed(seed)
  invisible(NULL)
}

#' Refuse anything that reached `...`
#'
#' Every argument after `...` in this package is name-only, which is what makes
#' their order a documentation choice rather than part of the API. The cost is
#' that a misspelled name would land in the dots and be swallowed, where before
#' it was an unused-argument error. So the dots are read and refused.
#'
#' @param ... The caller's dots.
#' @param .fn A string naming the calling function, for the message.
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
no_dots <- function(..., .fn) {
  d <- list(...)
  if (!length(d)) {
    return(invisible(NULL))
  }
  nm <- names(d) %||% rep("", length(d))
  named <- nm[nzchar(nm)]
  unnamed <- sum(!nzchar(nm))
  detail <- paste(
    c(
      if (length(named)) {
        paste0("Named: ", paste(named, collapse = ", "), ".")
      },
      if (unnamed) sprintf("Unnamed: %d.", unnamed)
    ),
    collapse = " "
  )
  stop(
    fmt_err(
      sprintf("`%s()` was given arguments it does not have.", .fn),
      detail,
      paste0(
        "Everything past the first few arguments is name-only, so a ",
        "misspelling lands here rather than being matched by position. ",
        "Check the spelling against ?",
        .fn,
        "."
      )
    ),
    call. = FALSE
  )
}

#' Reassemble the two frames for a function that needs both grains
#'
#' `measure()` returns `$probs` at one row per label and `$calls` at one row per
#' document and item. Most consumers work across both, so they join here rather
#' than each writing the join. `cols` names what is wanted from `$calls`, so a
#' function that needs the document does not also carry the error and the
#' prompt hash down every label row.
#'
#' Attributes are carried across, since `item_report()` and `as_wide()` read the
#' instrument and the exclusive flag off the object they were handed.
#'
#' @param x A `fathom_results`, or a plain frame from before the split.
#' @param cols A character vector of columns to take from `$calls`. `NULL` takes
#'   all of them.
#' @return A tibble at one row per document, item and label.
#' @keywords internal
#' @noRd
flat_results <- function(x, cols = NULL) {
  if (!is.list(x) || !all(c("probs", "calls") %in% names(x))) {
    return(x)
  }
  keys <- c("id", "item")
  take <- if (is.null(cols)) {
    names(x$calls)
  } else {
    intersect(union(keys, cols), names(x$calls))
  }
  out <- dplyr::left_join(
    x$probs,
    x$calls[, union(keys, take), drop = FALSE],
    by = keys
  )
  for (a in c(
    "instrument",
    "backend",
    "model",
    "exclusive",
    "num_ctx",
    "run_at",
    "docs"
  )) {
    if (!is.null(attr(x, a))) {
      attr(out, a) <- attr(x, a)
    }
  }
  out
}

#' Shannon entropy of a probability vector
#'
#' The spread of an answer across the labels, in nats. Zero when one label holds
#' everything. Carried as a column until the results were split, and recomputed
#' where it is read, since it is a function of `prob_renorm` and nothing else.
#'
#' @param p A numeric vector of probabilities. `NA` and zeros are dropped, so `0
#'   * log(0)` is 0.
#' @return One number, or `NA_real_` when nothing is left to spread.
#' @keywords internal
#' @noRd
shannon <- function(p) {
  nz <- p[!is.na(p) & p > 0]
  if (!length(nz)) {
    return(NA_real_)
  }
  -sum(nz * log(nz))
}
