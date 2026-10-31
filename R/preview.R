#' Print the assembled prompt
#'
#' @description
#' Prints the exact string the model will receive, before Ollama applies the
#' model's chat template. Makes no model calls. Prompt failures do not appear in
#' the output of [measure()].
#'
#' @section The matching table:
#' The table underneath has two columns. On the left the label, on the right
#' every token that will be counted as that label. They read the same when a
#' label is its own only variant, which is the usual arrangement. They diverge
#' once accented forms are supplied, and again after probing registers whatever
#' token a split label actually emits.
#'
#' Under prefix matching the right column is not the whole story, since a token
#' also counts when it merely opens one of those forms. `BU` counts as BUDGET
#' without appearing there. The instrument header says which rule is in force.
#'
#' @param instrument An [instrument()].
#' @param text An example document. A short placeholder by default.
#' @param image A character vector of paths, one or several, to the media the
#'   run would send, or `NULL` (the default) to send none. A video window is a
#'   vector of frame paths. Their presence removes the document block from the
#'   prompt, since an image corpus has no text to quote, and adds a bracketed
#'   line above it. That line is shown and never sent. Anything in square
#'   brackets here describes what travels beside the prompt rather than quoting
#'   what is inside it.
#' @param item A string naming the item to show. Defaults to the first.
#' @param truncate An integer giving the number of characters of document text
#'   to show. `NULL` shows all. The returned string always carries the whole
#'   document. Default is 300.
#' @return Invisibly, the assembled prompt string for the item shown, exactly as
#'   [measure()] sends it, before Ollama applies the model's chat template. It
#'   carries the whole document and leaves out the bracketed media line. A
#'   single character scalar.
#'
#'   The printed output and the return value differ, and the difference
#'   matters. Printing trims a long document at `truncate` and shows, beneath
#'   the prompt, the table of what each label will be matched against. That
#'   table is where most extraction failures are visible, and it is not in the
#'   returned string.
#'
#'   So capture the value to diff two prompts or to paste one into a paper, and
#'   read the printed output to check the instrument. No call is made either
#'   way.
#' @seealso [instrument()] for the prompt's parts and [measure()] for sending
#'   it.
#' @examples
#' b <- instrument(
#'   template_items("topic", c(econ = "the economy", immig = "immigration"),
#'                  medium = "text"),
#'   labels = c("YES", "NO")
#' )
#' preview(b)
#' @export
preview <- function(
  instrument,
  text = "[document text goes here]",
  item = NULL,
  image = NULL,
  truncate = 300L
) {
  stopifnot(inherits(instrument, "fathom_instrument"))

  item <- item %||% names(instrument$items)[1]
  if (!item %in% names(instrument$items)) {
    stop(
      fmt_err(paste0(
        "item '",
        item,
        "' is not in the instrument. Available items are ",
        paste(names(instrument$items), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }

  # An image corpus has no text, so the caller says so by naming a picture and
  # nothing else. The document block is then absent from the prompt, which is
  # what build_prompt() produces and what the model receives.
  if (!is.null(image) && identical(text, "[document text goes here]")) {
    text <- ""
  }
  shown <- text
  if (!is.null(truncate) && nchar(shown) > truncate) {
    shown <- paste0(substr(shown, 1, truncate), " [... truncated for display]")
  }
  p <- build_prompt(shown, instrument$items[[item]], instrument)
  # What measure() sends, built from the whole document. The trim above and the
  # bracketed line below are for the screen, so the returned string carries
  # neither.
  sent <- build_prompt(text, instrument$items[[item]], instrument)

  # Shown, never sent. Square brackets mark it as this function describing what
  # travels beside the prompt rather than quoting what is in it, the way the
  # scoring block below describes rather than quotes. A picture in the payload
  # is not a sentence about a picture in the prompt, and writing one there would
  # spend tokens telling the model what its own embeddings already carry.
  if (!is.null(image)) {
    sz <- suppressWarnings(file.size(image))
    gone <- sum(is.na(sz))
    what <- if (all(is.na(sz))) {
      if (length(sz) == 1L) "file not found" else "no files found"
    } else {
      paste0(
        format(sum(sz, na.rm = TRUE), big.mark = ","),
        " bytes",
        if (gone) sprintf(", %d not found", gone) else ""
      )
    }
    label <- if (length(image) == 1L) {
      "image"
    } else {
      sprintf("%d images", length(image))
    }
    p <- paste0(
      sprintf("[%s: %s, sent alongside the prompt]\n\n", label, what),
      p
    )
  }

  split <- if (!is.null(instrument$firsts)) {
    stringi::stri_trans_toupper(instrument$firsts) !=
      stringi::stri_trans_toupper(names(instrument$firsts))
  } else {
    FALSE
  }
  # The rules mark where the string the model receives starts and ends, blank
  # lines included, so they sit around the prompt and nowhere else.
  rule <- strrep("-", 70)
  cat(
    "Prompt preview\n\n",
    fmt_fields(list(
      Item = if (isTRUE(instrument$exclusive)) {
        sprintf("%s, exclusive, one answer from %d", item, instrument$k)
      } else {
        sprintf(
          "%s, %d of %d",
          item,
          match(item, names(instrument$items)),
          instrument$n_items
        )
      },
      Scoring = "generated position 1",
      Scores = if (!is.null(instrument$scores)) {
        paste(
          sprintf("%s %g", names(instrument$scores), instrument$scores),
          collapse = ", "
        )
      },
      Probed = if (any(split)) {
        paste(
          sprintf(
            "%s emits %s",
            names(instrument$firsts)[split],
            instrument$firsts[split]
          ),
          collapse = ", "
        )
      }
    )),
    "\n\n",
    rule,
    "\n",
    p,
    "\n",
    rule,
    "\n\n",
    sep = ""
  )

  # Headed, so the identity case reads as a label matching itself rather than
  # as the same string printed twice, which is the commonest arrangement and
  # the one worth confirming. The name column is as wide as the longest label.
  labs <- names(instrument$labels)
  cat(
    fmt_fields(
      c(
        list(Label = "Counts as"),
        stats::setNames(
          lapply(labs, function(nm) {
            paste(instrument$normalized[[nm]], collapse = ", ")
          }),
          labs
        )
      ),
      name_width = max(FIELD_WIDTH, max(nchar(labs)) + 2L)
    ),
    "\n",
    sep = ""
  )

  if (any(split)) {
    cat_note(
      "Note",
      "P(first token) is an upper bound on P(label). Watch prob_other."
    )
  } else if (
    is.null(instrument$firsts) &&
      isTRUE(instrument$exact_match) &&
      isTRUE(instrument$exclusive)
  ) {
    # Only reachable when exact matching is in force and probing did not run,
    # which now means it failed rather than that it was skipped. Under prefix
    # matching there is nothing to warn about. A split label is matched by the
    # fragment it emits, and the instrument says which rule is in force.
    cat_note(
      "Warning",
      paste(
        "Not probed. Exact matching scores against discovered variants, and",
        "none were found, so a split label will score zero."
      )
    )
  }

  invisible(sent)
}
