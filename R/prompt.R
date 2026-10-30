#' Assemble a classification prompt
#'
#' @description
#' Concatenates the document, the question, the instrument's answer cue, and the
#' response marker.
#'
#' @section Prompt Ordering:
#' The document comes first and the question second. Ollama matches its KV cache
#' by longest common prefix, so the document is prefilled once and later items
#' for that document pay only their own question tokens.
#'
#' @section No text means no block:
#' An image corpus has nothing to quote, and `Text: ""` is not the same claim as
#' saying nothing. It asserts a document that is empty rather than a document
#' that is elsewhere, and it spends the tokens to do so on every call. The
#' picture rides in its own field and the model has its embeddings, so the block
#' is omitted entirely.
#'
#' No placeholder goes in its place. A marker would tell the model nothing it
#' does not already have, and a file name would be worse, since
#' `angry_protest_042.png` in the prompt is the name answering the question
#' instead of the picture and nothing in the results would show it.
#'
#' `preview()` prints a bracketed line where the document would sit. That line
#' is shown and never sent.
#'
#' @param text A string giving the document text, or `""` for a corpus that has
#'   none.
#' @param question A string giving the item question, without the answer cue.
#' @param instrument A `fathom_instrument`.
#' @return Character scalar.
#' @keywords internal
#' @noRd
build_prompt <- function(text, question, instrument) {
  paste0(
    if (nzchar(text)) {
      paste0(instrument$text_label, ": \"", text, "\"\n\n")
    } else {
      ""
    },
    question,
    instrument$cue,
    "\n\n",
    instrument$answer_label,
    ":"
  )
}
