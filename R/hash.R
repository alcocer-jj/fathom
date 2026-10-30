#' Hash the invariant part of a prompt
#'
#' The fingerprint stored with each result so a later run can tell current
#' results from stale ones. Covers the question, the cue, the label surface
#' variants, the prompt scaffolding, and the model. The document is excluded
#' because it varies by row.
#'
#' `has_text` is in the key because it changes the prompt without changing
#' anything else in it. A run with no text column omits the document block
#' entirely, so the same instrument against the same model produces two
#' different prompts. Without this, a text run's checkpoints would read as
#' current for an image run and be resumed into.
#'
#' @param question A string giving the item question text.
#' @param instrument A `fathom_instrument`.
#' @param model A string giving the model name.
#' @param has_text A logical value indicating whether the run carries a text
#'   column, so the document block is emitted.
#' @return 12-character hex string.
#' @keywords internal
#' @noRd
prompt_hash <- function(question, instrument, model, has_text = TRUE) {
  key <- paste(
    question,
    instrument$cue,
    instrument$text_label,
    instrument$answer_label,
    paste(names(instrument$labels), collapse = "|"),
    paste(unlist(instrument$labels), collapse = "|"),
    model,
    if (isTRUE(has_text)) "text" else "no-text",
    sep = "\x1f"
  )
  substr(digest::digest(key, algo = "md5"), 1, 12)
}

#' Hash every item in an instrument
#'
#' Vectorized `prompt_hash()`. One fingerprint per item, so checkpoint
#' invalidation operates at item granularity. Rewording one question
#' re-extracts that question across the corpus and leaves the rest untouched.
#'
#' @param instrument A `fathom_instrument`.
#' @param model A string giving the model name.
#' @param has_text A logical value indicating whether the run carries a text
#'   column.
#' @return Named character vector.
#' @keywords internal
#' @noRd
item_hashes <- function(instrument, model, has_text = TRUE) {
  vapply(
    instrument$items,
    prompt_hash,
    character(1),
    instrument = instrument,
    model = model,
    has_text = has_text
  )
}
