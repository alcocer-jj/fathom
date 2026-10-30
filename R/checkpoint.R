#' Checkpoint path for one document
#'
#' Sanitizing an id for a filename is lossy, so a space, a slash, and an
#' underscore all become the same byte and two documents land on one path. The
#' readable stem is kept because it is what makes a checkpoint directory
#' navigable, and a short digest of the raw id is appended to separate ids the
#' stem cannot.
#'
#' The stem is also capped, since an id longer than the filesystem allows is a
#' write error rather than a collision. The digest carries the uniqueness, so
#' truncating the stem costs nothing.
#'
#' @param id A string giving the document identifier.
#' @param dir A string giving the checkpoint directory.
#' @return A string giving the path of the document's `.rds` file, its stem
#'   capped at 96 characters and followed by the first eight characters of an
#'   md5 digest of the raw `id`.
#' @keywords internal
#' @noRd
ckpt_path <- function(id, dir) {
  stem <- substr(gsub("[^A-Za-z0-9._-]", "_", id), 1, 96)
  tag <- substr(digest::digest(as.character(id), algo = "md5"), 1, 8)
  file.path(dir, paste0(stem, "-", tag, ".rds"))
}

#' Read one checkpoint, tolerating absence and corruption
#'
#' Returns NULL rather than erroring when the file is missing or unreadable, so
#' a truncated write from a killed run is treated as not done and re-extracted.
#'
#' A payload written before `ok` became `call_ok` is renamed on read, so an
#' existing directory resumes rather than re-extracting every item.
#'
#' @param id,dir As in `ckpt_path()`.
#' @return The stored payload, a list, or `NULL` when the file is missing,
#'   unreadable, or holds another document's `id`.
#' @keywords internal
#' @noRd
ckpt_read <- function(id, dir) {
  fp <- ckpt_path(id, dir)
  if (!file.exists(fp)) {
    return(NULL)
  }
  p <- tryCatch(readRDS(fp), error = function(e) NULL)
  # Sanitizing the id for a filename is lossy, so two ids differing only in
  # punctuation land on one path. Treating a mismatch as absent re-extracts the
  # second document instead of returning the first one twice under its own id.
  if (!is.null(p) && !is.null(p$id) && !identical(as.character(p$id), id)) {
    return(NULL)
  }
  # A payload written before `ok` became `call_ok` still carries the old name,
  # and ckpt_stale() reads the new one. Without this every item in an existing
  # directory looks stale and the corpus re-extracts against the model.
  if (
    !is.null(p) &&
      !is.null(p$results) &&
      "ok" %in% names(p$results) &&
      !("call_ok" %in% names(p$results))
  ) {
    names(p$results)[names(p$results) == "ok"] <- "call_ok"
  }
  p
}

#' Write one checkpoint atomically
#'
#' Writes to a temporary file in the same directory and renames, so a crash
#' during serialization leaves the previous checkpoint intact rather than a
#' half-written file that reads as corrupt.
#'
#' @param payload A list holding one document's record.
#' @param id,dir As in `ckpt_path()`.
#' @return The path written, invisibly.
#' @keywords internal
#' @noRd
ckpt_write <- function(payload, id, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  fp <- ckpt_path(id, dir)
  tmp <- paste0(fp, ".tmp")
  saveRDS(payload, tmp)
  file.rename(tmp, fp)
  invisible(fp)
}

#' Which items still need a model call
#'
#' An item is current only if it succeeded, under this model, with this prompt
#' hash. Everything else is stale. There is no separate resume function. Calling
#' measure() twice against the same directory does only what this reports as
#' outstanding.
#'
#' @param prev A payload from `ckpt_read()`, or `NULL`.
#' @param hashes A named character vector of prompt hashes, one per item, as
#'   `item_hashes()` returns it.
#' @param model A string giving the model name.
#' @return A character vector of the item names that still need a model call.
#' @keywords internal
#' @noRd
ckpt_stale <- function(prev, hashes, model) {
  wanted <- names(hashes)
  if (is.null(prev) || is.null(prev$results) || nrow(prev$results) == 0L) {
    return(wanted)
  }
  r <- prev$results
  cur <- unique(r[
    r$call_ok & r$model == model,
    c("item", "prompt_hash"),
    drop = FALSE
  ])
  stale <- vapply(
    wanted,
    function(nm) {
      h <- cur$prompt_hash[cur$item == nm]
      length(h) == 0L || !identical(h[[1]], unname(hashes[[nm]]))
    },
    logical(1)
  )
  wanted[stale]
}

#' Read a checkpoint directory back into results
#'
#' Reconstructs the return value of [measure()] from disk without making any
#' model calls. Use when a run completed but the return value was lost, or to
#' inspect how far a killed run progressed.
#'
#' @section Provenance:
#' The context ceiling, the exclusivity flag and the run timestamp are read
#' from disk rather than inferred from the current session. A run written under
#' one ceiling reports that ceiling even if the current session would resolve a
#' different one.
#'
#' @param dir A string giving the checkpoint directory.
#' @return A `fathom_results` object, the two frames [measure()] returns and
#' documents there. Rows arrive in whatever order the directory listing gives,
#' so sort if order matters.
#'
#' The attributes are read back off the stored payloads rather than stamped from
#' the current session, so a corpus reports the run that produced it.
#'
#' - `instrument` is the instrument as it was when the run happened.
#' - `num_ctx` is the ceiling that run used, not the one a run would get now. A
#'   resumed run keeps this, which is why mixing ceilings inside one corpus
#'   cannot happen silently.
#' - `exclusive` says whether the instrument shared one distribution.
#' - `run_at` says when the run happened, not when it was read.
#'
#' A file that cannot be read is treated as absent rather than as an error,
#' since a killed run can leave one half-written and re-extracting that document
#' is the only recoverable reading. Errors when the directory holds no readable
#' checkpoints at all.
#' @seealso [measure()], whose `checkpoint` argument writes what this reads.
#' @examples
#' \dontrun{
#' # Needs a running Ollama server, so it is not run here.
#' oll <- ollama_backend("gemma4:31b")
#' b <- instrument(c(tone = "Is this sentence positive?"),
#'   labels = c("YES", "NO"))
#' res <- measure(parlasent, instrument = b, backend = oll, text = "sentence",
#'   id = "id", checkpoint = "ckpt")
#'
#' # A run cut short leaves its finished documents in the directory.
#' res <- read_checkpoints("ckpt")
#' }
#' @export
read_checkpoints <- function(dir) {
  fps <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(fps) == 0L) {
    stop(fmt_err(paste0("no checkpoints found in ", dir, ".")), call. = FALSE)
  }
  payloads <- lapply(fps, function(fp) {
    tryCatch(readRDS(fp), error = function(e) NULL)
  })
  payloads <- Filter(Negate(is.null), payloads)
  # Every file listed and none of them read, which a run killed while writing
  # can leave behind. Said here, since indexing the first payload below would
  # otherwise fail with a bare subscript error.
  if (length(payloads) == 0L) {
    stop(
      fmt_err(
        paste0(
          "none of the ",
          length(fps),
          " checkpoint file(s) in ",
          dir,
          " could be read."
        ),
        paste0(
          "A run killed while writing can leave a file that does not read. ",
          "Running measure() against the directory measures those documents ",
          "again."
        )
      ),
      call. = FALSE
    )
  }
  res <- dplyr::bind_rows(lapply(payloads, `[[`, "results"))
  message(fmt_note(
    "Note",
    sprintf("%d checkpoint(s) -> %d rows", length(payloads), nrow(res))
  ))

  # Same reason as in measure(). Checkpoints written under different `scores`
  # would otherwise be read back inconsistently, and nothing in a row says which
  # values produced it.
  res <- apply_scores(res, payloads[[1]]$instrument)

  # The ceiling is resolved per run rather than stored on the backend, so it
  # comes off the payload. One written before the field existed carries none
  # and reads as NA. `exclusive` follows from the instrument inside
  # new_results().
  out <- new_results(
    res,
    payloads[[1]]$instrument,
    payloads[[1]]$backend,
    num_ctx = payloads[[1]]$num_ctx %||% NA_integer_
  )

  # new_results() stamps the current time, which on this path describes the
  # read rather than the run. ISO 8601 sorts lexically, so the latest write is
  # the last document to finish. A payload written before the version was
  # removed carries one and it is not read, which invalidates nothing, since
  # ckpt_stale() has only ever asked about `call_ok`, `model` and the prompt
  # hash.
  stamps <- vapply(
    payloads,
    function(p) as.character(p$run_at %||% NA_character_),
    character(1)
  )
  stamps <- stamps[!is.na(stamps)]
  if (length(stamps)) {
    attr(out, "run_at") <- max(stamps)
  }
  out
}
