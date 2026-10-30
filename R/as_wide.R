#' Pivot results into an analysis matrix
#'
#' @description
#' Pivots results into one column per item, holding the probability of the
#' target label.
#'
#' @section Normalizing across items:
#' There is no argument to normalize across items. K independent binary items do
#' not form a distribution, as each probability comes from its own softmax over
#' its own two-token distribution. Dividing them by their sum produces a number
#' that looks like a proportion and is not one.
#'
#' An exclusive instrument is the exception. Its K labels share one softmax at
#' one position, so its rows do sum to one and are comparable across categories.
#' This detects that from the results and defaults `target` to `NULL`, giving
#' one column per category.
#'
#' @section Exclusive results:
#' `instrument(..., exclusive = TRUE)` produces a real distribution. Row sums
#' are meaningful, minus whatever leaked outside the label set. On an exclusive
#' instrument scored on discovered first tokens, that remainder is mostly the
#' gap left by the approximation, since the probability of a first token is an
#' upper bound on the probability of the label it opens.
#'
#' @param results A `fathom_results` object from [measure()] or
#'   [read_checkpoints()].
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param target A string naming the label whose probability fills each cell.
#'   Defaults to the first label for a multi-item instrument, and to `NULL` for
#'   an exclusive instrument, where every category gets its own column.
#' @param value A string naming the column that supplies the value. Defaults to
#'   `"prob_renorm"`, the probability conditioned on the model having answered
#'   with one of your labels at all. That is the quantity a human coder given
#'   the same categories would have produced, and it is what [validate_sample()]
#'   and the expected score read, so a matrix built here can be compared against
#'   either without changing quantity partway through.
#'
#'   `"prob"` is the raw probability, measured against every token the model
#'   could have written. Use it when the claim is about whether the model
#'   committed rather than about which label it preferred.
#'
#'   `"expected_score"` requires an instrument built with `scores`, and returns
#'   one column per item rather than one per label, since an expected score is
#'   one number per document and item.
#' @param prefix A string prepended to column names. Default is `""`.
#' @param dichotomize A logical value indicating whether to threshold at 0.5 and
#'   return integers. Default is `FALSE`.
#' @param keep A character vector of row-level columns to carry alongside. Each
#'   must hold one value per document, since this returns one row per document.
#'   A column that varies within a document is refused rather than allowed to
#'   widen the pivot. Default is `character(0)`.
#' @param drop_incomplete A logical value indicating whether to drop documents
#'   missing any item. Default is `TRUE`.
#' @return A tibble, one row per document, carrying `id`, then any `keep`
#' columns, then the pivoted values. Column names take `prefix` if given.
#'
#' What the value columns are depends on the arguments, and there are three
#' shapes.
#'
#' - Naming `target` gives one column per item, holding that label's
#'   probability. The usual shape for an instrument of independent binary items,
#'   and the one a measurement model wants.
#' - Leaving `target` as `NULL` gives one column per label on a single-item
#'   instrument, or one per item and label combination on an instrument of
#'   several, named `item_label`. Use it when every label matters rather than
#'   one.
#' - `value = "expected_score"` gives one column per item, since an expected
#'   score is already one number per document and item. Requires an instrument
#'   built with `scores`, and cannot be combined with `dichotomize`.
#'
#' Values are probabilities, or integers when `dichotomize = TRUE`. A failed,
#' truncated, or all-absent record becomes `NA`, and `drop_incomplete` decides
#' whether its document leaves the frame. Either way the count is reported
#' rather than left to be noticed.
#' @seealso [measure()] for the results it pivots and [item_report()] for a
#'   summary per item.
#' @examples
#' # Results from a real run ship with the package, so this needs no server.
#' p <- system.file("extdata", "vignette_results.rds", package = "fathom")
#' baked <- readRDS(p)
#' res <- baked$res_six
#'
#' # An exclusive instrument gives one column per category.
#' head(as_wide(res, prefix = "p_"))
#' head(as_wide(res, prefix = "p_", dichotomize = TRUE))
#' @export
as_wide <- function(
  results,
  ...,
  target = NULL,
  value = c("prob_renorm", "prob", "expected_score"),
  prefix = "",
  dichotomize = FALSE,
  keep = character(0),
  drop_incomplete = TRUE
) {
  no_dots(..., .fn = "as_wide")
  # `keep` names columns the caller carried through `measure()`, which are per
  # document and therefore in $calls, so the join has to be told about them.
  results <- flat_results(
    results,
    c("all_absent", "expected_score", "truncated", keep)
  )
  value <- match.arg(value)
  labs <- unique(results$label)
  items <- unique(results$item)
  excl <- isTRUE(attr(results, "exclusive"))
  es_mode <- identical(value, "expected_score")

  if (es_mode) {
    if (
      !"expected_score" %in% names(results) ||
        all(is.na(results$expected_score))
    ) {
      stop(
        fmt_err(paste0(
          "these results carry no expected score. Build the instrument with ",
          "`scores` to get one."
        )),
        call. = FALSE
      )
    }
    if (isTRUE(dichotomize)) {
      stop(
        fmt_err(
          "`dichotomize` has no meaning for an expected score, which is not a probability."
        ),
        call. = FALSE
      )
    }
    target <- NULL
  }

  # An exclusive instrument has one item and K categories sharing one softmax,
  # so collapsing to a single target column would discard the distribution.
  if (!es_mode && missing(target) && length(items) > 1L) {
    target <- labs[1]
  }
  if (!is.null(target) && !target %in% labs) {
    stop(
      fmt_err(paste0(
        "target '",
        target,
        "' is not a label. Available labels are ",
        paste(labs, collapse = ", ")
      )),
      call. = FALSE
    )
  }

  nf <- sum(!results$call_ok, na.rm = TRUE)
  nt <- sum(results$truncated, na.rm = TRUE)
  na <- sum(results$all_absent, na.rm = TRUE)
  if (nf) {
    warning(
      fmt_warn(paste0(nf, " failed record(s) become NA. Re-run measure().")),
      call. = FALSE
    )
  }
  if (nt) {
    warning(
      fmt_warn(paste0(
        nt,
        " call(s) landed within 64 tokens of num_ctx. llama.cpp keeps the ",
        "tail, so the opening of those documents was dropped. Raise num_ctx ",
        "and re-extract."
      )),
      call. = FALSE
    )
  }
  if (na) {
    warning(
      fmt_warn(paste0(
        na,
        " call(s) had no label anywhere in the returned distribution. That ",
        "is a template or prompt problem, not a low score. Inspect before ",
        "trusting."
      )),
      call. = FALSE
    )
  }

  d <- results
  if (es_mode) {
    # One number per document and item rather than per label, so this collapses
    # instead of pivoting across labels.
    d <- d[!duplicated(d[, c("id", "item")]), , drop = FALSE]
    d$.col <- paste0(prefix, d$item)
  } else if (!is.null(target)) {
    d <- d[d$label == target, , drop = FALSE]
    d$.col <- paste0(prefix, d$item)
  } else {
    # With one item, repeating the item name in every column is redundant.
    d$.col <- if (length(items) == 1L) {
      paste0(prefix, d$label)
    } else {
      paste0(prefix, d$item, "_", d$label)
    }
  }
  d$.value <- if (dichotomize) as.integer(d[[value]] >= 0.5) else d[[value]]

  # `keep` is selected alongside `id` and therefore joins the pivot's implicit
  # grouping. A column holding one value per document leaves one row per
  # document, which is the documented return. One that varies within a document
  # gives a row per distinct value instead, each with a single item filled and
  # the rest NA, and under `drop_incomplete` that frame then empties. Neither
  # outcome reads as a bad argument, and the incomplete-document count reports
  # rows, so it overstates once the frame has already widened.
  #
  # Judged on `d` rather than on `results`, since `d` is what reaches the pivot.
  # Filtering to `target` makes `label` constant, so a column is read on the
  # rows that will actually be grouped rather than on rows already dropped.
  #
  # Through intersect(), so a name that is not a column at all still falls
  # through to all_of() below and gets its own message rather than a subscript
  # error from here.
  present <- intersect(keep, names(d))
  varies <- vapply(
    present,
    function(k) any(tapply(d[[k]], d$id, function(v) length(unique(v)) > 1L)),
    logical(1)
  )
  if (any(varies)) {
    stop(
      fmt_err(
        paste0(
          "`keep` names column(s) that vary within a document. ",
          paste(present[varies], collapse = ", "),
          "."
        ),
        paste0(
          "This returns one row per document, so a carried column has to hold ",
          "one value per document. An item-level or label-level column widens ",
          "the pivot instead, and the frame comes back with a row per distinct ",
          "value, or empty once the incomplete rows are dropped. Carry a ",
          "property of the document through the `keep` argument of measure(), ",
          "and read an item-level one off `$calls`."
        )
      ),
      call. = FALSE
    )
  }

  wide <- d %>%
    dplyr::select(dplyr::all_of(c("id", keep)), ".col", ".value") %>%
    tidyr::pivot_wider(names_from = ".col", values_from = ".value")

  cols <- setdiff(names(wide), c("id", keep))

  if (excl && is.null(target) && !es_mode) {
    rs <- rowSums(wide[, cols, drop = FALSE], na.rm = TRUE)
    message(fmt_note(
      "Note",
      sprintf(
        "Exclusive instrument. Rows are a distribution, sums %.3f to %.3f.",
        min(rs),
        max(rs)
      )
    ))
  }

  inc <- wide[rowSums(is.na(wide[, cols, drop = FALSE])) > 0, , drop = FALSE]
  if (nrow(inc) > 0L) {
    msg <- sprintf("%d document(s) incomplete.", nrow(inc))
    if (drop_incomplete) {
      wide <- wide[!wide$id %in% inc$id, , drop = FALSE]
      msg <- paste(msg, "Dropped. They remain in the results object.")
    }
    message(fmt_note("Note", msg))
  }
  tibble::as_tibble(wide)
}
