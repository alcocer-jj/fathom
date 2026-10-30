#' Read the documents that fired on an item
#'
#' @description
#' Pulls the documents that endorsed an item, with their full text, for checking
#' whether the model was right.
#'
#' @section The header line:
#' Three numbers, in this order. How many cases are printed, how many qualified
#' before sampling down to `n`, and how large the corpus was. The middle one is
#' the one to read, since it says whether the printed cases are all of them or a
#' sample. Reporting the corpus size alone would read as though every document
#' not shown had failed the filter, which is the opposite of what it means.
#'
#' @section Endorsement by label count:
#' On two labels it is the 0.5 cut on `prob_renorm`, which is what
#' [item_report()] counts as `endorse_renorm`. Above two the labels compete for
#' one pool, a winner at 0.3 clears no cut, and endorsement is instead the label
#' that took the most mass, which is what `item_report()` counts as `n_won`.
#'
#' `"uncertain"` is a rank rather than a threshold in both cases, so it commits
#' to no cut point. On two labels it sorts by distance from 0.5. Above two it
#' sorts by `margin`, the gap between the winning label and the runner-up, so
#' the first rows are the closest calls.
#'
#' @section Excerpts versus full text:
#' Full text is the default. The passage that triggered an item is often outside
#' a keyword-centered window, so an excerpt can read as a false negative when
#' the item was working. Setting `chars` truncates and warns.
#'
#' @param results A `fathom_results` object from [measure()] or
#'   [read_checkpoints()].
#' @param item A string naming the item to inspect.
#' @param docs A character vector of documents, or the data frame passed to
#'   [measure()]. Optional when `measure()` was given a character vector, since
#'   the documents are stored on the results in that case. Default is `NULL`.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param text,id Strings naming the text and identifier columns, if `docs` is a
#'   data frame. `text` is then required, and `id` defaults to the row position.
#' @param which A string choosing the documents, `"endorsed"` (the default) for
#'   those where `target` won or `"uncertain"` for the closest calls regardless
#'   of answer. `"uncertain"` has nothing to sort on when every document came
#'   back at the same extreme, and says so rather than returning corpus order
#'   under that label. To read the documents an item did not fire on, ask for
#'   the other label as `target` rather than for the complement of this one.
#' @param target A string naming the label counted as endorsement. Defaults to
#'   the first.
#' @param n An integer giving the number of documents to show. Default is 6.
#' @param chars An integer giving the characters each document is truncated to,
#'   or `NULL` (the default) for full text. Warns.
#' @param file A string giving a path to write markdown to instead of printing,
#'   or `NULL` (the default) to print.
#' @param seed An integer giving a seed for the sample, or `NULL` (the default).
#'   It reaches this draw only, and the session's random stream is put back
#'   afterwards.
#' @return Invisibly, a tibble of the selected cases, one row per document, in
#' the order they are printed. Printing is the point of this function and the
#' returned object is a convenience, so it carries the columns the printout drew
#' on rather than a curated set.
#'
#' - `id` is the document identifier.
#' - `item` is the item inspected, the same value on every row.
#' - `label` is `target`, the same value on every row, since the rows for other
#'   labels are dropped before selection.
#' - `prob_renorm` is the probability on `target` for that document, rescaled
#'   over the labels. This is what `which` filtered on and what the printout
#'   shows.
#' - `prob` is the raw probability on `target`, before rescaling.
#' - `prob_other` is the probability the model placed on tokens that were none
#'   of the labels, for that document.
#' - `text` is the document, joined from `docs`. Truncated if `chars` was set.
#'   Named `text` even when the source column was called something else.
#'
#' Any other columns the results carry travel through untouched, including
#' anything passed to `keep` in [measure()] and the remaining diagnostic
#' columns. A run that found no qualifying cases returns a tibble with no rows.
#' @seealso [item_report()] for the summary these documents sit behind.
#'   [coding_sample()] for drawing cases to hand to a coder rather than reading
#'   a few.
#' @examples
#' # Results from a real run ship with the package, so this needs no server.
#' p <- system.file("extdata", "vignette_results.rds", package = "fathom")
#' baked <- readRDS(p)
#' res <- baked$res_six
#'
#' # measure() was given a data frame, so the documents are passed again.
#' inspect_cases(res, "category", parlasent, text = "sentence", id = "id",
#'   target = "1", n = 2, seed = 1)
#' @export
inspect_cases <- function(
  results,
  item,
  docs = NULL,
  ...,
  text = NULL,
  id = NULL,
  which = c("endorsed", "uncertain"),
  target = NULL,
  n = 6L,
  chars = NULL,
  file = NULL,
  seed = NULL
) {
  no_dots(..., .fn = "inspect_cases")
  results <- flat_results(
    results,
    c("expected_score", "margin", "prob_other")
  )
  which <- match.arg(which)
  target <- target %||% results$label[1]
  check_target(target, unique(results$label))
  if (!item %in% results$item) {
    stop(
      fmt_err(paste0(
        "item '",
        item,
        "' is not in the results. Available items are ",
        paste(unique(results$item), collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }
  if (!is.null(chars)) {
    warning(
      fmt_warn(paste0(
        "Truncating documents to ",
        chars,
        " characters. The passage that ",
        "triggered the item is often outside a keyword-centered window, so an ",
        "excerpt can read as a false negative. Prefer full text."
      )),
      call. = FALSE
    )
  }

  # Both values are defined at any label count, so there is nothing to warn
  # about. On two labels endorsement is the 0.5 cut. Above two the labels
  # compete for one pool, a winner at 0.3 clears no cut, and the question is
  # which label took the most mass. Uncertainty is the same either way, read as
  # a rank rather than a threshold. It is closeness to 0.5 on two labels and the
  # smallest gap to the runner-up above two.
  n_lab <- dplyr::n_distinct(results$label)

  tx <- as_docs(docs, text, id, results)
  d <- results[
    results$item == item & results$label == target & results$call_ok,
    ,
    drop = FALSE
  ]
  if (nrow(d) == 0L) {
    message(fmt_note(
      "Note",
      paste0("No successful records for item '", item, "'.")
    ))
    return(invisible(tibble::tibble()))
  }

  # How many qualified, before sampling down to n. That is the number a reader
  # needs, since it says how many documents are being withheld. Reporting the
  # corpus size instead reads as though every non-shown document failed the
  # filter, which is the opposite of what it means.
  n_pool <- NULL

  # Both branches read prob_renorm rather than prob. That is the probability
  # conditioned on the model having answered with a label at all, which is the
  # measure item_report() reports and threshold_sensitivity() cuts on. Reading
  # the raw one here would disagree with both on any run that leaked.
  sel <- switch(
    which,
    endorsed = if (n_lab > 2L) {
      # No cut to make. The question is which label took the most mass, which
      # is the same rule item_report() counts as n_won and the same one
      # coding_sample() and validate_sample() use to name a winner.
      # A document whose distribution held none of the labels has no winner.
      won <- winning_rows(
        results[results$item == item & results$call_ok, , drop = FALSE]
      )
      d[d$id %in% won$id[won$label == target], , drop = FALSE]
    } else {
      # which() rather than the bare comparison. A document where no label
      # appeared has no prob_renorm, and an NA in a logical index returns a row
      # of NA, which printed as a case for a document called NA.
      d[which(d$prob_renorm >= 0.5), , drop = FALSE]
    },
    uncertain = {
      # A rank, not a threshold, so nothing here commits to a cut point. Above
      # two labels the closest call is the smallest gap to the runner-up.
      key <- if (n_lab > 2L) d$margin else abs(d$prob_renorm - 0.5)
      h <- d[order(key), , drop = FALSE]
      # The sort is only meaningful if the key varies. On a saturated item
      # every document ties and order() returns corpus order.
      if (length(unique(round(key, 6))) == 1L) {
        message(fmt_note(
          "Note",
          paste0(
            "No variation in ",
            if (n_lab > 2L) "margin" else "probability",
            ". Every document sits at ",
            sprintf("%.4f", sort(key)[1]),
            ". These are the first rows in corpus order, not uncertain cases. ",
            "There are none to find. Use which = \"endorsed\" instead, and read ",
            "whichever label the construct could plausibly appear on."
          )
        ))
      }
      h
    }
  )
  n_pool <- nrow(sel)

  if (nrow(sel) == 0L) {
    message(fmt_note(
      "Note",
      paste0("No '", which, "' cases for item '", item, "'.")
    ))
    return(invisible(tibble::tibble()))
  }
  if (which != "uncertain" && nrow(sel) > n) {
    local_seed(seed)
    sel <- sel[sample(nrow(sel), n), , drop = FALSE]
  }
  sel <- utils::head(sel, n)
  sel <- dplyr::left_join(sel, tx, by = "id")

  b <- attr(results, "instrument")
  con <- if (is.null(file)) {
    stdout()
  } else {
    file(file, open = "wt", encoding = "UTF-8")
  }
  on.exit(if (!is.null(file)) close(con), add = TRUE)

  # "2 of 3 endorsed, from 10 documents" rather than "2 of 10". The middle
  # number is what tells you whether you are seeing all of them.
  cat(
    sprintf(
      "\n# %s  (%d of %d %s, from %d documents)\n",
      item,
      nrow(sel),
      n_pool,
      which,
      nrow(d)
    ),
    file = con
  )
  if (!is.null(b)) {
    cat(
      "\n**Question**\n\n```\n",
      b$items[[item]],
      "\n```\n",
      sep = "",
      file = con
    )
  }
  for (i in seq_len(nrow(sel))) {
    body <- sel$.fathom_text[i]
    if (!is.null(chars) && nchar(body) > chars) {
      body <- paste0(substr(body, 1, chars), "\n\n[TRUNCATED]")
    }
    cat(
      sprintf(
        "\n---\n\n**%s** | P(%s) = %.4f, other = %.2e, %d words\n\n",
        sel$id[i],
        target,
        sel$prob_renorm[i],
        sel$prob_other[i],
        n_words(sel$.fathom_text[i])
      ),
      file = con
    )
    cat(body, "\n", file = con)
  }
  if (!is.null(file)) {
    message(fmt_note("Note", paste0("Wrote ", file)))
  }
  # The join column is namespaced so a results column named `text`, carried
  # through `keep`, cannot suffix it out from under the read above. That name is
  # internal, so it is put back before the frame is handed over. If the results
  # already carry their own `text` the namespaced one stays as it is, since
  # overwriting a column the caller asked to keep would be worse than an odd
  # name on the one they did not.
  if (".fathom_text" %in% names(sel) && !("text" %in% names(sel))) {
    names(sel)[names(sel) == ".fathom_text"] <- "text"
  }
  invisible(sel)
}

#' Item names in the order the instrument declares them
#'
#' `summarise()` returns its groups in alphabetical order, and arranging on the
#' item name does the same, so a report came back with the instrument
#' reshuffled. An instrument is a questionnaire, and reading down a report is
#' easier when Q10 sits after Q9 rather than between Q1 and Q2. `measure()`
#' already stores results in instrument order. Only the summaries lost it.
#'
#' Items in the results but not in the instrument keep their relative order at
#' the end, so a subset, a renamed item, or a hand-built results object still
#' gets every row back. Results carrying no instrument attribute fall back to
#' order of first appearance, which is instrument order already for anything
#' `measure()` produced. Every name seen in the results is therefore in the
#' returned vector, so matching against it cannot drop a row.
#'
#' @param results A results object, or a frame carrying an `item` column.
#' @return A character vector of item names, the declared ones first in
#'   instrument order and any others after them in order of appearance.
#' @keywords internal
#' @noRd
item_levels <- function(results) {
  declared <- names(attr(results, "instrument")$items)
  seen <- unique(as.character(results$item))
  c(declared[declared %in% seen], setdiff(seen, declared))
}

#' Labels in the order the instrument declared them
#'
#' The counterpart of `item_levels()`. Everything else in the package already
#' works in declaration order, since `score_labels()` builds its rows from
#' `names(instrument$labels)`, so `$probs`, `preview()` and `as_wide()` all
#' agree. This is what lets a report agree with them.
#'
#' Falls back to first-appearance order when no instrument is attached, which
#' is the only order available then. Anything seen but not declared is appended
#' rather than dropped, so matching against the result cannot lose a row.
#'
#' @param results A results object, or a frame carrying a `label` column.
#' @return A character vector of labels.
#' @keywords internal
#' @noRd
label_levels <- function(results) {
  b <- attr(results, "instrument")
  seen <- unique(as.character(results$label))
  if (is.null(b)) {
    return(seen)
  }
  declared <- names(b$labels)
  c(declared[declared %in% seen], setdiff(seen, declared))
}

#' Summarize how each item behaved
#'
#' @description
#' Produces a diagnostic data frame where each row is one item, or each row is
#' one item and label, summarizing how each item behaved across the corpus. Each
#' column answers a different question about whether an item is usable.
#'
#' It is purely descriptive, meaning no column here reports whether an item is
#' correct. A confident, low-leakage item can still be measuring something
#' other than the construct. [inspect_cases()] is what answers that, by showing
#' the documents.
#'
#' @section Two endorsement rates:
#' `endorse` and `endorse_renorm` count the same documents at the same cut of
#' 0.5, and differ only in which probability they cut. Do not read the gap
#' between them as measurement error or as disagreement between two estimates of
#' one quantity. It is two quantities.
#'
#' Neither cut is configurable. 0.5 on `prob_renorm` is the argmax on a
#' two-label instrument, and [threshold_sensitivity()] exists to show how little
#' the cut point matters on `prob`. Both are reported so the choice does not
#' have to be made in advance.
#'
#' @section Output shapes:
#' Three shapes come back, and which one is decided by the **labels** rather
#' than by the items. The number of items changes how many rows come back, not
#' which columns. The test runs in this order.
#'
#' \enumerate{
#'   \item Exactly two labels. **Endorsement shape.**
#'   \item Otherwise, the instrument carries `scores`. **Scored shape.**
#'   \item Otherwise, more than two labels. **Choice shape.**
#' }
#'
#' The label count is tested first because on two labels the scored shape
#' reports one number, the average expected score, and drops a dozen
#' diagnostics to do it. There is no reason to choose between them. A scored
#' instrument of two labels reports as endorsement and gains `mean_expected` as
#' a column, so the number arrives without the diagnostics leaving.
#'
#' `exclusive = TRUE` is not itself the test. An exclusive instrument lands in
#' the choice or scored shape because it usually has more than two labels or a
#' scale, and an exclusive instrument of two labels reports as endorsement like
#' any other. Equally, K separate questions sharing one scale reach the scored
#' shape without being exclusive at all.
#'
#' Nothing here decides whether your labels are ordered, and nothing can. What
#' `scores` says is that an average of those numbers is defined, which is why
#' the shape is named for the argument rather than for a measurement level.
#' [validate_sample()] is where ordering is declared, by its `level`.
#'
#' The shapes differ in row granularity as well as in columns. Endorsement gives
#' one row per item. Choice and scored give one row per item **and label**,
#' because a shared distribution has no single number per question to report.
#' Choice and scored report a winner distribution, and leaked mass reaches them
#' as a message rather than as a column.
#'
#' `target` applies to the endorsement shape only, and is ignored by the other
#' two. There is no per-label endorsement rate for a forced choice, since the
#' labels all compete for one pool of probability, and a rate for whichever
#' label sorted first is not a meaningful quantity.
#'
#' @param results A `fathom_results` object from [measure()] or
#'   [read_checkpoints()].
#' @param target A string naming the label counted as endorsement. Defaults to
#'   the first (e.g., "yes" in an instrument where "yes" was written first
#'   before "no"). Used by the endorsement shape only.
#' @param order A string setting how labels are ordered within a question.
#'   `"declared"`, the default, is the order the instrument wrote them, which
#'   for an ordered scale is the scale and which every other part of the package
#'   already uses. `"size"` puts the most frequent winner first, which is the
#'   reading for unordered categories. No effect on the endorsement shape, which
#'   has no label rows.
#' @return A tibble in one of three shapes, chosen as described below. Items
#' come back in the order the instrument declares them rather than
#' alphabetically, and an item present in the results but absent from the
#' instrument sorts after the declared ones.
#'
#' ## Endorsement shape
#'
#' Exactly two labels and no `scores`. One row per item.
#'
#' - `item` is the question name.
#' - `n` counts the documents scored on this item. Failed calls are excluded, so
#'   this can sit below the corpus size.
#' - `endorse_renorm` is the share of those documents where `target` reached 0.5
#'   on `prob_renorm`, the probability after the labels have been rescaled to
#'   sum to 1. Reaching it means the label beat the other one.
#' - `n_endorse_renorm` is the count behind `endorse_renorm`. Its denominator is
#'   `n` minus the documents where no label appeared, which have no renormalized
#'   probability. That count is `pct_both_absent` times `n`, and is 0 whenever
#'   `pct_both_absent` is.
#' - `endorse` is the same cut on `prob`, the probability against everything the
#'   model could have written. Reaching it means the model committed to the
#'   label.
#' - `n_endorse` is the count behind `endorse`. Its denominator is `n`.
#' - `mean_prob` is the mean `prob` on `target`, so it belongs to `endorse`
#'   rather than to `endorse_renorm`. It separates from `endorse` only when the
#'   probabilities cluster near the 0.5 cut. When they all sit near 0 or 1 the
#'   two agree almost exactly, so the gap between them says how far the
#'   endorsements were from being coin flips.
#' - `pct_extreme_renorm` is the share of renormalized probabilities at or
#'   beyond 0.01 and 0.99, so it asks how the on-label mass was shaped rather
#'   than how much of it there was. This is the substantive one, for the same
#'   reason `endorse_renorm` is. It is not unconditionally the safer one. A
#'   document that answered almost entirely outside the label set can leave this
#'   denominator, read as graded, or read as certain, depending on how its
#'   residue fell.
#' - `pct_extreme` is the share of raw probabilities at or beyond 0.01 and 0.99.
#'   Reads `prob`, so it cannot separate a decisive answer from one that never
#'   entered the label set, since both put every label at the floor or the
#'   ceiling. Read it against `pct_extreme_renorm` and `max_prob_other` rather
#'   than alone. Near 1 means almost every answer came back at one end or the
#'   other, so these are labels in numeric clothing. Reporting them as degrees
#'   of confidence would claim more than they carry.
#' - `pct_both_absent` is the share of documents where no label appeared at
#'   position 1. That is a template or prompt failure rather than a low score. A
#'   broken chat template registers in `n_off_label` rather than here, because
#'   the labels stay in the distribution at trace mass underneath whatever the
#'   template wrote first, which leaves `pct_both_absent` at 0 while every
#'   answer is unusable.
#' - `max_prob_other` is the most probability the model ever placed on tokens
#'   that were none of your labels, anywhere in the corpus. Leaked mass, in the
#'   term used elsewhere in these docs.
#' - `min_top1` is the least confident single answer in the corpus. For each
#'   document, take the probability of whichever token the model was most likely
#'   to write, label or not. This is the smallest of those. High values do not
#'   mean the item is correct.
#' - `n_off_label` counts the documents where the emitted token matched no
#'   label. It is judged by the same rule scoring uses, so a token that opens a
#'   label counts as that label and a split label is not reported as off-label.
#' - `n_failed` counts the documents whose call failed. Divided by the label
#'   count, so it counts documents rather than rows.
#' - `other_vs_median` is `max_prob_other` divided by the middle value of that
#'   column across the instrument, so 2 means this item placed twice as much
#'   probability outside the labels as the typical item did. Present only for an
#'   instrument of three or more items, since a middle value across one or two
#'   carries nothing. This is the one column the item count decides. Absolute
#'   leaked mass varies by model, by language, and by how close to 0 and 1 the
#'   instrument's probabilities sit, so values are not comparable across
#'   instruments.
#' - `mean_expected` is the average expected score across documents, which on
#'   two labels is the mean of `prob_renorm` on `target` rescaled onto the
#'   scores. Present only when the instrument carries `scores`. Note that
#'   `mean_prob` is the raw probability, so the two differ by whatever mass fell
#'   outside the labels.
#'
#' ## Choice shape
#'
#' More than two labels and no `scores`. One row per item and label.
#'
#' - `item` and `label` are the question and category.
#' - `n_won` counts the documents where this label took the largest probability.
#'   A document whose distribution held none of the labels wins nothing.
#' - `share_won` is `n_won` over the documents scored on that item, so the
#'   shares of an item fall short of 1 by the share that answered outside the
#'   labels.
#'
#' ## Scored shape
#'
#' Whenever the instrument carries `scores` and there are more than two labels.
#' One row per item and label, the same winner distribution plus the scale.
#'
#' - `item` and `label` are the question and category.
#' - `score` is the numeric value the instrument assigns that label.
#' - `n_won` and `share_won` are as in the choice shape.
#' - `mean_expected` is the average expected score across documents, per
#'   question. For a single document the expected score multiplies each label's
#'   value by its probability and adds them up, after those probabilities have
#'   been rescaled to sum to 1 across the labels. Because it uses every label
#'   rather than only the winner, a document the model split between two
#'   neighboring levels lands between them.
#' - `sd_expected` is the standard deviation of the same, across documents.
#' @seealso [inspect_cases()] for the documents behind an item, and
#'   [threshold_sensitivity()] for how the rates move with the cut point.
#' @examples
#' # Results from a real run ship with the package, so this needs no server.
#' p <- system.file("extdata", "vignette_results.rds", package = "fathom")
#' baked <- readRDS(p)
#' item_report(baked$res_indep)
#' @export
item_report <- function(results, target = NULL, order = c("declared", "size")) {
  order <- match.arg(order)
  results <- flat_results(
    results,
    c("expected_score", "prob_other", "top1_token", "top1_prob")
  )
  ok <- results[results$call_ok, , drop = FALSE]
  # The report shape follows the results rather than the instrument's exclusive
  # flag. What decides it is how many labels there are and whether they carry
  # values. An exclusive instrument is one way to reach more than two labels,
  # and K questions sharing one scale is another. Both need the same columns.
  # The scores live on the instrument, so a results object that lost its
  # attribute has an expected score it cannot label. It reports as a choice
  # instrument rather than erroring inside the mutate.
  scored <- "expected_score" %in%
    names(ok) &&
    !all(is.na(ok$expected_score)) &&
    !is.null(attr(results, "instrument")$scores)

  # Label count first. On two labels the scored shape reports one number and
  # drops a dozen diagnostics to do it, and the two are not alternatives. The
  # endorsement shape carries the number as a column instead. It is not a
  # duplicate of anything already there, since mean_prob is the raw probability
  # and the expected score is built on the renormalized one.
  if (dplyr::n_distinct(results$label) > 2L) {
    if (scored) {
      return(scored_report(results, ok, order))
    }
    return(choice_report(results, ok, order))
  }
  target <- target %||% results$label[1]
  check_target(target, unique(results$label))
  per_doc <- ok %>%
    dplyr::group_by(.data$item, .data$id) %>%
    dplyr::summarise(
      both_absent = all(!.data$present),
      prob_other = .data$prob_other[1],
      top1_prob = .data$top1_prob[1],
      top1_token = .data$top1_token[1],
      .groups = "drop"
    )
  tgt <- ok[ok$label == target, , drop = FALSE]

  # Judged by the rule scoring uses rather than by membership in the declared
  # variants. Under prefix matching those are different questions. BU is not in
  # the variant list and is scored as BUDGET, so a membership test would report
  # every correct answer on a split label as off-label.
  b <- attr(results, "instrument")
  off_label <- function(resp) {
    if (is.null(b)) {
      return(rep(FALSE, length(resp)))
    }
    tok <- stringi::stri_trans_toupper(trimws(resp))
    !vapply(
      tok,
      function(t) {
        any(vapply(
          seq_along(b$normalized),
          function(i) any(match_token(t, b, i)),
          logical(1)
        ))
      },
      logical(1)
    )
  }

  lv <- item_levels(results)
  out <- tgt %>%
    dplyr::group_by(.data$item) %>%
    dplyr::summarise(
      n = dplyr::n(),
      # Renormalized first, because it is the reported measure. `prob` is the
      # record every other column is derived from, and it earns its place here
      # as the thing `endorse_renorm` is read against rather than as the
      # headline rate.
      endorse_renorm = mean(.data$prob_renorm >= 0.5, na.rm = TRUE),
      n_endorse_renorm = sum(.data$prob_renorm >= 0.5, na.rm = TRUE),
      endorse = mean(.data$prob >= 0.5),
      n_endorse = sum(.data$prob >= 0.5),
      # The raw cut, reported beside the renormalized one rather than instead
      # of it. Both are 0.5, so neither is a tuning knob. What carries the
      # information is the gap between them, which is the probability the model
      # spent outside the label set.
      #
      # na.rm on the renormalized pair above, because prob_renorm is NA for a
      # document where no label appeared at all. There is nothing to condition
      # on there, so it leaves that denominator rather than counting as a
      # non-endorsement. pct_both_absent sizes how many those were.
      mean_prob = mean(.data$prob),
      # Saturation, asked twice, renormalized first for the same reason the
      # endorsement pair is. The measure leads and the diagnostic follows, so a
      # reader taking the first column of each pair takes the right one.
      #
      # `pct_extreme_renorm` conditions on the model having answered in the
      # label set at all, so it asks only whether the labels were graded
      # against each other. On a two-label instrument it is symmetric by
      # construction, since the pair sums to 1 and one crossing 0.99 puts the
      # other under 0.01.
      #
      # `pct_extreme` reads the raw probability, so a document the model
      # answered outside the label set entirely lands at the floor on every
      # label and reads as maximally decisive. That is the same place a
      # genuinely decisive answer's losing label sits, and the raw column
      # cannot separate the two.
      #
      # The gap needs the residual on-label mass split across labels, not
      # merely a large leak. A document leaking 0.9995 with one label surviving
      # renormalizes to exactly 0 and 1, which is extreme in both columns and
      # opens nothing. Leak size is not the condition. Degeneracy is.
      pct_extreme_renorm = mean(
        .data$prob_renorm <= 0.01 | .data$prob_renorm >= 0.99,
        na.rm = TRUE
      ),
      pct_extreme = mean(.data$prob <= 0.01 | .data$prob >= 0.99),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      per_doc %>%
        dplyr::group_by(.data$item) %>%
        dplyr::summarise(
          pct_both_absent = mean(.data$both_absent),
          max_prob_other = max(.data$prob_other, na.rm = TRUE),
          min_top1 = min(.data$top1_prob, na.rm = TRUE),
          n_off_label = sum(off_label(.data$top1_token)),
          .groups = "drop"
        ),
      by = "item"
    ) %>%
    dplyr::left_join(
      results %>%
        dplyr::group_by(.data$item) %>%
        dplyr::summarise(
          n_failed = sum(!.data$call_ok) / dplyr::n_distinct(.data$label),
          .groups = "drop"
        ),
      by = "item"
    ) %>%
    # summarise() is what imposed alphabetical order, so the sort goes after it.
    # Sorting on the match position rather than on a factor leaves `item` a
    # plain character column, which is what everything downstream reads.
    dplyr::arrange(match(.data$item, lv))

  # A scored instrument of two labels reports here rather than as a scored
  # shape, so the average score joins the diagnostics instead of replacing
  # them. Taken over calls rather than over rows, since expected_score is one
  # value per call and appears once per label.
  if (scored) {
    out <- out %>%
      dplyr::left_join(
        results %>%
          dplyr::group_by(.data$item, .data$id) %>%
          dplyr::summarise(
            e = .data$expected_score[1],
            .groups = "drop_last"
          ) %>%
          dplyr::summarise(
            mean_expected = mean(.data$e, na.rm = TRUE),
            .groups = "drop"
          ),
        by = "item"
      )
  }
  # `n_endorse` used to raise a message below thirty on a corpus of a hundred or
  # more. It is gone. This function describes rather than screens, and the same
  # paragraph two lines down declines to threshold leaked mass for exactly that
  # reason, so thresholding endorsement was the one place the contract broke.
  #
  # Sufficient n also depends on what happens next, which the function cannot
  # know. Thirty endorsements is thin for a two-parameter model and ample for a
  # prevalence estimate, and the message fired on rare categories that were
  # measured correctly. `n_endorse` is a column. A reader who needs the count
  # can read it.
  #
  # Leaked mass is comparable within an instrument and not across instruments,
  # so it is reported as a ratio to the instrument median. No threshold and no
  # message.
  if (nrow(out) >= 3L) {
    med <- stats::median(out$max_prob_other, na.rm = TRUE)
    out$other_vs_median <- if (med > 0) out$max_prob_other / med else NA_real_
  }
  out
}

#' Report for an instrument with more than two labels
#'
#' @description Called by [item_report()] when the labels number more than two
#'   and carry no scores. Reached by an exclusive instrument, and by any
#'   instrument whose questions share a common answer set.
#'
#' @section No mean probability column:
#' In a forced choice the winner takes nearly all the mass, so a label that
#' never wins averages zero however present it is in the corpus. That number
#' describes the method rather than the label. Independent binary items are
#' what measure presence. See [template_items()].
#'
#' @section What replaces endorsement:
#' There is no single-label endorsement rate for a forced choice. The summary is
#' the winner distribution, meaning how often each label was chosen, reported
#' per question.
#'
#' A label that never wins has not necessarily failed. A forced choice returns
#' one label per question per document, so any label that is present but never
#' dominant will show zero. Running the same labels as independent binary items
#' is what distinguishes the cases.
#'
#' @section Leaked mass means something different here:
#' For independent binary items, mass outside the labels often signals a
#' compound question. For labels scored on discovered first tokens, it is mostly
#' the gap left by that approximation, since a first token's probability is an
#' upper bound on the label's.
#'
#' @param results A data frame, the flat frame `item_report()` builds with
#'   `flat_results()`.
#' @param ok A data frame of the successful rows, precomputed by the caller.
#' @param order As in [item_report()].
#' @return A tibble, one row per question and label. Questions in instrument
#'   order, labels within a question in declared order, or by how often they
#'   won under `order = "size"`.
#' @keywords internal
#' @noRd
choice_report <- function(results, ok, order = "declared") {
  # Grouped by document and question. A single-item exclusive instrument has one
  # winner per document, and a multi-item instrument has one per question.
  winners <- winning_rows(ok)

  n_docs <- dplyr::n_distinct(ok$id)
  # Per question, since a call can fail on one question and not on another, and
  # `share_won` is defined over the documents scored on that question.
  scored_n <- ok %>%
    dplyr::group_by(.data$item) %>%
    dplyr::summarise(n_scored = dplyr::n_distinct(.data$id), .groups = "drop")
  lv <- item_levels(results)
  ll <- label_levels(results)

  out <- ok %>%
    dplyr::distinct(.data$item, .data$label) %>%
    dplyr::left_join(
      winners %>% dplyr::count(.data$item, .data$label, name = "n_won"),
      by = c("item", "label")
    ) %>%
    dplyr::left_join(scored_n, by = "item") %>%
    dplyr::mutate(
      n_won = dplyr::coalesce(.data$n_won, 0L),
      share_won = .data$n_won / .data$n_scored
    ) %>%
    dplyr::select("item", "label", "n_won", "share_won") %>%
    # Questions always in instrument order. Labels follow `order`, and the
    # default is declared because frequency is one arrange() away while
    # declaration order needs the instrument attribute to recover. The `if`
    # runs inside the data mask and returns one vector either way, which is
    # how this avoids taking a dependency on rlang for a splice.
    dplyr::arrange(
      match(.data$item, lv),
      if (order == "size") {
        dplyr::desc(.data$n_won)
      } else {
        match(.data$label, ll)
      }
    )

  per_doc <- ok %>%
    dplyr::group_by(.data$id, .data$item) %>%
    dplyr::summarise(
      prob_other = .data$prob_other[1],
      won_prob = max(.data$prob),
      .groups = "drop"
    )

  message(paste0(
    "Choice instrument\n\n",
    fmt_fields(list(
      Documents = format(n_docs, big.mark = ","),
      Items = as.character(dplyr::n_distinct(ok$item)),
      Labels = as.character(dplyr::n_distinct(ok$label)),
      `Leaked mass` = sprintf(
        "max %.4f, mean %.4f",
        max(per_doc$prob_other),
        mean(per_doc$prob_other)
      ),
      Winning = sprintf("probability min %.3f", min(per_doc$won_prob))
    )),
    "\n"
  ))

  out
}

#' Report for an instrument with scored labels
#'
#' @description
#' Called by [item_report()] when the labels carry `scores`. Reports each
#' question's labels in score order alongside the distribution of the expected
#' score across the corpus.
#'
#' @section What the expected score is:
#' One number per document and question, the sum of each label's value weighted
#' by its probability, after those probabilities have been rescaled to sum to 1
#' across the labels. It uses the whole distribution rather than
#' the winning label, so a document split between two adjacent labels lands
#' between them rather than on one.
#'
#' `mean_expected` and `sd_expected` are constant within a question and repeat
#' down its label rows, the way `prob_other` does on the results. For an
#' instrument of aspects sharing one scale, that column is the per-aspect score.
#'
#' @section Reading it against the winner counts:
#' A label that never wins can still move the expected score, since it
#' contributes its share of the probability on every document. The two columns
#' answer different questions, and a label with `n_won` at zero and a
#' non-trivial share of the mass is one the model considers without ever
#' choosing.
#'
#' @param results A data frame, the flat frame `item_report()` builds with
#'   `flat_results()`.
#' @param ok A data frame of the successful rows, precomputed by the caller.
#' @param order As in [item_report()].
#' @return A tibble, one row per question and label. Questions in instrument
#'   order, labels within a question in declared order, which on a scale is
#'   score order, or by how often they won under `order = "size"`.
#' @keywords internal
#' @noRd
scored_report <- function(results, ok, order = "declared") {
  scores <- attr(results, "instrument")$scores
  lv <- item_levels(results)
  ll <- label_levels(results)

  winners <- winning_rows(ok)

  n_docs <- dplyr::n_distinct(ok$id)
  # Per question, since a call can fail on one question and not on another, and
  # `share_won` is defined over the documents scored on that question.
  scored_n <- ok %>%
    dplyr::group_by(.data$item) %>%
    dplyr::summarise(n_scored = dplyr::n_distinct(.data$id), .groups = "drop")

  per_doc <- ok %>%
    dplyr::group_by(.data$id, .data$item) %>%
    dplyr::summarise(
      expected = .data$expected_score[1],
      prob_other = .data$prob_other[1],
      # Recomputed rather than carried. The same formula score.R used,
      # over the renormalized probabilities of this call, which the
      # group holds.
      entropy = shannon(.data$prob_renorm),
      .groups = "drop"
    )

  per_item <- per_doc %>%
    dplyr::group_by(.data$item) %>%
    dplyr::summarise(
      mean_expected = mean(.data$expected),
      sd_expected = stats::sd(.data$expected),
      .groups = "drop"
    )

  out <- ok %>%
    dplyr::distinct(.data$item, .data$label) %>%
    dplyr::mutate(score = unname(scores[.data$label])) %>%
    dplyr::left_join(
      winners %>% dplyr::count(.data$item, .data$label, name = "n_won"),
      by = c("item", "label")
    ) %>%
    dplyr::left_join(scored_n, by = "item") %>%
    dplyr::mutate(
      n_won = dplyr::coalesce(.data$n_won, 0L),
      share_won = .data$n_won / .data$n_scored
    ) %>%
    dplyr::left_join(per_item, by = "item") %>%
    dplyr::select(
      "item",
      "label",
      "score",
      "n_won",
      "share_won",
      "mean_expected",
      "sd_expected"
    ) %>%
    # Questions always in instrument order. Labels follow `order`, and on a
    # scored instrument declared is also score order, since a scale is declared
    # up or down.
    dplyr::arrange(
      match(.data$item, lv),
      if (order == "size") {
        dplyr::desc(.data$n_won)
      } else {
        match(.data$label, ll)
      }
    )

  n_lab <- dplyr::n_distinct(ok$label)

  # One question has no range across questions to report, so it reports its own
  # spread instead. Printing "-0.499 to -0.499 across questions" says nothing.
  score_line <- if (nrow(per_item) == 1L) {
    sprintf(
      "expected mean %.3f, sd %.3f",
      per_item$mean_expected[1],
      per_item$sd_expected[1]
    )
  } else {
    sprintf(
      "expected %.3f to %.3f across items",
      min(per_item$mean_expected),
      max(per_item$mean_expected)
    )
  }

  message(paste0(
    "Scored instrument\n\n",
    fmt_fields(list(
      Documents = format(n_docs, big.mark = ","),
      Items = as.character(nrow(per_item)),
      Labels = as.character(n_lab),
      Score = score_line,
      Entropy = sprintf(
        "mean %.3f, max possible %.3f",
        mean(per_doc$entropy),
        log(n_lab)
      ),
      `Leaked mass` = sprintf(
        "max %.4f, mean %.4f",
        max(per_doc$prob_other),
        mean(per_doc$prob_other)
      )
    )),
    "\n"
  ))

  out
}

#' Compute endorsement rates across a range of thresholds
#'
#' @description
#' Computes the endorsement rate per item at each cut point, which shows how
#' much of an item's rate is a property of the documents and how much is a
#' property of where the cut falls.
#'
#' @section More than two labels:
#' This assumes a two-label instrument. Thresholding one label's probability
#' asks whether that label is present, which is a question only a two-label
#' instrument answers. On an instrument of three or more labels the labels
#' compete for one pool of probability, so a label can win at 0.3 and lose at
#' 0.6, and its rate at a series of cut points describes the arithmetic rather
#' than the corpus. On a scored instrument the labels are levels of a scale, and
#' a cut point on one level is not a measurement of anything.
#'
#' The function does not refuse, because a cut point is the whole subject here
#' and there is no other quantity for it to report. It runs on whatever you pass
#' and the figures will be arithmetically correct.
#'
#' @param results A `fathom_results` object from [measure()] or
#'   [read_checkpoints()].
#' @param target A string naming the label counted as endorsement. Defaults to
#'   the first.
#' @param thresholds A numeric vector of cut points to evaluate. Default is
#'   `seq(0.1, 0.9, by = 0.1)`.
#' @param value A string naming the probability column to threshold. Defaults to
#'   `"prob_renorm"`, the figure after the label probabilities have been
#'   rescaled to sum to 1. That is the question a cut point is usually being
#'   asked to answer, which is whether documents pile up near the boundary
#'   between your labels, and it is the quantity everything downstream compares
#'   against.
#'
#'   `"prob"` is the raw probability, so a sweep over it also moves with mass
#'   the model spent outside your labels. That confounds two things, and a rate
#'   that shifts under it may be reporting compliance rather than the corpus.
#'   Use it when the claim is about whether the model committed.
#' @return A tibble with one row per item and threshold, items in instrument
#' order and thresholds ascending within an item. The message printed alongside
#' is a ranking rather than a table and stays widest swing first.
#'
#' - `item` is the question name.
#' - `threshold` is the cut point, one row per value in `thresholds`.
#' - `n` counts the documents scored on this item. The same on every row of an
#'   item, since the cut point changes which documents count as endorsing and
#'   not how many were asked.
#' - `n_endorse` counts the documents whose `value` for `target` reached the
#'   cut.
#' - `endorse` is `n_endorse` over the documents with a `value` to cut. A
#'   document where no label appeared has no `prob_renorm`, so under the default
#'   it leaves this denominator, and `endorse` is `n_endorse` over `n` only when
#'   no such document exists. That is the split [item_report()] makes between
#'   `endorse_renorm` and `endorse`. This is the column to read down, and the
#'   spread from its smallest to largest value within an item is what the
#'   message ranks on.
#' @seealso [item_report()] for the winner distribution, and for `mean_expected`
#'   on a scored instrument, which needs no cut.
#' @examples
#' # Results from a real run ship with the package, so this needs no server.
#' p <- system.file("extdata", "vignette_results.rds", package = "fathom")
#' baked <- readRDS(p)
#' threshold_sensitivity(baked$res_indep, target = "YES")
#' @export
threshold_sensitivity <- function(
  results,
  target = NULL,
  thresholds = seq(0.1, 0.9, by = 0.1),
  value = c("prob_renorm", "prob")
) {
  # No columns are wanted from $calls, but the body indexes results in two
  # dimensions, which a list refuses. The keys alone flatten it.
  results <- flat_results(results, character(0))
  value <- match.arg(value)
  target <- target %||% results$label[1]
  check_target(target, unique(results$label))

  d <- results[results$call_ok & results$label == target, , drop = FALSE]
  if (nrow(d) == 0L) {
    stop(fmt_err("no successful records to threshold"), call. = FALSE)
  }
  lv <- item_levels(results)

  out <- dplyr::bind_rows(lapply(thresholds, function(th) {
    d %>%
      dplyr::group_by(.data$item) %>%
      dplyr::summarise(
        threshold = th,
        n = dplyr::n(),
        n_endorse = sum(.data[[value]] >= th, na.rm = TRUE),
        endorse = mean(.data[[value]] >= th, na.rm = TRUE),
        .groups = "drop"
      )
  }))

  swing <- out %>%
    dplyr::group_by(.data$item) %>%
    dplyr::summarise(
      swing = max(.data$endorse) - min(.data$endorse),
      .groups = "drop"
    ) %>%
    # Not instrument order. This note is a ranking, and it says so.
    dplyr::arrange(dplyr::desc(.data$swing))

  # One line per item, aligned under the note's text, so an item never lands on
  # a different line from its value.
  w <- max(nchar(swing$item))
  message(paste(
    c(
      fmt_note("Note", "Endorsement swing across the range, widest first."),
      paste0(
        strrep(" ", NOTE_WIDTH),
        sprintf("%-*s  %.2f", w, swing$item, swing$swing)
      )
    ),
    collapse = "\n"
  ))

  # Instrument order, then up the range within an item, so each item's rows read
  # as one curve.
  out %>% dplyr::arrange(match(.data$item, lv), .data$threshold)
}


#' Split a total budget across items without exceeding any item's supply
#'
#' Largest remainder, with each item capped at the rows it actually has. The
#' remainder is handed out one row at a time so an item that runs out passes
#' its share to the others rather than the total silently coming up short.
#'
#' @param raw A numeric vector of fractional budgets, summing to the total.
#' @param sizes An integer vector of rows available per item.
#' @param total An integer giving the rows to allocate, already capped at what
#'   exists.
#' @return Integer budgets summing to `total` where supply allows.
#' @keywords internal
#' @noRd
allocate_budget <- function(raw, sizes, total) {
  a <- pmin(as.integer(floor(raw)), sizes)
  frac <- raw - floor(raw)
  while (sum(a) < total) {
    room <- sizes - a
    if (!any(room > 0L)) {
      break
    }
    cand <- which(room > 0L)
    pick <- cand[which.max(frac[cand])]
    a[pick] <- a[pick] + 1L
    frac[pick] <- frac[pick] - 1
  }
  a
}

#' Systematic probability-proportional-to-size draw
#'
#' Walks the cumulative chance in steps of one from a uniform random start, so
#' the drawn size is exactly `sum(chance)` on every call rather than in
#' expectation, and each row is selected with the chance it was given. The frame
#' is shuffled first, because a systematic step locks onto whatever periodicity
#' the ordering happens to carry.
#'
#' @param chance A numeric vector of selection chances summing to the intended
#'   sample size.
#' @return Row indices, sorted.
#' @keywords internal
#' @noRd
pps_draw <- function(chance) {
  n <- as.integer(round(sum(chance)))
  if (n <= 0L || length(chance) == 0L) {
    return(integer(0))
  }
  o <- sample(length(chance))
  cum <- cumsum(chance[o])
  picks <- stats::runif(1, 0, 1) + seq_len(n) - 1
  idx <- pmin(findInterval(picks, cum) + 1L, length(chance))
  sort(o[idx])
}

#' Draw a blinded sample for human coding
#'
#' @description
#' Draws a sheet of documents and questions with a blank column for a coder,
#' and the model's answer withheld, and writes it as a CSV when `file` is
#' given. A coder shown the model's label agrees with it more often, so
#' withholding it is what makes the resulting agreement figure interpretable.
#'
#' @section The draw:
#' Every document-item pair has the same chance of being drawn, and the budget
#' is split across questions in proportion to how many pairs each one has, so a
#' figure computed on the sheet is already a figure about the corpus.
#'
#' Item is a stratification variable rather than a filter, so the draw runs
#' within each question and `n` stays the total across them. The draw is
#' systematic, so the sample is exactly `n` rows on every call. An `n` at or
#' above the number of pairs is a census.
#'
#' @param results A `fathom_results` object from [measure()] or
#'   [read_checkpoints()].
#' @param docs A character vector, or the data frame passed to [measure()].
#'   Default is `NULL`, which uses the documents a character-vector run stored
#'   on the results.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param text,id Strings naming the text and identifier columns, if `docs` is a
#'   data frame. `text` is then required, and `id` defaults to the row position.
#' @param n An integer giving the total document-item pairs to draw, across all
#'   questions. Default is 200.
#' @param items A character vector restricting the draw to specific items, or
#'   `NULL` (the default) for all of them.
#' @param labels_allowed A character vector of permitted values for the
#'   `human_label` column, written into the sheet so a coder knows the answer
#'   scheme. Defaults to the instrument label names.
#' @param image A string naming a column in `docs` holding the media for each
#'   document, so the sheet carries it as well as any text. A character column
#'   is one file per document. A list column holds several, such as the frames
#'   [prepare_video()] returns, and they share one cell joined with `|`. Default
#'   is `NULL`.
#' @param copy_media A logical value indicating whether to copy the files
#'   `image` points at into a folder beside the CSV and rewrite the column
#'   relative to it. Acts only when both `file` and `image` are given. `FALSE`
#'   leaves the original absolute paths, which is right when the coders share
#'   the filesystem the sheet was written on and wrong otherwise. Default is
#'   `TRUE`.
#' @param file A string giving the path to write the CSV to, or `NULL` (the
#'   default) to write nothing.
#' @param seed An integer giving the seed for the draw, so the same call draws
#'   the same rows. The session's random stream is put back afterwards, so this
#'   seed reaches the draw and nothing after it. `NULL` draws from the session's
#'   stream instead. Default is 1234.
#' @return A tibble with one row per document-item pair drawn, in random order
#' so the order leaks nothing about the draw. The same document appears once per
#' item it was drawn on.
#'
#' The columns are for the coder, the document last so the others stay on
#' screen beside it.
#'
#' - `id` is the document identifier, matching the results.
#' - `item` names which question this row is about.
#' - `question` is the question text as the model received it, so a coder
#'   answers the same question rather than a paraphrase of it. `NA` when the
#'   results carry no instrument.
#' - `allowed` holds the permitted answers, pipe-separated, so the sheet states
#'   its own answer scheme.
#' - `human_label` is blank, for the coder to fill with one value from
#'   `allowed`. The model's answer is deliberately absent.
#' - `note` is blank, for the coder. Nothing reads it, and it exists so a coder
#'   has somewhere to record a case they were unsure about.
#' - `text` is the document, in full.
#' - `image` is the file `image` named, or `NA` when none was. Several files, a
#'   video window's frames, are joined in order with `|`, the way `allowed`
#'   joins the answers. Rewritten relative to the CSV when `copy_media` acted.
#' @seealso [validate_sample()], which reads the coded sheet, and
#'   [coder_reference()] for collapsing several coders into one.
#' @examples
#' # Results from a real run ship with the package, so this needs no server.
#' p <- system.file("extdata", "vignette_results.rds", package = "fathom")
#' baked <- readRDS(p)
#' sheet <- coding_sample(baked$res_six, parlasent, text = "sentence",
#'   id = "id", n = 20, seed = 1234)
#' head(sheet)
#' @export
coding_sample <- function(
  results,
  docs = NULL,
  ...,
  text = NULL,
  id = NULL,
  image = NULL,
  n = 200L,
  items = NULL,
  labels_allowed = NULL,
  copy_media = TRUE,
  file = NULL,
  seed = 1234L
) {
  no_dots(..., .fn = "coding_sample")
  results <- flat_results(results, "margin")
  b <- attr(results, "instrument")
  tx <- as_docs(docs, text, id, results)

  # Joined onto the same ids as the text, so a sheet can carry a caption and
  # the picture it describes. Read straight off `docs` rather than off the
  # results, since nothing in a results row holds a path.
  if (!is.null(image)) {
    if (!is.data.frame(docs) || !image %in% names(docs)) {
      stop(
        fmt_err(paste0(
          "`image` must name a column in `docs`. Columns are ",
          if (is.data.frame(docs)) {
            paste(names(docs), collapse = ", ")
          } else {
            "none, since `docs` is not a data frame"
          },
          "."
        )),
        call. = FALSE
      )
    }
    doc_ids <- if (is.null(id)) {
      as.character(seq_len(nrow(docs)))
    } else {
      as.character(docs[[id]])
    }
    # A video window is a list column, one vector of frame paths per document.
    # as.character() pastes each vector into one string that names no file, so
    # the paths are joined with `|` instead, in order, the way `allowed` joins
    # the answers.
    joined <- vapply(
      as_media(docs[[image]]),
      function(p) {
        p <- p[!is.na(p) & nzchar(p)]
        if (length(p)) paste(p, collapse = "|") else NA_character_
      },
      character(1)
    )
    tx$.fathom_image <- joined[match(tx$id, doc_ids)]
  }

  scored <- results[results$call_ok, , drop = FALSE]
  if (!is.null(items)) {
    scored <- scored[scored$item %in% items, , drop = FALSE]
  }
  if (nrow(scored) == 0L) {
    stop(fmt_err("no records to sample"), call. = FALSE)
  }

  # One row per document-item, so which row represents the pair is settled by
  # the argmax instead of by an argument.
  local_seed(seed)
  d <- scored %>%
    dplyr::group_by(.data$id, .data$item) %>%
    dplyr::slice_max(.data$prob, n = 1L, with_ties = FALSE) %>%
    dplyr::ungroup()

  # `margin` is NA on exactly the all-absent branch of score_labels(), since
  # failed calls left with `results$call_ok` above. So this is a pair where no
  # label appeared anywhere in the returned distribution, which leaves nothing
  # for a human to agree or disagree with.
  broken <- is.na(d$margin) | is.na(d$prob_renorm)
  if (any(broken)) {
    message(fmt_note(
      "Note",
      sprintf(
        paste0(
          "%d record(s) had no label anywhere in the returned distribution ",
          "and are excluded from the sample."
        ),
        sum(broken)
      )
    ))
    d <- d[!broken, , drop = FALSE]
  }
  if (nrow(d) == 0L) {
    stop(fmt_err("no scorable records to sample"), call. = FALSE)
  }

  its <- unique(d$item)
  sizes <- vapply(its, function(it) sum(d$item == it), integer(1))

  # Proportional to what each question holds, which is what makes the sheet
  # representative across questions as well as within one.
  raw <- n * sizes / sum(sizes)
  alloc <- allocate_budget(raw, sizes, min(n, nrow(d)))

  if (sum(alloc) < n) {
    message(fmt_note(
      "Note",
      sprintf(
        "Only %d of the %d requested pairs were available.",
        sum(alloc),
        n
      )
    ))
  }

  # Every pair in a question has the same chance, one at most, which is a
  # census once the budget reaches what the question holds.
  keep <- integer(0)
  for (k in seq_along(its)) {
    ix <- which(d$item == its[k])
    if (alloc[k] > 0L) {
      ch <- rep(min(1, alloc[k] / length(ix)), length(ix))
      keep <- c(keep, ix[pps_draw(ch)])
    }
  }

  take <- d[keep, , drop = FALSE]

  # Written into the sheet so a coder has the answer scheme in front of them.
  # Without it three coders return "yes", "Y", and "1", and you find out at
  # analysis time.
  allowed <- labels_allowed %||% label_levels(results)
  allowed_cell <- paste(allowed, collapse = " | ")

  take <- take[sample(nrow(take)), , drop = FALSE] # shuffle so order leaks nothing

  out <- take %>%
    dplyr::left_join(tx, by = "id") %>%
    dplyr::transmute(
      .data$id,
      .data$item,
      question = if (is.null(b)) NA_character_ else unlist(b$items)[.data$item],
      allowed = allowed_cell,
      human_label = NA_character_,
      note = NA_character_,
      text = .data$.fathom_text,
      # NA rather than absent when no `image` was named, so the sheet has one
      # shape and a coder opening two of them sees the same columns.
      image = if (is.null(image)) NA_character_ else .data$.fathom_image
    )

  # transmute() preserves the fathom_results class while dropping the columns
  # its print method needs. This is a coding sheet rather than results, so strip
  # the class.
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out <- stamp_provenance(out)

  drawn_k <- which(alloc > 0L)
  wid <- max(nchar(its[drawn_k]))
  drawn_lines <- vapply(
    drawn_k,
    function(k) {
      # The question is named only when there is more than one. On a single
      # item the Pairs line above has already said so.
      sprintf(
        "%s%d of %d",
        if (length(its) > 1L) formatC(its[k], width = -(wid + 2L)) else "",
        alloc[k],
        sizes[k]
      )
    },
    character(1)
  )
  n_items <- dplyr::n_distinct(out$item)
  message(paste0(
    "Blinded coding sample\n\n",
    fmt_fields(list(
      Pairs = sprintf(
        "%d across %d item%s",
        nrow(out),
        n_items,
        if (n_items == 1L) "" else "s"
      ),
      Drawn = drawn_lines
    ))
  ))
  message(fmt_note(
    "Tip",
    paste0(
      "Pass the completed sheet to validate_sample() together with these ",
      "results."
    )
  ))

  if (!is.null(file) && !is.null(image) && isTRUE(copy_media)) {
    out <- copy_sheet_media(out, file)
  }

  if (!is.null(file)) {
    if (!requireNamespace("readr", quietly = TRUE)) {
      utils::write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    } else {
      readr::write_csv(out, file)
    }
    message(fmt_note("Note", paste0("Wrote ", file)))
  }
  out
}

#' Copy the media a sheet references to a folder beside it
#'
#' An absolute path is the one thing that does not survive a sheet being
#' emailed, since a sheet that points at `/Users/someone/corpus` stops working
#' once it leaves the session. So the files are copied next to the CSV and the
#' column is rewritten relative to it, and a coder who opens the folder and the
#' spreadsheet together sees the documents.
#'
#' A path that does not resolve is left as it was rather than being blanked,
#' since the sheet then still says what was meant even though the file is
#' missing.
#'
#' @param out A data frame holding the sheet, carrying an `image` column of
#'   paths, several to a cell joined with `|`.
#' @param file A string giving the path the CSV is being written to.
#' @return `out`, with `image` rewritten relative to `file`'s folder.
#' @keywords internal
#' @noRd
copy_sheet_media <- function(out, file) {
  dir <- file.path(
    dirname(file),
    paste0(tools::file_path_sans_ext(basename(file)), "_media")
  )
  # A cell can hold several paths joined with `|`, a video window's frames, so
  # the files to copy are the union of what every cell names.
  has <- !is.na(out$image) & nzchar(out$image)
  src <- unique(unlist(strsplit(out$image[has], "|", fixed = TRUE)))
  src <- src[file.exists(src)]
  if (length(src) == 0L) {
    return(out)
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # Names can repeat across folders, so the destination is keyed on the path
  # rather than on the basename, and two files called page1.png stay two files.
  dest <- file.path(
    dir,
    paste0(
      substr(vapply(src, digest::digest, character(1), algo = "md5"), 1, 8),
      "_",
      basename(src)
    )
  )
  ok <- file.copy(src, dest, overwrite = TRUE)
  rel <- stats::setNames(
    file.path(basename(dir), basename(dest)),
    src
  )

  message(fmt_note(
    "Note",
    sprintf("Copied %d of %d file(s) to %s", sum(ok), length(src), dir),
    paste0(
      "The `image` column is relative to the sheet, so keep the two ",
      "together. `copy_media = FALSE` leaves the original paths."
    )
  ))

  out$image[has] <- vapply(
    strsplit(out$image[has], "|", fixed = TRUE),
    function(p) {
      hit <- p %in% names(rel)
      p[hit] <- unname(rel[p[hit]])
      paste(p, collapse = "|")
    },
    character(1)
  )
  out
}
