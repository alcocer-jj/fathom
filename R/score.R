#' Numerically stable log(sum(exp(x)))
#'
#' Aggregates the log probabilities of several surface variants into one log
#' probability for the label they share. Zero-length input returns `-Inf`. No
#' variant of that label appeared in the returned top-k, so the label has
#' probability zero. On saturated items the losing label can fall out of the
#' top-k this way.
#'
#' @param x A numeric vector of log probabilities. May be empty.
#' @return Scalar.
#' @keywords internal
#' @noRd
logsumexp <- function(x) {
  if (length(x) == 0) {
    return(-Inf)
  }
  m <- max(x)
  m + log(sum(exp(x - m)))
}

#' Recompute the expected score against an instrument's current values
#'
#' `scores` never reaches the prompt, so changing them changes no token the
#' model sees and rewording is not what happened. `item_hashes()` therefore
#' leaves them out, and re-extracting a corpus to change a multiplication would
#' be waste.
#'
#' What that costs is staleness. A restored checkpoint carries the expected
#' score computed under whatever values were in force when it was written, and
#' nothing in the row records which. So the quantity is recomputed from
#' `prob_renorm` on every path that assembles results, which makes it a function
#' of the instrument in hand rather than of the instrument that happened to run.
#'
#' Idempotent on freshly scored rows, since it recomputes what `score_labels()`
#' already produced.
#'
#' @param res A results tibble with `id`, `item`, `label`, and `prob_renorm`.
#' @param instrument A `fathom_instrument`.
#' @return `res`, with `expected_score` consistent with `instrument$scores`.
#' @keywords internal
#' @noRd
apply_scores <- function(res, instrument) {
  if (nrow(res) == 0L || !"prob_renorm" %in% names(res)) {
    return(res)
  }
  sc <- instrument$scores

  # An instrument without scores has no expected value, whatever a checkpoint
  # written under an earlier instrument may hold.
  if (is.null(sc)) {
    res$expected_score <- NA_real_
    return(res)
  }

  # A label the instrument no longer carries would give NA and silently poison
  # the sum, so it is refused rather than propagated. In practice the hash
  # catches a changed label set first, and this is the case where it did not.
  unknown <- setdiff(unique(res$label), names(sc))
  if (length(unknown)) {
    stop(
      fmt_err(paste0(
        "these results carry label(s) the instrument has no score for. ",
        paste(unknown, collapse = ", "),
        ". The checkpoint was written by a ",
        "different instrument than the one supplied."
      )),
      call. = FALSE
    )
  }

  grp <- paste(res$id, res$item, sep = "\r")
  es <- vapply(
    split(seq_len(nrow(res)), grp),
    function(ix) {
      pr <- res$prob_renorm[ix]
      if (all(is.na(pr))) NA_real_ else sum(pr * sc[res$label[ix]])
    },
    numeric(1)
  )
  res$expected_score <- unname(es[grp])
  res
}

#' Which returned tokens count as label i
#'
#' Two rules, and the instrument says which one applies.
#'
#' Prefix matching asks whether a token opens any of the label's declared
#' variants. It is the default because a model that writes BUDGET must emit a
#' prefix of BUDGET first, which follows from every tokenizer in use
#' detokenizing by concatenation rather than from any one model's vocabulary.
#' Under it, `BU` counts as BUDGET without anything having been discovered
#' first.
#'
#' Variants matter here. An instrument declaring SI and SÍ means both are forms
#' the model might write, so a token opening either is evidence for the label.
#'
#' Exact matching asks whether a token equals a label or one of its registered
#' variants. It is what applies when probing has run and recorded what the model
#' actually emits, and when an instrument's labels do not differ at their first
#' character, since prefix matching is then undefined.
#'
#' @section Why first characters have to differ:
#' If no two labels share a first character across any of their variants, then
#' no two labels share any prefix, so a token opening one cannot open another
#' and every match resolves uniquely at any length. That condition is checked at
#' construction, which is why nothing here has to arbitrate between candidates.
#'
#' @param norm A character vector of normalized tokens from the returned
#'   distribution.
#' @param instrument A `fathom_instrument`.
#' @param i An integer giving the index of the label.
#' @return Logical, one per token.
#' @keywords internal
#' @noRd
match_token <- function(norm, instrument, i) {
  variants <- instrument$normalized[[i]]
  if (isTRUE(instrument$exact_match)) {
    return(norm %in% variants)
  }
  # Every declared variant, not just the label name. An instrument that declares
  # SI and SÍ means both are things the model might write, and a token opening
  # either is evidence for the label. Checking the name alone would drop the
  # accented form, since Í is not I.
  nzchar(norm) &
    vapply(norm, function(t) any(startsWith(variants, t)), logical(1))
}

#' Score a label set against the position-1 token distribution
#'
#' @description
#' Converts a top-k log probability list into one row per label. Nothing here
#' branches on the number of labels. Log-sum-exp within each label's variants,
#' softmax across labels for the renormalized column, and whatever mass sits on
#' no label at all becomes `prob_other`. Binary is K = 2 and gets no special
#' handling.
#'
#' @section The three probability columns:
#' `prob` is unconditional P(next token is this label) and includes
#' contamination from mass on formatting tokens, so labels need not sum to 1.
#' `prob_renorm` is P(label | some label), which is what constrained decoding
#' would produce, computed in arithmetic rather than by restricting generation.
#' `prob_other` is the mass on no label at all.
#'
#' @section How a token becomes a label:
#' By default a token counts as a label when it opens it. `BU` counts as BUDGET.
#' That works because a model writing BUDGET must emit a prefix of it first, and
#' it needs no probing, since the property follows from how these tokenizers
#' detokenize rather than from any model's vocabulary.
#'
#' It requires the labels to differ at their first character, which
#' [instrument()] enforces. Given that, no two labels share any prefix and every
#' match resolves to one label.
#'
#' An instrument built with `exact = TRUE`, or one whose labels cannot satisfy
#' the condition and was probed instead, matches against the label and its
#' registered variants rather than by prefix.
#'
#' @section Uncertainty columns:
#' `entropy` and `margin` describe the call rather than any one label, so both
#' repeat down the rows the way `prob_other` does. Both read the renormalized
#' distribution, which measures uncertainty among your labels. Entropy is in
#' nats, with a maximum of `log(K)`. Margin is the gap between the largest and
#' second-largest renormalized probability.
#'
#' On a binary instrument neither carries information `prob` does not already
#' hold, since margin is `abs(2p - 1)` and entropy is a function of `p`. They
#' earn their place from three labels up.
#'
#' @section Expected score:
#' `expected_score` is the sum of each label's `scores` value weighted by its
#' renormalized probability, and `NA` when the instrument carries no scores. It
#' reads `prob_renorm` rather than `prob`, since an expected value needs a
#' distribution that sums to one.
#'
#' @section Leaked mass measures confidence, not correctness:
#' Low `prob_other` and high top-1 probability indicate the model was sure of
#' its answer. Neither says whether the answer was right. Use [inspect_cases()]
#' to read the documents behind an item.
#'
#' @param lp_list A list giving the `top_logprobs` for generated position 1.
#' @param instrument A `fathom_instrument`.
#' @return Tibble, one row per label.
#' @keywords internal
#' @noRd
score_labels <- function(lp_list, instrument) {
  toks <- vapply(
    lp_list,
    function(z) as.character(z$token %||% ""),
    character(1)
  )
  lps <- vapply(
    lp_list,
    function(z) as.numeric(z$logprob %||% NA_real_),
    numeric(1)
  )

  o <- order(lps, decreasing = TRUE) # returned sorted, but do not rely on it
  toks <- toks[o]
  lps <- lps[o]
  norm <- stringi::stri_trans_toupper(trimws(toks))

  ls <- vapply(
    seq_along(instrument$normalized),
    function(i) logsumexp(lps[match_token(norm, instrument, i)]),
    numeric(1)
  )
  names(ls) <- names(instrument$normalized)
  p <- exp(ls)
  p <- pmin(p, 1) # guard float overshoot at logprob ~ 0

  total <- sum(p)
  any_label <- total > 0

  # The renormalized distribution, computed once. Everything below reads it
  # rather than the raw top-k, which would largely restate prob_other.
  pr <- if (any_label) unname(p / total) else rep(NA_real_, length(p))

  if (any_label) {
    nz <- pr[pr > 0]
    ent <- -sum(nz * log(nz))
    srt <- sort(pr, decreasing = TRUE)
    mrg <- srt[1] - srt[2]
    es <- if (is.null(instrument$scores)) {
      NA_real_
    } else {
      sum(pr * instrument$scores)
    }
  } else {
    ent <- NA_real_
    mrg <- NA_real_
    es <- NA_real_
  }

  tibble::tibble(
    label = names(instrument$labels),
    logprob = unname(ls),
    prob = unname(p),
    prob_renorm = pr,
    present = is.finite(unname(ls)),
    top1_token = toks[1],
    top1_prob = min(1, exp(lps[1])),
    prob_other = max(0, 1 - total),
    entropy = ent,
    margin = mrg,
    expected_score = es,
    # NA rather than the first label. which.max() on an all-zero vector returns
    # 1, so an all-absent call would otherwise name a label carrying no mass.
    winner = if (any_label) {
      names(instrument$labels)[which.max(p)]
    } else {
      NA_character_
    }
  )
}
