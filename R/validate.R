#' Validate a completed coding sheet against the model
#'
#' @description
#' Takes the sheet [coding_sample()] wrote, with `human_label` filled in, and
#' reports how well the model agreed with the coder.
#'
#' The sheet carries no probabilities, which is what makes it blind. This joins
#' back to the results on `id` and `item` to recover them, so pass the same
#' results object the sample came from.
#'
#' `vignette("complete_run")` works a run end to end.
#'
#' @section Several coders:
#' `sheet` takes a named list as well as one sheet, so
#' `validate_sample(list(ana = ana, luis = luis), res)` scores the model against
#' each coder in one call and adds `$ceiling` and `$gap`. Each coder is scored
#' through the one-sheet path, so no figure is computed twice or computed
#' differently, and the model tables gain a `coder` column after `item`.
#'
#' Coders need not have worked every row. `$ceiling$alpha` uses every row two
#' or more coders reached, and each pair in `$ceiling$pairs` falls back to the
#' rows both of them saw, so the two tables can rest on different counts.
#'
#' `$gap` resamples whatever `ci` is set to, since the gap is read through its
#' interval. Every replicate redraws the documents once and recomputes both
#' sides on that draw, because the two share the documents they rest on.
#'
#' A list of one is refused. A sheet [coder_reference()] consolidated warns when
#' it is placed in a list, since agreeing with a consensus is easier than
#' agreeing with one person.
#'
#' @section Ordered labels:
#' Ordered labels need `level = "ordinal"`. The default charges every
#' disagreement the same, which is right for unordered labels and wrong for a
#' scale, where two coders one step apart have agreed more than two coders at
#' opposite ends. Pass `labels` in scale order, least to greatest, or an
#' `instrument` carrying `scores`. Nothing is inferred, because no order can be
#' recovered from the sheets.
#'
#' @section A soft reference:
#' A sheet carrying `human_prop` instead of `human_label` is scored as a
#' measurement and not as a classification. There is no second categorical
#' variable, so `$discrimination` and `$categories` are not returned. What comes
#' back is `$calibration`, with `brier` against the proportion, whatever
#' `calibration` is set to.
#'
#' @param sheet A data frame holding the completed coding sheet, or a string
#'   giving a path to the CSV. A named list of sheets scores the model against
#'   every coder in it and adds the ceiling and the gap. See the section below.
#' @param results A `fathom_results` object, the one the sheet was drawn from.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param level A string, `"nominal"` (the default) or `"ordinal"`. It decides
#'   how a disagreement is charged, so it selects the proper scoring rule in
#'   `$calibration` as well as the alpha.
#' @param labels A character vector giving, under `"ordinal"`, every label in
#'   scale order. Ignored under `"nominal"`. Default is `NULL`.
#' @param instrument A `fathom_instrument`, the one the run used. Under
#'   `"ordinal"` its `scores` supply the scale, so `labels` need not be retyped.
#'   An explicit `labels` wins over it. Default is `NULL`.
#' @param calibration A logical value indicating whether to return
#'   `$calibration`, for a run keeping `prob_renorm` and feeding it onward.
#'   Default is `FALSE`.
#' @param coded A logical value indicating whether to return `$coded`, the
#'   sheet joined to the model's answer. Default is `FALSE`.
#' @param ci A string choosing whether to report intervals. `"none"`, the
#'   default, returns none and the interval columns are absent rather than
#'   present and full of `NA`. `"bootstrap"` adds one bracketed column beside
#'   each figure.
#' @param conf_level A number giving the coverage of every interval reported,
#'   0.95 by default. It is stamped on the result as the `conf_level` attribute,
#'   and the print method states it under the first table that carries an
#'   interval.
#' @param bootstrap_n An integer giving the replicates for each interval. The
#'   default is a round two thousand. Below a hundred a percentile interval is
#'   reading a tail made of a handful of draws, and is refused.
#' @param seed An integer giving the seed for the bootstrap, so the intervals
#'   come out the same on every run with the same inputs. The session's random
#'   stream is put back on exit, so nothing drawn before this call reaches the
#'   intervals and this seed reaches nothing after it. `NULL` draws from the
#'   session's stream instead. Default is 1234.
#'
#' @return
#' An object of class `fathom_validation`, a list of tables. The model tables
#' carry `item` and, on the several-sheet path, `coder`. Under
#' `ci = "bootstrap"` each figure gains a bracketed `_ci` column beside it.
#'
#' | component | holds | when |
#' | :-- | :-- | :-- |
#' | `$discrimination` | how often the model chose the coder's label | a label sheet |
#' | `$categories` | precision, recall and AUC for each label | a label sheet |
#' | `$calibration` | how honest the probabilities were | `calibration = TRUE`, or a soft sheet |
#' | `$ceiling` | how well the coders agreed with each other | several sheets |
#' | `$gap` | the model against that ceiling | several sheets |
#' | `$coded` | the sheet joined to the model's answer | `coded = TRUE` |
#'
#' ## Discrimination
#'
#' One row per question, plus a pooled `(all items)` row where the instrument
#' has more than one.
#'
#' - `n` counts the coded rows.
#' - `agreement` is the share of rows where the model chose the coder's label.
#' - `alpha` is Krippendorff's alpha (Hayes and Krippendorff 2007) between the
#'   model and the coder, under the `level` asked for.
#'
#' ## Categories
#'
#' One row per label per question, with `label` and `n`, the rows the coder
#' gave that label.
#'
#' - `precision` is the share of the rows the model gave the label where the
#'   coder chose it too.
#' - `recall` is the share of the rows the coder gave the label where the model
#'   chose it too.
#' - `auc` is the chance that a row the coder gave the label gets a higher
#'   probability for it than a row the coder did not.
#'
#' All three count exact matches, so under `"ordinal"` a model one level off is
#' charged as a full miss. `alpha` and `rps` are the figures that respect the
#' order. On two labels the two `auc` rows are equal, since one label's
#' probability is one minus the other's.
#'
#' ## Calibration
#'
#' One row per question, plus the pooled row, with `n` and one score column
#' named for the rule `level` selects.
#'
#' - `brier` is squared error summed across categories, so on two labels a
#'   confident correct answer costs 0, a confident wrong one costs 2, and never
#'   committing costs 0.5.
#' - `rps` is the same sum on the cumulative probabilities and runs from 0 to
#'   `k - 1`. Lower is better for both. A soft reference stays on `brier`.
#'
#' ## Ceiling
#'
#' `$ceiling$alpha` is one row per question with `n_units`, the rows two or
#' more coders reached, and `alpha` over all coders at once. `$ceiling$pairs`
#' is one row per pair of coders per question, with `coder_a`, `coder_b`, `n`,
#' the rows both coded, `agreement` and `alpha`. This is what the coders
#' managed on these rows, and not a target.
#'
#' ## Gap
#'
#' One row per question.
#'
#' - `n` counts the rows the model and the coders share.
#' - `model_alpha` is the model's average alpha with the coders.
#' - `coder_alpha` is the coders' average alpha with each other on the same
#'   rows.
#' - `gap` is the first minus the second, positive where the model agreed with
#'   the coders more closely than they agreed with each other, and `gap_ci` is
#'   its interval.
#'
#' ## Coded
#'
#' The sheet joined to the model's label and its probabilities, one row per
#' coded document-item, or per document-item per coder on the several-sheet
#' path. It is returned because a design-based correction takes it as input
#' (Egami et al. 2023, Angelopoulos et al. 2023).
#'
#' The attributes carry `level`, `conf_level`, the number of coded rows as
#' `n_coded`, and on the several-sheet path the coder names as `coders`.
#' @references
#'   Angelopoulos, A. N., Bates, S., Fannjiang, C., Jordan, M. I., and Zrnic, T.
#'   (2023). Prediction-powered inference. \emph{Science} 382(6671), 669--674.
#'   \doi{10.1126/science.adi6000}
#'
#'   Brier, G. W. (1950). Verification of forecasts expressed in terms of
#'   probability. \emph{Monthly Weather Review} 78(1), 1--3.
#'   \doi{10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2}
#'
#'   Egami, N., Hinck, M., Stewart, B., and Wei, H. (2023). Using imperfect
#'   surrogates for downstream inference. Design-based supervised learning for
#'   social science applications of large language models. \emph{Advances in
#'   Neural Information Processing Systems} 36, 68589--68601.
#'   \doi{10.52202/075280-3000}
#'
#'   Hayes, A. F., and Krippendorff, K. (2007). Answering the call for a
#'   standard reliability measure for coding data. \emph{Communication Methods
#'   and Measures} 1(1), 77--89. \doi{10.1080/19312450709336664}
#' @seealso [coding_sample()] for drawing the sheet, and [coder_reference()] for
#'   consolidating several coders into one sheet.
#' @examples
#' # Results from a real run ship with the package, so this needs no server.
#' p <- system.file("extdata", "vignette_results.rds", package = "fathom")
#' baked <- readRDS(p)
#' res <- baked$res_six
#' sheet <- coding_sample(res, parlasent, text = "sentence", id = "id", n = 20,
#'   seed = 1234)
#'
#' # The parlasent coders, standing in for someone filling the sheet in.
#' six <- c("Negative", "M_Negative", "N_Neutral", "P_Neutral", "M_Positive",
#'   "Positive")
#' coded <- function(who) {
#'   out <- sheet
#'   out$human_label <- as.character(
#'     match(parlasent[[who]][match(sheet$id, parlasent$id)], six)
#'   )
#'   out
#' }
#' a1 <- coded("annotator1")
#'
#' v <- validate_sample(a1, res, level = "ordinal", labels = as.character(1:6))
#' v$discrimination
#' v$categories
#'
#' # A second coder adds the ceiling the two set between themselves, and the
#' # model against it.
#' a2 <- coded("annotator2")
#' v2 <- validate_sample(list(annotator1 = a1, annotator2 = a2), res,
#'   level = "ordinal", labels = as.character(1:6), bootstrap_n = 200)
#' v2$ceiling$pairs
#' v2$gap
#' @export
validate_sample <- function(
  sheet,
  results,
  ...,
  level = c("nominal", "ordinal"),
  labels = NULL,
  instrument = NULL,
  calibration = FALSE,
  coded = FALSE,
  ci = c("none", "bootstrap"),
  conf_level = 0.95,
  bootstrap_n = 2000L,
  seed = 1234L
) {
  no_dots(..., .fn = "validate_sample")
  results <- flat_results(results, "error")
  metric <- match.arg(level)
  ci <- match.arg(ci)
  check_conf_level(conf_level)
  check_flag(calibration, "calibration")
  check_flag(coded, "coded")
  local_seed(seed)

  # Several sheets, so the ceiling and the gap are computed here rather than in
  # more calls a reader has to keep in agreement with this one. A data frame is
  # a list, so the test is what it is not, and a path is neither.
  if (!is.data.frame(sheet) && is.list(sheet)) {
    return(validate_many(
      sheet,
      results,
      metric = metric,
      labels = labels,
      instrument = instrument,
      calibration = calibration,
      coded = coded,
      ci = ci,
      bootstrap_n = bootstrap_n,
      seed = seed,
      conf_level = conf_level,
      call = match.call()
    ))
  }
  bootstrap_n <- check_bootstrap_n(bootstrap_n)
  if (is.character(sheet) && length(sheet) == 1L) {
    sheet <- read_sheet(sheet)
  }
  stopifnot(is.data.frame(sheet))

  # Which column arrived decides the path, not a type check. coder_reference()
  # writes `human_label` under `majority` and `unanimous` and `human_prop`
  # under `soft`, and a proportion has no second categorical variable, so
  # nothing on the discrimination side is defined for it.
  soft <- !("human_label" %in% names(sheet)) && "human_prop" %in% names(sheet)
  need <- c("id", "item", if (soft) "human_prop" else "human_label")
  missing_cols <- setdiff(need, names(sheet))
  if (length(missing_cols)) {
    stop(
      fmt_err(paste0(
        "the sheet is missing ",
        paste(missing_cols, collapse = ", "),
        ". Pass the sheet coding_sample() wrote with human_label filled in, ",
        "or one coder_reference() returned."
      )),
      call. = FALSE
    )
  }

  sheet <- if (soft) {
    sheet[!is.na(sheet$human_prop), , drop = FALSE]
  } else {
    sheet[
      !is.na(sheet$human_label) & nzchar(trimws(sheet$human_label)),
      ,
      drop = FALSE
    ]
  }
  if (nrow(sheet) == 0L) {
    stop(
      fmt_err(sprintf(
        "no rows have %s filled in",
        if (soft) "human_prop" else "human_label"
      )),
      call. = FALSE
    )
  }
  if (soft) {
    if (any(sheet$human_prop < 0 | sheet$human_prop > 1)) {
      stop(
        fmt_err("`human_prop` must be a proportion between 0 and 1."),
        call. = FALSE
      )
    }
    if (!"human_target" %in% names(sheet)) {
      stop(
        fmt_err(paste0(
          "the sheet has `human_prop` but no `human_target`, so which label ",
          "the proportion is of cannot be recovered. Pass a sheet ",
          "coder_reference(method = \"soft\") returned."
        )),
        call. = FALSE
      )
    }
  }

  labs <- unique(results$label)
  if (length(labs) < 2L) {
    stop(
      fmt_err(paste0(
        "the results carry one label, ",
        labs[1],
        ". Agreement, alpha and the per-label figures each need a ",
        "distribution across labels to read, and one label leaves none. Pass ",
        "the results as `measure()` returned them."
      )),
      call. = FALSE
    )
  }
  labels <- labels %||% labels_from_instrument(instrument)

  # Which proper rule the metric implies. Brier charges a one-level miss and a
  # five-level miss alike, which is the defect the ordinal metric exists to
  # remove, so an ordinal run scores on the cumulative probabilities instead.
  # The column is named for the quantity rather than always `brier`, so a
  # reader cannot take one for the other. A nominal run is unchanged in every
  # respect.
  #
  # The soft path stays on Brier. It is two labels by construction, where the
  # cumulative rule has one term against Brier's two and carries no ordering
  # information that two labels do not already have.
  rps <- identical(metric, "ordinal") && !soft
  score_col <- if (rps) "rps" else "brier"
  ord <- labs
  if (rps) {
    ord <- as.character(labels %||% character(0))
    short <- setdiff(labs, ord)
    if (length(short)) {
      stop(
        fmt_err(
          paste0(
            "the ordinal metric scores on the cumulative probabilities, so ",
            "every label needs a position on the scale. `labels` has no place ",
            "for ",
            paste(sprintf("`%s`", short), collapse = ", "),
            "."
          ),
          paste0(
            "Pass `labels` in scale order, least to greatest. An instrument ",
            "carrying `scores` supplies it through `instrument =`."
          )
        ),
        call. = FALSE
      )
    }
    ord <- ord[ord %in% labs]
  }

  # A soft sheet names the label its proportion is of. A label sheet needs no
  # positive class, since every label gets its own row in `$categories`.
  target <- if (soft) as.character(sheet$human_target[1]) else NULL
  if (soft && !target %in% labs) {
    stop(
      fmt_err(sprintf(
        paste0(
          "the sheet reports the proportion choosing %s, which is not a label ",
          "in the results. Labels are %s."
        ),
        target,
        paste(labs, collapse = ", ")
      )),
      call. = FALSE
    )
  }
  if (soft) {
    message(fmt_note(
      "Note",
      paste0(
        "A soft reference has no second categorical variable, so agreement, ",
        "alpha, precision, recall and AUC are undefined rather than NA and are ",
        "not reported. The Brier score is."
      )
    ))
  }

  # Coders write what they write. Anything outside the label set is a data
  # problem rather than a disagreement, so it is refused rather than coerced.
  hl <- if (soft) character(0) else trimws(as.character(sheet$human_label))
  bad <- setdiff(unique(hl), labs)
  if (length(bad)) {
    stop(
      fmt_err(paste0(
        "human_label contains values that are not labels. Found ",
        paste(bad, collapse = ", "),
        ". Labels are ",
        paste(labs, collapse = ", "),
        "."
      )),
      call. = FALSE
    )
  }
  if (!soft) {
    sheet$human_label <- hl
  }

  # The probabilities live on the results, never on the sheet.
  scored <- results[is.na(results$error), , drop = FALSE]
  keys <- c("id", "item")
  sheet$id <- as.character(sheet$id)
  scored$id <- as.character(scored$id)

  # pivot_wider() answers a duplicated key with a list column rather than an
  # error, and the failure then surfaces much later as a coercion message that
  # says nothing about the cause. Binding two runs together is the way to get
  # here.
  if (anyDuplicated(scored[, c("id", "item", "label")])) {
    stop(
      fmt_err(paste0(
        "the results carry more than one row for the same id, item and label. ",
        "One probability per label per document-item is needed. Check that ",
        "these are a single run rather than several bound together."
      )),
      call. = FALSE
    )
  }

  # prob_renorm is p over total across the whole label set, computed once in
  # score.R and never recomputed here, so it sums to one for each document and
  # item unless no label appeared at all. A group that does not means labels
  # were removed from the frame after the run. slice_max() below takes the
  # argmax of whatever survived, so model_label and every figure resting on it
  # would be wrong while still printing. The separator is the one score.R uses
  # for the same grouping.
  sums <- vapply(
    split(scored$prob_renorm, paste(scored$id, scored$item, sep = "\r")),
    sum,
    numeric(1)
  )
  short <- !is.na(sums) & abs(sums - 1) > 1e-6
  if (any(short)) {
    stop(
      fmt_err(paste0(
        "the results are missing labels. `prob_renorm` is computed across ",
        "the whole label set, so it sums to 1 for each document and item. ",
        sum(short),
        " of ",
        length(sums),
        " groups do not. Subsetting results by label leaves `model_label` as ",
        "the argmax of whatever survived, so the figures would be wrong ",
        "rather than narrow. Pass the results as `measure()` returned them. ",
        "Subsetting by item keeps every label and is unaffected."
      )),
      call. = FALSE
    )
  }

  wide <- scored %>%
    dplyr::select(dplyr::all_of(c(keys, "label", "prob_renorm"))) %>%
    tidyr::pivot_wider(names_from = "label", values_from = "prob_renorm")

  # A call whose distribution held none of the labels gave no answer to score.
  # winning_rows() keeps no row for it, so the join below sets that coded row
  # aside, the way coding_sample() never draws one and item_report() leaves
  # such a document out of its renormalized figures.
  winner <- winning_rows(scored) %>%
    dplyr::select(dplyr::all_of(c(keys, "label"))) %>%
    dplyr::rename(model_label = "label")
  unanswered <- sheet %>%
    dplyr::semi_join(scored, by = keys) %>%
    dplyr::anti_join(winner, by = keys)
  if (nrow(unanswered)) {
    message(fmt_note(
      "Note",
      paste0(
        nrow(unanswered),
        " coded row(s) set aside. The model's distribution held none of the ",
        "labels there, so it gave no answer to score against the coder."
      )
    ))
  }

  d <- sheet %>%
    dplyr::inner_join(winner, by = keys) %>%
    dplyr::inner_join(wide, by = keys)

  if (nrow(d) == 0L) {
    stop(
      fmt_err(paste0(
        "no rows of the sheet matched the results on id and item. Check that ",
        "these are the results the sample was drawn from."
      )),
      call. = FALSE
    )
  }
  if (nrow(d) < nrow(sheet)) {
    message(fmt_note(
      "Warning",
      sprintf(
        "%d of %d coded rows had no match in the results and were dropped.",
        nrow(sheet) - nrow(d),
        nrow(sheet)
      )
    ))
  }

  if (soft) {
    # Two categories, target against the rest, matched to a target that is
    # itself two-valued. Identical to the hard Brier on a two-label
    # instrument, which is the only place a soft sheet is accepted.
    p <- as.numeric(d[[target]])
    d$brier <- (p - d$human_prop)^2 + ((1 - p) - (1 - d$human_prop))^2
  } else {
    d$correct <- d$model_label == d$human_label

    # Brier is the squared error summed over categories, which reduces to the
    # familiar form on two labels. The ranked probability score is the same
    # thing on the cumulative probabilities, so a miss is charged by how far
    # along the scale it lands. The last cumulative term carries nothing, since
    # both vectors reach one there.
    d[[score_col]] <- vapply(
      seq_len(nrow(d)),
      function(i) {
        p <- vapply(ord, function(L) as.numeric(d[[L]][i]), numeric(1))
        y <- as.numeric(ord == d$human_label[i])
        if (rps) {
          sum((cumsum(p)[-length(p)] - cumsum(y)[-length(y)])^2)
        } else {
          sum((p - y)^2)
        }
      },
      numeric(1)
    )
  }

  # One row per question, plus a pooled row when there is more than one.
  its <- unique(d$item)
  groups <- c(
    lapply(its, function(it) {
      list(g = d[d$item == it, , drop = FALSE], nm = it, per_label = TRUE)
    }),
    if (length(its) > 1L) {
      list(list(g = d, nm = ALL_ITEMS, per_label = FALSE))
    }
  )
  # Scale order on an ordered instrument, so the label rows read down the scale.
  cat_labs <- if (rps) ord else labs

  # Every figure for a question comes off one resample, so turning
  # `calibration` on or off changes no other interval. The labels are counted
  # per question only, since a pooled label mixes documents from one question
  # with documents from another.
  stats_for <- function(g, per_label) {
    function(ix) {
      h <- g[ix, , drop = FALSE]
      out <- c()
      if (!soft) {
        out["agreement"] <- mean(h$correct)
        out["alpha"] <- kripp_parts(
          cbind(h$model_label, h$human_label),
          metric = metric,
          labels = labels
        )$alpha
        if (per_label) {
          for (L in cat_labs) {
            pr <- prf(h, L)
            out[paste(L, "precision", sep = "\r")] <- pr$precision
            out[paste(L, "recall", sep = "\r")] <- pr$recall
            out[paste(L, "auc", sep = "\r")] <- auc_of(
              h[[L]],
              h$human_label == L
            )
          }
        }
      }
      out[score_col] <- mean(h[[score_col]], na.rm = TRUE)
      out
    }
  }
  boot <- function(grp) {
    if (ci == "none" || nrow(grp$g) < 2L) {
      return(NULL)
    }
    draws <- resample_units(
      nrow(grp$g),
      stats_for(grp$g, grp$per_label),
      b = bootstrap_n
    )
    ci_percentile(draws, conf_level)
  }
  pick <- function(q, keys) {
    if (is.null(q)) {
      return(NULL)
    }
    stats::setNames(lapply(keys, function(k) as.numeric(q[k, ])), names(keys))
  }

  disc <- list()
  cats <- list()
  calib <- list()
  for (grp in groups) {
    g <- grp$g
    q <- boot(grp)
    if (!soft) {
      disc[[length(disc) + 1L]] <- add_ci(
        tibble::tibble(
          item = grp$nm,
          n = nrow(g),
          agreement = mean(g$correct),
          alpha = kripp_parts(
            cbind(g$model_label, g$human_label),
            metric = metric,
            labels = labels
          )$alpha
        ),
        pick(q, c(agreement = "agreement", alpha = "alpha"))
      )
      if (grp$per_label) {
        for (L in cat_labs) {
          pr <- prf(g, L)
          cats[[length(cats) + 1L]] <- add_ci(
            tibble::tibble(
              item = grp$nm,
              label = L,
              n = sum(g$human_label == L),
              precision = pr$precision,
              recall = pr$recall,
              auc = auc_of(g[[L]], g$human_label == L)
            ),
            pick(
              q,
              stats::setNames(
                paste(L, c("precision", "recall", "auc"), sep = "\r"),
                c("precision", "recall", "auc")
              )
            )
          )
        }
      }
    }
    row <- tibble::tibble(item = grp$nm, n = nrow(g))
    row[[score_col]] <- mean(g[[score_col]], na.rm = TRUE)
    calib[[length(calib) + 1L]] <- add_ci(
      row,
      pick(q, stats::setNames(score_col, score_col))
    )
  }

  out <- list(
    discrimination = if (!soft) dplyr::bind_rows(disc),
    categories = if (!soft) dplyr::bind_rows(cats),
    calibration = if (soft || calibration) dplyr::bind_rows(calib),
    coded = if (coded) tibble::as_tibble(d)
  )
  out <- structure(Filter(Negate(is.null), out), class = "fathom_validation")
  attr(out, "level") <- metric
  attr(out, "conf_level") <- conf_level
  attr(out, "n_coded") <- nrow(d)
  stamp_provenance(out, call = match.call())
}

#' Score a model against several coders in one call
#'
#' @description
#' The several-sheet path of [validate_sample()]. Each coder is scored through
#' the one-sheet path, so nothing here reimplements a statistic. What it buys
#' is that the calls a reader would otherwise make cannot disagree about the
#' metric, the label order or the rows, because there is one call and the
#' mismatch is unrepresentable.
#'
#' The model's answer is the same whichever sheet it arrived beside, so the gap
#' reads one row per document-item from the union across coders. Taking the
#' first coder's rows would silently restrict the gap to that coder's coverage.
#'
#' @param sheets A list of completed sheets, optionally named.
#' @param results,metric,labels,instrument,calibration,coded,ci,bootstrap_n
#'   As passed to [validate_sample()].
#' @param seed As passed to [validate_sample()], and handed to every call made
#'   here, so each part is reproducible on its own.
#' @param conf_level As passed to [validate_sample()], and handed to every call
#'   made here.
#' @param call A call object holding the outer call, stamped on the result.
#' @return A `fathom_validation` carrying `$ceiling` and `$gap`.
#' @keywords internal
#' @noRd
validate_many <- function(
  sheets,
  results,
  metric,
  labels,
  instrument,
  calibration,
  coded,
  ci,
  bootstrap_n,
  seed,
  conf_level,
  call
) {
  if (length(sheets) < 2L) {
    stop(
      fmt_err(
        "a list of sheets is the several-coder path and it needs two or more.",
        paste0(
          "With one coder there is no ceiling and the alpha floats, which ",
          "is a reading rather than a fault. Pass the sheet itself, as ",
          "`validate_sample(sheet, results)`."
        )
      ),
      call. = FALSE
    )
  }
  # The naming rule merge_sheets() uses, applied here as well, so the `coder`
  # column on the model tables and the `coder_a` column on the ceiling are the
  # same words.
  nms <- names(sheets) %||% rep("", length(sheets))
  nms[!nzchar(nms)] <- paste0("coder", seq_along(sheets))[!nzchar(nms)]
  if (anyDuplicated(nms)) {
    stop(fmt_err("coder names must be unique."), call. = FALSE)
  }
  names(sheets) <- nms
  # Resolved once, so the ceiling and the gap read the scale every per-coder
  # call reads.
  labels <- labels %||% labels_from_instrument(instrument)

  # The same message once per coder is the same message k times, so only the
  # first call reports. Warnings are per sheet and all of them show. `$coded`
  # is kept on every call, since the gap reads the model's answers off it.
  run <- function(i) {
    validate_sample(
      sheets[[i]],
      results,
      level = metric,
      labels = labels,
      calibration = calibration,
      coded = TRUE,
      ci = ci,
      conf_level = conf_level,
      bootstrap_n = bootstrap_n,
      seed = seed
    )
  }
  each <- c(
    list(run(1L)),
    lapply(seq_along(sheets)[-1L], function(i) suppressMessages(run(i)))
  )
  names(each) <- nms

  # `coder` sits after `item`, so a row reads as one question against one
  # person.
  with_coder <- function(tb, k) {
    tb$coder <- k
    ord <- setdiff(names(tb), "coder")
    at <- if ("item" %in% ord) match("item", ord) else 0L
    tb[, append(ord, "coder", after = at), drop = FALSE]
  }
  stack <- function(nm) {
    parts <- lapply(nms, function(k) {
      tb <- each[[k]][[nm]]
      if (is.null(tb) || nrow(tb) == 0L) NULL else with_coder(tb, k)
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts)) dplyr::bind_rows(parts) else NULL
  }

  # The ceiling on the same sheets, the same metric and the same label order.
  ceil <- coder_agreement(
    sheets,
    metric = metric,
    labels = labels,
    ci = ci,
    conf_level = conf_level,
    bootstrap_n = bootstrap_n,
    seed = seed
  )

  # One row per document-item with the model's answer. `n_coders` rides along
  # where a consolidated sheet carries it, since that is what the gap warns
  # about.
  model <- dplyr::distinct(
    dplyr::bind_rows(lapply(each, function(v) {
      keep <- intersect(
        c("id", "item", "model_label", "n_coders"),
        names(v$coded)
      )
      v$coded[, keep, drop = FALSE]
    })),
    .data$id,
    .data$item,
    .keep_all = TRUE
  )
  gap <- gap_table(
    model,
    ceil$coded,
    nms,
    metric = metric,
    labels = labels,
    bootstrap_n = bootstrap_n,
    conf_level = conf_level,
    seed = seed
  )

  # One row per document-item per coder, which keeps `human_label` and
  # `correct` rather than widening and losing them.
  stacked <- if (coded) {
    tibble::as_tibble(dplyr::bind_rows(lapply(nms, function(k) {
      tb <- each[[k]]$coded
      tb$coder <- k
      tb
    })))
  }

  out <- list(
    discrimination = stack("discrimination"),
    categories = stack("categories"),
    calibration = stack("calibration"),
    ceiling = list(alpha = ceil$alpha, pairs = ceil$pairs),
    gap = gap,
    coded = stacked
  )
  out <- structure(Filter(Negate(is.null), out), class = "fathom_validation")
  attr(out, "level") <- metric
  attr(out, "coders") <- nms
  attr(out, "conf_level") <- conf_level
  attr(out, "n_coded") <- sum(vapply(
    each,
    function(v) as.integer(attr(v, "n_coded")),
    integer(1)
  ))
  stamp_provenance(out, call = call)
}

#' Refuse anything but a single TRUE or FALSE
#'
#' @param x The value passed.
#' @param name A string naming the argument, for the message.
#' @return `x`, invisibly.
#' @keywords internal
#' @noRd
check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(fmt_err(sprintf("`%s` must be TRUE or FALSE.", name)), call. = FALSE)
  }
  invisible(x)
}

#' Put an interval column beside each metric it belongs to
#'
#' One bracketed character column per metric, and absent entirely under
#' `ci = "none"` rather than present and full of `NA`, since a column of `NA`
#' reads as a failed computation instead of as a question that was not asked.
#'
#' Placed immediately after its metric, because a reader compares a figure with
#' its own interval and not with the one four columns away.
#'
#' @param out A data frame holding the wide table.
#' @param ci A named list of `c(lower, upper)`, or `NULL`.
#' @return `out`, with the interval columns interleaved.
#' @keywords internal
#' @noRd
add_ci <- function(out, ci) {
  if (is.null(ci)) {
    return(out)
  }
  base <- names(out)
  for (m in intersect(names(ci), base)) {
    out[[paste0(m, "_ci")]] <- fmt_ci(ci[[m]][1], ci[[m]][2])
  }
  ord <- unlist(lapply(base, function(nm) {
    c(nm, if (paste0(nm, "_ci") %in% names(out)) paste0(nm, "_ci"))
  }))
  out[, ord, drop = FALSE]
}

#' Precision and recall for one positive class
#'
#' One against the rest at any label count, so `target` names the class both
#' figures are about and everything else is the rest.
#'
#' `NA` rather than zero when a denominator is empty, and the two denominators
#' fail for different reasons. Precision has none when the model never chose
#' `target`, recall has none when no coder did. Zero would assert the model got
#' none of them right, where the truth is that the question was never put.
#'
#' @param g A data frame carrying `model_label` and `human_label`.
#' @param target A string naming the positive class.
#' @return A list of `precision` and `recall`, each `NA` when its denominator is
#'   empty.
#' @keywords internal
#' @noRd
prf <- function(g, target) {
  tp <- sum(g$model_label == target & g$human_label == target)
  fp <- sum(g$model_label == target & g$human_label != target)
  fn <- sum(g$model_label != target & g$human_label == target)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  list(precision = precision, recall = recall)
}

#' Area under the ROC curve
#'
#' The Mann-Whitney statistic, which is the chance that a randomly chosen
#' positive scores above a randomly chosen negative. Written out rather than
#' imported, since it is five lines and the alternative is a dependency for
#' five lines. `rank()` gives midranks, which is the standard treatment of tied
#' scores.
#'
#' @param score A numeric vector of scores.
#' @param positive A logical vector marking the positive cases, aligned with
#'   `score`.
#' @return A number giving the area under the ROC curve, or `NA` when either
#'   class is empty.
#' @keywords internal
#' @noRd
auc_of <- function(score, positive) {
  ok <- !is.na(score) & !is.na(positive)
  score <- score[ok]
  positive <- positive[ok]
  n1 <- sum(positive)
  n0 <- sum(!positive)
  if (n1 == 0L || n0 == 0L) {
    return(NA_real_)
  }
  r <- rank(score)
  (sum(r[positive]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' Read a coding sheet from disk with every column as text
#'
#' Guessing column types reads an id column of digits as a number. A double
#' holds about fifteen significant digits, so two nineteen-digit ids can come
#' back as one, and the base reader also drops leading zeros. Either way the
#' ids stop matching the ones on the results. Every column is read as text
#' instead, and `human_prop` is turned back into a number, since it is the one
#' column a sheet carries that is neither a key nor a label.
#'
#' @param path A string giving the path to the CSV.
#' @return A data frame.
#' @keywords internal
#' @noRd
read_sheet <- function(path) {
  s <- if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character())
    )
  } else {
    utils::read.csv(path, colClasses = "character")
  }
  if ("human_prop" %in% names(s)) {
    s$human_prop <- as.numeric(s$human_prop)
  }
  s
}

#' k coding sheets matched into one frame
#'
#' Shared by `coder_agreement()`, which builds the ceiling `validate_sample()`
#' reports, and `coder_reference()`. Both need the sheets named, validated, and
#' matched on `id` and `item` before anything else can happen. What each does with the result
#' differs, so only the mechanism lives here and the policy stays with the
#' caller.
#'
#' A full join rather than an inner one. Alpha's advantage over Fleiss is that
#' it handles units seen by different subsets of coders, and an inner join
#' throws exactly that away before alpha sees it. At three coders each missing a
#' row in ten it discards about a quarter of the sample, and more with each
#' coder added. So every row reaches the merge and `NA` marks who did not see
#' it. Whether a row with one coder is usable is the caller's question.
#'
#' @param sheets A list of data frames or paths, optionally named.
#' @return A list with `merged`, carrying `id`, `item` and one column per coder,
#'   `nms`, the coder names in order, and `seen`, how many coders labeled each
#'   row.
#' @keywords internal
#' @noRd
merge_sheets <- function(sheets) {
  if (length(sheets) < 2L) {
    stop(
      fmt_err("pass at least two completed sheets, one per coder."),
      call. = FALSE
    )
  }

  nms <- names(sheets) %||% rep("", length(sheets))
  nms[!nzchar(nms)] <- paste0("coder", seq_along(sheets))[!nzchar(nms)]
  if (anyDuplicated(nms)) {
    stop(fmt_err("coder names must be unique."), call. = FALSE)
  }
  # Without this a coder named id or item collides with the columns the sheets
  # are matched on, and the failure surfaces from inside the join as a message
  # about duplicate columns that says nothing about coders.
  clash <- intersect(nms, c("id", "item"))
  if (length(clash)) {
    stop(
      fmt_err(sprintf(
        paste0(
          "a coder cannot be named %s. Those are the columns the sheets ",
          "are matched on."
        ),
        paste(sprintf("`%s`", clash), collapse = " or ")
      )),
      call. = FALSE
    )
  }

  need <- c("id", "item", "human_label")
  prepped <- Map(
    function(s, nm) {
      if (is.character(s) && length(s) == 1L) {
        s <- read_sheet(s)
      }
      if (!is.data.frame(s)) {
        stop(
          fmt_err(sprintf("%s is not a data frame or a path to one.", nm)),
          call. = FALSE
        )
      }
      miss <- setdiff(need, names(s))
      if (length(miss)) {
        stop(
          fmt_err(sprintf(
            paste0(
              "%s is missing %s. Pass the sheet coding_sample() wrote, with ",
              "human_label filled in."
            ),
            nm,
            paste(miss, collapse = ", ")
          )),
          call. = FALSE
        )
      }
      keep <- !is.na(s$human_label) & nzchar(trimws(s$human_label))
      s <- s[keep, , drop = FALSE]
      if (nrow(s) == 0L) {
        stop(
          fmt_err(sprintf("%s has no rows with human_label filled in.", nm)),
          call. = FALSE
        )
      }
      out <- tibble::tibble(
        id = as.character(s$id),
        item = as.character(s$item),
        lab = trimws(as.character(s$human_label))
      )
      names(out)[3] <- nm
      out
    },
    sheets,
    nms
  )
  # A full join rather than an inner one. Alpha's advantage over Fleiss is that
  # it handles units seen by different subsets of coders, and an inner join
  # throws exactly that away before alpha sees it. At three coders each missing
  # a row in ten it discards about a quarter of the sample, and more with each
  # coder added. So every row reaches the merge, `NA` marks who did not see it,
  # alpha uses whatever had two or more, and each pairwise row falls back to
  # that pair's own complete cases.
  merged <- Reduce(
    function(a, b) dplyr::full_join(a, b, by = c("id", "item")),
    prepped
  )
  seen <- rowSums(!is.na(as.matrix(merged[, nms, drop = FALSE])))

  list(merged = merged, nms = nms, seen = seen)
}

#' Collapse several coders into one reference sheet
#'
#' Takes the completed sheets and returns one sheet, so
#' `validate_sample(coder_reference(ana, luis, bea), res)` composes without
#' changing a signature. Read `$ceiling` from [validate_sample()] given the
#' same sheets first. It says whether collapsing is honest, since a high alpha
#' means the coders were interchangeable and a low one means the reference is a
#' consensus that never existed.
#'
#' @section Collapse methods:
#' `"majority"` writes the label most coders chose. Where two or more labels tie
#' for the most it writes `NA` and names those rows, rather than breaking the
#' tie. A rule that broke it would invent a coding no coder produced.
#'
#' `"unanimous"` writes the label only where every coder who saw the row chose
#' it, and `NA` otherwise. It warns, since the rows it discards are the hard
#' ones, so what survives is easier than the draw was.
#'
#' A row only one coder saw has a majority of one and is trivially unanimous.
#' `n_coders` on the returned sheet is what separates those from a real
#' agreement.
#'
#' @section The soft method:
#' `"soft"` writes no label. It writes `human_prop`, the share of coders who
#' chose `target`, smoothed as `(n_target + 1) / (n_coders + 2)` so two coders
#' who both said yes give 0.75 rather than 1. That is the honest reading of two
#' observations, and it keeps the target off the boundaries, which matters
#' because `prob_renorm` reaches exactly zero and a log score would then be
#' infinite. [validate_sample()] scores it with Brier for the same reason.
#'
#' It needs exactly two labels. Above two the honest soft target is a vector of
#' proportions rather than one number, and reporting one label against the rest
#' would leave a Brier over two categories where a label sheet is scored over
#' all of them. Refused rather than silently rescaled.
#'
#' @param ... Two or more completed coding sheets, as data frames or paths.
#'   Name them to name the coders.
#' @param method A string naming how to collapse a row. `"majority"` takes the
#'   most common label, `"unanimous"` requires every coder to agree, `"soft"`
#'   keeps the disagreement as a proportion instead of resolving it. Default is
#'   `"majority"`.
#' @param target A string naming the label `"soft"` reports the proportion of.
#'   Defaults to the first of the two in sorted order, and the choice is written
#'   to the sheet so it is never implicit. Ignored by the other methods.
#' @return A tibble, one row per document-item pair that any coder saw. It
#' carries `id`, `item`, `human_label`, and `n_coders` and `n_agree` beside
#' them.
#'
#' Under `"soft"` the label column is replaced by `human_prop`, with
#' `human_target` naming the label the proportion is of and `n_target` in place
#' of `n_agree`. [validate_sample()] branches on which of the two arrived.
#' @seealso [validate_sample()], whose `$ceiling` says whether collapsing is
#'   honest and which reads the result.
#' @examples
#' ana <- data.frame(
#'   id = c("1", "2", "3"), item = "q",
#'   human_label = c("YES", "NO", "YES")
#' )
#' luis <- data.frame(
#'   id = c("1", "2", "3"), item = "q",
#'   human_label = c("YES", "YES", "NO")
#' )
#' bea <- data.frame(
#'   id = c("1", "2", "3"), item = "q",
#'   human_label = c("YES", "NO", "NO")
#' )
#' coder_reference(ana = ana, luis = luis, bea = bea)
#' @export
coder_reference <- function(
  ...,
  method = c("majority", "unanimous", "soft"),
  target = NULL
) {
  # A coder named for this formal never reaches `...`, and under the default
  # would vanish without a word.
  if (!is.character(method)) {
    stop(
      fmt_err(paste0(
        "`method` must be \"majority\", \"unanimous\" or \"soft\". A coder ",
        "cannot be named `method`, since it is an argument of this function."
      )),
      call. = FALSE
    )
  }
  method <- match.arg(method)

  sheets <- merge_sheets(list(...))
  merged <- sheets$merged
  nms <- sheets$nms

  votes <- as.matrix(merged[, nms, drop = FALSE])
  tally <- lapply(seq_len(nrow(votes)), function(i) {
    v <- votes[i, ]
    v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 0L) NULL else sort(table(v), decreasing = TRUE)
  })

  n_coders <- vapply(
    tally,
    function(t) if (is.null(t)) 0L else sum(t),
    integer(1)
  )
  n_agree <- vapply(
    tally,
    function(t) if (is.null(t)) 0L else as.integer(t[[1]]),
    integer(1)
  )
  top <- vapply(
    tally,
    function(t) if (is.null(t)) NA_character_ else names(t)[1],
    character(1)
  )
  tied <- vapply(
    tally,
    function(t) !is.null(t) && sum(t == t[[1]]) > 1L,
    logical(1)
  )

  if (method == "soft") {
    seen <- as.vector(votes)
    labs <- sort(unique(seen[!is.na(seen) & nzchar(seen)]))
    # One proportion is only a complete description of the coders on two
    # labels. Above two the honest soft target is a vector of k proportions,
    # and collapsing it to one label against the rest would leave a Brier over
    # two categories while validate_sample() scores a label sheet over k. The
    # two figures would not be on one scale and nothing would say so.
    if (length(labs) != 2L) {
      stop(
        fmt_err(sprintf(
          paste0(
            "`soft` needs exactly two labels and the coders used %d, %s. The ",
            "form that keeps k proportions is not built. Use `majority` here, ",
            "or run the soft path on a two-label item."
          ),
          length(labs),
          paste(labs, collapse = ", ")
        )),
        call. = FALSE
      )
    }
    target <- target %||% labs[1]
    if (!target %in% labs) {
      stop(
        fmt_err(sprintf(
          "`target` must be one of %s.",
          paste(labs, collapse = " or ")
        )),
        call. = FALSE
      )
    }
    n_target <- vapply(
      tally,
      function(t) {
        if (is.null(t) || !target %in% names(t)) 0L else as.integer(t[[target]])
      },
      integer(1)
    )
    # Laplace smoothing. Two coders who both said yes give 3/4 rather than 1,
    # which is the honest reading of two observations, and it keeps the target
    # off the boundaries where a log score would be infinite.
    out <- tibble::tibble(
      id = merged$id,
      item = merged$item,
      human_prop = (n_target + 1) / (n_coders + 2),
      human_target = target,
      n_coders = n_coders,
      n_target = n_target
    )
    return(finish_reference(out, match.call()))
  }

  if (method == "majority") {
    lab <- ifelse(tied, NA_character_, top)
  } else {
    lab <- ifelse(n_coders > 0L & n_agree == n_coders, top, NA_character_)
    warning(
      fmt_warn(paste0(
        "`unanimous` discards every row the coders split on. Those are the ",
        "hardest rows, so the reference that survives is easier than the draw ",
        "was, and the model's figures against it run high."
      )),
      call. = FALSE
    )
  }

  dropped <- which(is.na(lab) & n_coders > 0L)
  if (length(dropped)) {
    shown <- utils::head(unique(merged$id[dropped]), 10L)
    message(fmt_note(
      "Note",
      sprintf(
        paste0(
          "%d of %d rows were left blank because the coders did not settle ",
          "them. %s%s. Breaking a tie would invent a coding no coder wrote."
        ),
        length(dropped),
        nrow(merged),
        paste(shown, collapse = ", "),
        if (length(unique(merged$id[dropped])) > 10L) ", and more" else ""
      )
    ))
  }

  out <- tibble::tibble(
    id = merged$id,
    item = merged$item,
    human_label = lab,
    n_coders = n_coders,
    n_agree = n_agree
  )
  finish_reference(out, match.call())
}

#' Attach the provenance stamp to a reference sheet
#'
#' Shared by the three methods, which differ in what they put in the label
#' column and in nothing after that.
#'
#' @param out A data frame holding the collapsed sheet.
#' @param call A call object to stamp.
#' @return `out`, stamped with `call`.
#' @keywords internal
#' @noRd
finish_reference <- function(out, call) {
  # `call` reaches stamp_provenance() as an argument that is only assigned, so
  # the promise is never forced and the language object is stored rather than
  # run.
  stamp_provenance(out, call = call)
}

#' The model against the ceiling the coders set
#'
#' @description
#' One row per question. `model_alpha` is the model's average alpha with the
#' coders and `coder_alpha` is the coders' average alpha with each other, both
#' on the rows the model answered. If the model behaved like one more coder,
#' the gap would sit near zero, which is what makes the two comparable.
#'
#' The two coefficients are correlated through the documents they share, so the
#' pairing has to be right by construction. Every replicate draws documents once
#' and recomputes both figures on that same draw, which is what makes the
#' difference between them a quantity with a distribution. Holding the coders'
#' figure fixed and resampling only the model side would understate how
#' uncertain the comparison is, since that figure is an estimate with its own
#' sampling error.
#'
#' @param model A data frame of one row per document-item, carrying `id`,
#'   `item` and `model_label`, and `n_coders` where a consolidated sheet put it
#'   there.
#' @param coders A data frame holding the coder frame, `id`, `item` and one
#'   column per coder, `NA` where that coder did not see the row.
#' @param nms A character vector of coder names, in order.
#' @param metric,labels As passed to [validate_sample()].
#' @param bootstrap_n An integer giving the number of replicates.
#' @param conf_level A number giving the coverage of `gap_ci`.
#' @param seed An integer giving the seed for the resample, or `NULL`.
#' @return A tibble of `item`, `n`, `model_alpha`, `coder_alpha`, `gap` and
#'   `gap_ci`.
#' @keywords internal
#' @noRd
gap_table <- function(
  model,
  coders,
  nms,
  metric,
  labels,
  bootstrap_n,
  conf_level,
  seed
) {
  local_seed(seed)

  # coder_reference() writes `n_coders` onto the sheet it consolidates, so a
  # consensus among the coders is detectable rather than something to ask
  # about. Agreeing with one is easier than agreeing with a person.
  if ("n_coders" %in% names(model)) {
    warning(
      fmt_warn(paste0(
        "one of the sheets is a consolidated reference rather than one ",
        "person's coding. Agreeing with a cleaned consensus is easier than ",
        "two people agreeing with each other, so the gap runs in the model's ",
        "favor by however much the consolidation smoothed. Pass each coder's ",
        "own sheet instead."
      )),
      call. = FALSE
    )
  }

  j <- dplyr::inner_join(
    model[, c("id", "item", "model_label")],
    coders,
    by = c("id", "item")
  )
  if (nrow(j) == 0L) {
    stop(
      fmt_err(paste0(
        "no rows are common to the model's answers and the coders' sheets on ",
        "id and item. They have to describe the same documents."
      )),
      call. = FALSE
    )
  }

  pairs <- utils::combn(nms, 2L, simplify = FALSE)
  alpha_of <- function(a, b) {
    ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 2L) {
      return(NA_real_)
    }
    kripp_parts(cbind(a[ok], b[ok]), metric = metric, labels = labels)$alpha
  }
  avg <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  one <- function(ix) {
    h <- j[ix, , drop = FALSE]
    m <- avg(vapply(nms, function(a) alpha_of(h$model_label, h[[a]]), 1))
    c_ <- avg(vapply(pairs, function(p) alpha_of(h[[p[1]]], h[[p[2]]]), 1))
    c(model_alpha = m, coder_alpha = c_, gap = m - c_)
  }

  dplyr::bind_rows(lapply(unique(j$item), function(it) {
    idx <- which(j$item == it)
    point <- one(idx)
    draws <- resample_units(
      length(idx),
      function(s) one(idx[s]),
      b = bootstrap_n
    )
    q <- ci_percentile(draws, conf_level)
    tibble::tibble(
      item = it,
      n = length(idx),
      model_alpha = unname(point["model_alpha"]),
      coder_alpha = unname(point["coder_alpha"]),
      gap = unname(point["gap"]),
      gap_ci = fmt_ci(q["gap", "lower"], q["gap", "upper"])
    )
  }))
}

#' How much the coders agreed with each other
#'
#' @description
#' The ceiling [validate_sample()] reports under `$ceiling` when it is given a
#' list of sheets. One alpha over all coders per question, and one per pair.
#'
#' The sheets are merged with every row kept rather than only the rows every
#' coder reached, so alpha gets the benefit it was built for. Alpha then uses
#' whatever had two coders or more. Each pairwise row falls back to the rows
#' that pair both saw, which is narrower, so `$pairs` will often report a
#' smaller `n` than `$alpha` on the same question.
#'
#' The tables are per question rather than per instrument. Two coders who read
#' one of three questions completely differently come back as mediocre rather
#' than as broken once the questions are pooled, so the pooled row is there to
#' be reported and not to be read on its own.
#'
#' The unit resampled is a document-item pair, so a pair enters a replicate
#' whole or not at all and every figure for a question comes off the same
#' draws.
#'
#' @param sheets A named list of completed sheets, data frames or paths.
#' @param metric,labels,ci,conf_level,bootstrap_n As passed to
#'   [validate_sample()].
#' @param seed An integer giving the seed for the resample, or `NULL`.
#' @return A list of `alpha`, one row per question with `item`, `n_units` and
#'   `alpha`, `pairs`, one row per pair per question with `coder_a`, `coder_b`,
#'   `item`, `n`, `agreement` and `alpha`, and `coded`, the merged frame the
#'   gap reads.
#' @keywords internal
#' @noRd
coder_agreement <- function(
  sheets,
  metric = "nominal",
  labels = NULL,
  ci = "none",
  conf_level = 0.95,
  bootstrap_n = 2000L,
  seed = NULL
) {
  local_seed(seed)
  sheets <- merge_sheets(sheets)
  merged <- sheets$merged
  nms <- sheets$nms
  seen <- sheets$seen

  if (!any(seen >= 2L)) {
    stop(
      fmt_err(paste0(
        "no row was coded by two or more coders. Coders have to work the same ",
        "draw, at least in part, for agreement between them to mean anything."
      )),
      call. = FALSE
    )
  }

  # Coders who worked different draws are measured on whichever rows happened
  # to overlap, which is a different set from the one the draw produced. Said
  # out loud rather than absorbed.
  n_all <- sum(seen == length(nms))
  if (n_all < nrow(merged)) {
    message(fmt_note(
      "Note",
      sprintf(
        paste0(
          "%d rows in the merge. %d seen by every coder, %d by some of them, ",
          "%d by one alone and so unusable to alpha."
        ),
        nrow(merged),
        n_all,
        sum(seen >= 2L) - n_all,
        sum(seen < 2L)
      )
    ))
  }

  pairs <- utils::combn(nms, 2L, simplify = FALSE)
  its <- unique(merged$item)

  # Indices rather than a logical, because a resample repeats rows and a
  # logical cannot say a row was drawn twice.
  row_for <- function(p, ix, label) {
    a <- merged[[p[1]]][ix]
    b <- merged[[p[2]]][ix]
    # A pair compares two specific people, so it needs the rows both of them
    # saw. That is narrower than what alpha uses on the same question.
    both <- !is.na(a) & !is.na(b)
    a <- a[both]
    b <- b[both]
    # A matrix rather than a data frame. kripp_parts() coerces internally and
    # doing it here is two and a half times faster, which matters at two
    # thousand replicates and changes nothing about the answer.
    tibble::tibble(
      coder_a = p[1],
      coder_b = p[2],
      item = label,
      n = length(a),
      agreement = if (length(a) == 0L) NA_real_ else mean(a == b),
      alpha = kripp_parts(cbind(a, b), metric = metric, labels = labels)$alpha
    )
  }

  alpha_for <- function(ix, label) {
    m <- as.matrix(merged[ix, nms, drop = FALSE])
    usable <- if (nrow(m) == 0L) {
      integer(0)
    } else {
      rowSums(!is.na(m) & nzchar(m))
    }
    tibble::tibble(
      item = label,
      n_units = sum(usable >= 2L),
      alpha = kripp_parts(m, metric = metric, labels = labels)$alpha
    )
  }

  # One resample per scope, shared by `alpha` and `pairs`. Two separate draws
  # would give figures that cannot be compared with each other.
  boot_scope <- function(ix, label) {
    if (ci == "none" || length(ix) < 2L) {
      return(NULL)
    }
    resample_units(
      length(ix),
      function(k) {
        j <- ix[k]
        out <- c(alpha = alpha_for(j, label)$alpha)
        for (p in pairs) {
          r <- row_for(p, j, label)
          key <- paste(p, collapse = "\r")
          out[paste0(key, "\ragreement")] <- r$agreement
          out[paste0(key, "\ralpha")] <- r$alpha
        }
        out
      },
      b = bootstrap_n
    )
  }
  scope_ix <- c(
    lapply(its, function(it) which(merged$item == it)),
    if (length(its) > 1L) list(seq_len(nrow(merged))) else NULL
  )
  scope_nm <- c(its, if (length(its) > 1L) ALL_ITEMS else NULL)
  boots <- stats::setNames(
    lapply(seq_along(scope_ix), function(i) {
      boot_scope(scope_ix[[i]], scope_nm[i])
    }),
    scope_nm
  )
  # Pulled off the shared draws rather than recomputed, so a figure and its
  # interval always describe the same replicates.
  ci_for <- function(label, keys) {
    dr <- boots[[label]]
    if (is.null(dr)) {
      return(NULL)
    }
    qs <- ci_percentile(dr, conf_level)
    have <- intersect(names(keys), rownames(qs))
    if (length(have) == 0L) {
      return(NULL)
    }
    stats::setNames(lapply(have, function(k) as.numeric(qs[k, ])), keys[have])
  }

  tab <- lapply(pairs, function(p) {
    key <- paste(p, collapse = "\r")
    pair_keys <- stats::setNames(
      c("agreement", "alpha"),
      paste0(key, "\r", c("agreement", "alpha"))
    )
    rows <- lapply(seq_along(scope_ix), function(i) {
      add_ci(
        row_for(p, scope_ix[[i]], scope_nm[i]),
        ci_for(scope_nm[i], pair_keys)
      )
    })
    dplyr::bind_rows(rows)
  })
  alph <- lapply(seq_along(scope_ix), function(i) {
    add_ci(
      alpha_for(scope_ix[[i]], scope_nm[i]),
      ci_for(scope_nm[i], c(alpha = "alpha"))
    )
  })

  list(
    alpha = dplyr::bind_rows(alph),
    pairs = dplyr::bind_rows(tab),
    coded = merged
  )
}

# Label for the row pooled across questions, parenthesized so it reads as a
# pooled row rather than as a question.
ALL_ITEMS <- "(all items)"

#' Resample whole units and recompute
#'
#' Shared by the intervals on `validate_sample()`, the ones on the ceiling, and
#' the gap, which needs both coefficients recomputed on the same drawn documents
#' rather than on two independent draws.
#' The unit is a document-item pair throughout, so a pair either enters a
#' replicate whole or not at all and the correlation between statistics
#' computed on it is preserved by construction.
#'
#' `stat` receives an index vector into the units and returns a named numeric
#' vector. The names become the columns, so a caller that wants two coefficients
#' from one replicate returns both from one call.
#'
#' @param n_units An integer giving the number of units.
#' @param stat A function of an index vector, returning a named numeric vector.
#' @param b An integer giving the number of replicates.
#' @return A `b` by `length(stat(...))` matrix, columns named by `stat`.
#' @keywords internal
#' @noRd
resample_units <- function(n_units, stat, b = 2000L) {
  point <- stat(seq_len(n_units))
  out <- matrix(
    NA_real_,
    nrow = b,
    ncol = length(point),
    dimnames = list(NULL, names(point))
  )
  for (i in seq_len(b)) {
    out[i, ] <- stat(sample.int(n_units, n_units, replace = TRUE))
  }
  out
}

#' Percentile interval from resampled draws
#'
#' Percentile rather than normal-approximation, because alpha is bounded above
#' by one and a symmetric interval runs past it on a corpus where the coders
#' agreed closely.
#'
#' @param draws A matrix from `resample_units()`.
#' @param level A number giving the coverage.
#' @return A two-column matrix, one row per statistic, columns `lower`, `upper`.
#' @keywords internal
#' @noRd
ci_percentile <- function(draws, level = 0.95) {
  a <- (1 - level) / 2
  out <- t(apply(
    draws,
    2L,
    function(x) stats::quantile(x, c(a, 1 - a), na.rm = TRUE, names = FALSE)
  ))
  colnames(out) <- c("lower", "upper")
  out
}

#' An interval as one printable column
#'
#' A bracketed string, because a numeric pair needs two columns and a table with
#' two columns per metric stops being readable at four metrics.
#'
#' @param lower,upper Numbers giving the bounds.
#' @param digits An integer giving the decimal places.
#' @return Character, `NA` wherever either bound is `NA`.
#' @keywords internal
#' @noRd
fmt_ci <- function(lower, upper, digits = 2L) {
  out <- rep(NA_character_, length(lower))
  ok <- !is.na(lower) & !is.na(upper)
  out[ok] <- sprintf(
    "[%s, %s]",
    formatC(lower[ok], format = "f", digits = digits),
    formatC(upper[ok], format = "f", digits = digits)
  )
  out
}

#' Krippendorff's alpha together with the terms it is built from
#'
#' Written out rather than taken from a package, because it sits directly under
#' numbers the vignettes publish and a dependency would put those numbers
#' somewhere this package cannot see.
#'
#' Alpha is built from a coincidence matrix rather than from pairwise
#' comparisons, which is what lets one coefficient cover any number of coders
#' and units coded by different subsets of them. A unit seen by fewer than two
#' coders contributes nothing and is dropped, since it carries no agreement to
#' measure.
#'
#' Under the nominal metric every disagreement counts the same, which is right
#' for unordered labels and wrong for ordered ones. Under the ordinal metric a
#' disagreement is charged by how far apart the two labels sit on the scale,
#' weighted by how often the labels between them were used, so two coders one
#' step apart are treated as closer to agreeing than two coders at opposite
#' ends. The difference function is Krippendorff's, the squared distance from
#' `c` to `k` measured as the sum of the marginal frequencies spanned, less half
#' the two endpoints.
#'
#' Verified against Krippendorff's published worked example, four observers and
#' twelve units with missing data, where he reports 0.743 nominal and 0.815
#' ordinal.
#'
#' @param ratings A matrix or data frame, one row per unit and one column per
#'   coder. `NA` marks a unit that coder did not see.
#' @param metric A string, `"nominal"` or `"ordinal"`.
#' @param labels A character vector giving, under `"ordinal"`, every label in
#'   scale order, least to greatest. Required, because no order can be recovered
#'   from the data. Under `"nominal"` it is ignored and the labels are sorted
#'   for indexing only, which does not affect the result.
#' @return A list. `alpha` is the coefficient, or `NA_real_` where it is
#'   undefined, meaning fewer than two usable units or only one label across all
#'   of them. `expected` and `observed` are disagreement per pairable value, on
#'   the scale the chosen `metric` defines, so they compare within a metric and
#'   not across one. `n` counts pairable values, `marginals` is their
#'   distribution over the labels seen, and `metric` records which was used.
#' @keywords internal
#' @noRd
kripp_parts <- function(
  ratings,
  metric = c("nominal", "ordinal"),
  labels = NULL
) {
  metric <- match.arg(metric)
  undefined <- function(marg = numeric(0)) {
    list(
      alpha = NA_real_,
      expected = NA_real_,
      observed = NA_real_,
      n = NA_real_,
      marginals = marg,
      metric = metric
    )
  }
  units <- lapply(seq_len(nrow(ratings)), function(i) {
    v <- as.character(unlist(ratings[i, ], use.names = FALSE))
    v[!is.na(v) & nzchar(v)]
  })
  keep <- vapply(units, length, integer(1)) >= 2L
  units <- units[keep]
  if (length(units) == 0L) {
    return(undefined())
  }

  lv <- sort(unique(unlist(units)))
  # One label everywhere leaves no room to disagree, so expected disagreement
  # is zero and the ratio is undefined. NA rather than 1, which would read as
  # perfect agreement established rather than as a quantity that does not
  # exist.
  if (length(lv) < 2L) {
    return(undefined())
  }

  if (metric == "ordinal") {
    if (is.null(labels)) {
      stop(
        fmt_err(paste0(
          "`level = \"ordinal\"` needs `labels` giving the scale in order. ",
          "No order can be recovered from the sheets, and guessing one ",
          "would produce a number that looks right and is not."
        )),
        call. = FALSE
      )
    }
    labels <- as.character(labels)
    if (anyDuplicated(labels)) {
      stop(fmt_err("`labels` must not repeat a label."), call. = FALSE)
    }
    unseen <- setdiff(lv, labels)
    if (length(unseen)) {
      stop(
        fmt_err(sprintf(
          paste0(
            "`labels` is missing %s, which the coders used. Every label ",
            "has to have a place on the scale."
          ),
          paste(sprintf("`%s`", unseen), collapse = ", ")
        )),
        call. = FALSE
      )
    }
    # Labels nobody used stay on the scale. They carry zero frequency, so they
    # add nothing to the distance between the labels that were used, which is
    # what Krippendorff's own example shows with its unused rank.
    lv <- labels
  }

  k <- length(lv)
  o <- matrix(0, k, k)
  for (i in seq_along(units)) {
    u <- units[[i]]
    cnt <- tabulate(match(u, lv), nbins = k)
    o <- o + (outer(cnt, cnt) - diag(cnt, k)) / (length(u) - 1L)
  }

  nc <- rowSums(o)
  n <- sum(nc)
  if (n < 2) {
    return(undefined(stats::setNames(nc, lv)))
  }

  d2 <- delta_squared(nc, metric)
  de <- sum(outer(nc, nc) * d2)
  if (de == 0) {
    return(undefined(stats::setNames(nc, lv)))
  }
  do <- sum(o * d2)
  list(
    alpha = 1 - (n - 1) * do / de,
    expected = de / (n * (n - 1)),
    observed = do / n,
    n = n,
    marginals = stats::setNames(nc, lv),
    metric = metric
  )
}

#' Krippendorff's alpha
#'
#' The coefficient on its own, for callers that want nothing else.
#'
#' @param ratings A matrix or data frame, one row per unit, one column per
#'   coder.
#' @param metric A string, `"nominal"` or `"ordinal"`.
#' @param labels A character vector of the scale in order. Required under
#'   `"ordinal"`.
#' @return The coefficient, or `NA_real_` where it is undefined.
#' @keywords internal
#' @noRd
kripp_alpha <- function(
  ratings,
  metric = c("nominal", "ordinal"),
  labels = NULL
) {
  kripp_parts(ratings, metric = metric, labels = labels)$alpha
}

#' The scale an instrument already declared
#'
#' `instrument()` documents `scores` as asserting an order the package cannot
#' verify, which makes it the scale. Reading it here stops a user retyping
#' something they have already written down.
#'
#' @param instrument A `fathom_instrument`, or `NULL`.
#' @return The labels in scale order, or `NULL` when no scores were declared.
#' @keywords internal
#' @noRd
labels_from_instrument <- function(instrument) {
  if (is.null(instrument) || !is.list(instrument)) {
    return(NULL)
  }
  sc <- instrument$scores
  if (is.null(sc) || !is.numeric(sc) || is.null(names(sc))) {
    return(NULL)
  }
  names(sc)[order(sc)]
}

#' Krippendorff's difference functions
#'
#' How much a disagreement between two labels costs. Nominal charges 1 for any
#' mismatch. Ordinal charges the squared distance along the scale, measured in
#' marginal frequencies rather than in positions, so a gap across heavily used
#' labels costs more than a gap across rarely used ones.
#'
#' On exactly two labels the two metrics coincide, since the single off-diagonal
#' cost is a constant that cancels out of the ratio.
#'
#' @param nc A numeric vector of coincidence-matrix marginals, in scale order
#'   under `"ordinal"`.
#' @param metric A string, `"nominal"` or `"ordinal"`.
#' @return A square matrix of squared differences, zero on the diagonal.
#' @keywords internal
#' @noRd
delta_squared <- function(nc, metric) {
  k <- length(nc)
  if (metric == "nominal") {
    d <- matrix(1, k, k)
    diag(d) <- 0
    return(d)
  }
  outer(
    seq_len(k),
    seq_len(k),
    Vectorize(function(p, q) {
      if (p == q) {
        return(0)
      }
      span <- seq(min(p, q), max(p, q))
      (sum(nc[span]) - (nc[p] + nc[q]) / 2)^2
    })
  )
}

# Said once, under the first table that carries an interval, so a printed
# bracket says what it covers. Silent on objects from before the argument.
coverage_note <- function(x, tbl) {
  lvl <- attr(x, "conf_level")
  if (is.null(lvl) || is.null(tbl) || !any(grepl("_ci$", names(tbl)))) {
    return(FALSE)
  }
  cat_note("Note", sprintf("Intervals cover %s percent.", format(100 * lvl)))
  TRUE
}

# Plain comments rather than roxygen, matching the other print methods. A
# roxygen block here generates an .Rd file, pkgdown then requires the topic in
# the reference index, and the build fails on its absence. The other print
# methods carry `@export` alone for that reason.
#' @export
print.fathom_validation <- function(x, ...) {
  # Read with [[ so a hand-built object missing a column prints without a
  # warning, and the field it would fill is left out.
  coders <- attr(x, "coders")
  items <- unique(unlist(lapply(
    c("discrimination", "calibration"),
    function(k) x[[k]][["item"]]
  )))
  items <- setdiff(items, ALL_ITEMS)
  n_coded <- attr(x, "n_coded") %||% if (!is.null(x$coded)) nrow(x$coded)
  cat(
    "Validation against human coders\n\n",
    fmt_fields(list(
      `Coded rows` = if (!is.null(n_coded)) format(n_coded, big.mark = ","),
      Items = if (length(items)) as.character(length(items)),
      Coders = if (length(coders)) {
        sprintf("%d (%s)", length(coders), fmt_names(coders))
      },
      # A validation built before the rename stores it as "metric".
      Level = attr(x, "level") %||% attr(x, "metric")
    )),
    "\n",
    sep = ""
  )

  # Said once, under the first table that carries an interval.
  said <- FALSE
  note_once <- function(tbl) {
    if (!said) {
      said <<- coverage_note(x, tbl)
    }
  }

  if (!is.null(x$discrimination)) {
    cat("\nDiscrimination, did the model pick the label the coder did\n")
    print(x$discrimination)
    note_once(x$discrimination)
  }
  if (!is.null(x$categories)) {
    cat("\nCategories, precision, recall and AUC for each label\n")
    print(x$categories)
    note_once(x$categories)
  }
  if (!is.null(x$calibration)) {
    cat("\nCalibration, were the probabilities honest\n")
    print(x$calibration)
    note_once(x$calibration)
  }

  # Present only on the several-sheet path.
  if (!is.null(x$ceiling$pairs)) {
    cat("\nThe ceiling those coders set between themselves\n")
    print(x$ceiling$pairs)
    note_once(x$ceiling$pairs)
  }
  if (!is.null(x$gap)) {
    cat(
      "\nThe model's average alpha with the coders, less theirs with each other\n"
    )
    print(x$gap)
    note_once(x$gap)
    cat_note(
      "Note",
      paste(
        "gap is positive where the model agreed with the coders more",
        "closely than they agreed with each other."
      )
    )
  }

  extra <- list(
    `$ceiling` = if (!is.null(x$ceiling$alpha)) {
      sprintf(
        "$alpha holds alpha over all coders at once, %d row(s)",
        nrow(x$ceiling$alpha)
      )
    },
    `$coded` = if (!is.null(x$coded)) {
      sprintf(
        "Coded rows joined to the model's answer, %d row(s)",
        nrow(x$coded)
      )
    }
  )
  extra <- Filter(Negate(is.null), extra)
  if (length(extra)) {
    cat("\nAlso in the object\n\n", fmt_fields(extra), "\n", sep = "")
  }
  invisible(x)
}
