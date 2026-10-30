# The statistics here are written out by hand rather than taken from a package,
# and they sit directly under the numbers the vignettes publish. So the values
# asserted are closed forms worked out away from the code, not figures read back
# off a run.
#
# The offline harness gives every call the same distribution, so P(YES) is one
# number across the corpus and every expectation below can be written in terms
# of it.

# P(YES) renormalized, under the default stub. Written once here because most of
# the file is expressed in it.
p_yes <- function() exp(-0.01) / (exp(-0.01) + exp(-4.6))

# Six documents, one item, and a sheet whose two halves disagree completely.
val_case <- function(env = parent.frame()) {
  local_offline(env = env)
  d <- tibble::tibble(id = as.character(1:6), body = rep("doc", 6))
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  sheet <- tibble::tibble(
    id = as.character(1:6),
    item = "a",
    human_label = c("NO", "NO", "NO", "YES", "YES", "YES")
  )
  list(res = res, sheet = sheet)
}

# Two coders over the six rows val_case() scores, disagreeing on four of them
# so neither the ceiling nor the gap is computed on a tie.
many_case <- function(env = parent.frame(), q = NULL) {
  cs <- val_case(env = env)
  luis <- cs$sheet
  luis$human_label <- c("NO", "YES", "NO", "YES", "NO", "YES")
  # The stub answers every call alike, so a corpus whose confidence varies has
  # to be set on the results. `prob` and `prob_renorm` move together, since the
  # winner is read off the first and the figures off the second.
  if (!is.null(q)) {
    pr <- stats::setNames(q, cs$sheet$id)[cs$res$probs$id]
    win <- cs$res$probs$label == "YES"
    cs$res$probs$prob_renorm <- ifelse(win, pr, 1 - pr)
    cs$res$probs$prob <- cs$res$probs$prob_renorm
  }
  list(res = cs$res, ana = cs$sheet, luis = luis)
}

run_many <- function(sheets, res, ...) {
  suppressMessages(suppressWarnings(
    validate_sample(sheets, res, bootstrap_n = 200L, ...)
  ))
}

run_val <- function(sheet, res, ...) {
  suppressMessages(suppressWarnings(validate_sample(sheet, res, ...)))
}

# Three labels, built by hand so the probabilities are exactly what the checks
# need. validate_sample() reads id, item, label, prob, prob_renorm, top1_prob
# and call_ok off the results and nothing else. Two labels would not do, since
# dropping one from two leaves one and trips the other refusal first.
abc_results <- function() {
  tibble::tibble(
    id = rep(as.character(1:6), each = 3),
    item = "q",
    label = rep(c("A", "B", "C"), times = 6),
    prob = rep(c(0.6, 0.3, 0.1), times = 6),
    prob_renorm = rep(c(0.6, 0.3, 0.1), times = 6),
    top1_prob = 0.6,
    call_ok = TRUE
  )
}

abc_sheet <- function() {
  tibble::tibble(
    id = as.character(1:6),
    item = "q",
    human_label = rep(c("A", "B"), times = 3)
  )
}

# coder_agreement() is internal and takes the sheets as one list, which is how
# validate_sample() hands them over. This keeps the tests below reading the way
# a call with one argument per coder reads.
ca <- function(
  ...,
  level = "nominal",
  labels = NULL,
  instrument = NULL,
  ci = "none",
  conf_level = 0.95,
  bootstrap_n = 2000L,
  seed = 1234L
) {
  fathom:::coder_agreement(
    list(...),
    metric = level,
    labels = labels %||% fathom:::labels_from_instrument(instrument),
    ci = ci,
    conf_level = conf_level,
    bootstrap_n = bootstrap_n,
    seed = seed
  )
}

# --- the hand-rolled statistics ---------------------------------------------

test_that("results with labels removed are refused rather than scored", {
  # Subsetting results by label is an idiom the vignettes use for other calls.
  # Reaching validate_sample() with one leaves model_label as the argmax of
  # whatever survived, so agreement, alpha, precision, recall and Brier would
  # all be wrong while still printing. prob_renorm summing short of one is what
  # detects it.
  full <- abc_results()
  cut_frame <- full[full$label != "C", , drop = FALSE]

  expect_error(
    run_val(abc_sheet(), cut_frame),
    "missing labels"
  )
  expect_error(
    run_val(abc_sheet(), cut_frame),
    "argmax of whatever survived"
  )
})

test_that("results subset by item are not refused", {
  # Filtering rows by item keeps every label for the rows that remain, so the
  # sums stay at one and both the floor and the winner are still right. The
  # check has to separate that from a label filter rather than refusing both.
  z <- abc_results()
  z$item <- "z"
  two_items <- rbind(abc_results(), z)
  one_item <- two_items[two_items$item == "q", , drop = FALSE]

  expect_no_error(run_val(abc_sheet(), one_item))
})

test_that("a one-label results frame is refused", {
  # A complete frame can still carry one label when the model put every unit of
  # mass on it, so prob_renorm sums to one and the completeness check passes.
  # Nothing is then left to agree or disagree about.
  full <- abc_results()
  one <- full[full$label == "A", , drop = FALSE]
  one$prob_renorm <- 1
  one$prob <- 1

  expect_error(
    run_val(abc_sheet(), one),
    "carry one label"
  )
})

test_that("calibration and coded are returned only on request, and print", {
  # The default object is the two tables every run reads. The probabilities
  # and the joined rows are there when a run asks for them.
  cs <- val_case()

  v <- run_val(cs$sheet, cs$res)
  expect_named(v, c("discrimination", "categories"))
  expect_null(v$calibration)
  expect_null(v$coded)
  expect_output(print(v), "Discrimination")
  expect_output(print(v), "Categories")
  expect_match(printed(v), "^Validation against human coders")
  expect_match(printed(v), "Coded rows 6")

  both <- run_val(cs$sheet, cs$res, calibration = TRUE, coded = TRUE)
  expect_named(both, c("discrimination", "categories", "calibration", "coded"))
  expect_output(print(both), "Calibration")
  expect_match(
    printed(both),
    "\\$coded Coded rows joined to the model's answer"
  )

  # Nothing removed prints as the word NULL, and no coded row reaches the
  # console.
  expect_false(any(grepl("^NULL", capture.output(print(v)))))
  expect_false(grepl("human_label", printed(both)))
  expect_identical(withVisible(print(v))$visible, FALSE)
})

test_that("the switches refuse anything but TRUE or FALSE", {
  cs <- val_case()
  expect_error(
    validate_sample(cs$sheet, cs$res, calibration = "yes"),
    "`calibration` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    validate_sample(cs$sheet, cs$res, coded = NA),
    "`coded` must be TRUE or FALSE",
    fixed = TRUE
  )

  # The arguments the old shape took are refused by name rather than ignored.
  expect_error(
    validate_sample(cs$sheet, cs$res, target = "YES"),
    "Named:\\s+target"
  )
  expect_error(
    validate_sample(cs$sheet, cs$res, metrics = "label"),
    "Named:\\s+metrics"
  )
})

test_that("validate_sample records the call that produced it", {
  cs <- val_case()
  v <- run_val(cs$sheet, cs$res)
  expect_match(attr(v, "run_at"), "^\\d{4}-\\d{2}-\\d{2}T")
  expect_false(is.null(attr(v, "call")))
  expect_identical(attr(v, "level"), "nominal")
  expect_identical(attr(v, "n_coded"), 6L)
})

test_that("auc is the Mann-Whitney statistic, ties counted as half", {
  # The chance that a positive scores above a negative, counted pair by pair
  # here and computed from midranks in the function. The two have to agree
  # through ties, which is where a naive implementation breaks.
  brute <- function(score, pos) {
    p <- score[pos]
    n <- score[!pos]
    mean(outer(p, n, ">") + 0.5 * outer(p, n, "=="))
  }

  set.seed(7)
  for (score in list(
    runif(60),
    round(runif(60), 1),
    as.numeric(runif(60) > 0.5)
  )) {
    pos <- runif(60) < stats::plogis(3 * (score - 0.5))
    expect_equal(fathom:::auc_of(score, pos), brute(score, pos))
  }

  # One class missing leaves nothing to rank against.
  expect_true(is.na(fathom:::auc_of(runif(5), rep(TRUE, 5))))
})

test_that("every label gets precision, recall and auc, per question", {
  # Model answers YES on 2, 4 and 6 and NO elsewhere. The coder says NO on the
  # first three and YES on the last three, so each label has two hits, one
  # false alarm and one miss.
  m <- many_case(q = c(0.2, 0.6, 0.3, 0.9, 0.4, 0.8))
  v <- run_val(m$ana, m$res)
  cats <- v$categories

  expect_named(cats, c("item", "label", "n", "precision", "recall", "auc"))
  expect_equal(cats$item, c("a", "a"))
  expect_equal(cats$label, c("YES", "NO"))
  expect_equal(cats$n, c(3L, 3L))
  expect_equal(cats$precision, c(2 / 3, 2 / 3))
  expect_equal(cats$recall, c(2 / 3, 2 / 3))

  # Of the nine pairs of a coder YES against a coder NO, the model's P(YES)
  # ranks eight the right way round.
  expect_equal(cats$auc[cats$label == "YES"], 8 / 9)

  # On two labels one probability is one minus the other, so the two rows rank
  # the same pairs and their auc is equal.
  expect_equal(cats$auc[1], cats$auc[2])

  # Precision and recall on a label are equal exactly when the model used it as
  # often as the coder did, which is the case here, three each.
  expect_equal(cats$precision, cats$recall)
})

test_that("a label the model never chose is NA rather than zero", {
  # The stub answers YES everywhere, so no row was called NO by the model and
  # precision on NO has no denominator. Recall on NO is zero, since the coder
  # called three rows NO and the model caught none of them.
  cs <- val_case()
  cats <- run_val(cs$sheet, cs$res)$categories

  no <- cats[cats$label == "NO", ]
  yes <- cats[cats$label == "YES", ]
  expect_true(is.na(no$precision))
  expect_equal(no$recall, 0)
  expect_equal(yes$precision, 0.5)
  expect_equal(yes$recall, 1)
})

# Three coders over four rows. Row 1 all agree, row 2 splits two to one, row 3
# is a three-way tie, row 4 only ana saw.
ref_sheets <- function() {
  mk <- function(lab) {
    tibble::tibble(id = as.character(1:4), item = "q", human_label = lab)
  }
  list(
    ana = mk(c("YES", "YES", "YES", "YES")),
    luis = mk(c("YES", "YES", "NO", NA)),
    bea = mk(c("YES", "NO", "MAYBE", NA))
  )
}

test_that("majority takes the most common label and refuses to break a tie", {
  # Breaking a tie would invent a coding no coder wrote.
  s <- ref_sheets()
  r <- suppressMessages(
    coder_reference(ana = s$ana, luis = s$luis, bea = s$bea)
  )

  expect_equal(r$human_label, c("YES", "YES", NA, "YES"))
  expect_equal(r$n_coders, c(3L, 3L, 3L, 1L))
  expect_equal(r$n_agree, c(3L, 2L, 1L, 1L))
  expect_message(
    coder_reference(ana = s$ana, luis = s$luis, bea = s$bea),
    "left blank"
  )
})

test_that("unanimous keeps only rows every coder settled, and warns", {
  # What it discards are the hard rows, so the reference that survives is
  # easier than the draw was.
  s <- ref_sheets()
  r <- suppressWarnings(suppressMessages(
    coder_reference(
      ana = s$ana,
      luis = s$luis,
      bea = s$bea,
      method = "unanimous"
    )
  ))

  expect_equal(r$human_label, c("YES", NA, NA, "YES"))
  expect_warning(
    suppressMessages(coder_reference(
      ana = s$ana,
      luis = s$luis,
      bea = s$bea,
      method = "unanimous"
    )),
    "discards every row the coders split on"
  )
})

test_that("the reference is a sheet validate_sample() reads", {
  # Returning a sheet is what lets it compose without a signature of its own.
  s <- ref_sheets()
  r <- suppressMessages(
    coder_reference(ana = s$ana, luis = s$luis, bea = s$bea)
  )

  expect_true(is.data.frame(r))
  expect_named(r, c("id", "item", "human_label", "n_coders", "n_agree"))
  expect_true(is.call(attr(r, "call")))
})

test_that("a coder cannot be named method", {
  # Under the default it would never reach `...` and would vanish silently.
  s <- ref_sheets()
  # fmt_err() wraps at 80 columns and the break lands wherever the message
  # length puts it, so `\\s+` goes at every space rather than at the one that
  # happens to break today. A fixed substring here has already broken twice.
  expect_error(
    coder_reference(ana = s$ana, method = s$luis),
    "cannot\\s+be\\s+named\\s+`method`"
  )
})

test_that("soft keeps the disagreement as a smoothed proportion", {
  # (n_target + 1) / (n_coders + 2). Two coders who both said YES give 0.75,
  # not 1, which is the honest reading of two observations and keeps the
  # target off a boundary where a log score would be infinite.
  s <- ref_sheets()
  s <- lapply(s, function(x) {
    x$human_label[x$human_label == "MAYBE"] <- "NO"
    x
  })
  r <- coder_reference(
    ana = s$ana,
    luis = s$luis,
    bea = s$bea,
    method = "soft"
  )

  expect_false("human_label" %in% names(r))
  expect_equal(r$human_target, rep("NO", 4))
  expect_equal(r$n_target, c(0L, 1L, 2L, 0L))
  expect_equal(r$human_prop, c(1, 2, 3, 1) / c(5, 5, 5, 3))
  expect_true(all(r$human_prop > 0 & r$human_prop < 1))

  # target is recorded rather than implied, and can be named.
  y <- coder_reference(
    ana = s$ana,
    luis = s$luis,
    bea = s$bea,
    method = "soft",
    target = "YES"
  )
  expect_equal(y$human_target, rep("YES", 4))
  expect_equal(y$human_prop, 1 - r$human_prop)
})

test_that("soft refuses above two labels rather than rescaling", {
  # One proportion cannot describe three labels, and reporting one against the
  # rest would put a two-category Brier beside a k-category one.
  s <- ref_sheets()
  expect_error(
    coder_reference(ana = s$ana, luis = s$luis, bea = s$bea, method = "soft"),
    "needs exactly two labels"
  )
})

test_that("validate_sample branches on which column arrived", {
  # A proportion has no second categorical variable, so the label tables are
  # not returned at all rather than returned full of NA, and the calibration
  # table comes back whatever the switch says.
  cs <- val_case()
  soft <- tibble::tibble(
    id = cs$sheet$id,
    item = cs$sheet$item,
    human_prop = c(0.2, 0.2, 0.2, 0.8, 0.8, 0.8),
    human_target = "YES"
  )
  v <- suppressMessages(validate_sample(soft, cs$res, coded = TRUE))

  expect_null(v$discrimination)
  expect_null(v$categories)
  expect_false(is.null(v$calibration))

  # Two-category Brier against the proportion, which is the hard Brier on two
  # labels with a target that happens to be two-valued. The score reads the
  # probability given to the target, not the largest of the k.
  p <- v$coded[["YES"]]
  y <- v$coded$human_prop
  expect_equal(v$coded$brier, (p - y)^2 + ((1 - p) - (1 - y))^2)

  expect_message(validate_sample(soft, cs$res), "no second categorical")
})

test_that("a soft sheet without human_target is refused", {
  # Which label the proportion is of cannot be recovered from the number.
  cs <- val_case()
  bad <- tibble::tibble(
    id = cs$sheet$id,
    item = cs$sheet$item,
    human_prop = 0.5
  )
  expect_error(validate_sample(bad, cs$res), "but no `human_target`")

  out <- tibble::tibble(
    id = cs$sheet$id,
    item = cs$sheet$item,
    human_prop = 1.5,
    human_target = "YES"
  )
  expect_error(
    validate_sample(out, cs$res),
    "must be a proportion between 0 and 1"
  )
})

test_that("the resampler draws whole units and keeps statistics paired", {
  # A unit enters a replicate whole or not at all, which is what preserves the
  # correlation between two statistics computed on the same drawn documents.
  # The gap needs that and cannot get it from two separate draws.
  set.seed(42)
  x <- stats::rbinom(200, 1, 0.7)
  st <- function(ix) c(mean = mean(x[ix]), sd = stats::sd(x[ix]))
  dr <- fathom:::resample_units(200L, st, b = 500L)

  expect_equal(dim(dr), c(500L, 2L))
  expect_equal(colnames(dr), c("mean", "sd"))
  expect_false(anyNA(dr))

  # Both statistics come back from one replicate rather than two draws.
  expect_equal(ncol(dr), 2L)
})

test_that("the percentile interval lands where an analytic one would", {
  # Percentile rather than normal-approximation, because alpha is bounded at
  # one and a symmetric interval runs past it. On a mean, where the normal
  # approximation is good, the two should agree.
  set.seed(42)
  x <- stats::rbinom(200, 1, 0.7)
  dr <- fathom:::resample_units(
    200L,
    function(ix) c(m = mean(x[ix])),
    b = 2000L
  )
  ci <- fathom:::ci_percentile(dr)

  expect_equal(colnames(ci), c("lower", "upper"))
  an <- mean(x) + c(-1, 1) * 1.96 * sqrt(mean(x) * (1 - mean(x)) / 200)
  expect_lt(max(abs(as.numeric(ci["m", ]) - an)), 0.01)
  expect_lt(ci["m", "lower"], ci["m", "upper"])
})

test_that("an interval formats as one column and stays NA when it is NA", {
  # One bracketed string, because two numeric columns per metric stops being
  # readable at four metrics.
  out <- fathom:::fmt_ci(c(0.7912, 0.9555, NA, 0.5), c(0.9643, 1, 0.8, NA))

  expect_equal(out[1], "[0.79, 0.96]")
  expect_equal(out[2], "[0.96, 1.00]")
  expect_true(is.na(out[3]))
  expect_true(is.na(out[4]))
  expect_equal(fathom:::fmt_ci(0.5, 0.6, digits = 3L), "[0.500, 0.600]")
})

test_that("ci = none leaves the interval columns absent, not NA", {
  # A column of NA reads as a computation that failed. An absent column reads
  # as a question nobody asked, which is what happened.
  cs <- val_case()
  v <- run_val(cs$sheet, cs$res, calibration = TRUE)

  expect_false(any(grepl("_ci$", names(v$discrimination))))
  expect_false(any(grepl("_ci$", names(v$categories))))
  expect_false(any(grepl("_ci$", names(v$calibration))))
})

test_that("the documented column sets match the tables that get built", {
  # validate.R. The `@return` block names the columns of every table, which
  # makes it a contract, and R CMD check does not compare documentation against
  # code. The names below are the documented ones, transcribed, and they exist
  # to be compared against what the function returns.
  #
  # Under `ci = "none"` the sets are exact, since add_ci() is the only thing
  # that adds to them and it adds nothing here.
  cs <- val_case()
  v <- run_val(cs$sheet, cs$res, calibration = TRUE)

  expect_named(v$discrimination, c("item", "n", "agreement", "alpha"))
  expect_named(
    v$categories,
    c("item", "label", "n", "precision", "recall", "auc")
  )
  expect_named(v$calibration, c("item", "n", "brier"))
})

test_that("ci = bootstrap puts an interval beside each figure", {
  # One bracketed column per metric, placed immediately after its own figure,
  # because a reader compares a number with its own interval and not with the
  # one four columns away.
  cs <- val_case()
  v <- run_val(
    cs$sheet,
    cs$res,
    calibration = TRUE,
    ci = "bootstrap",
    bootstrap_n = 200L
  )
  next_to <- function(tb, m) {
    expect_true(paste0(m, "_ci") %in% names(tb))
    expect_equal(
      which(names(tb) == paste0(m, "_ci")),
      which(names(tb) == m) + 1L
    )
  }
  for (m in c("agreement", "alpha")) {
    next_to(v$discrimination, m)
  }
  for (m in c("precision", "recall", "auc")) {
    next_to(v$categories, m)
  }
  next_to(v$calibration, "brier")

  # A bracketed string, and the point estimate sits inside it.
  d <- v$discrimination
  expect_match(d$agreement_ci, "^\\[[0-9.]+, [0-9.]+\\]$")
  bounds <- as.numeric(strsplit(gsub("\\[|\\]", "", d$agreement_ci), ", ")[[1]])
  expect_lte(bounds[1], d$agreement)
  expect_gte(bounds[2], d$agreement)
})

test_that("turning calibration on changes no other interval", {
  # Every figure for a question comes off one resample, so asking for one more
  # table cannot move the intervals on the tables already there.
  m <- many_case(q = c(0.2, 0.6, 0.3, 0.9, 0.4, 0.8))
  off <- run_val(m$ana, m$res, ci = "bootstrap", bootstrap_n = 200L)
  on <- run_val(
    m$ana,
    m$res,
    ci = "bootstrap",
    bootstrap_n = 200L,
    calibration = TRUE
  )

  expect_identical(off$discrimination, on$discrimination)
  expect_identical(off$categories, on$categories)
})

test_that("bootstrap_n below a hundred is refused", {
  # A percentile interval reads the tails, and below a hundred replicates the
  # tail is a handful of draws.
  cs <- val_case()
  expect_error(
    validate_sample(cs$sheet, cs$res, ci = "bootstrap", bootstrap_n = 20),
    "at\\s+least\\s+100"
  )
})

test_that("a soft reference gets no discrimination interval, having no table", {
  cs <- val_case()
  soft <- tibble::tibble(
    id = cs$sheet$id,
    item = cs$sheet$item,
    human_prop = c(0.2, 0.2, 0.2, 0.8, 0.8, 0.8),
    human_target = "YES"
  )
  v <- suppressMessages(
    validate_sample(soft, cs$res, ci = "bootstrap", bootstrap_n = 200L)
  )

  expect_null(v$discrimination)
  expect_false(is.null(v$calibration))
  expect_true("brier_ci" %in% names(v$calibration))
})

# --- the gap -----------------------------------------------------------------

# The model's answers and a coder frame over the same sixty documents. The model
# tracks ana closely, two disagreements, and luis less closely, since luis
# departs from ana on five rows.
gap_frames <- function() {
  n <- 60L
  ids <- sprintf("d%02d", seq_len(n))
  a <- rep(c("YES", "NO"), length.out = n)
  flip <- function(x) ifelse(x == "YES", "NO", "YES")
  luis <- a
  luis[c(3, 9, 15, 21, 27)] <- flip(a[c(3, 9, 15, 21, 27)])
  m <- a
  m[c(5, 11)] <- flip(a[c(5, 11)])
  list(
    model = tibble::tibble(id = ids, item = "q", model_label = m),
    coders = tibble::tibble(id = ids, item = "q", ana = a, luis = luis)
  )
}

run_gap <- function(model, coders, nms = c("ana", "luis"), ...) {
  args <- list(
    metric = "nominal",
    labels = NULL,
    bootstrap_n = 200L,
    conf_level = 0.95,
    seed = 1234L
  )
  over <- list(...)
  args[names(over)] <- over
  do.call(fathom:::gap_table, c(list(model, coders, nms), args))
}

test_that("the gap is one row per question on the averaged alphas", {
  # Whether the model agrees with the coders as well as they agree with each
  # other. Which coder the model tracks more closely is in $discrimination,
  # one row per coder, so nothing is lost by averaging here.
  f <- gap_frames()
  g <- run_gap(f$model, f$coders)

  expect_named(
    g,
    c("item", "n", "model_alpha", "coder_alpha", "gap", "gap_ci")
  )
  expect_equal(nrow(g), 1L)
  expect_equal(g$n, 60L)

  alpha_of <- function(x, y) fathom:::kripp_alpha(cbind(x, y))
  m <- f$model$model_label
  expect_equal(
    g$model_alpha,
    mean(c(alpha_of(m, f$coders$ana), alpha_of(m, f$coders$luis)))
  )
  expect_equal(g$coder_alpha, alpha_of(f$coders$ana, f$coders$luis))
  expect_equal(g$gap, g$model_alpha - g$coder_alpha)
  expect_match(g$gap_ci, "^\\[-?[0-9.]+, -?[0-9.]+\\]$")
})

test_that("three coders average over three pairs", {
  f <- gap_frames()
  f$coders$bea <- f$coders$ana
  g <- run_gap(f$model, f$coders, nms = c("ana", "luis", "bea"))

  alpha_of <- function(x, y) fathom:::kripp_alpha(cbind(x, y))
  cs <- f$coders
  expect_equal(
    g$coder_alpha,
    mean(c(
      alpha_of(cs$ana, cs$luis),
      alpha_of(cs$ana, cs$bea),
      alpha_of(cs$luis, cs$bea)
    ))
  )
})

test_that("the gap refuses frames that share no documents", {
  f <- gap_frames()
  apart <- f$coders
  apart$id <- paste0("x", apart$id)
  expect_error(run_gap(f$model, apart), "no rows are common")
})

test_that("a consolidated reference is warned about rather than assumed away", {
  # coder_reference() leaves n_coders on the sheet it consolidates, which rides
  # through to the model's rows, so this is detectable rather than something
  # to ask the caller.
  f <- gap_frames()
  f$model$n_coders <- 3L
  expect_warning(run_gap(f$model, f$coders), "consolidated\\s+reference")
})

# --- several sheets ----------------------------------------------------------

test_that("several sheets score each coder and add the ceiling and the gap", {
  # Three calls otherwise, each told the same metric and labels, with nothing
  # able to check that they were.
  m <- many_case()
  v <- run_many(list(ana = m$ana, luis = m$luis), m$res, coded = TRUE)

  expect_s3_class(v, "fathom_validation")
  expect_named(
    v,
    c("discrimination", "categories", "ceiling", "gap", "coded")
  )
  expect_named(v$ceiling, c("alpha", "pairs"))

  # `coder` sits after `item`, so a row reads as one question against one
  # person.
  for (tb in list(v$discrimination, v$categories)) {
    expect_equal(
      which(names(tb) == "coder"),
      which(names(tb) == "item") + 1L
    )
  }
  expect_setequal(unique(v$discrimination$coder), c("ana", "luis"))
  expect_setequal(unique(v$coded$coder), c("ana", "luis"))
  expect_identical(attr(v, "coders"), c("ana", "luis"))
  expect_identical(attr(v, "n_coded"), 12L)

  # One row per question.
  expect_equal(nrow(v$gap), 1L)
  expect_false(any(c("coder", "versus") %in% names(v$gap)))

  expect_output(print(v), "The ceiling those coders set")
  expect_match(printed(v), "gap is positive where the model agreed")
})

test_that("the several-sheet path reimplements no statistic", {
  # Each coder goes through the one-sheet path, so a block of the stacked
  # table has to equal the standalone call exactly. Anything else means this
  # is computing where it should be orchestrating.
  m <- many_case()
  one <- run_val(m$ana, m$res, calibration = TRUE)
  many <- run_many(list(ana = m$ana, luis = m$luis), m$res, calibration = TRUE)

  strip <- function(tb) {
    tb <- tb[tb$coder == "ana", , drop = FALSE]
    tb$coder <- NULL
    tibble::as_tibble(tb)
  }
  expect_equal(
    strip(many$discrimination),
    tibble::as_tibble(one$discrimination)
  )
  expect_equal(strip(many$categories), tibble::as_tibble(one$categories))
  expect_equal(strip(many$calibration), tibble::as_tibble(one$calibration))

  # And the gap sets the model's alpha with each coder, the figure
  # $discrimination reports, against the alpha the pair reports in the ceiling.
  d <- many$discrimination
  expect_equal(many$gap$model_alpha, mean(d$alpha))
  expect_equal(many$gap$coder_alpha, many$ceiling$pairs$alpha)
})

test_that("a consolidated sheet in a list warns", {
  m <- many_case()
  ref <- suppressMessages(coder_reference(ana = m$ana, luis = m$luis))
  expect_warning(
    suppressMessages(validate_sample(
      list(ana = m$ana, ref = ref),
      m$res,
      bootstrap_n = 200L
    )),
    "consolidated\\s+reference"
  )
})

test_that("the ordinal metric scores calibration on the cumulative rule", {
  # Brier charges a one-level miss and a five-level miss alike, which is the
  # defect that metric exists to remove, so it selects the rule on the
  # calibration table as well as the alpha. The column is named for the
  # quantity.
  m <- many_case()

  v <- run_many(
    list(ana = m$ana, luis = m$luis),
    m$res,
    level = "ordinal",
    labels = c("NO", "YES"),
    calibration = TRUE
  )
  flat <- run_many(list(ana = m$ana, luis = m$luis), m$res, calibration = TRUE)

  expect_true("rps" %in% names(v$calibration))
  expect_false("brier" %in% names(v$calibration))
  expect_true("brier" %in% names(flat$calibration))

  # On two labels the cumulative rule has one term where Brier has two, so the
  # same forecast costs exactly half.
  expect_equal(v$calibration$rps, flat$calibration$brier / 2)
})

test_that("a label with no place on the scale is refused, once", {
  # kripp_parts() already refuses a label the CODERS used with no position.
  # This is the other case, a label the MODEL can emit that no coder used,
  # which reaches neither that check nor the scale and would be cumulated over
  # a partial order.
  res <- abc_results()
  mk <- function(lab) {
    tibble::tibble(id = as.character(1:6), item = "q", human_label = lab)
  }
  # A wins on every row, so the ratings matrix holds only A and B and the
  # ordinal alpha is content with a two-label scale. C has nowhere to sit.
  a <- mk(rep(c("A", "B"), times = 3))
  b <- mk(rep(c("A", "A", "B"), times = 2))

  expect_error(
    validate_sample(a, res, level = "ordinal", labels = c("A", "B")),
    "no\\s+place\\s+for"
  )
  expect_error(
    validate_sample(
      list(x = a, y = b),
      res,
      level = "ordinal",
      labels = c("A", "B"),
      bootstrap_n = 200L
    ),
    "no\\s+place\\s+for"
  )

  # And a scale naming a label the results do not carry is allowed, since it
  # has no column to read and nothing to cumulate over.
  expect_no_error(suppressMessages(suppressWarnings(
    validate_sample(a, res, level = "ordinal", labels = c("A", "B", "C", "D"))
  )))
})

test_that("one sheet has no ceiling and no gap", {
  # Nobody to agree with, so the tables are absent rather than empty.
  m <- many_case()
  one <- run_val(m$ana, m$res)

  expect_null(one$ceiling)
  expect_null(one$gap)
  expect_null(attr(one, "coders"))
})

test_that("a list of one is refused and unnamed sheets are numbered", {
  # An object quietly missing $ceiling and $gap is the shape that varies
  # invisibly, so the single-coder call is named instead.
  m <- many_case()

  expect_error(
    validate_sample(list(m$ana), m$res),
    "two\\s+or\\s+more"
  )

  v <- run_many(list(m$ana, m$luis), m$res)
  expect_setequal(unique(v$discrimination$coder), c("coder1", "coder2"))
})

test_that("precision and recall are NA rather than zero when undefined", {
  # A denominator of zero is a question the sample cannot answer. Zero would
  # read as an item that never got one right.
  g <- data.frame(
    model_label = c("YES", "YES", "NO", "NO"),
    human_label = c("YES", "NO", "YES", "NO")
  )
  expect_equal(unlist(fathom:::prf(g, "YES")), c(precision = 0.5, recall = 0.5))

  none <- data.frame(model_label = c("NO", "NO"), human_label = c("NO", "NO"))
  expect_true(is.na(fathom:::prf(none, "YES")$precision))
  expect_true(is.na(fathom:::prf(none, "YES")$recall))
})

# --- validate_sample ---------------------------------------------------------

test_that("more than one question adds a pooled row and not before", {
  # ab_instrument() carries two questions and val_case() measures both, so the
  # second only needs covering in the sheet. Duplicating the results instead
  # would put two rows on one id, item and label, which is a different fault.
  cs <- val_case()
  two <- dplyr::bind_rows(cs$sheet, dplyr::mutate(cs$sheet, item = "z"))
  v <- run_val(two, cs$res, calibration = TRUE)

  # The pooled row is named to match the ceiling's, so the two tables can be
  # set beside each other.
  expect_equal(v$discrimination$item, c("a", "z", "(all items)"))
  expect_equal(v$calibration$item, v$discrimination$item)

  # A label pooled across questions would rank documents from one question
  # against documents from another, so $categories has no pooled row.
  expect_equal(unique(v$categories$item), c("a", "z"))

  expect_equal(nrow(run_val(cs$sheet, cs$res)$discrimination), 1L)
})

test_that("results bound from two runs are refused, not silently mangled", {
  # pivot_wider() answers a duplicated key with a list column, and the failure
  # then arrives far downstream as a coercion error naming neither the column
  # nor the cause.
  #
  # bind_rows() on the results object itself now errors, since it is a list of
  # two frames rather than one frame, so the accidental version of this mistake
  # is harder to make than it was. The hazard is still reachable by doubling
  # $probs, which is the frame that gets pivoted, and that is what the guard
  # has to catch. $calls is left alone so the join stays many-to-one.
  cs <- val_case()
  doubled <- cs$res
  doubled$probs <- dplyr::bind_rows(cs$res$probs, cs$res$probs)
  expect_error(
    validate_sample(cs$sheet, doubled),
    "more than one row for the same id"
  )
})

test_that("alpha refuses to credit a constant predictor", {
  # The stub answers YES on every call, so the model is a single guess repeated.
  # With the coder saying YES on five rows of six, agreement reads 0.833 and
  # alpha reads 0, because the pooled labels already buy every point of that
  # agreement. An agreement column on its own would not say so.
  cs <- val_case()
  sheet <- cs$sheet
  sheet$human_label <- c("NO", rep("YES", 5))
  v <- run_val(sheet, cs$res, coded = TRUE)

  d <- v$discrimination
  expect_equal(d$agreement, 5 / 6)
  expect_equal(d$alpha, 0)
  expect_false("kappa" %in% names(d))

  # The same coefficient the ceiling reports, which is the whole point of
  # replacing kappa here rather than leaving one of each.
  expect_equal(
    d$alpha,
    fathom:::kripp_alpha(
      data.frame(
        m = v$coded$model_label,
        h = v$coded$human_label,
        stringsAsFactors = FALSE
      )
    )
  )

  # And the original sheet, where half the rows disagree, reads below zero.
  expect_equal(
    round(run_val(cs$sheet, cs$res)$discrimination$alpha, 10),
    round(-2 / 9, 10)
  )
})

test_that("Brier reads the whole distribution, not the chosen label", {
  # Summed over categories of (p_k - y_k)^2, which on two labels is the familiar
  # squared error twice over. A right answer given at 0.99 costs almost nothing
  # and a wrong one costs nearly two, so the column separates a confident miss
  # from an uncertain one.
  cs <- val_case()
  v <- run_val(cs$sheet, cs$res, calibration = TRUE)
  py <- p_yes()

  b_no <- py^2 + (1 - py - 1)^2
  b_yes <- (py - 1)^2 + (1 - py)^2
  expect_equal(v$calibration$brier, (3 * b_no + 3 * b_yes) / 6)
})

test_that("the coded rows come back when asked for", {
  # The sheet joined to the model's answer is what a design-based or
  # prediction-powered correction takes as input. Recomputing that join by hand
  # is where a user gets the key wrong, so it is returned on request rather
  # than built and thrown away.
  cs <- val_case()
  v <- run_val(cs$sheet, cs$res, coded = TRUE)

  expect_s3_class(v$coded, "tbl_df")
  expect_equal(nrow(v$coded), 6L)
  expect_true(all(
    c("id", "item", "human_label", "model_label", "correct", "brier") %in%
      names(v$coded)
  ))
  expect_null(run_val(cs$sheet, cs$res)$coded)
})

test_that("a sheet still carrying the old design columns scores the same", {
  # Sheets coded before the draw went uniform-only carry chance, weight,
  # drawn_n and design. They are read as any other column the coder left on
  # the sheet, which is not at all.
  cs <- val_case()
  old <- cs$sheet
  old$chance <- 0.5
  old$weight <- 2
  old$drawn_n <- 6L
  old$design <- "uncertainty"

  expect_no_warning(suppressMessages(validate_sample(old, cs$res)))
  expect_equal(
    run_val(old, cs$res)$discrimination,
    run_val(cs$sheet, cs$res)$discrimination
  )
})

test_that("a coder's answer outside the label set is refused", {
  # Coders write what they write, and "y" is a data problem rather than a
  # disagreement. Coercing it would silently score a correct answer as wrong.
  cs <- val_case()
  bad <- cs$sheet
  bad$human_label[1] <- "y"

  err <- expect_error(suppressMessages(validate_sample(bad, cs$res)))
  expect_match(conditionMessage(err), "not labels")
  expect_match(conditionMessage(err), "y")
})

test_that("an unusable sheet errors instead of returning an empty table", {
  cs <- val_case()

  blank <- cs$sheet
  blank$human_label <- NA_character_
  expect_error(suppressMessages(validate_sample(blank, cs$res)), "human_label")

  stranger <- cs$sheet
  stranger$id <- paste0("z", stranger$id)
  expect_error(suppressMessages(validate_sample(stranger, cs$res)), "no rows")

  expect_error(
    suppressMessages(validate_sample(cs$sheet[, c("id", "item")], cs$res)),
    "human_label"
  )
})

# --- the ceiling -------------------------------------------------------------

test_that("coder agreement is pairwise on the rows both of them coded", {
  # Four rows, one disagreement, so observed agreement is 0.75. Alpha pools both
  # coders' labels into one distribution, three YES against five NO, rather than
  # correcting against each coder's own marginals.
  a <- tibble::tibble(
    id = as.character(1:4),
    item = "a",
    human_label = c("YES", "YES", "NO", "NO")
  )
  b <- tibble::tibble(
    id = as.character(1:4),
    item = "a",
    human_label = c("YES", "NO", "NO", "NO")
  )

  r <- ca(ana = a, luis = b)

  expect_equal(nrow(r$pairs), 1L)
  expect_equal(r$pairs$coder_a, "ana")
  expect_equal(r$pairs$coder_b, "luis")
  expect_equal(r$pairs$n, 4L)
  expect_equal(r$pairs$agreement, 0.75)
  expect_equal(r$pairs$alpha, 8 / 15)

  # The merged frame is what the gap reads, one column per coder.
  expect_equal(nrow(r$coded), 4L)
  expect_true(all(c("ana", "luis") %in% names(r$coded)))
})

test_that("three coders give three pairs, so an outlier is visible", {
  # One figure across all raters would report a middling number and hide which
  # coder produced it. Three rows say plainly that two agree and one does not.
  ids <- as.character(1:6)
  a <- tibble::tibble(
    id = ids,
    item = "a",
    human_label = rep(c("YES", "NO"), each = 3)
  )
  b <- a
  odd <- tibble::tibble(
    id = ids,
    item = "a",
    human_label = rep(c("NO", "YES"), each = 3)
  )

  r <- ca(a = a, b = b, odd = odd)

  expect_equal(nrow(r$pairs), 3L)
  expect_equal(
    r$pairs$agreement[r$pairs$coder_a == "a" & r$pairs$coder_b == "b"],
    1
  )
  expect_true(all(r$pairs$agreement[r$pairs$coder_b == "odd"] == 0))
})

test_that("unnamed sheets are numbered rather than refused", {
  a <- tibble::tibble(id = "1", item = "a", human_label = "YES")
  b <- tibble::tibble(id = "1", item = "a", human_label = "YES")

  r <- ca(a, b)
  expect_equal(r$pairs$coder_a, "coder1")
  expect_equal(r$pairs$coder_b, "coder2")
})

test_that("a partial overlap is reported rather than absorbed", {
  # A coder skipping a row is fieldwork rather than a design, so it is handled
  # and said out loud. The message partitions the merge, so the three counts
  # have to add back to it.
  a <- tibble::tibble(
    id = as.character(1:4),
    item = "a",
    human_label = rep("YES", 4)
  )
  b <- tibble::tibble(
    id = as.character(1:2),
    item = "a",
    human_label = rep("YES", 2)
  )

  # fmt_lines() wraps at 80 columns, so a phrase can straddle the break and a
  # plain substring will not find it. Flatten the whitespace first, which makes
  # the assertion about what the message says rather than where it wrapped.
  said <- paste(capture_messages(ca(a, b)), collapse = " ")
  said <- gsub("\\s+", " ", said)

  expect_match(said, "4 rows in the merge")
  expect_match(said, "2 seen by every coder")
  expect_match(said, "2 by one alone")
})

test_that("the ceiling refuses what it cannot compare", {
  a <- tibble::tibble(id = "1", item = "a", human_label = "YES")

  expect_error(ca(a), "at least two")
  expect_error(ca(a, a[, c("id", "item")]), "human_label")

  blank <- tibble::tibble(id = "1", item = "a", human_label = NA_character_)
  expect_error(ca(a, blank), "human_label")

  stranger <- tibble::tibble(id = "z9", item = "a", human_label = "NO")
  expect_error(ca(a, stranger), "two or more coders")

  # A coder named for a join key would otherwise fail from inside the join,
  # with a message about duplicate columns that says nothing about coders.
  expect_error(ca(id = a, b = a), "cannot be named")
  expect_error(ca(item = a, b = a), "cannot be named")
})

test_that("the merge keeps every row, so alpha gets what it is built for", {
  # Three coders each skipping two different rows. An inner join would leave
  # alpha the six rows all three reached. A full join leaves it all twelve,
  # since a row two of three saw still carries agreement.
  ids <- as.character(1:12)
  lab <- rep(c("YES", "NO"), 6)
  full <- tibble::tibble(id = ids, item = "t", human_label = lab)

  r <- suppressMessages(ca(
    a = full[-c(1, 2), ],
    b = full[-c(3, 4), ],
    c = full[-c(5, 6), ]
  ))

  expect_equal(nrow(r$coded), 12L)
  expect_equal(r$alpha$n_units, 12L)

  # A pairwise row is narrower than alpha on the same question, because a pair
  # needs the rows both of them saw rather than any two coders.
  expect_true(all(r$pairs$n == 8L))
  expect_lt(max(r$pairs$n), r$alpha$n_units)
})

test_that("a row one coder saw is kept in the merge and dropped from alpha", {
  # Dropping it from the frame would hide that the coder skipped it. Counting
  # it in alpha would credit agreement that nobody could have disagreed with.
  a <- tibble::tibble(
    id = as.character(1:4),
    item = "t",
    human_label = c("YES", "YES", "NO", "NO")
  )
  b <- tibble::tibble(
    id = as.character(1:2),
    item = "t",
    human_label = c("YES", "NO")
  )

  r <- suppressMessages(ca(x = a, y = b))

  expect_equal(nrow(r$coded), 4L)
  expect_equal(sum(is.na(r$coded$y)), 2L)
  expect_equal(r$alpha$n_units, 2L)
  expect_equal(r$pairs$n, 2L)
})

test_that("coder agreement is reported per question, not pooled across them", {
  # The case the ceiling exists to catch. Two coders read one of three
  # questions completely differently. Pooled that lands at 0.633 and reads as
  # mediocre. Per question it reads as broken, which is what it is.
  ids <- as.character(1:20)
  mk <- function(it, a) tibble::tibble(id = ids, item = it, human_label = a)

  neg <- c(rep("YES", 9), rep("NO", 11))
  pos1 <- c(rep("YES", 8), rep("NO", 12))
  pos2 <- replace(pos1, c(8, 9), c("NO", "YES"))
  mix1 <- c(rep("YES", 6), rep("NO", 14))
  mix2 <- c(rep("NO", 6), rep("YES", 14))

  a <- rbind(mk("negative", neg), mk("positive", pos1), mk("mixed", mix1))
  b <- rbind(mk("negative", neg), mk("positive", pos2), mk("mixed", mix2))

  p <- ca(ana = a, luis = b)$pairs

  # Three questions and the pooled row, in the order they appear in the sheet
  # rather than alphabetically.
  expect_equal(nrow(p), 4L)
  expect_equal(p$item, c("negative", "positive", "mixed", "(all items)"))
  expect_equal(p$n, c(20L, 20L, 20L, 60L))

  expect_equal(p$agreement[p$item == "negative"], 1)
  expect_equal(p$agreement[p$item == "mixed"], 0)

  # The pooled row is the one that hides it, which is why it is not the only
  # row the ceiling returns.
  expect_equal(p$agreement[p$item == "(all items)"], 38 / 60)
  expect_gt(p$agreement[p$item == "(all items)"], 0.5)
})

test_that("the pooled row is left out where there is only one question", {
  # It would repeat the only row there is, so a single-question instrument gets
  # one row per pair and no summary of itself.
  ids <- as.character(1:6)
  y <- c("YES", "YES", "YES", "NO", "NO", "NO")
  a <- tibble::tibble(id = ids, item = "tone", human_label = y)
  b <- tibble::tibble(
    id = ids,
    item = "tone",
    human_label = replace(y, 3, "NO")
  )

  p <- ca(a, b)$pairs

  expect_equal(nrow(p), 1L)
  expect_equal(p$item, "tone")
  expect_false("(all items)" %in% p$item)
})

test_that("three coders on two questions give a pair by question table", {
  # Nine rows, three pairs across two questions plus a pooled row each. The odd
  # coder shows up on the question they read differently and not on the one they
  # did not, which pooling would average away.
  ids <- as.character(1:20)
  mk <- function(it, a) tibble::tibble(id = ids, item = it, human_label = a)

  neg <- c(rep("YES", 9), rep("NO", 11))
  pos <- c(rep("YES", 8), rep("NO", 12))

  a <- rbind(mk("negative", neg), mk("positive", pos))
  b <- a
  odd <- rbind(mk("negative", rev(neg)), mk("positive", pos))

  p <- ca(a = a, b = b, odd = odd)$pairs

  expect_equal(nrow(p), 9L)
  expect_setequal(unique(p$item), c("negative", "positive", "(all items)"))
  expect_equal(p$agreement[p$coder_b == "odd" & p$item == "positive"], c(1, 1))
  expect_true(all(p$agreement[p$coder_b == "odd" & p$item == "negative"] < 0.5))
})

test_that("Krippendorff's alpha reproduces its published example", {
  # Krippendorff (2004) section 11.3.3, the data behind the KALPHA macro
  # documentation, which publishes alpha as 0.7434 over 11 units, 4 observers
  # and 55 pairs. Twelve units go in and the twelfth is dropped, since one
  # coder alone leaves no agreement to measure.
  r <- t(rbind(
    obsa = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
    obsb = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
    obsc = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
    obsd = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)
  ))

  expect_equal(round(fathom:::kripp_alpha(r), 4), 0.7434)
  expect_equal(sum(rowSums(!is.na(r)) >= 2), 11L)
  expect_equal(sum(choose(rowSums(!is.na(r)), 2)), 55)
})

test_that("the ordinal metric reproduces its published value too", {
  # Krippendorff publishes both figures for this same example, 0.743 nominal
  # and 0.815 ordinal. Reproducing the second is what says the difference
  # function is his and not one that merely looks reasonable.
  r <- t(rbind(
    obsa = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
    obsb = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, 3),
    obsc = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
    obsd = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)
  ))
  lv <- as.character(1:5)

  expect_equal(round(fathom:::kripp_alpha(r, "ordinal", lv), 4), 0.8154)

  # A rank nobody used sits on the scale and costs nothing, which is the point
  # Krippendorff makes with the unused rank in his own difference table.
  expect_equal(
    fathom:::kripp_alpha(r, "ordinal", c(lv, "9")),
    fathom:::kripp_alpha(r, "ordinal", lv)
  )
})

test_that("the difference function matches Krippendorff's published table", {
  # Page 6 of Computing Krippendorff's Alpha-Reliability tabulates these ten
  # values for the marginals below. They are the whole of what separates the
  # ordinal metric from the nominal one.
  nc <- c(9, 13, 10, 5, 3)
  d <- sqrt(fathom:::delta_squared(nc, "ordinal"))

  expect_equal(d[1, 2], 11)
  expect_equal(d[1, 3], 22.5)
  expect_equal(d[1, 4], 30)
  expect_equal(d[1, 5], 34)
  expect_equal(d[2, 4], 19)
  expect_equal(d[3, 4], 7.5)
  expect_equal(d[4, 5], 4)
  expect_equal(diag(d), rep(0, 5))
  expect_equal(d, t(d))

  # Nominal is the constant that the ordinal metric replaces.
  n <- fathom:::delta_squared(nc, "nominal")
  expect_equal(sum(n), 20)
  expect_equal(diag(n), rep(0, 5))
})

test_that("the two metrics coincide on two labels and diverge above them", {
  # With two labels there is one way to disagree, so the single off-diagonal
  # cost is a constant and cancels out of the ratio. Any divergence here would
  # mean the ordinal branch is doing something the nominal branch is not.
  two <- cbind(
    c("A", "A", "B", "B", "A", "B"),
    c("A", "B", "B", "B", "A", "A")
  )
  expect_equal(
    fathom:::kripp_alpha(two),
    fathom:::kripp_alpha(two, "ordinal", labels = c("A", "B"))
  )

  # Above two labels the distinction is the entire point. Same number of
  # disagreements in both, and only how far apart they are differs.
  lv <- as.character(1:5)
  truth <- rep(lv, each = 8)
  near <- truth
  far <- truth
  moved <- seq(1, 40, by = 2)
  pos <- match(truth[moved], lv)
  # always one step, moving away from whichever end is nearer, so the move can
  # never land back on the truth and both vectors carry the same error rate
  near[moved] <- lv[ifelse(pos == 5L, 4L, pos + 1L)]
  far[moved] <- lv[ifelse(pos <= 3L, 5L, 1L)]

  expect_equal(mean(truth == near), mean(truth == far))
  expect_gt(
    fathom:::kripp_alpha(cbind(truth, near), "ordinal", lv),
    fathom:::kripp_alpha(cbind(truth, far), "ordinal", lv)
  )
})

test_that("the ordinal metric refuses to guess a scale", {
  r <- cbind(c("lo", "lo", "mid", "hi"), c("lo", "mid", "mid", "hi"))
  lv <- c("lo", "mid", "hi")

  # No order can be recovered from sheets, and a guessed one would give a
  # number that looks right and is not, so it is required rather than inferred.
  expect_error(fathom:::kripp_alpha(r, "ordinal"), "needs `labels`")
  expect_error(
    fathom:::kripp_alpha(r, "ordinal", c("lo", "lo", "hi")),
    "must not repeat"
  )
  expect_error(
    fathom:::kripp_alpha(r, "ordinal", c("lo", "mid")),
    "is missing"
  )
  expect_silent(fathom:::kripp_alpha(r, "ordinal", lv))
})

test_that("the ceiling carries the metric through to alpha", {
  lv <- c("lo", "mid", "hi")
  ids <- as.character(1:30)
  a <- tibble::tibble(id = ids, item = "q", human_label = rep(lv, 10))
  # every disagreement is one step, which nominal cannot see and ordinal can
  b <- a
  b$human_label <- ifelse(
    a$human_label == "lo",
    "mid",
    ifelse(a$human_label == "hi", "mid", "lo")
  )

  nom <- ca(c1 = a, c2 = b)
  ordn <- ca(c1 = a, c2 = b, level = "ordinal", labels = lv)

  expect_gt(ordn$alpha$alpha, nom$alpha$alpha)

  # The pairs carry alpha too, so they move with the metric exactly as the
  # overall figure does, which is what makes a pair and the whole set
  # comparable.
  expect_gt(ordn$pairs$alpha, nom$pairs$alpha)
})

test_that("alpha hits its bounds and goes below zero on opposed coders", {
  agree <- cbind(c("Y", "Y", "N", "N"), c("Y", "Y", "N", "N"))
  expect_equal(fathom:::kripp_alpha(agree), 1)

  # Systematic disagreement is worse than chance, which alpha reports as a
  # negative number rather than clamping at zero.
  opposed <- cbind(c("Y", "Y", "N", "N"), c("N", "N", "Y", "Y"))
  expect_lt(fathom:::kripp_alpha(opposed), 0)
})

test_that("alpha is NA rather than a number where it cannot be computed", {
  # One label across every unit leaves no expected disagreement to correct
  # against. Returning 1 would read as perfect agreement established.
  expect_true(is.na(fathom:::kripp_alpha(cbind(rep("Y", 4), rep("Y", 4)))))

  # And a unit only one coder saw carries no agreement, so a matrix of those
  # has nothing usable in it.
  lone <- cbind(c("Y", "N"), c(NA, NA))
  expect_true(is.na(fathom:::kripp_alpha(lone)))
})

test_that("alpha comes back as its own table beside the pairwise one", {
  # Alpha is one coefficient across all the coders, so it cannot sit in a table
  # keyed by pairs. Two tables answering two questions, how much they agreed
  # and which of them is the reason.
  ids <- as.character(1:20)
  mk <- function(it, a) tibble::tibble(id = ids, item = it, human_label = a)
  neg <- c(rep("YES", 9), rep("NO", 11))
  pos <- c(rep("YES", 8), rep("NO", 12))

  a <- rbind(mk("negative", neg), mk("positive", pos))
  b <- a
  odd <- rbind(mk("negative", rev(neg)), mk("positive", pos))

  r <- ca(a = a, b = b, odd = odd)

  # The exact set is asserted so an element added without a decision fails
  # here rather than surfacing in a vignette.
  expect_named(r, c("alpha", "pairs", "coded"))
  expect_equal(r$alpha$item, c("negative", "positive", "(all items)"))
  expect_equal(r$alpha$n_units, c(20L, 20L, 40L))

  # Three coders who agree completely on one question and where one is opposed
  # on the other. Alpha says so in three numbers where pairwise needs nine.
  expect_equal(r$alpha$alpha[r$alpha$item == "positive"], 1)
  expect_lt(r$alpha$alpha[r$alpha$item == "negative"], 0)
})

# --- shared internals ---------------------------------------------------------

test_that("kripp_parts returns the same coefficient kripp_alpha always did", {
  # Krippendorff's own worked example, so the refactor is checked against a
  # published value rather than against whatever it used to return.
  r <- data.frame(
    a = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
    b = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
    c = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, NA),
    d = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA)
  )
  expect_equal(round(fathom:::kripp_parts(r)$alpha, 4), 0.7434)
  expect_equal(fathom:::kripp_parts(r)$alpha, fathom:::kripp_alpha(r))
  expect_equal(fathom:::kripp_parts(r)$metric, "nominal")
})

test_that("kripp_parts reports NA and no marginals where alpha is undefined", {
  one <- data.frame(a = c("Y", "Y"), b = c("Y", "Y"), stringsAsFactors = FALSE)
  p <- fathom:::kripp_parts(one)
  expect_true(is.na(p$alpha))
  expect_true(is.na(p$expected))
  expect_length(p$marginals, 0L)
  expect_equal(p$metric, "nominal")
})

test_that("labels_from_instrument reads the order the user already declared", {
  b <- instrument(
    "How strongly?",
    labels = c("LOW", "MID", "HIGH"),
    exclusive = TRUE,
    scores = c(LOW = 1, MID = 2, HIGH = 3)
  )
  expect_equal(fathom:::labels_from_instrument(b), c("LOW", "MID", "HIGH"))

  unscored <- instrument("Is it?", labels = c("YES", "NO"))
  expect_null(fathom:::labels_from_instrument(unscored))
  expect_null(fathom:::labels_from_instrument(NULL))
})

test_that("labels_from_instrument follows the scores, not the declaration order", {
  # Scores assert an order the package cannot verify, so they are the scale and
  # the order the labels were typed in is not.
  b <- instrument(
    "How strongly?",
    labels = c("HIGH", "LOW", "MID"),
    exclusive = TRUE,
    scores = c(HIGH = 3, LOW = 1, MID = 2)
  )
  expect_equal(fathom:::labels_from_instrument(b), c("LOW", "MID", "HIGH"))
})

test_that("stamp_provenance records when and how, and not which version", {
  x <- fathom:::stamp_provenance(list(a = 1))
  expect_match(attr(x, "run_at"), "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$")
  expect_null(attr(x, "call"))
  # Removed rather than renamed, so the name is asserted absent as well.
  expect_null(attr(x, "fathom"))

  y <- fathom:::stamp_provenance(list(a = 1), call = quote(f(1)))
  expect_equal(attr(y, "call"), quote(f(1)))
})

# --- the ceiling, intervals and its scale -----------------------------------

three_label_sheets <- function() {
  ids <- as.character(seq_len(50))
  list(
    r1 = tibble::tibble(
      id = ids,
      item = "a",
      human_label = c(rep("A", 20), rep("B", 20), rep("C", 10))
    ),
    r2 = tibble::tibble(
      id = ids,
      item = "a",
      human_label = c(rep("A", 20), rep("B", 20), rep("A", 5), rep("B", 5))
    )
  )
}

test_that("the ceiling's intervals come off one shared resample", {
  # Every figure for a question is read from the same replicates, which is what
  # lets the overall alpha and a pair's alpha be set against each other rather
  # than only reported separately.
  s <- three_label_sheets()
  r <- suppressMessages(ca(
    r1 = s$r1,
    r2 = s$r2,
    ci = "bootstrap",
    bootstrap_n = 200L
  ))

  expect_true("alpha_ci" %in% names(r$alpha))
  expect_equal(
    which(names(r$alpha) == "alpha_ci"),
    which(names(r$alpha) == "alpha") + 1L
  )
  for (m in c("agreement", "alpha")) {
    expect_true(paste0(m, "_ci") %in% names(r$pairs))
  }

  # The point estimate sits inside its own interval.
  b <- as.numeric(strsplit(
    gsub("\\[|\\]", "", r$alpha$alpha_ci[1]),
    ", "
  )[[1]])
  expect_lte(b[1], r$alpha$alpha[1])
  expect_gte(b[2], r$alpha$alpha[1])
})

test_that("ci = none leaves the ceiling's interval columns absent", {
  s <- three_label_sheets()
  r <- suppressMessages(ca(r1 = s$r1, r2 = s$r2))

  expect_false(any(grepl("_ci$", names(r$alpha))))
  expect_false(any(grepl("_ci$", names(r$pairs))))
})

test_that("$pairs reports one coefficient, not a kappa against an alpha", {
  # A kappa here and an alpha in $alpha cannot be set beside each other, which
  # is the comparison the pair table exists to support.
  sh <- three_label_sheets()
  r <- ca(r1 = sh$r1, r2 = sh$r2)

  expect_named(
    r$pairs,
    c("coder_a", "coder_b", "item", "n", "agreement", "alpha")
  )
  expect_false("kappa" %in% names(r$pairs))
  # Two coders and complete rows, so the pair and the whole set are the same
  # units and the two coefficients have to match.
  expect_equal(r$pairs$alpha, r$alpha$alpha)
})

test_that("$alpha is one row per question with the units it rests on", {
  sh <- three_label_sheets()
  r <- ca(r1 = sh$r1, r2 = sh$r2)

  expect_named(r$alpha, c("item", "n_units", "alpha"))
  expect_equal(round(r$alpha$alpha, 6), 0.661538)
})

test_that("the instrument supplies the scale so labels need not be retyped", {
  b <- instrument(
    "How strongly?",
    labels = c("A", "B", "C"),
    exclusive = TRUE,
    scores = c(A = 1, B = 2, C = 3)
  )
  sh <- three_label_sheets()

  from_inst <- ca(r1 = sh$r1, r2 = sh$r2, level = "ordinal", instrument = b)
  typed <- ca(
    r1 = sh$r1,
    r2 = sh$r2,
    level = "ordinal",
    labels = c("A", "B", "C")
  )
  expect_equal(from_inst$alpha$alpha, typed$alpha$alpha)

  # The more specific statement wins, so an explicit scale overrides the one on
  # the instrument rather than being silently ignored.
  #
  # Reversing a scale cannot show this. Krippendorff's ordinal difference is
  # the summed marginals spanned between two positions, and mirroring a
  # contiguous span leaves that sum unchanged, so C,B,A must return exactly
  # what A,B,C does. Moving which label sits in the middle is a real
  # reordering and does change the coefficient.
  middle <- ca(
    r1 = sh$r1,
    r2 = sh$r2,
    level = "ordinal",
    labels = c("A", "C", "B"),
    instrument = b
  )
  expect_equal(round(from_inst$alpha$alpha, 6), 0.562727)
  expect_equal(round(middle$alpha$alpha, 6), 0.89)

  # And the symmetry itself, so the reason above is checked rather than
  # asserted.
  flipped <- ca(
    r1 = sh$r1,
    r2 = sh$r2,
    level = "ordinal",
    labels = c("C", "B", "A")
  )
  expect_equal(flipped$alpha$alpha, from_inst$alpha$alpha)

  # And validate_sample() reads the same scale off the instrument, so a list
  # of sheets needs it said once.
  res <- abc_results()
  s1 <- sh$r1[1:6, ]
  s1$item <- "q"
  s1$human_label <- rep(c("A", "B", "C"), 2)
  s2 <- s1
  s2$human_label <- rep(c("A", "C", "B"), 2)
  via_inst <- suppressMessages(validate_sample(
    list(r1 = s1, r2 = s2),
    res,
    level = "ordinal",
    instrument = b,
    bootstrap_n = 200L
  ))
  via_labels <- suppressMessages(validate_sample(
    list(r1 = s1, r2 = s2),
    res,
    level = "ordinal",
    labels = c("A", "B", "C"),
    bootstrap_n = 200L
  ))
  expect_equal(via_inst$ceiling$alpha, via_labels$ceiling$alpha)
  expect_equal(via_inst$gap, via_labels$gap)
})

# --- seeds, coverage and set-aside rows --------------------------------------

test_that("the bootstrap reads its own seed and leaves the session's alone", {
  # The intervals once came off whatever the session's stream held, and
  # coding_sample() setting that stream made the main article's intervals
  # reproducible by accident.
  withr::local_preserve_seed()
  m <- many_case()
  sheets <- list(ana = m$ana, luis = m$luis)

  set.seed(1)
  a <- run_many(sheets, m$res, ci = "bootstrap")
  set.seed(2)
  b <- run_many(sheets, m$res, ci = "bootstrap")
  expect_identical(a$gap$gap_ci, b$gap$gap_ci)
  expect_identical(a$ceiling$alpha, b$ceiling$alpha)
  expect_identical(a$discrimination, b$discrimination)

  # The ceiling is the internal function called with the same seed.
  alone <- suppressMessages(ca(
    ana = m$ana,
    luis = m$luis,
    ci = "bootstrap",
    bootstrap_n = 200L
  ))
  expect_identical(a$ceiling$alpha, alone$alpha)

  # The session's stream comes back as it was.
  set.seed(3)
  expected <- stats::runif(2)
  set.seed(3)
  run_many(sheets, m$res, ci = "bootstrap")
  expect_identical(stats::runif(2), expected)
})

test_that("a seed given is the seed the bootstrap uses", {
  withr::local_preserve_seed()
  cs <- val_case()
  run <- function(...) {
    run_val(cs$sheet, cs$res, ci = "bootstrap", bootstrap_n = 200L, ...)
  }

  # A seed passed in and the same seed set by hand before a NULL agree, so the
  # argument is what drives the replicates.
  given <- run(seed = 7)
  set.seed(7)
  session <- run(seed = NULL)
  expect_identical(given$discrimination, session$discrimination)
  expect_identical(given$categories, session$categories)

  # A session with no stream yet is left without one.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  run()
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

  expect_error(
    validate_sample(cs$sheet, cs$res, seed = "seven"),
    "`seed` must be a single number",
    fixed = TRUE
  )
})

test_that("a coded row the model gave no answer for is set aside and said so", {
  # slice_max() on an all-zero `prob` named the first declared label as the
  # model's answer, so a coder who chose that label scored as agreement on a
  # document where the model answered outside the labels.
  toks <- c("ALPHA", "Res", "BETA", "ALPHA")
  i <- 0L
  local_offline(
    gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
      i <<- i + 1L
      stub_generate(tokens = toks[i], logprobs = -0.01)(prompt, backend)
    }
  )
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = paste("doc", 1:4))
  res <- quiet_measure(
    docs,
    stub_backend(),
    instrument(list(q = "Which?"), labels = c("ALPHA", "BETA", "GAMMA")),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  sheet <- tibble::tibble(
    id = sprintf("d%d", 1:4),
    item = "q",
    human_label = c("ALPHA", "ALPHA", "BETA", "ALPHA")
  )
  said <- character(0)
  v <- withCallingHandlers(
    suppressWarnings(validate_sample(sheet, res, coded = TRUE)),
    message = function(m) {
      said <<- c(said, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("1 coded row(s) set aside", said, fixed = TRUE)))
  expect_equal(nrow(v$coded), 3L)
  expect_false("d2" %in% v$coded$id)
})

test_that("conf_level sets the coverage of the intervals and says so", {
  # Every interval was fixed at 95 percent, and with no replicates returned a
  # caller had no way to get another coverage afterwards.
  withr::local_preserve_seed()
  f <- gap_frames()
  width <- function(x) {
    b <- as.numeric(strsplit(gsub("\\[|\\]", "", x$gap_ci[1]), ",\\s*")[[1]])
    b[2] - b[1]
  }
  wide <- suppressWarnings(run_gap(f$model, f$coders, bootstrap_n = 400L))
  narrow <- suppressWarnings(run_gap(
    f$model,
    f$coders,
    bootstrap_n = 400L,
    conf_level = 0.5
  ))
  expect_lt(width(narrow), width(wide))

  # A list of sheets hands the coverage to every call it makes, and the print
  # method says what the brackets cover.
  m <- many_case()
  v <- run_many(
    list(ana = m$ana, luis = m$luis),
    m$res,
    ci = "bootstrap",
    conf_level = 0.8
  )
  expect_identical(attr(v, "conf_level"), 0.8)
  out <- capture.output(print(v))
  expect_true(any(grepl("Intervals cover 80 percent.", out, fixed = TRUE)))
  # Said once, under the first table that carries an interval.
  expect_equal(sum(grepl("Intervals cover", out, fixed = TRUE)), 1L)
})

test_that("conf_level is refused outside zero to one", {
  cs <- val_case()
  expect_error(
    validate_sample(cs$sheet, cs$res, conf_level = 1.5),
    "`conf_level` must be a single number between 0 and 1",
    fixed = TRUE
  )
})

test_that("a sheet read from disk keeps its ids as written", {
  local_offline()
  ids <- c("1234567890123456789", "1234567890123456788")
  res <- quiet_measure(
    stats::setNames(c("one", "two"), ids),
    backend = stub_backend(),
    instrument = ab_instrument()
  )
  # Digits only and unquoted, the way a spreadsheet saves them, so a reader
  # guessing column types takes them for numbers and both round to one double.
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id,item,human_label", paste0(ids, ",a,", c("YES", "NO"))), f)
  v <- suppressMessages(validate_sample(f, res))
  expect_identical(v$discrimination$n, 2L)
  ref <- suppressMessages(coder_reference(ana = f, luis = f))
  expect_identical(sort(ref$id), sort(ids))
})
