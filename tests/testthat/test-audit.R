# The offline stub answers every call identically, so a corpus scored through it
# has one probability and one margin throughout. Most of this file needs records
# that differ, and the margin is set on the results directly rather than coaxed
# out of a stub that cannot vary. That is the same move test-regressions.R makes
# when it needs an unscorable row.
#
# coding_sample() comes first and gets the most of it.

# Twenty documents, one item, and margins that are all different.
graded <- function(
  env = parent.frame(),
  margins = seq(0.02, 0.98, length.out = 20)
) {
  local_offline(env = env)
  docs <- tibble::tibble(id = sprintf("d%02d", 1:20), body = paste("doc", 1:20))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    items = "a"
  )
  # One margin per call now, where the flat frame wanted one per label row.
  res$calls$margin <- margins
  # `margin` and the probabilities have to agree. measure() derives one from
  # the other, so setting margin alone leaves a results object with a
  # contradiction in it, and any rule reading the probabilities sees a corpus
  # the margins say is varied and the probabilities say is flat. On two labels
  # the winner sits at (1 + margin) / 2.
  q <- rep((1 + margins) / 2, each = 2)
  win <- res$probs$label == "YES"
  res$probs$prob_renorm <- ifelse(win, q, 1 - q)
  res$probs$prob <- res$probs$prob_renorm
  list(res = res, docs = docs)
}

sample_quiet <- function(...) suppressMessages(coding_sample(...))

# --- coding_sample, the draw -------------------------------------------------

test_that("DECISION. coding_sample() reports its draw as a title over fields", {
  # The summary was a line of `|`-separated counts with one indented line per
  # question. Laid out on instruction the way measure() reports.
  g <- graded()
  msgs <- testthat::capture_messages(
    coding_sample(g$res, g$docs, text = "body", id = "id", n = 10, seed = 99)
  )
  out <- paste(msgs, collapse = "")
  expect_match(out, "^Blinded coding sample\n\n")
  expect_match(out, "Pairs         10 across 1 item\n", fixed = TRUE)
  expect_match(out, "Drawn         10 of 20\n", fixed = TRUE)
  expect_false(grepl("Design", out, fixed = TRUE))
  expect_match(out, "Tip      Pass the completed sheet", fixed = TRUE)
})

test_that("every pair has the same chance and the sheet carries no design", {
  # One draw, so nothing downstream has to be told which rule made the sheet
  # and no row carries a weight.
  g <- graded()
  s <- sample_quiet(g$res, g$docs, text = "body", id = "id", n = 10, seed = 99)

  expect_equal(nrow(s), 10L)
  expect_named(
    s,
    c(
      "id",
      "item",
      "question",
      "allowed",
      "human_label",
      "note",
      "text",
      "image"
    )
  )
  expect_null(attr(s, "drawn_n"))
  expect_null(attr(s, "design"))

  # The arguments that chose a tilted draw are gone, and a call still naming
  # one is refused rather than ignored.
  base <- list(g$res, g$docs, text = "body", id = "id", n = 10, seed = 99)
  expect_error(
    do.call(sample_quiet, c(base, list(design = "uncertainty"))),
    "Named:\\s+design"
  )
  expect_error(
    do.call(sample_quiet, c(base, list(uniform_share = 0.2))),
    "Named:\\s+uniform_share"
  )
})

test_that("the budget splits across questions by supply", {
  # Proportional to what each question holds, which is what makes the sheet
  # representative across questions as well as within one.
  local_offline()
  docs <- tibble::tibble(id = sprintf("d%02d", 1:20), body = paste("doc", 1:20))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  # Item z scored on half the corpus and item a on all of it, so the two
  # questions reach the allocation with different supply.
  thin <- res$probs$item == "z" & res$probs$id %in% sprintf("d%02d", 11:20)
  res$probs$call_ok[thin] <- FALSE

  s <- sample_quiet(res, docs, text = "body", id = "id", n = 15, seed = 99)

  # Thirty pairs, twenty on a and ten on z, so half of each.
  expect_equal(nrow(s), 15L)
  expect_equal(sum(s$item == "a"), 10L)
  expect_equal(sum(s$item == "z"), 5L)
})

test_that("the draw is fixed size and hits each row at its stated chance", {
  # Systematic, so fixed size on every call rather than in expectation, which a
  # Bernoulli draw would not give, and each row drawn at the chance it was
  # given.
  ch <- rep(0.4, 10)

  set.seed(99)
  reps <- 4000L
  hits <- integer(length(ch))
  sizes <- integer(reps)
  for (i in seq_len(reps)) {
    s <- fathom:::pps_draw(ch)
    hits[s] <- hits[s] + 1L
    sizes[i] <- length(s)
  }

  expect_true(all(sizes == 4L))
  # Four standard errors at a chance of 0.4 is about 0.031, so 0.05 catches a
  # draw that is systematically wrong without failing on the noise.
  expect_lt(max(abs(hits / reps - ch)), 0.05)
  expect_equal(length(fathom:::pps_draw(numeric(0))), 0L)
})

test_that("the model's answer is withheld from the coder", {
  # A coder shown the model's label agrees with it more often, and the
  # reliability figure then measures the anchoring rather than the instrument.
  g <- graded()
  s <- sample_quiet(g$res, g$docs, text = "body", id = "id", n = 6, seed = 99)

  expect_false(any(
    c("prob", "prob_renorm", "label", "margin") %in% names(s)
  ))
  expect_true(all(is.na(s$human_label)))

  # The answer scheme travels with the sheet, so three coders cannot return
  # "yes", "Y", and "1".
  expect_equal(unique(s$allowed), "YES | NO")
  expect_true(all(nzchar(s$question)))
})

test_that("a record with no label at position 1 is excluded, not sampled", {
  # There is nothing for a human to agree or disagree with on a row the model
  # never answered, so it leaves the frame before anything is drawn.
  g <- graded()
  g$res$calls$margin[g$res$calls$id %in% c("d01", "d02")] <- NA_real_
  s <- sample_quiet(g$res, g$docs, text = "body", id = "id", n = 18, seed = 99)

  expect_false(any(c("d01", "d02") %in% s$id))
  expect_equal(nrow(s), 18L)
})

test_that("a budget an item cannot fill passes to the others", {
  # The split never asks an item for more than it holds, and when the rule
  # would, the excess goes to the items that can still take it. Twenty-five
  # asked of an item holding twenty.
  expect_equal(
    fathom:::allocate_budget(c(25, 5), c(20L, 20L), 30L),
    c(20L, 10L)
  )

  # Asking for the whole corpus is a census.
  g <- graded()
  full <- sample_quiet(
    g$res,
    g$docs,
    text = "body",
    id = "id",
    n = 20,
    seed = 99
  )
  expect_equal(nrow(full), 20L)
  expect_setequal(full$id, g$docs$id)
})

test_that("the sheet is a sheet, not results wearing the class", {
  # transmute() preserves the fathom_results class while dropping the columns
  # its print method reads, so the class is stripped rather than left to print a
  # sheet as a results object.
  g <- graded()
  s <- sample_quiet(g$res, g$docs, text = "body", id = "id", n = 6, seed = 99)

  expect_false(inherits(s, "fathom_results"))
  expect_s3_class(s, "tbl_df")
})

test_that("a filled sheet goes back through validate_sample", {
  # The two halves of the pipeline agreeing about the rows. The sheet comes
  # back with half of it coded against the model, and the figures read off
  # exactly those rows.
  g <- graded()
  s <- sample_quiet(g$res, g$docs, text = "body", id = "id", n = 10, seed = 99)
  s$human_label <- rep(c("YES", "NO"), length.out = nrow(s))

  v <- suppressMessages(suppressWarnings(
    validate_sample(s, g$res, coded = TRUE)
  ))
  disc <- v$discrimination

  # One question, so one row, and no basis to choose between.
  expect_equal(nrow(disc), 1L)
  expect_false("basis" %in% names(disc))
  expect_equal(disc$n, 10L)
  expect_equal(disc$agreement, mean(v$coded$correct))
})

# --- item_report and its two siblings ----------------------------------------

test_that("items report in instrument order rather than alphabetically", {
  # summarise() returns its groups sorted, so an instrument declaring z before a
  # came back reshuffled. An instrument is a questionnaire and reads in the
  # order it was written.
  local_offline()
  b <- instrument(list(z = "Z?", a = "A?"), labels = c("YES", "NO"))
  docs <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  expect_equal(suppressMessages(item_report(res))$item, c("z", "a"))
})

test_that("the binary report describes each item's behavior", {
  local_offline()
  docs <- tibble::tibble(id = sprintf("d%02d", 1:10), body = rep("doc", 10))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  rep <- suppressMessages(item_report(res))

  expect_setequal(rep$item, c("a", "z"))
  expect_true(all(rep$n == 10L))
  # The stub answers YES every time, so every document endorses.
  expect_true(all(rep$endorse == 1))
  expect_true(all(rep$n_endorse == 10L))
  expect_true(all(rep$pct_both_absent == 0))
  expect_true(all(rep$n_failed == 0))
})

test_that("off-label is judged by the rule scoring uses", {
  # Under prefix matching BU is scored as BUDGET, so counting it as off-label
  # would report every correct answer on a split label as a failure. The token
  # is not in the declared variants, which is what a membership test would ask.
  local_offline(stub_generate(c("BU", "HO"), c(-0.05, -3.1)))
  docs <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    split_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  expect_equal(suppressMessages(item_report(res))$n_off_label, 0L)
})

test_that("more than two labels reports the winner distribution instead", {
  # There is no single-label endorsement rate for a forced choice, since the
  # labels share one softmax and a rate for whichever sorted first is not a
  # quantity.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  b <- instrument(list(a = "Q?"), labels = c("lo", "mid", "hi"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  rep <- suppressMessages(item_report(res))

  expect_true(all(c("n_won", "share_won") %in% names(rep)))
  expect_false("endorse" %in% names(rep))
  expect_equal(rep$n_won[rep$label == "mid"], 4L)
  expect_equal(rep$share_won[rep$label == "mid"], 1)
  # A label that is present but never dominant shows zero rather than dropping.
  expect_equal(sum(rep$n_won), 4L)
})

test_that("endorsed is the 0.5 cut on two labels and the winner above two", {
  # The two rules answer the same question at different label counts. On two
  # labels a winner is whatever clears 0.5, which is what item_report() counts
  # as endorse_renorm. Above two nothing need clear it, so the question is
  # which label took the most mass, which is what it counts as n_won.
  #
  # The stub answers "mid" on every document, so mid wins all four and lo and
  # hi win none. This also used to raise a warning about the 0.5 cut, and no
  # longer does, since there is no cut above two labels.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  b <- instrument(list(a = "Q?"), labels = c("lo", "mid", "hi"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  hit <- NULL
  expect_no_warning(
    invisible(capture.output(
      hit <- suppressMessages(inspect_cases(
        res,
        "a",
        docs,
        text = "body",
        id = "id",
        target = "mid",
        n = 4L
      ))
    ))
  )
  expect_equal(nrow(hit), 4L)

  # A label that never wins returns nothing, rather than everything that
  # happened to clear a cut it could not reach.
  invisible(capture.output(
    miss <- suppressMessages(inspect_cases(
      res,
      "a",
      docs,
      text = "body",
      id = "id",
      target = "lo"
    ))
  ))
  expect_equal(nrow(miss), 0L)

  # The value that used to exist is refused rather than quietly ignored.
  expect_error(
    inspect_cases(res, "a", docs, text = "body", id = "id", which = "not"),
    "should be one of"
  )
})

test_that("two labels report as endorsement even when the instrument scores", {
  # On two labels the scored shape reports one number and drops a dozen
  # diagnostics to do it. The label count is tested before the scores, so the
  # number arrives as a column and the diagnostics stay.
  local_offline(stub_generate(c("YES", "NO"), c(-0.2, -1.6)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  b <- instrument(
    list(a = "Q?"),
    labels = c("YES", "NO"),
    scores = c(YES = 1, NO = 0)
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  rep <- suppressMessages(item_report(res))

  # One row per item, not one per label, and the diagnostics survive.
  expect_equal(nrow(rep), 1L)
  expect_true(all(c("endorse", "mean_prob", "max_prob_other") %in% names(rep)))
  expect_false("n_won" %in% names(rep))

  # The score reaches it as a column instead of as a shape. With YES = 1 and
  # NO = 0 the expected score is prob_renorm on YES, so this pins what the
  # column holds rather than that it is present.
  expect_true("mean_expected" %in% names(rep))
  pr <- res$probs
  expect_equal(
    rep$mean_expected,
    mean(pr$prob_renorm[pr$label == "YES"]),
    tolerance = 1e-8
  )
  # And it is not mean_prob, which is the raw probability. The stub's two
  # labels sum above 1, so renormalizing moves the number.
  expect_false(isTRUE(all.equal(rep$mean_expected, rep$mean_prob)))
})

test_that("an unscored two-label instrument carries no mean_expected", {
  local_offline(stub_generate(c("YES", "NO"), c(-0.2, -1.6)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  rep <- suppressMessages(item_report(res))
  expect_false("mean_expected" %in% names(rep))
})

test_that("labels come back declared by default and by size on request", {
  # Everything else in the package is already in declaration order, since
  # score_labels() builds its rows from names(instrument$labels), so $probs,
  # preview() and as_wide() all agree. A report sorting by frequency made
  # item_report() the one place that disagreed.
  #
  # The stub always answers "mid", so the two orders cannot coincide here.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  b <- instrument(list(a = "Q?"), labels = c("lo", "mid", "hi"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  dec <- suppressMessages(item_report(res))
  expect_equal(dec$label, c("lo", "mid", "hi"))
  expect_equal(dec$n_won, c(0L, 4L, 0L))

  siz <- suppressMessages(item_report(res, order = "size"))
  expect_equal(siz$label[1], "mid")
  expect_equal(siz$n_won[1], 4L)

  expect_error(item_report(res, order = "frequency"), "should be one of")
})

test_that("scored labels report the expected score, in score order", {
  # Each question's labels read up the scale, and mean_expected is constant
  # within a question the way prob_other is on the results.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  b <- instrument(
    list(a = "Q?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  rep <- suppressMessages(item_report(res))

  expect_equal(rep$score, c(0, 1, 2))
  expect_equal(rep$label, c("lo", "mid", "hi"))
  expect_equal(length(unique(rep$mean_expected)), 1L)
})

test_that("DECISION. item_report() heads its report with aligned fields", {
  # The header was one line of `|`-separated counts with the statistics
  # indented under it. Laid out on instruction as a title over aligned fields,
  # the way measure() reports.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- tibble::tibble(id = sprintf("d%d", 1:4), body = rep("doc", 4))
  head_of <- function(b) {
    res <- quiet_measure(
      docs,
      stub_backend(),
      b,
      text = "body",
      id = "id",
      num_ctx = NULL
    )
    paste(testthat::capture_messages(item_report(res)), collapse = "")
  }

  choice <- head_of(instrument(list(a = "Q?"), labels = c("lo", "mid", "hi")))
  expect_match(choice, "^Choice instrument\n\n")
  expect_match(choice, "Documents     4\n", fixed = TRUE)
  expect_match(choice, "Items         1\n", fixed = TRUE)
  expect_match(choice, "Labels        3\n", fixed = TRUE)
  expect_match(choice, "Leaked mass   max ", fixed = TRUE)
  expect_match(choice, "Winning       probability min ", fixed = TRUE)

  scored <- head_of(instrument(
    list(a = "Q?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  ))
  expect_match(scored, "^Scored instrument\n\n")
  expect_match(scored, "Score         expected mean ", fixed = TRUE)
  expect_match(scored, "Entropy       mean ", fixed = TRUE)
  expect_match(scored, "Leaked mass   max ", fixed = TRUE)
})

# --- the rest of the audit surface -------------------------------------------

test_that("threshold sensitivity separates the corpus from the cut point", {
  # A rate that barely moves means 0.5 is as good a cut as any. A rate that
  # swings means the endorsement figure reported depends on a choice rather than
  # on the documents, which only a probability can show.
  g <- graded()
  res <- g$res
  spread <- seq(0.05, 0.95, length.out = 20)
  # Both columns, so the sweep is asserted on the default and on the alternative
  # rather than on whichever one happens to be the default today.
  res$probs$prob[res$probs$label == "YES"] <- rep(spread)
  res$probs$prob_renorm[res$probs$label == "YES"] <- rep(spread)
  ts <- suppressMessages(threshold_sensitivity(
    res,
    thresholds = c(0.1, 0.5, 0.9)
  ))

  expect_equal(nrow(ts), 3L)
  expect_equal(ts$threshold, c(0.1, 0.5, 0.9))
  expect_true(all(diff(ts$endorse) <= 0))
  expect_equal(ts$n_endorse[ts$threshold == 0.1], sum(spread >= 0.1))

  # The default is prob_renorm. `value` reaches the raw column, and on this
  # frame the two are set alike, so the sweep agrees.
  raw <- suppressMessages(
    threshold_sensitivity(res, thresholds = c(0.1, 0.5, 0.9), value = "prob")
  )
  expect_equal(raw$n_endorse, ts$n_endorse)
})

test_that("coding_sample copies media beside the CSV and relativizes paths", {
  local_offline()
  img <- vapply(
    1:4,
    function(i) {
      p <- tempfile(fileext = ".png")
      writeBin(as.raw(rep(i, 10)), p)
      p
    },
    character(1)
  )
  docs <- tibble::tibble(
    id = sprintf("d%02d", 1:4),
    body = paste("caption", 1:4),
    pic = img
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  csv <- file.path(tempdir(), "sheet.csv")
  unlink(file.path(tempdir(), "sheet_media"), recursive = TRUE)
  sheet <- suppressMessages(coding_sample(
    res,
    docs,
    text = "body",
    id = "id",
    image = "pic",
    n = 4L,
    file = csv
  ))

  # Relative to the sheet, so the folder and the CSV travel together.
  expect_false(any(grepl("^/", sheet$image)))
  expect_true(all(grepl("^sheet_media/", sheet$image)))
  expect_true(all(file.exists(file.path(tempdir(), sheet$image))))

  # copy_media = FALSE leaves what was there.
  unlink(csv)
  kept <- suppressMessages(coding_sample(
    res,
    docs,
    text = "body",
    id = "id",
    image = "pic",
    n = 4L,
    file = csv,
    copy_media = FALSE
  ))
  expect_true(all(kept$image %in% img))
})

test_that("coding_sample carries an image column either way", {
  local_offline()
  docs <- tibble::tibble(id = c("a", "b"), body = c("one", "two"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  # Absent `image` still yields the column, so two sheets have one shape.
  sheet <- suppressMessages(
    coding_sample(res, docs, text = "body", id = "id", n = 2L)
  )
  expect_true("image" %in% names(sheet))
  expect_true(all(is.na(sheet$image)))

  expect_error(
    suppressMessages(
      coding_sample(res, docs, text = "body", id = "id", image = "nope", n = 2L)
    ),
    "must name a column in `docs`"
  )
})

test_that("inspect_cases splits on the answer and says when it cannot", {
  local_offline()
  docs <- tibble::tibble(id = c("p", "q"), body = c("alpha doc", "beta doc"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  out <- capture.output(
    sel <- suppressMessages(inspect_cases(
      res,
      "a",
      docs,
      text = "body",
      id = "id"
    ))
  )
  expect_equal(nrow(sel), 2L)
  expect_true(any(grepl("alpha doc", out, fixed = TRUE)))

  # Every document sits at the same probability, so there is no uncertainty to
  # rank, and saying so beats returning corpus order under that label.
  expect_message(
    capture.output(
      inspect_cases(
        res,
        "a",
        docs,
        text = "body",
        id = "id",
        which = "uncertain"
      )
    ),
    "No variation in probability"
  )
})

test_that("audit functions ask for the documents when they were not stashed", {
  # A character vector stashes its documents on the results. A data frame does
  # not, since the long format would repeat every document once per item-label
  # pair.
  # The error names the argument rather than failing inside a join.
  local_offline()
  docs <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  from_df <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_error(inspect_cases(from_df, "a"), "`docs` is required")

  from_chr <- quiet_measure(
    c(p = "one", q = "two"),
    stub_backend(),
    ab_instrument(),
    num_ctx = NULL
  )
  expect_no_error(suppressMessages(
    capture.output(inspect_cases(from_chr, "a"))
  ))
})

test_that("as_docs namespaces the text column it returns", {
  # The audit functions join this onto results. A results column named `text`,
  # carried through keep, would otherwise suffix both sides of the join and
  # every read would come back NULL.
  tx <- fathom:::as_docs(c(p = "one", q = "two"))

  expect_equal(names(tx), c("id", ".fathom_text"))
  expect_equal(tx$id, c("p", "q"))
  expect_error(
    fathom:::as_docs(tibble::tibble(a = 1)),
    "must name the text column"
  )
  expect_error(fathom:::as_docs(42), "character vector or a data frame")
})

test_that("coding_sample keeps every frame of a video window", {
  # prepare_video() returns `image` as a list column. as.character() pasted each
  # window into one string that named no file, so the frames never reached the
  # sheet and copy_media found nothing to copy.
  local_offline()
  frames <- lapply(1:2, function(i) {
    vapply(
      1:3,
      function(j) {
        p <- tempfile(fileext = ".jpg")
        writeBin(as.raw(rep(10L * i + j, 10)), p)
        p
      },
      character(1)
    )
  })
  docs <- tibble::tibble(id = c("v1", "v2"), body = c("one", "two"))
  docs$frames <- frames
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  kept <- suppressMessages(coding_sample(
    res,
    docs,
    text = "body",
    id = "id",
    image = "frames",
    n = 4L
  ))
  paths <- strsplit(kept$image, "|", fixed = TRUE)
  expect_true(all(lengths(paths) == 3L))
  expect_true(all(unlist(paths) %in% unlist(frames)))

  csv <- file.path(withr::local_tempdir(), "frames.csv")
  sheet <- suppressMessages(coding_sample(
    res,
    docs,
    text = "body",
    id = "id",
    image = "frames",
    n = 4L,
    file = csv
  ))
  copied <- unlist(strsplit(sheet$image, "|", fixed = TRUE))
  # Two documents, two items and three frames each.
  expect_length(copied, 12L)
  expect_true(all(grepl("^frames_media/", copied)))
  expect_true(all(file.exists(file.path(dirname(csv), copied))))
})

test_that("inspect_cases prints the probability it selected on", {
  # Both branches select on prob_renorm and the header printed prob, so on a run
  # that leaked the printed figure did not explain why a case was chosen.
  local_offline(
    gen = stub_generate(
      tokens = c("YES", "Res", "NO"),
      logprobs = log(c(0.5, 0.3, 0.2))
    )
  )
  docs <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    items = "a",
    num_ctx = NULL
  )
  out <- capture.output(
    sel <- suppressMessages(
      inspect_cases(res, "a", docs, text = "body", id = "id")
    )
  )
  # 0.5 against everything the model could write, 0.5 / 0.7 over the labels.
  expect_true(any(grepl("P(YES) = 0.7143", out, fixed = TRUE)))
  expect_false(any(grepl("P(YES) = 0.5000", out, fixed = TRUE)))
  expect_false(any(grepl("\u2014", out, fixed = TRUE)))
  expect_true(all(c("prob_renorm", "prob") %in% names(sel)))
})

test_that("threshold_sensitivity divides the way it says it does", {
  # A document where no label appeared has no prob_renorm, so it leaves the
  # denominator of `endorse` under the default and stays in it under `prob`,
  # the same split item_report() makes between endorse_renorm and endorse.
  toks <- c("YES", "Res", "NO", "YES")
  i <- 0L
  local_offline(
    gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
      i <<- i + 1L
      stub_generate(tokens = toks[i], logprobs = -0.01)(prompt, backend)
    }
  )
  docs <- tibble::tibble(
    id = c("a", "b", "c", "d"),
    body = c("1", "2", "3", "4")
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    items = "a",
    num_ctx = NULL
  )

  renorm <- suppressMessages(threshold_sensitivity(res, thresholds = 0.5))
  expect_equal(renorm$n, 4L)
  expect_equal(renorm$n_endorse, 2L)
  expect_equal(renorm$endorse, 2 / 3)

  raw <- suppressMessages(
    threshold_sensitivity(res, thresholds = 0.5, value = "prob")
  )
  expect_equal(raw$n, 4L)
  expect_equal(raw$endorse, raw$n_endorse / raw$n)
})

test_that("a seed reaches its own draw and nothing after it", {
  # coding_sample() called set.seed() on the session's stream, so its seed
  # decided every draw the session made afterwards, including the bootstrap of
  # whatever validate_sample() call came next.
  withr::local_preserve_seed()
  g <- graded()
  set.seed(42)
  expected <- stats::runif(3)

  set.seed(42)
  sheet <- sample_quiet(
    g$res,
    g$docs,
    text = "body",
    id = "id",
    n = 10,
    seed = 99
  )
  expect_identical(stats::runif(3), expected)
  again <- sample_quiet(
    g$res,
    g$docs,
    text = "body",
    id = "id",
    n = 10,
    seed = 99
  )
  expect_identical(again$id, sheet$id)

  set.seed(42)
  out <- capture.output(suppressMessages(inspect_cases(
    g$res,
    "a",
    g$docs,
    text = "body",
    id = "id",
    n = 2L,
    seed = 5
  )))
  expect_identical(stats::runif(3), expected)
})

# A run where each call answers with the next token in turn, so a token that is
# none of the labels gives a call whose distribution holds no label at all.
outside_run <- function(toks, labels, env = parent.frame()) {
  i <- 0L
  local_offline(
    gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
      i <<- i + 1L
      stub_generate(tokens = toks[i], logprobs = -0.01)(prompt, backend)
    },
    env = env
  )
  docs <- tibble::tibble(
    id = sprintf("d%d", seq_along(toks)),
    body = vapply(
      seq_along(toks),
      function(k) paste(rep("w", k), collapse = " "),
      character(1)
    )
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    instrument(list(q = "Which?"), labels = labels),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  list(res = res, docs = docs)
}

test_that("a document that answered outside the labels wins nothing", {
  # slice_max() on an all-zero `prob` handed the win to the first declared
  # label, so item_report() counted the second document for ALPHA and
  # inspect_cases() showed it as endorsing ALPHA.
  o <- outside_run(
    c("ALPHA", "Res", "BETA", "ALPHA"),
    c("ALPHA", "BETA", "GAMMA")
  )
  rep <- suppressMessages(item_report(o$res))
  expect_equal(rep$n_won[rep$label == "ALPHA"], 2L)
  # Over the four documents scored, so the shares fall short of 1 by the one
  # that answered outside the labels.
  expect_equal(sum(rep$share_won), 0.75)

  out <- capture.output(
    sel <- suppressMessages(inspect_cases(
      o$res,
      "q",
      o$docs,
      text = "body",
      id = "id"
    ))
  )
  expect_setequal(sel$id, c("d1", "d4"))
})

test_that("two labels and no label in the distribution print no empty case", {
  # The bare comparison put NA into a logical index, which returns a row of NA,
  # and inspect_cases() printed a case for a document called NA.
  o <- outside_run(c("YES", "Res", "NO", "YES"), c("YES", "NO"))
  out <- capture.output(
    sel <- suppressMessages(inspect_cases(
      o$res,
      "q",
      o$docs,
      text = "body",
      id = "id"
    ))
  )
  expect_setequal(sel$id, c("d1", "d4"))
  expect_false(any(grepl("**NA**", out, fixed = TRUE)))
})

test_that("share_won is over the documents scored on that question", {
  # It divided by every document scored on any question, so a question whose
  # call failed on one document could not reach a share of 1.
  calls <- 0L
  local_offline(
    gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
      calls <<- calls + 1L
      if (calls == 4L) {
        stop("connection refused")
      }
      stub_generate(tokens = "ALPHA", logprobs = -0.01)(prompt, backend)
    }
  )
  b <- instrument(
    list(q1 = "First?", q2 = "Second?"),
    labels = c("ALPHA", "BETA", "GAMMA")
  )
  res <- quiet_measure(
    tibble::tibble(id = c("d1", "d2"), body = c("one", "two")),
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  rep <- suppressMessages(item_report(res))
  won <- rep[rep$label == "ALPHA", ]
  expect_equal(won$share_won, c(1, 1))
})

test_that("a target that is not a label is refused rather than read as empty", {
  local_offline()
  res <- quiet_measure(
    c(d1 = "one", d2 = "two"),
    backend = stub_backend(),
    instrument = ab_instrument()
  )
  expect_error(item_report(res, target = "yes"), "not\\s+a\\s+label")
  expect_error(
    suppressMessages(threshold_sensitivity(res, target = "yes")),
    "not\\s+a\\s+label"
  )
  expect_error(
    suppressMessages(inspect_cases(res, "a", target = "yes")),
    "not\\s+a\\s+label"
  )
})
