# The four paths a run takes that the rest of the suite never reaches.
#
# The context ceiling, which decides whether long documents survive intact. The
# failure branch of a single call, which is the only reason a dead server marks
# rows for retry instead of ending a corpus run. Reading a checkpoint directory
# back, which is what makes one .rds per document worth the files. And the half
# of as_wide() that the vignette tables actually use.
#
# The stub answers every call the same way, so where a run has to fail the stub
# is replaced with one that raises instead. That is the only way `call_ok =
# FALSE` ever appears in this suite.

two_docs <- function() {
  tibble::tibble(id = c("p", "q"), body = c("one", "two"))
}

# A generator that dies the way a stopped server does.
stub_dead <- function(msg = "connection refused") {
  force(msg)
  function(prompt, backend, num_ctx = NULL, image = NULL) stop(msg)
}

# A generator that answers but returns nothing to score.
stub_no_logprobs <- function() {
  function(prompt, backend, num_ctx = NULL, image = NULL) {
    list(response = "YES", logprobs = list())
  }
}

# --- the context ceiling -----------------------------------------------------

test_that("auto measures the corpus and rounds up to a power of two", {
  # The longest document is sent once with no ceiling and its token count read
  # back, then 1.25 headroom and the next power of two. 3000 becomes 3750
  # becomes 4096. The headroom is there because the longest document by words is
  # not always the longest by tokens.
  local_offline(stub_generate(prompt_eval_count = 3000L))
  txt <- c("short one", paste(rep("word", 400), collapse = " "))

  expect_equal(
    suppressMessages(
      fathom:::resolve_num_ctx("auto", txt, ab_instrument(), stub_backend())
    ),
    4096L
  )
})

test_that("the floor decides when the measurement is small", {
  # 50 tokens rounds to 64, which is not a context window. The 2048 floor takes
  # over.
  local_offline(stub_generate(prompt_eval_count = 50L))
  txt <- c("short one", "short two")

  expect_equal(
    suppressMessages(
      fathom:::resolve_num_ctx("auto", txt, ab_instrument(), stub_backend())
    ),
    2048L
  )
})

test_that("the header gives the ceiling alone and the longest doc by id", {
  # The reason a ceiling was reached and the document's place in the corpus
  # both left the header. The Documents line already gives the total.
  local_offline(stub_generate(prompt_eval_count = 50L))
  pre <- new.env()
  suppressMessages(fathom:::resolve_num_ctx(
    "auto",
    c("short one", "a longer second document"),
    ab_instrument(),
    stub_backend(),
    ids = c("d1", "d2"),
    report = pre
  ))
  expect_identical(pre$context, "2,048 tokens")
  expect_identical(
    pre$measured_doc,
    list(`Longest doc` = "'d2', 4 words, 50 tokens")
  )
})

test_that("an explicit ceiling is used as given, and NULL means none", {
  local_offline()
  txt <- c("one", "two")
  b <- ab_instrument()
  bk <- stub_backend()

  expect_equal(
    suppressMessages(fathom:::resolve_num_ctx(8192, txt, b, bk)),
    8192L
  )
  expect_null(suppressMessages(fathom:::resolve_num_ctx(NULL, txt, b, bk)))
  expect_null(suppressMessages(fathom:::resolve_num_ctx(NA, txt, b, bk)))

  # A single document cannot be measured against itself without circularity.
  expect_null(suppressMessages(fathom:::resolve_num_ctx("auto", txt[1], b, bk)))
})

test_that("a ceiling too small to hold a prompt is refused", {
  local_offline()
  txt <- c("one", "two")

  expect_error(
    fathom:::resolve_num_ctx(500, txt, ab_instrument(), stub_backend()),
    "too small"
  )
  expect_error(
    fathom:::resolve_num_ctx("nope", txt, ab_instrument(), stub_backend()),
    "must be"
  )
})

test_that("a failed measurement proceeds without a ceiling rather than stopping", {
  # A corpus run should not end because one probe call did not come back. The
  # warning says to supply num_ctx explicitly, since the alternative is letting
  # Ollama choose.
  local_offline(stub_dead())
  txt <- c("one", "two")

  expect_warning(
    v <- fathom:::resolve_num_ctx("auto", txt, ab_instrument(), stub_backend()),
    "could not measure"
  )
  expect_null(v)
})

test_that("a resumed run keeps its own ceiling and ignores the caller", {
  # Mixing ceilings within one corpus means some documents were truncated and
  # others were not, with nothing in the results to tell them apart. The
  # checkpoint's ceiling wins and the override is announced rather than silent.
  ck <- withr::local_tempdir()
  local_offline()
  invisible(quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L,
    checkpoint = ck
  ))

  expect_equal(fathom:::ckpt_num_ctx(ck), 4096L)

  msgs <- testthat::capture_messages(
    v <- fathom:::resolve_num_ctx(
      16384L,
      c("a", "b"),
      ab_instrument(),
      stub_backend(),
      checkpoint = ck
    )
  )
  expect_equal(v, 4096L)
  expect_match(paste(msgs, collapse = "\n"), "existing checkpoints")

  # The default asks for no ceiling in particular, so deferring to the
  # checkpoints is not an override and gets no note. The header already says
  # where the ceiling came from.
  quiet_default <- testthat::capture_messages(
    a <- fathom:::resolve_num_ctx(
      "auto",
      c("a", "b"),
      ab_instrument(),
      stub_backend(),
      checkpoint = ck
    )
  )
  expect_equal(a, 4096L)
  expect_length(quiet_default, 0L)
})

# --- what happens when a call fails ------------------------------------------

test_that("a dead server marks rows for retry instead of ending the run", {
  # measure_one() never throws. The loop keeps going and the failure lands in
  # the frame, which is what lets a long corpus run survive an interruption and
  # be resumed by re-running.
  local_offline(stub_dead())
  res <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  expect_equal(nrow(res$probs), 8L)
  expect_true(all(!is.na(res$calls$error)))
  expect_match(res$calls$error[1], "connection refused")
  expect_true(all(is.na(res$probs$prob_renorm)))

  # The failure frame carries the same columns a scored one does. If it did not,
  # a corpus mixing a failure with a success would bind into a ragged frame
  # before new_results() ever splits it.
  expect_setequal(names(res$probs), fathom:::PROB_COLS)
  # Exact now, not a ceiling. measure() writes no column CALL_COLS does not
  # name, and this run carries nothing through `keep`.
  expect_setequal(names(res$calls), fathom:::CALL_COLS)
})

test_that("a reply with nothing to score fails the same way", {
  # A model that answers but returns no distribution is the Ollama version
  # problem, and the error names the version rather than reporting zeros.
  local_offline(stub_no_logprobs())
  res <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  expect_true(all(!is.na(res$calls$error)))
  expect_match(res$calls$error[1], "0.12.11")
})

test_that("re-running retries the failures and nothing else", {
  # There is no separate retry sweep. A failed item is stale on the next pass by
  # definition, so calling measure() again against the same directory does
  # exactly the outstanding work.
  ck <- withr::local_tempdir()
  local_offline(stub_dead())
  first <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  )
  expect_true(all(!is.na(first$calls$error)))

  local_offline()
  second <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  )
  expect_true(all(is.na(second$calls$error)))
})

test_that("the truncation flag is set only when there is a ceiling to hit", {
  # Within 64 tokens of the ceiling. llama.cpp keeps the tail, so with
  # document-first prompts it is the opening of the document that went missing.
  # With no ceiling truncation can be neither ruled out nor confirmed, so the
  # column is NA rather than FALSE.
  local_offline(stub_generate(prompt_eval_count = 4090L))
  near <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L
  )
  expect_true(all(near$calls$truncated))

  local_offline(stub_generate(prompt_eval_count = 100L))
  clear <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L
  )
  expect_false(any(clear$calls$truncated))

  none <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_true(all(is.na(none$calls$truncated)))
})

# --- reading a checkpoint directory back -------------------------------------

test_that("a checkpoint written before the call_ok rename still resumes", {
  # ckpt_stale() reads `r$call_ok`. On a payload carrying the old `ok`, that is
  # NULL, `NULL & r$model == model` is logical(0), and every item looks stale.
  # The corpus would re-extract against the model rather than resume.
  dir <- withr::local_tempdir()
  h <- fathom:::item_hashes(ab_instrument(), "m")
  old <- list(
    id = "d1",
    results = tibble::tibble(
      item = names(h),
      prompt_hash = unname(h),
      model = "m",
      ok = TRUE
    )
  )
  saveRDS(old, fathom:::ckpt_path("d1", dir))

  back <- fathom:::ckpt_read("d1", dir)
  expect_true("call_ok" %in% names(back$results))
  expect_false("ok" %in% names(back$results))
  expect_length(fathom:::ckpt_stale(back, h, "m"), 0L)
})

test_that("read_checkpoints restores provenance rather than recomputing it", {
  # A run written under one ceiling reports that ceiling, and the timestamp is
  # the run's rather than the read's. Otherwise reading a corpus back would
  # relabel it with the current session's configuration.
  ck <- withr::local_tempdir()
  local_offline()
  original <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L,
    checkpoint = ck
  )

  restored <- suppressMessages(read_checkpoints(ck))

  expect_s3_class(restored, "fathom_results")
  # Row counts per frame. A fathom_results is a list, so nrow() on the object
  # itself is NULL on both sides and compares nothing.
  expect_equal(nrow(restored$probs), nrow(original$probs))
  expect_equal(nrow(restored$calls), nrow(original$calls))

  # The documented contract is that this returns the same columns measure()
  # does, which is why the reference page cross-references rather than restates.
  expect_setequal(names(restored$probs), fathom:::PROB_COLS)
  expect_equal(attr(restored, "num_ctx"), 4096L)
  expect_false(isTRUE(attr(restored, "exclusive")))
  # `run_at` comes off the checkpoints rather than off the clock, and that is
  # what "restored, not recomputed" means. Comparing it to the returned
  # object's stamp asserted more than the contract and was a race, which it
  # eventually lost. ckpt_write() stamps each document as it finishes and
  # new_results() stamps the return value after the loop, so the restored
  # value is the earlier of two moments and the two print equal only while
  # both land in the same second.
  #
  # Against the files instead. That is the contract exactly, it cannot race,
  # and a read-time Sys.time() fails it.
  written <- vapply(
    list.files(ck, full.names = TRUE),
    function(f) readRDS(f)$run_at,
    character(1)
  )
  expect_equal(attr(restored, "run_at"), max(written))
  expect_true(attr(restored, "run_at") <= attr(original, "run_at"))
})

test_that("a truncated checkpoint reads as absent rather than as corrupt", {
  # A killed run can leave a half-written file. Treating it as not done
  # re-extracts that document, which is the only recoverable reading.
  ck <- withr::local_tempdir()
  local_offline()
  invisible(quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  ))
  writeLines("not an rds", list.files(ck, full.names = TRUE)[1])

  expect_null(fathom:::ckpt_read("p", ck))
  expect_error(read_checkpoints(withr::local_tempdir()), "no checkpoints")
})

test_that("rescaling an instrument rewrites checkpoints without a model call", {
  # `scores` sit outside the prompt hash on purpose, so a rescaled resume skips
  # every document. Without the rewrite the stored expected score would stay on
  # the old scale and read_checkpoints() would return a corpus under a scale
  # nobody is using.
  ck <- withr::local_tempdir()
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- two_docs()
  small <- instrument(
    list(a = "A?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  )
  big <- instrument(
    list(a = "A?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 10, hi = 100)
  )

  invisible(quiet_measure(
    docs,
    stub_backend(),
    small,
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  ))
  rescaled <- quiet_measure(
    docs,
    stub_backend(),
    big,
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  )

  pr <- exp(c(-0.1, -2, -3))
  pr <- pr / sum(pr)
  expect_equal(unique(rescaled$calls$expected_score), sum(pr * c(10, 0, 100)))
  # And the rewrite reached disk, so a later read agrees with the run.
  expect_equal(
    unique(suppressMessages(read_checkpoints(ck))$calls$expected_score),
    sum(pr * c(10, 0, 100))
  )
})

# --- the half of as_wide the vignettes use -----------------------------------

test_that("the expected score pivots one column per item, not per label", {
  # An expected score is one number per document and question, so this collapses
  # rather than spreading across labels the way a probability does.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- tibble::tibble(
    id = c("p", "q"),
    body = c("one", "two"),
    yr = c(2011, 2012)
  )
  b <- instrument(
    list(a = "A?", z = "Z?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL,
    keep = "yr"
  )
  w <- suppressMessages(as_wide(res, value = "expected_score", keep = "yr"))

  expect_equal(names(w), c("id", "yr", "a", "z"))
  expect_equal(nrow(w), 2L)

  pr <- exp(c(-0.1, -2, -3))
  pr <- pr / sum(pr)
  expect_equal(w$a[1], sum(pr * c(1, 0, 2)))
})

test_that("as_wide refuses the combinations that have no meaning", {
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  docs <- two_docs()
  scored <- instrument(
    list(a = "A?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    scored,
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  # An expected score is not a probability, so there is nothing to threshold.
  expect_error(
    suppressMessages(as_wide(
      res,
      value = "expected_score",
      dichotomize = TRUE
    )),
    "no meaning"
  )
  expect_error(suppressMessages(as_wide(res, target = "zzz")), "is not a label")

  # An instrument with no scores has no expected value to pivot.
  plain <- instrument(list(a = "A?"), labels = c("lo", "mid", "hi"))
  res2 <- quiet_measure(
    docs,
    stub_backend(),
    plain,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_error(
    suppressMessages(as_wide(res2, value = "expected_score")),
    "carry no expected score"
  )
})

test_that("dichotomize returns integers at the half mark", {
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  b <- instrument(list(a = "A?", z = "Z?"), labels = c("lo", "mid", "hi"))
  res <- quiet_measure(
    two_docs(),
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  w <- suppressMessages(as_wide(res, target = "mid", dichotomize = TRUE))

  expect_true(is.integer(w$a))
  expect_setequal(unique(c(w$a, w$z)), 1L)
})

test_that("as_wide warns about the records it is turning into NA", {
  # A failed record becomes NA in the matrix, and saying so is what separates a
  # gap in the data from a document the model scored low.
  local_offline(stub_dead())
  res <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  expect_warning(
    suppressMessages(as_wide(res, drop_incomplete = FALSE)),
    "failed record"
  )
})

test_that("no text means no document block, and no placeholder either", {
  b <- ab_instrument()
  with_text <- fathom:::build_prompt("a caption", "Is it red?", b)
  without <- fathom:::build_prompt("", "Is it red?", b)

  expect_true(startsWith(with_text, b$text_label))
  # Not an empty block, which asserts a document that is empty rather than one
  # that is elsewhere, and not a placeholder either.
  expect_false(grepl(b$text_label, without, fixed = TRUE))
  expect_false(grepl('""', without, fixed = TRUE))
  expect_false(grepl("[IMG]", without, fixed = TRUE))
  expect_true(startsWith(without, "Is it red?"))

  # The question and everything after it are untouched by the block going.
  tail <- substr(with_text, nchar(with_text) - 40L, nchar(with_text))
  expect_true(endsWith(without, tail))
})

test_that("the prompt shape is in the hash, so a resume cannot cross it", {
  # Same instrument, same model, two different prompts. Without the shape in
  # the key, a text run's checkpoints would read as current for an image run
  # and be resumed into.
  b <- ab_instrument()
  with_text <- fathom:::item_hashes(b, "m", TRUE)
  without <- fathom:::item_hashes(b, "m", FALSE)

  expect_equal(names(with_text), names(without))
  expect_false(any(with_text == without))
  # And the default is the text shape, so nothing already written moves.
  expect_equal(fathom:::item_hashes(b, "m"), with_text)
})

test_that("preview brackets what travels beside the prompt, never sending it", {
  b <- ab_instrument()
  f <- tempfile(fileext = ".png")
  writeBin(as.raw(rep(1, 2796)), f)

  out <- capture.output(p <- preview(b, image = f))
  txt <- paste(out, collapse = "\n")

  expect_true(grepl("[image: 2,796 bytes", txt, fixed = TRUE))
  expect_true(grepl("sent alongside the prompt", txt, fixed = TRUE))
  # The default placeholder document is gone, since naming a picture says the
  # corpus has no text.
  expect_false(grepl("document text goes here", txt, fixed = TRUE))
  expect_false(grepl(b$text_label, txt, fixed = TRUE))
  # The path itself never appears. A file name in the prompt would be the name
  # answering the question instead of the picture.
  expect_false(grepl(basename(f), txt, fixed = TRUE))
  # The returned string is what measure() sends, so the bracketed line, which
  # describes the picture rather than quoting the prompt, stays on the screen.
  expect_false(grepl("[image:", p, fixed = TRUE))
  expect_identical(p, fathom:::build_prompt("", b$items[[1]], b))

  # A caption plus a picture shows both, because both are sent.
  both <- paste(
    capture.output(preview(b, "a caption", image = f)),
    collapse = "\n"
  )
  expect_true(grepl("[image:", both, fixed = TRUE))
  expect_true(grepl("a caption", both, fixed = TRUE))

  # A path that does not resolve says so rather than printing NA bytes.
  gone <- paste(
    capture.output(preview(b, image = "no_such.png")),
    collapse = "\n"
  )
  expect_true(grepl("file not found", gone, fixed = TRUE))
})

test_that("auto sizes by the document with the most media, not the biggest file", {
  # A window of twelve frames costs twelve encodes, so the widest prompt is the
  # row with the most bytes in total rather than the one holding the largest
  # single picture.
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  gen <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    seen$n <- length(image %||% character(0))
    stub_generate()(prompt, backend, num_ctx)
  }
  local_offline(gen = gen)

  big <- tempfile(fileext = ".png")
  writeBin(as.raw(rep(1, 900)), big)
  small <- vapply(
    1:4,
    function(i) {
      p <- tempfile(fileext = ".png")
      writeBin(as.raw(rep(i, 400)), p)
      p
    },
    character(1)
  )

  # One document holds the largest single file. The other holds four smaller
  # ones totalling more. The second is the one to probe with.
  suppressMessages(fathom:::resolve_num_ctx(
    "auto",
    c("", ""),
    ab_instrument(),
    stub_backend(),
    images = list(big, small)
  ))
  expect_equal(seen$n, 4L)
})

test_that("preview brackets a set of frames as a set", {
  b <- ab_instrument()
  f <- vapply(
    1:3,
    function(i) {
      p <- tempfile(fileext = ".png")
      writeBin(as.raw(rep(i, 100)), p)
      p
    },
    character(1)
  )

  one <- paste(capture.output(preview(b, image = f[1])), collapse = "\n")
  expect_true(grepl("[image: 100 bytes", one, fixed = TRUE))

  many <- paste(capture.output(preview(b, image = f)), collapse = "\n")
  expect_true(grepl("[3 images: 300 bytes", many, fixed = TRUE))

  # A missing file among present ones is counted rather than swallowed.
  mixed <- paste(
    capture.output(preview(b, image = c(f[1], "no_such.png"))),
    collapse = "\n"
  )
  expect_true(grepl("1 not found", mixed, fixed = TRUE))
})

# --- what the returned object carries ----------------------------------------

test_that("a live run carries the ceiling it ran under", {
  # measure() set the attribute on the flat frame and new_results() built a new
  # object without it, so only read_checkpoints() ever returned one and the
  # header of a live result printed the ceiling as none.
  local_offline()
  fixed <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L
  )
  expect_identical(attr(fixed, "num_ctx"), 4096L)
  out <- capture.output(print(fixed))
  expect_true(any(grepl("^Context +4,096 tokens$", out)))

  none <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_identical(attr(none, "num_ctx"), NA_integer_)
  # Stored as NA, and printed as none rather than as a count of none.
  expect_true(any(grepl("^Context +none$", capture.output(print(none)))))
})

test_that("the printed failure count is one per failed call", {
  # Two documents and two items make four calls and eight label rows. The count
  # comes from `$calls`, so it needs no division by the number of labels.
  local_offline(gen = stub_dead())
  res <- quiet_measure(
    two_docs(),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_equal(sum(!is.na(res$calls$error)), 4L)
  out <- capture.output(print(res))
  expect_true(any(grepl("4 calls failed", out, fixed = TRUE)))
})

test_that("the character method refuses an argument it no longer has", {
  # `id_from` was documented as ignored and swallowed whatever it was given.
  # Ids come from the vector's names, so the name now reaches no_dots() like any
  # other misspelling.
  local_offline()
  expect_error(
    measure(
      c(a = "one", b = "two"),
      stub_backend(),
      ab_instrument(),
      id_from = "a",
      num_ctx = NULL
    ),
    "id_from",
    fixed = TRUE
  )
})

test_that("read_checkpoints() names the problem when no file in the directory reads", {
  ck <- withr::local_tempdir()
  writeLines("not a checkpoint", file.path(ck, "doc-0123abcd.rds"))
  expect_error(
    suppressMessages(read_checkpoints(ck)),
    "none\\s+of\\s+the\\s+1\\s+checkpoint\\s+file\\(s\\)"
  )
})

test_that("a resumed run carries the keep columns asked for now", {
  local_offline()
  ck <- withr::local_tempdir()
  d <- data.frame(
    id = c("a", "b"),
    txt = c("one", "two"),
    yr = c(1, 2),
    grp = c("x", "y")
  )
  run <- function(keep) {
    quiet_measure(
      d,
      backend = stub_backend(),
      instrument = ab_instrument(),
      text = "txt",
      id = "id",
      keep = keep,
      checkpoint = ck
    )
  }
  run("yr")
  # A corrected covariate and a column added on the resume, which skips every
  # document since nothing about the prompts changed.
  d$yr <- c(10, 20)
  res <- run(c("yr", "grp"))
  first <- res$calls[res$calls$item == "a", , drop = FALSE]
  expect_identical(first$yr, c(10, 20))
  expect_identical(first$grp, c("x", "y"))
})

test_that("a backend and instrument passed the other way round are named", {
  expect_error(
    measure("one", ab_instrument(), stub_backend()),
    "other\\s+way\\s+round"
  )
})

test_that("a failed call at verbose prints its line without a warning", {
  local_offline(gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
    stop("connection refused")
  })
  expect_no_warning(suppressMessages(measure(
    c(d1 = "one"),
    backend = stub_backend(),
    instrument = ab_instrument(),
    num_ctx = NULL,
    verbosity = "verbose"
  )))
})

test_that("a failed call always carries a reason", {
  # The results read a call as successful exactly when `error` is NA, so a
  # failure has to arrive with a reason even when its condition carries none.
  local_offline(
    gen = function(prompt, backend, num_ctx = NULL, image = NULL) stop("")
  )
  res <- quiet_measure(
    tibble::tibble(id = c("a", "b"), body = c("one", "two")),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_true(all(!is.na(res$calls$error)))
  expect_true(all(nzchar(trimws(res$calls$error))))
})

test_that("the results carry nothing derivable from what they keep", {
  # prob is prob_renorm * (1 - prob_other), present is prob_renorm above 0,
  # all_absent is prob_other == 1, call_ok is is.na(error), and top1_prob is
  # bounded by prob_other. All five stay in the checkpoints and leave here.
  local_offline()
  res <- quiet_measure(
    tibble::tibble(id = c("a", "b"), body = c("one", "two")),
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  expect_named(res$probs, c("id", "item", "label", "prob_renorm"))
  expect_named(
    res$calls,
    c(
      "id",
      "item",
      "error",
      "truncated",
      "top1_token",
      "prob_other",
      "margin",
      "expected_score",
      "prompt_hash"
    )
  )
})
