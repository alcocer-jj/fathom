test_that("instrument accepts the simple shapes", {
  b <- instrument("Is this about weather?", labels = c("YES", "NO"))
  expect_s3_class(b, "fathom_instrument")
  expect_equal(names(b$items), "Q1")
  expect_equal(b$k, 2L)

  b2 <- instrument(c(a = "First?", b = "Second?"))
  expect_equal(names(b2$items), c("a", "b"))
  expect_equal(b2$n_items, 2L)
})

test_that("the documented field set matches the object instrument() builds", {
  # instrument.R. Its @return names all sixteen fields and marks two of them as
  # implementation rather than interface. That makes the block a contract, and
  # R CMD check does not compare documentation against code, so a field added
  # or renamed would leave the reference page quietly wrong.
  b <- instrument(
    list(a = "A?", z = "Z?"),
    labels = c("YES", "NO"),
    scores = c(YES = 1, NO = 0)
  )

  documented <- c(
    # the instrument
    "items",
    "labels",
    "normalized",
    "definitions",
    "scores",
    # how it gets asked
    "cue",
    "text_label",
    "answer_label",
    "lang",
    # shape and mode
    "exclusive",
    "exact_match",
    "k",
    "n_items",
    "name",
    # named as implementation rather than interface
    "probe_texts",
    "firsts"
  )

  expect_setequal(names(b), documented)
  expect_equal(anyDuplicated(documented), 0L)

  # An exclusive instrument carrying definitions has the same fields, so the
  # set does not depend on how the instrument was built.
  ex <- instrument(
    "best",
    labels = c("alpha", "beta"),
    definitions = c(alpha = "the first", beta = "the second"),
    exclusive = TRUE,
    medium = "text"
  )
  expect_setequal(names(ex), documented)
})

test_that("instrument rejects malformed input", {
  expect_error(instrument(list()), "named list")
  expect_error(instrument(list(a = "Q?", a = "Q?")), "unique")
  expect_error(instrument(list(a = "")), "non-empty")
  expect_error(instrument(list(a = "Q?"), labels = "YES"), "at least two")
  expect_error(
    instrument(list(a = "Q?"), labels = c("SI", "si")),
    "normalize onto the same string"
  )
  expect_error(
    instrument(list(a = "Q?"), labels = list(A = "SI", B = "NO")),
    "not a list"
  )
  expect_error(
    instrument(list(a = "Q?"), labels = c(SI = "S\u00cd", NO = "NO")),
    "names do not match"
  )
})

test_that("a label carrying whitespace is refused", {
  # instrument.R. Documented since the first-token section was written and never
  # enforced. Scoring reads one token and the cue asks for exactly one word, so
  # a two-word label is a promise the prompt cannot keep.
  #
  # It does not fail loudly on its own. NOT is a prefix of NOT SURE, so under
  # either matching rule the label scores P(NOT), summed over NOTHING and every
  # other continuation, and no column separates that from the label's own mass.
  expect_error(
    instrument(list(q = "Q?"), labels = c("YES", "NOT SURE")),
    "must not contain whitespace"
  )
  # The message names the offender rather than restating the rule alone.
  expect_error(
    instrument(list(q = "Q?"), labels = c("YES", "NOT SURE")),
    "NOT SURE"
  )

  # Leading and trailing whitespace goes the same way rather than being
  # trimmed. trimws() runs on the matching surface and not on the label name,
  # so a trimmed label matches as one string and prints into the cue as another.
  expect_error(
    instrument(list(q = "Q?"), labels = c(" YES", "NO")),
    "must not contain whitespace"
  )

  # An exclusive instrument reaches the same check, since its categories become
  # answer tokens and flow into the same block.
  expect_error(
    instrument(
      "best",
      labels = c("Budget policy", "Housing"),
      exclusive = TRUE,
      medium = "text"
    ),
    "must not contain whitespace"
  )

  # Ahead of the first-character check, so a label that fails both is told
  # about the one it can act on. Budget policy and Housing differ at B and H,
  # which is what makes the case above test the order rather than the rule.
  expect_error(
    instrument(list(q = "Q?"), labels = c("NO WAY", "NOT SURE")),
    "must not contain whitespace"
  )
})

test_that("a whitespace-free label builds however it is spelled", {
  # The rule is whitespace, not word count. Joining keeps the answer one word
  # for the cue and leaves any emitted fragment a prefix of the whole label
  # rather than of one word out of two, which is the property scoring rests on.
  expect_s3_class(
    instrument(list(q = "Q?"), labels = c("NOT-SURE", "YES")),
    "fathom_instrument"
  )
  expect_s3_class(
    instrument(list(q = "Q?"), labels = c("NOT_SURE", "YES")),
    "fathom_instrument"
  )
  # parlasent's own six-level scheme, which is where the underscored form is
  # not hypothetical.
  expect_s3_class(
    instrument(list(q = "Q?"), labels = c("M_Positive", "P_Neutral")),
    "fathom_instrument"
  )
  # A script that does not space its words is several tokens and no whitespace,
  # which is the same position a split PRESUPUESTO is in.
  expect_s3_class(
    instrument(list(q = "Q?"), labels = c("\u591a", "\u5c11")),
    "fathom_instrument"
  )
})

test_that("the whitespace check is Unicode-aware rather than ASCII-aware", {
  # Through stringi for the reason the case folding is. Base `[[:space:]]`
  # misses U+00A0 under both regex engines, so a non-breaking space would pass
  # a base check and then fail at scoring with nothing raised. The pair below
  # differs at N and Y, so the first-character check cannot be what fires.
  expect_error(
    instrument(list(q = "Q?"), labels = c("NO\u00a0BREAK", "YES")),
    "must not contain whitespace"
  )
})

test_that("cue is generated when absent and kept when supplied", {
  b <- instrument(list(a = "Q?"), labels = c("YES", "NO"))
  expect_match(b$cue, "YES or NO")

  b2 <- instrument(list(a = "Q?"), cue = "\n\nSay SI or NO.")
  expect_equal(b2$cue, "\n\nSay SI or NO.")
})

test_that("labels normalize for matching but originals are kept", {
  b <- instrument(list(a = "Q?"), labels = c("s\u00ed", "no"))
  expect_equal(
    b$normalized[["s\u00ed"]],
    stringi::stri_trans_toupper("s\u00ed")
  )
  expect_equal(b$labels[["s\u00ed"]], "s\u00ed")
})

test_that("template_items substitutes and names", {
  it <- template_items(
    "Does the text express {label}?",
    c(joy = "joy", anger = "anger")
  )
  expect_equal(names(it), c("joy", "anger"))
  expect_equal(it$joy, "Does the text express joy?")
  expect_error(template_items("no placeholder", c("a", "b")), "\\{label\\}")
})

test_that("prompt puts the document first", {
  b <- instrument(list(a = "Is it raining?"), labels = c("YES", "NO"))
  p <- fathom:::build_prompt("It rained.", b$items[[1]], b)

  expect_lt(
    regexpr("It rained.", p, fixed = TRUE),
    regexpr("Is it raining?", p, fixed = TRUE)
  )
  expect_match(p, "Answer:$")
})

test_that("item hashes cover the cue and labels, not just the question", {
  b1 <- instrument(list(a = "Q?"), labels = c("YES", "NO"))
  b2 <- instrument(list(a = "Q?"), labels = c("SI", "NO"))
  b3 <- instrument(list(a = "Q?"), labels = c("YES", "NO"), cue = "\n\nAnswer.")

  h1 <- fathom:::item_hashes(b1, "m")
  expect_false(identical(h1, fathom:::item_hashes(b2, "m")))
  expect_false(identical(h1, fathom:::item_hashes(b3, "m")))
  expect_false(identical(h1, fathom:::item_hashes(b1, "other-model")))
  expect_identical(h1, fathom:::item_hashes(b1, "m"))
})

test_that("definitions reach the prompt and the hash", {
  # An exclusive instrument lists its definitions under the question, so they
  # are part of what the model reads and rewording one is a different
  # instrument.
  b1 <- instrument(
    "primary",
    labels = c("APPLE", "BANANA"),
    definitions = c(APPLE = "apples and pears", BANANA = "bananas"),
    exclusive = TRUE,
    medium = "text"
  )
  expect_match(b1$items[[1]], "apples and pears", fixed = TRUE)

  b2 <- instrument(
    "primary",
    labels = c("APPLE", "BANANA"),
    definitions = c(APPLE = "apricots and plums", BANANA = "bananas"),
    exclusive = TRUE,
    medium = "text"
  )
  expect_false(identical(
    fathom:::item_hashes(b1, "m"),
    fathom:::item_hashes(b2, "m")
  ))
})

test_that("scores do not reach the prompt, and so do not reach the hash", {
  # Deliberate. Rescaling an ordinal instrument changes the arithmetic
  # downstream and not one token the model sees, so re-extracting would be
  # waste.
  b1 <- instrument(
    list(a = "Q?"),
    labels = c("low", "mid", "high"),
    scores = c(low = 0, mid = 1, high = 2)
  )
  b2 <- instrument(
    list(a = "Q?"),
    labels = c("low", "mid", "high"),
    scores = c(low = 0, mid = 10, high = 100)
  )
  expect_identical(
    fathom:::item_hashes(b1, "m"),
    fathom:::item_hashes(b2, "m")
  )
})

test_that("the expected score follows the instrument, not the checkpoint", {
  # Since the hash ignores `scores`, a restored row would otherwise carry the
  # value computed under whatever scale was in force when it was written.
  # apply_scores() recomputes it from prob_renorm on every path that assembles
  # results, so the column is a function of the instrument in hand.
  b1 <- instrument(
    list(a = "Q?"),
    labels = c("low", "mid", "high"),
    scores = c(low = 0, mid = 1, high = 2)
  )
  b2 <- instrument(
    list(a = "Q?"),
    labels = c("low", "mid", "high"),
    scores = c(low = 0, mid = 10, high = 100)
  )

  stale <- tibble::tibble(
    id = "d1",
    item = "a",
    label = c("low", "mid", "high"),
    prob_renorm = c(0.2, 0.3, 0.5),
    expected_score = 99 # whatever an older instrument wrote
  )

  expect_equal(
    fathom:::apply_scores(stale, b1)$expected_score,
    rep(0.2 * 0 + 0.3 * 1 + 0.5 * 2, 3)
  )
  expect_equal(
    fathom:::apply_scores(stale, b2)$expected_score,
    rep(0.2 * 0 + 0.3 * 10 + 0.5 * 100, 3)
  )
})

test_that("apply_scores handles a scoreless instrument and an unknown label", {
  b_none <- instrument(list(a = "Q?"), labels = c("low", "high"))
  rows <- tibble::tibble(
    id = "d1",
    item = "a",
    label = c("low", "high"),
    prob_renorm = c(0.4, 0.6),
    expected_score = 7
  )
  expect_true(all(is.na(fathom:::apply_scores(rows, b_none)$expected_score)))

  # An all-absent call has no distribution to take an expectation over.
  b <- instrument(
    list(a = "Q?"),
    labels = c("low", "high"),
    scores = c(low = 0, high = 1)
  )
  absent <- tibble::tibble(
    id = "d1",
    item = "a",
    label = c("low", "high"),
    prob_renorm = NA_real_,
    expected_score = NA_real_
  )
  expect_true(all(is.na(fathom:::apply_scores(absent, b)$expected_score)))

  # A label the instrument has no score for means the checkpoint came from a
  # different instrument, which is refused rather than silently made NA.
  wrong <- tibble::tibble(
    id = "d1",
    item = "a",
    label = c("low", "middling"),
    prob_renorm = c(0.5, 0.5),
    expected_score = NA_real_
  )
  expect_error(fathom:::apply_scores(wrong, b), "no score for")
})

test_that("checkpoint staleness is per item", {
  b <- instrument(c(a = "A?", z = "Z?"), labels = c("YES", "NO"))
  h <- fathom:::item_hashes(b, "m")

  expect_equal(fathom:::ckpt_stale(NULL, h, "m"), c("a", "z"))

  prev <- list(
    results = tibble::tibble(
      item = c("a", "z"),
      prompt_hash = unname(h),
      model = "m",
      call_ok = TRUE
    )
  )
  expect_length(fathom:::ckpt_stale(prev, h, "m"), 0L)

  prev$results$call_ok[1] <- FALSE
  expect_equal(fathom:::ckpt_stale(prev, h, "m"), "a")

  prev$results$call_ok[1] <- TRUE
  prev$results$prompt_hash[2] <- "stale00000000"
  expect_equal(fathom:::ckpt_stale(prev, h, "m"), "z")
})

test_that("default_cue writes the cue instrument() has always written", {
  expect_identical(
    fathom:::default_cue(c("YES", "NO"), "en"),
    "\n\nAnswer with exactly one word: YES or NO."
  )
  expect_identical(
    fathom:::default_cue(c("SI", "NO", "TAL"), "es"),
    "\n\nResponde con exactamente una palabra: SI, NO, o TAL."
  )
  expect_identical(
    instrument(list(q = "Q?"), labels = c("SI", "NO"), lang = "es")$cue,
    fathom:::default_cue(c("SI", "NO"), "es")
  )
})

test_that("reorder_labels moves the cue and the listing and nothing else", {
  b <- instrument(
    "Which category?",
    labels = c("ECON", "HEALTH", "OTHER"),
    definitions = c(
      ECON = "the economy",
      HEALTH = "health care",
      OTHER = "anything else"
    ),
    scores = c(ECON = 1, HEALTH = 2, OTHER = 3),
    exclusive = TRUE
  )
  r <- fathom:::reorder_labels(b, c("OTHER", "HEALTH", "ECON"))
  expect_identical(
    r$items$category,
    paste0(
      "Which category?\n\n",
      "OTHER = anything else\nHEALTH = health care\nECON = the economy"
    )
  )
  expect_identical(
    r$cue,
    "\n\nAnswer with exactly one word: OTHER, HEALTH, or ECON."
  )
  expect_identical(names(r$labels), c("OTHER", "HEALTH", "ECON"))
  expect_identical(r$scores, c(OTHER = 3, HEALTH = 2, ECON = 1))
  expect_identical(r$name, b$name)
  # Put back in the declared order, it is the instrument instrument() built.
  expect_identical(fathom:::reorder_labels(r, names(b$labels)), b)
})

test_that("reorder_labels keeps a cue written by hand and refuses an edited question", {
  b <- instrument(
    "Which category?",
    labels = c("ECON", "HEALTH"),
    exclusive = TRUE,
    cue = "\n\nOne word, please."
  )
  r <- fathom:::reorder_labels(b, c("HEALTH", "ECON"))
  expect_identical(r$cue, b$cue)
  expect_true(endsWith(r$items$category, "HEALTH = HEALTH\nECON = ECON"))

  b$items$category <- "Which category?"
  expect_error(
    fathom:::reorder_labels(b, c("HEALTH", "ECON")),
    "does\\s+not\\s+end\\s+in\\s+the\\s+listing"
  )
})
