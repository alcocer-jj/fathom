# Offline tests for named stems, exclusive assembly, and preview().
# No server. Everything here is string construction and validation.

test_that("both language halves of every stem table are documented", {
  # instrument.R. The roxygen for template_items() and instrument() now prints
  # all three tables in English and Spanish, which makes them contracts. R CMD
  # check does not compare documentation against code, and the whole reason
  # these sections exist is that the Spanish halves shipped undocumented for a
  # long time. The names below are the documented ones.
  expect_setequal(names(fathom:::ITEM_STEMS), c("en", "es"))
  expect_setequal(names(fathom:::CHOICE_STEMS), c("en", "es"))
  expect_setequal(names(fathom:::LANG_PRESETS), c("en", "es"))

  # Every medium has its own set and the sets are not the same. A picture
  # mentions nothing, so image drops `mention` and gains `show` and `depict`,
  # and video carries both plus `discuss` because a row holds frames and a
  # transcript at once.
  want <- list(
    text = c("topic", "mention", "express", "about", "frame"),
    audio = c("topic", "mention", "express", "about", "frame"),
    image = c("show", "depict", "express", "about", "frame"),
    video = c("show", "depict", "discuss", "express", "about", "frame")
  )
  for (lg in c("en", "es")) {
    expect_setequal(names(fathom:::ITEM_STEMS[[lg]]), fathom:::STEM_MEDIA)
    expect_setequal(names(fathom:::CHOICE_STEMS[[lg]]), fathom:::STEM_MEDIA)
    for (m in fathom:::STEM_MEDIA) {
      expect_setequal(names(fathom:::ITEM_STEMS[[lg]][[m]]), want[[m]])
      expect_setequal(
        names(fathom:::CHOICE_STEMS[[lg]][[m]]),
        c("primary", "best")
      )
    }
    expect_setequal(
      names(fathom:::LANG_PRESETS[[lg]]),
      c(
        "text_label",
        "answer_label",
        "or_word",
        "cue_fmt"
      )
    )
  }

  # The four preset strings the instrument() section prints verbatim.
  expect_equal(fathom:::LANG_PRESETS$es$text_label, "Texto")
  expect_equal(fathom:::LANG_PRESETS$es$answer_label, "Respuesta")
  expect_equal(fathom:::LANG_PRESETS$es$or_word, "o")
  expect_match(fathom:::LANG_PRESETS$es$cue_fmt, "Responde con exactamente una")

  # Neither choice stem takes a {label}, which is what separates them from the
  # item stems and is stated in both sections.
  for (lg in c("en", "es")) {
    for (m in fathom:::STEM_MEDIA) {
      for (st in fathom:::CHOICE_STEMS[[lg]][[m]]) {
        expect_false(grepl("{label}", st, fixed = TRUE))
      }
      for (st in fathom:::ITEM_STEMS[[lg]][[m]]) {
        expect_true(grepl("{label}", st, fixed = TRUE))
      }
    }
  }
})

test_that("the roxygen tables print the stems the package ships", {
  # The block above checks names. It passed while the table in
  # template_items() printed `Does the text discuss {label}?` against a
  # `Does this text discuss {label}?` in ITEM_STEMS, in both languages, on
  # every one of the five text stems, because nothing compared the text.
  # R CMD check does not do this and neither did anything here.
  #
  # The same shape as the three source-reading tests in test-regressions.R,
  # down to the message, since two spellings of one guard is the drift this
  # package keeps finding. Under R CMD check the tests run from a copy of
  # tests/ with no R/ beside it, so this runs under devtools::test().
  #
  # `encoding` is the one difference and it is load bearing. These stems carry
  # non-ASCII and the comparison below is `fixed = TRUE`, so the lines have to
  # be marked UTF-8 rather than native. The regression tests match ASCII and
  # do not need it.
  src <- "../../R/instrument.R"
  skip_if_not(file.exists(src), "package sources not reachable from here")

  rox <- readLines(src, encoding = "UTF-8", warn = FALSE)
  rox <- sub("^#' ?", "", rox[startsWith(rox, "#'")])
  rox <- paste(rox, collapse = " ")

  # Three escapes stand between the table and the string it prints. The braces
  # around `label` are escaped for Rd. `\enc{a}{b}` carries the UTF-8 form and
  # its ASCII fallback, and the first half is what the stem contains. The
  # whitespace squeeze comes last, since a table row wraps across `#'` lines
  # and rejoining leaves the break wherever it fell.
  rox <- gsub("\\{label\\}", "{label}", rox, fixed = TRUE)
  rox <- gsub("\\\\enc\\{([^{}]*)\\}\\{[^{}]*\\}", "\\1", rox)
  rox <- gsub("[[:space:]]+", " ", rox)

  # The text set is the one both sections print in full, so it is the one the
  # documentation can be wrong about. The other three media are covered by
  # name above, which is all their tables claim.
  for (tbl in list(fathom:::ITEM_STEMS, fathom:::CHOICE_STEMS)) {
    for (lg in c("en", "es")) {
      for (st in tbl[[lg]]$text) {
        expect_true(
          grepl(gsub("[[:space:]]+", " ", st), rox, fixed = TRUE),
          info = paste("undocumented or misdocumented stem:", st)
        )
      }
    }
  }
})

test_that("named stems resolve and custom strings still work", {
  it <- template_items(
    "topic",
    c(econ = "the economy", immig = "immigration"),
    medium = "text"
  )
  expect_equal(names(it), c("econ", "immig"))
  expect_equal(it$econ, "Does this text discuss the economy?")

  expect_equal(
    template_items("mention", c(a = "x"), medium = "text")$a,
    "Does this text mention x?"
  )
  expect_equal(
    template_items("about", c(a = "x"), medium = "text")$a,
    "Is this text primarily about x?"
  )

  # A custom string carries its own wording, so it needs no medium and is
  # passed through untouched. That is what keeps every existing call working.
  expect_equal(
    template_items("Does the author endorse {label}?", c(a = "x"))$a,
    "Does the author endorse x?"
  )
})

test_that("the medium decides the question, and has no default", {
  # The same stem name asks a different question of a different document.
  expect_equal(
    template_items("about", c(a = "x"), medium = "audio")$a,
    "Is this recording primarily about x?"
  )
  expect_equal(
    template_items("about", c(a = "x"), medium = "image")$a,
    "Is this image primarily about x?"
  )
  expect_equal(
    template_items("about", c(a = "x"), medium = "video")$a,
    "Is this video primarily about x?"
  )

  # Naming a preset without a medium refuses rather than guessing, since the
  # sets differ by medium rather than translating.
  expect_error(template_items("about", c(a = "x")), "`medium` is")
  expect_error(template_items("about", c(a = "x")), "text, audio, image, video")

  # A stem that exists for one medium and not another says so by name.
  expect_error(
    template_items("mention", c(a = "x"), medium = "image"),
    "not a preset for medium"
  )
  expect_error(
    template_items("mention", c(a = "x"), medium = "image"),
    "show, depict, express, about, frame"
  )

  # And an unknown medium is refused before the lookup.
  expect_error(
    template_items("about", c(a = "x"), medium = "pdf"),
    "must be one of"
  )
})

test_that("stems are language-aware", {
  es <- template_items(
    "topic",
    c(a = "la econom\u00eda"),
    lang = "es",
    medium = "text"
  )
  expect_match(es$a, "^\u00bfEste texto trata sobre")

  # The Spanish demonstrative agrees with the noun, which is why the sets are
  # written rather than converted from a shared template.
  expect_match(
    template_items("show", c(a = "x"), lang = "es", medium = "image")$a,
    "^\u00bfEsta imagen"
  )
  expect_match(
    template_items("show", c(a = "x"), lang = "es", medium = "video")$a,
    "^\u00bfEste video"
  )
})

test_that("an unknown stem without a placeholder errors helpfully", {
  expect_error(template_items("nonsense", c(a = "x")), "must contain")
  # The message lists the presets by medium, since which ones exist depends on
  # which kind of document is being asked about.
  expect_error(template_items("nonsense", c(a = "x")), "image \\(show, depict")
  expect_error(template_items("nonsense", c(a = "x")), "video \\(show, depict")
})

test_that("exclusive instruments build without a backend", {
  # They used to require one, because the categories become answer tokens and
  # multi-syllable labels rarely survive tokenization, so the scoring targets
  # had to be discovered. Prefix matching removes that. BU opens BUDGET and
  # nothing else here starts with B.
  b <- instrument(
    "primary",
    labels = c("BUDGET", "HOUSING"),
    exclusive = TRUE,
    medium = "text"
  )
  expect_false(b$exact_match)
  expect_null(b$firsts)
  expect_match(b$items[[1]], "PRIMARY subject of this text")

  # The choice stems follow the same nouns.
  expect_match(
    instrument(
      "primary",
      labels = c("BUDGET", "HOUSING"),
      exclusive = TRUE,
      medium = "image"
    )$items[[1]],
    "PRIMARY subject of this image"
  )

  # A question rather than a preset name needs no medium, so an exclusive
  # instrument written out by hand is unaffected.
  q <- instrument(
    "Which one is it?",
    labels = c("BUDGET", "HOUSING"),
    exclusive = TRUE
  )
  expect_match(q$items[[1]], "Which one is it")

  expect_error(
    instrument("primary", labels = c("BUDGET", "HOUSING"), exclusive = TRUE),
    "`medium` is"
  )
})

test_that("exact matching needs a backend to discover anything", {
  expect_error(
    instrument(
      "primary",
      labels = c("BUDGET", "HOUSING"),
      exclusive = TRUE,
      medium = "text",
      exact = TRUE
    ),
    "requires `backend`"
  )
})

test_that("labels must differ at their first character", {
  # A split label often emits only its first character, so two labels sharing
  # one cannot be attributed. Checked at construction, with no calls.
  expect_error(
    instrument(list(q = "Q?"), labels = c("economia", "educacion")),
    "differ at their first character"
  )
  # The message should name the pair, not just the character.
  expect_error(
    instrument(list(q = "Q?"), labels = c("economia", "educacion")),
    "economia and educacion"
  )

  expect_error(
    instrument(list(q = "Q?"), labels = c("SI", "SIEMPRE")),
    "differ at their first character"
  )

  # Distinct first characters build and stay on prefix matching.
  b <- instrument(list(q = "Q?"), labels = c("SI", "NO"))
  expect_false(b$exact_match)
})

test_that("exact matching always needs a backend", {
  # Not only for exclusive instruments. Labels that survive tokenization whole
  # score the same under either rule, so exact matching is only ever worth
  # asking for when probing found something, and asking for it unprobed skips
  # the first-character check and then matches no fragment at all.
  expect_error(
    instrument(list(q = "Q?"), labels = c("YES", "NO"), exact = TRUE),
    "requires `backend`"
  )
  expect_error(
    instrument(
      "primary",
      labels = c("A", "B"),
      exclusive = TRUE,
      medium = "text",
      exact = TRUE
    ),
    "requires `backend`"
  )
})

test_that("exact matching lifts the first-character constraint", {
  # Colliding labels are refused by default and allowed under exact = TRUE,
  # since scoring then matches what the model emitted rather than any prefix.
  # The backend requirement is what errors here, and it errors after the
  # constraint would have, which is how the skip is observable offline.
  expect_error(
    instrument(list(q = "Q?"), labels = c("economia", "educacion")),
    "differ at their first character"
  )
  expect_error(
    instrument(
      list(q = "Q?"),
      labels = c("economia", "educacion"),
      exact = TRUE
    ),
    "requires `backend`"
  )
})

test_that("exclusive instruments take exactly one question", {
  # A bare structure is enough. The length check fires before anything is sent,
  # which is also why passing a backend here costs no calls.
  expect_error(
    instrument(
      c("primary", "best"),
      labels = c("A", "B"),
      exclusive = TRUE,
      backend = structure(list(), class = "fathom_backend"),
      medium = "text"
    ),
    "takes one question"
  )
})

test_that("definitions are separate from labels", {
  # A bare structure is enough, since every assertion below errors before
  # probe_labels() is reached, so nothing here calls out.
  fake <- structure(list(), class = "fathom_backend")

  # Text passed through `labels` is refused rather than reinterpreted. Treating
  # a description as a surface variant registers a token the model never emits,
  # and that category then scores zero on every document with no error.
  expect_error(
    instrument(
      "primary",
      labels = c(BUDGET = "spending", HOUSING = "homes"),
      exclusive = TRUE,
      backend = fake,
      medium = "text"
    ),
    "belongs in `definitions`"
  )

  expect_error(
    instrument(
      "primary",
      labels = c("BUDGET", "HOUSING"),
      definitions = c(BUDGET = "spending"),
      exclusive = TRUE,
      backend = fake,
      medium = "text"
    ),
    "name every label"
  )
})

test_that("scores do not require an exclusive instrument", {
  b <- instrument(
    list(tone = "How does it read?"),
    labels = c("low", "mid", "high"),
    scores = c(low = 0, mid = 1, high = 2)
  )
  # Stored in label order, so score_labels() can multiply element-wise.
  expect_equal(names(b$scores), c("low", "mid", "high"))
  expect_equal(unname(b$scores), c(0, 1, 2))

  expect_error(
    instrument(
      list(tone = "Q?"),
      labels = c("low", "mid", "high"),
      scores = c(low = 0, mid = 1)
    ),
    "name every label"
  )
  expect_error(
    instrument(
      list(tone = "Q?"),
      labels = c("low", "mid", "high"),
      scores = c(low = "a", mid = "b", high = "c")
    ),
    "numeric"
  )
})

test_that("paste_or drops the comma at two items", {
  expect_equal(fathom:::paste_or(c("A", "B", "C")), "A, B, or C")
  expect_equal(fathom:::paste_or(c("A", "B")), "A or B")
  expect_equal(fathom:::paste_or("A"), "A")
  expect_equal(fathom:::paste_or(c("A", "B"), "o"), "A o B")
  expect_equal(fathom:::paste_or(c("A", "B", "C"), "o"), "A, B, o C")
})

test_that("the generated cue lists labels the same way everywhere", {
  # The general branch and the exclusive branch both go through paste_or(), so
  # a three-label instrument reads "a, b, or c" rather than "a or b or c".
  b2 <- instrument(list(q = "Q?"), labels = c("YES", "NO"))
  expect_match(b2$cue, "YES or NO", fixed = TRUE)

  b3 <- instrument(list(q = "Q?"), labels = c("a", "b", "c"))
  expect_match(b3$cue, "a, b, or c", fixed = TRUE)
})

test_that("preview shows the document before the question", {
  b <- instrument(
    template_items("topic", c(econ = "the economy"), medium = "text"),
    labels = c("YES", "NO")
  )
  p <- preview(b, text = "UNIQUEDOC")

  expect_lt(
    regexpr("UNIQUEDOC", p, fixed = TRUE),
    regexpr("the economy", p, fixed = TRUE)
  )
  expect_match(p, "Answer:$")
})

test_that("preview truncates long documents for display only", {
  b <- instrument(list(q = "Is it long?"), labels = c("YES", "NO"))
  long <- paste(rep("word", 500), collapse = " ")
  out <- paste(
    capture.output(p <- preview(b, text = long, truncate = 50L)),
    collapse = "\n"
  )

  # The screen is trimmed and says so.
  expect_match(out, "truncated for display", fixed = TRUE)
  expect_false(grepl(long, out, fixed = TRUE))
  # The returned string is what measure() sends, whole and unmarked.
  expect_false(grepl("truncated for display", p, fixed = TRUE))
  expect_identical(p, fathom:::build_prompt(long, b$items$q, b))
})

test_that("preview rejects an unknown item", {
  b <- instrument(list(q = "Q?"), labels = c("YES", "NO"))
  expect_error(preview(b, item = "nope"), "not in the instrument")
})
