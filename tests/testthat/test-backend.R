# check_backend(), which had no tests at all.
#
# That is why its @return and its body disagreed. The block enumerates six
# failures returning FALSE, the sixth being a first token that is not a label,
# and the body warned and returned TRUE. Nothing in the suite reached the real
# function to notice. local_offline() mocks it away, so every measure() test in
# the suite runs without ever calling it.
#
# The three failures before the probe run through ollama_tags(), which nothing
# else mocks either, so they are driven the same way ollama_generate() is.

# A generator returning a payload with no label at position 1. fx_no_label()
# holds the tokens; this wraps them in the shape a call returns.
stub_tokens <- function(payload) {
  force(payload)
  function(prompt, backend, num_ctx = NULL, image = NULL) {
    list(
      response = payload[[1]]$token,
      prompt_eval_count = 100L,
      prompt_eval_duration = 1e6,
      wall_ms = 1,
      logprobs = list(list(top_logprobs = payload))
    )
  }
}

# Capture every prompt in order, so a test can assert what was sent rather than
# only what came back. The probe is always the first, which is what separates it
# from the run that follows.
stub_capture <- function(sink, payload = lp(c("YES", "NO"), c(-0.01, -4.6))) {
  force(payload)
  sink$prompts <- character(0)
  function(prompt, backend, num_ctx = NULL, image = NULL) {
    sink$prompts <- c(sink$prompts, prompt)
    list(
      response = payload[[1]]$token,
      prompt_eval_count = 100L,
      prompt_eval_duration = 1e6,
      wall_ms = 1,
      logprobs = list(list(top_logprobs = payload))
    )
  }
}

# The tag list a healthy server returns, in the four columns ollama_tags()
# produces plus the `modalities` that show_models() adds, rather than only the
# column check_backend() happens to read. Mocked rather than short circuited,
# so the three pre-probe failures are reachable at all.
fake_tags <- function(names = "m", modalities = "text") {
  tibble::tibble(
    name = names,
    size = rep(NA_real_, length(names)),
    params = rep("", length(names)),
    quant = rep("", length(names)),
    modalities = rep_len(modalities, length(names))
  )
}

local_tags <- function(
  tags = fake_tags(),
  gen = stub_generate(),
  env = parent.frame()
) {
  testthat::local_mocked_bindings(
    ollama_tags = function(host) tags,
    ollama_generate = gen,
    .package = "fathom",
    .env = env
  )
}

# --- the three failures before any call is made ------------------------------

test_that("an unreachable host is refused and named", {
  testthat::local_mocked_bindings(
    ollama_tags = function(host) {
      structure(
        list(message = "connection refused"),
        class = "fathom_tags_failure"
      )
    },
    .package = "fathom"
  )

  msgs <- testthat::capture_messages(
    v <- check_backend(stub_backend())
  )
  expect_false(v)
  expect_match(paste(msgs, collapse = "\n"), "http://stub")
  expect_match(paste(msgs, collapse = "\n"), "connection refused")
})

test_that("a server with nothing installed is refused", {
  local_tags(tags = fake_tags(character(0)))

  msgs <- testthat::capture_messages(v <- check_backend(stub_backend()))
  expect_false(v)
  expect_match(paste(msgs, collapse = "\n"), "no models installed")
})

test_that("a tag that is not installed is refused, and the installed ones listed", {
  local_tags(tags = fake_tags(c("other:7b", "third:3b")))

  msgs <- testthat::capture_messages(v <- check_backend(stub_backend("m")))
  expect_false(v)
  expect_match(paste(msgs, collapse = "\n"), "'m'")
  expect_match(paste(msgs, collapse = "\n"), "other:7b")
})

# --- the two failures at the call --------------------------------------------

test_that("a probe call that errors is refused rather than propagated", {
  local_tags(gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
    stop("connection reset")
  })

  msgs <- testthat::capture_messages(v <- check_backend(stub_backend()))
  expect_false(v)
  expect_match(paste(msgs, collapse = "\n"), "Probe call failed")
  expect_match(paste(msgs, collapse = "\n"), "connection reset")
})

test_that("a reply with no distribution names the Ollama version", {
  # backend.R. Three shapes reach this, and only the last one used to. `[[1]]`
  # on an empty list or on NULL raises rather than returning NULL, so `%||%`
  # never saw them and a subscript error came out of a function contracted to
  # return a logical. measure_one() reads the same field under tryCatch, which
  # is why the equivalent case in test-pipeline.R passed all along.
  shapes <- list(
    empty = list(response = "YES", logprobs = list()),
    absent = list(response = "YES"),
    hollow = list(response = "YES", logprobs = list(list()))
  )

  for (nm in names(shapes)) {
    local_tags(
      gen = local({
        r <- shapes[[nm]]
        function(prompt, backend, num_ctx = NULL, image = NULL) r
      })
    )
    msgs <- testthat::capture_messages(v <- check_backend(stub_backend()))
    expect_false(v, info = nm)
    expect_match(paste(msgs, collapse = "\n"), "0.12.11")
  }
})

# --- the seventh failure, which is the reason this function exists ----------

test_that("DECISION. a first token that is not a label returns FALSE", {
  # backend.R. The @return block has always documented this as one of the FALSE
  # conditions, and named it the reason to run the check before a corpus rather
  # than after. The body messaged and returned TRUE, so measure() proceeded.
  #
  # A thirteen-model sweep found four templates writing <think>, <|channel|>, a
  # bare newline, or Okay at position 1. Every one of them returned a full
  # results frame built from trace mass under a token holding everything, and
  # two of the four put the largest probability on a different label across
  # five documents.
  #
  # The behavior was defensible as a warning while nothing downstream depended
  # on it. What is not defensible is a documented FALSE that never happens. This
  # records which way that went, so a later change argues with it.
  local_tags(gen = stub_tokens(fx_no_label()))

  msgs <- testthat::capture_messages(v <- check_backend(stub_backend()))
  expect_false(v)
  expect_match(paste(msgs, collapse = "\n"), "counts as none of")
  expect_match(paste(msgs, collapse = "\n"), "reasoning block")
})

test_that("a label at position 1 returns TRUE and records what it read", {
  local_tags(gen = stub_generate())

  # The configuration is part of measure()'s header now, so a passing check
  # prints nothing and hands over what it read instead.
  pre <- new.env()
  msgs <- testthat::capture_messages(
    v <- check_backend(stub_backend(), num_ctx = 4096L, report = pre)
  )
  expect_true(v)
  expect_length(msgs, 0L)
  expect_identical(pre$top1, "YES")
  expect_identical(pre$n_logprobs, 2L)
})

test_that("a passing check is silent and a failing one still reports", {
  local_tags(gen = stub_generate())
  expect_silent(v <- check_backend(stub_backend()))
  expect_true(v)

  local_tags(gen = stub_tokens(fx_no_label()))
  msgs <- testthat::capture_messages(
    v <- check_backend(stub_backend())
  )
  expect_false(v)
  expect_match(paste(msgs, collapse = "\n"), "counts as none of")
})

# --- the two modes -----------------------------------------------------------

test_that("without text the instrument is ignored and the generic probe sent", {
  # The point of the split. A question written for a corpus has no answer about
  # a sentence invented here, so pairing the two could refuse a working setup.
  # What the generic probe tests is the chat template, which is what the four
  # failing models above have in common and which no label set changes.
  sink <- new.env()
  local_tags(gen = stub_capture(sink))

  expect_true(suppressMessages(
    check_backend(stub_backend(), split_instrument())
  ))
  expect_match(
    sink$prompts[1],
    "It rained heavily all afternoon.",
    fixed = TRUE
  )
  expect_match(sink$prompts[1], "Is the text about weather?", fixed = TRUE)
  expect_match(sink$prompts[1], "YES or NO", fixed = TRUE)
  expect_no_match(sink$prompts[1], "BUDGET", fixed = TRUE)
})

test_that("with text the caller's document, item, and cue are sent whole", {
  sink <- new.env()
  local_tags(gen = stub_capture(sink, lp(c("BU", "HO"), c(-0.05, -3.1))))

  expect_true(suppressMessages(check_backend(
    stub_backend(),
    split_instrument(),
    text = "a document about municipal spending"
  )))
  expect_match(sink$prompts[1], "municipal spending", fixed = TRUE)
  expect_match(sink$prompts[1], "Which category?", fixed = TRUE)
  expect_match(sink$prompts[1], "BUDGET", fixed = TRUE)
  expect_no_match(sink$prompts[1], "weather", fixed = TRUE)
})

test_that("text without an instrument still takes the generic path", {
  # Neither half alone is enough, since the document is only meaningful beside
  # the question it was written for.
  sink <- new.env()
  local_tags(gen = stub_capture(sink))

  expect_true(suppressMessages(
    check_backend(stub_backend(), text = "some document")
  ))
  expect_match(
    sink$prompts[1],
    "It rained heavily all afternoon.",
    fixed = TRUE
  )
  expect_no_match(sink$prompts[1], "some document", fixed = TRUE)
})

test_that("a fragment counts on a prefix instrument and not on an exact one", {
  # Matching is asked the way the run asks it. BU is scored as BUDGET under
  # prefix matching, so flagging it here would refuse a run that works. Under
  # exact matching it is not BUDGET, and the check has to agree with that too.
  sink <- new.env()
  local_tags(gen = stub_capture(sink, fx_split()))
  expect_true(suppressMessages(check_backend(
    stub_backend(),
    split_instrument(),
    text = "a doc"
  )))

  local_tags(gen = stub_capture(sink, fx_split()))
  expect_false(suppressMessages(check_backend(
    stub_backend(),
    as_exact(split_instrument()),
    text = "a doc"
  )))
})

test_that("an exclusive instrument is checked rather than skipped", {
  # It used to return TRUE without probing, on the grounds that a weather
  # question has no answer among a user's categories. With text the question is
  # the instrument's own, so that reason is gone and gpt-oss style templates are
  # no longer waved through on the exclusive shape.
  ex <- instrument(
    "primary",
    labels = c("BUDGET", "HOUSING"),
    definitions = c(BUDGET = "spending", HOUSING = "shelter"),
    exclusive = TRUE,
    medium = "text"
  )

  local_tags(gen = stub_tokens(fx_no_label()))
  expect_false(suppressMessages(
    check_backend(stub_backend(), ex, text = "a doc")
  ))

  local_tags(gen = stub_capture(new.env(), fx_split()))
  expect_true(suppressMessages(
    check_backend(stub_backend(), ex, text = "a doc")
  ))
})

# --- the document measure() picks --------------------------------------------

test_that("probe_doc takes the document nearest the median word count", {
  x <- c(
    paste(rep("w", 1), collapse = " "),
    paste(rep("w", 5), collapse = " "),
    paste(rep("w", 100), collapse = " ")
  )
  expect_equal(fathom:::probe_doc(x), x[2])

  # Nearest rather than equal, so an even count still returns a real document.
  y <- c("a", "a a a a", "a a a a a a", "a a a a a a a a a a")
  expect_true(fathom:::probe_doc(y) %in% y)
})

test_that("probe_doc skips empty documents and gives up rather than sending one", {
  expect_equal(
    fathom:::probe_doc(c(NA, "", "   ", "real one here")),
    "real one here"
  )
  expect_null(fathom:::probe_doc(c(NA_character_, "", "  ")))
  expect_null(fathom:::probe_doc(character(0)))
})

test_that("probe_doc is stable, so a rerun checks the same document", {
  x <- c("a a a", "b b b", "c c c c c c c c")
  expect_equal(fathom:::probe_doc(x), fathom:::probe_doc(x))
  expect_equal(fathom:::probe_doc(x), "a a a")
})

# --- what the change means for a run -----------------------------------------

test_that("measure() stops on a template that writes no label", {
  # The whole point. Without this the run returns a full frame, and every column
  # downstream reads as measurement.
  testthat::local_mocked_bindings(
    ollama_tags = function(host) fake_tags(),
    ollama_generate = stub_tokens(fx_no_label()),
    .package = "fathom"
  )
  d <- tibble::tibble(id = c("p", "q"), body = c("one two three", "four five"))

  expect_error(
    suppressMessages(measure(
      d,
      stub_backend(),
      ab_instrument(),
      text = "body",
      id = "id",
      num_ctx = NULL
    )),
    "Not starting"
  )
})

test_that("measure() probes with a real document before sending anything else", {
  # The probe is the first prompt out, and it carries a document from the corpus
  # rather than the invented sentence. That is the whole reason for the text
  # argument, so it is asserted on the wire rather than inferred from probe_doc.
  sink <- new.env()
  testthat::local_mocked_bindings(
    ollama_tags = function(host) fake_tags(),
    ollama_generate = stub_capture(sink),
    .package = "fathom"
  )
  d <- tibble::tibble(
    id = c("p", "q", "r"),
    body = c(
      "one",
      "a document of middling length here",
      paste(rep("w", 90), collapse = " ")
    )
  )

  invisible(suppressMessages(measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )))

  first <- sink$prompts[1]
  expect_match(first, "a document of middling length here", fixed = TRUE)
  expect_no_match(first, "It rained heavily", fixed = TRUE)

  # And the run itself still happened, so the probe is an addition rather than a
  # replacement. Three documents times two items, plus the one probe.
  expect_equal(length(sink$prompts), 3L * 2L + 1L)
})

test_that("check_backend refuses exact scoring with no discovered tokens", {
  # The state is unreachable through instrument() alone. `exact = TRUE` demands
  # a backend, and a probe that answers writes `firsts`, so reproducing the
  # failure means setting the pair directly. That is the state a user reaches
  # when the probe warned and they carried on.
  bk <- ollama_backend("m", validate = FALSE)
  inst <- instrument(list(q = "Is it about weather?"), labels = c("YES", "NO"))
  inst$exact_match <- TRUE
  inst$firsts <- NULL

  expect_message(
    expect_false(check_backend(bk, inst)),
    "exact match and carries no discovered"
  )

  # It is settled before the network, so a host that does not exist is never
  # reached. Any other failure would have to call out to report itself.
  dead <- ollama_backend("m", host = "http://127.0.0.1:1", validate = FALSE)
  expect_false(check_backend(dead, inst))

  # And it is silent on every instrument that did not ask for exact scoring,
  # which is the default and is where firsts is legitimately NULL.
  plain <- instrument(list(q = "Is it about weather?"), labels = c("YES", "NO"))
  expect_null(plain$firsts)
  expect_false(isTRUE(plain$exact_match))
})

test_that("a backend declares what it takes, and refuses what it does not", {
  expect_equal(
    ollama_backend("m", validate = FALSE)$modalities,
    c("text", "image")
  )
  expect_equal(
    ollama_backend("m", modalities = "text", validate = FALSE)$modalities,
    "text"
  )
  expect_error(
    ollama_backend("m", modalities = "smell", validate = FALSE),
    "smell"
  )
  expect_error(
    ollama_backend("m", modalities = character(0), validate = FALSE),
    "character vector"
  )
  expect_output(print(ollama_backend("m", validate = FALSE)), "Modalities")
})

test_that("audio and video are refused, and the message says where they go", {
  # Both were accepted and nothing read them, since measure() compares only
  # text and image against this field. A native audio path would add its word
  # back when it sends one.
  expect_error(
    ollama_backend("m", modalities = c("text", "audio"), validate = FALSE),
    "prepare_audio()",
    fixed = TRUE
  )
  expect_error(
    ollama_backend("m", modalities = "video", validate = FALSE),
    "prepare_video()",
    fixed = TRUE
  )
  # A plain misspelling gets no pointer to either.
  msg <- tryCatch(
    ollama_backend("m", modalities = "imgae", validate = FALSE),
    error = conditionMessage
  )
  expect_match(msg, "imgae", fixed = TRUE)
  expect_false(grepl("prepare_audio", msg, fixed = TRUE))
})

test_that("measure refuses a column the backend does not declare", {
  # Locally, before anything is spent. Ollama returns 200 for a field it does
  # not recognize, so sending it and waiting to be told is not an option.
  local_offline()
  docs <- tibble::tibble(id = "a", body = "one", pic = "no_such.png")
  text_only <- ollama_backend("m", modalities = "text", validate = FALSE)

  expect_error(
    measure(docs, text_only, ab_instrument(), text = "body", image = "pic"),
    "does not take image"
  )
  # And neither column named is not a call with a document in it.
  expect_error(
    measure(docs, stub_backend(), ab_instrument()),
    "name `text`, `image`, or both"
  )
  # A typo is reported as a missing column rather than as a modality problem.
  expect_error(
    measure(docs, stub_backend(), ab_instrument(), image = "nope"),
    "column\\(s\\) not found"
  )
})

test_that("image_b64 refuses a path that is not there", {
  expect_error(fathom:::image_b64("no_such_file.png"), "not found")

  f <- tempfile(fileext = ".png")
  writeBin(as.raw(c(1, 2, 3)), f)
  # Read as raw, so the bytes survive rather than being guessed as text.
  expect_equal(fathom:::image_b64(f), jsonlite::base64_enc(as.raw(c(1, 2, 3))))
})

test_that("measure sends the image to the backend, not into the prompt", {
  # The one thing testable offline. A live call is the only proof the model
  # reads it, but this proves the path carries it and that the prompt is
  # unchanged by its presence.
  seen <- new.env(parent = emptyenv())
  seen$img <- character(0)
  seen$prompt <- character(0)
  capture <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    seen$prompt <- c(seen$prompt, prompt)
    seen$img <- c(seen$img, if (is.null(image)) NA_character_ else image)
    stub_generate()(prompt, backend, num_ctx)
  }
  local_offline(gen = capture)

  f <- tempfile(fileext = ".png")
  writeBin(as.raw(c(9, 9, 9)), f)
  docs <- tibble::tibble(id = "a", body = "one", pic = f)

  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    text = "body",
    image = "pic",
    id = "id",
    num_ctx = NULL
  )

  expect_true(all(res$probs$call_ok))
  # Two items, so two calls, each carrying the path.
  expect_equal(seen$img, c(f, f))
  # And the picture is not in the prompt. It rides in its own field.
  expect_false(any(grepl(basename(f), seen$prompt, fixed = TRUE)))
})

test_that("measure works from an image alone, with no text column", {
  seen <- new.env(parent = emptyenv())
  seen$img <- character(0)
  capture <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    seen$img <- c(seen$img, if (is.null(image)) NA_character_ else image)
    stub_generate()(prompt, backend, num_ctx)
  }
  local_offline(gen = capture)

  f <- tempfile(fileext = ".png")
  writeBin(as.raw(1), f)
  docs <- tibble::tibble(id = "a", pic = f)

  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    image = "pic",
    id = "id",
    num_ctx = NULL
  )
  expect_true(all(res$probs$call_ok))
  expect_equal(unique(seen$img), f)
})

test_that("the image probe sends a readable path and compares two pictures", {
  # It sent base64 where ollama_generate() wanted a path, so image_b64()
  # encoded an already-encoded string, file.exists() failed, and the probe
  # reported itself as having failed rather than as having been misused. The
  # stub asserts what it received rather than only what it was asked.
  seen <- new.env(parent = emptyenv())
  seen$paths <- character(0)

  gen <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    if (!is.null(image)) {
      seen$paths <- c(seen$paths, image)
      # A path, present on disk, and big enough to be a PNG rather than a
      # string that happens to look like one.
      testthat::expect_true(file.exists(image))
      testthat::expect_gt(file.size(image), 50)
    }
    # A different distribution per picture, which is what passing looks like.
    tok <- if (length(seen$paths) %% 2L == 1L) {
      c("YES", "NO")
    } else {
      c("NO", "YES")
    }
    stub_generate(tokens = tok)(prompt, backend, num_ctx)
  }

  local_tags(gen = gen)

  ok <- suppressMessages(check_backend(
    stub_backend(),
    ab_instrument(),
    text = "one",
    images = TRUE
  ))
  expect_true(ok)
  # Two pictures, and not the same one twice.
  expect_equal(length(seen$paths), 2L)
  expect_false(identical(seen$paths[1], seen$paths[2]))
})

test_that("the image probe fails when both pictures give one distribution", {
  # What an ignored `images` field looks like from the outside, which is the
  # whole reason the probe exists. Ollama answers HTTP 200 to a field it does
  # not recognize.
  same <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    stub_generate()(prompt, backend, num_ctx)
  }
  local_tags(gen = same)

  expect_message(
    expect_false(suppressWarnings(check_backend(
      stub_backend(),
      ab_instrument(),
      text = "one",
      images = TRUE
    ))),
    "identical distribution"
  )
})

test_that("a failed image call says what can be done about it", {
  # It told the caller to pass `images = FALSE`, which measure() sets itself,
  # or to drop `image` from the backend, which measure() then refuses.
  fails <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    if (!is.null(image)) {
      stop("vision is not available")
    }
    stub_generate()(prompt, backend, num_ctx)
  }
  local_tags(gen = fails)

  said <- character(0)
  ok <- withCallingHandlers(
    check_backend(stub_backend(), ab_instrument(), text = "one", images = TRUE),
    message = function(m) {
      said <<- c(said, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(ok)
  joined <- paste(said, collapse = " ")
  expect_match(joined, "Check that the model takes images", fixed = TRUE)
  expect_false(grepl("images = FALSE", joined, fixed = TRUE))
})

test_that("a list column sends every frame, a character column sends one", {
  # The probe settled that Ollama takes a vector. One picture cost 86 prompt
  # tokens, two cost 137 and four cost 239, so each is encoded rather than the
  # first being kept. This is the package catching up to that.
  seen <- new.env(parent = emptyenv())
  seen$n <- integer(0)
  seen$paths <- character(0)
  gen <- function(prompt, backend, num_ctx = NULL, image = NULL) {
    seen$n <- c(seen$n, length(image %||% character(0)))
    seen$paths <- c(seen$paths, image)
    stub_generate()(prompt, backend, num_ctx)
  }
  local_offline(gen = gen)

  f <- vapply(
    1:3,
    function(i) {
      p <- tempfile(fileext = ".png")
      writeBin(as.raw(rep(i, 20)), p)
      p
    },
    character(1)
  )

  # A window of two frames and a window of one, in one corpus.
  docs <- tibble::tibble(
    id = c("w1", "w2"),
    pic = list(f[1:2], f[3])
  )
  res <- quiet_measure(
    docs,
    stub_backend(),
    ab_instrument(),
    image = "pic",
    id = "id",
    num_ctx = NULL
  )
  expect_true(all(res$probs$call_ok))
  # Two items per document, so four calls, carrying 2, 2, 1, 1 frames.
  expect_equal(sort(seen$n), c(1L, 1L, 2L, 2L))
  expect_setequal(unique(seen$paths), f)

  # A character column is the same corpus with one frame each, and it is what
  # every existing call passes, so it must keep working unchanged.
  seen$n <- integer(0)
  flat <- tibble::tibble(id = c("a", "b"), pic = f[1:2])
  quiet_measure(
    flat,
    stub_backend(),
    ab_instrument(),
    image = "pic",
    id = "id",
    num_ctx = NULL
  )
  expect_equal(unique(seen$n), 1L)
})

test_that("as_media returns one shape whichever it was handed", {
  # as.character() on a list column pastes each element into one string, so
  # every path after the first would be lost without the branch.
  flat <- fathom:::as_media(c("a.png", "b.png"))
  expect_type(flat, "list")
  expect_equal(lengths(flat), c(1L, 1L))

  nested <- fathom:::as_media(list(c("f1.png", "f2.png"), "f3.png"))
  expect_equal(lengths(nested), c(2L, 1L))
  expect_equal(nested[[1]], c("f1.png", "f2.png"))
})


# --- what each installed model accepts ----------------------------------------

test_that("the listing says what each model takes, in measure()'s words", {
  # `text` and `image` and nothing else, because those are the two modalities
  # measure() acts on. Naming `audio` or `tools` here would invite a call that
  # fails later rather than at the point of choosing.
  tb <- fake_tags(c("a", "b"), c("text", "text, image"))
  out <- fathom:::format_model_table(tb)
  expect_match(out[1], "text$")
  expect_match(out[2], "text, image$")

  # The columns before it stay aligned, since a short modality on one row
  # would otherwise pull the name and size columns in on the other.
  expect_equal(
    regexpr("text", out[1], fixed = TRUE),
    regexpr("text", out[2], fixed = TRUE)
  )
})

test_that("a listing with no modalities column still prints, and quietly", {
  # A tibble from a checkpoint or a mock written before this column existed.
  # Reaching for it with `$` would warn here, where a data frame would not, so
  # the guard asks whether the name is present instead.
  tb <- fake_tags(c("a", "b"))
  tb$modalities <- NULL
  expect_silent(out <- fathom:::format_model_table(tb))
  expect_length(out, 2L)
  expect_false(any(grepl("NA", out)))
})

test_that("the listing pays the per-model call and nothing else does", {
  # ollama_tags() serves the listing branch, the model-validation branch and
  # check_backend(), and measure() runs the third before every corpus. A fetch
  # there would be paid on runs that print nothing, so it lives in
  # show_models() instead and the tibble tags returns carries four columns. The
  # server is answered from a stub at the HTTP layer, so ollama_tags() runs as
  # written rather than being replaced.
  asked <- 0L
  testthat::local_mocked_bindings(
    ollama_modalities = function(host, names, timeout = 5L) {
      asked <<- asked + 1L
      rep("text", length(names))
    },
    .package = "fathom"
  )
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(
      body = list(
        models = list(
          list(
            name = "a:1b",
            size = 1e9,
            details = list(parameter_size = "1B", quantization_level = "Q4_K_M")
          ),
          list(
            name = "b:2b",
            size = 2e9,
            details = list(parameter_size = "2B", quantization_level = "Q4_K_M")
          )
        )
      )
    )
  })

  tb <- fathom:::ollama_tags("http://localhost:11434")
  expect_equal(names(tb), c("name", "size", "params", "quant"))
  expect_equal(tb$name, c("a:1b", "b:2b"))
  expect_equal(asked, 0L)

  expect_message(
    fathom:::show_models(tb, "http://localhost:11434"),
    "Models on"
  )
  expect_equal(asked, 1L)
})

test_that("the server's word becomes the one modalities takes", {
  # vision is what /api/show says and image is what measure() checks, so the
  # listing prints the second. completion, tools and thinking are dropped
  # because none of them changes what can be sent.
  expect_equal(fathom:::modality_words(c("completion", "tools")), "text")
  expect_equal(
    fathom:::modality_words(c("completion", "vision", "tools", "thinking")),
    "text, image"
  )
  # audio is declared by two models and measure() cannot send it, so naming it
  # would invite a call that fails later rather than at the point of choosing.
  expect_equal(
    fathom:::modality_words(c("completion", "vision", "audio")),
    "text, image"
  )
  # A server that returns nothing leaves text, which every model has.
  expect_equal(fathom:::modality_words(character(0)), "text")
})

test_that("a host is read the way Ollama's own client reads it", {
  nh <- fathom:::normalize_host
  expect_identical(nh("0.0.0.0"), "http://0.0.0.0:11434")
  expect_identical(nh("localhost:11434/"), "http://localhost:11434")
  expect_identical(nh("http://server.lan"), "http://server.lan")
  expect_identical(
    nh("https://ollama.example.org/"),
    "https://ollama.example.org"
  )
  # Set and empty counts as unset, rather than as a host with no name.
  withr::local_envvar(OLLAMA_HOST = "")
  expect_identical(
    ollama_backend("m", validate = FALSE)$host,
    "http://localhost:11434"
  )
})
