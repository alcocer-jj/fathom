# Regression tests. Each defect was found by reading the sources, reproduced
# offline, and pinned by a test written to fail against the code that carried
# it. Each names the file and the mechanism rather than only the symptom, so a
# later reader can tell a real regression from a deliberate change.
#
# They pass now. A failure here is a defect coming back, not one being reported
# for the first time.
#
# Three tests encode a decision rather than a correction, and are marked. The
# behavior each covers was defensible either way. What was not defensible was
# reaching it silently, so each test records which way the decision went and a
# later change has to argue with it rather than drift past.

# --- identity ---------------------------------------------------------------

test_that("two ids that sanitize to one filename do not share a checkpoint", {
  # checkpoint.R ckpt_path(). gsub("[^A-Za-z0-9._-]", "_", id) maps a space, a
  # slash, and an underscore onto the same byte, so the second document reads
  # the first one's checkpoint, finds every item current, is never sent, and the
  # first document is returned twice under its own id.
  local_offline()
  ck <- withr::local_tempdir()
  d <- tibble::tibble(
    id = c("NYT 2011-01-01", "NYT/2011-01-01"),
    body = c("alpha", "beta")
  )

  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  )

  expect_length(list.files(ck, pattern = "\\.rds$"), 2L)
  expect_setequal(unique(res$probs$id), d$id)
})

test_that("duplicate ids are refused rather than silently doubled", {
  # measure.data.frame() never checks the id column for uniqueness. With
  # checkpointing on this reproduces the collision above. With it off the
  # results carry duplicate id-item-label keys, which turns pivot_wider() into
  # list columns in as_wide() and validate_sample().
  local_offline()
  d <- tibble::tibble(id = c("x", "x"), body = c("alpha", "beta"))

  expect_error(
    quiet_measure(
      d,
      stub_backend(),
      ab_instrument(),
      text = "body",
      id = "id",
      num_ctx = NULL
    ),
    "unique"
  )
})

# --- argument validation ----------------------------------------------------

test_that("an unknown item in `items` reports the item, not a type error", {
  # measure.R. `text` and `keep` are both validated; `items` is not. A typo
  # empties `hashes`, every document is skipped, res is a 0x0 tibble, and
  # `sum(!res$probs$call_ok)` evaluates `!NULL`.
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))

  err <- expect_error(
    quiet_measure(
      d,
      stub_backend(),
      ab_instrument(),
      text = "body",
      id = "id",
      num_ctx = NULL,
      items = "typo"
    )
  )
  expect_no_match(conditionMessage(err), "invalid argument type", fixed = TRUE)
  expect_match(conditionMessage(err), "typo")
})

test_that("ollama_backend() rejects a zero-length top_logprobs", {
  # backend.R. `if (top_logprobs > 20L)` on NULL is a length-zero condition.
  expect_error(
    ollama_backend("m", top_logprobs = NULL, validate = FALSE),
    "top_logprobs"
  )
})

test_that("an exclusive stem may be wrapped in a list", {
  # instrument.R. `stem_key <- if (is.character(items)) items[[1]] else items`
  # passes a list into `[[`. The items documentation presents a list as the
  # general form, so this is reachable from a fair reading.
  b <- instrument(
    list("primary"),
    labels = c("A", "B"),
    exclusive = TRUE,
    medium = "text"
  )
  expect_s3_class(b, "fathom_instrument")
  expect_match(b$items[[1]], "PRIMARY subject")
})

test_that("a `keep` column named after a results column is refused", {
  # measure.R. `keep` was checked for existence in `x` and never against the
  # schema measure() produces. A column named label, prob, call_ok, winner, or
  # any of the other twenty reached bind_cols() beside its namesake, both came
  # back suffixed as label...2 and label...6, and every downstream $label then
  # read NULL. The failure surfaced in as_wide() or an audit join with nothing
  # pointing back at the argument that caused it. The bake worked around this by
  # carrying ground truth as `coded`.
  local_offline()
  d <- tibble::tibble(
    id = c("p", "q"),
    body = c("one", "two"),
    label = c("yes", "no")
  )

  err <- expect_error(
    quiet_measure(
      d,
      stub_backend(),
      ab_instrument(),
      text = "body",
      id = "id",
      num_ctx = NULL,
      keep = "label"
    )
  )
  expect_match(conditionMessage(err), "label")
  expect_no_match(conditionMessage(err), "not found", fixed = TRUE)

  # A typo is still reported as a missing column rather than as a name that
  # happens not to be reserved, which is why the existence check runs first.
  err2 <- expect_error(
    quiet_measure(
      d,
      stub_backend(),
      ab_instrument(),
      text = "body",
      id = "id",
      num_ctx = NULL,
      keep = "labl"
    )
  )
  expect_match(conditionMessage(err2), "not found")
})

test_that("`keep` naming the same column twice is refused", {
  # measure.R. dat[i, keep, drop = FALSE] with a repeated name carries the
  # column twice and bind_cols() suffixes both copies, which is the same failure
  # a reserved name produces by a different route. Neither the existence check
  # nor the schema check sees it, since the name exists and is not reserved.
  local_offline()
  d <- tibble::tibble(
    id = c("p", "q"),
    body = c("one", "two"),
    headline = c("first", "second")
  )

  err <- expect_error(
    quiet_measure(
      d,
      stub_backend(),
      ab_instrument(),
      text = "body",
      id = "id",
      num_ctx = NULL,
      keep = c("headline", "headline")
    )
  )
  expect_match(conditionMessage(err), "more than once")
  expect_match(conditionMessage(err), "headline")

  # Named once it is still carried, which is what the check must not break.
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    keep = "headline"
  )
  # A kept column is per document, so it rides in $calls rather than repeating
  # down every label row.
  expect_true("headline" %in% names(res$calls))
})

test_that("the reserved set tracks the schema it is derived from", {
  # measure.R. reserved_cols() reads its names off fail_rows() and ITEM_COLS
  # rather than listing them, because a hand-kept list is the same class of
  # defect the check above exists to catch, silent and one edit away. This is
  # what holds the derivation honest.
  #
  # It also pins the correspondence the derivation rests on. score_labels() and
  # fail_rows() must produce the same columns, or bind_rows() over a corpus
  # mixing a failed document with a successful one gives a ragged frame. A
  # column added to one and not the other shows here rather than on the first
  # run that happens to fail.
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  b <- ab_instrument()

  res <- quiet_measure(
    d,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  # The split is the specification. Everything the flat frame carried is either
  # in one of the two frames or in DROP_COLS, and nothing is in both.
  expect_setequal(
    union(names(res$probs), names(res$calls)),
    setdiff(fathom:::reserved_cols(b), fathom:::DROP_COLS)
  )
  expect_equal(
    intersect(setdiff(names(res$probs), c("id", "item")), names(res$calls)),
    character(0)
  )
  expect_setequal(
    names(fathom:::fail_rows(b)),
    setdiff(fathom:::reserved_cols(b), c("id", "item", "prompt_hash", "model"))
  )
})

test_that("as_wide() refuses a `keep` column that varies within a document", {
  # as_wide.R. The third `keep` defect and the only one in this function. It
  # cannot fail the way measure()'s did, since as_wide() builds its frame with
  # select() rather than bind_cols() and selecting an existing column suffixes
  # nothing. What an item-level column does instead is join the pivot's implicit
  # grouping, giving a row per distinct value rather than the one row per
  # document the return contract promises, each with a single item filled. Under
  # the default drop_incomplete the frame then empties, which reads as a corpus
  # that produced nothing rather than as an argument that was wrong.
  #
  # `item` is used rather than prob_other or winner because a stub returns the
  # same distribution on every call, so those are constant across the corpus and
  # do not reproduce it. `item` varies within a document by construction.
  local_offline()
  d <- tibble::tibble(
    id = c("p", "q"),
    body = c("one", "two"),
    date = c("2011-01-01", "2011-02-02")
  )
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    keep = "date"
  )
  n_docs <- length(unique(res$probs$id))

  # The widening needs an instrument of more than one item, so the fixture this
  # rests on is asserted rather than assumed.
  expect_gt(length(unique(res$probs$item)), 1L)

  # A document-level column is still carried, which is what the check must not
  # break. Judged on the frame that reaches the pivot rather than on the raw
  # results, so a column made constant by the target filter still passes.
  expect_equal(nrow(suppressMessages(as_wide(res, keep = "date"))), n_docs)
  expect_no_error(suppressMessages(as_wide(res, keep = "label")))

  err <- expect_error(suppressMessages(as_wide(res, keep = "item")))
  expect_match(conditionMessage(err), "vary within a document")
  expect_match(conditionMessage(err), "item")

  # The same refusal ahead of the pivot either way, since drop_incomplete only
  # decides whether the widened frame comes back sparse or empty.
  expect_error(
    suppressMessages(as_wide(res, keep = "item", drop_incomplete = FALSE)),
    "vary within a document"
  )
})

test_that("the documented schema matches the one measure() returns", {
  # measure.R. Its @return enumerates every column of both frames by name, which
  # makes the block a contract. R CMD check does not compare documentation
  # against code, so a column added to fail_rows() or renamed in score.R would
  # leave the reference page quietly wrong, and the vignettes link to it rather
  # than restating the schema.
  #
  # PROB_COLS, CALL_COLS and reserved_cols() all read the live schema, so they
  # are the side that cannot drift. The names below are the documented ones,
  # transcribed. They exist to be compared against them.
  b <- ab_instrument()

  documented <- c(
    # keys, in both frames
    "id",
    "item",
    # $probs, per label
    "label",
    "prob_renorm",
    "prob",
    "present",
    "call_ok",
    # $calls, per document and item
    "error",
    "truncated",
    "top1_token",
    "top1_prob",
    "prob_other",
    "all_absent",
    "margin",
    "expected_score",
    "prompt_hash"
  )

  # The two constants are the split, so the documentation is checked against
  # them rather than against a hand list that would drift the same way.
  expect_setequal(documented, union(fathom:::PROB_COLS, fathom:::CALL_COLS))

  # And against the live flat schema, which is what fail_rows() and score.R
  # still produce below the split. The three agree exactly. measure() writes no
  # column the schema does not name, and a `keep` column is the caller's rather
  # than the schema's.
  expect_setequal(
    documented,
    setdiff(fathom:::reserved_cols(b), fathom:::DROP_COLS)
  )
  expect_equal(anyDuplicated(documented), 0L)

  # And against the roxygen itself where the sources are reachable, which is the
  # case under devtools::test() and not under R CMD check, since the latter runs
  # from a copy without R/. Each column is a bullet opening with its name in
  # backticks, so the first code span on a bullet line is the column.
  src <- "../../R/measure.R"
  skip_if_not(file.exists(src), "package sources not reachable from here")
  lines <- readLines(src, warn = FALSE)
  from <- grep("^#' @return A `fathom_results`", lines)[1]
  to <- grep("^#' @export", lines)
  to <- to[to > from][1]
  bullets <- grep("^#' - `", lines[from:to], value = TRUE)
  in_roxygen <- sub("^#' - `([^`]+)`.*$", "\\1", bullets)

  expect_setequal(in_roxygen, documented)
})

test_that("the documented field set matches what ollama_backend() builds", {
  # backend.R. Its @return names all eight settings, which makes the block a
  # contract that R CMD check does not check. Built with validate = FALSE so
  # nothing here needs a server.
  bk <- ollama_backend("gemma3:27b", validate = FALSE)

  documented <- c(
    "model",
    "host",
    "top_logprobs",
    "think",
    "keep_alive",
    "timeout",
    "modalities",
    "validated"
  )

  expect_setequal(names(bk), documented)
  expect_equal(anyDuplicated(documented), 0L)

  # Two of the eight are documented as fixed rather than configurable, so they
  # are asserted rather than left to a reader's assumption. Reasoning is off on
  # every call because scoring reads generated position 1, and validate = FALSE
  # skips the installed-model check rather than failing it.
  expect_false(bk$think)
  expect_false(bk$validated)
})

# --- scoring ----------------------------------------------------------------

test_that("winner is NA when no label appeared at position 1", {
  # score.R. which.max() on an all-zero vector returns 1, so an all-absent call
  # reports the first label as the winner. Every other column on that row is
  # correctly NA or FALSE. Internal consumers recompute the winner through
  # slice_max(), so no report is wrong, but the column is exported.
  b <- ab_instrument()
  lp <- list(
    list(token = "Res", logprob = -0.01),
    list(token = "Aqui", logprob = -5)
  )
  s <- fathom:::score_labels(lp, b)

  expect_true(all(!s$present))
  expect_true(all(is.na(s$winner)))
})

test_that("a blank probe token is not registered as a scoring variant", {
  # instrument.R probe_labels(). trimws(token %||% "") can be "", and
  # startsWith(x, "") is TRUE, so the forced check passes and "" is appended to
  # normalized. The exact branch of scoring has no nzchar() guard, so under
  # `exact = TRUE` any whitespace-only token in the top-k then scores as that
  # label. Probing alone no longer switches that branch on, so the guard is
  # checked under both rules.
  local_offline(stub_generate_seq(list(" ", "NO")))

  # A blank token is no discovery, so the label falls through to the existing
  # "could not probe" path. That warning is the correct outcome and is asserted
  # rather than left to leak out of the run.
  expect_warning(
    b <- instrument(
      list(q = "Q?"),
      labels = c("YES", "NO"),
      backend = stub_backend()
    ),
    "could not probe"
  )
  expect_false("" %in% b$normalized$YES)

  s <- fathom:::score_labels(
    list(
      list(token = "  ", logprob = -0.02),
      list(token = "Res", logprob = -3)
    ),
    b
  )
  expect_equal(sum(s$prob), 0)

  # The same probe under `exact = TRUE`. The blank token falls back to the label
  # string here too, so the recorded variants hold no "" for a whitespace token
  # to match exactly.
  local_offline(stub_generate_seq(list(" ", "NO")))
  expect_warning(
    e <- instrument(
      list(q = "Q?"),
      labels = c("YES", "NO"),
      backend = stub_backend(),
      exact = TRUE
    ),
    "could not probe"
  )
  expect_false("" %in% e$normalized$YES)
  s <- fathom:::score_labels(
    list(
      list(token = "  ", logprob = -0.02),
      list(token = "Res", logprob = -3)
    ),
    e
  )
  expect_equal(sum(s$prob), 0)
})

test_that("probing accepts a label whose token is already recorded", {
  # instrument.R. forced <- startsWith(up, norm_firsts) tested against the
  # uppercased label NAMES rather than everything recorded for the label. A
  # second probe of an already-probed instrument, where the model repeats the
  # token it gave the first time, was refused as a probe that failed to
  # separate.
  local_offline(stub_generate_seq(list("ECON", "EDUC")))

  b <- suppressMessages(instrument(
    list(q = "Q?"),
    labels = c("economia", "educacion"),
    backend = stub_backend(),
    exact = TRUE
  ))
  expect_s3_class(b, "fathom_instrument")
  expect_setequal(b$normalized$economia, c("ECONOMIA", "ECON"))

  # Re-probing the built instrument must not error on its own recorded token.
  local_offline(stub_generate_seq(list("ECON", "EDUC")))
  expect_no_error(suppressMessages(fathom:::probe_labels(b, stub_backend())))
})

# --- audit and validation ---------------------------------------------------

test_that("one unscorable row does not make the whole Brier score NA", {
  # validate.R. d$brier is NA for a row whose prob_renorm is NA, and mean() and
  # weighted.mean() are called without na.rm, so a single all-absent record
  # empties the calibration table. coding_sample() excludes those rows, so a
  # sheet it wrote is safe; validate_sample() accepts any data frame.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  d <- tibble::tibble(id = as.character(1:4), body = rep("doc", 4))
  b <- instrument(
    list(a = "Q?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  )
  res <- quiet_measure(
    d,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  res$probs$prob_renorm[res$probs$id == "1"] <- NA_real_

  sheet <- tibble::tibble(
    id = as.character(1:4),
    item = "a",
    human_label = c("mid", "mid", "lo", "hi")
  )
  v <- suppressMessages(suppressWarnings(
    validate_sample(sheet, res, calibration = TRUE)
  ))

  # The table has to be there for the check below to mean anything, since
  # `$calibration` is returned only on request.
  expect_false(is.null(v$calibration))
  expect_false(any(is.na(v$calibration$brier)))
})

test_that("item_report survives a lost instrument attribute", {
  # audit.R scored_report(). scores[.data$label] is NULL[...] when the attribute
  # is gone, and the mutate produces a zero-length column. item_levels() already
  # defends against the same absence; this path does not. Reachable through
  # bind_rows() and as_tibble(), which drop attributes.
  local_offline(stub_generate(c("mid", "lo", "hi"), c(-0.1, -2, -3)))
  d <- tibble::tibble(id = as.character(1:4), body = rep("doc", 4))
  b <- instrument(
    list(a = "Q?"),
    labels = c("lo", "mid", "hi"),
    scores = c(lo = 0, mid = 1, hi = 2)
  )
  res <- quiet_measure(
    d,
    stub_backend(),
    b,
    text = "body",
    id = "id",
    num_ctx = NULL
  )
  attr(res, "instrument") <- NULL

  expect_no_error(suppressMessages(item_report(res)))
})

test_that("a carried column named text does not empty the audit output", {
  # audit.R. inspect_cases() and coding_sample() both left_join the document
  # frame and then read $text. keep = "text" suffixes both sides, so $text
  # returns NULL. coding_sample() errors. inspect_cases() prints a header
  # claiming N cases and then prints nothing, which is the worse of the two
  # failures.
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), text = c("first doc", "second doc"))
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "text",
    id = "id",
    num_ctx = NULL,
    keep = "text"
  )

  out <- suppressWarnings(capture.output(
    sel <- inspect_cases(res, "a", d, text = "text", id = "id")
  ))
  expect_true(any(grepl("first doc", out, fixed = TRUE)))

  expect_no_error(suppressMessages(
    coding_sample(res, d, text = "text", id = "id", n = 2)
  ))
})

# --- decisions rather than corrections ---------------------------------------

test_that("DECISION. items= returns the same items for every document", {
  # measure.R. With a checkpoint present the skip branch returns every stored
  # item, and the compute branch returns the stored items plus the new ones.
  # With no checkpoint only the requested items come back. On a partly cached
  # corpus the result is ragged, and as_wide(drop_incomplete = TRUE) then drops
  # the documents that ran fresh with a message reading as a model failure.
  #
  # Either filter merged to `wanted` on the way out while keeping the full
  # record on disk, or say in the documentation that `items` restricts what is
  # computed rather than what is returned. This test assumes the first.
  local_offline()
  ck <- withr::local_tempdir()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))

  invisible(quiet_measure(
    d[1, ],
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck
  ))
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL,
    checkpoint = ck,
    items = "a"
  )

  per_doc <- tapply(res$probs$item, res$probs$id, function(x) sort(unique(x)))
  expect_length(unique(per_doc), 1L)
})

test_that("declaring alternative spellings is refused, and says why", {
  # Reverses the earlier decision to preserve them in the exclusive branch. A
  # ten-model sweep across six tokenizer families, on labels from two to eleven
  # characters and on both instrument shapes, found no model emitting a
  # position-1 token that was not a fragment of a label. Prefix matching covers
  # the case the mechanism existed for, so the declaration is gone from the API.
  #
  # What survives is the record the probe writes under `exact = TRUE`, which is
  # what lets colliding labels be told apart and which no user types.
  expect_error(
    instrument(
      "primary",
      labels = list(SI = c("SI", "S\u00cd"), NO = "NO"),
      definitions = c(SI = "afirmativo", NO = "negativo"),
      exclusive = TRUE,
      medium = "text"
    ),
    "not a list"
  )
  expect_error(
    instrument(list(q = "Q?"), labels = c(SI = "S\u00cd", NO = "NO")),
    "names do not match"
  )
})

test_that("check_backend's documented failures match its FALSE paths", {
  # backend.R. The @return block names each failure, which makes it a contract,
  # and R CMD check does not compare documentation against code. This function
  # has carried exactly that defect before. test-backend.R's own header records
  # it, the block enumerating six failures returning FALSE while the body
  # warned and returned TRUE, with nothing in the suite reaching the real
  # function to notice.
  #
  # Counting rather than pairing by name. The \item labels are prose and the
  # messages are different prose, so nothing matches textually without a marker
  # convention the package does not have. A path added or deleted is caught. A
  # renamed one is not, and closing that costs a tag beside every return.
  src <- "../../R/backend.R"
  skip_if_not(file.exists(src), "package sources not reachable from here")
  lines <- readLines(src, warn = FALSE)

  # Anchored on the definition rather than on `@export`, which this function no
  # longer carries. It is internal, and the contract between the block that
  # documents its failures and the body that produces them is unchanged by
  # that.
  from <- grep("^#'   `FALSE` covers", lines)[1]
  to <- grep("^check_backend <- function", lines)
  to <- to[to > from][1]
  blk <- paste(lines[from:to], collapse = "\n")
  documented <- regmatches(blk, gregexpr("\\\\item\\{[^}]+\\}", blk))[[1]]

  body <- lines[to:length(lines)]
  paths <- which(trimws(body) == "return(FALSE)")

  expect_equal(length(documented), length(paths))
  expect_equal(length(paths), 9L)

  # And none of them returns without saying which failure it was, which is what
  # "named in the message rather than collapsed into one" promises.
  spoke <- vapply(
    paths,
    function(i) any(grepl("message(", body[max(1L, i - 14L):i], fixed = TRUE)),
    logical(1)
  )
  expect_true(all(spoke))
})

# --- provenance --------------------------------------------------------------

test_that("DECISION. nothing carries or prints the package version", {
  # measure.R stamped utils::packageVersion("fathom") onto every results
  # object and into every checkpoint payload, utils.R stamped it onto every
  # validation object, checkpoint.R read it back, and the results header
  # printed it beside the model, the ceiling and the run time. Removed on
  # instruction rather than found broken, so this records which way it went
  # and a later change argues with it.
  #
  # The printed check is not vacuous. The header carried `| fathom 0.0.0.9000`
  # before the removal, which the pattern below matches, and nothing else the
  # header or the probs tibble prints carries two dots between digits.
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = NULL
  )

  # The name as well as the printed line, since renaming the attribute would
  # satisfy a test written on the header alone.
  expect_null(attr(res, "fathom"))
  expect_null(attr(fathom:::flat_results(res), "fathom"))
  expect_null(attr(fathom:::stamp_provenance(list(a = 1)), "fathom"))

  out <- capture.output(print(res))
  expect_false(any(grepl("[0-9]+\\.[0-9]+\\.[0-9]+", out)))
})

test_that("DECISION. reasoning is off and is not an argument", {
  # The constructor documented `think` as always FALSE and stored whatever it
  # was given, so TRUE reached every call. Scoring reads generated position 1,
  # which is the design rather than a setting, so the argument is gone rather
  # than guarded, and the field stays FALSE for the request body and for every
  # checkpointed backend.
  expect_false("think" %in% names(formals(ollama_backend)))
  expect_error(
    ollama_backend("gemma3:27b", validate = FALSE, think = TRUE),
    "think",
    fixed = TRUE
  )
  expect_false(ollama_backend("gemma3:27b", validate = FALSE)$think)
})

test_that("DECISION. console output carries no package prefix", {
  # Every line the package printed opened with `[fathom] `, and its wrapped
  # lines were indented nine spaces to sit under the tag. Removed on
  # instruction. A run's own lines and the printed backend are both checked,
  # since a tag surviving in either would read as the old convention.
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  msgs <- testthat::capture_messages(suppressWarnings(measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L
  )))
  expect_true(length(msgs) > 0L)
  expect_false(any(grepl("[fathom]", msgs, fixed = TRUE)))

  # Reasoning is off on every call and nothing sets it, so the printed backend
  # no longer shows it either.
  shown <- capture.output(print(stub_backend()))
  expect_false(any(grepl("[fathom]", shown, fixed = TRUE)))
  expect_false(any(grepl("think", shown, fixed = TRUE)))
})

test_that("DECISION. a run opens with a header and closes with a summary", {
  # The run printed a line of `|`-separated counts, then the context ceiling
  # and the backend check on lines of their own. Laid out on instruction as a
  # title over aligned fields, the way the author's other packages report.
  local_offline()
  # Pinned to lines. An interactive session would draw a bar instead, which
  # cli shows only after two seconds and never as a message, so the test would
  # pass or fail on where it was run.
  testthat::local_mocked_bindings(
    use_bar = function(verbose) FALSE,
    .package = "fathom"
  )
  d <- tibble::tibble(id = sprintf("p%02d", 1:12), body = rep("one two", 12))
  run <- function(...) {
    paste(
      testthat::capture_messages(suppressWarnings(measure(
        d,
        stub_backend(),
        ab_instrument(),
        text = "body",
        id = "id",
        num_ctx = 4096L,
        ...
      ))),
      collapse = ""
    )
  }

  out <- run()
  expect_match(out, "^Constrained-label measurement\n\n")
  expect_match(out, "Model               m\n", fixed = TRUE)
  expect_match(out, "Context Ceiling     4,096 tokens\n", fixed = TRUE)
  expect_match(out, "Documents           12\n", fixed = TRUE)
  expect_match(out, "Items               2 (a, z)\n", fixed = TRUE)
  expect_match(out, "Labels              2 (YES, NO)\n", fixed = TRUE)
  expect_match(out, "Checkpoints         off\n", fixed = TRUE)
  # Outside an interactive session progress is a line every ten documents.
  expect_match(out, "Measuring           10/12   83%", fixed = TRUE)
  expect_match(
    out,
    "Done                12 processed, 0 skipped, 0 item failures\n",
    fixed = TRUE
  )
  expect_match(out, "Elapsed             ", fixed = TRUE)

  # Quiet keeps the progress and the summary and drops the header.
  quiet <- run(verbosity = "quiet")
  expect_no_match(quiet, "Constrained-label measurement", fixed = TRUE)
  expect_match(quiet, "Done                ", fixed = TRUE)
})

test_that("DECISION. a printed object opens with a title, not a class tag", {
  # Each print method opened with `<fathom_...>` and a line of `|`-separated
  # counts. Laid out on instruction as a title over aligned fields, the way a
  # run reports.
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L
  )
  backend <- printed(stub_backend())
  inst <- printed(ab_instrument())
  results <- printed(res)
  expect_match(backend, "^Ollama backend Model m Host ")
  expect_match(inst, "^Instrument Shape independent items Items 2 \\(a, z\\)")
  expect_match(results, "^Measurement results Documents 2 Items 2 Labels 2")
  expect_match(
    results,
    "Tip Next, item_report(), then inspect_cases().",
    fixed = TRUE
  )
  for (out in c(backend, inst, results)) {
    expect_no_match(out, "<fathom_", fixed = TRUE)
  }
})

test_that("DECISION. preview() has a title, and rules around the prompt only", {
  # The output opened with a rule and a lowercase item line, and closed the
  # label table with another rule. Laid out on instruction as a title over
  # fields, with the two rules kept around the prompt alone, since they show
  # where the string the model receives starts and ends.
  out <- capture.output(p <- preview(ab_instrument(), text = "one two"))
  expect_identical(out[1], "Prompt preview")
  expect_true("Item          a, 1 of 2" %in% out)
  expect_true("Scoring       generated position 1" %in% out)
  rules <- which(out == strrep("-", 70))
  expect_length(rules, 2L)
  # Between the rules is exactly the string preview() returns.
  expect_identical(
    paste(out[(rules[1] + 1):(rules[2] - 1)], collapse = "\n"),
    p
  )
  expect_true("Label         Counts as" %in% out)
  expect_true("YES           YES" %in% out)
})

test_that("the threshold note lists one aligned line per item", {
  local_offline()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  res <- quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L
  )
  msg <- paste(
    testthat::capture_messages(threshold_sensitivity(res)),
    collapse = ""
  )
  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
  expect_true(
    "Note     Endorsement swing across the range, widest first." %in% lines
  )
  expect_true(any(grepl("^ {9}a  [0-9]\\.[0-9]{2}$", lines)))
  expect_true(any(grepl("^ {9}z  [0-9]\\.[0-9]{2}$", lines)))
  expect_no_match(msg, "|", fixed = TRUE)
})

test_that("a resume that skips every document leaves one blank line", {
  # The summary opened with a blank line of its own, which doubled the one under
  # the header whenever nothing printed between them. The default num_ctx also
  # drew a note that only repeated the header's Context Ceiling line.
  local_offline()
  ck <- withr::local_tempdir()
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  invisible(quiet_measure(
    d,
    stub_backend(),
    ab_instrument(),
    text = "body",
    id = "id",
    num_ctx = 4096L,
    checkpoint = ck
  ))
  out <- paste(
    testthat::capture_messages(
      testthat::with_mocked_bindings(
        measure(
          d,
          stub_backend(),
          ab_instrument(),
          text = "body",
          id = "id",
          checkpoint = ck
        ),
        use_bar = function(...) FALSE,
        .package = "fathom"
      )
    ),
    collapse = ""
  )
  expect_match(out, "Done                0 processed, 2 skipped", fixed = TRUE)
  expect_match(out, "(from the saved run)\n", fixed = TRUE)
  expect_no_match(out, "\n\n\n", fixed = TRUE)
  expect_no_match(out, "Ignoring the supplied", fixed = TRUE)
})

test_that("verbose output never draws a bar", {
  # A bar would draw over the per-item lines, whatever the session.
  expect_false(use_bar(verbose = TRUE))
})

test_that("DECISION. the retry tip appears only on a checkpointed run", {
  # Re-running retries only the failed items when checkpoints hold the rest.
  # Without them a re-run starts again from the first document, so the tip
  # would be wrong there.
  local_offline(gen = function(prompt, backend, num_ctx = NULL, image = NULL) {
    stop("no answer")
  })
  d <- tibble::tibble(id = c("p", "q"), body = c("one", "two"))
  run <- function(...) {
    paste(
      testthat::capture_messages(suppressWarnings(measure(
        d,
        stub_backend(),
        ab_instrument(),
        text = "body",
        id = "id",
        num_ctx = 4096L,
        ...
      ))),
      collapse = ""
    )
  }

  plain <- run()
  expect_match(plain, "4 item failures", fixed = TRUE)
  expect_no_match(plain, "Tip", fixed = TRUE)

  ck <- withr::local_tempdir()
  kept <- run(checkpoint = ck)
  expect_match(kept, "4 item failures", fixed = TRUE)
  expect_match(
    kept,
    "Tip                 Re-run to retry failures only.",
    fixed = TRUE
  )
})

test_that("the context ceiling goes to the header rather than printing", {
  pre <- new.env()
  expect_silent(
    n <- resolve_num_ctx(
      4096L,
      c("a b", "c"),
      ab_instrument(),
      stub_backend(),
      report = pre
    )
  )
  expect_identical(n, 4096L)
  expect_identical(pre$context, "4,096 tokens")

  pre <- new.env()
  expect_silent(
    resolve_num_ctx(
      NULL,
      c("a", "b"),
      ab_instrument(),
      stub_backend(),
      report = pre
    )
  )
  expect_identical(pre$context, "none")
})

test_that("fields align their values and hang wrapped lines under them", {
  x <- fmt_fields(list(
    Model = "m",
    Skipped = NULL,
    Context = paste(rep("word", 30), collapse = " ")
  ))
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  expect_identical(lines[1], "Model         m")
  # A NULL field is left out, so a header can list one that applies sometimes.
  expect_false(any(grepl("Skipped", lines, fixed = TRUE)))
  expect_match(lines[2], "^Context       word")
  expect_true(all(startsWith(lines[-(1:2)], strrep(" ", 14L))))
  expect_true(all(nchar(lines) <= 80L))
})

test_that("a numbered step hangs its wrapped lines under its own text", {
  x <- fmt_lines(
    "Two known causes.",
    paste("1.", paste(rep("word", 30), collapse = " ")),
    prefix = "",
    indent = ""
  )
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  expect_match(lines[2], "^1\\. word")
  expect_true(all(startsWith(lines[-(1:2)], "   ")))
})

test_that("a field keeps a value that fits as written and stacks a vector", {
  x <- fmt_fields(list(Drawn = c("a    10 of 20", "zz   10 of 20")))
  # Inner spacing survives, so values a caller aligned stay aligned.
  expect_identical(
    strsplit(x, "\n", fixed = TRUE)[[1]],
    c("Drawn         a    10 of 20", "              zz   10 of 20")
  )
})

test_that("progress names its verb and its unit", {
  expect_identical(
    progress_line(3, 10, 12.3, verb = "Transcribing", unit = "file"),
    "Transcribing   3/10   30%  12.3 s/file  ETA 1.4 min"
  )
  # The defaults keep the verb and two spaces, as prepare_audio() and
  # prepare_video() print them.
  expect_match(progress_line(10, 12, 0.1), "^Measuring  10/12   83%")
})

test_that("measure() starts its progress in the header's value column", {
  expect_match(
    progress_line(10, 12, 0.1, pad = HEADER_WIDTH),
    "^Measuring {11}10/12   83%"
  )
  # The bar starts at the same column and is shorter by the nine columns it
  # moved, so the line keeps the length it had.
  line <- progress_line(
    100,
    100,
    0.7,
    bar = TRUE,
    pad = HEADER_WIDTH,
    cells = BAR_CELLS
  )
  expect_identical(regexpr("[", line, fixed = TRUE)[[1]], HEADER_WIDTH + 1L)
  expect_identical(nchar(line), nchar(progress_line(100, 100, 0.7, bar = TRUE)))
})

test_that("a progress line is a message a caller can drop on its own", {
  # A message, so suppressMessages() still silences it. Its own class, so
  # prepare_video() can drop the line prepare_audio() prints for each video
  # and keep that function's notes.
  m <- tryCatch(progress_message("x"), message = function(m) m)
  expect_s3_class(m, c("fathom_progress", "message"))
  expect_identical(conditionMessage(m), "x\n")
})

test_that("a note keeps its label in a column and hangs lines under the text", {
  x <- fmt_note(
    "Warning",
    paste(rep("word", 40), collapse = " "),
    "A second paragraph."
  )
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  # A blank line opens the note, so consecutive notes stay apart.
  expect_identical(lines[1], "")
  expect_match(lines[2], "^Warning  word")
  # Every later line, the second paragraph included, starts under the text.
  expect_true(all(startsWith(lines[-(1:2)], strrep(" ", 9L))))
  expect_true(all(nchar(lines) <= 80L))
  expect_identical(fmt_note("Tip", "x"), "\nTip      x")
})

test_that("coder_reference stores the call rather than evaluating it", {
  # validate.R finish_reference(). Two attributes to stamp conditionally, so
  # both were routed through do.call(stamp_provenance, ...) to keep one call
  # site. do.call() evaluates its argument list unless told not to, `call` in
  # that list is match.call()'s language object, and the recorded call was
  # therefore run inside stamp_provenance()'s frame rather than stored. Every
  # coder_reference() call naming anything local died on object not found, and
  # the direct call it replaced had never evaluated anything, since an
  # argument that is only assigned is never forced.
  mk <- function(lab) {
    tibble::tibble(id = c("1", "2"), item = "q", human_label = lab)
  }
  # Held in a local, which is what the evaluated call could not resolve. A
  # fixture referenced by a bare name would not have reproduced this.
  held <- list(a = mk(c("YES", "NO")), b = mk(c("YES", "YES")))

  r <- suppressMessages(coder_reference(ana = held$a, luis = held$b))

  # Unevaluated. Before the fix this line was never reached, since building the
  # object raised.
  expect_true(is.call(attr(r, "call")))
})
