test_that("clean binary scoring is correct", {
  b <- bin_instrument()
  s <- fathom:::score_labels(fx_clean(), b)

  expect_equal(nrow(s), 2L)
  expect_equal(s$label, c("SI", "NO"))
  expect_equal(s$prob[s$label == "SI"], exp(-0.0001), tolerance = 1e-10)
  expect_true(all(s$present))
  expect_equal(s$winner[1], "SI")
  expect_lt(s$prob_other[1], 1e-4)
})

test_that("a bare fragment adds to the label it opens", {
  # The same payload as the clean case with an S added. Under exact matching
  # that S is leaked mass; under prefix matching it counts toward SI. Which
  # reading is right is not decidable here, so what is asserted is the
  # arithmetic and the size, 4.7e-5, rather than a claim about the token.
  b <- bin_instrument()
  s <- fathom:::score_labels(fx_clean_frag(), b)

  expect_equal(
    s$prob[s$label == "SI"],
    exp(-0.0001) + exp(-9.97),
    tolerance = 1e-10
  )
  expect_lt(exp(-9.97), 1e-4)

  sx <- fathom:::score_labels(fx_clean_frag(), as_exact(b))
  expect_equal(sx$prob[sx$label == "SI"], exp(-0.0001), tolerance = 1e-10)
})

test_that("a fragment counts as the label it opens", {
  # The case the whole matching rule exists for. Neither token equals its
  # label, so exact matching would score both at zero and report all the mass
  # as leaked, which is the silent failure.
  b <- split_instrument()
  s <- fathom:::score_labels(fx_split(), b)

  expect_equal(s$prob[s$label == "BUDGET"], exp(-0.05), tolerance = 1e-10)
  expect_equal(s$prob[s$label == "HOUSING"], exp(-3.10), tolerance = 1e-10)
  expect_true(all(s$present))
  expect_equal(s$winner[1], "BUDGET")
})

test_that("exact matching scores the same fragments at zero", {
  # What the same distribution looks like under the older rule. as_exact() is
  # the suite's one reach into an object, and it is there because the public
  # route to this branch is a backend and a network call.
  s <- fathom:::score_labels(fx_split(), as_exact(split_instrument()))

  expect_equal(sum(s$prob), 0)
  expect_true(all(!s$present))
  expect_gt(s$prob_other[1], 0.99)
})

test_that("exact matching reads every recorded string, not just the name", {
  # Under `exact = TRUE` a label carries its own name plus whatever the probe
  # saw the model write. Checking the name alone would drop the discovered
  # token, which is the only string the model actually emits.
  b <- probed_instrument()
  s <- fathom:::score_labels(lp(c("ECON", "EDUC"), c(-0.01, -12.0)), b)

  expect_gt(s$prob[s$label == "economia"], 0.98)
  expect_true(s$present[s$label == "economia"])
})

test_that("a missing label gives probability zero, not an error", {
  b <- bin_instrument()
  s <- fathom:::score_labels(fx_saturated(), b)

  expect_equal(s$prob[s$label == "NO"], 0)
  expect_true(is.infinite(s$logprob[s$label == "NO"]))
  expect_false(s$present[s$label == "NO"])
  expect_true(s$present[s$label == "SI"])
})

test_that("all labels absent is detectable", {
  b <- bin_instrument()
  s <- fathom:::score_labels(fx_no_label(), b)

  expect_true(all(!s$present))
  expect_equal(sum(s$prob), 0)
  expect_gt(s$prob_other[1], 0.99)
})

test_that("leaked mass is reported, not hidden", {
  b <- bin_instrument()
  s <- fathom:::score_labels(fx_leaky(), b)

  # The model wanted to say something outside the label set.
  expect_gt(s$prob_other[1], 0.8)
  expect_equal(s$top1_token[1], "Res")
  # Renormalization still picks a winner among the labels.
  expect_equal(s$winner[1], "SI")
  expect_gt(s$prob_renorm[s$label == "SI"], 0.9)
})

test_that("two recorded strings for one label aggregate by log-sum-exp", {
  # The label name and the probed token both appear in the top-k. Their mass is
  # summed rather than either one winning, so the total exceeds the larger.
  b <- probed_instrument()
  s <- fathom:::score_labels(
    lp(c("ECONOMIA", "ECON", "EDUC"), c(-0.70, -0.70, -6.0)),
    b
  )

  expect_gt(s$prob[s$label == "economia"], exp(-0.70))
  expect_true(s$present[s$label == "economia"])
})

test_that("scoring is generic over K", {
  b <- five_instrument()
  s <- fathom:::score_labels(fx_five(), b)

  expect_equal(nrow(s), 5L)
  expect_equal(s$winner[1], "3")
  expect_equal(sum(s$prob_renorm), 1, tolerance = 1e-9)
})

test_that("probabilities never exceed one", {
  b <- bin_instrument()
  # logprob of exactly 0 can exp() to marginally above 1 after float error
  s <- fathom:::score_labels(lp(c("SI", "NO"), c(0, -20)), b)
  expect_lte(s$prob[s$label == "SI"], 1)
  expect_lte(s$top1_prob[1], 1)
  expect_gte(s$prob_other[1], 0)
})

test_that("an unrelated token is leaked mass under either rule", {
  # Prefix matching widens what counts as a label. It does not make everything
  # count. Res opens neither SI nor NO and is reported as leaked.
  b <- bin_instrument()
  s <- fathom:::score_labels(lp(c("Res", "SI"), c(-0.10, -2.5)), b)

  expect_gt(s$prob_other[1], 0.85)
  expect_equal(s$top1_token[1], "Res")
  expect_equal(s$winner[1], "SI")
})

test_that("logsumexp handles the empty case", {
  expect_equal(fathom:::logsumexp(numeric(0)), -Inf)
  expect_equal(fathom:::logsumexp(c(0)), 0)
  expect_equal(fathom:::logsumexp(c(log(0.5), log(0.5))), 0, tolerance = 1e-12)
})
