# Synthetic top_logprobs payloads.
#
# Hand-written rather than captured, so every edge case is exact and
# constructible without a server. Real captures serve a different purpose,
# catching a change in what Ollama returns. Grab a handful of those separately
# and add them as regression fixtures.
#
# Tokens are passed as a character vector rather than as argument names, because
# R does not process \uXXXX escapes inside backticks and some fixtures need
# accented tokens.

lp <- function(tokens, logprobs) {
  stopifnot(length(tokens) == length(logprobs))
  unname(Map(function(t, l) list(token = t, logprob = l), tokens, logprobs))
}

bin_instrument <- function() {
  instrument(list(q1 = "Is the text about weather?"), labels = c("SI", "NO"))
}

acc_instrument <- function() {
  instrument(
    list(q1 = "\u00bfEl texto habla del clima?"),
    labels = c("SI", "NO"),
    cue = "\n\nResponde con exactamente una palabra: SI o NO"
  )
}

# An instrument whose labels collide at their first character, so it can only be
# built through the probe. This is the one shape where a label carries more than
# one string, and nobody typed the second one. Used for the aggregation tests,
# which need a label with two matchable strings and can no longer get one by
# declaration.
probed_instrument <- function() {
  i <- 0L
  testthat::local_mocked_bindings(
    ollama_generate = function(prompt, backend, num_ctx = NULL, image = NULL) {
      i <<- i + 1L
      tok <- c("ECON", "EDUC")[min(i, 2L)]
      list(
        response = tok,
        logprobs = list(list(
          top_logprobs = list(list(token = tok, logprob = -0.01))
        ))
      )
    },
    .package = "fathom",
    .env = parent.frame()
  )
  suppressMessages(instrument(
    list(q1 = "Q?"),
    labels = c("economia", "educacion"),
    # stub_backend() rather than a second hand-built object. The two had
    # drifted apart by one field, and a shared constructor is what stops that
    # happening again.
    backend = stub_backend(),
    exact = TRUE
  ))
}

# Labels that split under a real tokenizer, for testing the matching rule.
# BUDGET and HOUSING give B and H, so a fragment of either is unambiguous.
split_instrument <- function() {
  instrument(list(q1 = "Which category?"), labels = c("BUDGET", "HOUSING"))
}

five_instrument <- function() {
  instrument(
    list(q1 = "How positive is the text?"),
    labels = c("1", "2", "3", "4", "5")
  )
}

#' Set an instrument to exact matching without a backend
#'
#' score_labels() branches on `exact_match`, and the only public route to
#' setting it is a backend and a probe call. This sets it directly so the branch
#' can be tested offline. It is the one place in the suite that reaches into an
#' object's internals, named so that it is obvious and so that a rename breaks
#' here rather than in a test body.
as_exact <- function(b) {
  b$exact_match <- TRUE
  b
}

# --- the cases ---------------------------------------------------------------

# Clean. One label dominant, the other far down, and nothing in the payload that
# opens either. Deliberately free of fragments, so a test of ordinary scoring
# does not depend on how fragments are treated.
fx_clean <- function() {
  lp(c("SI", "NO", "Res", "**"), c(-0.0001, -12.46, -15.11, -16.20))
}

# The same, plus a bare S. Whether that is a fragment of SI or the opening of
# another word is not decidable from a synthetic payload, which is exactly why
# it lives in its own fixture rather than inside the clean one.
fx_clean_frag <- function() {
  lp(c("SI", "S", "NO", "Res"), c(-0.0001, -9.97, -12.46, -15.11))
}

# Saturated. The losing label fell out of the top-k entirely.
fx_saturated <- function() {
  lp(c("SI", "S", "Res", "**"), c(-0.00002, -11.0, -15.1, -16.2))
}

# Broken template. No label at position 1 at all.
fx_no_label <- function() {
  lp(c("Aqu\u00ed", "Bas", "A", "Este"), c(-0.025, -4.30, -4.60, -6.84))
}

# High leaked mass. The compound-question signature.
fx_leaky <- function() {
  lp(c("Res", "SI", "neut", "NO"), c(-0.143, -2.01, -9.92, -11.2))
}

# Both labels split, which is what a real tokenizer does to BUDGET and HOUSING.
# Neither fragment equals its label, so exact matching finds nothing here.
fx_split <- function() {
  lp(c("BU", "HO", "Res", "**"), c(-0.05, -3.10, -9.20, -12.0))
}

# Five labels, one dominant.
fx_five <- function() {
  lp(c("3", "4", "2", "5", "1"), c(-0.10, -2.40, -3.10, -6.00, -7.20))
}
