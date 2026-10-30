# Offline harness for the paths that look like they need a server and do not.
#
# measure(), the checkpoint loop, and every audit function reach the network
# through exactly one function, ollama_generate(). Replacing it and short
# circuiting check_backend() puts the whole pipeline under test with no calls.
#
# Everything here is deterministic. A test that wants a particular scoring
# outcome states the tokens it wants back rather than hoping a model produces
# them.

# A backend that satisfies the class check and is never dialled.
stub_backend <- function(model = "m") {
  structure(
    list(
      model = model,
      keep_alive = "24h",
      timeout = 1,
      think = FALSE,
      top_logprobs = 20L,
      host = "http://stub",
      # Mirrors what ollama_backend() builds. measure() reads this before
      # any call, so
      # a stub missing it would exercise the `%||%` fallback rather than the
      # object under test.
      modalities = c("text", "image"),
      validated = FALSE
    ),
    class = "fathom_backend"
  )
}

# Build a generate() replacement returning a fixed top-k.
#
# `tokens` and `logprobs` are the position-1 distribution every call returns.
# `prompt_eval_count` is what the truncation flag and the num_ctx measurement
# read.
stub_generate <- function(
  tokens = c("YES", "NO"),
  logprobs = c(-0.01, -4.6),
  prompt_eval_count = 100L
) {
  force(tokens)
  force(logprobs)
  # `image` is accepted and ignored, and the signature has to match
  # ollama_generate()'s. When the argument was added and this was not, every
  # call through the stub failed with an unused argument, every row came back
  # call_ok = FALSE, and 70 tests failed on empty results. A stub is a contract
  # with the function it replaces.
  function(prompt, backend, num_ctx = NULL, image = NULL) {
    list(
      response = tokens[1],
      prompt_eval_count = prompt_eval_count,
      prompt_eval_duration = 1e6,
      wall_ms = 1,
      logprobs = list(list(
        top_logprobs = unname(Map(
          function(t, l) list(token = t, logprob = l),
          tokens,
          logprobs
        ))
      ))
    )
  }
}

# Return a different first token on each successive call, for probe tests where
# each label has to be answered separately.
stub_generate_seq <- function(tokens) {
  i <- 0L
  # `image` is accepted and ignored, and the signature has to match
  # ollama_generate()'s. When the argument was added and this was not, every
  # call through the stub failed with an unused argument, every row came back
  # call_ok = FALSE, and 70 tests failed on empty results. A stub is a contract
  # with the function it replaces.
  function(prompt, backend, num_ctx = NULL, image = NULL) {
    i <<- i + 1L
    tok <- tokens[[min(i, length(tokens))]]
    list(
      response = tok,
      logprobs = list(list(
        top_logprobs = list(list(token = tok, logprob = -0.01))
      ))
    )
  }
}

# Install the stubs for the duration of the calling test.
local_offline <- function(gen = stub_generate(), env = parent.frame()) {
  testthat::local_mocked_bindings(
    ollama_generate = gen,
    check_backend = function(...) TRUE,
    .package = "fathom",
    .env = env
  )
  invisible(TRUE)
}

# A two-item binary instrument, the shape most of these tests use.
ab_instrument <- function() {
  instrument(list(a = "A?", z = "Z?"), labels = c("YES", "NO"))
}

# Run measure() with the noise suppressed. Every test here asserts on the
# returned object rather than on the progress reporting.
quiet_measure <- function(...) {
  suppressMessages(suppressWarnings(measure(...)))
}

# What print() writes, as one line with every run of whitespace squeezed to one
# space. Notes wrap, so a phrase checked in printed output can land across a
# line break. The checks are about wording, not about where a line breaks.
printed <- function(x) {
  gsub("\\s+", " ", paste(utils::capture.output(print(x)), collapse = " "))
}
