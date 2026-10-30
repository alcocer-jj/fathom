# Configure an Ollama backend

Bundles connection and sampling settings into one object, passed down
whole so parameters cannot drift between call sites. Drift can create
context size differences between calls, defeating the prefix cache and
forcing Ollama to reload the model.

Scoring reads log probabilities, which need an Ollama server at version
0.12.11 or later.
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
checks for them before a run and stops when none come back.

## Usage

``` r
ollama_backend(
  model = NULL,
  ...,
  keep_alive = "24h",
  timeout = 600,
  top_logprobs = 20L,
  host = NULL,
  modalities = c("text", "image"),
  validate = TRUE
)
```

## Arguments

- model:

  A string giving the model name as Ollama knows it, e.g.
  `"gemma3:27b"`. The full tag is required. Shorthand is refused. Omit
  it entirely to list what the server has installed.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- keep_alive:

  A string setting how long Ollama holds the model after a call. The
  default, `"24h"`, is long because a corpus run over one model should
  not pay a reload between documents. A loop over several models on one
  machine wants a short value instead, since each one called stays
  resident for the whole window and the cost arrives as memory pressure
  rather than as an error.

- timeout:

  A number giving the request timeout in seconds for generation calls. A
  cold prefill on a very long document can run minutes. Discovery and
  validation use their own shorter timeout. Default is 600.

- top_logprobs:

  An integer giving the number of alternatives to return at each
  position. Ollama caps this at 20. Lower it only if every label
  reliably appears in the top-k. Default is 20.

- host:

  A string giving the server URL. Defaults to `OLLAMA_HOST` or
  localhost. A host given with no scheme and no port takes port 11434,
  the way Ollama's own client reads `OLLAMA_HOST`.

- modalities:

  A character vector of what the server accepts, which is `text`,
  `image` or both. A fact about the server rather than the model.
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  refuses a column naming something absent here, locally and before any
  call, because Ollama returns HTTP 200 for a field it does not
  recognize. Audio and video are not among them.
  [`prepare_audio()`](https://alcocer-jj.github.io/fathom/reference/prepare_audio.md)
  turns a recording into a transcript, and
  [`prepare_video()`](https://alcocer-jj.github.io/fathom/reference/prepare_video.md)
  turns a video into frames plus a transcript when it has sound, which
  reach the model as `text` and `image`. Default is
  `c("text", "image")`.

- validate:

  A logical value indicating whether to contact the server and confirm
  `model` is installed. `FALSE` constructs blind with no network call,
  which is what examples, documentation builds, and constructor tests
  use. Default is `TRUE`.

## Value

An object of class `fathom_backend`, a list of eight settings. It holds
no connection, so it is cheap to build and safe to keep.

- `model` is the tag calls are sent to. Recorded on the results as the
  `model` attribute, so a corpus says which model produced it.

- `host` is the server address, normalized to include a scheme.

- `modalities` holds what this server accepts.
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  refuses a column naming something absent here, before any call,
  because Ollama answers HTTP 200 to a field it does not recognize and
  an undeclared column would otherwise be discarded in silence.

- `top_logprobs` says how many alternatives are requested at the answer
  position. Ollama caps this at 20. A label whose token falls outside
  the returned set is scored as absent, so `prob` is a lower bound and
  `prob_other` absorbs the difference.

- `think` is always `FALSE`, and not an argument. Reasoning is disabled
  on every call, because scoring reads generated position 1 and a model
  that reasons first puts something else there.

- `keep_alive` says how long the server holds the model in memory
  between calls. Long enough that a corpus run does not pay reload cost.

- `timeout` is the number of seconds before a single call is abandoned.
  A call that times out returns a failed row rather than ending the run.

- `validated` says whether the constructor confirmed the model is
  installed. `FALSE` when built with `validate = FALSE`, which skips the
  check rather than failing it.

Called with no model name it prints what is installed and stops, which
doubles as a way to confirm the server is running.

## Omitting `model`

The call contacts the server, prints the models installed there, and
errors. It does not return `NULL` or a partial object, so
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
and
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
never receive a backend with no model attached.

## Temperature and seed

Neither is exposed. Temperature is fixed at 0, where the returned
probabilities are the model's unmodified posterior. Scoring reads the
distribution at generated position 1 and discards the token that would
have been drawn from it, so temperature rescales the logits being
measured without changing what is read.

At temperature 0 there is no sampling, and identical calls return
byte-identical token sequences. A seed has nothing to control.

## See also

[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md),
which takes the backend, and
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md),
which can probe labels against it.

## Examples

``` r
ollama_backend("gemma3:27b", validate = FALSE)
#> Ollama backend
#> 
#> Model         gemma3:27b
#> Host          http://localhost:11434
#> Logprobs      top 20
#> Modalities    text, image
#> 
#> Note     Not validated. Built with validate = FALSE, so the model name has not
#>          been checked against the server.
```
