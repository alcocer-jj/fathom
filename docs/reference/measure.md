# Measure documents against an instrument

Runs every item in the instrument against every document and returns the
probabilities in `$probs`, one row per document, item and label, beside
`$calls`, one row per document and item.

## Usage

``` r
measure(x, ...)

# S3 method for class 'character'
measure(x, backend, instrument, ...)

# S3 method for class 'data.frame'
measure(
  x,
  backend,
  instrument,
  ...,
  text = NULL,
  id = NULL,
  image = NULL,
  keep = character(0),
  num_ctx = "auto",
  items = NULL,
  checkpoint = NULL,
  verbosity = c("normal", "quiet", "verbose")
)
```

## Arguments

- x:

  A character vector of documents, or a data frame.

- ...:

  Passed to methods by the generic. On a method it is not used, and is
  refused if anything reaches it, since every argument after it is
  name-only and a misspelling would otherwise be swallowed.

- backend:

  An
  [`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)
  object.

- instrument:

  An
  [`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md).

- text:

  A string naming the column holding document text. Optional when
  `image` names one, so a picture with no caption is a corpus. Default
  is `NULL`.

- id:

  A string naming an identifier column. Defaults to the integer row
  index.

- image:

  A string naming a column holding media sent alongside the prompt
  rather than inside it. A character column is one picture per document.
  A list column is several, one element per document holding its paths
  in order, which is how a video window arrives. Nothing is shared
  between calls, so a run costs documents times items at close to full
  price, and a window of K frames costs K encodes on every call.
  [`prepare_image()`](https://alcocer-jj.github.io/fathom/reference/prepare_image.md)
  builds a corpus in the character shape. Default is `NULL`.

- keep:

  A character vector of additional columns to carry into the results.
  Names must be unique, and a column whose name the results already use
  is refused, since both copies would come back suffixed and the name
  would then read as `NULL`. Rename it in `x` first.

- num_ctx:

  An integer giving the context window ceiling, or `"auto"`. Left unset,
  Ollama allocates the model's full window and the cache that goes with
  it whether the prompts need it or not, which on a local machine is
  memory taken from elsewhere. Setting the ceiling to what the corpus
  needs returns it. Setting it too low truncates silently, since
  llama.cpp keeps the tail and the answer looks ordinary, and the size
  needed is not knowable by inspection because documents and questions
  both vary and the answer needs room after both.

  So `"auto"` (the default) measures it. The longest document by word
  count is sent once with no ceiling and its token count read back, then
  rounded up with headroom. An integer is used as given, for a corpus
  already known. `NULL` or `NA` sends no ceiling and lets Ollama choose,
  in which case the `truncated` column is `NA` for every row. A resumed
  run uses whatever its checkpoints were written under, overriding all
  of these.

- items:

  A character vector of item names to restrict the run to, or `NULL`
  (the default) for every item.

- checkpoint:

  A string giving the directory for per-document `.rds` files, or `NULL`
  (the default) to hold everything in memory.

- verbosity:

  A string setting how much to report. Each level is the one before it
  plus more.

  - `"quiet"` reports progress and the run summary only.

  - `"normal"` is the default. It adds the header, meaning a title over
    one line each for the model, the context ceiling, the first token
    the backend check read, and the size of the job.

  - `"verbose"` adds one line per item, with its winning label,
    probability, leaked mass, and wall time.

  Every check runs at every level. Only the reporting changes. `"quiet"`
  is for loops over several instruments against one corpus, where the
  caller states the configuration once rather than once per call. For no
  output at all, wrap the call in
  [`suppressMessages()`](https://rdrr.io/r/base/message.html). Progress
  is a bar redrawn in place in an interactive session, and a line every
  ten documents anywhere else, such as a knitted document or a log.

## Value

A `fathom_results` object. A list of two frames, with the run's
provenance on its attributes. `$probs` holds one row per document, item
and label. `$calls` holds one row per document and item, carrying
everything that describes the answer rather than any one label, so it is
stored once instead of repeating down the label rows.

### Keys, in both frames, and what a join is on

- `id` is the document identifier, from the `id` column or from the
  names of a character vector.

- `item` names which instrument question produced the row.

### `$probs`, differing down the labels of one call

- `label` is the label this row scores, in the order the instrument
  declares them.

- `prob_renorm` is the probability of this label rescaled so the labels
  sum to 1. The measure. `NA` when no label appeared at all, since there
  is nothing to condition on, and exactly 0 for a label that fell
  outside the `top_logprobs` alternatives. The raw probability, against
  every token the model could have written, is
  `prob_renorm * (1 - prob_other)`.

### `$calls`, one row per document and item

Anything named in `keep` rides here too, since a carried column
describes the document rather than any one label.

- `error` is the message when the call failed, otherwise `NA`, so a call
  succeeded exactly when `error` is `NA`.

- `truncated` is `TRUE` when the prompt came within 64 tokens of
  `num_ctx`, so the document may have lost its opening.

- `top1_token` is the token the model actually generated, whether or not
  it counts as a label.

- `prob_other` is the mass on tokens counting as no label. The
  diagnostic that says whether the question landed. It is exactly 1 when
  no label appeared anywhere in the returned distribution.

- `margin` is the gap between the top two `prob_renorm` values. What
  [`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
  sorts on to find the closest calls above two labels.

- `expected_score` is `prob_renorm` weighted by the instrument's
  `scores`. `NA` for an instrument built without them.

- `prompt_hash` is the hash of the assembled prompt. What checkpoint
  invalidation matches on, and not for reading.

### On the attributes, constant for the run

`instrument` and `backend` as given, `model`, `num_ctx` as resolved,
`exclusive`, and `run_at`.

## Prompt ordering

The document goes before the question, and items for a document run
contiguously, so Ollama's prefix cache prefills the document once per
document rather than once per item.

## Checkpointing

Checkpointing is off by default. Supply `checkpoint` to enable it. One
`.rds` per document holds the full record plus provenance, and a
document whose items are all current is skipped without a model call.
There is no separate resume function. Calling this twice against the
same directory does only what is outstanding.

Invalidation is per item, keyed on a hash of the question, cue, labels,
scaffolding, and model. Rewording one item re-extracts that item across
the corpus at question-only cost, since the first call still warms the
document cache. This is not proportionally cheap. A partial rerun still
pays the full document prefill for every row.

`scores` are the exception, and they are outside the hash on purpose.
Rescaling an ordinal instrument changes no token the model sees, so
re-extracting would be waste. The expected score is instead recomputed
from the stored probabilities against whatever instrument is in hand, on
this path and in
[`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md),
so a rescaled resume returns the new arithmetic without spending a call.

There is no schema tag. A change to scoring code that leaves prompts
unchanged will not invalidate anything. Delete the directory by hand.

## Cost of `num_ctx = "auto"`

One extra call per `measure()` invocation, not per document. Running
several instruments over the same corpus measures once each, because a
different instrument means a different prompt and therefore a different
token count. Passing an integer once the corpus is known skips it.

## Retrying failures

A failed item is stored with its reason in `error` and is therefore
stale on the next pass, so re-running retries exactly those. There is no
separate retry sweep, which would run against a cold cache.

## When the check before the first document fails

Before any document, `measure()` sends one prompt built from your
instrument and the corpus document nearest the median word count, or a
built-in question when the corpus has no text. Among other checks, the
run starts only if the first generated token is one of your labels. When
it is not, nothing is measured and the message names the token the model
opened with.

Three causes are known. The cue may not name the labels clearly enough,
which
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
shows. The chat template may open a reasoning block although reasoning
is off on every call, which is a property of the model's template rather
than of your instrument. Or the model may open with prose, a word such
as `The` or `Okay` before its answer.

Worked examples in the question, each a short document followed by its
one-word answer, are the known fix for prose. Without them Qwen3.8-27B
answered 941 of 945 tweets in full sentences in Denney and DiGiuseppe's
(2026) replication of a tweet-sentiment study, and with them its answers
could be scored. Examples become part of the instrument, so validate and
audit the instrument that carries them.

## Character input

A character vector stores the documents on the results in a `docs`
attribute, so the audit functions find them without a `docs` argument.
Ids are the vector's names when it has them, and its positions
otherwise.

A data frame does not, and `measure()` never writes the document into a
column. It reads the one you name and keeps a hash of the assembled
prompt. Carrying the text instead would put a copy of the corpus in
`$calls`, once per item, and the results are the object that gets saved.

Naming that column in `keep` carries it deliberately, and then it is a
kept column like any other rather than something the schema promises.

## References

Denney, S. and DiGiuseppe, M. (2026). JEV versus LLMs. Accuracy, cost
and calibration on seven political science replications.
arXiv:2610.06625.

## See also

[`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)
and
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
for its two inputs,
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
for the prompt it sends,
[`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md)
for a run cut short, and
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
and
[`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
for reading what comes back.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs a running Ollama server, so it is not run here.
oll <- ollama_backend("gemma4:31b")
b <- instrument(
  items = "Where does this sentence fall on the six-point sentiment scale?",
  labels = c("1", "2", "3", "4", "5", "6"),
  exclusive = TRUE
)
res <- measure(parlasent, instrument = b, backend = oll, text = "sentence",
  id = "id")
res$probs
} # }
```
