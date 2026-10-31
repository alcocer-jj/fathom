# Ask again with the labels in another order

Measures every document again, or a seeded sample of them, with the
labels shown in reverse and, on three or more labels, in one shuffled
order, and compares the answers with the ones the run already holds.
Every call in a run sees the labels in the order the instrument
declared, so nothing in the run can show whether that order moved its
answers.

A prompt shows the labels in order in two places, the cue and, on an
exclusive instrument, the listing of definitions under the question.
Both are rebuilt for each order. Matching and any probed tokens carry
over by label name rather than being probed again, so only the
presentation moves.

## Usage

``` r
order_sensitivity(
  results,
  docs = NULL,
  ...,
  text = NULL,
  id = NULL,
  image = NULL,
  items = NULL,
  n = NULL,
  level = c("nominal", "ordinal"),
  labels = NULL,
  ci = c("none", "bootstrap"),
  conf_level = 0.95,
  bootstrap_n = 2000L,
  seed = 1234L
)
```

## Arguments

- results:

  A `fathom_results` object from
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
  or
  [`read_checkpoints()`](https://www.alcocerjj.com/fathom/reference/read_checkpoints.md).
  It supplies the instrument, the backend and the context ceiling the
  calls are made with.

- docs:

  A character vector, or the data frame passed to
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md).
  Default is `NULL`, which uses the documents a character-vector run
  stored on the results.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- text, id:

  Strings naming the text and identifier columns, if `docs` is a data
  frame. `id` defaults to the row position, as in
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md).
  `text` may be left out when `image` is given and the run sent no text.

- image:

  A string naming the column in `docs` that holds each document's media,
  as passed to
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md).
  A run that sent media needs it here too, or every call asks about a
  prompt the run never sent. Default is `NULL`.

- items:

  A character vector restricting the audit to specific items, or `NULL`
  (the default) for all of them.

- n:

  An integer giving the documents drawn per item, from those with a
  label in the run, or `NULL` to ask about all of them. An item with
  fewer than `n` uses all it has. Default is `NULL`. Drawing 50 puts the
  standard error of an agreement near 0.9 at about 0.04.

- level:

  A string, `"nominal"` (the default) or `"ordinal"`, deciding how
  `alpha` charges a disagreement, as in
  [`validate_sample()`](https://www.alcocerjj.com/fathom/reference/validate_sample.md).

- labels:

  A character vector giving, under `"ordinal"`, every label in scale
  order. Ignored under `"nominal"`. Default is `NULL`, which takes the
  order from the instrument's `scores`.

- ci:

  A string choosing whether to report intervals. `"none"`, the default,
  returns none and the interval columns are absent. `"bootstrap"` adds
  one bracketed column beside `agreement` and `alpha`.

- conf_level:

  A number giving the coverage of every interval, 0.95 by default. It is
  stamped on the result as the `conf_level` attribute.

- bootstrap_n:

  An integer giving the replicates for each interval. The default is a
  round two thousand, and below a hundred is refused.

- seed:

  An integer giving the seed for the shuffled order, the draw and the
  intervals, so the same call asks about the same documents. The
  session's random stream is put back on exit. `NULL` draws from the
  session's stream instead. Default is 1234.

## Value

A tibble with one row per item and order, items in instrument order and
`"reversed"` before `"shuffled"`. The label sequences are on the
`orders` attribute, the declared one first, and in the printed header.

- `item` is the question name.

- `order` is `"reversed"`, or `"shuffled"` on three or more labels.

- `n` counts the documents compared, those where the reordered call
  returned a label. A call that failed or returned none is left out and
  warned about, since an order that pushes the answer off the labels is
  itself an effect of the order.

- `agreement` is the share of those documents whose winning label is the
  same under both orders.

- `alpha` is Krippendorff's alpha with the two orders as two coders of
  the same documents. On two labels the nominal and ordinal metrics
  coincide.

`level` and `conf_level` are stamped as attributes, and `run_at` and
`call` as on the other objects here.

## What it costs

The declared order is read off `results`, so it costs no calls. Each
other order costs one call per document asked about. The default asks
about every document with a label, so the cost grows with the corpus. A
number for `n` caps it, and `n = 50` makes it 50 calls an item on two
labels and 100 on three or more.

The instrument, the backend and the context ceiling all come off
`results`, so the calls run under the model and settings of the run
being audited. Nothing is checkpointed, and the run's checkpoint
directory is never touched. A reordered prompt hashes differently, so a
call stored there would mark the item stale and replace the run's rows
with the audit's.

## A cue written by hand

The package does not rewrite wording it did not write. An exclusive
instrument with its own `cue` keeps it, and only its listing is
reordered. An instrument of items lists its labels nowhere but the cue,
so with its own `cue` there is nothing to reorder and the call is
refused.

## See also

[`threshold_sensitivity()`](https://www.alcocerjj.com/fathom/reference/threshold_sensitivity.md)
for the other choice a measurement should not depend on, where the cut
falls, and
[`validate_sample()`](https://www.alcocerjj.com/fathom/reference/validate_sample.md)
for agreement with a human rather than with the model itself.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs a running Ollama server, so it is not run here.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
order_sensitivity(baked$res_indep, parlasent, text = "sentence", id = "id")
} # }
```
