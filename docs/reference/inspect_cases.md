# Read the documents that fired on an item

Pulls the documents that endorsed an item, with their full text, for
checking whether the model was right.

## Usage

``` r
inspect_cases(
  results,
  item,
  docs = NULL,
  ...,
  text = NULL,
  id = NULL,
  which = c("endorsed", "uncertain"),
  target = NULL,
  n = 6L,
  chars = NULL,
  file = NULL,
  seed = NULL
)
```

## Arguments

- results:

  A `fathom_results` object from
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
  or
  [`read_checkpoints()`](https://www.alcocerjj.com/fathom/reference/read_checkpoints.md).

- item:

  A string naming the item to inspect.

- docs:

  A character vector of documents, or the data frame passed to
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md).
  Optional when
  [`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
  was given a character vector, since the documents are stored on the
  results in that case. Default is `NULL`.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- text, id:

  Strings naming the text and identifier columns, if `docs` is a data
  frame. `text` is then required, and `id` defaults to the row position.

- which:

  A string choosing the documents, `"endorsed"` (the default) for those
  where `target` won or `"uncertain"` for the closest calls regardless
  of answer. `"uncertain"` has nothing to sort on when every document
  came back at the same extreme, and says so rather than returning
  corpus order under that label. To read the documents an item did not
  fire on, ask for the other label as `target` rather than for the
  complement of this one.

- target:

  A string naming the label counted as endorsement. Defaults to the
  first.

- n:

  An integer giving the number of documents to show. Default is 6.

- chars:

  An integer giving the characters each document is truncated to, or
  `NULL` (the default) for full text. Warns.

- file:

  A string giving a path to write markdown to instead of printing, or
  `NULL` (the default) to print.

- seed:

  An integer giving a seed for the sample, or `NULL` (the default). It
  reaches this draw only, and the session's random stream is put back
  afterwards.

## Value

Invisibly, a tibble of the selected cases, one row per document, in the
order they are printed. Printing is the point of this function and the
returned object is a convenience, so it carries the columns the printout
drew on rather than a curated set.

- `id` is the document identifier.

- `item` is the item inspected, the same value on every row.

- `label` is `target`, the same value on every row, since the rows for
  other labels are dropped before selection.

- `prob_renorm` is the probability on `target` for that document,
  rescaled over the labels. This is what `which` filtered on and what
  the printout shows.

- `prob_other` is the probability the model placed on tokens that were
  none of the labels, for that document.

- `text` is the document, joined from `docs`. Truncated if `chars` was
  set. Named `text` even when the source column was called something
  else.

Any other columns the results carry travel through untouched, including
anything passed to `keep` in
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md) and
the remaining diagnostic columns. A run that found no qualifying cases
returns a tibble with no rows.

## The header line

Three numbers, in this order. How many cases are printed, how many
qualified before sampling down to `n`, and how large the corpus was. The
middle one is the one to read, since it says whether the printed cases
are all of them or a sample. Reporting the corpus size alone would read
as though every document not shown had failed the filter, which is the
opposite of what it means.

## Endorsement by label count

On two labels it is the 0.5 cut on `prob_renorm`, which is what
[`item_report()`](https://www.alcocerjj.com/fathom/reference/item_report.md)
counts as `endorse_renorm`. Above two the labels compete for one pool, a
winner at 0.3 clears no cut, and endorsement is instead the label that
took the most mass, which is what
[`item_report()`](https://www.alcocerjj.com/fathom/reference/item_report.md)
counts as `n_won`.

`"uncertain"` is a rank rather than a threshold in both cases, so it
commits to no cut point. On two labels it sorts by distance from 0.5.
Above two it sorts by `margin`, the gap between the winning label and
the runner-up, so the first rows are the closest calls.

## Excerpts versus full text

Full text is the default. The passage that triggered an item is often
outside a keyword-centered window, so an excerpt can read as a false
negative when the item was working. Setting `chars` truncates and warns.

## See also

[`item_report()`](https://www.alcocerjj.com/fathom/reference/item_report.md)
for the summary these documents sit behind.
[`coding_sample()`](https://www.alcocerjj.com/fathom/reference/coding_sample.md)
for drawing cases to hand to a coder rather than reading a few.

## Examples

``` r
# Results from a real run ship with the package, so this needs no server.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
res <- baked$res_six

# measure() was given a data frame, so the documents are passed again.
inspect_cases(res, "category", parlasent, text = "sentence", id = "id",
  target = "1", n = 2, seed = 1)
#> 
#> # category  (2 of 17 endorsed, from 100 documents)
#> 
#> **Question**
#> 
#> ```
#> Where does this sentence fall on the six-point sentiment scale?
#> 
#> 1 = entirely or predominantly negative
#> 2 = ambiguous, or a mixture of sentiments, leaning negative
#> 3 = no sentiment of its own, leaning negative
#> 4 = no sentiment of its own, leaning positive
#> 5 = ambiguous, or a mixture of sentiments, leaning positive
#> 6 = entirely or predominantly positive
#> ```
#> 
#> ---
#> 
#> **uk_017** | P(1) = 1.0000, other = 0.00e+00, 26 words
#> 
#> Apparent conflicting activities (where two announcements seem to contradict) and stating ambitious targets that are then not achieved were aligned to eroding trust with the public.” 
#> 
#> ---
#> 
#> **uk_040** | P(1) = 0.9834, other = 2.42e-07, 13 words
#> 
#> We know that the Government have paid £33 million to settle a lawsuit. 
```
