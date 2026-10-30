# Pivot results into an analysis matrix

Pivots results into one column per item, holding the probability of the
target label.

## Usage

``` r
as_wide(
  results,
  ...,
  target = NULL,
  value = c("prob_renorm", "prob", "expected_score"),
  prefix = "",
  dichotomize = FALSE,
  keep = character(0),
  drop_incomplete = TRUE
)
```

## Arguments

- results:

  A `fathom_results` object from
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  or
  [`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md).

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- target:

  A string naming the label whose probability fills each cell. Defaults
  to the first label for a multi-item instrument, and to `NULL` for an
  exclusive instrument, where every category gets its own column.

- value:

  A string naming the column that supplies the value. Defaults to
  `"prob_renorm"`, the probability conditioned on the model having
  answered with one of your labels at all. That is the quantity a human
  coder given the same categories would have produced, and it is what
  [`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
  and the expected score read, so a matrix built here can be compared
  against either without changing quantity partway through.

  `"prob"` is the raw probability, measured against every token the
  model could have written. Use it when the claim is about whether the
  model committed rather than about which label it preferred.

  `"expected_score"` requires an instrument built with `scores`, and
  returns one column per item rather than one per label, since an
  expected score is one number per document and item.

- prefix:

  A string prepended to column names. Default is `""`.

- dichotomize:

  A logical value indicating whether to threshold at 0.5 and return
  integers. Default is `FALSE`.

- keep:

  A character vector of row-level columns to carry alongside. Each must
  hold one value per document, since this returns one row per document.
  A column that varies within a document is refused rather than allowed
  to widen the pivot. Default is `character(0)`.

- drop_incomplete:

  A logical value indicating whether to drop documents missing any item.
  Default is `TRUE`.

## Value

A tibble, one row per document, carrying `id`, then any `keep` columns,
then the pivoted values. Column names take `prefix` if given.

What the value columns are depends on the arguments, and there are three
shapes.

- Naming `target` gives one column per item, holding that label's
  probability. The usual shape for an instrument of independent binary
  items, and the one a measurement model wants.

- Leaving `target` as `NULL` gives one column per label on a single-item
  instrument, or one per item and label combination on an instrument of
  several, named `item_label`. Use it when every label matters rather
  than one.

- `value = "expected_score"` gives one column per item, since an
  expected score is already one number per document and item. Requires
  an instrument built with `scores`, and cannot be combined with
  `dichotomize`.

Values are probabilities, or integers when `dichotomize = TRUE`. A
failed, truncated, or all-absent record becomes `NA`, and
`drop_incomplete` decides whether its document leaves the frame. Either
way the count is reported rather than left to be noticed.

## Normalizing across items

There is no argument to normalize across items. K independent binary
items do not form a distribution, as each probability comes from its own
softmax over its own two-token distribution. Dividing them by their sum
produces a number that looks like a proportion and is not one.

An exclusive instrument is the exception. Its K labels share one softmax
at one position, so its rows do sum to one and are comparable across
categories. This detects that from the results and defaults `target` to
`NULL`, giving one column per category.

## Exclusive results

`instrument(..., exclusive = TRUE)` produces a real distribution. Row
sums are meaningful, minus whatever leaked outside the label set. On an
exclusive instrument scored on discovered first tokens, that remainder
is mostly the gap left by the approximation, since the probability of a
first token is an upper bound on the probability of the label it opens.

## See also

[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
for the results it pivots and
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
for a summary per item.

## Examples

``` r
# Results from a real run ship with the package, so this needs no server.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
res <- baked$res_six

# An exclusive instrument gives one column per category.
head(as_wide(res, prefix = "p_"))
#> 
#> Note     Exclusive instrument. Rows are a distribution, sums 1.000 to 1.000.
#> # A tibble: 6 × 7
#>   id       p_1      p_2      p_3       p_4           p_5       p_6
#>   <chr>  <dbl>    <dbl>    <dbl>     <dbl>         <dbl>     <dbl>
#> 1 uk_001 0     0        6.65e- 6 1.000e+ 0 0.0000000147  3.17 e- 7
#> 2 uk_002 1.000 5.26e-11 0        0         0             1.28 e-10
#> 3 uk_003 0     0        0        4.87 e-10 0.00000000110 1.000e+ 0
#> 4 uk_004 0     5.54e-10 6.46e-10 1.35 e- 7 1.000         1.58 e- 6
#> 5 uk_005 0     6.08e- 8 1.00e- 1 9.00 e- 1 0.00000143    1.98 e- 6
#> 6 uk_006 1.000 0        0        0         0             1.39 e-10
head(as_wide(res, prefix = "p_", dichotomize = TRUE))
#> 
#> Note     Exclusive instrument. Rows are a distribution, sums 1.000 to 1.000.
#> # A tibble: 6 × 7
#>   id       p_1   p_2   p_3   p_4   p_5   p_6
#>   <chr>  <int> <int> <int> <int> <int> <int>
#> 1 uk_001     0     0     0     1     0     0
#> 2 uk_002     1     0     0     0     0     0
#> 3 uk_003     0     0     0     0     0     1
#> 4 uk_004     0     0     0     0     1     0
#> 5 uk_005     0     0     0     1     0     0
#> 6 uk_006     1     0     0     0     0     0
```
