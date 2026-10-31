# Compute endorsement rates across a range of thresholds

Computes the endorsement rate per item at each cut point, which shows
how much of an item's rate is a property of the documents and how much
is a property of where the cut falls. `pct_extreme_renorm` in
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
gives the short answer in one number, since a probability at or beyond
0.01 and 0.99 sits on the same side of every cut between them.

## Usage

``` r
threshold_sensitivity(
  results,
  target = NULL,
  thresholds = seq(0.1, 0.9, by = 0.1)
)
```

## Arguments

- results:

  A `fathom_results` object from
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  or
  [`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md).

- target:

  A string naming the label counted as endorsement. Defaults to the
  first.

- thresholds:

  A numeric vector of cut points to evaluate. Default is
  `seq(0.1, 0.9, by = 0.1)`.

## Value

A tibble with one row per item and threshold, items in instrument order
and thresholds ascending within an item. The message printed alongside
is a ranking rather than a table and stays widest swing first.

- `item` is the question name.

- `threshold` is the cut point, one row per value in `thresholds`.

- `n` counts the documents scored on this item. The same on every row of
  an item, since the cut point changes which documents count as
  endorsing and not how many were asked.

- `n_endorse` counts the documents whose `prob_renorm` for `target`
  reached the cut.

- `endorse` is `n_endorse` over the documents with a `prob_renorm` to
  cut. A document where no label appeared has none, so it leaves this
  denominator, and `endorse` is `n_endorse` over `n` only when no such
  document exists.
  [`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
  treats `endorse_renorm` the same way. This is the column to read down,
  and the spread from its smallest to largest value within an item is
  what the message ranks on.

## More than two labels

This assumes a two-label instrument. Thresholding one label's
probability asks whether that label is present, which is a question only
a two-label instrument answers. On an instrument of three or more labels
the labels compete for one pool of probability, so a label can win at
0.3 and lose at 0.6, and its rate at a series of cut points describes
the arithmetic rather than the corpus. On a scored instrument the labels
are levels of a scale, and a cut point on one level is not a measurement
of anything.

The function does not refuse, because a cut point is the whole subject
here and there is no other quantity for it to report. It runs on
whatever you pass and the figures will be arithmetically correct.

## See also

[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
for the winner distribution, and for `mean_expected` on a scored
instrument, which needs no cut.

## Examples

``` r
# Results from a real run ship with the package, so this needs no server.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
threshold_sensitivity(baked$res_indep, target = "YES")
#> 
#> Note     Endorsement swing across the range, widest first.
#>          mixed     0.02
#>          negative  0.02
#>          positive  0.00
#> # A tibble: 27 × 5
#>    item     threshold     n n_endorse endorse
#>    <chr>        <dbl> <int>     <int>   <dbl>
#>  1 negative       0.1   100        47    0.47
#>  2 negative       0.2   100        47    0.47
#>  3 negative       0.3   100        47    0.47
#>  4 negative       0.4   100        47    0.47
#>  5 negative       0.5   100        47    0.47
#>  6 negative       0.6   100        46    0.46
#>  7 negative       0.7   100        46    0.46
#>  8 negative       0.8   100        46    0.46
#>  9 negative       0.9   100        45    0.45
#> 10 positive       0.1   100        42    0.42
#> # ℹ 17 more rows
```
