# Collapse several coders into one reference sheet

Takes the completed sheets and returns one sheet, so
`validate_sample(coder_reference(ana, luis, bea), res)` composes without
changing a signature. Read `$ceiling` from
[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
given the same sheets first. It says whether collapsing is honest, since
a high alpha means the coders were interchangeable and a low one means
the reference is a consensus that never existed.

## Usage

``` r
coder_reference(
  ...,
  method = c("majority", "unanimous", "soft"),
  target = NULL
)
```

## Arguments

- ...:

  Two or more completed coding sheets, as data frames or paths. Name
  them to name the coders.

- method:

  A string naming how to collapse a row. `"majority"` takes the most
  common label, `"unanimous"` requires every coder to agree, `"soft"`
  keeps the disagreement as a proportion instead of resolving it.
  Default is `"majority"`.

- target:

  A string naming the label `"soft"` reports the proportion of. Defaults
  to the first of the two in sorted order, and the choice is written to
  the sheet so it is never implicit. Ignored by the other methods.

## Value

A tibble, one row per document-item pair that any coder saw. It carries
`id`, `item`, `human_label`, and `n_coders` and `n_agree` beside them.

Under `"soft"` the label column is replaced by `human_prop`, with
`human_target` naming the label the proportion is of and `n_target` in
place of `n_agree`.
[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
branches on which of the two arrived.

## Collapse methods

`"majority"` writes the label most coders chose. Where two or more
labels tie for the most it writes `NA` and names those rows, rather than
breaking the tie. A rule that broke it would invent a coding no coder
produced.

`"unanimous"` writes the label only where every coder who saw the row
chose it, and `NA` otherwise. It warns, since the rows it discards are
the hard ones, so what survives is easier than the draw was.

A row only one coder saw has a majority of one and is trivially
unanimous. `n_coders` on the returned sheet is what separates those from
a real agreement.

## The soft method

`"soft"` writes no label. It writes `human_prop`, the share of coders
who chose `target`, smoothed as `(n_target + 1) / (n_coders + 2)` so two
coders who both said yes give 0.75 rather than 1. That is the honest
reading of two observations, and it keeps the target off the boundaries,
which matters because `prob_renorm` reaches exactly zero and a log score
would then be infinite.
[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
scores it with Brier for the same reason.

It needs exactly two labels. Above two the honest soft target is a
vector of proportions rather than one number, and reporting one label
against the rest would leave a Brier over two categories where a label
sheet is scored over all of them. Refused rather than silently rescaled.

## See also

[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md),
whose `$ceiling` says whether collapsing is honest and which reads the
result.

## Examples

``` r
ana <- data.frame(
  id = c("1", "2", "3"), item = "q",
  human_label = c("YES", "NO", "YES")
)
luis <- data.frame(
  id = c("1", "2", "3"), item = "q",
  human_label = c("YES", "YES", "NO")
)
bea <- data.frame(
  id = c("1", "2", "3"), item = "q",
  human_label = c("YES", "NO", "NO")
)
coder_reference(ana = ana, luis = luis, bea = bea)
#> # A tibble: 3 × 5
#>   id    item  human_label n_coders n_agree
#>   <chr> <chr> <chr>          <int>   <int>
#> 1 1     q     YES                3       3
#> 2 2     q     NO                 3       2
#> 3 3     q     NO                 3       2
```
