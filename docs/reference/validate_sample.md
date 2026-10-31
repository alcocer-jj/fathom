# Validate a completed coding sheet against the model

Takes the sheet
[`coding_sample()`](https://www.alcocerjj.com/fathom/reference/coding_sample.md)
wrote, with `human_label` filled in, and reports how well the model
agreed with the coder.

The sheet carries no probabilities, which is what makes it blind. This
joins back to the results on `id` and `item` to recover them, so pass
the same results object the sample came from.

[`vignette("complete_run")`](https://www.alcocerjj.com/fathom/articles/complete_run.md)
works a run end to end.

## Usage

``` r
validate_sample(
  sheet,
  results,
  ...,
  level = c("nominal", "ordinal"),
  labels = NULL,
  instrument = NULL,
  calibration = FALSE,
  coded = FALSE,
  ci = c("none", "bootstrap"),
  conf_level = 0.95,
  bootstrap_n = 2000L,
  seed = 1234L
)
```

## Arguments

- sheet:

  A data frame holding the completed coding sheet, or a string giving a
  path to the CSV. A named list of sheets scores the model against every
  coder in it and adds the ceiling and the gap. See the section below.

- results:

  A `fathom_results` object, the one the sheet was drawn from.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- level:

  A string, `"nominal"` (the default) or `"ordinal"`. It decides how a
  disagreement is charged, so it selects the proper scoring rule in
  `$calibration` as well as the alpha.

- labels:

  A character vector giving, under `"ordinal"`, every label in scale
  order. Ignored under `"nominal"`. Default is `NULL`.

- instrument:

  A `fathom_instrument`, the one the run used. Under `"ordinal"` its
  `scores` supply the scale, so `labels` need not be retyped. An
  explicit `labels` wins over it. Default is `NULL`.

- calibration:

  A logical value indicating whether to return `$calibration`, for a run
  keeping `prob_renorm` and feeding it onward. Default is `FALSE`.

- coded:

  A logical value indicating whether to return `$coded`, the sheet
  joined to the model's answer. Default is `FALSE`.

- ci:

  A string choosing whether to report intervals. `"none"`, the default,
  returns none and the interval columns are absent rather than present
  and full of `NA`. `"bootstrap"` adds one bracketed column beside each
  figure.

- conf_level:

  A number giving the coverage of every interval reported, 0.95 by
  default. It is stamped on the result as the `conf_level` attribute,
  and the print method states it under the first table that carries an
  interval.

- bootstrap_n:

  An integer giving the replicates for each interval. The default is a
  round two thousand. Below a hundred a percentile interval is reading a
  tail made of a handful of draws, and is refused.

- seed:

  An integer giving the seed for the bootstrap, so the intervals come
  out the same on every run with the same inputs. The session's random
  stream is put back on exit, so nothing drawn before this call reaches
  the intervals and this seed reaches nothing after it. `NULL` draws
  from the session's stream instead. Default is 1234.

## Value

An object of class `fathom_validation`, a list of tables. The model
tables carry `item` and, on the several-sheet path, `coder`. Under
`ci = "bootstrap"` each figure gains a bracketed `_ci` column beside it.

|  |  |  |
|----|----|----|
| component | holds | when |
| `$discrimination` | how often the model chose the coder's label | a label sheet |
| `$categories` | precision, recall and AUC for each label | a label sheet |
| `$calibration` | how honest the probabilities were | `calibration = TRUE`, or a soft sheet |
| `$ceiling` | how well the coders agreed with each other | several sheets |
| `$gap` | the model against that ceiling | several sheets |
| `$coded` | the sheet joined to the model's answer | `coded = TRUE` |

### Discrimination

One row per question, plus a pooled `(all items)` row where the
instrument has more than one.

- `n` counts the coded rows.

- `agreement` is the share of rows where the model chose the coder's
  label.

- `alpha` is Krippendorff's alpha (Hayes and Krippendorff 2007) between
  the model and the coder, under the `level` asked for.

### Categories

One row per label per question, with `label` and `n`, the rows the coder
gave that label.

- `precision` is the share of the rows the model gave the label where
  the coder chose it too.

- `recall` is the share of the rows the coder gave the label where the
  model chose it too.

- `auc` is the chance that a row the coder gave the label gets a higher
  probability for it than a row the coder did not.

All three count exact matches, so under `"ordinal"` a model one level
off is charged as a full miss. `alpha` and `rps` are the figures that
respect the order. On two labels the two `auc` rows are equal, since one
label's probability is one minus the other's.

### Calibration

One row per question, plus the pooled row, with `n` and one score column
named for the rule `level` selects.

- `brier` is squared error summed across categories, so on two labels a
  confident correct answer costs 0, a confident wrong one costs 2, and
  never committing costs 0.5.

- `rps` is the same sum on the cumulative probabilities and runs from 0
  to `k - 1`. Lower is better for both. A soft reference stays on
  `brier`.

### Ceiling

`$ceiling$alpha` is one row per question with `n_units`, the rows two or
more coders reached, and `alpha` over all coders at once.
`$ceiling$pairs` is one row per pair of coders per question, with
`coder_a`, `coder_b`, `n`, the rows both coded, `agreement` and `alpha`.
This is what the coders managed on these rows, and not a target.

### Gap

One row per question.

- `n` counts the rows the model and the coders share.

- `model_alpha` is the model's average alpha with the coders.

- `coder_alpha` is the coders' average alpha with each other on the same
  rows.

- `gap` is the first minus the second, positive where the model agreed
  with the coders more closely than they agreed with each other, and
  `gap_ci` is its interval.

### Coded

The sheet joined to the model's label and its probabilities, one row per
coded document-item, or per document-item per coder on the several-sheet
path. It is returned because a design-based correction takes it as input
(Egami et al. 2023, Angelopoulos et al. 2023).

The attributes carry `level`, `conf_level`, the number of coded rows as
`n_coded`, and on the several-sheet path the coder names as `coders`.

## Several coders

`sheet` takes a named list as well as one sheet, so
`validate_sample(list(ana = ana, luis = luis), res)` scores the model
against each coder in one call and adds `$ceiling` and `$gap`. Each
coder is scored through the one-sheet path, so no figure is computed
twice or computed differently, and the model tables gain a `coder`
column after `item`.

Coders need not have worked every row. `$ceiling$alpha` uses every row
two or more coders reached, and each pair in `$ceiling$pairs` falls back
to the rows both of them saw, so the two tables can rest on different
counts.

`$gap` resamples whatever `ci` is set to, since the gap is read through
its interval. Every replicate redraws the documents once and recomputes
both sides on that draw, because the two share the documents they rest
on.

A list of one is refused. A sheet
[`coder_reference()`](https://www.alcocerjj.com/fathom/reference/coder_reference.md)
consolidated warns when it is placed in a list, since agreeing with a
consensus is easier than agreeing with one person.

## Ordered labels

Ordered labels need `level = "ordinal"`. The default charges every
disagreement the same, which is right for unordered labels and wrong for
a scale, where two coders one step apart have agreed more than two
coders at opposite ends. Pass `labels` in scale order, least to
greatest, or an `instrument` carrying `scores`. Nothing is inferred,
because no order can be recovered from the sheets.

## A soft reference

A sheet carrying `human_prop` instead of `human_label` is scored as a
measurement and not as a classification. There is no second categorical
variable, so `$discrimination` and `$categories` are not returned. What
comes back is `$calibration`, with `brier` against the proportion,
whatever `calibration` is set to.

## References

Angelopoulos, A. N., Bates, S., Fannjiang, C., Jordan, M. I., and Zrnic,
T. (2023). Prediction-powered inference. *Science* 382(6671), 669–674.
[doi:10.1126/science.adi6000](https://doi.org/10.1126/science.adi6000)

Brier, G. W. (1950). Verification of forecasts expressed in terms of
probability. *Monthly Weather Review* 78(1), 1–3.
[doi:10.1175/1520-0493(1950)078\<0001:VOFEIT\>2.0.CO;2](https://doi.org/10.1175/1520-0493%281950%29078%3C0001%3AVOFEIT%3E2.0.CO%3B2)

Egami, N., Hinck, M., Stewart, B., and Wei, H. (2023). Using imperfect
surrogates for downstream inference. Design-based supervised learning
for social science applications of large language models. *Advances in
Neural Information Processing Systems* 36, 68589–68601.
[doi:10.52202/075280-3000](https://doi.org/10.52202/075280-3000)

Hayes, A. F., and Krippendorff, K. (2007). Answering the call for a
standard reliability measure for coding data. *Communication Methods and
Measures* 1(1), 77–89.
[doi:10.1080/19312450709336664](https://doi.org/10.1080/19312450709336664)

## See also

[`coding_sample()`](https://www.alcocerjj.com/fathom/reference/coding_sample.md)
for drawing the sheet, and
[`coder_reference()`](https://www.alcocerjj.com/fathom/reference/coder_reference.md)
for consolidating several coders into one sheet.

## Examples

``` r
# Results from a real run ship with the package, so this needs no server.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
res <- baked$res_six
sheet <- coding_sample(res, parlasent, text = "sentence", id = "id", n = 20,
  seed = 1234)
#> Blinded coding sample
#> 
#> Pairs         20 across 1 item
#> Drawn         20 of 100
#> 
#> Tip      Pass the completed sheet to validate_sample() together with these
#>          results.

# The parlasent coders, standing in for someone filling the sheet in.
six <- c("Negative", "M_Negative", "N_Neutral", "P_Neutral", "M_Positive",
  "Positive")
coded <- function(who) {
  out <- sheet
  out$human_label <- as.character(
    match(parlasent[[who]][match(sheet$id, parlasent$id)], six)
  )
  out
}
a1 <- coded("annotator1")

v <- validate_sample(a1, res, level = "ordinal", labels = as.character(1:6))
v$discrimination
#> # A tibble: 1 × 4
#>   item         n agreement alpha
#>   <chr>    <int>     <dbl> <dbl>
#> 1 category    20      0.65 0.753
v$categories
#> # A tibble: 6 × 6
#>   item     label     n precision recall   auc
#>   <chr>    <chr> <int>     <dbl>  <dbl> <dbl>
#> 1 category 1         6     1      0.667 0.887
#> 2 category 2         1     1      1     1    
#> 3 category 3         5     0.75   0.6   0.933
#> 4 category 4         1     0      0     0.842
#> 5 category 5         4     0.667  0.5   0.781
#> 6 category 6         3     0.6    1     0.980

# A second coder adds the ceiling the two set between themselves, and the
# model against it.
a2 <- coded("annotator2")
v2 <- validate_sample(list(annotator1 = a1, annotator2 = a2), res,
  level = "ordinal", labels = as.character(1:6), bootstrap_n = 200)
v2$ceiling$pairs
#> # A tibble: 1 × 6
#>   coder_a    coder_b    item         n agreement alpha
#>   <chr>      <chr>      <chr>    <int>     <dbl> <dbl>
#> 1 annotator1 annotator2 category    20       0.6 0.704
v2$gap
#> # A tibble: 1 × 6
#>   item         n model_alpha coder_alpha     gap gap_ci       
#>   <chr>    <int>       <dbl>       <dbl>   <dbl> <chr>        
#> 1 category    20       0.669       0.704 -0.0352 [-0.39, 0.29]
```
