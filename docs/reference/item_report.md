# Summarize how each item behaved

Produces a diagnostic data frame where each row is one item, or each row
is one item and label, summarizing how each item behaved across the
corpus. Each column answers a different question about whether an item
is usable.

It is purely descriptive, meaning no column here reports whether an item
is correct. A confident, low-leakage item can still be measuring
something other than the construct.
[`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
is what answers that, by showing the documents.

## Usage

``` r
item_report(results, target = NULL, order = c("declared", "size"))
```

## Arguments

- results:

  A `fathom_results` object from
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  or
  [`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md).

- target:

  A string naming the label counted as endorsement. Defaults to the
  first (e.g., "yes" in an instrument where "yes" was written first
  before "no"). Used by the endorsement shape only.

- order:

  A string setting how labels are ordered within a question.
  `"declared"`, the default, is the order the instrument wrote them,
  which for an ordered scale is the scale and which every other part of
  the package already uses. `"size"` puts the most frequent winner
  first, which is the reading for unordered categories. No effect on the
  endorsement shape, which has no label rows.

## Value

A tibble in one of three shapes, chosen as described below. Items come
back in the order the instrument declares them rather than
alphabetically, and an item present in the results but absent from the
instrument sorts after the declared ones.

### Endorsement shape

Exactly two labels and no `scores`. One row per item.

- `item` is the question name.

- `n` counts the documents scored on this item. Failed calls are
  excluded, so this can sit below the corpus size.

- `endorse_renorm` is the share of those documents where `target`
  reached 0.5 on `prob_renorm`, the probability after the labels have
  been rescaled to sum to 1. Reaching it means the label beat the other
  one. Its denominator leaves out the documents where no label appeared,
  which have no renormalized probability, and `pct_both_absent` says how
  many those were.

- `pct_extreme_renorm` is the share of renormalized probabilities at or
  beyond 0.01 and 0.99, so it asks how the on-label mass was shaped
  rather than how much of it there was. Near 1 means almost every answer
  came back at one end or the other, so these are labels in numeric
  clothing, and reporting them as degrees of confidence would claim more
  than they carry.

- `pct_both_absent` is the share of documents where no label appeared at
  position 1. That is a template or prompt failure rather than a low
  score. A broken chat template registers in `pct_off_label` rather than
  here, because the labels stay in the distribution at trace mass
  underneath whatever the template wrote first, which leaves
  `pct_both_absent` at 0 while every answer is unusable.

- `pct_label_absent` is the share of documents where at least one label
  was missing from the `top_logprobs` alternatives the backend returned,
  so it includes `pct_both_absent`. A label missing beside one that
  appeared has a probability of exactly 0 and puts its partner at
  exactly 1, so on those documents `pct_extreme_renorm` records the cap
  on alternatives rather than the model.

- `mean_prob_other` is the average probability the model placed on
  tokens that were none of your labels. Leaked mass, in the term used
  elsewhere in these docs. It compares across the items of one
  instrument and not across instruments, since absolute leaked mass
  varies by model, by language, and by how close to 0 and 1 the
  instrument's probabilities sit.

- `pct_off_label` is the share of documents where the emitted token
  matched no label. It is judged by the same rule scoring uses, so a
  token that opens a label counts as that label and a split label is not
  reported as off-label.

- `n_failed` counts the documents whose call failed.

- `mean_expected` is the average expected score across documents, which
  on two labels is the mean of `prob_renorm` on `target` rescaled onto
  the scores. Present only when the instrument carries `scores`.

### Choice shape

More than two labels and no `scores`. One row per item and label.

- `item` and `label` are the question and category.

- `n_won` counts the documents where this label took the largest
  probability. A document whose distribution held none of the labels
  wins nothing.

- `share_won` is `n_won` over the documents scored on that item, so the
  shares of an item fall short of 1 by the share that answered outside
  the labels.

- `pct_label_absent`, `mean_prob_other` and `pct_off_label` are as in
  the endorsement shape. They describe the item's calls rather than the
  label, so they repeat down its label rows.

### Scored shape

Whenever the instrument carries `scores` and there are more than two
labels. One row per item and label, the same winner distribution plus
the scale.

- `item` and `label` are the question and category.

- `score` is the numeric value the instrument assigns that label.

- `n_won` and `share_won` are as in the choice shape.

- `mean_expected` is the average expected score across documents, per
  question. For a single document the expected score multiplies each
  label's value by its probability and adds them up, after those
  probabilities have been rescaled to sum to 1 across the labels.
  Because it uses every label rather than only the winner, a document
  the model split between two neighboring levels lands between them.

- `sd_expected` is the standard deviation of the same, across documents.

- `pct_label_absent`, `mean_prob_other` and `pct_off_label` are as in
  the choice shape.

## The endorsement cut

`endorse_renorm` cuts `prob_renorm` at 0.5, which on a two-label
instrument is the label the model ranked first. The cut is not
configurable.
[`threshold_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md)
reports the rate at other cut points, and `pct_extreme_renorm` says in
one number how far it can move, since a probability at or beyond 0.01
and 0.99 sits on the same side of every cut between them.

## Output shapes

Three shapes come back, and which one is decided by the **labels**
rather than by the items. The number of items changes how many rows come
back, not which columns. The test runs in this order.

1.  Exactly two labels. **Endorsement shape.**

2.  Otherwise, the instrument carries `scores`. **Scored shape.**

3.  Otherwise, more than two labels. **Choice shape.**

The label count is tested first because on two labels the scored shape
would report one number, the average expected score, and lose the
endorsement diagnostics to do it. There is no reason to choose between
them. A scored instrument of two labels reports as endorsement and gains
`mean_expected` as a column, so the number arrives without the
diagnostics leaving.

`exclusive = TRUE` is not itself the test. An exclusive instrument lands
in the choice or scored shape because it usually has more than two
labels or a scale, and an exclusive instrument of two labels reports as
endorsement like any other. Equally, K separate questions sharing one
scale reach the scored shape without being exclusive at all.

Nothing here decides whether your labels are ordered, and nothing can.
What `scores` says is that an average of those numbers is defined, which
is why the shape is named for the argument rather than for a measurement
level.
[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
is where ordering is declared, by its `level`.

The shapes differ in row granularity as well as in columns. Endorsement
gives one row per item. Choice and scored give one row per item **and
label**, because a shared distribution has no single number per question
to report. Choice and scored report a winner distribution, and the
columns describing an item's calls rather than any one label repeat down
its label rows.

`target` applies to the endorsement shape only, and is ignored by the
other two. There is no per-label endorsement rate for a forced choice,
since the labels all compete for one pool of probability, and a rate for
whichever label sorted first is not a meaningful quantity.

## See also

[`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
for the documents behind an item, and
[`threshold_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md)
for how the rates move with the cut point.

## Examples

``` r
# Results from a real run ship with the package, so this needs no server.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
item_report(baked$res_indep)
#> # A tibble: 3 × 9
#>   item      n endorse_renorm pct_extreme_renorm pct_both_absent pct_label_absent
#>   <chr> <int>          <dbl>              <dbl>           <dbl>            <dbl>
#> 1 nega…   100           0.47               0.97               0             0.64
#> 2 posi…   100           0.42               0.97               0             0.69
#> 3 mixed   100           0.31               0.95               0             0.45
#> # ℹ 3 more variables: mean_prob_other <dbl>, pct_off_label <dbl>,
#> #   n_failed <int>
```
