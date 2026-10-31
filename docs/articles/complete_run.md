# A complete run

This article provides an example of how to run the whole `fathom`
package pipeline. The [**Quick
start**](https://alcocer-jj.github.io/fathom/articles/quick_start.md)
page covers package installation and Ollama setup.

The pipeline uses a dataset available in the package (`parlasent`) of
one hundred UK parliamentary sentences that were sampled from the
English portion of ParlaSent 1.0 by Mochtak, Rupnik, Meden, and Ljubešić
(2023).

## Overview

1.  **Select a model** with
    [`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md).
2.  **Code the qualitative instrument** with
    [`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
    and use
    [`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
    to see what the local large language model (LLM) will read.
3.  **Extract response probabilities** with
    [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md).
4.  **Read the results** with
    [`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
    for descriptive summaries, use
    [`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
    to manually validate some entries, and apply
    [`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)
    to hand off for downstream analyses.
5.  **Validate the LLM results** with
    [`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
    and
    [`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md).
6.  **Test the measurement** with
    [`order_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/order_sensitivity.md)
    to check whether the order of the labels drove the answers.

## The corpus

`parlasent` contains data on two annotators who coded every
parliamentary sentence independently based on a six-level ordered scale.
In addition, it contains a reconciliation column resulting from those
two. `annotator1` and `annotator2` are the two codings, and
`reconciliation` is what they settled on.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`fathom`](https://alcocer-jj.github.io/fathom)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`pander`](https://rapporter.github.io/pander/)`)`\
\
[`data`](https://rdrr.io/r/utils/data.html)`(``parlasent``)`\
`parlasent`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``id``, ``sentence``, ``annotator1``, ``annotator2``, ``reconciliation``)`\
`#> # A tibble: 100 × 5`\
`#>    id     sentence                          annotator1 annotator2 reconciliation`\
`#>    <chr>  <chr>                             <chr>      <chr>      <fct>         `\
`#>  1 uk_001 First, the UK Coalition against … N_Neutral  N_Neutral  N_Neutral     `\
`#>  2 uk_002 The thought that on top of that … Negative   Negative   Negative      `\
`#>  3 uk_003 Thanks to their many victories, … P_Neutral  P_Neutral  P_Neutral     `\
`#>  4 uk_004 Even the then Commissioner for P… M_Positive M_Positive M_Positive    `\
`#>  5 uk_005 The Secretary of State told the … N_Neutral  N_Neutral  N_Neutral     `\
`#>  6 uk_006 The family do not know where the… Negative   Negative   Negative      `\
`#>  7 uk_007 I cannot go into the outcomes of… Positive   P_Neutral  Positive      `\
`#>  8 uk_008 Gentleman’s references to those … M_Positive N_Neutral  M_Positive    `\
`#>  9 uk_009 However, there are many problems… Negative   M_Negative M_Negative    `\
`#> 10 uk_010 Member for Leicester West (Liz K… P_Neutral  P_Neutral  P_Neutral     `\
`#> # ℹ 90 more rows`\
\
[`levels`](https://rdrr.io/r/base/levels.html)`(``parlasent``$``reconciliation``)`\
`#> [1] "Negative"   "M_Negative" "N_Neutral"  "P_Neutral"  "M_Positive"`\
`#> [6] "Positive"`

The `Positive` and `Negative` labels are used for sentences that emit
these emotions entirely or predominantly. The `M_Positive` and
`M_Negative` labels are used for sentences that emit these emotions in
an ambiguous or mixed manner. The `P_Neutral` and `N_Neutral` labels
carry no sentiment and differ only in which way they tilt. Overall, the
sample of 100 documents holds around sixteen or seventeen of each.

## Step 1. Select a model

The
[`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)
function accepts an existing local model name and returns an object
holding the settings that every model call will use.

\
`oll`` ``<-`` `[`ollama_backend`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)`(``"gemma4:31b"``)`\
\
`oll`\
`#> Ollama backend`\
`#>`\
`#> Model         gemma4:31b`\
`#> Host          http://localhost:11434`\
`#> Logprobs      top 20`\
`#> Modalities    text, image`

## Step 2. Code the qualitative instrument

The
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
function constructs a codebook containing the question and response
categories the local LLM may answer that question with, and it returns
an object that the
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
function accepts. The annotators who coded the parliamentary speeches
categorized each sentence on one of the previously discussed six ordinal
levels, and the article uses `fathom` to ask a local LLM that same
question using those same six categories.

\
`b`` ``<-`` `[`instrument`](https://alcocer-jj.github.io/fathom/reference/instrument.md)`(`\
`  items ``=`` ``"Where does this sentence fall on the six-point sentiment scale?"``,`\
`  labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"1"``, ``"2"``, ``"3"``, ``"4"``, ``"5"``, ``"6"``)``,`\
`  definitions ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`    ``"1"`` ``=`` ``"entirely or predominantly negative"``,`\
`    ``"2"`` ``=`` ``"ambiguous, or a mixture of sentiments, leaning negative"``,`\
`    ``"3"`` ``=`` ``"no sentiment of its own, leaning negative"``,`\
`    ``"4"`` ``=`` ``"no sentiment of its own, leaning positive"``,`\
`    ``"5"`` ``=`` ``"ambiguous, or a mixture of sentiments, leaning positive"``,`\
`    ``"6"`` ``=`` ``"entirely or predominantly positive"`\
`  ``)``,`\
`  exclusive ``=`` ``TRUE`\
`)`\
\
`b`\
`#> Instrument`\
`#> `\
`#> Shape         exclusive choice, so the labels share one softmax`\
`#> Items         1 (category)`\
`#> Labels        6 (1, 2, 3, 4, 5, 6)`\
`#> Definitions   1  entirely or predominantly negative`\
`#>               2  ambiguous, or a mixture of sentiments, leaning negative`\
`#>               3  no sentiment of its own, leaning negative`\
`#>               4  no sentiment of its own, leaning positive`\
`#>               5  ambiguous, or a mixture of sentiments, leaning positive`\
`#>               6  entirely or predominantly positive`\
`#> Cue           \n\nAnswer with exactly one word: 1, 2, 3, 4, 5, or 6.`\
`#> Matching      by prefix, so a token counts when it opens a label`

The `items` argument holds the question that the local LLM reads exactly
as written. The `labels` argument holds the one-word labels that the
local LLM is instructed to answer with. Because scoring reads a single
token, the labels chosen must have differing first characters. Given
that the actual scheme’s own names do not meet this requirement (e.g.,
`Negative` and `N_Neutral` both start with N), the codebook uses
numerals instead. The `definitions` argument holds the definitions of
the labels that the local LLM reads in tandem with the question. Setting
`exclusive = TRUE` places every category in one question, causing their
probabilities to share a single distribution.[^1] The other mechanism is
independent binary items, which ask one question per category. The
article [What Mexicans ask their government
for](https://alcocer-jj.github.io/fathom/articles/example_ati.md) shows
that kind of example.

### Previewing the model call

The
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
function prints what the local LLM would receive without executing the
call, which is everything in between the dashed lines.[^2]

\
[`preview`](https://alcocer-jj.github.io/fathom/reference/preview.md)`(``b``, text ``=`` ``parlasent``$``sentence``[``1``]``)`\
`#> Prompt preview`\
`#> `\
`#> Item          category, exclusive, one answer from 6`\
`#> Scoring       generated position 1`\
`#> `\
`#> ----------------------------------------------------------------------`\
`#> Text: "First, the UK Coalition against Neglected Tropical Diseases said that there must be national government leadership to integrate programmes with other health, water, sanitation and education initiatives."`\
`#> `\
`#> Where does this sentence fall on the six-point sentiment scale?`\
`#> `\
`#> 1 = entirely or predominantly negative`\
`#> 2 = ambiguous, or a mixture of sentiments, leaning negative`\
`#> 3 = no sentiment of its own, leaning negative`\
`#> 4 = no sentiment of its own, leaning positive`\
`#> 5 = ambiguous, or a mixture of sentiments, leaning positive`\
`#> 6 = entirely or predominantly positive`\
`#> `\
`#> Answer with exactly one word: 1, 2, 3, 4, 5, or 6.`\
`#> `\
`#> Answer:`\
`#> ----------------------------------------------------------------------`\
`#> `\
`#> Label         Counts as`\
`#> 1             1`\
`#> 2             2`\
`#> 3             3`\
`#> 4             4`\
`#> 5             5`\
`#> 6             6`

## Step 3. Extract response probabilities

Using the same prompt as the
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
function, the
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
function takes every document in the corpus and pairs it with each
question and its labels from the codebook instrument, sending each
pairing to the local LLM configured in Step 1. The function then
extracts the probability that the local LLM assigned to each label at
its first generated token, and returns an object holding all
probabilities along with a record of each call.

\
`res`` ``<-`` `[`measure`](https://alcocer-jj.github.io/fathom/reference/measure.md)`(`\
`  ``parlasent``,                     ``# the data frame`\
`  backend    ``=`` ``oll``,              ``# the model, from Step 1`\
`  instrument ``=`` ``b``,                ``# the codebook, from Step 2`\
`  id         ``=`` ``"id"``,             ``# column holding the document identifier`\
`  text       ``=`` ``"sentence"``,       ``# column holding the document`\
`  keep       ``=`` ``"reconciliation"``  ``` # existing columns to carry into `res` ``\
`)`\
`#> Constrained-label measurement`\
`#>`\
`#> Model               gemma4:31b`\
`#> Context Ceiling     2,048 tokens`\
`#> Longest doc         'uk_063', 32 words, 157 tokens`\
`#> First token         '6', a label, from 20 log probabilities`\
`#> Documents           100`\
`#> Items               1 (category)`\
`#> Labels              6 (1, 2, 3, 4, 5, 6)`\
`#> Checkpoints         off`\
`#>`\
`#> Measuring           [===============]  100/100  100%  0.7 s/doc  ETA 0.0 s`\
`#>`\
`#> Done                100 processed, 0 skipped, 0 item failures`\
`#> Elapsed             1.2 min, 0.7 s per document`

Before the very first call,
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
by default establishes the context window that the run needs by
measuring the token count of the longest document-question pair and
rounding it up with headroom, so that no prompt is cut short and Ollama
does not reset its cache between calls. The help page for
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
covers setting the window by hand with `num_ctx`.

## Step 4. Read the results

The results object holds two data frames.

- `$probs` is one row per document, item, and label, and contains the
  results themselves, the probability that the local LLM assigned to
  each label of each document-question pair.

- `$calls` is one row per document and item, and contains call-level
  information for checking the run, along with any columns `keep`
  carried through.

### Probabilities (`$probs`)

\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``res``$``probs``)``, n ``=`` ``6``)`

|   id   |   item   | label | prob_renorm |
|:------:|:--------:|:-----:|:-----------:|
| uk_001 | category |   1   |      0      |
| uk_001 | category |   2   |      0      |
| uk_001 | category |   3   |  6.648e-06  |
| uk_001 | category |   4   |      1      |
| uk_001 | category |   5   |  1.473e-08  |
| uk_001 | category |   6   |  3.169e-07  |

Here, the `prob_renorm` column conveys the probability that the local
LLM assigned to a label, renormalized to the codebook instrument’s label
set, making it the main measure of interest.

### Calls (`$calls`)

\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``res``$``calls``)``, split.table ``=`` ``Inf``)`

| id | item | error | truncated | top1_token | prob_other | margin | expected_score | prompt_hash | reconciliation |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| uk_001 | category | NA | FALSE | 4 | 2.924e-07 | 1 | NA | e80af6d394fe | N_Neutral |
| uk_002 | category | NA | FALSE | 1 | 0 | 1 | NA | e80af6d394fe | Negative |
| uk_003 | category | NA | FALSE | 6 | 0 | 1 | NA | e80af6d394fe | P_Neutral |
| uk_004 | category | NA | FALSE | 5 | 0 | 1 | NA | e80af6d394fe | M_Positive |
| uk_005 | category | NA | FALSE | 4 | 5.1e-05 | 0.7994 | NA | e80af6d394fe | N_Neutral |
| uk_006 | category | NA | FALSE | 1 | 0 | 1 | NA | e80af6d394fe | Negative |

Given that this data frame is at the call level, the `prob_other` column
conveys the raw probability mass that the local LLM gave to all other
tokens not corresponding to the codebook instrument’s label set.
Additionally, the `error` and `truncated` columns flag whether certain
calls would benefit from further examination or a re-run.

For further information, the documentation page for
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
outlines every column found in both data frames.

### Summarizing the results by question and label

The
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
function summarizes the run
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
produced. On a forced-choice codebook instrument it returns one row per
category and prints a header above the table.

\
[`item_report`](https://alcocer-jj.github.io/fathom/reference/item_report.md)`(``res``)`\
`#> Choice instrument`\
`#> `\
`#> Documents     100`\
`#> Items         1`\
`#> Labels        6`\
`#> # A tibble: 6 × 7`\
`#>   item     label n_won share_won pct_label_absent mean_prob_other pct_off_label`\
`#>   <chr>    <chr> <int>     <dbl>            <dbl>           <dbl>         <dbl>`\
`#> 1 category 1        17      0.17             0.72      0.00000346             0`\
`#> 2 category 2        15      0.15             0.72      0.00000346             0`\
`#> 3 category 3        13      0.13             0.72      0.00000346             0`\
`#> 4 category 4        18      0.18             0.72      0.00000346             0`\
`#> 5 category 5        18      0.18             0.72      0.00000346             0`\
`#> 6 category 6        19      0.19             0.72      0.00000346             0`

Here, the `n_won` column counts the number of documents where each label
had the highest probability, whereas the `share_won` column expresses it
as a proportion of the documents scored.

The last three columns describe each question’s calls rather than any
one category, so they repeat down its rows. The `pct_label_absent`
column is the share of documents where at least one category fell
outside the alternatives the local LLM returned, the `mean_prob_other`
column is the average probability mass that landed on no category at
all, and the `pct_off_label` column is the share of documents where the
first token the local LLM generated was not a category.

### Inspecting question-document pairs

The
[`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
function can be used to print a question alongside the corpus documents
it was answered on, by the codebook instrument label of interest and by
whether that label won or the local LLM’s answer was a close call
between two labels.

\
[`inspect_cases`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)`(`\
`  ``res``,                  ``# model results`\
`  ``"category"``,           ``# which item to read`\
`  ``parlasent``,            ``# data frame containing corpus`\
`  text   ``=`` ``"sentence"``,  ``# corpus documents`\
`  id     ``=`` ``"id"``,        ``# document ids`\
`  which  ``=`` ``"endorsed"``,  ``` # documents where `target` won ``\
`  target ``=`` ``"1"``,         ``# most negative sentiment`\
`  n      ``=`` ``2``,           ``# number of documents to print`\
`  seed   ``=`` ``444``          ``# fixed seed`\
`)`

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
    #> **uk_031** | P(1) = 1.0000, other = 0.00e+00, 18 words
    #>
    #> These were huge changes about which the British people were never asked and to which they never consented.
    #>
    #> ---
    #>
    #> **uk_012** | P(1) = 1.0000, other = 0.00e+00, 18 words
    #>
    #> We have a virtual oil slick, like the pollution streaming out into the Gulf of Mexico in 2010.

### The analysis matrix

Finally, the
[`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)
function takes the results object and creates a wide data frame where
each document is one row and each category is a column, which is where
the package hands off.

\
[`as_wide`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)`(``res``, prefix ``=`` ``"p_"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`head`](https://rdrr.io/r/utils/head.html)`(``6``)`\
`#> `\
`#> Note     Exclusive instrument. Rows are a distribution, sums 1.000 to 1.000.`\
`#> # A tibble: 6 × 7`\
`#>   id       p_1      p_2      p_3       p_4           p_5       p_6`\
`#>   <chr>  <dbl>    <dbl>    <dbl>     <dbl>         <dbl>     <dbl>`\
`#> 1 uk_001 0     0        6.65e- 6 1.000e+ 0 0.0000000147  3.17 e- 7`\
`#> 2 uk_002 1.000 5.26e-11 0        0         0             1.28 e-10`\
`#> 3 uk_003 0     0        0        4.87 e-10 0.00000000110 1.000e+ 0`\
`#> 4 uk_004 0     5.54e-10 6.46e-10 1.35 e- 7 1.000         1.58 e- 6`\
`#> 5 uk_005 0     6.08e- 8 1.00e- 1 9.00 e- 1 0.00000143    1.98 e- 6`\
`#> 6 uk_006 1.000 0        0        0         0             1.39 e-10`

## Step 5. Validate the LLM results

An LLM cannot entirely discern whether its answers are right. Some
degree of human validation is often necessary. To do this, the
[`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
function draws a sample of document-question pairs that a human coder
can then fill out as the first step.

\
`sheet`` ``<-`` `[`coding_sample`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)`(`\
`  ``res``,                  ``# model results`\
`  ``parlasent``,            ``# data frame containing corpus`\
`  text   ``=`` ``"sentence"``,  ``# corpus documents`\
`  id     ``=`` ``"id"``,        ``# document ids`\
`  n      ``=`` ``20``,          ``# document-item pairs to sample`\
`  seed   ``=`` ``444``          ``# fixed seed`\
`)`\
`#> Blinded coding sample`\
`#> `\
`#> Pairs         20 across 1 item`\
`#> Drawn         20 of 100`\
`#> `\
`#> Tip      Pass the completed sheet to validate_sample() together with these`\
`#>          results.`\
\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(`[`head`](https://rdrr.io/r/utils/head.html)`(``sheet``, n ``=`` ``2``)``, split.table ``=`` ``Inf``)`

| id | item | question | allowed | human_label | note | text | image |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| uk_002 | category | Where does this sentence fall on the six-point sentiment scale? 1 = entirely or predominantly negative 2 = ambiguous, or a mixture of sentiments, leaning negative 3 = no sentiment of its own, leaning negative 4 = no sentiment of its own, leaning positive 5 = ambiguous, or a mixture of sentiments, leaning positive 6 = entirely or predominantly positive | 1 \| 2 \| 3 \| 4 \| 5 \| 6 | NA | NA | The thought that on top of that we would reduce the possibility for people to attend their local community pharmacy for simple healthcare needs is frightening. | NA |
| uk_061 | category | Where does this sentence fall on the six-point sentiment scale? 1 = entirely or predominantly negative 2 = ambiguous, or a mixture of sentiments, leaning negative 3 = no sentiment of its own, leaning negative 4 = no sentiment of its own, leaning positive 5 = ambiguous, or a mixture of sentiments, leaning positive 6 = entirely or predominantly positive | 1 \| 2 \| 3 \| 4 \| 5 \| 6 | NA | NA | Either way, there should be a means of capturing that information, and I am very sympathetic to doing so. | NA |

Here, each row of the sample sheet pairs a corpus document with one of
the codebook instrument’s questions, named in the `item` column and
written out with its label definitions in the `question` column. Since
this coding instrument asks a single question, every row carries the
same question and each pair is simply a document. However, on a coding
instrument of several questions, a document appears once per question it
was drawn on and `n` counts pairs either way. The `human_label` column
is left blank for human coders to fill in using the labels that the
`allowed` column lists.

### Validating the sample using two coders

Handing the
[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
function a named list of
[`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
sheets will get it to score the local LLM against each of the human
coders, calculate the ceiling the coders set between themselves, and
return the distance between the two. Because the categories are ordinal,
the call sets `level = "ordinal"`. Additionally, because the `parlasent`
dataset already has two independent hand codings, this article uses
those for validation.

\
`v`` ``<-`` `[`validate_sample`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)`(`\
`  `[`list`](https://rdrr.io/r/base/list.html)`(`\
`    annotator1 ``=`` ``coding_sample1``,`\
`    annotator2 ``=`` ``coding_sample2`\
`  ``)``,`\
`  ``res``,`\
`  level ``=`` ``"ordinal"``,`\
`  labels ``=`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``1``:``6``)``,`\
`  seed ``=`` ``444`\
`)`\
\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(``v``$``ceiling``$``pairs``, split.table ``=`` ``Inf``)`

|  coder_a   |  coder_b   |   item   |  n  | agreement | alpha  |
|:----------:|:----------:|:--------:|:---:|:---------:|:------:|
| annotator1 | annotator2 | category | 20  |    0.7    | 0.7903 |

The `v$ceiling$pairs` table establishes a ceiling between the two coders
on the same corpus documents. The `agreement` column denotes the
proportion of documents where the two coders wrote the same label, while
the `alpha` column corresponds to Krippendorff’s alpha (Krippendorff
2019). Rather than counting coding matches, Krippendorff’s alpha
compares the disagreement among the coders with how much they would
disagree if each label were assigned at random, in the proportions the
coders actually used. Alpha is calculated as one minus the ratio of the
two, where 0 is chance-level agreement, 1 is a perfect match, and a
value in between is the share of chance-level disagreement the coders
avoided. Calculating alpha when using ordinal labels accounts for
distance between disagreeing labels by weighting those that disagree by
fewer levels less and vice versa. Given that Krippendorff’s alpha is a
chance-corrected index, it usually sits below raw agreement. Here,
however, an alpha of 0.79 sits above agreement of 0.7, suggesting that
most of the disagreements between the coders were by few levels rather
than by being far off between them. Additionally, the precision of alpha
depends on the sample size `n`.

### Validating the model against the coders

\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(``v``$``gap``, split.table ``=`` ``Inf``)`

|   item   |  n  | model_alpha | coder_alpha |   gap   |     gap_ci      |
|:--------:|:---:|:-----------:|:-----------:|:-------:|:---------------:|
| category | 20  |   0.8353    |   0.7903    | 0.04505 | \[-0.16, 0.25\] |

The `v$gap` table compares the model with the ceiling the coders set,
with one row per question. The `model_alpha` column is the model’s
average alpha with each of the coders, while the `coder_alpha` column is
the coders’ average alpha with each other, which with two coders is
simply their one pairwise alpha. The `gap` column subtracts the second
from the first, so it reads in the same units as alpha (see the
`v$discrimination` table for individual-level comparisons between the
model and each coder). A gap value below zero would indicate that the
model on average disagrees with the coders more than they on average
disagree with each other. Conversely, a gap value above zero would
indicate that the model had a higher average agreement with the coders
than the coders had with each other. The `gap_ci` column holds a
percentile confidence interval around the gap (default of 95 percent),
built from a nonparametric bootstrap. Here, the local LLM’s average
alpha of 0.835 against the human coders’ average alpha of 0.79 has a gap
of 0.045 with an interval of \[-0.16, 0.25\]. Both alpha values sitting
at or near Krippendorff’s 0.80 threshold along with its gap interval
crossing zero suggest that the local LLM’s judgment matched the
annotators’ about as closely as their judgments matched each other,
further suggesting that the LLM’s coding can serve as a viable
alternative to human coders for this corpus.

## Step 6. Test the measurement

LLM classification responses have been shown to be sensitive to label
ordering (e.g., Halterman and Keith 2026). To assess label order
sensitivity, the
[`order_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/order_sensitivity.md)
function runs
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
again (holding the original run’s parameters constant) on every document
with the labels listed in reverse. When a codebook instrument uses three
or more labels (such as this case), the function also runs
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
with the labels shuffled. These two runs are then compared with the
original run.

\
`ord`` ``<-`` `[`order_sensitivity`](https://alcocer-jj.github.io/fathom/reference/order_sensitivity.md)`(`\
`  ``res``,                         ``# model results`\
`  ``parlasent``,                   ``# data frame containing corpus`\
`  text   ``=`` ``"sentence"``,         ``# corpus documents`\
`  id     ``=`` ``"id"``,               ``# document ids`\
`  level  ``=`` ``"ordinal"``,          ``# weight disagreements by distance`\
`  labels ``=`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``1``:``6``)``,  ``# the scale, least to greatest`\
`  seed   ``=`` ``444``                 ``# fixed seed`\
`)`

\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(``ord``, split.table ``=`` ``Inf``)`

|   item   |  order   |  n  | agreement | alpha  |
|:--------:|:--------:|:---:|:---------:|:------:|
| category | reversed | 100 |   0.75    | 0.936  |
| category | shuffled | 100 |   0.77    | 0.9034 |

\
\
[`attr`](https://rdrr.io/r/base/attr.html)`(``ord``, ``"orders"``)`\
`#> $declared`\
`#> [1] "1" "2" "3" "4" "5" "6"`\
`#> `\
`#> $reversed`\
`#> [1] "6" "5" "4" "3" "2" "1"`\
`#> `\
`#> $shuffled`\
`#> [1] "6" "3" "5" "1" "4" "2"`

Here, each row compares a reordering method model run with the original
order model run on the same documents. Like with the `v$ceiling$pairs`
table, the `agreement` column denotes the proportion of documents where
both model runs wrote the same label, while the `alpha` column
corresponds to the Krippendorff’s alpha between both model runs. Under
both methods of reordering, the model’s share of agreement and high
alpha scores suggest the model was still labeling documents similarly,
suggesting that the main results are stable and consistent.

## References

Halterman, A., & Keith, K. A. (2026). Codebook LLMs: Evaluating LLMs as
Measurement Tools for Political Science Concepts. *Political Analysis*,
34(2), 188–204. <https://doi.org/10.1017/pan.2025.10017>

Krippendorff, K. (2019). *Content Analysis: An Introduction to Its
Methodology* (4th ed.). Sage. <https://doi.org/10.4135/9781071878781>

Mochtak, M., Rupnik, P., Meden, K., & Ljubešić, N. (2023). The
multilingual sentiment dataset of parliamentary debates ParlaSent 1.0.
*Slovenian language resource repository CLARIN.SI*.
<http://hdl.handle.net/11356/1868>

[^1]: When `exclusive = TRUE`, the `items` argument must be a single
    string, and passing more than one is refused. That question is named
    *category*, which is the name that appears in the results and in
    every table below.

[^2]: [`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
    cosmetically trims the document at `truncate`, 300 characters by
    default, and marks where it cut. The actual run does not truncate
    the text.
