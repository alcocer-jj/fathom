# What Mexicans ask their government for

[A complete
run](https://alcocer-jj.github.io/fathom/articles/complete_run.md) uses
`fathom` to build an exclusive coding instrument, where every label
category is placed in the question itself and their probabilities share
a single distribution. This article provides an example of how to build
an independent binary qualitative instrument, where each label category
is asked as its own yes-or-no question, so that a document can fall into
multiple categories. The pipeline uses a dataset available in the
package (`ati`) of three hundred Spanish-language access-to-information
requests that Mexicans filed with their federal government through
INFOMEX, sampled from a corpus hand-coded by Erlich et al. (2022).

## Overview

1.  **Preprocess the corpus** by dropping the documents whose text
    contains no meaningful information.
2.  **Code the qualitative instrument** with
    [`template_items()`](https://alcocer-jj.github.io/fathom/reference/template_items.md)
    and
    [`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md),
    and use
    [`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
    to see what the local large language model (LLM) will read.
3.  **Extract response probabilities** with
    [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md).
4.  **Read the results** with
    [`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
    and
    [`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md).
5.  **Check cut point sensitivity** with
    [`threshold_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md).

## The corpus

`ati` contains the text of Spanish-language requests that were filed by
Mexicans between 2003 and 2015, along with the fields that trace them
back to their source. `folio` is the request number from INFOMEX, the
portal created by Mexico’s 2002 transparency law, *la Ley Federal de
Transparencia y Acceso a la Información Pública*, and `dependency` is
the federal agency that received the request. Unlike `parlasent`, this
dataset carries no human codings, so the article does not validate the
results against coders.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`fathom`](https://alcocer-jj.github.io/fathom)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
`#> `\
`#> Attaching package: 'dplyr'`\
`#> The following objects are masked from 'package:stats':`\
`#> `\
`#>     filter, lag`\
`#> The following objects are masked from 'package:base':`\
`#> `\
`#>     intersect, setdiff, setequal, union`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`pander`](https://rapporter.github.io/pander/)`)`\
\
[`data`](https://rdrr.io/r/utils/data.html)`(``ati``)`\
\
`ati`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`slice`](https://dplyr.tidyverse.org/reference/slice.html)`(``1``:``5``)`\
`#> # A tibble: 5 × 5`\
`#>   id    folio         date       dependency                               text  `\
`#>   <chr> <chr>         <date>     <chr>                                    <chr> `\
`#> 1 1007  1816400171610 2010-10-04 COMISIÓN FEDERAL DE ELECTRICIDAD         Monto…`\
`#> 2 1010  1860000007206 2006-09-14 PMI COMERCIO INTERNACIONAL S.A. DE C.V.  ¿Cómo…`\
`#> 3 1018  0411100021603 2003-11-26 INSTITUTO NACIONAL DE MIGRACIÓN          Solic…`\
`#> 4 1030  1111200026214 2014-06-06 CONSEJO NACIONAL DE CIENCIA Y TECNOLOGÍA Solic…`\
`#> 5 1093  0064101889614 2014-08-12 INSTITUTO MEXICANO DEL SEGURO SOCIAL     Se me…`

## Step 1. Preprocess the corpus

Around 13% of the corpus documents consisted of requests with an
attachment. As a result, the text found in these requests contained only
pointer language. Given that this is just noise, the first step consists
of dropping those documents along with requests shorter than six
words.[^1]

\
`vocab`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`  ``"ver|favor de ver|se anexa|se adjunta|solicito|solicita|"``,`\
`  ``"archivo|documento|documentos|cuestionario|anexo|anexa|adjunto|adjunta|"``,`\
`  ``"informacion|información|el|la|los|las|un|una|de|del|en|y|mi|se"`\
`)`\
\
`pointer`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"^([[:space:]]*\\b("``, ``vocab``, ``")\\b)+[[:space:].]*$"``)`\
\
`ati`` ``<-`` ``ati`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(`\
`    n_words ``=`` `[`lengths`](https://rdrr.io/r/base/lengths.html)`(`[`strsplit`](https://rdrr.io/r/base/strsplit.html)`(`[`trimws`](https://rdrr.io/r/base/trimws.html)`(``text``)``, ``"\\s+"``)``)`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(`\
`    ``!`[`grepl`](https://rdrr.io/r/base/grep.html)`(``pointer``, `[`tolower`](https://rdrr.io/r/base/chartr.html)`(`[`trimws`](https://rdrr.io/r/base/trimws.html)`(``text``)``)``, perl ``=`` ``TRUE``)``,`\
`    ``n_words`` ``>=`` ``6`\
`  ``)`

The coded pattern matches text made of nothing but pointer vocabulary,
so a document request that opens with *documento* and then says
something survives the filter.

## Step 2. Code the qualitative instrument

Erlich et al. (2022) coded their requests on twenty-one features, and
their topic block names six categories. However, their codebook was
never published, and their supplemental material only named the
variables without describing how they were defined. To circumvent this
challenge, the pipeline instead draws from five categories found in
Article 7 of the 2002 Mexican transparency law, which lists what every
federal agency must publish.

The
[`template_items()`](https://alcocer-jj.github.io/fathom/reference/template_items.md)
function can build questions from a single question stem, slotting each
category into the stem’s `{label}` placeholder so that the only thing
varying across the questions is the category they name. While `fathom`
comes with shipped stems, the function also accepts a custom string as
the stem.

\
`# Build custom question stem in Spanish`\
`custom_stem`` ``<-`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`  ``"¿Esta solicitud pide a la dependencia o entidad información "``,`\
`  ``"sobre {label}?"`\
`)`\
\
`# Build list of topics from Article 7 also in Spanish`\
`topics`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  actividades ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`    ``"las actividades, servicios, trámites o programas que ofrece o "``,`\
`    ``"realiza"`\
`  ``)``,`\
`  presupuesto ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`    ``"el presupuesto, el gasto, los recursos financieros o los montos "``,`\
`    ``"destinados a algo"`\
`  ``)``,`\
`  contratos ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`    ``"contratos, contrataciones, licitaciones, proveedores, concesiones o "``,`\
`    ``"permisos otorgados"`\
`  ``)``,`\
`  estructura ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`    ``"su estructura, sus unidades administrativas, su personal, los puestos "``,`\
`    ``"o las remuneraciones"`\
`  ``)``,`\
`  normatividad ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(`\
`    ``"normatividad, reglamentos, lineamientos o el marco legal que le aplica"`\
`  ``)`\
`)`

`fathom` ships with two prompt scaffolding languages, English and
Spanish. The `lang = "es"` argument in the
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
function writes the scaffolding text around the question in the language
chosen. The question text itself is not affected by this argument.

\
`b_topics`` ``<-`` `[`instrument`](https://alcocer-jj.github.io/fathom/reference/instrument.md)`(`\
`  `[`template_items`](https://alcocer-jj.github.io/fathom/reference/template_items.md)`(``custom_stem``, ``topics``)``,`\
`  labels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"sí"``, ``"no"``)``,`\
`  lang ``=`` ``"es"`\
`)`\
\
`b_topics`\
`#> Instrument`\
`#> `\
`#> Shape         independent items`\
`#> Items         5 (actividades, presupuesto, contratos, estructura, normatividad)`\
`#> Labels        2 (sí, no)`\
`#> Cue           \n\nResponde con exactamente una palabra: sí o no.`\
`#> Matching      by prefix, so a token counts when it opens a label`

### Previewing the model call

The
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
function prints what the local LLM would receive without executing the
call.[^2]

\
[`preview`](https://alcocer-jj.github.io/fathom/reference/preview.md)`(``b_topics``, text ``=`` ``ati``$``text``[``1``]``)`\
`#> Prompt preview`\
`#> `\
`#> Item          actividades, 1 of 5`\
`#> Scoring       generated position 1`\
`#> `\
`#> ----------------------------------------------------------------------`\
`#> Texto: "Monto del adeudo que mantiene con la CFE a la fecha cada uno de los sistemas de agua de los 33 municipios del Estado de Morelos por concepto de consumo de energía eléctrica."`\
`#> `\
`#> ¿Esta solicitud pide a la dependencia o entidad información sobre las actividades, servicios, trámites o programas que ofrece o realiza?`\
`#> `\
`#> Responde con exactamente una palabra: sí o no.`\
`#> `\
`#> Respuesta:`\
`#> ----------------------------------------------------------------------`\
`#> `\
`#> Label         Counts as`\
`#> sí            SÍ`\
`#> no            NO`

## Step 3. Extract response probabilities

The
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
function takes every document in the corpus and pairs it with each
question and its labels from the codebook instrument, sending each
pairing to the local LLM configured with
[`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md).
The function then extracts the probability that the local LLM assigned
to each label at its first generated token, and returns an object
holding all probabilities along with a record of each call.

\
`oll`` ``<-`` `[`ollama_backend`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)`(``"gemma4:31b"``)`\
\
`oll`\
`#> Ollama backend`\
`#>`\
`#> Model         gemma4:31b`\
`#> Host          http://localhost:11434`\
`#> Logprobs      top 20`\
`#> Modalities    text, image`\
\
`res_topics`` ``<-`` `[`measure`](https://alcocer-jj.github.io/fathom/reference/measure.md)`(`\
`  ``ati``,                    ``# the data frame`\
`  backend ``=`` ``oll``,          ``# the model, from above`\
`  instrument ``=`` ``b_topics``,  ``# the codebook, from Step 2`\
`  id      ``=`` ``"id"``,         ``# column holding the document identifier`\
`  text    ``=`` ``"text"``,       ``# column holding the document`\
`  keep    ``=`` ``"dependency"``  ``` # existing columns to carry into `res_topics` ``\
`)`\
`#> Constrained-label measurement`\
`#>`\
`#> Model               gemma4:31b`\
`#> Context Ceiling     2,048 tokens`\
`#> Longest doc         '3635', 588 words, 995 tokens`\
`#> First token         'NO', a label, from 20 log probabilities`\
`#> Documents           286`\
`#> Items               5 (actividades, presupuesto, contratos, estructura,`\
`#>                     normatividad)`\
`#> Labels              2 (sí, no)`\
`#> Checkpoints         off`\
`#>`\
`#> Measuring           [===============]  286/286  100%  3.2 s/doc  ETA 0.0 s`\
`#>`\
`#> Done                286 processed, 0 skipped, 0 item failures`\
`#> Elapsed             15.3 min, 3.2 s per document`

## Step 4. Read the results

As explained in [A complete
run](https://alcocer-jj.github.io/fathom/articles/complete_run.md), the
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
function returns an object holding two data frames.

- `$probs`, which contains the results themselves, these being the
  probabilities that the local LLM assigned to each label of each
  document-question pair.

- `$calls`, which contains call-level information for checking the run,
  along with the columns `keep` carried through (in this case
  `dependency`).

### Summarizing the results by question and label

The
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
function summarizes each item across the corpus, and its shape depends
on the results object that it receives. If the `target` parameter is
left unset, it uses the instrument’s first label, which here is `sí`.

\
`report`` ``<-`` `[`item_report`](https://alcocer-jj.github.io/fathom/reference/item_report.md)`(``res_topics``)`\
[`pander`](https://rdrr.io/pkg/pander/man/pander.html)`(``report``, split.table ``=`` ``Inf``)`

| item | n | endorse_renorm | pct_extreme_renorm | pct_both_absent | pct_label_absent | mean_prob_other | pct_off_label | n_failed |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| actividades | 286 | 0.2413 | 0.9266 | 0 | 0.06993 | 1.377e-06 | 0 | 0 |
| presupuesto | 286 | 0.2937 | 0.979 | 0 | 0.1993 | 1.035e-06 | 0 | 0 |
| contratos | 286 | 0.2378 | 0.965 | 0 | 0.1503 | 8.275e-07 | 0 | 0 |
| estructura | 286 | 0.2168 | 0.986 | 0 | 0.07343 | 9.752e-07 | 0 | 0 |
| normatividad | 286 | 0.09091 | 0.9895 | 0 | 0.03846 | 1.202e-06 | 0 | 0 |

Here, the `endorse_renorm` column depicts the proportion of requests
where `sí` had the highest probability mass, whereas
`pct_extreme_renorm` depicts the share of requests whose probability on
`sí` sat at the most extreme ends (\<= 0.01 or \>= 0.99). Based on this
share, which runs from 0.927 to 0.990 across the five questions, it can
be said that the local LLM answered nearly every request with certainty.

Visit the
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
documentation page for information that outlines every column for this
shape.

### The analysis matrix

Finally, the
[`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)
function takes the results object and creates a wide data frame where
each request is one row and each question is a column, which is where
the package hands off. Setting `dichotomize = TRUE` turns each
probability into a binary using a 0.5 cutoff.

\
`wide`` ``<-`` `[`as_wide`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)`(`\
`  ``res_topics``,`\
`  prefix      ``=`` ``"t_"``,`\
`  dichotomize ``=`` ``TRUE`\
`)`\
\
[`head`](https://rdrr.io/r/utils/head.html)`(``wide``, ``6``)`\
`#> # A tibble: 6 × 6`\
`#>   id    t_actividades t_presupuesto t_contratos t_estructura t_normatividad`\
`#>   <chr>         <int>         <int>       <int>        <int>          <int>`\
`#> 1 1007              0             1           0            0              0`\
`#> 2 1010              0             1           1            0              0`\
`#> 3 1018              0             0           0            0              0`\
`#> 4 1030              0             0           0            1              0`\
`#> 5 1093              0             0           0            0              1`\
`#> 6 11                1             0           0            0              0`

## Step 5. Check cut point sensitivity

By default, `fathom` sets the endorsement cut point at 0.5. The
[`threshold_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md)
function conducts a grid search testing each cut point across the 0.1 to
0.9 range and reports the endorsement rates at each.

\
`sens`` ``<-`` `[`threshold_sensitivity`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md)`(``res_topics``)`\
`#> `\
`#> Note     Endorsement swing across the range, widest first.`\
`#>          actividades   0.02`\
`#>          contratos     0.02`\
`#>          normatividad  0.01`\
`#>          estructura    0.00`\
`#>          presupuesto   0.00`\
\
`sens`\
`#> # A tibble: 45 × 5`\
`#>    item        threshold     n n_endorse endorse`\
`#>    <chr>           <dbl> <int>     <int>   <dbl>`\
`#>  1 actividades       0.1   286        74   0.259`\
`#>  2 actividades       0.2   286        71   0.248`\
`#>  3 actividades       0.3   286        71   0.248`\
`#>  4 actividades       0.4   286        70   0.245`\
`#>  5 actividades       0.5   286        69   0.241`\
`#>  6 actividades       0.6   286        69   0.241`\
`#>  7 actividades       0.7   286        68   0.238`\
`#>  8 actividades       0.8   286        68   0.238`\
`#>  9 actividades       0.9   286        68   0.238`\
`#> 10 presupuesto       0.1   286        85   0.297`\
`#> # ℹ 35 more rows`

Across the whole range the widest swing is 2.1 percentage points, on
`actividades`, and the rest move by 2.1 on `contratos`, 0.7 on
`normatividad`, 0.3 on `estructura`, and 0.3 on `presupuesto`,
suggesting that the endorsement rates are not sensitive to where the cut
point is placed.

## References

Erlich, A., Dantas, S. G., Bagozzi, B. E., Berliner, D., & Palmer-Rubin,
B. (2022). Multi-label prediction for political text-as-data. *Political
Analysis*, 30(4), 463–480. <https://doi.org/10.1017/pan.2021.15>

[^1]: Six is a mere arbitrary heuristic.

[^2]: [`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
    cosmetically trims the document at `truncate`, 300 characters by
    default, and marks where it cut. The actual run does not truncate
    the text.
