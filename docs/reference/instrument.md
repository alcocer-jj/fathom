# Construct a validated instrument of items

Bundles the questions to be answered, the labels the model may answer
with, and the instruction that elicits them.

## Usage

``` r
instrument(
  items,
  labels = c("YES", "NO"),
  ...,
  definitions = NULL,
  scores = NULL,
  exclusive = FALSE,
  exact = FALSE,
  probe_texts = NULL,
  cue = NULL,
  lang = c("en", "es"),
  medium = NULL,
  text_label = NULL,
  answer_label = NULL,
  name = NULL,
  backend = NULL
)
```

## Arguments

- items:

  A named list or named character vector of question strings. An unnamed
  single string is accepted and named `Q1`. When `exclusive = TRUE`,
  this may instead be a stem name (`"primary"` or `"best"`) or a custom
  question, with the categories supplied through `labels`.

  A question is a free string and it is yours. It may run to a
  paragraph, reproduce a codebook definition verbatim, carry an
  exclusion clause, or include worked examples. Nothing about the
  scoring changes, since the question sits in the prompt and the answer
  is read from one token.

- labels:

  A character vector of answer words, at least two. Each element is its
  own label. Matching is case folded and reads by prefix, so a token
  counts when it opens a label and a tokenizer splitting `PRESUPUESTO`
  into `P` still scores. Text explaining what a label means goes in
  `definitions`, not here.

  One word each, and whitespace is refused. `NOT_SURE` and `NOT-SURE`
  build, since joining leaves a single answer word that a tokenizer may
  split into a prefix of itself. `NOT SURE` does not, since one token
  cannot span the space and the label would be matched by `NOT` alone.
  Default is `c("YES", "NO")`.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- definitions:

  A named character vector explaining what each label means, or `NULL`
  (the default). An exclusive instrument lists these in the question, so
  the model reads the explanation and types only the name. Any
  instrument can carry them, where they appear in
  [`print()`](https://rdrr.io/r/base/print.html) and improve the
  generated probe document.

- scores:

  A named numeric vector giving each label a value, or `NULL` (the
  default). Names must match the label names exactly. Supplying them
  asserts that the spacing between the numbers is meaningful, which is
  what makes an average of them defined and is a stronger claim than
  that the labels are ordered. Nothing checks it, since unequal spacing
  is legitimate on a real scale.

- exclusive:

  A logical value indicating whether to build an exclusive-choice
  instrument. One question lists all K categories, the model picks one,
  and all K share a softmax at position 1. The result is a distribution,
  unlike an instrument of independent binary items. The category names
  become answer tokens here, so they must differ at their first
  character. Default is `FALSE`.

- exact:

  A logical value indicating whether to score by exact match against
  discovered tokens rather than by prefix. Requires `backend`, since
  those tokens come from probing.

  This is the one case where a label carries more than one string.
  Prefix matching needs the labels to differ at their first character,
  because a split label often emits only that character and `E` cannot
  be attributed between `economia` and `educacion`. Probing sends one
  throwaway document per label, reads back what the model actually
  writes, `ECON` and `EDUC`, and records it. Scoring then consults that
  record instead of guessing from a prefix, and a bare `E` counts for
  nothing and shows up in `prob_other` rather than being assigned to
  whichever label sorted first.

  Nobody types those entries. Use `exact` when the labels cannot be made
  to differ at their first character, and rename them if they can.
  Default is `FALSE`.

- probe_texts:

  A named character vector of example documents, one per label, used in
  place of the generated probe document, or `NULL` (the default). Supply
  these when the labels describe a property of the text rather than a
  topic, since the generated document is *about* the label rather than
  one that exhibits it.

- cue:

  A string giving the instruction appended after each question.
  Generated from `labels` and `lang` if `NULL`. Supply your own for a
  language without a preset.

- lang:

  A string giving the language preset for the prompt scaffolding, `"en"`
  (the default) or `"es"`. Sets `cue`, `text_label`, and `answer_label`
  together, and any of the three can still be overridden individually.
  For any other language, pass all three explicitly rather than
  expecting a preset. See the section below for what each preset
  contains.

  This is the scaffolding, not the question. A question written in
  Spanish under the default `"en"` preset produces a Spanish item
  wrapped in an English cue, which is a real prompt and rarely the
  intended one.

- medium:

  A string naming the stem set a preset name is looked up in, one of
  `"text"`, `"audio"`, `"image"` or `"video"`. Read only when `items`
  names a preset, since a literal question carries its own wording.
  There is no default, because the sets are not translations of one
  another. A picture mentions nothing, so `image` has `show` and
  `depict` where `text` has `topic` and `mention`, and `video` has both
  plus `discuss`, since a row holds frames and a transcript at once and
  the verb is what says which one the question is about.

- text_label, answer_label:

  Strings giving the prompt scaffolding. Default to the `lang` preset.

- name:

  A string giving a label for the instrument, carried into provenance,
  or `NULL` (the default).

- backend:

  An
  [`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)
  object, or `NULL` (the default). When supplied, each label is probed
  against a production-shaped prompt and its observed first token is
  recorded. Probing checks that the labels are distinguishable at
  position 1 and prints what each one emits. It changes scoring only
  under `exact = TRUE`, since a discovered token already opens its own
  label and so is matched by prefix regardless.

## Value

An object of class `fathom_instrument`, a list. Its print method shows
the parts usually wanted, and
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
shows what the model will actually receive.

### The instrument

- `items` is the named list of questions, one per item, in the order
  declared. That order is the order
  [`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
  and the results use, so an instrument reads as the questionnaire it
  was written as.

- `labels` is the named list of answer labels.

- `normalized` holds what each label is actually matched against. Under
  prefix matching this is the label itself, and under `exact = TRUE` it
  is the variants probing discovered. This is the field to read to find
  out why a token did or did not count.

- `definitions` is the category text shown to the model beneath the
  question, or `NULL`. Where long category wording goes, since it has no
  first-character constraint on it.

- `scores` is the numeric value per label, or `NULL`. Sits outside the
  prompt hash on purpose, so rescaling re-extracts nothing.

### How it gets asked

- `cue` is the assembled instruction naming the permitted answers.

- `text_label` and `answer_label` are the words introducing the document
  and the answer slot.

- `lang` names which scaffolding preset supplied the three above.

### Shape and mode

- `exclusive` says whether the labels share one distribution. `FALSE`
  gives independent binary items, so a document may match several.

- `exact_match` says whether scoring compares against probed tokens
  rather than by prefix. The escape for a label set that has to collide.

- `k` and `n_items` are the counts of labels and of items.

- `name` is the short label-set name used in printing.

### Implementation

`probe_texts` and `firsts` are implementation. They hold what probing
was run against and the token each label emitted when probed, and
neither is a stable interface.

## Label matching

Scoring reads one token and counts it as a label when it opens that
label, so `BU` counts as BUDGET. Nothing has to be discovered first,
because a model writing BUDGET must emit a prefix of it, and that
follows from every tokenizer in use detokenizing by concatenation rather
than from any one model's vocabulary.

The price is that labels must differ at their first character. Under
prefix matching this is checked at construction from the strings alone,
with no backend. A split label frequently emits only its first
character, and two labels sharing one cannot then be told apart. With
distinct first characters no two labels share any prefix, so every match
resolves to one label.

A label carrying whitespace is refused at construction, because scoring
reads one token. One token cannot span a space, so a two-word label is
matched by its opening word and the rest is never read, which scores the
opening word rather than scoring zero. Join the words if the answer
really is compound, since `NOT_SURE` and `NOT-SURE` are both accepted.

`exact = TRUE` is the way out when they have to collide. Each label is
probed once, the emitted token is recorded, and scoring consults that
record instead of reading a prefix. Passing a `backend` alone runs the
probe without changing how scoring works, which is worth doing once on a
new model to see the token map.

## Labels and definitions

Labels and definitions are different things. `labels` names the answer
words and the surface forms they may appear in. `definitions` holds the
text a reader or a model needs to know what a label means. Keeping them
apart matters because scoring matches the token at position 1 against
`labels`, so a definition sitting in `labels` registers a token the
model never emits and that label scores zero on every document with no
error raised.

An exclusive instrument lists the definitions under its question, which
is what lets a category be a long phrase while the answer stays one
word. Other instruments store them without putting them in the prompt,
since their question is yours to write.

## Ordinal labels

`scores` attaches a numeric value to each label. The values are
metadata. They never appear in the prompt and the model never types
them, so the labels stay semantic words.

Scoring then returns an `expected_score` column, the sum of each label's
value weighted by its renormalized probability. The labels of any one
call share a softmax, since they are read at a single position from a
single distribution, so this works on any instrument rather than only
exclusive ones.

Nothing checks that the labels are ordered. `scores` asserts an order
the package cannot verify, and reversing the values reverses every score
without complaint.

## Language presets

Three strings the prompt is built from, plus the word used to join the
labels inside the cue.

|  |  |  |
|----|----|----|
| part | en | es |
| `text_label` | Text | Texto |
| `answer_label` | Answer | Respuesta |
| label joiner | or | o |
| cue | Answer with exactly one word: ... | Responde con exactamente una palabra: ... |

So a two-label instrument under `lang = "es"` closes with *Responde con
exactamente una palabra: sí o no.* and labels its parts *Texto* and
*Respuesta*.

Two stems ship for `exclusive = TRUE`, in both languages, and unlike the
item stems in
[`template_items()`](https://alcocer-jj.github.io/fathom/reference/template_items.md)
neither takes a `{label}`, since the categories arrive through
`definitions` instead.

|  |  |  |
|----|----|----|
| stem | en | es |
| `"primary"` | What is the PRIMARY subject of this text? | ¿Cuál es el tema PRINCIPAL de este texto? |
| `"best"` | Which category best describes this text? | ¿Qué categoría describe mejor este texto? |

They are not interchangeable. `"primary"` asks what the text is about,
which assumes the categories are subjects. `"best"` asks which category
describes it, which assumes only that they are a scheme worth sorting
into. Any other string is used as the question verbatim, in whatever
language it is written.

## Case folding and accented labels

A generated token is folded to upper case before it is compared to a
label, so sí, Sí, and SÍ all count as one answer.

[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
and `check_backend()` fold the same way, which is what makes the preview
table a reliable picture of what scoring will do.

The first-character rule is therefore about characters and not bytes.
Two labels differing only in an accent still collide once folded, and
this function refuses them. Where a coding scheme forces that collision,
`exact = TRUE` is the escape.

## See also

[`template_items()`](https://alcocer-jj.github.io/fathom/reference/template_items.md)
for items built from one stem,
[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
for the prompt it assembles, and
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
for running it.

## Examples

``` r
b <- instrument(
  items = list(
    econ = "Does the text discuss economic conditions?",
    immig = "Does the text discuss immigration?"
  ),
  labels = c("YES", "NO")
)
b
#> Instrument
#> 
#> Shape         independent items
#> Items         2 (econ, immig)
#> Labels        2 (YES, NO)
#> Cue           \n\nAnswer with exactly one word: YES or NO.
#> Matching      by prefix, so a token counts when it opens a label
```
