# Package index

## Building an instrument

An instrument bundles the questions with the labels the model may answer
with. Read the assembled prompt before spending anything on a run.

- [`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
  : Construct a validated instrument of items
- [`template_items()`](https://alcocer-jj.github.io/fathom/reference/template_items.md)
  : Build items by substituting categories into one question stem
- [`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
  : Print the assembled prompt
- [`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)
  : Configure an Ollama backend

## Extracting

One unconstrained call per document-item, reading the probability
distribution at generated position 1.

- [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  : Measure documents against an instrument
- [`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md)
  : Read a checkpoint directory back into results

## Reading the results

Aggregate statistics cannot tell you whether an item measures what you
intended. These describe; only inspect_cases() answers that question, by
showing you the documents.

- [`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
  : Summarize how each item behaved
- [`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
  : Read the documents that fired on an item

## Preparing a corpus

A corpus of files rather than a column of text. One row per document,
with the columns measure() and the audit functions expect.

- [`prepare_image()`](https://alcocer-jj.github.io/fathom/reference/prepare_image.md)
  : Turn a folder of images into a corpus measure() can take
- [`prepare_audio()`](https://alcocer-jj.github.io/fathom/reference/prepare_audio.md)
  : Turn recordings into a corpus of annotated transcripts
- [`prepare_video()`](https://alcocer-jj.github.io/fathom/reference/prepare_video.md)
  : Turn videos into a corpus carrying both what was shown and what was
  said

## Testing the measurement

A probability is more than a label, and these are what that buys. How
much of an item’s endorsement rate is the threshold rather than the
corpus, whether the model agreed with a human, how far two humans agreed
with each other, and whether its probabilities were honest.

- [`threshold_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md)
  : Compute endorsement rates across a range of thresholds
- [`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
  : Draw a blinded sample for human coding
- [`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)
  : Validate a completed coding sheet against the model
- [`coder_reference()`](https://alcocer-jj.github.io/fathom/reference/coder_reference.md)
  : Collapse several coders into one reference sheet

## Downstream

- [`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)
  : Pivot results into an analysis matrix

## Data

Two corpora hand-coded by the researchers who collected them. parlasent
is the only one carrying two independent codings before reconciliation,
so it is the only one against which a model can be read at a human
ceiling rather than against perfect agreement. It ships under CC BY-SA
4.0 rather than MIT, see LICENSE.note.

- [`ati`](https://alcocer-jj.github.io/fathom/reference/ati.md) :
  Mexican access-to-information requests
- [`parlasent`](https://alcocer-jj.github.io/fathom/reference/parlasent.md)
  : UK parliamentary sentences, coded for sentiment on a six-level scale

## Package

- [`fathom`](https://alcocer-jj.github.io/fathom/reference/fathom-package.md)
  [`fathom-package`](https://alcocer-jj.github.io/fathom/reference/fathom-package.md)
  : fathom: Qualitative Coding and Measurement with Local Language
  Models
