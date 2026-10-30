# fathom: Qualitative Coding and Measurement with Local Language Models

![Dictionary Definition](reference/figures/definition.png)

The `fathom` R package is a toolkit for qualitative coding and
measurement of a corpus of text, images, audio, or video with *local*
large language models (LLMs), where every answer comes back as a
probability rather than a word.

With the function
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md),
R users can ask an Ollama-hosted LLM a constrained set of labeled
questions about a corpus document and get back the model’s next-token
probability distribution for those constrained labels. The questions
themselves are built and bundled with the function
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md).
The function
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)
provides a clear summary of how each question behaved across the entire
corpus, while the function
[`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
displays documents that the model answered with a specific label. The
function
[`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)
prepares the output for any downstream analysis by pivoting the results
so that each row is one document and each question becomes a column. To
audit the model’s answers, the function
[`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
samples a random set of document-question pairs for human validation,
which is then executed through the function
[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md).

## Motivation

Getting an LLM’s response about a question provides no insight as to how
confident it was in determining that answer. This is further obfuscated
if a prompt does not provide a clear and explicit instruction to answer
with a set of constrained response labels. Constraining a labeled set of
responses and extracting their log probabilities allows a user to use
the responses as a classification tool or continuous measurement item.
Additionally, being able to read a model’s probability distribution
allows a user to assess how much probability mass was placed on each of
the labels, revealing the presence of a compound question, a missing
category, or a bad prompt.

## The fathom workflow

**0. Prepare the corpus (non-text corpora)**

**[`prepare_image()`](https://alcocer-jj.github.io/fathom/reference/prepare_image.md)**

- Turns a set of image files into a valid corpus, where each row is one
  image filepath alongside its file size.

**[`prepare_audio()`](https://alcocer-jj.github.io/fathom/reference/prepare_audio.md)**

- Turns a set of audio files into a valid corpus by using an Automatic
  Speech Recognition (ASR) model to transcribe each recording to text.

**[`prepare_video()`](https://alcocer-jj.github.io/fathom/reference/prepare_video.md)**

- Turns a set of video files into a valid corpus, where each row is one
  video holding its extracted frames alongside an ASR transcript of its
  audio.

**1. Prepare a local model**

**[`ollama_backend()`](https://alcocer-jj.github.io/fathom/reference/ollama_backend.md)**

- Connects to a local [Ollama](https://ollama.com) server and confirms
  the model is installed.
- Called with no model name, lists the models the server has installed,
  so it doubles as a way to confirm the server is running.
- Works with an instruction-following model whose first generated token
  is a label, including one that reasons by default, since reasoning is
  disabled on every call.

**2. Write the coding instrument**

**[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)**

- Bundles the questions with the single-word labels the model may answer
  with.
- Questions themselves can be of any length and consist of anything a
  user wants the model to read alongside the corpus.
- Questions can be structured as (1) independent items or (2) exclusive
  items.
- 1.  Independent items ask a separate binary question for each
      category, meaning that a document can match several at once. (2)
      Exclusive items make the categories the labels of a single
      question, meaning that they share a single distribution.

**[`template_items()`](https://alcocer-jj.github.io/fathom/reference/template_items.md)
(optional)**

- Builds an instrument of independent items by inserting each category
  from a list into one question template.
- Templates can be written by the user or chosen from a provided set
  that comes in English and Spanish for each type of corpus (text,
  image, audio, and video).

**[`preview()`](https://alcocer-jj.github.io/fathom/reference/preview.md)
(optional)**

- Prints a preview of the exact string that the model will receive.

**3. Extract response probabilities**

**[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)**

- Calls the LLM with an instrument of questions and labels over a data
  frame or a character vector of documents, where each call processes
  one document and one question.
- Returns an object containing (1) a table holding the probability of
  every label for each document-question pair and (2) a table holding
  details for each document-question pair, including the model’s first
  response token, the share of probability that fell outside the
  constrained labels, and the probability gap between the top two
  labels.
- If given a checkpoint folder, the run saves one file per document,
  allowing an interrupted run to pick up where it left off when called
  again on the same folder.

**4. Summarize the results**

**[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md)**

- Summarizes how each question was answered by the model across the
  corpus.
- For two-label questions, such as independent items, it reports how
  often the model chose the label of interest (e.g., how often it
  answered “yes” over “no”), how often it was close to certain with
  probabilities near 0 or 1, and the most probability it ever placed on
  words outside the specified labels.
- For questions with more than two labels, such as most exclusive items,
  it reports how often each label won.

**[`inspect_cases()`](https://alcocer-jj.github.io/fathom/reference/inspect_cases.md)
(optional)**

- Prints the full text of a sample of documents where a question’s
  target label won (e.g., text where the model answered “yes”), allowing
  a user to gut-check the model’s answers.
- It can also print the closest calls, meaning the documents where the
  gap between the two most likely labels was smallest (e.g., text where
  the model put nearly equal probability on “yes” and “no”).

**[`as_wide()`](https://alcocer-jj.github.io/fathom/reference/as_wide.md)**

- Pivots the first table from
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  into one row per document and one column per question, each holding
  the probability of the target label (e.g., the probability of “yes”).
  For exclusive items, each category gets its own column instead.
- With `dichotomize = TRUE`, returns 0 or 1 instead of probabilities,
  cut at 0.5.
- With `value = "expected_score"`, returns each document’s expected
  score on each question, the average of the label scores weighted by
  their probabilities, which requires an instrument built with `scores`.

**5. Validate response probabilities**

**[`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)**

- Draws a seeded random sample of document-question pairs into an
  exportable data frame coding sheet.
- Every pair has a uniform chance of being drawn.

**[`coder_reference()`](https://alcocer-jj.github.io/fathom/reference/coder_reference.md)
(optional)**

- Consolidates two or more completed
  [`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
  sheets into one, allowing a group of coders to serve as the single
  reference for
  [`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md).
- Its default is majority rule, but can also be switched to unanimous
  agreement (`method = "unanimous"`) or to report the share of coders
  who chose the target label (`method = "soft"`).
- Leaves a tie blank rather than breaking it.

**[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md)**

- Validates the model’s answers from
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  against a completed
  [`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
  sheet in terms of discrimination and ranking, with calibration on
  request.
- If given a named list of
  [`coding_sample()`](https://alcocer-jj.github.io/fathom/reference/coding_sample.md)
  sheets, it also reports intercoder reliability and how far the model’s
  responses are from the coders, with a bootstrap confidence interval.

**6. Get measurement diagnostics (optional)**

**[`threshold_sensitivity()`](https://alcocer-jj.github.io/fathom/reference/threshold_sensitivity.md)**

- Reports endorsement rates at a range of cut points.

## Requirements

A running [Ollama](https://ollama.com) server at version 0.12.11 or
later, and a
[compatible](https://alcocer-jj.github.io/fathom/quick_start.md) Ollama
instruction-following model.

[`ffmpeg`](https://www.ffmpeg.org/) for measuring audio and video.

[`whisperx`](https://github.com/m-bain/whisperx) or
[`whispermlx`](https://github.com/KalebJS/whispermlx) (Apple Silicon
optimized) for measuring audio, with a HuggingFace [User Access
Token](https://huggingface.co/docs/hub/en/security-tokens) and access to
the
[Community-1](https://huggingface.co/pyannote/speaker-diarization-community-1)
model to use speaker diarization.

See the [Quick
start](https://alcocer-jj.github.io/fathom/quick_start.md) page for
information on how to set each of these up.

## Installation

`fathom` is distributed through R-Universe.

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(`\
`  ``"fathom"``,`\
`  repos ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"https://alcocer-jj.r-universe.dev"``, ``"https://cloud.r-project.org"``)`\
`)`

The development version installs from GitHub.

\
`# install.packages("pak")`\
`pak``::`[`pak`](https://pak.r-lib.org/reference/pak.html)`(``"alcocer-jj/fathom"``)`

## Acknowledgments

Development of this package was assisted by [Claude
Code](https://claude.com/product/claude-code), an AI coding assistant by
Anthropic, for code review, documentation building, vignette drafting,
and the validation components.
