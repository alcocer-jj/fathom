# Turn recordings into a corpus of annotated transcripts

Transcribes a recording and writes its pauses and speakers back into the
text, so `measure(text = )` reads a transcript that carries them. A
plain transcript records what was said and discards how. Pauses,
hesitation and who was speaking leave with it, and for a construct that
lives in delivery rather than in wording that is the absence of the
thing being measured.

## Usage

``` r
prepare_audio(
  x,
  ...,
  diarize_names = FALSE,
  annotate_timing = FALSE,
  tool = c("whisperx", "whispermlx"),
  asr_model = "large-v3",
  min_speakers = NULL,
  max_speakers = NULL,
  language = NULL,
  hf_token = NULL,
  extra = NULL,
  min_pause = 0.2,
  speakers = NULL,
  id = NULL,
  cache_dir = NULL,
  force = FALSE,
  verbosity = c("normal", "quiet", "verbose")
)
```

## Arguments

- x:

  A character vector of paths to audio files, one row each. A path that
  does not resolve comes back with `exists = FALSE` rather than being
  dropped.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- diarize_names:

  A logical value indicating whether to label who is speaking. This is a
  transcription setting rather than a rendering one. It runs the
  diarizer, needs `hf_token`, and is in the cache key, so switching it
  re-transcribes. It also decides whether the text has turns in it at
  all, since a turn is a run of words by one speaker. Off, every silence
  is a `[pause]` and the transcript is one run of text. Default is
  `FALSE`.

- annotate_timing:

  A logical value indicating whether silences are written into the text,
  as `[pause 0.4s]` inside a turn and `[gap 0.8s]` between two. Default
  is `FALSE`.

- tool:

  A string naming the recognizer to run, `"whisperx"` (the default) or
  `"whispermlx"`. Both emit the same JSON.

- asr_model:

  A string naming the Whisper checkpoint, which decides the words. Both
  tools take the same short names. The command-line default is `small`,
  which is not the default here. Default is `"large-v3"`.

- min_speakers, max_speakers:

  Integers passed to the diarizer when known, or `NULL` (the default).
  An interview has two speakers and the researcher knows it beforehand,
  which is the cheapest reduction in diarization error available.

- language:

  A string giving the spoken language, detected per file when `NULL`
  (the default).

- hf_token:

  A string giving a Hugging Face read token, required when
  `diarize_names` is on. Default is `NULL`.

- extra:

  A character vector of further flags for the recognizer, passed through
  verbatim and recorded on the result. Each element is its own argument,
  so `c("--beam_size", "10")` rather than one string. Default is `NULL`.

- min_pause:

  A number giving the seconds below which a silence is not marked or
  counted. Jefferson leaves anything under 0.2 as an untimed micropause
  and gives a figure above it, which is where the default comes from.

- speakers:

  A named character vector renaming the speakers, such as
  `c(SPEAKER_00 = "Interviewer")`. A name not in the timings is refused
  rather than ignored. Default is `NULL`.

- id:

  A character vector of identifiers, one per file. Defaults to the file
  names without their extensions, and must be unique.

- cache_dir:

  A string giving the directory where transcripts are written and looked
  for. Defaults to beside each recording.

- force:

  A logical value indicating whether to transcribe again even where a
  usable transcript is cached. Default is `FALSE`.

- verbosity:

  A string setting how much to report.

  - `"quiet"` reports nothing.

  - `"normal"` is the default. One line per recording as it is
    transcribed, or in an interactive session a bar redrawn in place
    when there is more than one. The recognizer's own output is held
    rather than printed, and appears only if the call fails.

  - `"verbose"` lets the recognizer write to the console as it runs.
    These tools report their dependencies at length on every invocation,
    so this is for one long file rather than for a corpus.

## Value

A tibble of one row per file.

- `id`, `audio` and `exists` identify the recording.

- `text` is the transcript, with turns where `diarize_names` was on and
  silences where `annotate_timing` was.

- `duration_s` and `n_words` hold what the recording holds.

- `n_turns` and `n_speakers` are `NA` when the timings carry no speaker.

- `n_pauses` counts the silences inside a turn at or above `min_pause`.

- `n_gaps` counts the silences at a turn boundary, `NA` when not
  diarized.

- `median_gap_s` is the middle silence between two words.

- `speech_rate` and `articulation_rate` are words per second over the
  whole recording, and over voiced time only. The first counts the
  silences and the second does not, so the ratio between them is how
  much of the elapsed time was pausing rather than talking.

- `median_turn_s` is the middle turn, `NA` without diarization.

- `median_score` is the middle alignment confidence, `NA` where the
  transcript carries none. Every silence figure rests on the word
  boundaries this describes, so it is what says how far to trust them.

- `diarized` says whether the transcript carried speakers, which is also
  whether the text has turns in it.

- `annotate_timing` says whether the silences were written in.

- `transcribed` says whether this call produced the transcript.

- `tool`, `asr_model` and `extra` hold what made the words, or `NA`
  where a transcript was found with no record beside it. `transcribed`
  and these together say whether a row was made now, read from a
  verified transcript, or read from one of unknown origin.

- `language` and `min_pause` are read off the transcript or applied by
  this call, so both hold however the transcript was obtained.

## Caching

A transcript already on disk is read instead of transcribing again, so
an interrupted run resumes and a hand-corrected transcript survives.
`force = TRUE` returns to the recognizer.

What counts as already done is the question
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
asks of its checkpoints, which is whether the work was done under the
settings now being requested rather than merely done. `tool`,
`asr_model`, `diarize_names`, `language` and `extra` are recorded in a
`.fathom.json` beside each transcript, and a run whose settings differ
transcribes again. Without that, a call with `diarize_names` off in a
directory that already holds a diarized transcript would read the
diarized one and quietly ignore the argument.

`annotate_timing`, `min_pause` and `speakers` are not in that record,
since none of them reaches the recognizer. Changing a rendering costs
nothing and re-reads the same transcript.

A transcript with no record beside it is used rather than discarded,
because that is what a hand-corrected one looks like. Its `tool`,
`asr_model` and `extra` come back `NA`, since naming settings that did
not produce those words would be worse than saying nothing.

## Transcript format

The row a model is asked about is the whole recording, and the structure
inside it is written into `text` rather than spread across rows.

Inside a row, a line is a turn, meaning a maximal run of words by one
speaker. Nothing splits a turn further. A silence inside one person's
talk is written where it happened, as `[pause 0.4s]`, which says how
long it was rather than only that it passed a threshold.

Silence at a turn boundary is written `[gap 0.8s]` and belongs to
neither speaker, so it goes on a line of its own. Whether a silence is a
hesitation or a turn transition is a fact about the recording, so the
two markers are told apart by whether the timings carry a speaker, not
by whether labels were asked for.

Lines are newline separated with the speaker in brackets ahead of them.

## Speakers

Speakers reach the output only from the recognizer. Absent, `n_speakers`
and `n_turns` are `NA` and `diarized` is `FALSE`, so a transcript with
no speaker in it is never read as a transcript with one speaker in it.

`speakers` renames what the recognizer emitted, since `SPEAKER_00`
carries nothing a model can use. It is also the fastest way to put the
answer in the prompt. A speaker renamed `Angry Protester` has been coded
before the model saw the text, and nothing in the results would show it.

## Pause and gap counts

`n_pauses` and `n_gaps` count what `min_pause` found in the timings, so
they hold with `annotate_timing` off, where the text shows nothing. Only
`text` and `annotate_timing` change with the rendering.

None of them reaches the model, since
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
reads `id` and `text` and nothing else.

`n_pauses` also moves with diarization on the same words, because
without turns a turn transition is just another silence.

## References

Jefferson, G. (1989). Preliminary notes on a possible metric which
provides for a standard maximum silence of approximately one second in
conversation. In D. Roger and P. Bull (eds), *Conversation. An
Interdisciplinary Perspective*, 166–196. Multilingual Matters.

Stivers, T., Enfield, N. J., Brown, P., Englert, C., Hayashi, M.,
Heinemann, T., Hoymann, G., Rossano, F., de Ruiter, J. P., Yoon, K.-E.,
and Levinson, S. C. (2009). Universals and cultural variation in
turn-taking in conversation. *Proceedings of the National Academy of
Sciences* 106(26), 10587–10592.
[doi:10.1073/pnas.0903616106](https://doi.org/10.1073/pnas.0903616106)

## See also

[`prepare_video()`](https://www.alcocerjj.com/fathom/reference/prepare_video.md)
for recordings with pictures, and
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md),
which reads the `text` column.
[`vignette("quick_start", package = "fathom")`](https://www.alcocerjj.com/fathom/articles/quick_start.md)
for installing the recognizer and the diarization token.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs ffmpeg and a recognizer, so it is not run here.

# Everything off, which is a plain transcript and needs no token and no
# optional package.
plain <- prepare_audio("hearing_04.wav")

# Two speakers known in advance, labeled turns with the silences marked.
docs <- prepare_audio(
  c("hearing_04.wav", "hearing_11.wav"),
  diarize_names = TRUE,
  annotate_timing = TRUE,
  min_speakers = 2,
  max_speakers = 2,
  hf_token = Sys.getenv("HF_TOKEN")
)

# Anything else the recognizer takes.
tuned <- prepare_audio(
  "hearing_04.wav",
  extra = c("--no_speech_threshold", "0.4", "--beam_size", "10")
)
} # }
```
