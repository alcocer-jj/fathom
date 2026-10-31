# Turn videos into a corpus carrying both what was shown and what was said

Turns each recording into one row, with frames in a list column and a
transcript beside them. `measure(text = , image = )` then reads both on
the same call, since they are two columns of one row rather than two
objects to align.

The audio half is
[`prepare_audio()`](https://www.alcocerjj.com/fathom/reference/prepare_audio.md)
unchanged, running on a wav `ffmpeg` extracts.

## Usage

``` r
prepare_video(
  x,
  ...,
  frames = 8,
  select = c("uniform", "change"),
  include_first = FALSE,
  min_gap = NULL,
  quality = 3,
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

  A character vector of paths to video files, one row each. A vector
  rather than a directory, so
  [`list.files()`](https://rdrr.io/r/base/list.files.html) is where the
  pattern lives.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- frames:

  An integer giving the number of frames per recording. Fixed across the
  corpus. Default is 8.

- select:

  A string naming the rule that chooses the frames. `"uniform"` spreads
  them evenly and is the default. `"change"` takes the highest scene
  scores, which finds cuts and concentrates on visually busy stretches.

- include_first:

  A logical value indicating whether the opening frame is always taken.
  It spends one of `frames` rather than adding to it, so `n_frames` does
  not vary with a flag. Frame zero has no predecessor and scores exactly
  zero, so `change` can never reach it otherwise, and an establishing
  shot is sometimes the most informative picture in a recording. Default
  is `FALSE`.

- min_gap:

  A number giving the seconds below which two chosen frames may not sit.
  `"change"` only, and ignored rather than refused under `"uniform"`,
  where even spacing cannot bunch. Defaults to `duration / frames`,
  which says no two frames sit closer than an even sample would put
  them, so `"change"` moves frames toward the action without being able
  to pack them into one stretch. Where that leaves fewer candidates than
  the budget, the remainder go in the middle of the widest space left.

- quality:

  An integer giving the JPEG quality passed to `ffmpeg` as `-q:v`, 2
  best to 31 worst. Default is 3.

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

  Passed to
  [`prepare_audio()`](https://www.alcocerjj.com/fathom/reference/prepare_audio.md)
  on the extracted audio, behaving there exactly as on a recording
  handed to it directly.

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

  A string giving the directory where frames, the wav and the transcript
  are written and looked for. Defaults to beside each recording.

- force:

  A logical value indicating whether to extract and transcribe again
  even where usable output is cached. Default is `FALSE`.

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

A tibble of one row per file, carrying every column
[`prepare_audio()`](https://www.alcocerjj.com/fathom/reference/prepare_audio.md)
returns plus these.

- `video` is the recording, and `has_audio` says whether it had a sound
  track at all.

- `image` is a list column of frame paths in time order, which is the
  shape `measure(image = )` takes for several pictures per document.

- `n_frames` and `frame_times` say how many were taken and when.

- `select` and `include_first` name which rule chose them.

- `scene_median` and `scene_max` are the middle and largest scene score
  across every frame, which say whether the recording had cuts in it or
  whether the spacing did the work. Read before trusting `"change"`.

## Choosing frames

`select = "uniform"` divides the recording into `frames` equal segments
and takes the middle of each. Midpoints rather than endpoints, since a
frame at exactly zero is often a title card and one at exactly the end
is often black.

`select = "change"` takes the `frames` highest scene scores instead,
which `ffmpeg` computes for every frame as how much this picture differs
from the one before it. It finds cuts that even spacing misses, and it
concentrates. `min_gap` is what stops that, and it exists only for this
mode.

## Recordings without audio

`ffprobe` is asked whether an audio stream exists before anything is
extracted. Where there is none, the row comes back with its frames and
with `has_audio = FALSE`, `text` and every audio count `NA`, and the
count is reported. A batch of campaign advertisements does not stop
because three of them are silent.

`exists` and `has_audio` answer different questions and both are needed.

|                     |          |             |                |
|---------------------|----------|-------------|----------------|
| case                | `exists` | `has_audio` | `text`         |
| file not found      | `FALSE`  | `NA`        | `NA`           |
| present, silent     | `TRUE`   | `FALSE`     | `NA`           |
| present, with audio | `TRUE`   | `TRUE`      | the transcript |

`NA` on `has_audio` is doing work. The file was never opened, so `FALSE`
would assert something that was not checked.

`n_words` is `NA` for a silent recording rather than `0`, because
nothing was transcribed. A recording whose audio track is silence is a
different thing, gets transcribed, and comes back with `n_words = 0`.

Every other audio column is `NA` there too, `diarized` and `tool` and
`asr_model` included. Nothing ran, so there is nothing to report and
naming a model would assert settings that produced no words.

## Frame storage

On disk beside the recording, in `<stem>_frames/`, because
`measure(image = )` reads paths.

The wav lands beside it, which is what lets
[`prepare_audio()`](https://www.alcocerjj.com/fathom/reference/prepare_audio.md)'s
cache work unchanged. The frames keep a record of their own,
`<stem>_frames.fathom.json` in the frames directory, holding the times
they were cut at and the JPEG quality. Changing `frames`, `select`,
`include_first`, `min_gap` or `quality` changes that record, so every
frame is cut again and the old pictures are removed, while the
transcript is read back from its own cache.

## See also

[`prepare_audio()`](https://www.alcocerjj.com/fathom/reference/prepare_audio.md),
which transcribes the sound track, and
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md),
which reads both columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs ffmpeg, a recognizer and an Ollama server, so it is not run here.

ads <- list.files("campaign_ads", "[.]mp4$", full.names = TRUE)
ins <- instrument(c(attack = "Does this advertisement attack an opponent?"))

# Frames spread evenly, and whatever was said alongside them.
d <- prepare_video(ads, frames = 8)
measure(
  d,
  backend = ollama_backend("gemma3:27b"),
  instrument = ins,
  text = "text",
  image = "image"
)

# More frames, chosen where the picture moved, with the speakers labeled.
d <- prepare_video(
  ads,
  frames = 16,
  select = "change",
  diarize_names = TRUE,
  annotate_timing = TRUE,
  hf_token = Sys.getenv("HF_TOKEN")
)
} # }
```
