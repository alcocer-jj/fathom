# prepare_image() and prepare_audio() build a corpus of files. No network is
# touched, no audio is decoded and no recognizer is spawned. prepare_audio()
# reads a transcript that sits beside the recording and is newer than it, so
# writing one is how the whole path is exercised offline.

test_that("prepare_image reports a missing file rather than dropping it", {
  good <- tempfile(fileext = ".PNG")
  writeBin(as.raw(c(1, 2, 3, 4)), good)
  gone <- file.path(tempdir(), "no_such_file_here.png")

  out <- suppressMessages(prepare_image(c(good, gone)))

  expect_equal(nrow(out), 2L)
  expect_equal(out$exists, c(TRUE, FALSE))
  expect_equal(out$bytes[1], 4)
  expect_true(is.na(out$bytes[2]))
  # Lowercased, so a corpus of .PNG and .png sorts as one format.
  expect_equal(out$ext, c("png", "png"))
  # The default id is the file name without its extension.
  expect_equal(out$id[1], tools::file_path_sans_ext(basename(good)))

  expect_message(prepare_image(c(good, gone)), "do not resolve")
})

test_that("prepare_image refuses ids it cannot key on", {
  f <- tempfile(fileext = ".png")
  writeBin(as.raw(1), f)

  expect_error(prepare_image(character(0)), "non-empty character")
  expect_error(prepare_image(f, id = c("a", "b")), "One each")
  # Two files in different folders can share a name, which would silently
  # collapse two documents into one on the audit join.
  a <- file.path(tempdir(), "one", "page.png")
  b <- file.path(tempdir(), "two", "page.png")
  expect_error(prepare_image(c(a, b)), "ids must be unique")
})

fake_words <- function() {
  list(
    list(word = "I", start = 0.10, end = 0.20, speaker = "S0"),
    list(word = "already", start = 0.22, end = 0.60, speaker = "S0"),
    list(word = "told", start = 1.00, end = 1.25, speaker = "S0"),
    list(word = "you", start = 1.27, end = 1.40, speaker = "S0"),
    list(word = "no", start = 3.90, end = 4.10, speaker = "S1"),
    list(word = "you", start = 4.12, end = 4.30, speaker = "S1")
  )
}

# A recording with a transcript already beside it, so nothing is spawned.
#
# `diarize` is the transcription setting the sidecar records, and it defaults
# off to match `prepare_audio()`. A block that wants turns asks for
# `fake_clip(diarize = TRUE)` and calls with `diarize_names = TRUE`, and the
# two have to agree or the call is a cache miss looking for a recognizer.
#
# The settings are the ones the transcript was made under, named the same way
# the call under test names them. A test asking for anything else is asking for
# a transcript this fixture did not make, which is a cache miss and a spawned
# recognizer, so they have to be kept in step deliberately.
#
# `sidecar = FALSE` writes no record at all, which is the hand-corrected case.
fake_clip <- function(
  words = fake_words(),
  name = "clip",
  tool = "whisperx",
  asr_model = "large-v3",
  diarize = FALSE,
  language = NULL,
  extra = NULL,
  sidecar = TRUE
) {
  d <- tempfile("fathom_clip_")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  wav <- file.path(d, paste0(name, ".wav"))
  writeBin(as.raw(c(1, 2, 3, 4)), wav)
  json <- file.path(d, paste0(name, ".json"))
  jsonlite::write_json(
    list(word_segments = words, language = "en"),
    json,
    auto_unbox = TRUE
  )
  if (isTRUE(sidecar)) {
    fathom:::cache_write(
      json,
      fathom:::cache_settings(tool, asr_model, diarize, language, extra)
    )
  }
  wav
}

test_that("a cached transcript is read and the recognizer is never reached", {
  # No whisperx on the machine running this, and none needed. If the cache
  # check were not ahead of the PATH check, every test below would fail here.
  wav <- fake_clip()
  out <- prepare_audio(wav)

  expect_equal(nrow(out), 1L)
  expect_true(out$exists)
  expect_equal(out$id, "clip")
  expect_equal(out$audio, wav)
})

test_that("two switches, each writing one thing, and they compose", {
  # 1.00 - 0.60 is a 0.40 silence inside S0's turn. 3.90 - 1.40 is a 2.50
  # silence at the turn boundary. 0.22 - 0.20 is below min_pause and neither.
  plain_clip <- fake_clip()
  dia_clip <- fake_clip(diarize = TRUE)

  # Everything off. No token, no diarizer, no optional package, no markers.
  plain <- prepare_audio(plain_clip)
  expect_equal(plain$text, "I already told you no you")
  expect_false(plain$annotate_timing)
  expect_false(plain$diarized)

  # Timing alone. One run of text, since without diarization there are no
  # turns and every silence is a pause.
  tim <- prepare_audio(plain_clip, annotate_timing = TRUE)
  expect_equal(
    tim$text,
    "I already [pause 0.4s] told you [pause 2.5s] no you"
  )
  expect_true(tim$annotate_timing)

  # Names alone, which is where turns come from.
  spk <- prepare_audio(dia_clip, diarize_names = TRUE)
  expect_equal(spk$text, "[S0]: I already told you\n[S1]: no you")
  expect_true(spk$diarized)

  # Both, and the turn boundary silence is a gap rather than a pause.
  both <- prepare_audio(dia_clip, diarize_names = TRUE, annotate_timing = TRUE)
  expect_equal(
    both$text,
    "[S0]: I already [pause 0.4s] told you\n[gap 2.5s]\n[S1]: no you"
  )
})

test_that("names off means the speaker column is ignored, not just unprinted", {
  # A hand-corrected transcript can carry speakers whatever this call asked
  # for. Printing no labels but still breaking the text into turns would be
  # the text obeying something the caller switched off.
  wav <- fake_clip(sidecar = FALSE)
  out <- suppressMessages(prepare_audio(wav, annotate_timing = TRUE))

  expect_false(grepl("\n", out$text, fixed = TRUE))
  expect_false(grepl("[gap", out$text, fixed = TRUE))
  expect_true(grepl("[pause 2.5s]", out$text, fixed = TRUE))
  expect_false(out$diarized)
  expect_true(is.na(out$n_turns))
  expect_true(is.na(out$n_speakers))
  expect_true(is.na(out$n_gaps))
  # Five silences, two of them at or above min_pause, and both are pauses
  # because there is nothing to make either a transition.
  expect_equal(out$n_pauses, 2L)
})

test_that("the counts describe the recording, not the rendering", {
  wav <- fake_clip(diarize = TRUE)
  four <- list(
    prepare_audio(wav, diarize_names = TRUE, annotate_timing = TRUE),
    prepare_audio(wav, diarize_names = TRUE),
    prepare_audio(wav, diarize_names = TRUE, annotate_timing = TRUE),
    prepare_audio(wav, diarize_names = TRUE)
  )

  # Only text and the annotate_* columns move. Everything else is a fact about
  # the audio, so n_pauses holds where the text shows nothing.
  for (col in c("n_words", "n_turns", "n_speakers", "n_pauses", "n_gaps")) {
    vals <- vapply(four, function(x) x[[col]], integer(1))
    expect_equal(length(unique(vals)), 1L, info = col)
  }
  expect_equal(four[[1]]$n_words, 6L)
  expect_equal(four[[1]]$n_turns, 2L)
  expect_equal(four[[1]]$n_speakers, 2L)
  expect_equal(four[[1]]$n_pauses, 1L)
  expect_equal(four[[1]]$n_gaps, 1L)

  # Five words follow another, so five silences, and the middle one is the
  # 0.02 between "no" and "you" rather than either marked pause.
  expect_equal(four[[1]]$median_gap_s, 0.02)
})

test_that("the two rates answer different questions, and both are free", {
  # Six words spanning 4.20 s of which 1.24 s is voiced. Fast on the clock and
  # fast while speaking are different states, and the ratio between them is
  # how much of the elapsed time was pausing.
  d <- prepare_audio(fake_clip(diarize = TRUE), diarize_names = TRUE)
  expect_equal(d$speech_rate, 6 / 4.2)
  expect_equal(d$articulation_rate, 6 / 1.24)
  expect_true(d$articulation_rate > d$speech_rate)

  # Both come off word boundaries rather than off what was written, so they
  # hold with the markers off.
  plain <- prepare_audio(fake_clip())
  expect_equal(plain$speech_rate, 6 / 4.2)
  expect_equal(plain$articulation_rate, 6 / 1.24)
})

test_that("median_turn_s needs turns, and median_score needs a transcript", {
  # Turns of 1.30 s and 0.40 s.
  d <- prepare_audio(fake_clip(diarize = TRUE), diarize_names = TRUE)
  expect_equal(d$median_turn_s, 0.85)
  # Without diarization there are no turns to measure, so NA rather than the
  # whole recording counted as one.
  expect_true(is.na(prepare_audio(fake_clip())$median_turn_s))

  # The fixture writes no `score`, which is what a hand-made transcript looks
  # like, so there is nothing to report rather than a number to invent.
  expect_true(is.na(d$median_score))

  scored <- fake_words()
  for (i in seq_along(scored)) {
    scored[[i]]$score <- c(0.9, 0.8, 0.2, 0.7, 0.6, 0.5)[i]
  }
  w <- fathom:::read_timings(scored)
  expect_equal(w$score, c(0.9, 0.8, 0.2, 0.7, 0.6, 0.5))
  expect_equal(
    prepare_audio(fake_clip(scored))$median_score,
    stats::median(c(0.9, 0.8, 0.2, 0.7, 0.6, 0.5))
  )
})

test_that("a turn is a maximal run by one speaker and nothing splits it", {
  # S0 holds the floor across a 0.40 silence, so it stays one line and the
  # silence is written where it happened rather than breaking the turn.
  both <- prepare_audio(
    fake_clip(diarize = TRUE),
    diarize_names = TRUE,
    annotate_timing = TRUE
  )
  expect_equal(length(strsplit(both$text, "\n", fixed = TRUE)[[1]]), 3L)
  expect_equal(both$n_turns, 2L)

  # A speaker change with almost no silence is still a turn, since Stivers et
  # al. put the mean transition at 207 milliseconds.
  quick <- fake_words()
  quick[[5]]$start <- 1.42
  quick[[5]]$end <- 1.55
  quick[[6]]$start <- 1.57
  quick[[6]]$end <- 1.70
  q <- prepare_audio(
    fake_clip(quick, diarize = TRUE),
    diarize_names = TRUE,
    annotate_timing = TRUE
  )
  expect_equal(q$n_turns, 2L)
  # Below min_pause, so the line break carries the turn and no marker does.
  expect_equal(q$n_gaps, 0L)
  expect_false(grepl("[gap", q$text, fixed = TRUE))
})

test_that("pause and gap are told apart by diarization, not by the labels", {
  # Whether a silence is hesitation or a turn transition is a fact about the
  # recording, so the distinction holds even where no labels are written.
  tim <- prepare_audio(
    fake_clip(diarize = TRUE),
    diarize_names = TRUE,
    annotate_timing = TRUE
  )
  expect_true(grepl("[pause 0.4s]", tim$text, fixed = TRUE))
  expect_true(grepl("[gap 2.5s]", tim$text, fixed = TRUE))
  expect_equal(tim$n_pauses, 1L)
  expect_equal(tim$n_gaps, 1L)

  # The same words without diarization. Without turns the 2.50 is just another
  # silence, so it counts as a pause and n_gaps has nothing to report.
  b <- prepare_audio(fake_clip(), annotate_timing = TRUE)
  expect_true(grepl("[pause 2.5s]", b$text, fixed = TRUE))
  expect_false(grepl("[gap", b$text, fixed = TRUE))
  expect_equal(b$n_pauses, 2L)
  expect_true(is.na(b$n_gaps))
})

test_that("speaker is passed through, never invented, and never silent", {
  a <- prepare_audio(fake_clip(diarize = TRUE), diarize_names = TRUE)
  expect_true(a$diarized)
  expect_equal(a$n_speakers, 2L)

  bare <- lapply(fake_words(), function(x) {
    x$speaker <- NULL
    x
  })
  wav <- fake_clip(bare, diarize = TRUE)
  # A transcript that cannot supply the labels asked for is reported, and the
  # reason stays visible in the row afterwards.
  expect_message(prepare_audio(wav, diarize_names = TRUE), "no speaker")
  b <- suppressMessages(prepare_audio(wav, diarize_names = TRUE))
  expect_false(b$diarized)
  # NA rather than 1, so a transcript with no speaker is never read as a
  # transcript with one speaker.
  expect_true(is.na(b$n_speakers))
  expect_true(is.na(b$n_turns))
})

test_that("speakers renames, and refuses a name that is not there", {
  wav <- fake_clip(diarize = TRUE)
  a <- prepare_audio(
    wav,
    diarize_names = TRUE,
    speakers = c(S0 = "Interviewer", S1 = "Respondent")
  )
  expect_true(grepl("[Interviewer]:", a$text, fixed = TRUE))
  expect_true(grepl("[Respondent]:", a$text, fixed = TRUE))
  expect_false(grepl("[S0]:", a$text, fixed = TRUE))

  # A typo that renamed nothing would leave a corpus that looks relabelled and
  # is not, so it is an error rather than a silent no-op.
  expect_error(
    prepare_audio(wav, diarize_names = TRUE, speakers = c(S9 = "Interviewer")),
    "not in the timings"
  )
  expect_error(
    prepare_audio(wav, diarize_names = TRUE, speakers = c(S9 = "Interviewer")),
    "Present"
  )
  expect_error(
    prepare_audio(wav, diarize_names = TRUE, speakers = "Interviewer"),
    "named character vector"
  )
})

test_that("what the caller typed is refused before the machine is inspected", {
  # Literally so. With no PATH there is no recognizer to find, so anything here
  # that reached one would fail saying it could not, rather than by handing a
  # fixture to ffmpeg and reporting that.
  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  Sys.setenv(PATH = "")

  expect_error(
    prepare_audio("nowhere.wav", min_speakers = 2),
    "need `diarize_names = TRUE`"
  )
  expect_error(
    prepare_audio("nowhere.wav", extra = "--no_align"),
    "removes the word timings"
  )
  expect_error(
    prepare_audio("nowhere.wav", extra = c("--output_format=srt")),
    "found again"
  )
  expect_error(
    prepare_audio("nowhere.wav", extra = list("--beam_size")),
    "character vector"
  )
  expect_error(prepare_audio(character(0)), "non-empty character")
  expect_error(
    prepare_audio(c("a.wav", "b/a.wav")),
    "ids must be unique"
  )

  # Three independent switches make the combinations that used to need
  # refusing unrepresentable. Nothing implies another, so there is no way to
  # ask for labels and switch off the thing that produces them.
  wav <- fake_clip()
  expect_false(prepare_audio(wav)$diarized)
  expect_false(prepare_audio(wav)$transcribed)
})

test_that("a path that does not resolve is kept rather than dropped", {
  wav <- fake_clip()
  gone <- file.path(tempdir(), "no_such_recording.wav")

  expect_message(prepare_audio(c(wav, gone)), "do not resolve")
  out <- suppressMessages(prepare_audio(c(wav, gone)))

  expect_equal(nrow(out), 2L)
  expect_equal(out$exists, c(TRUE, FALSE))
  expect_true(is.na(out$text[2]))
  expect_true(is.na(out$n_words[2]))
  expect_equal(out$id, c("clip", "no_such_recording"))

  # Nothing resolving at all is an error rather than a frame of NAs.
  expect_error(prepare_audio(gone), "none of the paths")
})

test_that("provenance travels on the row, since two runs are two corpora", {
  wav <- fake_clip()
  out <- prepare_audio(wav)

  expect_false(out$transcribed)
  expect_equal(out$tool, "whisperx")
  expect_equal(out$asr_model, "large-v3")
  expect_true(is.na(out$extra))
  # Detected per file and read back off the transcript rather than assumed.
  expect_equal(out$language, "en")

  # min_pause reaches no recognizer, so it is not in the cache key and a
  # different value re-reads the same transcript rather than re-transcribing.
  expect_equal(prepare_audio(wav, min_pause = 0.5)$min_pause, 0.5)
  expect_equal(
    prepare_audio(wav, min_pause = 0.5, annotate_timing = TRUE)$n_pauses,
    1L
  )
  expect_equal(
    prepare_audio(wav, min_pause = 0.01, annotate_timing = TRUE)$n_pauses,
    5L
  )
  expect_false(prepare_audio(wav, min_pause = 0.5)$transcribed)
})

test_that("the cache asks what measure() asks, whether these settings ran", {
  # A transcript made under other settings is not an answer to this call. With
  # no recognizer reachable here, a stale record has to send it looking for
  # one, which is the whole assertion.
  wav <- fake_clip()
  expect_equal(
    fathom:::cache_state(
      sub("[.]wav$", ".json", wav),
      wav,
      fathom:::cache_settings("whisperx", "large-v3", FALSE, NULL, NULL)
    ),
    "current"
  )

  for (changed in list(
    fathom:::cache_settings("whispermlx", "large-v3", FALSE, NULL, NULL),
    fathom:::cache_settings("whisperx", "medium", FALSE, NULL, NULL),
    fathom:::cache_settings("whisperx", "large-v3", TRUE, NULL, NULL),
    fathom:::cache_settings("whisperx", "large-v3", FALSE, "es", NULL),
    fathom:::cache_settings(
      "whisperx",
      "large-v3",
      FALSE,
      NULL,
      "--beam_size 4"
    )
  )) {
    expect_equal(
      fathom:::cache_state(sub("[.]wav$", ".json", wav), wav, changed),
      "stale"
    )
  }

  # An unreadable transcript is absent rather than an error, which is what
  # ckpt_read() does with a checkpoint from a killed run.
  broken <- fake_clip()
  writeLines("{not json", sub("[.]wav$", ".json", broken))
  expect_equal(
    fathom:::cache_state(
      sub("[.]wav$", ".json", broken),
      broken,
      fathom:::cache_settings("whisperx", "large-v3", FALSE, NULL, NULL)
    ),
    "absent"
  )

  # So is a truncated settings record. A half-written one is not evidence.
  torn <- fake_clip()
  writeLines("{", fathom:::cache_sidecar(sub("[.]wav$", ".json", torn)))
  expect_equal(
    fathom:::cache_state(
      sub("[.]wav$", ".json", torn),
      torn,
      fathom:::cache_settings("whisperx", "large-v3", FALSE, NULL, NULL)
    ),
    "unverified"
  )
})

test_that("a transcript of unknown origin is used, and says it is unknown", {
  # What a hand-corrected transcript looks like. Discarding it would throw away
  # the correction, and naming settings that did not produce those words would
  # be worse than saying nothing.
  wav <- fake_clip(sidecar = FALSE)
  expect_message(prepare_audio(wav), "no record of how they were made")

  out <- suppressMessages(prepare_audio(wav))
  expect_false(out$transcribed)
  expect_true(is.na(out$tool))
  expect_true(is.na(out$asr_model))
  expect_true(is.na(out$extra))
  # Read off the transcript and applied here, so these hold either way.
  expect_equal(out$language, "en")
  expect_equal(out$min_pause, 0.2)
  expect_equal(out$n_words, 6L)
})

test_that("id names the recording and can be set", {
  wav <- fake_clip()
  expect_equal(prepare_audio(wav)$id, "clip")
  expect_equal(prepare_audio(wav, id = "hearing_04")$id, "hearing_04")
  expect_error(prepare_audio(wav, id = c("a", "b")), "one per file")
})

test_that("the three input shapes all parse, and untimed words are counted", {
  w <- fake_words()
  flat <- fathom:::read_timings(w)
  nested <- fathom:::read_timings(list(segments = list(list(words = w))))
  seg <- fathom:::read_timings(list(word_segments = w))
  expect_equal(flat$word, nested$word)
  expect_equal(flat$word, seg$word)
  expect_equal(nrow(flat), 6L)

  # An aligner leaves a token it could not place. Dropping it is right, since a
  # gap measured against a missing boundary is not a gap.
  holed <- w
  holed[[3]]$start <- NULL
  expect_message(fathom:::read_timings(holed), "no timing")
  expect_equal(nrow(suppressMessages(fathom:::read_timings(holed))), 5L)
})

test_that("a shape with no word objects anywhere is refused, not crashed on", {
  # One real recognizer writes a whole transcript as a string beside a list of
  # sentences, with sub-word pieces underneath and no `word` field at any
  # level. Neither element is a word object, so both the guard and `pull()`
  # meet something they cannot subset by name.
  other <- list(
    text = "a whole transcript as one string",
    sentences = list(
      list(
        text = " Superman.",
        start = 0.24,
        end = 1.76,
        tokens = list(list(text = " Su", start = 0.24, end = 0.48))
      )
    )
  )
  expect_error(fathom:::read_timings(other), "segment-level output")
  expect_error(fathom:::read_timings(other), "one object per word")
})

test_that("a file with no word-level timings is refused, not half read", {
  expect_error(fathom:::read_timings(list()), "no words found")
  expect_error(fathom:::read_timings("no_such_file.json"), "no file at")
  # A WhisperX segment has `text`, `start` and `end`, so falling back from
  # `word` to `text` would parse this as one paragraph-length word with no
  # gaps in it and report nothing wrong.
  seg_only <- list(list(text = "a whole segment", start = 0, end = 4))
  expect_error(fathom:::read_timings(seg_only), "segment-level output")
})

test_that("verbosity is checked like every other choice argument", {
  wav <- fake_clip()
  expect_error(prepare_audio(wav, verbosity = "loud"), "should be one of")
  expect_error(
    prepare_audio(wav, verbosity = c("quiet", "verbose")),
    "must be of length 1"
  )
  # Nothing is transcribed here, so no level says anything. The per-recording
  # line belongs to the transcription, not to reading a cached transcript.
  for (v in c("quiet", "normal", "verbose")) {
    expect_silent(prepare_audio(wav, verbosity = v))
  }
})

test_that("the missing recognizer refusal diagnoses before it prescribes", {
  # Named so that nothing could have installed it, which is what lets this run
  # everywhere. The block below covers the wiring and can only run on a machine
  # without whisperx, so what the message says is pinned here instead.
  gone <- "whisperx_no_such_tool"
  expect_error(fathom:::find_recognizer(gone), "PATH this R session")
  # The commoner cause is a shorter PATH than the shell's, so the refusal has
  # to say so before it sends anyone to reinstall what they already have.
  expect_error(fathom:::find_recognizer(gone), "Renviron")
  expect_error(fathom:::find_recognizer(gone), "uv tool install whisperx_no")
  # And it resolves rather than refuses when the tool is there.
  skip_if_not(nzchar(Sys.which("whisperx")), "whisperx is not installed here")
  expect_true(nzchar(fathom:::find_recognizer("whisperx")))
})

test_that("the recognizer is only demanded when something needs it", {
  # A recording with no transcript beside it has to reach Sys.which(), and the
  # refusal names the install line rather than the missing binary alone. The
  # skip comes first because on a machine that has whisperx this would spawn
  # it on a fixture.
  skip_if(nzchar(Sys.which("whisperx")), "whisperx is installed here")

  d <- tempfile("fathom_bare_")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  wav <- file.path(d, "untranscribed.wav")
  writeBin(as.raw(c(1, 2, 3, 4)), wav)
  expect_error(prepare_audio(wav), "PATH this R session")
})

# prepare_video() rests on ffmpeg for the frames and prepare_audio() for the
# transcript. The frame arithmetic needs neither, so it is tested against
# `pick_frames()` directly with score vectors built here.

fake_scores <- function() {
  # 300 frames over 12 seconds, cuts at 3.00, 6.00 and 9.00, everything else
  # near zero. This is the shape a real scene_score track has, three peaks two
  # orders of magnitude above a floor that is not quite flat.
  t <- seq(0, 11.96, by = 0.04)
  s <- rep(0.001, length(t))
  s[seq_along(t) %% 25 == 1] <- 0.009
  s[which.min(abs(t - 3))] <- 0.70
  s[which.min(abs(t - 6))] <- 0.89
  s[which.min(abs(t - 9))] <- 0.64
  data.frame(t = t, score = s)
}
empty_scores <- function() data.frame(t = numeric(0), score = numeric(0))

test_that("uniform divides the recording evenly and takes the middles", {
  v <- fathom:::pick_frames(empty_scores(), 12, 8, "uniform", FALSE, NULL)
  expect_equal(v, c(0.75, 2.25, 3.75, 5.25, 6.75, 8.25, 9.75, 11.25))
  # Midpoints rather than endpoints, so neither a title card nor black.
  expect_true(v[1] > 0)
  expect_true(v[length(v)] < 12)
  expect_equal(diff(v), rep(1.5, 7))

  expect_equal(
    fathom:::pick_frames(empty_scores(), 12, 4, "uniform", FALSE, NULL),
    c(1.5, 4.5, 7.5, 10.5)
  )
})

test_that("change finds the cuts that even spacing misses", {
  v <- fathom:::pick_frames(fake_scores(), 12, 8, "change", FALSE, NULL)
  for (cut in c(3, 6, 9)) {
    expect_true(any(abs(v - cut) < 0.05), info = paste("cut at", cut))
  }
  # Uniform gets no nearer than 0.75 s to any of them.
  u <- fathom:::pick_frames(empty_scores(), 12, 8, "uniform", FALSE, NULL)
  expect_true(all(vapply(c(3, 6, 9), function(k) min(abs(u - k)), 1) > 0.5))
})

test_that("min_gap stops change packing the budget into one stretch", {
  # At duration / (2 * frames) every frame fits inside a fifth of the
  # recording, and once the three peaks are used the remaining scores are
  # within a hundredth of each other, so the walk becomes index order.
  packed <- fathom:::pick_frames(fake_scores(), 12, 8, "change", FALSE, 0.75)
  loose <- fathom:::pick_frames(fake_scores(), 12, 8, "change", FALSE, NULL)
  expect_true(sum(packed < 4) > sum(loose < 4))
  expect_true(diff(range(loose)) > diff(range(packed)))
})

test_that("every rule returns exactly frames", {
  for (n in c(1, 4, 8, 16)) {
    for (sel in c("uniform", "change")) {
      for (inc in c(TRUE, FALSE)) {
        v <- fathom:::pick_frames(fake_scores(), 12, n, sel, inc, NULL)
        expect_equal(length(v), n, info = paste(sel, n, inc))
        expect_false(is.unsorted(v))
      }
    }
  }
  # A budget wider than the candidates can supply is filled rather than
  # returned short, since n_frames varying with the footage would confound
  # the count with whatever is being measured.
  wide <- fathom:::pick_frames(fake_scores(), 12, 8, "change", FALSE, 3)
  expect_equal(length(wide), 8L)
})

test_that("include_first spends a frame rather than adding one", {
  a <- fathom:::pick_frames(empty_scores(), 12, 8, "uniform", FALSE, NULL)
  b <- fathom:::pick_frames(empty_scores(), 12, 8, "uniform", TRUE, NULL)
  expect_equal(length(a), length(b))
  expect_equal(b[1], 0)
  expect_true(a[1] > 0)
  # Frame zero has no predecessor and scores exactly zero, so change can
  # never reach it on merit.
  expect_equal(
    fathom:::pick_frames(fake_scores(), 12, 4, "change", TRUE, NULL)[1],
    0
  )
})

test_that("change with no scores is uniform, not a different answer", {
  # A recording ffmpeg could not score is not a reason to sample it
  # differently, so the fallback is the other rule rather than a third one.
  expect_equal(
    fathom:::pick_frames(empty_scores(), 12, 8, "change", FALSE, NULL),
    fathom:::pick_frames(empty_scores(), 12, 8, "uniform", FALSE, NULL)
  )
})

test_that("what the caller typed is refused before ffmpeg is looked for", {
  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  Sys.setenv(PATH = "")

  expect_error(prepare_video(character(0)), "non-empty character")
  expect_error(prepare_video("a.mp4", frames = 0), "one or more")
  expect_error(prepare_video("a.mp4", frames = c(4, 8)), "single number")
  expect_error(prepare_video("a.mp4", select = "keyframe"), "should be one of")
  expect_error(prepare_video("a.mp4", id = c("a", "b")), "one per file")
  expect_error(prepare_video(c("a/x.mp4", "b/x.mp4")), "ids must be unique")
  expect_error(prepare_video("no_such_video.mp4"), "none of the paths")
})

test_that("a video path that does not resolve is kept rather than dropped", {
  skip_if_not(nzchar(Sys.which("ffmpeg")), "ffmpeg is not installed here")
  d <- tempfile("fathom_vid_")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  real <- file.path(d, "clip.mp4")
  # Four seconds of color with no audio stream at all, which is also the
  # silent case below.
  ok <- suppressWarnings(system2(
    "ffmpeg",
    c(
      "-hide_banner",
      "-loglevel",
      "error",
      "-f",
      "lavfi",
      "-i",
      "testsrc=s=64x48:d=4",
      "-r",
      "10",
      shQuote(real),
      "-y"
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  skip_if_not(file.exists(real), "ffmpeg could not write a fixture")
  gone <- file.path(d, "no_such_clip.mp4")

  expect_message(prepare_video(c(real, gone), frames = 3), "do not resolve")
  out <- suppressMessages(prepare_video(c(real, gone), frames = 3))

  expect_equal(nrow(out), 2L)
  expect_equal(out$exists, c(TRUE, FALSE))
  expect_equal(out$id, c("clip", "no_such_clip"))
  # A missing file was never opened, so has_audio asserts nothing.
  expect_true(is.na(out$has_audio[2]))
  expect_true(is.na(out$n_frames[2]))
  expect_equal(length(out$image[[2]]), 0L)
})

test_that("a silent video is a document, and the batch does not stop", {
  skip_if_not(nzchar(Sys.which("ffmpeg")), "ffmpeg is not installed here")
  d <- tempfile("fathom_vid_")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  v <- file.path(d, "silent.mp4")
  suppressWarnings(system2(
    "ffmpeg",
    c(
      "-hide_banner",
      "-loglevel",
      "error",
      "-f",
      "lavfi",
      "-i",
      "testsrc=s=64x48:d=4",
      "-r",
      "10",
      shQuote(v),
      "-y"
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  skip_if_not(file.exists(v), "ffmpeg could not write a fixture")

  expect_message(prepare_video(v, frames = 3), "no audio stream")
  out <- suppressMessages(prepare_video(v, frames = 3))

  expect_true(out$exists)
  expect_false(out$has_audio)
  # The frames are unaffected by the audio being absent.
  expect_equal(out$n_frames, 3L)
  expect_equal(length(out$image[[1]]), 3L)
  expect_true(all(file.exists(out$image[[1]])))
  # NA rather than 0. Nothing was transcribed, which is not the same as a
  # transcript that came back with no words in it.
  expect_true(is.na(out$text))
  expect_true(is.na(out$n_words))
  # NA rather than FALSE, on the same argument. Nothing was transcribed, so
  # whether there were speakers was never learned, and naming the tool that
  # did not run would assert settings that produced no words.
  expect_true(is.na(out$diarized))
  expect_true(is.na(out$tool))
  expect_true(is.na(out$asr_model))
  expect_true(is.na(out$transcribed))

  # Frames live beside the recording rather than in a temp directory, because
  # measure() reads paths and coding_sample() copies them.
  expect_true(all(startsWith(out$image[[1]], d)))
})

test_that("changing the frame settings cuts the frames again", {
  # one_video() skipped any frame file that existed and was newer than the
  # video, so a second call under other settings kept the old pictures under
  # the old names while frame_times reported the new times.
  skip_if_not(nzchar(Sys.which("ffmpeg")), "ffmpeg is not installed here")
  d <- withr::local_tempdir()
  v <- file.path(d, "clip.mp4")
  suppressWarnings(system2(
    "ffmpeg",
    c(
      "-hide_banner",
      "-loglevel",
      "error",
      "-f",
      "lavfi",
      "-i",
      "testsrc=s=64x48:d=4",
      "-r",
      "10",
      shQuote(v),
      "-y"
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  skip_if_not(file.exists(v), "ffmpeg could not write a fixture")
  on_disk <- function() {
    list.files(file.path(d, "clip_frames"), pattern = "\\.jpg$")
  }

  three <- suppressMessages(prepare_video(v, frames = 3))
  first <- unname(tools::md5sum(three$image[[1]][1]))
  expect_length(on_disk(), 3L)

  two <- suppressMessages(prepare_video(v, frames = 2))
  expect_equal(two$n_frames, 2L)
  # The third picture of the earlier set is removed rather than left behind.
  expect_length(on_disk(), 2L)
  # Two frames sit at other times than the first two of three, so the first
  # picture is a different picture.
  expect_false(identical(unname(tools::md5sum(two$image[[1]][1])), first))

  # The same settings again reuse what is on disk.
  stamp <- file.mtime(two$image[[1]])
  again <- suppressMessages(prepare_video(v, frames = 2))
  expect_equal(file.mtime(again$image[[1]]), stamp)
})

test_that("ffprobe is found beside ffmpeg, and refused when absent", {
  # probe_video() called ffprobe by name and never checked it was there, so a
  # missing one gave back the shell's complaint in place of a duration.
  d <- withr::local_tempdir()
  withr::local_envvar(PATH = d)
  expect_error(
    fathom:::find_ffprobe(file.path(d, "ffmpeg")),
    "ffprobe is not beside ffmpeg",
    fixed = TRUE
  )
  file.create(file.path(d, "ffprobe"))
  # On Windows dirname() returns / where local_tempdir() hands back \, so the
  # same file is spelled two ways. Normalizing both compares the file itself.
  found <- fathom:::find_ffprobe(file.path(d, "ffmpeg"))
  expect_equal(
    normalizePath(found, winslash = "/"),
    normalizePath(file.path(d, "ffprobe"), winslash = "/")
  )
})

test_that("the refusals give each system its own PATH line and ffmpeg command", {
  # Windows joins PATH entries with `;` rather than `:`, and R drops every
  # backslash outside quotes when it reads ~/.Renviron, after expanding
  # ${PATH}. Unquoted there, the line would strip the backslashes from every
  # directory already on the PATH.
  expect_identical(
    fathom:::renviron_path_line("unix"),
    "PATH=${PATH}:/your/dir"
  )
  expect_identical(
    fathom:::renviron_path_line("windows"),
    "PATH=\"${PATH};C:\\your\\dir\""
  )
  # The install commands are the quick start's, one per system.
  hint <- fathom:::ffmpeg_install_hint
  expect_match(hint("Darwin"), "brew install ffmpeg", fixed = TRUE)
  expect_match(
    hint("Windows"),
    "winget install -e --id Gyan.FFmpeg",
    fixed = TRUE
  )
  expect_match(hint("Linux"), "sudo apt install ffmpeg", fixed = TRUE)
  expect_match(hint("SunOS"), "https://www.ffmpeg.org", fixed = TRUE)

  # Each refusal carries what fits the system it runs on. fmt_err() wraps at
  # 80 columns, so the spaces it breaks on are put back before matching.
  refusal <- function(expr) {
    gsub("[[:space:]]+", " ", tryCatch(expr, error = conditionMessage))
  }
  d <- withr::local_tempdir()
  withr::local_envvar(PATH = d)
  line <- fathom:::renviron_path_line()
  expect_match(
    refusal(fathom:::find_recognizer("whisperx_no_such_tool")),
    line,
    fixed = TRUE
  )
  expect_match(refusal(fathom:::find_ffmpeg()), line, fixed = TRUE)
  expect_match(refusal(fathom:::find_ffmpeg()), hint(), fixed = TRUE)
  expect_match(
    refusal(fathom:::find_ffprobe(file.path(d, "ffmpeg"))),
    line,
    fixed = TRUE
  )
})

test_that("the recognizer receives a path holding spaces as one argument", {
  skip_on_os("windows")
  dir <- file.path(withr::local_tempdir(), "my recordings")
  dir.create(dir)
  wav <- file.path(dir, "clip one.wav")
  file.create(wav)
  # A stand-in recognizer that writes its JSON where --output_dir says, which
  # is a directory that exists only when the path arrived whole.
  bin <- file.path(dir, "fake recognizer")
  writeLines(
    c(
      "#!/bin/sh",
      "out=''",
      "while [ $# -gt 0 ]; do",
      "  if [ \"$1\" = \"--output_dir\" ]; then out=\"$2\"; fi",
      "  shift",
      "done",
      "echo '{}' > \"$out/clip one.json\""
    ),
    bin
  )
  Sys.chmod(bin, "755")
  json <- fathom:::run_recognizer(
    x = wav,
    out_dir = dir,
    bin = bin,
    asr_model = "large-v3",
    diarize = FALSE,
    min_speakers = NULL,
    max_speakers = NULL,
    language = NULL,
    hf_token = NULL,
    extra = NULL
  )
  expect_true(file.exists(json))
})

test_that("scene scores are read off the log ffmpeg writes", {
  # Lines as ffmpeg 9 writes them, with a line from elsewhere in ffmpeg landing
  # between a frame and its score, and the closing stats line after them.
  log <- c(
    "Stream mapping:",
    "[Parsed_metadata_1 @ 0x7f674c001780] frame:0    pts:0       pts_time:0",
    "[Parsed_metadata_1 @ 0x7f674c001780] lavfi.scene_score=0.000000",
    "[Parsed_metadata_1 @ 0x7f674c001780] frame:30   pts:30720   pts_time:3",
    "Output #0, null, to 'pipe:':",
    "[Parsed_metadata_1 @ 0x7f674c001780] lavfi.scene_score=0.400000",
    "frame=  120 fps=0.0 q=-0.0 Lsize=N/A time=00:00:11.90 bitrate=N/A speed= 600x"
  )
  s <- fathom:::scene_lines(log)
  expect_equal(s$t, c(0, 3))
  expect_equal(s$score, c(0, 0.4))
})
