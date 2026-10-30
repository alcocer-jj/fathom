#' Turn a folder of images into a corpus measure() can take
#'
#' @description
#' Builds a tibble of one row per file, with the columns the rest of the package
#' expects. `id` keys the results and the audit joins, `image` is the path
#' `measure()` reads, and `bytes` is a size covariate.
#'
#' Nothing is decoded. `bytes` comes from `file.size()`, which needs no image
#' reader and no system library.
#'
#' @section Missing files:
#' A path that does not resolve comes back with `exists = FALSE` and `bytes` as
#' `NA` rather than being silently removed, because a corpus that quietly lost
#' four documents is the failure this exists to prevent. Filter on `exists`
#' after reading the count.
#'
#' @param paths A character vector of file paths.
#' @param id A character vector of identifiers, one per path. Defaults to the
#'   file name without its extension, which is what a corpus of scanned pages
#'   usually keys on.
#' @return A tibble with `id`, `image`, `bytes`, `exists` and `ext`.
#' @seealso [measure()], which reads the `image` column.
#' @examples
#' f <- tempfile(fileext = ".png")
#' grDevices::png(f, width = 8, height = 8)
#' graphics::par(mar = c(0, 0, 0, 0))
#' graphics::plot.new()
#' grDevices::dev.off()
#'
#' prepare_image(f)
#' @export
prepare_image <- function(paths, id = NULL) {
  if (!is.character(paths) || length(paths) == 0L) {
    stop(
      fmt_err("`paths` must be a non-empty character vector."),
      call. = FALSE
    )
  }
  if (!is.null(id) && length(id) != length(paths)) {
    stop(
      fmt_err(sprintf(
        "`id` has %d value(s) for %d path(s). One each.",
        length(id),
        length(paths)
      )),
      call. = FALSE
    )
  }

  ids <- as.character(id %||% tools::file_path_sans_ext(basename(paths)))
  if (anyDuplicated(ids)) {
    dup <- unique(ids[duplicated(ids)])
    stop(
      fmt_err(
        paste0(
          "ids must be unique. Repeated: ",
          paste(utils::head(dup, 5L), collapse = ", "),
          "."
        ),
        paste0(
          "Two files in different folders share a name. Pass `id` yourself, ",
          "or include the folder in it."
        )
      ),
      call. = FALSE
    )
  }

  present <- file.exists(paths)
  out <- tibble::tibble(
    id = ids,
    image = as.character(paths),
    bytes = ifelse(present, file.size(paths), NA_real_),
    exists = present,
    ext = tolower(tools::file_ext(paths))
  )

  if (any(!present)) {
    message(fmt_note(
      "Warning",
      sprintf(
        "%d of %d path(s) do not resolve, kept with `exists = FALSE`.",
        sum(!present),
        length(present)
      ),
      paste0(
        "First: ",
        paste(utils::head(paths[!present], 3L), collapse = ", "),
        "."
      )
    ))
  }
  out
}

#' Turn recordings into a corpus of annotated transcripts
#'
#' @description
#' Transcribes a recording and writes its pauses and speakers back into the
#' text, so `measure(text = )` reads a transcript that carries them. A plain
#' transcript records what was said and discards how. Pauses, hesitation and who
#' was speaking leave with it, and for a construct that lives in delivery rather
#' than in wording that is the absence of the thing being measured.
#'
#' @section Caching:
#' A transcript already on disk is read instead of transcribing again, so an
#' interrupted run resumes and a hand-corrected transcript survives. `force =
#' TRUE` returns to the recognizer.
#'
#' What counts as already done is the question [measure()] asks of its
#' checkpoints, which is whether the work was done under the settings now being
#' requested rather than merely done. `tool`, `asr_model`, `diarize_names`,
#' `language` and `extra` are recorded in a `.fathom.json` beside each
#' transcript, and a run whose settings differ transcribes again. Without that,
#' a call with `diarize_names` off in a directory that already holds a diarized
#' transcript would read the diarized one and quietly ignore the argument.
#'
#' `annotate_timing`, `min_pause` and `speakers` are not in that record, since
#' none of them reaches the recognizer. Changing a rendering costs nothing and
#' re-reads the same transcript.
#'
#' A transcript with no record beside it is used rather than discarded, because
#' that is what a hand-corrected one looks like. Its `tool`, `asr_model` and
#' `extra` come back `NA`, since naming settings that did not produce those
#' words would be worse than saying nothing.
#'
#' @section Transcript format:
#' The row a model is asked about is the whole recording, and the structure
#' inside it is written into `text` rather than spread across rows.
#'
#' Inside a row, a line is a turn, meaning a maximal run of words by one
#' speaker. Nothing splits a turn further. A silence inside one person's talk is
#' written where it happened, as `[pause 0.4s]`, which says how long it was
#' rather than only that it passed a threshold.
#'
#' Silence at a turn boundary is written `[gap 0.8s]` and belongs to neither
#' speaker, so it goes on a line of its own. Whether a silence is a hesitation
#' or a turn transition is a fact about the recording, so the two markers are
#' told apart by whether the timings carry a speaker, not by whether labels were
#' asked for.
#'
#' Lines are newline separated with the speaker in brackets ahead of them.
#'
#' @section Speakers:
#' Speakers reach the output only from the recognizer. Absent, `n_speakers` and
#' `n_turns` are `NA` and `diarized` is `FALSE`, so a transcript with no speaker
#' in it is never read as a transcript with one speaker in it.
#'
#' `speakers` renames what the recognizer emitted, since `SPEAKER_00` carries
#' nothing a model can use. It is also the fastest way to put the answer in the
#' prompt. A speaker renamed `Angry Protester` has been coded before the model
#' saw the text, and nothing in the results would show it.
#'
#' @section Pause and gap counts:
#' `n_pauses` and `n_gaps` count what `min_pause` found in the timings, so they
#' hold with `annotate_timing` off, where the text shows nothing. Only `text`
#' and `annotate_timing` change with the rendering.
#'
#' None of them reaches the model, since `measure()` reads `id` and `text` and
#' nothing else.
#'
#' `n_pauses` also moves with diarization on the same words, because without
#' turns a turn transition is just another silence.
#'
#' @param x A character vector of paths to audio files, one row each. A path
#'   that does not resolve comes back with `exists = FALSE` rather than being
#'   dropped.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param diarize_names A logical value indicating whether to label who is
#'   speaking. This is a transcription setting rather than a rendering one. It
#'   runs the diarizer, needs `hf_token`, and is in the cache key, so switching
#'   it re-transcribes. It also decides whether the text has turns in it at all,
#'   since a turn is a run of words by one speaker. Off, every silence is a
#'   `[pause]` and the transcript is one run of text. Default is `FALSE`.
#' @param annotate_timing A logical value indicating whether silences are
#'   written into the text, as `[pause 0.4s]` inside a turn and `[gap 0.8s]`
#'   between two. Default is `FALSE`.
#' @param tool A string naming the recognizer to run, `"whisperx"` (the default)
#'   or `"whispermlx"`. Both emit the same JSON.
#' @param asr_model A string naming the Whisper checkpoint, which decides the
#'   words. Both tools take the same short names. The command-line default is
#'   `small`, which is not the default here. Default is `"large-v3"`.
#' @param min_speakers,max_speakers Integers passed to the diarizer when known,
#'   or `NULL` (the default). An interview has two speakers and the researcher
#'   knows it beforehand, which is the cheapest reduction in diarization error
#'   available.
#' @param language A string giving the spoken language, detected per file when
#'   `NULL` (the default).
#' @param hf_token A string giving a HuggingFace read token, required when
#'   `diarize_names` is on. Default is `NULL`.
#' @param extra A character vector of further flags for the recognizer, passed
#'   through verbatim and recorded on the result. Each element is its own
#'   argument, so `c("--beam_size", "10")` rather than one string. Default is
#'   `NULL`.
#' @param min_pause A number giving the seconds below which a silence is not
#'   marked or counted. Jefferson leaves anything under 0.2 as an untimed
#'   micropause and gives a figure above it, which is where the default comes
#'   from.
#' @param speakers A named character vector renaming the speakers, such as
#'   `c(SPEAKER_00 = "Interviewer")`. A name not in the timings is refused
#'   rather than ignored. Default is `NULL`.
#' @param id A character vector of identifiers, one per file. Defaults to the
#'   file names without their extensions, and must be unique.
#' @param cache_dir A string giving the directory where transcripts are written
#'   and looked for. Defaults to beside each recording.
#' @param force A logical value indicating whether to transcribe again even
#'   where a usable transcript is cached. Default is `FALSE`.
#' @param verbosity A string setting how much to report.
#'
#'   - `"quiet"` reports nothing.
#'   - `"normal"` is the default. One line per recording as it is transcribed,
#'     or in an interactive session a bar redrawn in place when there is more
#'     than one.
#'     The recognizer's own output is held rather than printed, and appears only
#'     if the call fails.
#'   - `"verbose"` lets the recognizer write to the console as it runs. These
#'     tools report their dependencies at length on every invocation, so this is
#'     for one long file rather than for a corpus.
#' @return A tibble of one row per file.
#'
#' - `id`, `audio` and `exists` identify the recording.
#' - `text` is the transcript, with turns where `diarize_names` was on and
#'   silences where `annotate_timing` was.
#' - `duration_s` and `n_words` hold what the recording holds.
#' - `n_turns` and `n_speakers` are `NA` when the timings carry no speaker.
#' - `n_pauses` counts the silences inside a turn at or above `min_pause`.
#' - `n_gaps` counts the silences at a turn boundary, `NA` when not diarized.
#' - `median_gap_s` is the middle silence between two words.
#' - `speech_rate` and `articulation_rate` are words per second over the whole
#'   recording, and over voiced time only. The first counts the silences and the
#'   second does not, so the ratio between them is how much of the elapsed time
#'   was pausing rather than talking.
#' - `median_turn_s` is the middle turn, `NA` without diarization.
#' - `median_score` is the middle alignment confidence, `NA` where the
#'   transcript carries none. Every silence figure rests on the word boundaries
#'   this describes, so it is what says how far to trust them.
#' - `diarized` says whether the transcript carried speakers, which is also
#'   whether the text has turns in it.
#' - `annotate_timing` says whether the silences were written in.
#' - `transcribed` says whether this call produced the transcript.
#' - `tool`, `asr_model` and `extra` hold what made the words, or `NA` where a
#'   transcript was found with no record beside it. `transcribed` and these
#'   together say whether a row was made now, read from a verified transcript,
#'   or read from one of unknown origin.
#' - `language` and `min_pause` are read off the transcript or applied by this
#'   call, so both hold however the transcript was obtained.
#' @references
#'   Jefferson, G. (1989). Preliminary notes on a possible metric which
#'   provides for a standard maximum silence of approximately one second in
#'   conversation. In D. Roger and P. Bull (eds), \emph{Conversation. An
#'   Interdisciplinary Perspective}, 166--196. Multilingual Matters.
#'
#'   Stivers, T., Enfield, N. J., Brown, P., Englert, C., Hayashi, M.,
#'   Heinemann, T., Hoymann, G., Rossano, F., de Ruiter, J. P., Yoon, K.-E.,
#'   and Levinson, S. C. (2009). Universals and cultural variation in
#'   turn-taking in conversation. \emph{Proceedings of the National Academy
#'   of Sciences} 106(26), 10587--10592. \doi{10.1073/pnas.0903616106}
#' @seealso [prepare_video()] for recordings with pictures, and [measure()],
#'   which reads the `text` column. `vignette("quick_start", package =
#'   "fathom")` for installing the recognizer and the diarization token.
#' @examples
#' \dontrun{
#' # Needs ffmpeg and a recognizer, so it is not run here.
#'
#' # Everything off, which is a plain transcript and needs no token and no
#' # optional package.
#' plain <- prepare_audio("hearing_04.wav")
#'
#' # Two speakers known in advance, labeled turns with the silences marked.
#' docs <- prepare_audio(
#'   c("hearing_04.wav", "hearing_11.wav"),
#'   diarize_names = TRUE,
#'   annotate_timing = TRUE,
#'   min_speakers = 2,
#'   max_speakers = 2,
#'   hf_token = Sys.getenv("HF_TOKEN")
#' )
#'
#' # Anything else the recognizer takes.
#' tuned <- prepare_audio(
#'   "hearing_04.wav",
#'   extra = c("--no_speech_threshold", "0.4", "--beam_size", "10")
#' )
#' }
#' @export
prepare_audio <- function(
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
) {
  no_dots(..., .fn = "prepare_audio")
  tool <- match.arg(tool)
  verbosity <- match.arg(verbosity)

  if (!is.character(x) || !length(x)) {
    stop(
      fmt_err("`x` must be a non-empty character vector of file paths."),
      call. = FALSE
    )
  }
  ids <- if (is.null(id)) {
    tools::file_path_sans_ext(basename(x))
  } else {
    as.character(id)
  }
  if (length(ids) != length(x)) {
    stop(fmt_err("`id` must be one per file, or NULL."), call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop(
      fmt_err(
        "ids must be unique.",
        paste0(
          "Repeated: ",
          paste(unique(ids[duplicated(ids)]), collapse = ", "),
          "."
        )
      ),
      call. = FALSE
    )
  }

  # Everything the caller typed is checked before the environment is, so a
  # contradiction is reported on a machine with no recognizer installed.
  #
  # There are fewer of these than there were. Three independent switches make
  # the combinations that used to need refusing unrepresentable, since nothing
  # is any longer implied by a shorthand.
  if (!diarize_names && (!is.null(min_speakers) || !is.null(max_speakers))) {
    stop(
      fmt_err(paste0(
        "`min_speakers` and `max_speakers` need `diarize_names = TRUE`. ",
        "Without diarization there is nothing for them to bound."
      )),
      call. = FALSE
    )
  }
  refuse_blocked(extra)

  keep <- file.exists(x)
  if (!any(keep)) {
    stop(
      fmt_err(
        "none of the paths in `x` resolve to a file.",
        paste0("First: ", x[1], ".")
      ),
      call. = FALSE
    )
  }
  if (any(!keep)) {
    message(fmt_note(
      "Warning",
      sprintf(
        "%d of %d paths do not resolve. Those rows carry exists = FALSE.",
        sum(!keep),
        length(keep)
      )
    ))
  }

  dirs <- if (is.null(cache_dir)) dirname(x) else rep_len(cache_dir, length(x))
  jsons <- file.path(
    dirs,
    paste0(tools::file_path_sans_ext(basename(x)), ".json")
  )
  settings <- cache_settings(tool, asr_model, diarize_names, language, extra)
  state <- rep("absent", length(x))
  for (i in which(keep)) {
    state[i] <- if (force) "absent" else cache_state(jsons[i], x[i], settings)
  }
  fresh <- state %in% c("current", "unverified")

  # The recognizer is only needed for what is not already transcribed, so a
  # fully cached corpus reads on a machine that has never had one installed.
  if (any(keep & !fresh)) {
    bin <- find_recognizer(tool)
    if (diarize_names && !nzchar(hf_token %||% "")) {
      stop(
        fmt_err(
          paste0(
            "`diarize_names = TRUE` needs `hf_token`. The diarization ",
            "model is ",
            "gated on HuggingFace."
          ),
          paste0(
            "Accept the conditions at https://huggingface.co/pyannote/",
            "speaker-diarization-community-1 then create a read token at ",
            "https://hf.co/settings/tokens. Leave `diarize_names` off for ",
            "a ",
            "transcript with no speakers, which needs neither."
          )
        ),
        call. = FALSE
      )
    }
    todo <- which(keep & !fresh)
    # A bar when more than one recording is transcribed and use_bar() says one
    # can be shown. Otherwise a line as each recording starts, which on a slow
    # transcription says which file the time is going to. A single recording
    # gets the line, since a bar of one says nothing more.
    talk <- !identical(verbosity, "quiet")
    run_env <- environment()
    pb <- NULL
    pb_line <- ""
    if (talk && length(todo) > 1L && use_bar(identical(verbosity, "verbose"))) {
      pb <- cli::cli_progress_bar(
        total = length(todo),
        format = "{pb_line}",
        clear = FALSE,
        .envir = run_env
      )
    }
    t0 <- Sys.time()
    for (k in seq_along(todo)) {
      i <- todo[k]
      if (talk && is.null(pb)) {
        progress_message(sprintf(
          "Transcribing  %*d/%d  %s",
          nchar(length(todo)),
          k,
          length(todo),
          basename(x[i])
        ))
      }
      dir.create(dirs[i], showWarnings = FALSE, recursive = TRUE)
      run_recognizer(
        x = x[i],
        out_dir = dirs[i],
        bin = bin,
        asr_model = asr_model,
        diarize = diarize_names,
        min_speakers = min_speakers,
        max_speakers = max_speakers,
        language = language,
        hf_token = hf_token,
        extra = extra,
        verbose = identical(verbosity, "verbose")
      )
      cache_write(jsons[i], settings)
      state[i] <- "current"
      if (!is.null(pb)) {
        el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        pb_line <- progress_line(
          k,
          length(todo),
          el / k,
          bar = TRUE,
          verb = "Transcribing",
          unit = "file"
        )
        cli::cli_progress_update(id = pb, set = k, .envir = run_env)
      }
    }
    if (!is.null(pb)) {
      cli::cli_progress_done(id = pb, .envir = run_env)
    }
  }

  unverified <- keep & state == "unverified"
  if (any(unverified) && !identical(verbosity, "quiet")) {
    message(fmt_note(
      "Note",
      sprintf(
        paste0(
          "%d transcript(s) carry no record of how they were made, so tool, ",
          "asr_model and extra are NA on those rows. Use force = TRUE to ",
          "transcribe them again."
        ),
        sum(unverified)
      )
    ))
  }

  rows <- lapply(seq_along(x), function(i) {
    if (!keep[i]) {
      return(empty_row(
        ids[i],
        x[i],
        tool,
        asr_model,
        language,
        min_pause,
        extra,
        annotate_timing
      ))
    }
    render_recording(
      timings = jsons[i],
      id = ids[i],
      audio = x[i],
      diarize_names = diarize_names,
      annotate_timing = annotate_timing,
      min_pause = min_pause,
      speakers = speakers,
      tool = tool,
      asr_model = asr_model,
      language = language,
      extra = extra,
      transcribed = !fresh[i],
      verified = state[i] == "current"
    )
  })
  dplyr::bind_rows(rows)
}

#' Flags the caller may not pass through `extra`
#'
#' Four of them, each because this function depends on the setting. The output
#' format and directory are how the transcript is found again, alignment is
#' where every timing comes from, and diarization has its own argument.
#'
#' @param extra As passed to [prepare_audio()].
#' @return `NULL`, invisibly.
#' @keywords internal
#' @noRd
refuse_blocked <- function(extra) {
  if (is.null(extra)) {
    return(invisible(NULL))
  }
  if (!is.character(extra)) {
    stop(
      fmt_err(paste0(
        "`extra` must be a character vector, one element per argument, such ",
        "as c(\"--beam_size\", \"10\")."
      )),
      call. = FALSE
    )
  }
  blocked <- c(
    "--output_format" = "it is how the transcript is found again",
    "--output_dir" = "it is how the transcript is found again",
    "--no_align" = "it removes the word timings this is built on",
    "--diarize" = "`diarize_names` sets it"
  )
  # Split on `=` first, so --output_format=json is caught like the spaced form.
  given <- sub("=.*$", "", extra[startsWith(extra, "--")])
  hit <- given[given %in% names(blocked)]
  if (length(hit)) {
    stop(
      fmt_err(paste0(
        "`extra` cannot carry ",
        hit[1],
        ", since ",
        blocked[[hit[1]]],
        "."
      )),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' The recognizer binary, or what to do about its absence
#'
#' `Sys.which()` reads the PATH of the R session, which is not always the PATH
#' of the shell that installed the tool. On macOS in particular a recognizer
#' that answers in the terminal can be invisible here, so the refusal names
#' that before it names installation. Telling someone to install what they
#' already have sends them to fix the wrong thing.
#'
#' Where it is found is not searched for. A binary this reaches by guessing at
#' install locations would be one `Rscript` and `R CMD check` still could not
#' see, so the shorter PATH is reported rather than worked around.
#'
#' @param tool As passed to [prepare_audio()].
#' @return A path.
#' @keywords internal
#' @noRd
find_recognizer <- function(tool) {
  bin <- Sys.which(tool)
  if (!nzchar(bin)) {
    stop(
      fmt_err(
        paste0(
          tool,
          " is not on the PATH this R session sees, so nothing can be ",
          "transcribed."
        ),
        paste0(
          "R does not always inherit the PATH a shell uses, so a tool that ",
          "runs in your terminal can be invisible here. Check ",
          "Sys.getenv(\"PATH\") before reinstalling, and add a directory ",
          "permanently with PATH=${PATH}:/your/dir in ~/.Renviron."
        ),
        paste0(
          "If it is genuinely absent, `uv tool install ",
          tool,
          "` installs it. `uvx ",
          tool,
          "` runs it without putting it on PATH."
        )
      ),
      call. = FALSE
    )
  }
  unname(bin)
}

#' One call to the recognizer
#'
#' Output is captured rather than inherited, because these tools write tens of
#' lines about their own dependencies on every invocation and a corpus of forty
#' recordings would bury everything else. What is captured is not discarded.
#' It is held and printed with the error if the call fails, which puts the
#' diagnosis in the condition rather than in the scrollback above it.
#'
#' `verbose` inherits instead, for the case where progress on one long file
#' matters more than quiet.
#'
#' @param x A string giving one audio path.
#' @param out_dir A string giving the directory where the JSON is written.
#' @param bin A string giving the resolved binary.
#' @param asr_model,diarize,min_speakers,max_speakers,language,hf_token,extra
#'   As passed to [prepare_audio()].
#' @param verbose A logical value indicating whether to let the recognizer write
#'   to the console.
#' @return The JSON path, invisibly.
#' @keywords internal
#' @noRd
run_recognizer <- function(
  x,
  out_dir,
  bin,
  asr_model,
  diarize,
  min_speakers,
  max_speakers,
  language,
  hf_token,
  extra,
  verbose = FALSE
) {
  args <- c(
    x,
    "--model",
    asr_model,
    "--output_format",
    "json",
    "--output_dir",
    out_dir
  )
  if (!is.null(language)) {
    args <- c(args, "--language", language)
  }
  if (diarize) {
    args <- c(args, "--diarize", "--hf_token", hf_token)
    if (!is.null(min_speakers)) {
      args <- c(args, "--min_speakers", as.character(min_speakers))
    }
    if (!is.null(max_speakers)) {
      args <- c(args, "--max_speakers", as.character(max_speakers))
    }
  }
  # Quoted, since system2() passes its arguments to the shell as typed, and a
  # path holding a space or an apostrophe would otherwise split in two. The
  # ffmpeg calls quote their paths for the same reason.
  argv <- shQuote(c(args, extra))
  if (verbose) {
    status <- system2(bin, argv)
    said <- character(0)
  } else {
    said <- suppressWarnings(
      system2(bin, argv, stdout = TRUE, stderr = TRUE)
    )
    status <- attr(said, "status")
    if (is.null(status)) {
      status <- 0L
    }
  }
  if (!identical(as.integer(status), 0L)) {
    # The tail unwrapped, since wrapping a traceback at 80 columns makes it
    # harder to read than leaving it alone.
    stop(
      fmt_err(paste0(
        basename(bin),
        " exited with status ",
        status,
        " on ",
        basename(x),
        "."
      )),
      if (length(said)) {
        paste0("\n\n", paste(utils::tail(said, 25L), collapse = "\n"))
      } else {
        ""
      },
      call. = FALSE
    )
  }
  invisible(file.path(
    out_dir,
    paste0(tools::file_path_sans_ext(basename(x)), ".json")
  ))
}

#' One recording, read and rendered
#'
#' @param timings A string giving the path to the transcript.
#' @param id,audio,min_pause,speakers What the row is built from.
#' @param diarize_names,annotate_timing As passed to [prepare_audio()].
#' @param tool,asr_model,language,extra Character vectors recording provenance.
#' @param transcribed A logical value indicating whether this call produced the
#'   transcript.
#' @param verified A logical value indicating whether the settings that produced
#'   the transcript are known.
#' @return A one-row tibble.
#' @keywords internal
#' @noRd
render_recording <- function(
  timings,
  id,
  audio,
  diarize_names,
  annotate_timing,
  min_pause,
  speakers,
  tool,
  asr_model,
  language,
  extra,
  transcribed,
  verified
) {
  if (!file.exists(timings)) {
    stop(
      fmt_err(
        paste0("no transcript at ", timings, "."),
        paste0(
          "The recognizer exited cleanly without writing one. Check its ",
          "output above."
        )
      ),
      call. = FALSE
    )
  }
  parsed <- jsonlite::fromJSON(timings, simplifyVector = FALSE)
  w <- read_timings(parsed)
  # `diarize_names = FALSE` ignores the speaker column outright rather than
  # merely declining to print it. Otherwise a hand-corrected transcript that
  # happens to carry speakers would reintroduce turns, and the text would come
  # back broken into lines by something the caller switched off.
  if (!diarize_names) {
    w$speaker <- NA_character_
  } else {
    w$speaker <- rename_speakers(w$speaker, speakers)
  }
  diarized <- any(!is.na(w$speaker))

  if (diarize_names && !diarized) {
    message(fmt_note(
      "Note",
      paste0(
        "The transcript for ",
        id,
        " carries no speaker, so no labels are ",
        "written and every silence is a pause. `diarized` records that."
      )
    ))
  }
  label <- diarized
  timed <- annotate_timing

  turn <- turns_of(w)
  parts <- lapply(unique(turn), function(k) w[turn == k, , drop = FALSE])
  between <- vapply(
    seq_along(parts)[-1L],
    function(i) parts[[i]]$start[1L] - max(parts[[i - 1L]]$end),
    numeric(1)
  )

  pieces <- character(0)
  for (i in seq_along(parts)) {
    if (timed && i > 1L && between[i - 1L] >= min_pause) {
      pieces <- c(pieces, sprintf("[gap %.1fs]", between[i - 1L]))
    }
    # The label is the first speaker actually recorded on the turn. A turn
    # with none is written bare, since a placeholder would be a speaker this
    # function invented.
    lab <- parts[[i]]$speaker[!is.na(parts[[i]]$speaker)]
    body <- render_turn(parts[[i]], timed, min_pause)
    pieces <- c(
      pieces,
      if (label && length(lab)) {
        sprintf("[%s]: %s", lab[1L], body)
      } else {
        body
      }
    )
  }

  # within_gaps() leads each turn with a zero, so drop one per turn rather
  # than one overall. What is left is every real silence, nrow(w) - 1 of them.
  all_gaps <- c(
    unlist(lapply(parts, function(p) within_gaps(p)[-1L])),
    between
  )
  tibble::tibble(
    id = id,
    audio = audio,
    exists = TRUE,
    text = paste(pieces, collapse = if (label) "\n" else " "),
    duration_s = max(w$end) - min(w$start),
    n_words = nrow(w),
    n_turns = if (diarized) length(parts) else NA_integer_,
    n_speakers = if (diarized) {
      length(unique(w$speaker[!is.na(w$speaker)]))
    } else {
      NA_integer_
    },
    n_pauses = sum(vapply(
      parts,
      function(p) {
        sum(within_gaps(p) >= min_pause)
      },
      integer(1)
    )),
    n_gaps = if (diarized) sum(between >= min_pause) else NA_integer_,
    median_gap_s = if (length(all_gaps)) stats::median(all_gaps) else NA_real_,
    # Two rates rather than one, because they answer different questions.
    # Someone can be fast on the clock because they talk quickly or because
    # they never stop, and the ratio between these is the pausing.
    speech_rate = nrow(w) / (max(w$end) - min(w$start)),
    articulation_rate = if (sum(w$end - w$start) > 0) {
      nrow(w) / sum(w$end - w$start)
    } else {
      NA_real_
    },
    median_turn_s = if (diarized) {
      stats::median(vapply(parts, function(p) max(p$end) - min(p$start), 1))
    } else {
      NA_real_
    },
    # How far to trust everything above it. A recording whose words were
    # placed at low confidence has `[pause]` values resting on boundaries the
    # aligner was unsure of, and this is what makes that filterable.
    median_score = if (any(!is.na(w$score))) {
      stats::median(w$score, na.rm = TRUE)
    } else {
      NA_real_
    },
    diarized = diarized,
    annotate_timing = annotate_timing,
    transcribed = transcribed,
    # Withheld rather than guessed at. A transcript with no sidecar was made by
    # something this call did not do and cannot name, so the arguments in hand
    # describe the request rather than the words.
    tool = if (verified) tool else NA_character_,
    asr_model = if (verified) asr_model else NA_character_,
    # Read off the transcript itself, so it holds either way.
    language = as.character(parsed$language %||% language %||% NA_character_),
    # Applied by this call rather than by the recognizer, so it holds too.
    min_pause = min_pause,
    extra = if (!verified || is.null(extra)) {
      NA_character_
    } else {
      paste(extra, collapse = " ")
    }
  )
}

#' The settings that decide whether a transcript answers this call
#'
#' Everything as one string per field, so the sidecar reads back the way it was
#' written and a comparison is never a question about types.
#'
#' `min_pause` and `speakers` are absent on purpose. Neither reaches the
#' recognizer, so neither changes the transcript, and putting them here would
#' re-run an hour of audio to change a rendering.
#'
#' @param tool,asr_model,diarize,language,extra As passed to [prepare_audio()].
#' @return A named list of strings.
#' @keywords internal
#' @noRd
cache_settings <- function(tool, asr_model, diarize, language, extra) {
  list(
    tool = as.character(tool),
    asr_model = as.character(asr_model),
    diarize = if (isTRUE(diarize)) "yes" else "no",
    language = if (is.null(language)) "" else as.character(language),
    extra = if (is.null(extra)) "" else paste(extra, collapse = " ")
  )
}

#' Where the record of a transcript's settings lives
#'
#' Beside the transcript rather than inside it, because the recognizer owns
#' that file and a researcher may edit it by hand.
#'
#' @param json A string giving the path to a transcript.
#' @return A path.
#' @keywords internal
#' @noRd
cache_sidecar <- function(json) {
  paste0(tools::file_path_sans_ext(json), ".fathom.json")
}

#' Record how a transcript was made, atomically
#'
#' Written to a temporary file in the same directory and renamed, as
#' `ckpt_write()` does, so a crash leaves the previous record intact rather
#' than a half-written one that reads as a mismatch and re-transcribes.
#'
#' @param json A string giving the path to the transcript just written.
#' @param settings A list from `cache_settings()`, which has no topic to link
#'   to.
#' @return The sidecar path, invisibly.
#' @keywords internal
#' @noRd
cache_write <- function(json, settings) {
  fp <- cache_sidecar(json)
  tmp <- paste0(fp, ".tmp")
  jsonlite::write_json(settings, tmp, auto_unbox = TRUE)
  file.rename(tmp, fp)
  invisible(fp)
}

#' Read a settings record, tolerating absence and corruption
#'
#' `NULL` for both, as `ckpt_read()` does, since a half-written record is not
#' evidence of anything and treating it as missing is the only reading that
#' does not destroy work.
#'
#' @param json A string giving the path to a transcript.
#' @return A named list, or `NULL`.
#' @keywords internal
#' @noRd
cache_read <- function(json) {
  fp <- cache_sidecar(json)
  if (!file.exists(fp)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(fp, simplifyVector = TRUE),
    error = function(e) NULL
  )
}

#' Whether a transcript on disk answers this call
#'
#' The same question `ckpt_stale()` asks, which is whether the work was done
#' under the settings now being requested rather than merely done. Four
#' answers instead of two, because the transcript is written by an external
#' tool and can predate this record or be edited by hand.
#'
#' \describe{
#'   \item{`"absent"`}{Nothing usable. Missing, older than the recording, or
#'     unreadable. Unreadable counts as absent for `ckpt_read()`'s reason, that
#'     a run killed mid-write leaves one and re-transcribing is the only
#'     recoverable reading.}
#'   \item{`"stale"`}{Made under different settings. Re-transcribed, which is
#'     what stops a diarized transcript answering `diarize = FALSE`.}
#'   \item{`"unverified"`}{Usable, with no record of what made it. Used rather
#'     than discarded, since this is the hand-corrected transcript, and the
#'     provenance columns come back `NA` instead of naming settings that did
#'     not produce those words.}
#'   \item{`"current"`}{Made under these settings.}
#' }
#'
#' @param json A string giving the path to a transcript.
#' @param audio A string giving the path to the recording.
#' @param settings A list from `cache_settings()`.
#' @return One of the four strings above.
#' @keywords internal
#' @noRd
cache_state <- function(json, audio, settings) {
  if (!file.exists(json) || file.mtime(json) < file.mtime(audio)) {
    return("absent")
  }
  readable <- tryCatch(
    {
      jsonlite::fromJSON(json, simplifyVector = FALSE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!readable) {
    return("absent")
  }
  side <- cache_read(json)
  keys <- names(settings)
  if (is.null(side) || !all(keys %in% names(side))) {
    return("unverified")
  }
  same <- identical(
    as.character(unlist(side[keys], use.names = FALSE)),
    as.character(unlist(settings[keys], use.names = FALSE))
  )
  if (same) "current" else "stale"
}

#' A row for a path that does not resolve
#'
#' Kept rather than dropped, on the same argument as [prepare_image()]. A
#' corpus that quietly lost four recordings is the failure this prevents, and
#' transcription is slow enough that a run left unattended is exactly when a
#' bad path goes unnoticed.
#'
#' @param id,audio,tool,asr_model,language,min_pause,extra As in
#'   `render_recording()`.
#' @param annotate_timing As in `render_recording()`.
#' @return A one-row tibble.
#' @keywords internal
#' @noRd
empty_row <- function(
  id,
  audio,
  tool,
  asr_model,
  language,
  min_pause,
  extra,
  annotate_timing
) {
  tibble::tibble(
    id = id,
    audio = audio,
    exists = FALSE,
    text = NA_character_,
    duration_s = NA_real_,
    n_words = NA_integer_,
    n_turns = NA_integer_,
    n_speakers = NA_integer_,
    n_pauses = NA_integer_,
    n_gaps = NA_integer_,
    median_gap_s = NA_real_,
    speech_rate = NA_real_,
    articulation_rate = NA_real_,
    median_turn_s = NA_real_,
    median_score = NA_real_,
    diarized = NA,
    annotate_timing = annotate_timing,
    transcribed = NA,
    tool = tool,
    asr_model = asr_model,
    language = as.character(language %||% NA_character_),
    min_pause = min_pause,
    extra = if (is.null(extra)) NA_character_ else paste(extra, collapse = " ")
  )
}
#' Word timings, from a file or a parsed list, as one flat table
#'
#' The shape varies. WhisperX nests words inside `segments`, also offers a flat
#' `word_segments`, and a bare list of word objects is what a converter usually
#' produces. All three arrive here.
#'
#' A word with no timing is dropped and counted rather than kept with an `NA`,
#' since a gap computed against a missing boundary is not a gap. WhisperX omits
#' timings on tokens its aligner could not place, so this is the ordinary case
#' rather than a broken file.
#'
#' @param timings A path or a list.
#' @return A tibble with `word`, `start`, `end`, `speaker`, `score`.
#' @keywords internal
#' @noRd
read_timings <- function(timings) {
  if (is.character(timings) && length(timings) == 1L) {
    if (!file.exists(timings)) {
      stop(fmt_err(paste0("no file at ", timings)), call. = FALSE)
    }
    timings <- jsonlite::fromJSON(timings, simplifyVector = FALSE)
  }
  if (!is.list(timings)) {
    stop(
      fmt_err("`timings` must be a path to JSON or a parsed list."),
      call. = FALSE
    )
  }

  words <- if (!is.null(timings$word_segments)) {
    timings$word_segments
  } else if (!is.null(timings$segments)) {
    unlist(lapply(timings$segments, function(s) s$words), recursive = FALSE)
  } else {
    timings
  }
  if (!length(words)) {
    stop(fmt_err("no words found in `timings`."), call. = FALSE)
  }

  # `nm %in% names(x)` rather than `x[[nm]]` directly. Subsetting an atomic
  # value or an unnamed list by name is a subscript error, not a miss, so a
  # file whose elements are not all word objects would fail here rather than
  # at the check below that exists to describe it.
  pull <- function(x, ...) {
    if (!is.list(x)) {
      return(NA)
    }
    for (nm in c(...)) {
      if (nm %in% names(x) && !is.null(x[[nm]])) {
        return(x[[nm]])
      }
    }
    NA
  }

  # `word` only, never falling back to `text`. A WhisperX segment carries
  # `text`, `start` and `end` too, so a fallback parses a segment-level file as
  # a corpus of paragraph-length words with no gaps between them and says
  # nothing. Refusing by name is the difference between a clear error and a
  # silently wrong corpus.
  has_word <- vapply(
    words,
    function(x) is.list(x) && "word" %in% names(x),
    logical(1)
  )
  if (!any(has_word)) {
    stop(
      fmt_err(
        paste0(
          "no object in `timings` has a `word` field, so this looks like ",
          "segment-level output rather than word-level timings."
        ),
        paste0(
          "Re-run the recognizer so it writes one object per word. WhisperX ",
          "and whispermlx do this by default."
        )
      ),
      call. = FALSE
    )
  }
  tok <- vapply(words, function(x) as.character(pull(x, "word")), "")
  st <- vapply(words, function(x) as.numeric(pull(x, "start")), 1)
  en <- vapply(words, function(x) as.numeric(pull(x, "end")), 1)
  sp <- vapply(words, function(x) as.character(pull(x, "speaker")), "")
  # The aligner's own confidence in where it put this word. WhisperX and
  # whispermlx both write it, a hand-made transcript may not, and every
  # `[pause]` and `[gap]` figure rests on the boundaries it describes.
  scr <- vapply(words, function(x) as.numeric(pull(x, "score")), 1)

  ok <- !is.na(st) & !is.na(en) & nzchar(tok) & !is.na(tok)
  if (!any(ok)) {
    stop(
      fmt_err(paste0(
        "no word in `timings` carries both a start and an end. Check that ",
        "the file was written with word-level timings rather than segment ",
        "ones."
      )),
      call. = FALSE
    )
  }
  if (any(!ok)) {
    message(fmt_note(
      "Note",
      sprintf(
        "%d of %d words have no timing and are dropped. An aligner leaves these where it could not place a token.",
        sum(!ok),
        length(ok)
      )
    ))
  }

  o <- order(st[ok])
  tibble::tibble(
    word = tok[ok][o],
    start = st[ok][o],
    end = en[ok][o],
    speaker = sp[ok][o],
    score = scr[ok][o]
  )
}

#' Rename the speakers a recognizer emitted
#'
#' A name in `speakers` that is not in the timings is refused rather than
#' ignored, since a typo that silently renames nothing leaves a corpus that
#' looks relabelled and is not.
#'
#' @param sp A character vector holding the speaker column.
#' @param speakers As passed to [prepare_audio()].
#' @return `sp`, renamed.
#' @keywords internal
#' @noRd
rename_speakers <- function(sp, speakers) {
  if (is.null(speakers)) {
    return(sp)
  }
  if (
    !is.character(speakers) ||
      is.null(names(speakers)) ||
      any(!nzchar(names(speakers)))
  ) {
    stop(
      fmt_err(paste0(
        "`speakers` must be a named character vector, such as ",
        "c(SPEAKER_00 = \"Interviewer\")."
      )),
      call. = FALSE
    )
  }
  present <- unique(sp[!is.na(sp)])
  missed <- setdiff(names(speakers), present)
  if (length(missed)) {
    stop(
      fmt_err(
        paste0(
          "`speakers` renames ",
          paste(missed, collapse = ", "),
          ", not in the timings."
        ),
        paste0(
          "Present: ",
          if (length(present)) {
            paste(present, collapse = ", ")
          } else {
            "no speaker at all"
          },
          "."
        )
      ),
      call. = FALSE
    )
  }
  hit <- !is.na(sp) & sp %in% names(speakers)
  sp[hit] <- unname(speakers[sp[hit]])
  sp
}

#' Which turn each word belongs to
#'
#' A turn is a maximal run of words by one speaker, and nothing splits one
#' further. Silence alone would not do it. Stivers et al. put the mean turn
#' transition at 207 milliseconds, so any threshold low enough to catch turns
#' would cut hesitation as well, and the silence inside a turn is written where
#' it happened instead.
#'
#' @param w A data frame holding the word table.
#' @return An integer, one per word.
#' @keywords internal
#' @noRd
turns_of <- function(w) {
  n <- nrow(w)
  if (n < 2L || all(is.na(w$speaker))) {
    return(rep(1L, n))
  }
  changed <- c(
    FALSE,
    w$speaker[-1L] != w$speaker[-n] &
      !is.na(w$speaker[-1L]) &
      !is.na(w$speaker[-n])
  )
  cumsum(changed) + 1L
}

#' Silences inside one turn
#'
#' @param p A data frame of the words of one turn.
#' @return A numeric, one per word, the first being zero.
#' @keywords internal
#' @noRd
within_gaps <- function(p) {
  n <- nrow(p)
  if (n < 2L) {
    return(0)
  }
  c(0, p$start[-1L] - p$end[-n])
}

#' One turn, rendered
#'
#' @param p A data frame of the words of one turn.
#' @param timed A logical value indicating whether silences are written in.
#' @param min_pause As passed to [prepare_audio()].
#' @return A single string.
#' @keywords internal
#' @noRd
render_turn <- function(p, timed, min_pause) {
  if (!timed) {
    return(paste(p$word, collapse = " "))
  }
  gap <- within_gaps(p)
  pieces <- character(0)
  for (i in seq_len(nrow(p))) {
    if (gap[i] >= min_pause) {
      pieces <- c(pieces, sprintf("[pause %.1fs]", gap[i]))
    }
    pieces <- c(pieces, p$word[i])
  }
  paste(pieces, collapse = " ")
}

#' Turn videos into a corpus carrying both what was shown and what was said
#'
#' @description
#' Turns each recording into one row, with frames in a list column and a
#' transcript beside them. `measure(text = , image = )` then reads both on the
#' same call, since they are two columns of one row rather than two objects to
#' align.
#'
#' The audio half is [prepare_audio()] unchanged, running on a wav `ffmpeg`
#' extracts.
#'
#' @section Choosing frames:
#' `select = "uniform"` divides the recording into `frames` equal segments and
#' takes the middle of each. Midpoints rather than endpoints, since a frame at
#' exactly zero is often a title card and one at exactly the end is often black.
#'
#' `select = "change"` takes the `frames` highest scene scores instead, which
#' `ffmpeg` computes for every frame as how much this picture differs from the
#' one before it. It finds cuts that even spacing misses, and it concentrates.
#' `min_gap` is what stops that, and it exists only for this mode.
#'
#' @section Recordings without audio:
#' `ffprobe` is asked whether an audio stream exists before anything is
#' extracted. Where there is none, the row comes back with its frames and with
#' `has_audio = FALSE`, `text` and every audio count `NA`, and the count is
#' reported. A batch of campaign advertisements does not stop because three of
#' them are silent.
#'
#' `exists` and `has_audio` answer different questions and both are needed.
#'
#' | case | `exists` | `has_audio` | `text` |
#' | :-- | :-- | :-- | :-- |
#' | file not found | `FALSE` | `NA` | `NA` |
#' | present, silent | `TRUE` | `FALSE` | `NA` |
#' | present, with audio | `TRUE` | `TRUE` | the transcript |
#'
#' `NA` on `has_audio` is doing work. The file was never opened, so `FALSE`
#' would assert something that was not checked.
#'
#' `n_words` is `NA` for a silent recording rather than `0`, because nothing
#' was transcribed. A recording whose audio track is silence is a different
#' thing, gets transcribed, and comes back with `n_words = 0`.
#'
#' Every other audio column is `NA` there too, `diarized` and `tool` and
#' `asr_model` included. Nothing ran, so there is nothing to report and naming
#' a model would assert settings that produced no words.
#'
#' @section Frame storage:
#' On disk beside the recording, in `<stem>_frames/`, because `measure(image =
#' )` reads paths.
#'
#' The wav lands beside it, which is what lets [prepare_audio()]'s cache work
#' unchanged. The frames keep a record of their own, `<stem>_frames.fathom.json`
#' in the frames directory, holding the times they were cut at and the JPEG
#' quality. Changing `frames`, `select`, `include_first`, `min_gap` or `quality`
#' changes that record, so every frame is cut again and the old pictures are
#' removed, while the transcript is read back from its own cache.
#'
#' @param x A character vector of paths to video files, one row each. A vector
#'   rather than a directory, so `list.files()` is where the pattern lives.
#' @param ... Not used, and refused if anything reaches it. Every argument
#'   after it is name-only, so a misspelling lands here rather than being
#'   matched by position.
#' @param frames An integer giving the number of frames per recording. Fixed
#'   across the corpus. Default is 8.
#' @param select A string naming the rule that chooses the frames. `"uniform"`
#'   spreads them evenly and is the default. `"change"` takes the highest scene
#'   scores, which finds cuts and concentrates on visually busy stretches.
#' @param include_first A logical value indicating whether the opening frame is
#'   always taken. It spends one of `frames` rather than adding to it, so
#'   `n_frames` does not vary with a flag. Frame zero has no predecessor and
#'   scores exactly zero, so `change` can never reach it otherwise, and an
#'   establishing shot is sometimes the most informative picture in a recording.
#'   Default is `FALSE`.
#' @param min_gap A number giving the seconds below which two chosen frames may
#'   not sit. `"change"` only, and ignored rather than refused under
#'   `"uniform"`, where even spacing cannot bunch. Defaults to `duration /
#'   frames`, which says no two frames sit closer than an even sample would put
#'   them, so `"change"` moves frames toward the action without being able to
#'   pack them into one stretch. Where that leaves fewer candidates than the
#'   budget, the remainder go in the middle of the widest space left.
#' @param quality An integer giving the JPEG quality passed to `ffmpeg` as
#'   `-q:v`, 2 best to 31 worst. Default is 3.
#' @param cache_dir A string giving the directory where frames, the wav and the
#'   transcript are written and looked for. Defaults to beside each recording.
#' @param force A logical value indicating whether to extract and transcribe
#'   again even where usable output is cached. Default is `FALSE`.
#' @inheritParams prepare_audio
#' @param min_speakers,max_speakers
#'   Passed to [prepare_audio()] on the extracted audio, behaving there
#'   exactly as on a recording handed to it directly.
#' @param id A character vector of identifiers, one per file. Defaults to the
#'   file names without their extensions, and must be unique.
#' @return A tibble of one row per file, carrying every column [prepare_audio()]
#' returns plus these.
#'
#' - `video` is the recording, and `has_audio` says whether it had a sound track
#'   at all.
#' - `image` is a list column of frame paths in time order, which is the shape
#'   `measure(image = )` takes for several pictures per document.
#' - `n_frames` and `frame_times` say how many were taken and when.
#' - `select` and `include_first` name which rule chose them.
#' - `scene_median` and `scene_max` are the middle and largest scene score
#'   across every frame, which say whether the recording had cuts in it or
#'   whether the spacing did the work. Read before trusting `"change"`.
#' @seealso [prepare_audio()], which transcribes the sound track, and
#'   [measure()], which reads both columns.
#' @examples
#' \dontrun{
#' # Needs ffmpeg, a recognizer and an Ollama server, so it is not run here.
#'
#' ads <- list.files("campaign_ads", "[.]mp4$", full.names = TRUE)
#' ins <- instrument(c(attack = "Does this advertisement attack an opponent?"))
#'
#' # Frames spread evenly, and whatever was said alongside them.
#' d <- prepare_video(ads, frames = 8)
#' measure(
#'   d,
#'   backend = ollama_backend("gemma3:27b"),
#'   instrument = ins,
#'   text = "text",
#'   image = "image"
#' )
#'
#' # More frames, chosen where the picture moved, with the speakers labeled.
#' d <- prepare_video(
#'   ads,
#'   frames = 16,
#'   select = "change",
#'   diarize_names = TRUE,
#'   annotate_timing = TRUE,
#'   hf_token = Sys.getenv("HF_TOKEN")
#' )
#' }
#' @export
prepare_video <- function(
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
) {
  no_dots(..., .fn = "prepare_video")
  select <- match.arg(select)
  verbosity <- match.arg(verbosity)
  if (!is.numeric(frames) || length(frames) != 1L || frames < 1) {
    stop(
      fmt_err("`frames` must be a single number, one or more."),
      call. = FALSE
    )
  }
  frames <- as.integer(frames)

  if (!is.character(x) || !length(x)) {
    stop(
      fmt_err("`x` must be a non-empty character vector of file paths."),
      call. = FALSE
    )
  }
  ids <- if (is.null(id)) {
    tools::file_path_sans_ext(basename(x))
  } else {
    as.character(id)
  }
  if (length(ids) != length(x)) {
    stop(fmt_err("`id` must be one per file, or NULL."), call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop(
      fmt_err(
        "ids must be unique.",
        paste0(
          "Repeated: ",
          paste(unique(ids[duplicated(ids)]), collapse = ", "),
          "."
        )
      ),
      call. = FALSE
    )
  }

  keep <- file.exists(x)
  if (!any(keep)) {
    stop(
      fmt_err(
        "none of the paths in `x` resolve to a file.",
        paste0("First: ", x[1], ".")
      ),
      call. = FALSE
    )
  }
  if (any(!keep) && !identical(verbosity, "quiet")) {
    message(fmt_note(
      "Warning",
      sprintf(
        "%d of %d paths do not resolve. Those rows carry exists = FALSE.",
        sum(!keep),
        length(keep)
      )
    ))
  }

  dirs <- if (is.null(cache_dir)) dirname(x) else rep_len(cache_dir, length(x))
  bin <- NULL
  probe <- NULL
  if (any(keep)) {
    bin <- find_ffmpeg()
    probe <- find_ffprobe(bin)
  }

  rows <- vector("list", length(x))
  silent <- logical(length(x))
  # Progress the way prepare_audio() reports it, a bar over several videos where
  # use_bar() allows one and a line as each video starts otherwise.
  talk <- !identical(verbosity, "quiet")
  run_env <- environment()
  pb <- NULL
  pb_line <- ""
  if (talk && sum(keep) > 1L && use_bar(identical(verbosity, "verbose"))) {
    pb <- cli::cli_progress_bar(
      total = length(x),
      format = "{pb_line}",
      clear = FALSE,
      .envir = run_env
    )
  }
  t0 <- Sys.time()
  n_done <- 0L
  for (i in seq_along(x)) {
    if (!keep[i]) {
      rows[[i]] <- empty_video_row(
        ids[i],
        x[i],
        frames,
        select,
        include_first,
        annotate_timing
      )
      next
    }
    if (talk && is.null(pb)) {
      progress_message(sprintf(
        "Preparing  %*d/%d  %s",
        nchar(length(x)),
        i,
        length(x),
        basename(x[i])
      ))
    }
    rows[[i]] <- one_video(
      video = x[i],
      id = ids[i],
      out_dir = dirs[i],
      frames = frames,
      select = select,
      include_first = include_first,
      min_gap = min_gap,
      quality = quality,
      bin = bin,
      probe = probe,
      force = force,
      audio_args = list(
        diarize_names = diarize_names,
        annotate_timing = annotate_timing,
        tool = tool,
        asr_model = asr_model,
        min_speakers = min_speakers,
        max_speakers = max_speakers,
        language = language,
        hf_token = hf_token,
        extra = extra,
        min_pause = min_pause,
        speakers = speakers,
        cache_dir = dirs[i],
        force = force,
        verbosity = verbosity
      )
    )
    silent[i] <- !isTRUE(rows[[i]]$has_audio)
    n_done <- n_done + 1L
    if (!is.null(pb)) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      pb_line <- progress_line(
        i,
        length(x),
        el / n_done,
        bar = TRUE,
        verb = "Preparing",
        unit = "video"
      )
      cli::cli_progress_update(id = pb, set = i, .envir = run_env)
    }
  }
  if (!is.null(pb)) {
    cli::cli_progress_done(id = pb, .envir = run_env)
  }

  if (any(silent) && !identical(verbosity, "quiet")) {
    message(fmt_note(
      "Note",
      sprintf(
        paste0(
          "%d of %d recordings carry no audio stream, so text is NA on those ",
          "rows. Their frames are unaffected."
        ),
        sum(silent),
        sum(keep)
      )
    ))
  }
  dplyr::bind_rows(rows)
}

#' The ffmpeg binary, or what to do about its absence
#'
#' Same shape as `find_recognizer()`, and for the same reason. On macOS a tool
#' that answers in the terminal can be invisible to `Sys.which()`, so the
#' refusal names the PATH before it names installation.
#'
#' @return A path.
#' @keywords internal
#' @noRd
find_ffmpeg <- function() {
  bin <- Sys.which("ffmpeg")
  if (!nzchar(bin)) {
    stop(
      fmt_err(
        paste0(
          "ffmpeg is not on the PATH this R session sees, so no video can ",
          "be read."
        ),
        paste0(
          "R does not always inherit the PATH a shell uses, so a tool that ",
          "runs in your terminal can be invisible here. Check ",
          "Sys.getenv(\"PATH\") before reinstalling, and add a directory ",
          "permanently with PATH=${PATH}:/your/dir in ~/.Renviron."
        ),
        "If it is genuinely absent, `brew install ffmpeg` on macOS."
      ),
      call. = FALSE
    )
  }
  unname(bin)
}

#' Find the ffprobe that ships with the ffmpeg in use
#'
#' Looked for beside `bin` first, since the two are installed together and a
#' second copy elsewhere on the PATH may be another version, then on the PATH.
#' Refused when neither has it. Called by name, a missing ffprobe gave back the
#' shell's complaint in place of a duration.
#'
#' @param bin A string giving the resolved ffmpeg binary.
#' @return The path to ffprobe.
#' @keywords internal
#' @noRd
find_ffprobe <- function(bin) {
  near <- file.path(dirname(bin), c("ffprobe", "ffprobe.exe"))
  near <- near[file.exists(near)]
  probe <- if (length(near)) near[1] else unname(Sys.which("ffprobe"))
  if (!nzchar(probe)) {
    stop(
      fmt_err(
        paste0(
          "ffprobe is not beside ffmpeg or on the PATH this R session sees, ",
          "so no video can be measured."
        ),
        paste0(
          "It ships with ffmpeg, so the install that provided ",
          bin,
          " should have it. Check Sys.getenv(\"PATH\"), and add a directory ",
          "permanently with PATH=${PATH}:/your/dir in ~/.Renviron."
        )
      ),
      call. = FALSE
    )
  }
  probe
}

#' What ffprobe says a file is
#'
#' Duration and whether an audio stream exists, asked before anything is
#' extracted, because `ffmpeg` errors rather than writing an empty wav when
#' there is no sound track and a silent recording is a document rather than a
#' failure.
#'
#' @param video A string giving one path.
#' @param probe A string giving the resolved ffprobe binary.
#' @return A list with `duration` and `has_audio`.
#' @keywords internal
#' @noRd
probe_video <- function(video, probe) {
  out <- suppressWarnings(system2(
    probe,
    c(
      "-v",
      "error",
      "-show_entries",
      "format=duration",
      "-show_entries",
      "stream=codec_type",
      "-of",
      "default=nw=1",
      shQuote(video)
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  dur <- grep("^duration=", out, value = TRUE)
  list(
    duration = if (length(dur)) {
      as.numeric(sub("^duration=", "", dur[1]))
    } else {
      NA_real_
    },
    has_audio = any(grepl("^codec_type=audio$", out))
  )
}

#' Scene scores for every frame
#'
#' `select='gte(scene,0)'` keeps every frame rather than thresholding, and
#' `metadata=print` writes the score `ffmpeg` computed for each. Scoring a four
#' minute video took half a second, so nothing is sampled or downscaled first.
#'
#' There is no threshold anywhere in this. A score is a fact about two
#' consecutive frames, where a cutoff on it would be a claim about where scenes
#' begin, which is the argument that removed `window` from [prepare_audio()].
#'
#' @param video A string giving one path.
#' @param bin A string giving the resolved ffmpeg binary.
#' @return A data frame of `t` and `score`, or zero rows if nothing parsed.
#' @keywords internal
#' @noRd
scene_scores <- function(video, bin) {
  # Read off the log rather than out of a file. A file path inside the filter
  # string has to survive ffmpeg's own escaping, and a Windows drive letter
  # ends the option at its colon. `repeat` stops ffmpeg folding identical
  # lines into a count.
  out <- suppressWarnings(system2(
    bin,
    c(
      "-hide_banner",
      "-nostats",
      "-loglevel",
      "repeat+info",
      "-i",
      shQuote(video),
      "-filter:v",
      shQuote("select='gte(scene,0)',metadata=print"),
      "-f",
      "null",
      "-"
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  scene_lines(out)
}

#' Scene scores from the log ffmpeg writes
#'
#' `metadata=print` with no file writes two lines per frame to the log, the
#' frame and its time and then the score, each behind the tag of the filter
#' that wrote it. Only the tagged lines are read, so a line printed from
#' elsewhere in ffmpeg cannot land between a frame and its score.
#'
#' @param txt A character vector of log lines.
#' @return A data frame of `t` and `score`, or zero rows if nothing parsed.
#' @keywords internal
#' @noRd
scene_lines <- function(txt) {
  tag <- "^\\[Parsed_metadata_[0-9]+ @ [^]]*\\]\\s*"
  txt <- sub(tag, "", txt[grepl(tag, txt)])
  # One line carries the frame and its time, the next carries the score.
  h <- grep("^frame:", txt)
  h <- h[h < length(txt)]
  if (!length(h)) {
    return(data.frame(t = numeric(0), score = numeric(0)))
  }
  tm <- suppressWarnings(as.numeric(sub(
    ".*pts_time:\\s*([0-9.]+).*",
    "\\1",
    txt[h]
  )))
  sc <- suppressWarnings(as.numeric(sub(
    ".*lavfi\\.scene_score=",
    "",
    txt[h + 1L]
  )))
  ok <- !is.na(tm) & !is.na(sc)
  data.frame(t = tm[ok], score = sc[ok])
}

#' Which timestamps to take
#'
#' @param scores A data frame from `scene_scores()`, possibly zero rows.
#' @param duration A number giving the duration in seconds.
#' @param frames,select,include_first,min_gap As passed to [prepare_video()].
#' @return A numeric vector of times, sorted.
#' @keywords internal
#' @noRd
pick_frames <- function(
  scores,
  duration,
  frames,
  select,
  include_first,
  min_gap
) {
  first <- if (include_first) 0 else numeric(0)
  n <- frames - length(first)
  if (n < 1L) {
    return(first)
  }
  if (identical(select, "uniform") || !nrow(scores)) {
    # Midpoints of n equal segments of whatever is left after the opening
    # frame, so the spacing stays even rather than the first slot being
    # stolen from the front of the recording.
    lo <- if (include_first) duration / (2 * frames) else 0
    return(sort(c(first, lo + (duration - lo) * (seq_len(n) - 0.5) / n)))
  }
  # `duration / frames`, not half of it. Half lets every frame fit inside a
  # fifth of the recording, and once the real peaks are used up every
  # remaining score is within a hundredth of every other, so the walk becomes
  # index order and takes the earliest frames. On a twelve second fixture
  # with cuts at 3, 6 and 9 it put four of eight in the first two seconds.
  # At `duration / frames` no two frames sit closer than an even sample would
  # put them, which makes this a perturbation of `"uniform"` rather than a
  # free choice.
  if (is.null(min_gap)) {
    min_gap <- duration / frames
  }
  keep <- first
  for (i in order(scores$score, decreasing = TRUE)) {
    if (length(keep) >= frames) {
      break
    }
    tt <- scores$t[i]
    if (length(keep) && any(abs(tt - keep) < min_gap)) {
      next
    }
    keep <- c(keep, tt)
  }
  # A gap that wide can also exhaust the candidates before the budget, and
  # `n_frames` varying across a corpus is the confound this avoids. The
  # shortfall goes in the middle of the widest space left, counting both
  # ends, which needs no threshold and spreads rather than clumping.
  while (length(keep) < frames) {
    e <- c(0, sort(keep), duration)
    w <- diff(e)
    j <- which.max(w)
    keep <- c(keep, (e[j] + e[j + 1L]) / 2)
  }
  sort(keep)
}

#' One video, extracted and rendered
#'
#' @param video,id,out_dir,frames,select,include_first,min_gap,quality As in
#'   `pick_frames()` and [prepare_video()].
#' @param bin A string giving the resolved ffmpeg binary.
#' @param probe A string giving the resolved ffprobe binary.
#' @param force A logical value indicating whether to redo work already on disk.
#' @param audio_args A list forwarded to [prepare_audio()].
#' @return A one-row tibble.
#' @keywords internal
#' @noRd
one_video <- function(
  video,
  id,
  out_dir,
  frames,
  select,
  include_first,
  min_gap,
  quality,
  bin,
  probe,
  force,
  audio_args
) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  stem <- tools::file_path_sans_ext(basename(video))
  info <- probe_video(video, probe)

  sc <- if (identical(select, "change")) {
    scene_scores(video, bin)
  } else {
    data.frame(t = numeric(0), score = numeric(0))
  }
  times <- pick_frames(
    sc,
    info$duration,
    frames,
    select,
    include_first,
    min_gap
  )

  fdir <- file.path(out_dir, paste0(stem, "_frames"))
  dir.create(fdir, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(fdir, sprintf("%s_f%02d.jpg", stem, seq_along(times)))

  # The times the frames were cut at, and the quality, recorded beside them. A
  # file name says only which frame of the set a picture is, so without this a
  # second call under other settings kept the old pictures while frame_times
  # reported the new times. A set cut under anything else, or with no record,
  # is removed and cut again, which costs one ffmpeg call per frame and never
  # touches the transcript.
  record <- file.path(fdir, paste0(stem, "_frames.fathom.json"))
  want <- list(times = sprintf("%.3f", times), quality = as.character(quality))
  had <- if (file.exists(record)) {
    tryCatch(
      jsonlite::fromJSON(record, simplifyVector = TRUE),
      error = function(e) NULL
    )
  }
  recut <- force ||
    is.null(had) ||
    !identical(as.character(had$times), want$times) ||
    !identical(as.character(had$quality), want$quality)
  if (recut) {
    old <- list.files(fdir, pattern = "\\.jpg$")
    unlink(file.path(fdir, old[startsWith(old, paste0(stem, "_f"))]))
  }
  for (k in seq_along(times)) {
    if (
      !recut &&
        file.exists(paths[k]) &&
        file.mtime(paths[k]) >= file.mtime(video)
    ) {
      next
    }
    suppressWarnings(system2(
      bin,
      c(
        "-hide_banner",
        "-loglevel",
        "error",
        "-ss",
        sprintf("%.3f", times[k]),
        "-i",
        shQuote(video),
        "-frames:v",
        "1",
        "-q:v",
        as.character(quality),
        shQuote(paths[k]),
        "-y"
      ),
      stdout = TRUE,
      stderr = TRUE
    ))
  }
  got <- file.exists(paths)
  jsonlite::write_json(want, paste0(record, ".tmp"), auto_unbox = TRUE)
  file.rename(paste0(record, ".tmp"), record)

  audio <- if (info$has_audio) {
    wav <- file.path(out_dir, paste0(stem, ".wav"))
    if (force || !file.exists(wav) || file.mtime(wav) < file.mtime(video)) {
      suppressWarnings(system2(
        bin,
        c(
          "-hide_banner",
          "-loglevel",
          "error",
          "-i",
          shQuote(video),
          "-vn",
          "-ac",
          "1",
          "-ar",
          "16000",
          shQuote(wav),
          "-y"
        ),
        stdout = TRUE,
        stderr = TRUE
      ))
    }
    # The video's own progress already names it, so the line prepare_audio()
    # would print for this one recording is dropped. Its notes still print.
    withCallingHandlers(
      do.call(prepare_audio, c(list(x = wav, id = id), audio_args)),
      fathom_progress = function(m) invokeRestart("muffleMessage")
    )
  } else {
    # Every audio column NA, the same row a missing file gets. Naming a tool
    # and a model on a recording that was never transcribed asserts settings
    # that produced nothing, which is the lie already removed from a cached
    # transcript and from one with no record beside it. Only `exists` differs,
    # because the file is there.
    empty_row(
      id,
      NA_character_,
      NA_character_,
      NA_character_,
      NULL,
      NA_real_,
      NULL,
      audio_args$annotate_timing
    )
  }
  audio$exists <- TRUE

  vid <- tibble::tibble(
    video = video,
    has_audio = info$has_audio,
    image = list(paths[got]),
    n_frames = sum(got),
    frame_times = list(times[got]),
    select = select,
    include_first = include_first,
    scene_median = if (nrow(sc)) stats::median(sc$score) else NA_real_,
    scene_max = if (nrow(sc)) max(sc$score) else NA_real_
  )
  dplyr::bind_cols(audio, vid)
}

#' A row for a video path that does not resolve
#'
#' Kept rather than dropped, on [prepare_image()]'s argument. A corpus that
#' quietly lost four recordings is the failure this prevents, and a batch of
#' forty advertisements should not stop because one path was mistyped.
#'
#' @param id,video,frames,select,include_first,annotate_timing As in
#'   `one_video()` and [prepare_video()].
#' @return A one-row tibble.
#' @keywords internal
#' @noRd
empty_video_row <- function(
  id,
  video,
  frames,
  select,
  include_first,
  annotate_timing
) {
  audio <- empty_row(
    id,
    NA_character_,
    NA_character_,
    NA_character_,
    NULL,
    NA_real_,
    NULL,
    annotate_timing
  )
  dplyr::bind_cols(
    audio,
    tibble::tibble(
      video = video,
      has_audio = NA,
      image = list(character(0)),
      n_frames = NA_integer_,
      frame_times = list(numeric(0)),
      select = select,
      include_first = include_first,
      scene_median = NA_real_,
      scene_max = NA_real_
    )
  )
}
