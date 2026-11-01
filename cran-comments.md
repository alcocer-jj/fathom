## Test environments

* local macOS (Apple Silicon), devtools::check(remote = TRUE, manual = TRUE)
* win-builder, R-devel
* win-builder, R-release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes for the reviewer

* 'Ollama' is a local server the package sends requests to over HTTP. It is
  not needed to install the package, to run the examples outside \dontrun{},
  or to run the tests.
* The examples for measure(), order_sensitivity(), read_checkpoints(),
  prepare_audio() and prepare_video() are wrapped in \dontrun{}, since they
  need a running 'Ollama' server, 'ffmpeg' or a speech recognizer, none of
  which can be assumed on a check machine. Every other example runs, several
  of them on results from a real run that ship in inst/extdata.
* The tests need no server and no network access. Calls to 'Ollama' go
  through stubs or mocked HTTP responses, no test runs a speech recognizer,
  and tests that need 'ffmpeg' skip when it is absent.
* Outside tempdir(), nothing is written unless the user names a location.
  measure() writes checkpoints only when `checkpoint` is supplied,
  coding_sample() and inspect_cases() write only when given `file`, and
  prepare_audio() and prepare_video() require `cache_dir`. Examples and tests
  write only under tempdir().
* preview() and inspect_cases() write to the console with cat(), because
  displaying the assembled prompt and the sampled documents is what they are
  for. Both return their content invisibly, and inspect_cases() writes to a
  file instead when given one.
* graphics::par() is called only on a PNG device that the same code opens and
  closes, in the probe that checks a backend before a run with images and in
  the prepare_image() example, so the user's graphics state is not changed.
* coding_sample(), order_sensitivity() and validate_sample() take a `seed`
  argument that defaults to 1234 so their draws reproduce, and each restores
  the caller's random number state on exit.
* The `parlasent` dataset is derived from ParlaSent 1.0 and is distributed
  under CC BY-SA 4.0, as LICENSE.note and its help page state. The rest of the
  package is MIT.
