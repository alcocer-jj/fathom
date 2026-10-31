# Read a checkpoint directory back into results

Reconstructs the return value of
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
from disk without making any model calls. Use when a run completed but
the return value was lost, or to inspect how far a killed run
progressed.

## Usage

``` r
read_checkpoints(dir)
```

## Arguments

- dir:

  A string giving the checkpoint directory.

## Value

A `fathom_results` object, the two frames
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
returns and documents there. Rows arrive in whatever order the directory
listing gives, so sort if order matters.

The attributes are read back off the stored payloads rather than stamped
from the current session, so a corpus reports the run that produced it.

- `instrument` is the instrument as it was when the run happened.

- `num_ctx` is the ceiling that run used, not the one a run would get
  now. A resumed run keeps this, which is why mixing ceilings inside one
  corpus cannot happen silently.

- `exclusive` says whether the instrument shared one distribution.

- `run_at` says when the run happened, not when it was read.

A file that cannot be read is treated as absent rather than as an error,
since a killed run can leave one half-written and re-extracting that
document is the only recoverable reading. Errors when the directory
holds no readable checkpoints at all.

## Provenance

The context ceiling, the exclusivity flag and the run timestamp are read
from disk rather than inferred from the current session. A run written
under one ceiling reports that ceiling even if the current session would
resolve a different one.

## See also

[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md),
whose `checkpoint` argument writes what this reads.

## Examples

``` r
if (FALSE) { # \dontrun{
# Needs a running Ollama server, so it is not run here.
oll <- ollama_backend("gemma4:31b")
b <- instrument(c(tone = "Is this sentence positive?"),
  labels = c("YES", "NO"))
res <- measure(parlasent, instrument = b, backend = oll, text = "sentence",
  id = "id", checkpoint = "ckpt")

# A run cut short leaves its finished documents in the directory.
res <- read_checkpoints("ckpt")
} # }
```
