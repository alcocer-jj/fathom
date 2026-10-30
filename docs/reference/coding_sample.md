# Draw a blinded sample for human coding

Draws a sheet of documents and questions with a blank column for a
coder, and the model's answer withheld, and writes it as a CSV when
`file` is given. A coder shown the model's label agrees with it more
often, so withholding it is what makes the resulting agreement figure
interpretable.

## Usage

``` r
coding_sample(
  results,
  docs = NULL,
  ...,
  text = NULL,
  id = NULL,
  image = NULL,
  n = 200L,
  items = NULL,
  labels_allowed = NULL,
  copy_media = TRUE,
  file = NULL,
  seed = 1234L
)
```

## Arguments

- results:

  A `fathom_results` object from
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
  or
  [`read_checkpoints()`](https://alcocer-jj.github.io/fathom/reference/read_checkpoints.md).

- docs:

  A character vector, or the data frame passed to
  [`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md).
  Default is `NULL`, which uses the documents a character-vector run
  stored on the results.

- ...:

  Not used, and refused if anything reaches it. Every argument after it
  is name-only, so a misspelling lands here rather than being matched by
  position.

- text, id:

  Strings naming the text and identifier columns, if `docs` is a data
  frame. `text` is then required, and `id` defaults to the row position.

- image:

  A string naming a column in `docs` holding the media for each
  document, so the sheet carries it as well as any text. A character
  column is one file per document. A list column holds several, such as
  the frames
  [`prepare_video()`](https://alcocer-jj.github.io/fathom/reference/prepare_video.md)
  returns, and they share one cell joined with `|`. Default is `NULL`.

- n:

  An integer giving the total document-item pairs to draw, across all
  questions. Default is 200.

- items:

  A character vector restricting the draw to specific items, or `NULL`
  (the default) for all of them.

- labels_allowed:

  A character vector of permitted values for the `human_label` column,
  written into the sheet so a coder knows the answer scheme. Defaults to
  the instrument label names.

- copy_media:

  A logical value indicating whether to copy the files `image` points at
  into a folder beside the CSV and rewrite the column relative to it.
  Acts only when both `file` and `image` are given. `FALSE` leaves the
  original absolute paths, which is right when the coders share the
  filesystem the sheet was written on and wrong otherwise. Default is
  `TRUE`.

- file:

  A string giving the path to write the CSV to, or `NULL` (the default)
  to write nothing.

- seed:

  An integer giving the seed for the draw, so the same call draws the
  same rows. The session's random stream is put back afterwards, so this
  seed reaches the draw and nothing after it. `NULL` draws from the
  session's stream instead. Default is 1234.

## Value

A tibble with one row per document-item pair drawn, in random order so
the order leaks nothing about the draw. The same document appears once
per item it was drawn on.

The columns are for the coder, the document last so the others stay on
screen beside it.

- `id` is the document identifier, matching the results.

- `item` names which question this row is about.

- `question` is the question text as the model received it, so a coder
  answers the same question rather than a paraphrase of it. `NA` when
  the results carry no instrument.

- `allowed` holds the permitted answers, pipe-separated, so the sheet
  states its own answer scheme.

- `human_label` is blank, for the coder to fill with one value from
  `allowed`. The model's answer is deliberately absent.

- `note` is blank, for the coder. Nothing reads it, and it exists so a
  coder has somewhere to record a case they were unsure about.

- `text` is the document, in full.

- `image` is the file `image` named, or `NA` when none was. Several
  files, a video window's frames, are joined in order with `|`, the way
  `allowed` joins the answers. Rewritten relative to the CSV when
  `copy_media` acted.

## The draw

Every document-item pair has the same chance of being drawn, and the
budget is split across questions in proportion to how many pairs each
one has, so a figure computed on the sheet is already a figure about the
corpus.

Item is a stratification variable rather than a filter, so the draw runs
within each question and `n` stays the total across them. The draw is
systematic, so the sample is exactly `n` rows on every call. An `n` at
or above the number of pairs is a census.

## See also

[`validate_sample()`](https://alcocer-jj.github.io/fathom/reference/validate_sample.md),
which reads the coded sheet, and
[`coder_reference()`](https://alcocer-jj.github.io/fathom/reference/coder_reference.md)
for collapsing several coders into one.

## Examples

``` r
# Results from a real run ship with the package, so this needs no server.
p <- system.file("extdata", "vignette_results.rds", package = "fathom")
baked <- readRDS(p)
sheet <- coding_sample(baked$res_six, parlasent, text = "sentence",
  id = "id", n = 20, seed = 1234)
#> Blinded coding sample
#> 
#> Pairs         20 across 1 item
#> Drawn         20 of 100
#> 
#> Tip      Pass the completed sheet to validate_sample() together with these
#>          results.
head(sheet)
#> # A tibble: 6 × 8
#>   id     item     question                 allowed human_label note  text  image
#>   <chr>  <chr>    <chr>                    <chr>   <chr>       <chr> <chr> <chr>
#> 1 uk_088 category "Where does this senten… 1 | 2 … NA          NA    He i… NA   
#> 2 uk_049 category "Where does this senten… 1 | 2 … NA          NA    Pres… NA   
#> 3 uk_032 category "Where does this senten… 1 | 2 … NA          NA    Does… NA   
#> 4 uk_073 category "Where does this senten… 1 | 2 … NA          NA    For … NA   
#> 5 uk_047 category "Where does this senten… 1 | 2 … NA          NA    The … NA   
#> 6 uk_036 category "Where does this senten… 1 | 2 … NA          NA    So w… NA   
```
