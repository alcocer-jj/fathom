# Print the assembled prompt

Prints the exact string the model will receive. Makes no model calls.
Prompt failures do not appear in the output of
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md).

## Usage

``` r
preview(
  instrument,
  text = "[document text goes here]",
  item = NULL,
  image = NULL,
  truncate = 300L
)
```

## Arguments

- instrument:

  An
  [`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md).

- text:

  An example document. A short placeholder by default.

- item:

  A string naming the item to show. Defaults to the first.

- image:

  A character vector of paths, one or several, to the media the run
  would send, or `NULL` (the default) to send none. A video window is a
  vector of frame paths. Their presence removes the document block from
  the prompt, since an image corpus has no text to quote, and adds a
  bracketed line above it. That line is shown and never sent. Anything
  in square brackets here describes what travels beside the prompt
  rather than quoting what is inside it.

- truncate:

  An integer giving the number of characters of document text to show.
  `NULL` shows all. Default is 300.

## Value

Invisibly, the assembled prompt string for the item shown, exactly as
the model would receive it. A single character scalar.

The printed output is larger than the return value and the difference
matters. Printing shows the prompt and, beneath it, the table of what
each label will be matched against. That table is where most extraction
failures are visible, and it is not in the returned string.

So capture the value to diff two prompts or to paste one into a paper,
and read the printed output to check the instrument. No call is made
either way.

## The matching table

The table underneath has two columns. On the left the label, on the
right every token that will be counted as that label. They read the same
when a label is its own only variant, which is the usual arrangement.
They diverge once accented forms are supplied, and again after probing
registers whatever token a split label actually emits.

Under prefix matching the right column is not the whole story, since a
token also counts when it merely opens one of those forms. `BU` counts
as BUDGET without appearing there. The instrument header says which rule
is in force.

## See also

[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
for the prompt's parts and
[`measure()`](https://alcocer-jj.github.io/fathom/reference/measure.md)
for sending it.

## Examples

``` r
b <- instrument(
  template_items("topic", c(econ = "the economy", immig = "immigration"),
                 medium = "text"),
  labels = c("YES", "NO")
)
preview(b)
#> Prompt preview
#> 
#> Item          econ, 1 of 2
#> Scoring       generated position 1
#> 
#> ----------------------------------------------------------------------
#> Text: "[document text goes here]"
#> 
#> Does this text discuss the economy?
#> 
#> Answer with exactly one word: YES or NO.
#> 
#> Answer:
#> ----------------------------------------------------------------------
#> 
#> Label         Counts as
#> YES           YES
#> NO            NO
```
