# Build items by substituting categories into one question stem

Turns a list of categories into an instrument of independent binary
items. The category goes into the question, not the answer, so
categories may be any length and the tokenization constraint never
applies.

## Usage

``` r
template_items(stem, categories, lang = c("en", "es"), medium = NULL)
```

## Arguments

- stem:

  A named stem, or a custom string containing `{label}`.

- categories:

  A character vector of categories. Names, if present, become item
  names.

- lang:

  A string giving the language for the named stems, `"en"` (the default)
  or `"es"`. Ignored for a custom string, which is used exactly as
  written. This sets the question only. The scaffolding around it is
  [`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)'s
  own `lang`, and leaving that at its default gives a Spanish question
  under an English cue.

- medium:

  A string naming the stem set a named stem is looked up in, one of
  `"text"`, `"audio"`, `"image"` or `"video"`. Read only for a named
  stem, since a custom string carries its own wording. There is no
  default, because the sets differ by medium rather than translating, so
  a default would answer a question about the document with a guess.

## Value

A named list of questions, one per category, ready to pass to
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
as its first argument.

Names are the names of `categories`, so they become the item names in
the results and in
[`item_report()`](https://alcocer-jj.github.io/fathom/reference/item_report.md).
Pass a named vector to control them. An unnamed vector uses the values
themselves, which rarely suits a category that runs to a phrase.

Values are the stem with `{label}` replaced by each category's text.
Order is preserved, and an instrument reads in the order its items were
declared rather than alphabetically.

The result is an ordinary list, so an item can be edited before it
reaches
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md)
when one category needs wording the stem cannot give it.

## The named stems

Four sets, one per medium, each in English and in Spanish. `medium`
picks the set and `lang` picks the language.

|           |                                              |
|-----------|----------------------------------------------|
| medium    | stems                                        |
| `"text"`  | topic, mention, express, about, frame        |
| `"audio"` | topic, mention, express, about, frame        |
| `"image"` | show, depict, express, about, frame          |
| `"video"` | show, depict, discuss, express, about, frame |

The sets are not translations of one another, which is why `medium` has
no default.

The `text` set in full. The other three take the same shape with their
own noun and their own verbs.

|  |  |  |
|----|----|----|
| stem | en | es |
| `"topic"` | Does this text discuss {label}? | ¿Este texto trata sobre {label}? |
| `"mention"` | Does this text mention {label}? | ¿Este texto menciona {label}? |
| `"express"` | Does this text express {label}? | ¿Este texto expresa {label}? |
| `"about"` | Is this text primarily about {label}? | ¿Este texto trata principalmente de {label}? |
| `"frame"` | Does this text frame the issue in terms of {label}? | ¿Este texto enmarca el tema en términos de {label}? |

These differ in what they assert, not in topic. `"mention"` fires on a
passing reference. `"about"` requires the text to be centrally concerned
with the category. The choice is a measurement decision, and it is the
same decision in either language, since the two halves of a row are
translations of one another rather than separate instruments.

A custom string ignores `lang` entirely, so a Spanish question written
by hand is passed through as typed. The scaffolding around it is a
separate setting, on
[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md),
and the two can disagree. See its `lang` argument.

## See also

[`instrument()`](https://alcocer-jj.github.io/fathom/reference/instrument.md),
which takes the items.

## Examples

``` r
template_items("topic", c(econ = "the economy", immig = "immigration"),
               medium = "text")
#> $econ
#> [1] "Does this text discuss the economy?"
#> 
#> $immig
#> [1] "Does this text discuss immigration?"
#> 

# The same stem name asks a different question of a different document.
template_items("about", c(econ = "the economy"), medium = "image")
#> $econ
#> [1] "Is this image primarily about the economy?"
#> 

# A custom string carries its own wording, so it needs no medium.
template_items("Does the author appear to endorse {label}?", c("austerity"))
#> $austerity
#> [1] "Does the author appear to endorse austerity?"
#> 
```
