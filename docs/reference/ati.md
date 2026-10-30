# Mexican access-to-information requests

Three hundred requests filed through Mexico's INFOMEX transparency
system, drawn from a corpus Erlich et al. hand-coded with six Mexico
City research assistants over ten weeks. Their codings are not shipped
here. What ships is the request text and the four fields that trace one
back to its source, which is what a run with no training data needs. The
coded variables are in the replication materials named under `@source`,
but the codebook was never published and the supplemental material names
them without saying how they were defined, so what arrives is column
names. The requests are in Spanish and run from a single word to 588,
median 42.

## Usage

``` r
ati
```

## Format

A tibble with 300 rows and 5 columns.

- `id` is the row identifier from the source data.

- `folio` is the INFOMEX request number, which traces back to the
  source.

- `date` is the date of the request.

- `dependency` is the agency the request was filed with.

- `text` is the request as filed, in Spanish.

## Source

Erlich, A., S. G. Dantas, B. E. Bagozzi, D. Berliner, and B.
Palmer-Rubin. 2022. Multi-label Prediction for Political Text-as-Data.
*Political Analysis* 30(4), 463–480.
[doi:10.1017/pan.2021.15](https://doi.org/10.1017/pan.2021.15)

Replication data at
[doi:10.7910/DVN/SOVPA4](https://doi.org/10.7910/DVN/SOVPA4) , released
under CC0 1.0. Sampled with seed 444. The underlying requests are
published by Mexico's federal transparency system.

## The text

`text` is what the requester typed. Commas were stripped when the source
was written to CSV, which is not recoverable and shows as double spaces.

## Examples

``` r
data(ati)
```
