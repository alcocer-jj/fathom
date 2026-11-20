# Turn a folder of images into a corpus measure() can take

Builds a tibble of one row per file, with the columns the rest of the
package expects. `id` keys the results and the audit joins, `image` is
the path
[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md)
reads, and `bytes` is a size covariate.

Nothing is decoded. `bytes` comes from
[`file.size()`](https://rdrr.io/r/base/file.info.html), which needs no
image reader and no system library.

## Usage

``` r
prepare_image(paths, id = NULL)
```

## Arguments

- paths:

  A character vector of file paths.

- id:

  A character vector of identifiers, one per path. Defaults to the file
  name without its extension, which is what a corpus of scanned pages
  usually keys on.

## Value

A tibble with `id`, `image`, `bytes`, `exists` and `ext`.

## Missing files

A path that does not resolve comes back with `exists = FALSE` and
`bytes` as `NA` rather than being silently removed, because a corpus
that quietly lost four documents is the failure this exists to prevent.
Filter on `exists` after reading the count.

## See also

[`measure()`](https://www.alcocerjj.com/fathom/reference/measure.md),
which reads the `image` column.

## Examples

``` r
f <- tempfile(fileext = ".png")
grDevices::png(f)
graphics::plot.new()
grDevices::dev.off()
#> agg_record_81817554ee37 
#>                       2 

prepare_image(f)
#> # A tibble: 1 × 5
#>   id               image                                      bytes exists ext  
#>   <chr>            <chr>                                      <dbl> <lgl>  <chr>
#> 1 file81815455b593 /var/folders/0d/6_y1swz50c7gy3g7v86g93240…  6128 TRUE   png  
```
