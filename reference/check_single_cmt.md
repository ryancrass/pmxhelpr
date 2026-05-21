# Warn when a post-`EVID == 0` filter still contains multiple unique CMT values

Defensive check intended to be called immediately after an internal
`dplyr::filter(data, EVID == 0)` step. If the filtered data carries more
than one unique value of `CMT`, the function emits a
[`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html)
recommending the caller pre-filter to a single observation compartment.
No-op when the `CMT` column is absent or the data is empty.

## Usage

``` r
check_single_cmt(data, name = "data")
```

## Arguments

- data:

  Filtered data frame.

- name:

  Character scalar, name of the upstream argument used in the warning
  text. Defaults to `"data"`.

## Value

`invisible(NULL)`.
