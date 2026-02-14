# Count the non-missing observations in each exact bin

`df_nobsbin()` is a helper function to count the number of missing and
non-missing observations in exact bins.

## Usage

``` r
df_nobsbin(data, bin_var = "NTIME", strat_vars = NULL)
```

## Arguments

- data:

  Input dataset.

- bin_var:

  Binning variable. Default is `"NTIME"`.

- strat_vars:

  Stratifying variables. Must be a character vector.

## Value

A data.frame containing one row per unique combination of `bin_var` and
`strat_vars` and new variables `n_obs`, a count of non-missing
observations, and `n_miss`, a count of missing observations.

## Examples

``` r
df_nobsbin(data_sad)
#> # A tibble: 19 × 4
#>    NTIME   CMT n_obs n_miss
#>    <dbl> <dbl> <int>  <int>
#>  1   0       2     0     36
#>  2   0.5     2    34      2
#>  3   1       2    36      0
#>  4   1.5     2    36      0
#>  5   2       2    36      0
#>  6   3       2    36      0
#>  7   4       2    36      0
#>  8   5       2    36      0
#>  9   8       2    36      0
#> 10  12       2    36      0
#> 11  16       2    36      0
#> 12  24       2    36      0
#> 13  36       2    36      0
#> 14  48       2    33      3
#> 15  72       2    29      7
#> 16  96       2    16     20
#> 17 120       2     6     30
#> 18 144       2     1     35
#> 19 168       2     0     36
```
