# Append counts of unique identifiers to group labels

`var_addn()` counts distinct values of `id_var` within each level of
`grp_var` and returns a factor with labels like `"100 mg (n=6)"`. Factor
levels follow the order in which values first appear in `grp_var`, so a
pre-sorted input like `c(10, 200, 1000)` yields levels in numeric order
rather than the alphabetic default of
[`factor()`](https://rdrr.io/r/base/factor.html).

## Usage

``` r
var_addn(grp_var, id_var, sep = NULL)
```

## Arguments

- grp_var:

  Vector of grouping variable values.

- id_var:

  Vector of identifier values to count distinct entries of.

- sep:

  Optional separator to add between values of `grp_var` and appended
  counts. Default is NULL.

## Value

A factor vector with group labels appended with subject counts (e.g.,
`"100 mg (n=6)"`)

## See also

Other vectorized helpers:
[`var_dosenorm()`](https://ryancrass.github.io/pmxhelpr/reference/var_dosenorm.md),
[`var_predcorr()`](https://ryancrass.github.io/pmxhelpr/reference/var_predcorr.md)

## Examples

``` r
data <- dplyr::filter(data_sad, CMT != 3)
var_addn(data$DOSE, data$ID, sep = "mg")
#>   [1] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>   [6] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [11] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [16] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [21] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [26] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [31] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [36] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [41] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [46] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [51] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [56] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [61] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [66] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [71] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [76] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [81] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [86] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [91] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#>  [96] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#> [101] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#> [106] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#> [111] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#> [116] 10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)   10 mg (n=6)  
#> [121] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [126] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [131] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [136] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [141] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [146] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [151] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [156] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [161] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [166] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [171] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [176] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [181] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [186] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [191] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [196] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [201] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [206] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [211] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [216] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [221] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [226] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [231] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [236] 50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)   50 mg (n=6)  
#> [241] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [246] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [251] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [256] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [261] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [266] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [271] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [276] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [281] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [286] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [291] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [296] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [301] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [306] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [311] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [316] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [321] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [326] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [331] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [336] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [341] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [346] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [351] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [356] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [361] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [366] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [371] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [376] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [381] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [386] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [391] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [396] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [401] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [406] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [411] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [416] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [421] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [426] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [431] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [436] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [441] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [446] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [451] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [456] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [461] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [466] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [471] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [476] 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12) 100 mg (n=12)
#> [481] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [486] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [491] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [496] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [501] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [506] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [511] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [516] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [521] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [526] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [531] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [536] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [541] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [546] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [551] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [556] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [561] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [566] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [571] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [576] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [581] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [586] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [591] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [596] 200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6)  200 mg (n=6) 
#> [601] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [606] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [611] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [616] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [621] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [626] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [631] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [636] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [641] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [646] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [651] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [656] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [661] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [666] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [671] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [676] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [681] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [686] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [691] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [696] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [701] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [706] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [711] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> [716] 400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6)  400 mg (n=6) 
#> Levels: 10 mg (n=6) 50 mg (n=6) 100 mg (n=12) 200 mg (n=6) 400 mg (n=6)
```
