# Customized Concentration-time theme with pmxhelpr default aesthetics

Customized Concentration-time theme with pmxhelpr default aesthetics

## Usage

``` r
plot_dvtime_theme(update = NULL)
```

## Arguments

- update:

  list containing the plot elements to be updated. Run
  `plot_dvtime_theme()` with no arguments to view defaults.

## Value

a named list `list`

## Examples

``` r
plot_dvtime_theme()
#> $linewidth_ref
#> [1] 0.5
#> 
#> $linetype_ref
#> [1] 2
#> 
#> $alpha_line_ref
#> [1] 1
#> 
#> $shape_point_obs
#> [1] 1
#> 
#> $size_point_obs
#> [1] 0.75
#> 
#> $alpha_point_obs
#> [1] 0.5
#> 
#> $linewidth_obs
#> [1] 0.5
#> 
#> $linetype_obs
#> [1] 1
#> 
#> $alpha_line_obs
#> [1] 0.5
#> 
#> $shape_point_cent
#> [1] 16
#> 
#> $size_point_cent
#> [1] 1.25
#> 
#> $alpha_point_cent
#> [1] 1
#> 
#> $linewidth_cent
#> [1] 0.75
#> 
#> $linetype_cent
#> [1] 1
#> 
#> $alpha_line_cent
#> [1] 1
#> 
#> $linewidth_errorbar
#> [1] 0.75
#> 
#> $linetype_errorbar
#> [1] 1
#> 
#> $alpha_errorbar
#> [1] 1
#> 
#> $width_errorbar
#> NULL
#> 
new_theme <- plot_dvtime_theme(update = list(linewidth_ref = 1))
```
