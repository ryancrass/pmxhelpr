# Customized Response versus drug concentration theme with pmxhelpr default aesthetics

Customized Response versus drug concentration theme with pmxhelpr
default aesthetics

## Usage

``` r
plot_dvconc_theme(update = NULL)
```

## Arguments

- update:

  list containing the plot elements to be updated. Run
  `plot_dvconc_theme()` with no arguments to view defaults.

## Value

a named list `list`

## Examples

``` r
plot_dvconc_theme()
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
#> [1] 1.25
#> 
#> $alpha_point_obs
#> [1] 0.5
#> 
#> $linewidth_loess
#> [1] 1
#> 
#> $linetype_loess
#> [1] 1
#> 
#> $linewidth_linear
#> [1] 1
#> 
#> $linetype_linear
#> [1] 2
#> 
#> $color_loess
#> [1] "black"
#> 
#> $color_linear
#> [1] "black"
#> 
#> $color_se_loess
#> [1] "lightgrey"
#> 
#> $color_se_linear
#> [1] "lightgrey"
#> 
#> $alpha_se_loess
#> [1] 0.4
#> 
#> $alpha_se_linear
#> [1] 0.4
#> 
new_theme <- plot_dvconc_theme(update = list(linewidth_ref = 1))
```
