# Customized VPC theme with pmxhelpr default aesthetics

Customized VPC theme with pmxhelpr default aesthetics

## Usage

``` r
pmxhelpr_vpc_theme(update = NULL)
```

## Arguments

- update:

  list containing the plot elements to be updated. Run
  `pmxhelpr_vpc_theme()` with no arguments to view defaults.

## Value

a `list`with vpc theme specifiers

## Examples

``` r
pmxhelpr_vpc_theme()
#> $obs_color
#> [1] "#0000FF"
#> 
#> $obs_size
#> [1] 1
#> 
#> $obs_median_color
#> [1] "#FF0000"
#> 
#> $obs_median_linetype
#> [1] "solid"
#> 
#> $obs_median_size
#> [1] 1
#> 
#> $obs_alpha
#> [1] 0.7
#> 
#> $obs_shape
#> [1] 1
#> 
#> $obs_ci_color
#> [1] "#0000FF"
#> 
#> $obs_ci_linetype
#> [1] "dashed"
#> 
#> $obs_ci_fill
#> [1] "#80808033"
#> 
#> $obs_ci_size
#> [1] 0.5
#> 
#> $sim_pi_fill
#> [1] "#0000FF"
#> 
#> $sim_pi_alpha
#> [1] 0.15
#> 
#> $sim_pi_color
#> [1] "#000000"
#> 
#> $sim_pi_linetype
#> [1] "dotted"
#> 
#> $sim_pi_size
#> [1] 1
#> 
#> $sim_median_fill
#> [1] "#FF0000"
#> 
#> $sim_median_alpha
#> [1] 0.3
#> 
#> $sim_median_color
#> [1] "#000000"
#> 
#> $sim_median_linetype
#> [1] "dashed"
#> 
#> $sim_median_size
#> [1] 1
#> 
#> $bin_separators_color
#> [1] "#000000"
#> 
#> $loq_color
#> [1] "#990000"
#> 
#> attr(,"class")
#> [1] "vpc_theme"
new_theme <- pmxhelpr_vpc_theme(update = vpc::new_vpc_theme()) #restores vpc package defaults
```
