
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidysmd

<!-- badges: start -->
<!-- badges: end -->

The goal of tidysmd is to easily create tidy data frames of SMDs.
tidysmd wraps the smd package to easily calculate SMDs across many
variables and using several weights in order to easily compare different
adjustment strategies.

## Installation

You can install the development version of tidysmd from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("malcolmbarrett/tidysmd")
```

## Example

`tidy_smd()` supports both unweighted SMDs and weighted SMDs.

``` r
library(tidysmd)
tidy_smd(nhefs_weights, c(age, education, race), .group = qsmk)
#> # A tibble: 3 × 3
#>   variable  weights       smd
#>   <chr>     <chr>       <dbl>
#> 1 age       unweighted -0.282
#> 2 education unweighted  0.196
#> 3 race      unweighted  0.177
```

`nhefs_weights` contains several types of propensity score weights for
which we can calculate SMDs. Unweighted SMDs are also included by
default.

``` r
tidy_smd(
  nhefs_weights,
  c(age, race, education),
  .group = qsmk,
  .wts = c(w_ate, w_att, w_atm)
)
#> # A tibble: 12 × 3
#>    variable  weights         smd
#>    <chr>     <chr>         <dbl>
#>  1 age       unweighted -0.282  
#>  2 race      unweighted  0.177  
#>  3 education unweighted  0.196  
#>  4 age       w_ate      -0.00585
#>  5 race      w_ate       0.00664
#>  6 education w_ate       0.0347 
#>  7 age       w_att      -0.0120 
#>  8 race      w_att       0.00365
#>  9 education w_att       0.0267 
#> 10 age       w_atm      -0.00184
#> 11 race      w_atm       0.00113
#> 12 education w_atm       0.00934
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
library(ggplot2)
plot_df <- tidy_smd(
  nhefs_weights,
  race:active,
  .group = qsmk,
  .wts = starts_with("w_")
)

ggplot(
  data = plot_df,
  mapping = aes(x = abs(smd), y = variable, group = weights, color = weights)
) +
  geom_line(orientation = "y") +
  geom_point() + 
  geom_vline(xintercept = 0.1, color = "black", size = 0.1)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
