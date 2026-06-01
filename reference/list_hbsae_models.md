# List Registered HBSAE Models

Returns the keys of all model specs currently registered in the hbsaems
model registry. Built-in models (`"gaussian"`, `"beta"`, `"binomial"`,
`"lognormal"`, etc.) are always included; user-registered models appear
in addition.

## Usage

``` r
list_hbsae_models(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. If `TRUE`, return a data.frame summarising each registered
  model: family name, default brms link function, whether the family is
  discrete, and whether brms-canonical
  [`mi()`](https://paulbuerkner.com/brms/reference/mi.html) imputation
  is supported. Default `FALSE` returns a plain character vector of keys
  (backward compatible).

## Value

Character vector of model keys, or a `data.frame` with columns
`key, family, link, discrete, supports_mi` when `verbose = TRUE`.

## See also

[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md),
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)

## Examples

``` r
list_hbsae_models()
#>  [1] "beta"                        "beta-binomial"              
#>  [3] "binomial"                    "categorical"                
#>  [5] "gamma"                       "gaussian"                   
#>  [7] "geometric"                   "hurdle_lognormal"           
#>  [9] "loglogistic"                 "lognormal"                  
#> [11] "multinomial"                 "negbinomial"                
#> [13] "poisson"                     "shifted_loglogistic"        
#> [15] "skew_normal"                 "student"                    
#> [17] "zero_inflated_beta_binomial" "zero_inflated_binomial"     
#> [19] "zero_inflated_negbinomial"   "zero_inflated_poisson"      
list_hbsae_models(verbose = TRUE)
#>                            key                      family     link discrete
#> 1                         beta                        Beta    logit    FALSE
#> 2                beta-binomial               beta-binomial    logit     TRUE
#> 3                     binomial                    binomial    logit     TRUE
#> 4                  categorical                 categorical    logit     TRUE
#> 5                        gamma                       Gamma      log    FALSE
#> 6                     gaussian                    gaussian identity    FALSE
#> 7                    geometric                   geometric      log     TRUE
#> 8             hurdle_lognormal            hurdle_lognormal identity    FALSE
#> 9                  loglogistic           hbsae_loglogistic      log    FALSE
#> 10                   lognormal                   lognormal identity    FALSE
#> 11                 multinomial                 multinomial    logit     TRUE
#> 12                 negbinomial                 negbinomial      log     TRUE
#> 13                     poisson                     poisson      log     TRUE
#> 14         shifted_loglogistic   hbsae_shifted_loglogistic identity    FALSE
#> 15                 skew_normal                 skew_normal identity    FALSE
#> 16                     student                     student identity    FALSE
#> 17 zero_inflated_beta_binomial zero_inflated_beta_binomial     <NA>     TRUE
#> 18      zero_inflated_binomial      zero_inflated_binomial     <NA>     TRUE
#> 19   zero_inflated_negbinomial   zero_inflated_negbinomial     <NA>     TRUE
#> 20       zero_inflated_poisson       zero_inflated_poisson     <NA>     TRUE
#>    supports_mi
#> 1         TRUE
#> 2        FALSE
#> 3        FALSE
#> 4        FALSE
#> 5        FALSE
#> 6         TRUE
#> 7        FALSE
#> 8        FALSE
#> 9        FALSE
#> 10        TRUE
#> 11       FALSE
#> 12       FALSE
#> 13       FALSE
#> 14       FALSE
#> 15        TRUE
#> 16        TRUE
#> 17       FALSE
#> 18       FALSE
#> 19       FALSE
#> 20       FALSE
```
