# List Registered HBSAE Models

Returns the keys of all model specs currently registered in the hbsaems
model registry. Built-in models (`"gaussian"`, `"beta"`, `"binomial"`,
`"lognormal"`, etc.) are always included; user-registered models appear
in addition.

## Usage

``` r
list_hbsae_models()
```

## Value

Character vector of model keys.

## See also

[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md),
[`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)

## Examples

``` r
list_hbsae_models()
#>  [1] "bernoulli"                   "beta"                       
#>  [3] "beta-binomial"               "binomial"                   
#>  [5] "categorical"                 "gaussian"                   
#>  [7] "geometric"                   "loglogistic"                
#>  [9] "lognormal"                   "multinomial"                
#> [11] "negbinomial"                 "poisson"                    
#> [13] "shifted_loglogistic"         "zero_inflated_beta_binomial"
#> [15] "zero_inflated_binomial"      "zero_inflated_negbinomial"  
#> [17] "zero_inflated_poisson"      
```
