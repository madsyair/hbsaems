# Inspect a Registered HBSAE Model Specification

Inspect a Registered HBSAE Model Specification

## Usage

``` r
get_hbsae_model(key)
```

## Arguments

- key:

  Character. A model key returned by
  [`list_hbsae_models`](https://madsyair.github.io/hbsaems/reference/list_hbsae_models.md).

## Value

The named list spec, or `NULL` if not found.

## See also

[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md)

## Examples

``` r
get_hbsae_model("lognormal")
#> $family
#> [1] "lognormal"
#> 
#> $link
#> [1] "identity"
#> 
#> $discrete
#> [1] FALSE
#> 
#> $supports_mi
#> [1] TRUE
#> 
#> $has_addition_term
#> [1] FALSE
#> 
#> $addition_template
#> NULL
#> 
#> $response_check
#> function (y) 
#> {
#>     v <- y[!is.na(y)]
#>     length(v) == 0L || all(v > 0)
#> }
#> <bytecode: 0x562e43e78298>
#> <environment: 0x562e43e9c350>
#> 
#> $response_check_msg
#> [1] "Lognormal response must be strictly positive (y > 0)."
#> 
#> $default_priors
#> function (...) 
#> NULL
#> <bytecode: 0x562e43e69ce8>
#> <environment: 0x562e43e9c350>
#> 
```
