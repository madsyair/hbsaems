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

The named list spec, or `NULL` if not found. Useful fields include:

- `family` – brms family name passed to
  [`brmsfamily`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

- `link` – default link function used by the family (`"identity"`,
  `"logit"`, `"log"`, ...). See
  [`brmsfamily`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for the complete set of supported links per family.

- `discrete` – whether the response is discrete.

- `supports_mi` – whether brms-canonical
  [`mi()`](https://paulbuerkner.com/brms/reference/mi.html) imputation
  is allowed for this family (FALSE for all discrete responses).

## See also

[`register_hbsae_model`](https://madsyair.github.io/hbsaems/reference/register_hbsae_model.md),
[`brmsfamily`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
for the canonical brms family / link reference.

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
#> function(y) {
#>         v <- y[!is.na(y)]
#>         length(v) == 0L || all(v > 0)
#>       }
#> <bytecode: 0x557a8b9a71a0>
#> <environment: 0x557a8b9bafe0>
#> 
#> $response_check_msg
#> [1] "Lognormal response must be strictly positive (y > 0)."
#> 
#> $default_priors
#> function(...) NULL
#> <bytecode: 0x557a8b9abe50>
#> <environment: 0x557a8b9bafe0>
#> 
get_hbsae_model("beta")$link  # "logit"
#> [1] "logit"
```
