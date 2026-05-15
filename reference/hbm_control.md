# Sampler Configuration for HBSAE Models

Bundles the MCMC sampler arguments of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) (and the
`hbm_*` family wrappers) into a single named list. Use this when you
want a reusable sampler profile or to reduce the size of long
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) calls.

## Usage

``` r
hbm_control(
  chains = 4L,
  iter = 4000L,
  warmup = NULL,
  thin = 1L,
  cores = 1L,
  seed = NULL,
  refresh = NULL,
  adapt_delta = NULL,
  max_treedepth = NULL,
  control = NULL
)
```

## Arguments

- chains:

  Integer. Number of Markov chains (default `4L`).

- iter:

  Integer. Total iterations per chain (default `4000L`).

- warmup:

  Integer. Warm-up iterations per chain. Default `floor(iter / 2)`.

- thin:

  Integer. Thinning interval (default `1L`).

- cores:

  Integer. Number of cores for parallel chains (default `1L`).

- seed:

  Optional integer seed for reproducibility.

- refresh:

  Integer. Stan progress refresh frequency (default `NULL`: brms
  default).

- adapt_delta:

  Numeric in \\(0, 1)\\. When supplied, included in the `control` list
  as `control = list(adapt_delta = ...)`.

- max_treedepth:

  Integer. Max tree depth, included in `control` when supplied.

- control:

  Optional `list` of additional NUTS control options. Merged with any
  `adapt_delta` / `max_treedepth` above.

## Value

A named list whose elements are valid arguments of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).

## Details

This is entirely **opt-in**: the flat signatures of
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md),
[`hbm_lnln()`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md),
etc.\\ continue to work exactly as before. Pass the result directly to
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) – it is
auto-spliced via `...`.

## See also

[`hbm_priors`](https://madsyair.github.io/hbsaems/reference/hbm_priors.md),
[`hbm_nonlinear`](https://madsyair.github.io/hbsaems/reference/hbm_nonlinear.md),
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md)

## Examples

``` r
# Build a reusable "high-quality" profile
hq <- hbm_control(chains = 4, iter = 8000, cores = 4,
                  adapt_delta = 0.99, seed = 1)
str(hq)
#> List of 7
#>  $ chains : int 4
#>  $ iter   : int 8000
#>  $ warmup : int 4000
#>  $ thin   : int 1
#>  $ cores  : int 4
#>  $ seed   : num 1
#>  $ control:List of 1
#>   ..$ adapt_delta: num 0.99
#>  - attr(*, "class")= chr [1:3] "hbm_config_control" "hbm_config" "list"

# Quick draft profile
draft <- hbm_control(chains = 2, iter = 1000)
```
