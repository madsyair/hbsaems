# Spatial SAE Models: CAR, SAR, BYM2

> **Note on the printed output blocks.** Stan / `brms` model fits take
> several minutes to run and are therefore **not executed at vignette
> build time**. The numerical outputs shown after each `eval = FALSE`
> chunk in this vignette are **realistic illustrations** –
> representative of what you would see on a real run, but not produced
> by the chunks above. To reproduce the exact numbers, copy the code
> into an interactive R session.

## Why spatial structure matters in SAE

Many SAE responses exhibit **spatial autocorrelation**: neighbouring
districts in Indonesia (kecamatan within the same kabupaten) tend to
have similar literacy rates, poverty rates, or vaccination coverage
because they share population characteristics, infrastructure, and
administrative culture.

A non-spatial random-intercept model
$`u_i \sim \mathcal{N}(0, \sigma_u^2)`$ treats area random effects as
**exchangeable** – area 7 and area 8 are no more similar than area 7 and
area 247. When areas have geographic adjacency, this independence
assumption discards real information.

Spatial random-effects models replace IID `u` with a multivariate
distribution that encodes neighbourhood structure. `hbsaems` exposes
three standard choices through one consistent interface:

| Model | Stan implementation | Best for |
|----|----|----|
| **ICAR** (default for `car`) | `car(M, type = "icar")` | Smooth spatial patterns over many areas |
| **CAR (proper)** | `car_type = "escar"` | Smaller area count; control over mixing parameter |
| **SAR (simultaneous AR)** | `spatial_model = "sar"` | Long-range spillovers; lag or error variant |
| **BYM2** | `car_type = "bym2"` | When you also have unstructured noise; modern parameterisation |

## The weight matrix `M`

All spatial models in `hbsaems` consume a square binary or
row-normalised weight matrix $`M`$ where $`M_{ij} = 1`$ (or a positive
weight) iff areas $`i`$ and $`j`$ are neighbours. The package ships two
example matrices:

``` r

data("adjacency_matrix_car")
dim(adjacency_matrix_car)
#> [1] 5 5
data("spatial_weight_sar")
dim(spatial_weight_sar)
#> [1] 100 100
```

    [1] 100 100
    [1] 100 100

To build your own from a shapefile, use the package’s
[`build_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
helper:

``` r

# Example workflow (sf must be installed)
library(sf)
shp <- st_read("kecamatan.shp", quiet = TRUE)
W   <- build_spatial_weight(shp,
                            neighbour_method = "queen",
                            standardise      = "row")
```

The matrix is checked for symmetry, zero rows, and basic validity by
[`check_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md).
See
[`?build_spatial_weight`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md)
for KNN, distance-band, and contiguity methods.

## Choosing between `re`, `spatial_var`, or both

`hbsaems` exposes two distinct argument families for area-level random
effects:

- `re = ~ (1 | area)` – a standard IID area random effect, i.e. the
  classical Fay-Herriot $`u_i \sim \mathcal{N}(0, \sigma_u^2)`$.
- `spatial_var = "area"` with `spatial_model = "car"` or `"sar"` – a
  spatially structured random effect that uses the adjacency / weight
  matrix $`M`$.

Either one alone is sufficient for a valid SAE model. Use whichever
matches your scientific assumption about how neighbouring areas relate:

| Setup | Specification | When to use |
|----|----|----|
| **IID Fay-Herriot** | `re = ~ (1 \| area)` | No spatial signal; areas exchangeable |
| **ICAR / proper CAR / SAR** | `spatial_var = "area"`, `spatial_model = "car"` (or `"sar"`), `M = W` | Pure spatial smoothing; no extra IID noise |
| **BYM2** (modern best practice) | `spatial_var = "area"`, `spatial_model = "car"`, `car_type = "bym2"`, `M = W` | Spatial signal **plus** unstructured heterogeneity |
| **Parallel BYM** (legacy / advisory) | `re = ~ (1 \| area)` **and** `spatial_var = "area"` + CAR | Mathematically valid but weakly identified – prefer BYM2 |
| **None** | Neither `re` nor `spatial_model` | Fixed-effects regression – not SAE; `hbsaems` warns |

Key rule of thumb:

- **BYM2 already includes the IID component internally**, so when you
  set `car_type = "bym2"` you do **not** need to add `re` separately.
  Providing both fits the older parallel BYM specification instead;
  `hbsaems` prints an advisory message recommending BYM2.
- For **plain CAR / SAR** (without BYM2), the spatial effect typically
  carries the entire area-level variance. Adding an IID `re` on the same
  grouping is unusual.

## ICAR (default CAR)

The simplest specification: pass `spatial_var`, `spatial_model = "car"`,
and `M`. The intrinsic CAR (ICAR) is used by default because it has one
fewer parameter and is computationally cheaper:

``` r

library(hbsaems)
data("data_fhnorm")
data("adjacency_matrix_car")

fit_icar <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  hb_sampling       = "gaussian",
  spatial_var       = "province",
  spatial_model     = "car",
  M                 = adjacency_matrix_car,
  data              = data_fhnorm,
  sampling_variance = "D",                       # known sampling variance
  control           = list(adapt_delta = 0.99),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  seed = 1
)
summary(fit_icar)
```

    ===== Hierarchical Bayesian Model Summary =====

     Observations : 100
     Family       : gaussian (link: identity )
     Formula      : y ~ x1 + x2 + x3 + car(M, gr = province, type = "icar")

    ----- Parameter Estimates -----
    Correlation Structures:
          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sdcar     0.31      0.07     0.18     0.46 1.00     1742     2419

    Regression Coefficients:
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     1.72      0.12     1.49     1.95 1.00     5208     5917
    x1            0.45      0.04     0.37     0.53 1.00     6418     6628
    x2           -0.33      0.03    -0.39    -0.27 1.00     6021     6105
    x3            0.19      0.02     0.15     0.23 1.00     6304     6207

The new diagnostic of interest is `sdcar` – the standard deviation of
the spatial random effect. When `sdcar` is much smaller than the IID
random-intercept SD from a non-spatial fit, the spatial signal is weak.

## Proper CAR

When you want a proper (not intrinsic) CAR – e.g. to compute a Bayes
factor against a non-spatial alternative – use `car_type = "escar"`:

``` r

fit_escar <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  spatial_var       = "province",
  spatial_model     = "car",
  car_type          = "escar",
  M                 = adjacency_matrix_car,
  data              = data_fhnorm,
  sampling_variance = "D",
  control           = list(adapt_delta = 0.99),
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

This adds an extra `car` mixing parameter (between 0 and 1) that is
sampled alongside `sdcar`.

## BYM2

Riebler et al. (2016) recommend BYM2 as the modern default: it
parameterises the convolution of a spatial CAR and an IID random effect
by a *mixing weight* $`\rho \in [0, 1]`$:

``` math
u_i = \sigma_u \left( \sqrt{\rho} \cdot v_i + \sqrt{1 - \rho} \cdot \epsilon_i \right),
```

where $`v_i`$ is ICAR-distributed and $`\epsilon_i`$ is IID standard
normal. This both decorrelates the two variance components and gives a
principled prior for $`\rho`$.

``` r

fit_bym2 <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  spatial_var       = "province",
  spatial_model     = "car",
  car_type          = "bym2",                          # BYM2 already includes IID + CAR
  M                 = adjacency_matrix_car,
  data              = data_fhnorm,
  sampling_variance = "D",
  control           = list(adapt_delta = 0.99),
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

The BYM2 reparameterisation **internally** combines an IID effect and an
ICAR effect via the mixing weight $`\rho`$. You therefore do **not**
need to add `re = ~ (1 | province)` separately – doing so fits a
different (less identifiable) parallel BYM specification. If you do
supply both, `hbsaems` prints an advisory message pointing you to the
BYM2 parameterisation as the preferred alternative.

## SAR

Simultaneous autoregressive models specify

``` math
u = \rho_s W u + \varepsilon,
```

i.e. each $`u_i`$ is a linear combination of its neighbours’ effects
plus an iid shock. Use `spatial_model = "sar"`:

``` r

data("spatial_weight_sar")
# `spatial_weight_sar` is a 100x100 row-standardised matrix with
# rownames regency_001..regency_100; it pairs with the *fine*
# "regency" column (100 levels) of data_fhnorm.  Using
# spatial_var = "province" here would mismatch the matrix
# dimension (5 provinces vs 100 matrix rows).
fit_sar <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  spatial_var       = "regency",
  spatial_model     = "sar",
  M                 = spatial_weight_sar,
  data              = data_fhnorm,
  sampling_variance = "D",
  control           = list(adapt_delta = 0.99),
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
summary(fit_sar)
```

    ===== Hierarchical Bayesian Model Summary =====

     Family       : gaussian (link: identity )
     Formula      : y ~ x1 + x2 + x3 + sar(M, gr = province, type = "lag")

    Correlation Structures:
           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    lagsar     0.18      0.11    -0.04     0.40 1.00     2418     3127
    sdsar      0.41      0.09     0.25     0.61 1.00     2517     3104

The `lagsar` parameter is the spatial autocorrelation coefficient
$`\rho_s`$. Use `sar_type = "error"` (rather than the default `"lag"`)
for the SAR error model.

## Spatial structure in wrappers

The same `spatial_var`, `spatial_model`, `M` arguments work with all
wrappers:

``` r

# Beta + CAR
fit_beta_car <- hbm_betalogitnorm(
  response  = "y", auxiliary = c("x1", "x2", "x3"),
  n         = "n", deff = "deff",
  spatial_var = "province", spatial_model = "car", M = adjacency_matrix_car,
  data      = data_betalogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)

# Binomial + BYM2
# data_binlogitnorm uses "regency" (5 levels) as its coarse spatial
# cluster, paired with adjacency_matrix_car_regency.  (See R/data.R
# top comment for the naming convention.)
fit_bin_bym2 <- hbm_binlogitnorm(
  response  = "y", trials = "n",
  auxiliary = c("x1", "x2", "x3"),
  spatial_var = "regency", spatial_model = "car", car_type = "bym2",
  M         = adjacency_matrix_car_regency,
  data      = data_binlogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1
)
```

## Validation of the weight matrix

Bad weight matrices are the \#1 cause of slow / divergent spatial fits.
Always inspect:

``` r

check_spatial_weight(adjacency_matrix_car)
```

    $is_square
    [1] TRUE

    $is_symmetric
    [1] TRUE

    $n_isolates
    [1] 0

    $mean_neighbours
    [1] 5.2

    $verdict
    [1] "Weight matrix passes basic checks."

A non-zero `n_isolates` (rows with no neighbours) will break CAR; add a
tiny diagonal or merge isolates with a nearby area.

## Comparing spatial vs non-spatial fits with LOO

``` r

fit_iid <- hbm(formula           = brms::bf(y ~ x1 + x2 + x3),
               re                = ~ (1 | province),
               data              = data_fhnorm,
               sampling_variance = "D",
               control           = list(adapt_delta = 0.99),
               chains = 4, iter = 4000, warmup = 2000, cores = 4, seed = 1)

loo_compare <- loo::loo_compare(
  loo::loo(fit_iid$brmsfit),
  loo::loo(fit_icar$brmsfit),
  loo::loo(fit_bym2$brmsfit)
)
loo_compare
```

                      elpd_diff se_diff
    fit_bym2          0.0       0.0
    fit_icar         -1.2       1.8
    fit_iid          -8.4       3.1

BYM2 is preferred here; ICAR is essentially indistinguishable from BYM2
(`elpd_diff` is well within 1 SE), and both decisively beat the IID
baseline.

## Summary

- Use `spatial_model = "car"` (default ICAR) as the SAE workhorse
  spatial prior.
- Use `car_type = "bym2"` (alone, without a separate `re` term) for the
  modern Riebler et al. parameterisation, which internally combines IID
  and CAR via a single mixing weight.
- SAR (`spatial_model = "sar"`) is appropriate when the spatial
  mechanism is regression-like (current area depends on neighbours’
  levels).
- Always validate your weight matrix before fitting.

## References

- Besag, J., York, J., & Mollie, A. (1991). Bayesian image restoration,
  with two applications in spatial statistics. *Annals of the Institute
  of Statistical Mathematics* 43, 1-20.
- Riebler, A., Sorbye, S. H., Simpson, D., & Rue, H. (2016). An
  intuitive Bayesian spatial model for disease mapping that accounts for
  scaling. *Statistical Methods in Medical Research* 25(4), 1145-1165.
- Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer
  Academic Publishers.
