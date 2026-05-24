# Simulated Fay-Herriot Normal Data

A simulated dataset for 100 regencies under the Fay-Herriot Normal
small-area model. Used as the running example throughout the package
documentation and vignettes. The simulation is engineered so that the
canonical Fay-Herriot fit (`hbm(..., sampling_variance = "D")`)
converges with default brms / Stan settings – no divergent transitions,
no manual tuning required.

## Usage

``` r
data_fhnorm
```

## Format

A data frame with 100 rows and 9 variables:

- `y`:

  Direct (survey) estimator of the area mean.

- `D`:

  Sampling variance of the direct estimator (KNOWN from the survey
  design; treat as input, not as a parameter).

- `x1`, `x2`, `x3`:

  Auxiliary covariates at the area level, simulated from
  \\\mathcal{N}(0, 1)\\.

- `theta_true`:

  True area-level latent value \\\theta_i\\.

- `u`:

  True area-level random effect \\u_i\\.

- `regency`:

  Regency identifier (`"regency_001"` through `"regency_100"`) used as
  the IID random-effect grouping variable. Use with
  `re = ~ (1 | regency)` or `area_var = "regency"`.

- `province`:

  Province identifier (`"province_01"` through `"province_05"`) – 20
  regencies per province. Used as the spatial random-effect grouping
  variable for CAR / SAR / BYM2 examples; also serves as the higher
  level in the hierarchical-area example
  `area_var = c("province", "regency")`.

## Source

Simulated. Reproducible script in `data-raw/data_fhnorm.R`.

## Details

**Generative model.** For each regency \\i = 1, \ldots, 100\\, \$\$ y_i
= \theta_i + \varepsilon_i, \quad \varepsilon_i \sim \mathcal{N}(0, D_i)
\$\$ \$\$ \theta_i = 10 + 0.8 \\ x\_{1i} - 0.5 \\ x\_{2i} + 0.3 \\
x\_{3i} + u_i, \quad u_i \sim \mathcal{N}(0, \sigma_u^2) \$\$ with
auxiliary covariates \\x_j \sim \mathcal{N}(0, 1)\\ (already
standardised), area RE SD \\\sigma_u = 1\\, and **known** sampling
variances \\D_i \sim \mathrm{Gamma}(\mathrm{shape} = 4, \mathrm{rate} =
4)\\ – a realistic spread (\\\approx \[0.2, 3.0\]\\) that mirrors
varying sample sizes across regencies.

**Important: pass `D` as the sampling variance.** In any fit on this
dataset, supply `sampling_variance = "D"`; otherwise the residual
\\\sigma\\ and the area-RE \\\sigma_u\\ compete to explain the same
variance, producing weak identifiability and divergent transitions.
