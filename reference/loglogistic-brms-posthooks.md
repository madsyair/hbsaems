# brms Post-Processing Functions for the Loglogistic Family

Companion functions to
[`brms_custom_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_loglogistic.md)
that enable [`brms::loo()`](https://mc-stan.org/loo/reference/loo.html),
[`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html),
and
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
(plus
[`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html),
[`pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html),
etc.) on fitted brms models that use the loglogistic custom family.

## Usage

``` r
log_lik_loglogistic(i, prep)

posterior_predict_loglogistic(i, prep, ...)

posterior_epred_loglogistic(prep)
```

## Arguments

- i:

  Integer observation index (1-based).

- prep:

  brms preparation object passed by post-processing methods.

- ...:

  Additional arguments forwarded by brms (ignored here).

## Value

- `log_lik_loglogistic`:

  Vector of length `ndraws` containing log densities of \\y_i\\ per
  posterior draw.

- `posterior_predict_loglogistic`:

  Vector of length `ndraws` of posterior predictive draws for \\y_i\\.

- `posterior_epred_loglogistic`:

  Matrix of conditional means \\E\[Y \mid X\]\\ of size `ndraws x N`.

## Details

Users typically do not call these directly – they are wired into the
custom-family object automatically by
[`brms_custom_loglogistic()`](https://madsyair.github.io/hbsaems/reference/brms_custom_loglogistic.md).

The closed-form mean of the loglogistic distribution exists only when
\\\beta \> 1\\: \$\$E\[Y\] = \mu \pi / \[\beta \sin(\pi / \beta)\].\$\$
For \\\beta \le 1\\, the mean is undefined;
`posterior_epred_loglogistic` returns `NaN` for the offending posterior
draws.
