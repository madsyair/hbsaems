# brms Post-Processing Functions for the Shifted Loglogistic Family

Companion functions to
[`brms_custom_shifted_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_shifted_loglogistic.md)
that enable [`brms::loo()`](https://mc-stan.org/loo/reference/loo.html),
[`brms::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html),
and
[`brms::posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
(plus related helpers) on brmsfits using the shifted-loglogistic custom
family.

## Usage

``` r
log_lik_shifted_loglogistic(i, prep)

posterior_predict_shifted_loglogistic(i, prep, ...)

posterior_epred_shifted_loglogistic(prep)
```

## Arguments

- i:

  Integer observation index (1-based).

- prep:

  brms preparation object.

- ...:

  Forwarded extra arguments (ignored).

## Value

- `log_lik_shifted_loglogistic`:

  Log-density vector.

- `posterior_predict_shifted_loglogistic`:

  One predictive draw per posterior sample.

- `posterior_epred_shifted_loglogistic`:

  Conditional mean matrix. The mean is finite only when \\\xi \< 1\\;
  otherwise the return is `NaN` for those draws.

## Details

The conditional mean of the shifted loglogistic is \$\$E\[Y\] = \mu +
\sigma \\ (\Gamma(1 + \xi) \Gamma(1 - \xi) - 1) / \xi \quad (\xi \< 1,
\xi \neq 0),\$\$ with the logistic limit \\E\[Y\] = \mu\\ at \\\xi =
0\\.
