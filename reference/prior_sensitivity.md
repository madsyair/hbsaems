# Power-Scale Prior Sensitivity Diagnostics for Fitted HBMs

Computes prior and likelihood power-scaling sensitivity diagnostics for
a fitted `hbmfit` model using the priorsense package. Useful for
assessing whether posterior conclusions are driven by the prior or the
data – a critical step in any principled Bayesian SAE workflow.

## Usage

``` r
prior_sensitivity(model, ...)
```

## Arguments

- model:

  An `hbmfit` object returned by
  [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) (or one
  of its wrappers) or a `brmsfit` object directly.

- ...:

  Additional arguments forwarded to
  [`priorsense::powerscale_sensitivity()`](https://n-kall.github.io/priorsense/reference/powerscale-sensitivity.html),
  e.g.\\ `variable = c("b_x1", "sd_regency__Intercept")` to restrict the
  report to specific parameters.

## Value

A `powerscale_sensitivity_summary` object (data frame) with one row per
monitored parameter and columns `variable`, `prior`, `likelihood`,
`diagnosis`. `NULL` (with a message) when the priorsense package is not
installed.

## Details

Prior sensitivity analysis answers the question: “If I had used a
slightly different prior, would the substantive conclusions change?”.
The power-scaling approach of Kallioinen et al.\\ (2023) detects:

- **Prior–likelihood conflict**: the posterior moves non-negligibly when
  the prior is up- or down-weighted. Often indicates an overly
  informative or misspecified prior.

- **Weak likelihood**: the posterior is dominated by the prior. Common
  in SAE for areas with few sampled units.

Reported diagnostics include the Kullback–Leibler divergence between the
original posterior and the power-scaled posterior (`prior`,
`likelihood`) and a categorical flag (`prior-data conflict`,
`strong prior`, `-`).

**Computational cost.** No re-sampling is required: importance sampling
reuses the existing posterior draws. Hence a typical run costs only a
few seconds even for large hierarchical models.

## When to run prior sensitivity

Always. Specifically:

- After every model fit, before drawing substantive conclusions.

- Whenever convergence diagnostics from
  [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md)
  are clean but the posterior seems implausibly narrow or implausibly
  wide.

- When comparing models with shrinkage priors – horseshoe and R2D2 are
  both informative, and small differences in their hyperparameters can
  move estimates noticeably.

## References

Kallioinen, N., Paananen, T., Burkner, P.-C., & Vehtari, A.\\ (2024).
Detecting and diagnosing prior and likelihood sensitivity with
power-scaling. *Statistics and Computing*, 34, 57.
[doi:10.1007/s11222-023-10366-5](https://doi.org/10.1007/s11222-023-10366-5)

## See also

[`prior_check`](https://madsyair.github.io/hbsaems/reference/prior_check.md),
[`convergence_check`](https://madsyair.github.io/hbsaems/reference/convergence_check.md),
[`model_compare`](https://madsyair.github.io/hbsaems/reference/model_compare.md)

## Examples

``` r
# \donttest{
if (requireNamespace("priorsense", quietly = TRUE)) {
  data("data_fhnorm")
  fit <- hbm(brms::bf(y ~ x1 + x2),
             data = data_fhnorm, re = ~(1 | regency),
             chains = 4, iter = 2000, refresh = 0)
  ps  <- prior_sensitivity(fit)
  print(ps)
}
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 106 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.06, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> Sensitivity based on cjs_dist
#> Prior selection: all priors
#> Likelihood selection: all data
#> 
#>                          variable prior likelihood diagnosis
#>                       b_Intercept 0.002      0.322         -
#>                              b_x1 0.001      0.361         -
#>                              b_x2 0.000      0.584         -
#>             sd_regency__Intercept 0.002      5.708         -
#>                             sigma 0.002      6.771         -
#>                         Intercept 0.002      0.369         -
#>  r_regency[regency_001,Intercept] 0.001      3.680         -
#>  r_regency[regency_002,Intercept] 0.000      1.984         -
#>  r_regency[regency_003,Intercept] 0.001      6.253         -
#>  r_regency[regency_004,Intercept] 0.001      0.611         -
#>  r_regency[regency_005,Intercept] 0.000      0.648         -
#>  r_regency[regency_006,Intercept] 0.001      2.706         -
#>  r_regency[regency_007,Intercept] 0.000      4.668         -
#>  r_regency[regency_008,Intercept] 0.001      2.813         -
#>  r_regency[regency_009,Intercept] 0.000      3.517         -
#>  r_regency[regency_010,Intercept] 0.000      0.716         -
#>  r_regency[regency_011,Intercept] 0.001      0.750         -
#>  r_regency[regency_012,Intercept] 0.000      5.377         -
#>  r_regency[regency_013,Intercept] 0.001      2.913         -
#>  r_regency[regency_014,Intercept] 0.000      2.798         -
#>  r_regency[regency_015,Intercept] 0.001      3.399         -
#>  r_regency[regency_016,Intercept] 0.001      4.071         -
#>  r_regency[regency_017,Intercept] 0.001      5.837         -
#>  r_regency[regency_018,Intercept] 0.001      3.588         -
#>  r_regency[regency_019,Intercept] 0.001      6.081         -
#>  r_regency[regency_020,Intercept] 0.000      1.649         -
#>  r_regency[regency_021,Intercept] 0.000      3.614         -
#>  r_regency[regency_022,Intercept] 0.000      0.608         -
#>  r_regency[regency_023,Intercept] 0.000      2.652         -
#>  r_regency[regency_024,Intercept] 0.000      1.573         -
#>  r_regency[regency_025,Intercept] 0.001      3.255         -
#>  r_regency[regency_026,Intercept] 0.000      0.684         -
#>  r_regency[regency_027,Intercept] 0.001      1.540         -
#>  r_regency[regency_028,Intercept] 0.001      3.461         -
#>  r_regency[regency_029,Intercept] 0.000      1.603         -
#>  r_regency[regency_030,Intercept] 0.000      0.920         -
#>  r_regency[regency_031,Intercept] 0.001      1.234         -
#>  r_regency[regency_032,Intercept] 0.001      0.657         -
#>  r_regency[regency_033,Intercept] 0.000      5.370         -
#>  r_regency[regency_034,Intercept] 0.000      4.777         -
#>  r_regency[regency_035,Intercept] 0.001      3.235         -
#>  r_regency[regency_036,Intercept] 0.001      1.466         -
#>  r_regency[regency_037,Intercept] 0.001      2.906         -
#>  r_regency[regency_038,Intercept] 0.001      1.558         -
#>  r_regency[regency_039,Intercept] 0.000      2.427         -
#>  r_regency[regency_040,Intercept] 0.000      4.616         -
#>  r_regency[regency_041,Intercept] 0.000      3.551         -
#>  r_regency[regency_042,Intercept] 0.000      2.811         -
#>  r_regency[regency_043,Intercept] 0.000      3.537         -
#>  r_regency[regency_044,Intercept] 0.001      1.057         -
#>  r_regency[regency_045,Intercept] 0.001      4.034         -
#>  r_regency[regency_046,Intercept] 0.001      4.371         -
#>  r_regency[regency_047,Intercept] 0.001      3.408         -
#>  r_regency[regency_048,Intercept] 0.001      2.612         -
#>  r_regency[regency_049,Intercept] 0.000      1.443         -
#>  r_regency[regency_050,Intercept] 0.001      1.149         -
#>  r_regency[regency_051,Intercept] 0.000      1.383         -
#>  r_regency[regency_052,Intercept] 0.001      2.378         -
#>  r_regency[regency_053,Intercept] 0.000      2.580         -
#>  r_regency[regency_054,Intercept] 0.001      1.988         -
#>  r_regency[regency_055,Intercept] 0.000      1.679         -
#>  r_regency[regency_056,Intercept] 0.001      2.207         -
#>  r_regency[regency_057,Intercept] 0.000      1.154         -
#>  r_regency[regency_058,Intercept] 0.001      0.908         -
#>  r_regency[regency_059,Intercept] 0.001      1.226         -
#>  r_regency[regency_060,Intercept] 0.001      2.882         -
#>  r_regency[regency_061,Intercept] 0.000      2.991         -
#>  r_regency[regency_062,Intercept] 0.000      4.570         -
#>  r_regency[regency_063,Intercept] 0.001      1.596         -
#>  r_regency[regency_064,Intercept] 0.001      2.057         -
#>  r_regency[regency_065,Intercept] 0.001      2.301         -
#>  r_regency[regency_066,Intercept] 0.001      2.980         -
#>  r_regency[regency_067,Intercept] 0.000      2.356         -
#>  r_regency[regency_068,Intercept] 0.001      3.639         -
#>  r_regency[regency_069,Intercept] 0.000      1.832         -
#>  r_regency[regency_070,Intercept] 0.001      3.185         -
#>  r_regency[regency_071,Intercept] 0.000      2.622         -
#>  r_regency[regency_072,Intercept] 0.001      3.140         -
#>  r_regency[regency_073,Intercept] 0.000      4.041         -
#>  r_regency[regency_074,Intercept] 0.001      0.729         -
#>  r_regency[regency_075,Intercept] 0.000      1.085         -
#>  r_regency[regency_076,Intercept] 0.000      2.606         -
#>  r_regency[regency_077,Intercept] 0.001      1.059         -
#>  r_regency[regency_078,Intercept] 0.001      2.562         -
#>  r_regency[regency_079,Intercept] 0.000      4.921         -
#>  r_regency[regency_080,Intercept] 0.001      1.452         -
#>  r_regency[regency_081,Intercept] 0.001      0.549         -
#>  r_regency[regency_082,Intercept] 0.000      3.394         -
#>  r_regency[regency_083,Intercept] 0.001      2.732         -
#>  r_regency[regency_084,Intercept] 0.001      0.641         -
#>  r_regency[regency_085,Intercept] 0.001      2.241         -
#>  r_regency[regency_086,Intercept] 0.001      3.554         -
#>  r_regency[regency_087,Intercept] 0.000      1.855         -
#>  r_regency[regency_088,Intercept] 0.001      0.971         -
#>  r_regency[regency_089,Intercept] 0.000      2.801         -
#>  r_regency[regency_090,Intercept] 0.000      0.806         -
#>  r_regency[regency_091,Intercept] 0.000      2.468         -
#>  r_regency[regency_092,Intercept] 0.001      0.731         -
#>  r_regency[regency_093,Intercept] 0.001      1.859         -
#>  r_regency[regency_094,Intercept] 0.001      3.581         -
#>  r_regency[regency_095,Intercept] 0.001      0.860         -
#>  r_regency[regency_096,Intercept] 0.001      2.478         -
#>  r_regency[regency_097,Intercept] 0.001      2.139         -
#>  r_regency[regency_098,Intercept] 0.001      0.871         -
#>  r_regency[regency_099,Intercept] 0.001      0.938         -
#>  r_regency[regency_100,Intercept] 0.001      2.604         -
#>                          z_1[1,1] 0.000      2.155         -
#>                          z_1[1,2] 0.000      1.520         -
#>                          z_1[1,3] 0.000      3.520         -
#>                          z_1[1,4] 0.000      1.068         -
#>                          z_1[1,5] 0.000      1.040         -
#>                          z_1[1,6] 0.000      1.696         -
#>                          z_1[1,7] 0.000      2.565         -
#>                          z_1[1,8] 0.000      1.659         -
#>                          z_1[1,9] 0.000      1.980         -
#>                         z_1[1,10] 0.000      1.004         -
#>                         z_1[1,11] 0.000      1.000         -
#>                         z_1[1,12] 0.001      2.983         -
#>                         z_1[1,13] 0.000      1.710         -
#>                         z_1[1,14] 0.000      1.922         -
#>                         z_1[1,15] 0.000      2.210         -
#>                         z_1[1,16] 0.000      2.265         -
#>                         z_1[1,17] 0.000      3.515         -
#>                         z_1[1,18] 0.000      2.176         -
#>                         z_1[1,19] 0.000      3.542         -
#>                         z_1[1,20] 0.000      1.386         -
#>                         z_1[1,21] 0.000      2.099         -
#>                         z_1[1,22] 0.000      1.025         -
#>                         z_1[1,23] 0.000      1.836         -
#>                         z_1[1,24] 0.000      1.396         -
#>                         z_1[1,25] 0.000      1.876         -
#>                         z_1[1,26] 0.000      1.078         -
#>                         z_1[1,27] 0.000      1.290         -
#>                         z_1[1,28] 0.000      2.115         -
#>                         z_1[1,29] 0.000      1.439         -
#>                         z_1[1,30] 0.000      0.996         -
#>                         z_1[1,31] 0.000      1.267         -
#>                         z_1[1,32] 0.000      1.142         -
#>                         z_1[1,33] 0.000      2.761         -
#>                         z_1[1,34] 0.000      2.205         -
#>                         z_1[1,35] 0.000      1.833         -
#>                         z_1[1,36] 0.000      1.407         -
#>                         z_1[1,37] 0.000      1.979         -
#>                         z_1[1,38] 0.000      1.177         -
#>                         z_1[1,39] 0.000      1.626         -
#>                         z_1[1,40] 0.000      2.820         -
#>                         z_1[1,41] 0.000      1.976         -
#>                         z_1[1,42] 0.000      1.992         -
#>                         z_1[1,43] 0.000      2.018         -
#>                         z_1[1,44] 0.000      1.140         -
#>                         z_1[1,45] 0.000      2.515         -
#>                         z_1[1,46] 0.000      2.707         -
#>                         z_1[1,47] 0.000      2.023         -
#>                         z_1[1,48] 0.000      1.842         -
#>                         z_1[1,49] 0.000      1.319         -
#>                         z_1[1,50] 0.000      1.330         -
#>                         z_1[1,51] 0.000      1.227         -
#>                         z_1[1,52] 0.000      1.881         -
#>                         z_1[1,53] 0.000      1.725         -
#>                         z_1[1,54] 0.000      1.255         -
#>                         z_1[1,55] 0.000      1.447         -
#>                         z_1[1,56] 0.000      1.567         -
#>                         z_1[1,57] 0.000      0.987         -
#>                         z_1[1,58] 0.000      0.991         -
#>                         z_1[1,59] 0.000      1.188         -
#>                         z_1[1,60] 0.000      2.015         -
#>                         z_1[1,61] 0.000      1.911         -
#>                         z_1[1,62] 0.000      2.517         -
#>                         z_1[1,63] 0.000      1.362         -
#>                         z_1[1,64] 0.000      1.486         -
#>                         z_1[1,65] 0.000      1.779         -
#>                         z_1[1,66] 0.000      1.968         -
#>                         z_1[1,67] 0.000      1.497         -
#>                         z_1[1,68] 0.000      2.147         -
#>                         z_1[1,69] 0.000      1.482         -
#>                         z_1[1,70] 0.000      2.035         -
#>                         z_1[1,71] 0.000      1.791         -
#>                         z_1[1,72] 0.000      1.807         -
#>                         z_1[1,73] 0.000      2.571         -
#>                         z_1[1,74] 0.000      0.990         -
#>                         z_1[1,75] 0.000      1.223         -
#>                         z_1[1,76] 0.000      1.837         -
#>                         z_1[1,77] 0.000      0.998         -
#>                         z_1[1,78] 0.000      1.679         -
#>                         z_1[1,79] 0.000      2.924         -
#>                         z_1[1,80] 0.000      1.448         -
#>                         z_1[1,81] 0.000      1.006         -
#>                         z_1[1,82] 0.000      1.916         -
#>                         z_1[1,83] 0.000      1.699         -
#>                         z_1[1,84] 0.000      1.011         -
#>                         z_1[1,85] 0.000      1.591         -
#>                         z_1[1,86] 0.000      2.132         -
#>                         z_1[1,87] 0.000      1.470         -
#>                         z_1[1,88] 0.000      1.134         -
#>                         z_1[1,89] 0.000      1.618         -
#>                         z_1[1,90] 0.000      1.000         -
#>                         z_1[1,91] 0.000      1.631         -
#>                         z_1[1,92] 0.000      1.148         -
#>                         z_1[1,93] 0.000      1.320         -
#>                         z_1[1,94] 0.000      2.257         -
#>                         z_1[1,95] 0.000      0.987         -
#>                         z_1[1,96] 0.000      1.802         -
#>                         z_1[1,97] 0.000      1.556         -
#>                         z_1[1,98] 0.000      1.004         -
#>                         z_1[1,99] 0.000      1.267         -
#>                        z_1[1,100] 0.000      1.673         -
# }
```
