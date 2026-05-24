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
             chains = 2, iter = 1000, refresh = 0)
  ps  <- prior_sensitivity(fit)
  print(ps)
}
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 343 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.66, indicating chains have not mixed.
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
#>                       b_Intercept 0.001      0.926         -
#>                              b_x1 0.001      2.252         -
#>                              b_x2 0.001      1.389         -
#>             sd_regency__Intercept 0.003     10.708         -
#>                             sigma 0.001     10.709         -
#>                         Intercept 0.002      0.963         -
#>  r_regency[regency_001,Intercept] 0.001      6.730         -
#>  r_regency[regency_002,Intercept] 0.000      2.447         -
#>  r_regency[regency_003,Intercept] 0.001      9.335         -
#>  r_regency[regency_004,Intercept] 0.001      1.681         -
#>  r_regency[regency_005,Intercept] 0.000      1.005         -
#>  r_regency[regency_006,Intercept] 0.001      5.189         -
#>  r_regency[regency_007,Intercept] 0.002      7.951         -
#>  r_regency[regency_008,Intercept] 0.001      2.570         -
#>  r_regency[regency_009,Intercept] 0.001      6.382         -
#>  r_regency[regency_010,Intercept] 0.000      0.966         -
#>  r_regency[regency_011,Intercept] 0.000      1.009         -
#>  r_regency[regency_012,Intercept] 0.001      8.947         -
#>  r_regency[regency_013,Intercept] 0.000      2.663         -
#>  r_regency[regency_014,Intercept] 0.001      4.591         -
#>  r_regency[regency_015,Intercept] 0.001      4.919         -
#>  r_regency[regency_016,Intercept] 0.001      6.645         -
#>  r_regency[regency_017,Intercept] 0.001      8.869         -
#>  r_regency[regency_018,Intercept] 0.001      5.432         -
#>  r_regency[regency_019,Intercept] 0.001      9.197         -
#>  r_regency[regency_020,Intercept] 0.000      1.954         -
#>  r_regency[regency_021,Intercept] 0.001      6.164         -
#>  r_regency[regency_022,Intercept] 0.001      1.207         -
#>  r_regency[regency_023,Intercept] 0.001      4.616         -
#>  r_regency[regency_024,Intercept] 0.001      3.306         -
#>  r_regency[regency_025,Intercept] 0.001      4.438         -
#>  r_regency[regency_026,Intercept] 0.001      0.558         -
#>  r_regency[regency_027,Intercept] 0.000      1.162         -
#>  r_regency[regency_028,Intercept] 0.001      5.957         -
#>  r_regency[regency_029,Intercept] 0.000      1.155         -
#>  r_regency[regency_030,Intercept] 0.000      1.008         -
#>  r_regency[regency_031,Intercept] 0.001      0.843         -
#>  r_regency[regency_032,Intercept] 0.001      1.751         -
#>  r_regency[regency_033,Intercept] 0.002      9.077         -
#>  r_regency[regency_034,Intercept] 0.001      5.859         -
#>  r_regency[regency_035,Intercept] 0.001      6.722         -
#>  r_regency[regency_036,Intercept] 0.000      2.292         -
#>  r_regency[regency_037,Intercept] 0.001      4.915         -
#>  r_regency[regency_038,Intercept] 0.000      3.139         -
#>  r_regency[regency_039,Intercept] 0.000      3.745         -
#>  r_regency[regency_040,Intercept] 0.001      8.976         -
#>  r_regency[regency_041,Intercept] 0.001      7.016         -
#>  r_regency[regency_042,Intercept] 0.001      6.977         -
#>  r_regency[regency_043,Intercept] 0.001      4.888         -
#>  r_regency[regency_044,Intercept] 0.001      1.937         -
#>  r_regency[regency_045,Intercept] 0.002      6.789         -
#>  r_regency[regency_046,Intercept] 0.001      7.022         -
#>  r_regency[regency_047,Intercept] 0.001      4.964         -
#>  r_regency[regency_048,Intercept] 0.001      3.899         -
#>  r_regency[regency_049,Intercept] 0.000      1.709         -
#>  r_regency[regency_050,Intercept] 0.000      1.882         -
#>  r_regency[regency_051,Intercept] 0.001      1.472         -
#>  r_regency[regency_052,Intercept] 0.001      4.296         -
#>  r_regency[regency_053,Intercept] 0.001      5.296         -
#>  r_regency[regency_054,Intercept] 0.001      4.987         -
#>  r_regency[regency_055,Intercept] 0.001      3.498         -
#>  r_regency[regency_056,Intercept] 0.001      3.656         -
#>  r_regency[regency_057,Intercept] 0.000      1.230         -
#>  r_regency[regency_058,Intercept] 0.000      1.918         -
#>  r_regency[regency_059,Intercept] 0.001      1.255         -
#>  r_regency[regency_060,Intercept] 0.001      4.636         -
#>  r_regency[regency_061,Intercept] 0.001      5.537         -
#>  r_regency[regency_062,Intercept] 0.002      9.353         -
#>  r_regency[regency_063,Intercept] 0.000      2.897         -
#>  r_regency[regency_064,Intercept] 0.001      3.757         -
#>  r_regency[regency_065,Intercept] 0.001      3.696         -
#>  r_regency[regency_066,Intercept] 0.001      4.372         -
#>  r_regency[regency_067,Intercept] 0.001      4.079         -
#>  r_regency[regency_068,Intercept] 0.001      5.835         -
#>  r_regency[regency_069,Intercept] 0.000      2.003         -
#>  r_regency[regency_070,Intercept] 0.001      6.147         -
#>  r_regency[regency_071,Intercept] 0.001      4.318         -
#>  r_regency[regency_072,Intercept] 0.001      4.138         -
#>  r_regency[regency_073,Intercept] 0.002      9.039         -
#>  r_regency[regency_074,Intercept] 0.001      1.266         -
#>  r_regency[regency_075,Intercept] 0.001      2.265         -
#>  r_regency[regency_076,Intercept] 0.001      3.424         -
#>  r_regency[regency_077,Intercept] 0.000      2.166         -
#>  r_regency[regency_078,Intercept] 0.001      4.711         -
#>  r_regency[regency_079,Intercept] 0.001      8.235         -
#>  r_regency[regency_080,Intercept] 0.000      2.541         -
#>  r_regency[regency_081,Intercept] 0.001      0.923         -
#>  r_regency[regency_082,Intercept] 0.001      6.692         -
#>  r_regency[regency_083,Intercept] 0.001      3.763         -
#>  r_regency[regency_084,Intercept] 0.000      0.993         -
#>  r_regency[regency_085,Intercept] 0.001      1.907         -
#>  r_regency[regency_086,Intercept] 0.001      5.295         -
#>  r_regency[regency_087,Intercept] 0.000      2.652         -
#>  r_regency[regency_088,Intercept] 0.000      1.558         -
#>  r_regency[regency_089,Intercept] 0.001      4.375         -
#>  r_regency[regency_090,Intercept] 0.001      0.876         -
#>  r_regency[regency_091,Intercept] 0.001      4.279         -
#>  r_regency[regency_092,Intercept] 0.002      1.484         -
#>  r_regency[regency_093,Intercept] 0.001      2.683         -
#>  r_regency[regency_094,Intercept] 0.001      6.587         -
#>  r_regency[regency_095,Intercept] 0.001      2.252         -
#>  r_regency[regency_096,Intercept] 0.000      1.912         -
#>  r_regency[regency_097,Intercept] 0.001      3.498         -
#>  r_regency[regency_098,Intercept] 0.000      0.892         -
#>  r_regency[regency_099,Intercept] 0.001      1.195         -
#>  r_regency[regency_100,Intercept] 0.002      4.502         -
#>                          z_1[1,1] 0.001      5.143         -
#>                          z_1[1,2] 0.001      3.103         -
#>                          z_1[1,3] 0.001      7.341         -
#>                          z_1[1,4] 0.001      2.909         -
#>                          z_1[1,5] 0.000      2.688         -
#>                          z_1[1,6] 0.000      3.974         -
#>                          z_1[1,7] 0.001      6.032         -
#>                          z_1[1,8] 0.001      2.520         -
#>                          z_1[1,9] 0.001      5.325         -
#>                         z_1[1,10] 0.001      2.567         -
#>                         z_1[1,11] 0.000      2.246         -
#>                         z_1[1,12] 0.001      7.010         -
#>                         z_1[1,13] 0.001      3.002         -
#>                         z_1[1,14] 0.001      4.547         -
#>                         z_1[1,15] 0.001      4.516         -
#>                         z_1[1,16] 0.000      4.379         -
#>                         z_1[1,17] 0.001      7.090         -
#>                         z_1[1,18] 0.001      4.051         -
#>                         z_1[1,19] 0.001      6.874         -
#>                         z_1[1,20] 0.001      2.259         -
#>                         z_1[1,21] 0.001      4.931         -
#>                         z_1[1,22] 0.001      2.878         -
#>                         z_1[1,23] 0.001      4.252         -
#>                         z_1[1,24] 0.001      3.086         -
#>                         z_1[1,25] 0.001      4.265         -
#>                         z_1[1,26] 0.001      2.349         -
#>                         z_1[1,27] 0.001      2.296         -
#>                         z_1[1,28] 0.001      4.932         -
#>                         z_1[1,29] 0.001      2.467         -
#>                         z_1[1,30] 0.001      2.662         -
#>                         z_1[1,31] 0.001      2.435         -
#>                         z_1[1,32] 0.001      2.211         -
#>                         z_1[1,33] 0.001      7.451         -
#>                         z_1[1,34] 0.001      5.280         -
#>                         z_1[1,35] 0.001      5.380         -
#>                         z_1[1,36] 0.001      2.749         -
#>                         z_1[1,37] 0.001      4.323         -
#>                         z_1[1,38] 0.000      3.065         -
#>                         z_1[1,39] 0.001      3.722         -
#>                         z_1[1,40] 0.001      6.678         -
#>                         z_1[1,41] 0.001      5.562         -
#>                         z_1[1,42] 0.001      4.777         -
#>                         z_1[1,43] 0.001      3.867         -
#>                         z_1[1,44] 0.001      3.214         -
#>                         z_1[1,45] 0.001      5.323         -
#>                         z_1[1,46] 0.001      5.792         -
#>                         z_1[1,47] 0.001      3.844         -
#>                         z_1[1,48] 0.001      4.028         -
#>                         z_1[1,49] 0.001      2.938         -
#>                         z_1[1,50] 0.001      3.155         -
#>                         z_1[1,51] 0.001      3.058         -
#>                         z_1[1,52] 0.001      3.960         -
#>                         z_1[1,53] 0.001      3.549         -
#>                         z_1[1,54] 0.000      4.639         -
#>                         z_1[1,55] 0.001      3.679         -
#>                         z_1[1,56] 0.001      3.203         -
#>                         z_1[1,57] 0.000      2.857         -
#>                         z_1[1,58] 0.001      2.512         -
#>                         z_1[1,59] 0.001      2.449         -
#>                         z_1[1,60] 0.001      4.226         -
#>                         z_1[1,61] 0.001      4.337         -
#>                         z_1[1,62] 0.001      6.866         -
#>                         z_1[1,63] 0.001      3.521         -
#>                         z_1[1,64] 0.001      3.449         -
#>                         z_1[1,65] 0.001      3.561         -
#>                         z_1[1,66] 0.001      3.984         -
#>                         z_1[1,67] 0.000      3.658         -
#>                         z_1[1,68] 0.001      4.318         -
#>                         z_1[1,69] 0.000      2.586         -
#>                         z_1[1,70] 0.000      4.435         -
#>                         z_1[1,71] 0.001      3.638         -
#>                         z_1[1,72] 0.001      3.458         -
#>                         z_1[1,73] 0.001      6.544         -
#>                         z_1[1,74] 0.001      2.982         -
#>                         z_1[1,75] 0.001      3.136         -
#>                         z_1[1,76] 0.001      3.813         -
#>                         z_1[1,77] 0.001      2.913         -
#>                         z_1[1,78] 0.001      3.310         -
#>                         z_1[1,79] 0.001      6.946         -
#>                         z_1[1,80] 0.001      3.250         -
#>                         z_1[1,81] 0.001      2.401         -
#>                         z_1[1,82] 0.001      5.297         -
#>                         z_1[1,83] 0.001      3.667         -
#>                         z_1[1,84] 0.001      2.827         -
#>                         z_1[1,85] 0.001      2.646         -
#>                         z_1[1,86] 0.001      5.057         -
#>                         z_1[1,87] 0.000      3.456         -
#>                         z_1[1,88] 0.001      2.339         -
#>                         z_1[1,89] 0.001      4.110         -
#>                         z_1[1,90] 0.000      2.485         -
#>                         z_1[1,91] 0.001      2.975         -
#>                         z_1[1,92] 0.001      3.017         -
#>                         z_1[1,93] 0.001      3.375         -
#>                         z_1[1,94] 0.001      4.180         -
#>                         z_1[1,95] 0.000      3.170         -
#>                         z_1[1,96] 0.001      3.062         -
#>                         z_1[1,97] 0.001      3.851         -
#>                         z_1[1,98] 0.001      2.626         -
#>                         z_1[1,99] 0.001      3.018         -
#>                        z_1[1,100] 0.001      5.100         -
# }
```
