# MCMC Convergence Diagnostics for Fitted HBMs

Computes a battery of convergence tests and diagnostic plots for an
`hbmfit` object. This is the primary convergence diagnostic function in
hbsaems (supersedes the deprecated
[`hbcc`](https://madsyair.github.io/hbsaems/reference/deprecated.md)).

## Usage

``` r
convergence_check(
  model,
  diag_tests = c("rhat", "geweke", "heidel", "raftery"),
  plot_types = c("trace", "dens", "acf", "nuts_energy", "rhat", "neff"),
  ...
)
```

## Arguments

- model:

  An `hbmfit` or `brmsfit` object.

- diag_tests:

  Character vector of tests to run. Any subset of
  `c("rhat", "geweke", "heidel", "raftery")`.

- plot_types:

  Character vector of plot types to generate. Any subset of
  `c("trace", "dens", "acf", "nuts_energy", "rhat", "neff")`.

- ...:

  Additional arguments passed to the underlying brms or coda routines.

## Value

An `hbcc_results` object containing:

- `rhat_ess`:

  Matrix with columns `Rhat`, `Bulk_ESS`, `Tail_ESS`.

- `geweke,raftery,heidel`:

  Outputs from the corresponding coda routines, or `NULL` when the test
  fails.

- `plots`:

  Named list of `ggplot`/`bayesplot` objects.

## Details

For each parameter \\\theta_j\\, the Gelman-Rubin statistic is computed
as \$\$\widehat{R}\_j =
\sqrt{\frac{\widehat{\mathrm{var}}^+(\theta_j)}{W_j}}\$\$ where \\W_j\\
is the within-chain variance and \\\widehat{\mathrm{var}}^+\\ is the
marginal posterior variance estimate. Values close to 1 (typically below
1.1) indicate convergence.

## See also

[`is_converged`](https://madsyair.github.io/hbsaems/reference/is_converged.md),
[`diagnostic_summary`](https://madsyair.github.io/hbsaems/reference/diagnostic_summary.md),
[`hbm_warnings`](https://madsyair.github.io/hbsaems/reference/hbm_warnings.md)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
# Uses brms's default MCMC settings (chains = 4, iter = 2000,
# warmup = 1000).  For challenging posteriors (e.g. funnel
# geometries in Fay-Herriot with small D_i), consider
# chains = 4, iter = 4000, warmup = 2000 and
# control = list(adapt_delta = 0.99).
model <- hbm(brms::bf(y ~ x1 + x2 + x3),
             data   = data_fhnorm,
             re     = ~ (1 | regency),    # area-level random effect
             chains = 4, iter = 2000, warmup = 1000,
             cores  = 1, seed = 123, refresh = 0)
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 25 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.35, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

diag <- convergence_check(model)
#> Warning: Low E-BFMI (< 0.3) in 4 chain(s); the posterior may have heavy tails the sampler explores poorly. Consider reparameterisation or longer warmup.
#> Warning: 25 divergent transition(s) detected; estimates may be biased. Increase adapt_delta (e.g. 0.99) or reparameterise.
summary(diag)
#> 
#> ===== Convergence Diagnostics Summary =====
#> 
#> Divergent transitions: 25 (0.62%)  <-- WARNING
#> E-BFMI (min over chains): 0.008  <-- WARNING (< 0.3)
#> Max-treedepth hit rate: 0.00%
#> 
#> R-hat:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.005   1.049   1.073   1.078   1.095   1.346 
#> 
#> Bulk ESS:
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    9.364   31.783  114.041  456.203  680.919 2670.063 
#> 
#> Tail ESS:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   12.63  601.22  792.12  855.54 1032.55 2115.53 
#> 
#> Geweke test (Z-scores):
#> [[1]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                         -0.15824                          1.60612 
#>                             b_x2                             b_x3 
#>                          0.88420                         -0.53576 
#>            sd_regency__Intercept                            sigma 
#>                         -2.52915                          2.29708 
#>                        Intercept r_regency[regency_001,Intercept] 
#>                         -0.27931                          2.88638 
#> r_regency[regency_002,Intercept] r_regency[regency_003,Intercept] 
#>                          0.65180                          1.96971 
#> r_regency[regency_004,Intercept] r_regency[regency_005,Intercept] 
#>                          1.56594                         -0.71116 
#> r_regency[regency_006,Intercept] r_regency[regency_007,Intercept] 
#>                         -1.89830                          1.92871 
#> r_regency[regency_008,Intercept] r_regency[regency_009,Intercept] 
#>                         -1.62656                          1.90548 
#> r_regency[regency_010,Intercept] r_regency[regency_011,Intercept] 
#>                          1.26861                         -0.32483 
#> r_regency[regency_012,Intercept] r_regency[regency_013,Intercept] 
#>                         -2.41782                          3.13014 
#> r_regency[regency_014,Intercept] r_regency[regency_015,Intercept] 
#>                          2.26103                         -3.13972 
#> r_regency[regency_016,Intercept] r_regency[regency_017,Intercept] 
#>                          2.19214                          2.61427 
#> r_regency[regency_018,Intercept] r_regency[regency_019,Intercept] 
#>                          3.12511                          2.31012 
#> r_regency[regency_020,Intercept] r_regency[regency_021,Intercept] 
#>                         -2.59151                         -2.34553 
#> r_regency[regency_022,Intercept] r_regency[regency_023,Intercept] 
#>                         -2.18168                         -2.37945 
#> r_regency[regency_024,Intercept] r_regency[regency_025,Intercept] 
#>                         -2.08990                         -1.77198 
#> r_regency[regency_026,Intercept] r_regency[regency_027,Intercept] 
#>                         -0.43900                          1.34708 
#> r_regency[regency_028,Intercept] r_regency[regency_029,Intercept] 
#>                         -2.59289                          2.07400 
#> r_regency[regency_030,Intercept] r_regency[regency_031,Intercept] 
#>                          1.30394                          0.17685 
#> r_regency[regency_032,Intercept] r_regency[regency_033,Intercept] 
#>                          0.46099                         -2.30008 
#> r_regency[regency_034,Intercept] r_regency[regency_035,Intercept] 
#>                         -1.78855                          3.67220 
#> r_regency[regency_036,Intercept] r_regency[regency_037,Intercept] 
#>                          1.00633                          1.96558 
#> r_regency[regency_038,Intercept] r_regency[regency_039,Intercept] 
#>                         -0.37151                         -2.37390 
#> r_regency[regency_040,Intercept] r_regency[regency_041,Intercept] 
#>                         -2.09146                         -1.99156 
#> r_regency[regency_042,Intercept] r_regency[regency_043,Intercept] 
#>                         -1.91026                         -1.31370 
#> r_regency[regency_044,Intercept] r_regency[regency_045,Intercept] 
#>                         -3.04051                          2.58990 
#> r_regency[regency_046,Intercept] r_regency[regency_047,Intercept] 
#>                         -3.08812                         -2.00331 
#> r_regency[regency_048,Intercept] r_regency[regency_049,Intercept] 
#>                          3.39274                         -0.75400 
#> r_regency[regency_050,Intercept] r_regency[regency_051,Intercept] 
#>                          0.92543                         -2.41609 
#> r_regency[regency_052,Intercept] r_regency[regency_053,Intercept] 
#>                          2.00808                          1.54248 
#> r_regency[regency_054,Intercept] r_regency[regency_055,Intercept] 
#>                         -1.41961                         -0.61713 
#> r_regency[regency_056,Intercept] r_regency[regency_057,Intercept] 
#>                          1.41193                         -0.29367 
#> r_regency[regency_058,Intercept] r_regency[regency_059,Intercept] 
#>                         -2.10775                         -3.65790 
#> r_regency[regency_060,Intercept] r_regency[regency_061,Intercept] 
#>                          2.28392                         -2.09853 
#> r_regency[regency_062,Intercept] r_regency[regency_063,Intercept] 
#>                         -2.67487                         -1.52616 
#> r_regency[regency_064,Intercept] r_regency[regency_065,Intercept] 
#>                          2.64206                          3.16218 
#> r_regency[regency_066,Intercept] r_regency[regency_067,Intercept] 
#>                          2.48188                         -2.25529 
#> r_regency[regency_068,Intercept] r_regency[regency_069,Intercept] 
#>                          2.04454                          3.01248 
#> r_regency[regency_070,Intercept] r_regency[regency_071,Intercept] 
#>                          3.32831                         -2.42450 
#> r_regency[regency_072,Intercept] r_regency[regency_073,Intercept] 
#>                          2.85159                          2.40675 
#> r_regency[regency_074,Intercept] r_regency[regency_075,Intercept] 
#>                         -1.45759                          1.49635 
#> r_regency[regency_076,Intercept] r_regency[regency_077,Intercept] 
#>                         -2.05644                          1.26249 
#> r_regency[regency_078,Intercept] r_regency[regency_079,Intercept] 
#>                          2.91413                         -2.15001 
#> r_regency[regency_080,Intercept] r_regency[regency_081,Intercept] 
#>                         -0.85733                          3.69162 
#> r_regency[regency_082,Intercept] r_regency[regency_083,Intercept] 
#>                          2.20703                          0.91390 
#> r_regency[regency_084,Intercept] r_regency[regency_085,Intercept] 
#>                          0.15637                         -2.90992 
#> r_regency[regency_086,Intercept] r_regency[regency_087,Intercept] 
#>                         -1.91843                         -0.54891 
#> r_regency[regency_088,Intercept] r_regency[regency_089,Intercept] 
#>                         -0.23481                         -2.63679 
#> r_regency[regency_090,Intercept] r_regency[regency_091,Intercept] 
#>                          1.92409                         -2.02769 
#> r_regency[regency_092,Intercept] r_regency[regency_093,Intercept] 
#>                          2.39729                          4.36079 
#> r_regency[regency_094,Intercept] r_regency[regency_095,Intercept] 
#>                         -2.38297                          0.02689 
#> r_regency[regency_096,Intercept] r_regency[regency_097,Intercept] 
#>                          4.00009                         -2.27511 
#> r_regency[regency_098,Intercept] r_regency[regency_099,Intercept] 
#>                         -0.25542                         -1.92359 
#> r_regency[regency_100,Intercept]                           lprior 
#>                          2.62163                         -0.37048 
#>                             lp__                         z_1[1,1] 
#>                         -2.28691                          2.11962 
#>                         z_1[1,2]                         z_1[1,3] 
#>                          0.17088                          2.61201 
#>                         z_1[1,4]                         z_1[1,5] 
#>                          1.24362                         -1.13379 
#>                         z_1[1,6]                         z_1[1,7] 
#>                         -1.09598                          2.06534 
#>                         z_1[1,8]                         z_1[1,9] 
#>                         -0.78965                          2.58728 
#>                        z_1[1,10]                        z_1[1,11] 
#>                          1.87922                         -0.60529 
#>                        z_1[1,12]                        z_1[1,13] 
#>                         -2.61012                          3.89437 
#>                        z_1[1,14]                        z_1[1,15] 
#>                          2.64685                         -3.34579 
#>                        z_1[1,16]                        z_1[1,17] 
#>                          2.11491                          2.40921 
#>                        z_1[1,18]                        z_1[1,19] 
#>                          3.45070                          2.70676 
#>                        z_1[1,20]                        z_1[1,21] 
#>                         -2.48499                         -2.12125 
#>                        z_1[1,22]                        z_1[1,23] 
#>                         -2.00494                         -1.20430 
#>                        z_1[1,24]                        z_1[1,25] 
#>                         -1.68542                         -2.43801 
#>                        z_1[1,26]                        z_1[1,27] 
#>                          0.54678                          1.53436 
#>                        z_1[1,28]                        z_1[1,29] 
#>                         -2.19845                          0.74513 
#>                        z_1[1,30]                        z_1[1,31] 
#>                          1.05580                          0.15998 
#>                        z_1[1,32]                        z_1[1,33] 
#>                          0.03103                         -2.27982 
#>                        z_1[1,34]                        z_1[1,35] 
#>                         -2.39870                          2.94442 
#>                        z_1[1,36]                        z_1[1,37] 
#>                         -0.02712                          2.69929 
#>                        z_1[1,38]                        z_1[1,39] 
#>                         -0.27441                         -2.51660 
#>                        z_1[1,40]                        z_1[1,41] 
#>                         -2.65653                         -2.48684 
#>                        z_1[1,42]                        z_1[1,43] 
#>                         -1.45412                         -0.63725 
#>                        z_1[1,44]                        z_1[1,45] 
#>                         -2.01200                          2.40829 
#>                        z_1[1,46]                        z_1[1,47] 
#>                         -3.80717                         -1.68514 
#>                        z_1[1,48]                        z_1[1,49] 
#>                          2.42407                         -1.43542 
#>                        z_1[1,50]                        z_1[1,51] 
#>                          1.02343                         -3.09506 
#>                        z_1[1,52]                        z_1[1,53] 
#>                          2.06200                          0.15922 
#>                        z_1[1,54]                        z_1[1,55] 
#>                         -0.30823                         -0.51112 
#>                        z_1[1,56]                        z_1[1,57] 
#>                          0.39254                          0.85266 
#>                        z_1[1,58]                        z_1[1,59] 
#>                         -2.40147                         -2.56096 
#>                        z_1[1,60]                        z_1[1,61] 
#>                          3.11313                         -2.24344 
#>                        z_1[1,62]                        z_1[1,63] 
#>                         -3.31091                          0.31605 
#>                        z_1[1,64]                        z_1[1,65] 
#>                          2.06285                          3.29296 
#>                        z_1[1,66]                        z_1[1,67] 
#>                          1.76390                         -1.70594 
#>                        z_1[1,68]                        z_1[1,69] 
#>                          2.34826                          3.17592 
#>                        z_1[1,70]                        z_1[1,71] 
#>                          3.03904                         -1.71170 
#>                        z_1[1,72]                        z_1[1,73] 
#>                          1.68881                          3.09550 
#>                        z_1[1,74]                        z_1[1,75] 
#>                         -0.34911                          0.54012 
#>                        z_1[1,76]                        z_1[1,77] 
#>                         -2.43748                          0.84206 
#>                        z_1[1,78]                        z_1[1,79] 
#>                          2.06353                         -2.33366 
#>                        z_1[1,80]                        z_1[1,81] 
#>                         -1.59284                          2.28628 
#>                        z_1[1,82]                        z_1[1,83] 
#>                          1.78741                          0.66343 
#>                        z_1[1,84]                        z_1[1,85] 
#>                         -0.91615                         -2.18634 
#>                        z_1[1,86]                        z_1[1,87] 
#>                         -2.18771                         -0.48931 
#>                        z_1[1,88]                        z_1[1,89] 
#>                         -1.41842                         -1.63330 
#>                        z_1[1,90]                        z_1[1,91] 
#>                          0.87632                         -2.02861 
#>                        z_1[1,92]                        z_1[1,93] 
#>                          2.03824                          1.55679 
#>                        z_1[1,94]                        z_1[1,95] 
#>                         -3.47342                          0.84319 
#>                        z_1[1,96]                        z_1[1,97] 
#>                          0.69043                         -1.61334 
#>                        z_1[1,98]                        z_1[1,99] 
#>                          0.02948                         -1.67558 
#>                       z_1[1,100] 
#>                          3.30813 
#> 
#> 
#> [[2]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                         -1.95358                         -7.60496 
#>                             b_x2                             b_x3 
#>                          4.08080                          0.36287 
#>            sd_regency__Intercept                            sigma 
#>                          3.92596                         -4.61273 
#>                        Intercept r_regency[regency_001,Intercept] 
#>                         -1.37008                         -4.66496 
#> r_regency[regency_002,Intercept] r_regency[regency_003,Intercept] 
#>                         -1.16556                         -3.38597 
#> r_regency[regency_004,Intercept] r_regency[regency_005,Intercept] 
#>                          4.54002                          6.47226 
#> r_regency[regency_006,Intercept] r_regency[regency_007,Intercept] 
#>                          5.94546                         -2.67606 
#> r_regency[regency_008,Intercept] r_regency[regency_009,Intercept] 
#>                          4.47313                         -2.71448 
#> r_regency[regency_010,Intercept] r_regency[regency_011,Intercept] 
#>                         -3.56064                         -2.25937 
#> r_regency[regency_012,Intercept] r_regency[regency_013,Intercept] 
#>                          2.95943                         -2.09253 
#> r_regency[regency_014,Intercept] r_regency[regency_015,Intercept] 
#>                         -1.33955                          4.79756 
#> r_regency[regency_016,Intercept] r_regency[regency_017,Intercept] 
#>                         -3.53830                         -3.13828 
#> r_regency[regency_018,Intercept] r_regency[regency_019,Intercept] 
#>                         -1.90589                         -2.78391 
#> r_regency[regency_020,Intercept] r_regency[regency_021,Intercept] 
#>                          3.64799                          3.74094 
#> r_regency[regency_022,Intercept] r_regency[regency_023,Intercept] 
#>                          1.38284                          2.40322 
#> r_regency[regency_024,Intercept] r_regency[regency_025,Intercept] 
#>                          3.94104                          6.11481 
#> r_regency[regency_026,Intercept] r_regency[regency_027,Intercept] 
#>                          4.95190                          2.71749 
#> r_regency[regency_028,Intercept] r_regency[regency_029,Intercept] 
#>                          4.41540                         -2.35089 
#> r_regency[regency_030,Intercept] r_regency[regency_031,Intercept] 
#>                          2.54996                          2.35183 
#> r_regency[regency_032,Intercept] r_regency[regency_033,Intercept] 
#>                          8.73909                          4.57671 
#> r_regency[regency_034,Intercept] r_regency[regency_035,Intercept] 
#>                          2.84530                         -2.73561 
#> r_regency[regency_036,Intercept] r_regency[regency_037,Intercept] 
#>                         -0.86591                         -2.95689 
#> r_regency[regency_038,Intercept] r_regency[regency_039,Intercept] 
#>                          6.41156                          3.91242 
#> r_regency[regency_040,Intercept] r_regency[regency_041,Intercept] 
#>                          3.54186                          3.36529 
#> r_regency[regency_042,Intercept] r_regency[regency_043,Intercept] 
#>                          3.42232                          2.95368 
#> r_regency[regency_044,Intercept] r_regency[regency_045,Intercept] 
#>                          5.12949                         -2.68025 
#> r_regency[regency_046,Intercept] r_regency[regency_047,Intercept] 
#>                          4.97750                          5.05569 
#> r_regency[regency_048,Intercept] r_regency[regency_049,Intercept] 
#>                         -1.46592                          1.20026 
#> r_regency[regency_050,Intercept] r_regency[regency_051,Intercept] 
#>                         -0.25201                          2.28409 
#> r_regency[regency_052,Intercept] r_regency[regency_053,Intercept] 
#>                         -4.36636                         -1.60295 
#> r_regency[regency_054,Intercept] r_regency[regency_055,Intercept] 
#>                         10.33209                          0.13763 
#> r_regency[regency_056,Intercept] r_regency[regency_057,Intercept] 
#>                         -1.63304                          0.85220 
#> r_regency[regency_058,Intercept] r_regency[regency_059,Intercept] 
#>                          5.07435                          1.21834 
#> r_regency[regency_060,Intercept] r_regency[regency_061,Intercept] 
#>                         -4.00153                          3.15308 
#> r_regency[regency_062,Intercept] r_regency[regency_063,Intercept] 
#>                          4.04068                          4.78043 
#> r_regency[regency_064,Intercept] r_regency[regency_065,Intercept] 
#>                         -3.70226                         -3.16944 
#> r_regency[regency_066,Intercept] r_regency[regency_067,Intercept] 
#>                         -3.08322                          2.35621 
#> r_regency[regency_068,Intercept] r_regency[regency_069,Intercept] 
#>                         -4.18234                          1.02832 
#> r_regency[regency_070,Intercept] r_regency[regency_071,Intercept] 
#>                         -3.49818                          5.24672 
#> r_regency[regency_072,Intercept] r_regency[regency_073,Intercept] 
#>                         -3.84102                         -2.83323 
#> r_regency[regency_074,Intercept] r_regency[regency_075,Intercept] 
#>                          0.45650                          1.01657 
#> r_regency[regency_076,Intercept] r_regency[regency_077,Intercept] 
#>                          4.05114                          1.69138 
#> r_regency[regency_078,Intercept] r_regency[regency_079,Intercept] 
#>                         -2.33362                          3.28153 
#> r_regency[regency_080,Intercept] r_regency[regency_081,Intercept] 
#>                          9.24354                         -4.21282 
#> r_regency[regency_082,Intercept] r_regency[regency_083,Intercept] 
#>                         -2.51509                         -0.47672 
#> r_regency[regency_084,Intercept] r_regency[regency_085,Intercept] 
#>                         -1.24322                          3.39027 
#> r_regency[regency_086,Intercept] r_regency[regency_087,Intercept] 
#>                          4.21053                          6.63743 
#> r_regency[regency_088,Intercept] r_regency[regency_089,Intercept] 
#>                          6.50785                          5.93525 
#> r_regency[regency_090,Intercept] r_regency[regency_091,Intercept] 
#>                         -3.22985                          7.61296 
#> r_regency[regency_092,Intercept] r_regency[regency_093,Intercept] 
#>                         -3.00287                         -2.76285 
#> r_regency[regency_094,Intercept] r_regency[regency_095,Intercept] 
#>                          3.35161                         -0.80392 
#> r_regency[regency_096,Intercept] r_regency[regency_097,Intercept] 
#>                         -1.58111                          3.74010 
#> r_regency[regency_098,Intercept] r_regency[regency_099,Intercept] 
#>                          9.19121                          3.83075 
#> r_regency[regency_100,Intercept]                           lprior 
#>                         -3.12281                          0.25977 
#>                             lp__                         z_1[1,1] 
#>                          7.54083                         -4.21147 
#>                         z_1[1,2]                         z_1[1,3] 
#>                          0.19295                         -3.44074 
#>                         z_1[1,4]                         z_1[1,5] 
#>                          3.90997                          5.29122 
#>                         z_1[1,6]                         z_1[1,7] 
#>                          7.26798                         -2.49447 
#>                         z_1[1,8]                         z_1[1,9] 
#>                          3.20150                         -2.13547 
#>                        z_1[1,10]                        z_1[1,11] 
#>                         -3.13490                         -1.82488 
#>                        z_1[1,12]                        z_1[1,13] 
#>                          2.30047                         -1.78469 
#>                        z_1[1,14]                        z_1[1,15] 
#>                         -0.29988                          3.88001 
#>                        z_1[1,16]                        z_1[1,17] 
#>                         -3.28691                         -2.65156 
#>                        z_1[1,18]                        z_1[1,19] 
#>                         -1.55187                         -2.33990 
#>                        z_1[1,20]                        z_1[1,21] 
#>                          3.74930                          3.94971 
#>                        z_1[1,22]                        z_1[1,23] 
#>                          0.75840                          2.62373 
#>                        z_1[1,24]                        z_1[1,25] 
#>                          3.14306                          6.41276 
#>                        z_1[1,26]                        z_1[1,27] 
#>                          4.83589                          5.38955 
#>                        z_1[1,28]                        z_1[1,29] 
#>                          3.88941                         -1.87839 
#>                        z_1[1,30]                        z_1[1,31] 
#>                          2.69289                          1.99825 
#>                        z_1[1,32]                        z_1[1,33] 
#>                          6.31600                          5.36197 
#>                        z_1[1,34]                        z_1[1,35] 
#>                          2.44164                         -2.27807 
#>                        z_1[1,36]                        z_1[1,37] 
#>                         -0.62864                         -2.61372 
#>                        z_1[1,38]                        z_1[1,39] 
#>                          5.50224                          4.09869 
#>                        z_1[1,40]                        z_1[1,41] 
#>                          3.63843                          3.92030 
#>                        z_1[1,42]                        z_1[1,43] 
#>                          4.54465                          2.57441 
#>                        z_1[1,44]                        z_1[1,45] 
#>                          4.83986                         -2.42157 
#>                        z_1[1,46]                        z_1[1,47] 
#>                          5.84947                          4.98051 
#>                        z_1[1,48]                        z_1[1,49] 
#>                         -0.20457                          1.03576 
#>                        z_1[1,50]                        z_1[1,51] 
#>                         -0.24082                          2.00922 
#>                        z_1[1,52]                        z_1[1,53] 
#>                         -3.33487                         -0.67367 
#>                        z_1[1,54]                        z_1[1,55] 
#>                          9.52758                          0.34512 
#>                        z_1[1,56]                        z_1[1,57] 
#>                         -0.67135                          0.80572 
#>                        z_1[1,58]                        z_1[1,59] 
#>                          4.69380                          0.78473 
#>                        z_1[1,60]                        z_1[1,61] 
#>                         -4.13058                          2.88603 
#>                        z_1[1,62]                        z_1[1,63] 
#>                          4.46975                          3.79994 
#>                        z_1[1,64]                        z_1[1,65] 
#>                         -3.42287                         -1.96022 
#>                        z_1[1,66]                        z_1[1,67] 
#>                         -2.63962                          1.96729 
#>                        z_1[1,68]                        z_1[1,69] 
#>                         -4.66713                          3.09414 
#>                        z_1[1,70]                        z_1[1,71] 
#>                         -2.59709                          5.90209 
#>                        z_1[1,72]                        z_1[1,73] 
#>                         -2.92235                         -2.52277 
#>                        z_1[1,74]                        z_1[1,75] 
#>                          0.23165                          2.01532 
#>                        z_1[1,76]                        z_1[1,77] 
#>                          4.38227                          1.34967 
#>                        z_1[1,78]                        z_1[1,79] 
#>                         -1.78014                          2.75461 
#>                        z_1[1,80]                        z_1[1,81] 
#>                         10.15230                         -3.52112 
#>                        z_1[1,82]                        z_1[1,83] 
#>                         -1.69371                         -0.06622 
#>                        z_1[1,84]                        z_1[1,85] 
#>                         -0.84943                          2.63431 
#>                        z_1[1,86]                        z_1[1,87] 
#>                          3.64757                          6.21705 
#>                        z_1[1,88]                        z_1[1,89] 
#>                          4.14507                          5.31947 
#>                        z_1[1,90]                        z_1[1,91] 
#>                         -2.23136                          7.58994 
#>                        z_1[1,92]                        z_1[1,93] 
#>                         -2.36636                         -2.18725 
#>                        z_1[1,94]                        z_1[1,95] 
#>                          2.69595                         -0.97403 
#>                        z_1[1,96]                        z_1[1,97] 
#>                         -1.05842                          5.51356 
#>                        z_1[1,98]                        z_1[1,99] 
#>                          7.72840                          3.29455 
#>                       z_1[1,100] 
#>                         -3.24142 
#> 
#> 
#> [[3]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                         -0.33615                          2.61820 
#>                             b_x2                             b_x3 
#>                          1.63275                          1.18901 
#>            sd_regency__Intercept                            sigma 
#>                          2.57078                         -2.12549 
#>                        Intercept r_regency[regency_001,Intercept] 
#>                         -0.57831                         -2.46299 
#> r_regency[regency_002,Intercept] r_regency[regency_003,Intercept] 
#>                         -0.33961                         -1.85363 
#> r_regency[regency_004,Intercept] r_regency[regency_005,Intercept] 
#>                          4.34994                          6.97587 
#> r_regency[regency_006,Intercept] r_regency[regency_007,Intercept] 
#>                          2.79184                         -1.69408 
#> r_regency[regency_008,Intercept] r_regency[regency_009,Intercept] 
#>                          3.29772                         -1.12953 
#> r_regency[regency_010,Intercept] r_regency[regency_011,Intercept] 
#>                         -0.18530                         -0.70729 
#> r_regency[regency_012,Intercept] r_regency[regency_013,Intercept] 
#>                          1.38534                         -2.04923 
#> r_regency[regency_014,Intercept] r_regency[regency_015,Intercept] 
#>                         -1.33950                          2.94184 
#> r_regency[regency_016,Intercept] r_regency[regency_017,Intercept] 
#>                         -1.98671                         -2.37257 
#> r_regency[regency_018,Intercept] r_regency[regency_019,Intercept] 
#>                         -1.30640                         -1.84580 
#> r_regency[regency_020,Intercept] r_regency[regency_021,Intercept] 
#>                          5.00190                          1.83303 
#> r_regency[regency_022,Intercept] r_regency[regency_023,Intercept] 
#>                          5.96547                          2.78556 
#> r_regency[regency_024,Intercept] r_regency[regency_025,Intercept] 
#>                          2.99181                          2.18277 
#> r_regency[regency_026,Intercept] r_regency[regency_027,Intercept] 
#>                          1.30718                         -1.90331 
#> r_regency[regency_028,Intercept] r_regency[regency_029,Intercept] 
#>                          2.99975                         -2.08964 
#> r_regency[regency_030,Intercept] r_regency[regency_031,Intercept] 
#>                          3.32208                         -0.42922 
#> r_regency[regency_032,Intercept] r_regency[regency_033,Intercept] 
#>                          2.31243                          1.66747 
#> r_regency[regency_034,Intercept] r_regency[regency_035,Intercept] 
#>                          1.67823                         -1.65864 
#> r_regency[regency_036,Intercept] r_regency[regency_037,Intercept] 
#>                          0.88746                         -0.92504 
#> r_regency[regency_038,Intercept] r_regency[regency_039,Intercept] 
#>                          4.11681                          2.61317 
#> r_regency[regency_040,Intercept] r_regency[regency_041,Intercept] 
#>                          2.09196                          1.79843 
#> r_regency[regency_042,Intercept] r_regency[regency_043,Intercept] 
#>                          2.05046                          1.22541 
#> r_regency[regency_044,Intercept] r_regency[regency_045,Intercept] 
#>                          5.24039                         -1.45629 
#> r_regency[regency_046,Intercept] r_regency[regency_047,Intercept] 
#>                          1.77451                          2.60953 
#> r_regency[regency_048,Intercept] r_regency[regency_049,Intercept] 
#>                         -1.57636                          0.49798 
#> r_regency[regency_050,Intercept] r_regency[regency_051,Intercept] 
#>                         -0.33022                          5.55240 
#> r_regency[regency_052,Intercept] r_regency[regency_053,Intercept] 
#>                         -3.43673                         -1.76407 
#> r_regency[regency_054,Intercept] r_regency[regency_055,Intercept] 
#>                          1.44958                          2.03856 
#> r_regency[regency_056,Intercept] r_regency[regency_057,Intercept] 
#>                         -0.75024                          0.10786 
#> r_regency[regency_058,Intercept] r_regency[regency_059,Intercept] 
#>                          2.35615                          2.86265 
#> r_regency[regency_060,Intercept] r_regency[regency_061,Intercept] 
#>                         -3.34030                          1.81799 
#> r_regency[regency_062,Intercept] r_regency[regency_063,Intercept] 
#>                          1.76286                          0.83788 
#> r_regency[regency_064,Intercept] r_regency[regency_065,Intercept] 
#>                         -1.57042                         -2.59346 
#> r_regency[regency_066,Intercept] r_regency[regency_067,Intercept] 
#>                         -2.19599                          1.47350 
#> r_regency[regency_068,Intercept] r_regency[regency_069,Intercept] 
#>                         -2.44086                         -1.31605 
#> r_regency[regency_070,Intercept] r_regency[regency_071,Intercept] 
#>                         -3.15755                          3.20485 
#> r_regency[regency_072,Intercept] r_regency[regency_073,Intercept] 
#>                         -1.66828                         -2.91003 
#> r_regency[regency_074,Intercept] r_regency[regency_075,Intercept] 
#>                          2.40241                         -1.40687 
#> r_regency[regency_076,Intercept] r_regency[regency_077,Intercept] 
#>                          3.39491                         -3.47794 
#> r_regency[regency_078,Intercept] r_regency[regency_079,Intercept] 
#>                         -2.11954                          1.78426 
#> r_regency[regency_080,Intercept] r_regency[regency_081,Intercept] 
#>                          3.28672                         -2.13282 
#> r_regency[regency_082,Intercept] r_regency[regency_083,Intercept] 
#>                         -1.15450                         -1.15865 
#> r_regency[regency_084,Intercept] r_regency[regency_085,Intercept] 
#>                          0.65410                          1.31614 
#> r_regency[regency_086,Intercept] r_regency[regency_087,Intercept] 
#>                          2.62817                         -2.42291 
#> r_regency[regency_088,Intercept] r_regency[regency_089,Intercept] 
#>                          2.84586                          1.93853 
#> r_regency[regency_090,Intercept] r_regency[regency_091,Intercept] 
#>                          0.05776                          3.24772 
#> r_regency[regency_092,Intercept] r_regency[regency_093,Intercept] 
#>                         -2.02220                         -1.69903 
#> r_regency[regency_094,Intercept] r_regency[regency_095,Intercept] 
#>                          2.24011                          0.67441 
#> r_regency[regency_096,Intercept] r_regency[regency_097,Intercept] 
#>                         -2.39041                          2.09650 
#> r_regency[regency_098,Intercept] r_regency[regency_099,Intercept] 
#>                          0.60892                          6.41766 
#> r_regency[regency_100,Intercept]                           lprior 
#>                         -3.11250                         -0.48636 
#>                             lp__                         z_1[1,1] 
#>                          2.22015                         -2.57215 
#>                         z_1[1,2]                         z_1[1,3] 
#>                          0.15098                         -1.57463 
#>                         z_1[1,4]                         z_1[1,5] 
#>                          3.34179                          4.59979 
#>                         z_1[1,6]                         z_1[1,7] 
#>                          1.94838                         -2.10966 
#>                         z_1[1,8]                         z_1[1,9] 
#>                          2.71813                         -0.49367 
#>                        z_1[1,10]                        z_1[1,11] 
#>                          0.13045                         -0.57648 
#>                        z_1[1,12]                        z_1[1,13] 
#>                          1.31344                         -2.56096 
#>                        z_1[1,14]                        z_1[1,15] 
#>                         -0.79196                          2.70977 
#>                        z_1[1,16]                        z_1[1,17] 
#>                         -1.92924                         -2.37459 
#>                        z_1[1,18]                        z_1[1,19] 
#>                         -1.33476                         -2.31377 
#>                        z_1[1,20]                        z_1[1,21] 
#>                          4.70778                          1.42784 
#>                        z_1[1,22]                        z_1[1,23] 
#>                          5.37145                          2.67931 
#>                        z_1[1,24]                        z_1[1,25] 
#>                          3.32091                          2.33938 
#>                        z_1[1,26]                        z_1[1,27] 
#>                          0.68386                         -1.74830 
#>                        z_1[1,28]                        z_1[1,29] 
#>                          3.35196                         -1.48279 
#>                        z_1[1,30]                        z_1[1,31] 
#>                          3.01263                         -0.50463 
#>                        z_1[1,32]                        z_1[1,33] 
#>                          1.49737                          1.42724 
#>                        z_1[1,34]                        z_1[1,35] 
#>                          0.86990                         -1.91494 
#>                        z_1[1,36]                        z_1[1,37] 
#>                          1.28205                         -0.22859 
#>                        z_1[1,38]                        z_1[1,39] 
#>                          2.90272                          2.53025 
#>                        z_1[1,40]                        z_1[1,41] 
#>                          1.68602                          1.80361 
#>                        z_1[1,42]                        z_1[1,43] 
#>                          1.89609                          0.65115 
#>                        z_1[1,44]                        z_1[1,45] 
#>                          5.21451                         -1.28369 
#>                        z_1[1,46]                        z_1[1,47] 
#>                          2.18709                          2.39232 
#>                        z_1[1,48]                        z_1[1,49] 
#>                         -1.51789                          0.03439 
#>                        z_1[1,50]                        z_1[1,51] 
#>                         -0.04333                          5.05168 
#>                        z_1[1,52]                        z_1[1,53] 
#>                         -3.05517                         -1.54686 
#>                        z_1[1,54]                        z_1[1,55] 
#>                          1.17467                          2.48659 
#>                        z_1[1,56]                        z_1[1,57] 
#>                         -0.07149                         -0.02887 
#>                        z_1[1,58]                        z_1[1,59] 
#>                          2.23867                          2.21328 
#>                        z_1[1,60]                        z_1[1,61] 
#>                         -2.87877                          1.25077 
#>                        z_1[1,62]                        z_1[1,63] 
#>                          1.45702                          0.51686 
#>                        z_1[1,64]                        z_1[1,65] 
#>                         -0.83717                         -2.33342 
#>                        z_1[1,66]                        z_1[1,67] 
#>                         -1.46761                          0.89563 
#>                        z_1[1,68]                        z_1[1,69] 
#>                         -2.41142                         -0.79586 
#>                        z_1[1,70]                        z_1[1,71] 
#>                         -2.58466                          2.60683 
#>                        z_1[1,72]                        z_1[1,73] 
#>                         -1.26957                         -2.13064 
#>                        z_1[1,74]                        z_1[1,75] 
#>                          1.93720                         -1.33290 
#>                        z_1[1,76]                        z_1[1,77] 
#>                          2.80733                         -1.85324 
#>                        z_1[1,78]                        z_1[1,79] 
#>                         -1.77678                          1.76344 
#>                        z_1[1,80]                        z_1[1,81] 
#>                          2.52932                         -1.70323 
#>                        z_1[1,82]                        z_1[1,83] 
#>                         -0.83905                         -0.82037 
#>                        z_1[1,84]                        z_1[1,85] 
#>                          1.73662                          1.05846 
#>                        z_1[1,86]                        z_1[1,87] 
#>                          3.11650                         -2.41838 
#>                        z_1[1,88]                        z_1[1,89] 
#>                          2.34459                          2.82350 
#>                        z_1[1,90]                        z_1[1,91] 
#>                          0.75884                          3.46833 
#>                        z_1[1,92]                        z_1[1,93] 
#>                         -1.84745                         -1.37934 
#>                        z_1[1,94]                        z_1[1,95] 
#>                          2.83016                          0.56784 
#>                        z_1[1,96]                        z_1[1,97] 
#>                         -1.98180                          2.06597 
#>                        z_1[1,98]                        z_1[1,99] 
#>                          1.13812                          5.11925 
#>                       z_1[1,100] 
#>                         -2.55534 
#> 
#> 
#> [[4]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                         4.306798                         0.138522 
#>                             b_x2                             b_x3 
#>                         3.217878                         2.091704 
#>            sd_regency__Intercept                            sigma 
#>                         1.706977                        -1.169097 
#>                        Intercept r_regency[regency_001,Intercept] 
#>                         4.821207                        -2.644912 
#> r_regency[regency_002,Intercept] r_regency[regency_003,Intercept] 
#>                        -3.584002                        -1.455519 
#> r_regency[regency_004,Intercept] r_regency[regency_005,Intercept] 
#>                         0.153535                        -0.807236 
#> r_regency[regency_006,Intercept] r_regency[regency_007,Intercept] 
#>                        -0.671253                        -2.015644 
#> r_regency[regency_008,Intercept] r_regency[regency_009,Intercept] 
#>                        -1.611947                        -2.168900 
#> r_regency[regency_010,Intercept] r_regency[regency_011,Intercept] 
#>                        -3.845515                        -3.349155 
#> r_regency[regency_012,Intercept] r_regency[regency_013,Intercept] 
#>                        -0.598528                        -3.966197 
#> r_regency[regency_014,Intercept] r_regency[regency_015,Intercept] 
#>                        -1.672882                         0.216526 
#> r_regency[regency_016,Intercept] r_regency[regency_017,Intercept] 
#>                        -2.156602                        -1.444862 
#> r_regency[regency_018,Intercept] r_regency[regency_019,Intercept] 
#>                        -1.199866                        -0.998165 
#> r_regency[regency_020,Intercept] r_regency[regency_021,Intercept] 
#>                         1.913834                        -0.734336 
#> r_regency[regency_022,Intercept] r_regency[regency_023,Intercept] 
#>                        -0.771281                        -0.390361 
#> r_regency[regency_024,Intercept] r_regency[regency_025,Intercept] 
#>                        -0.550246                         0.270728 
#> r_regency[regency_026,Intercept] r_regency[regency_027,Intercept] 
#>                        -0.358525                        -2.610382 
#> r_regency[regency_028,Intercept] r_regency[regency_029,Intercept] 
#>                         0.406868                        -4.840734 
#> r_regency[regency_030,Intercept] r_regency[regency_031,Intercept] 
#>                        -0.979597                        -0.853206 
#> r_regency[regency_032,Intercept] r_regency[regency_033,Intercept] 
#>                        -2.902062                         0.240789 
#> r_regency[regency_034,Intercept] r_regency[regency_035,Intercept] 
#>                        -0.397651                        -2.277866 
#> r_regency[regency_036,Intercept] r_regency[regency_037,Intercept] 
#>                        -2.937674                        -1.476411 
#> r_regency[regency_038,Intercept] r_regency[regency_039,Intercept] 
#>                        -1.087061                         1.066855 
#> r_regency[regency_040,Intercept] r_regency[regency_041,Intercept] 
#>                         0.299457                        -0.321866 
#> r_regency[regency_042,Intercept] r_regency[regency_043,Intercept] 
#>                        -0.589560                        -2.213468 
#> r_regency[regency_044,Intercept] r_regency[regency_045,Intercept] 
#>                         1.681134                        -1.509631 
#> r_regency[regency_046,Intercept] r_regency[regency_047,Intercept] 
#>                         0.925785                        -0.632817 
#> r_regency[regency_048,Intercept] r_regency[regency_049,Intercept] 
#>                        -2.446031                        -2.232771 
#> r_regency[regency_050,Intercept] r_regency[regency_051,Intercept] 
#>                        -7.190352                         0.474496 
#> r_regency[regency_052,Intercept] r_regency[regency_053,Intercept] 
#>                        -3.287363                        -2.971393 
#> r_regency[regency_054,Intercept] r_regency[regency_055,Intercept] 
#>                        -0.223817                        -1.068024 
#> r_regency[regency_056,Intercept] r_regency[regency_057,Intercept] 
#>                        -1.703835                        -2.756484 
#> r_regency[regency_058,Intercept] r_regency[regency_059,Intercept] 
#>                         1.366060                        -2.107006 
#> r_regency[regency_060,Intercept] r_regency[regency_061,Intercept] 
#>                        -2.950260                        -2.068420 
#> r_regency[regency_062,Intercept] r_regency[regency_063,Intercept] 
#>                         0.008517                        -2.754926 
#> r_regency[regency_064,Intercept] r_regency[regency_065,Intercept] 
#>                        -2.891131                        -2.954195 
#> r_regency[regency_066,Intercept] r_regency[regency_067,Intercept] 
#>                        -2.561695                        -0.826576 
#> r_regency[regency_068,Intercept] r_regency[regency_069,Intercept] 
#>                        -2.247551                        -2.188642 
#> r_regency[regency_070,Intercept] r_regency[regency_071,Intercept] 
#>                        -2.864885                        -0.082241 
#> r_regency[regency_072,Intercept] r_regency[regency_073,Intercept] 
#>                        -2.836939                        -1.789548 
#> r_regency[regency_074,Intercept] r_regency[regency_075,Intercept] 
#>                        -0.639663                        -2.895855 
#> r_regency[regency_076,Intercept] r_regency[regency_077,Intercept] 
#>                         1.193387                        -4.021551 
#> r_regency[regency_078,Intercept] r_regency[regency_079,Intercept] 
#>                        -2.827248                         0.313596 
#> r_regency[regency_080,Intercept] r_regency[regency_081,Intercept] 
#>                        -1.961982                        -3.726726 
#> r_regency[regency_082,Intercept] r_regency[regency_083,Intercept] 
#>                        -1.527196                        -1.400072 
#> r_regency[regency_084,Intercept] r_regency[regency_085,Intercept] 
#>                        -3.429709                        -2.150009 
#> r_regency[regency_086,Intercept] r_regency[regency_087,Intercept] 
#>                         0.470954                        -2.224197 
#> r_regency[regency_088,Intercept] r_regency[regency_089,Intercept] 
#>                        -1.953428                         0.275214 
#> r_regency[regency_090,Intercept] r_regency[regency_091,Intercept] 
#>                        -3.368603                         1.368208 
#> r_regency[regency_092,Intercept] r_regency[regency_093,Intercept] 
#>                        -2.588248                        -6.612935 
#> r_regency[regency_094,Intercept] r_regency[regency_095,Intercept] 
#>                         0.374481                        -1.361233 
#> r_regency[regency_096,Intercept] r_regency[regency_097,Intercept] 
#>                        -2.805933                        -0.144346 
#> r_regency[regency_098,Intercept] r_regency[regency_099,Intercept] 
#>                        -3.280423                         1.071588 
#> r_regency[regency_100,Intercept]                           lprior 
#>                        -2.344496                        -1.531353 
#>                             lp__                         z_1[1,1] 
#>                         1.421865                        -3.344242 
#>                         z_1[1,2]                         z_1[1,3] 
#>                        -3.376196                        -1.438500 
#>                         z_1[1,4]                         z_1[1,5] 
#>                         0.014319                        -1.054411 
#>                         z_1[1,6]                         z_1[1,7] 
#>                        -0.892320                        -2.136647 
#>                         z_1[1,8]                         z_1[1,9] 
#>                        -2.031110                        -1.706400 
#>                        z_1[1,10]                        z_1[1,11] 
#>                        -4.171387                        -3.491678 
#>                        z_1[1,12]                        z_1[1,13] 
#>                        -1.201080                        -4.206893 
#>                        z_1[1,14]                        z_1[1,15] 
#>                        -1.994205                        -0.163600 
#>                        z_1[1,16]                        z_1[1,17] 
#>                        -3.612151                        -1.618987 
#>                        z_1[1,18]                        z_1[1,19] 
#>                        -1.478478                        -1.174784 
#>                        z_1[1,20]                        z_1[1,21] 
#>                         2.215308                        -1.359440 
#>                        z_1[1,22]                        z_1[1,23] 
#>                        -1.158873                        -0.955924 
#>                        z_1[1,24]                        z_1[1,25] 
#>                        -1.014166                        -0.126942 
#>                        z_1[1,26]                        z_1[1,27] 
#>                        -0.083022                        -2.283947 
#>                        z_1[1,28]                        z_1[1,29] 
#>                         0.258311                        -5.540468 
#>                        z_1[1,30]                        z_1[1,31] 
#>                        -1.072474                        -0.668203 
#>                        z_1[1,32]                        z_1[1,33] 
#>                        -3.187014                        -0.329338 
#>                        z_1[1,34]                        z_1[1,35] 
#>                        -1.187802                        -2.917143 
#>                        z_1[1,36]                        z_1[1,37] 
#>                        -2.326368                        -1.424360 
#>                        z_1[1,38]                        z_1[1,39] 
#>                        -1.306850                         1.167718 
#>                        z_1[1,40]                        z_1[1,41] 
#>                        -0.256971                        -0.701562 
#>                        z_1[1,42]                        z_1[1,43] 
#>                        -1.059850                        -2.241285 
#>                        z_1[1,44]                        z_1[1,45] 
#>                         2.072568                        -1.736575 
#>                        z_1[1,46]                        z_1[1,47] 
#>                         0.822205                        -1.024810 
#>                        z_1[1,48]                        z_1[1,49] 
#>                        -3.182766                        -2.597025 
#>                        z_1[1,50]                        z_1[1,51] 
#>                        -7.173679                         0.390322 
#>                        z_1[1,52]                        z_1[1,53] 
#>                        -3.745516                        -2.990428 
#>                        z_1[1,54]                        z_1[1,55] 
#>                        -0.284172                        -0.839273 
#>                        z_1[1,56]                        z_1[1,57] 
#>                        -1.723342                        -2.960291 
#>                        z_1[1,58]                        z_1[1,59] 
#>                         1.286633                        -1.668382 
#>                        z_1[1,60]                        z_1[1,61] 
#>                        -3.166122                        -2.188757 
#>                        z_1[1,62]                        z_1[1,63] 
#>                        -0.459177                        -3.252710 
#>                        z_1[1,64]                        z_1[1,65] 
#>                        -3.304451                        -4.120516 
#>                        z_1[1,66]                        z_1[1,67] 
#>                        -3.395075                        -1.403177 
#>                        z_1[1,68]                        z_1[1,69] 
#>                        -3.186673                        -2.766750 
#>                        z_1[1,70]                        z_1[1,71] 
#>                        -5.505045                        -0.579112 
#>                        z_1[1,72]                        z_1[1,73] 
#>                        -3.373123                        -2.061582 
#>                        z_1[1,74]                        z_1[1,75] 
#>                        -1.024341                        -2.988373 
#>                        z_1[1,76]                        z_1[1,77] 
#>                         1.440211                        -4.601670 
#>                        z_1[1,78]                        z_1[1,79] 
#>                        -7.355370                        -0.180179 
#>                        z_1[1,80]                        z_1[1,81] 
#>                        -2.029339                        -4.108763 
#>                        z_1[1,82]                        z_1[1,83] 
#>                        -2.675617                        -1.095311 
#>                        z_1[1,84]                        z_1[1,85] 
#>                        -2.762787                        -2.060242 
#>                        z_1[1,86]                        z_1[1,87] 
#>                         0.312556                        -1.927770 
#>                        z_1[1,88]                        z_1[1,89] 
#>                        -2.632345                         0.041515 
#>                        z_1[1,90]                        z_1[1,91] 
#>                        -3.821679                         1.472091 
#>                        z_1[1,92]                        z_1[1,93] 
#>                        -2.983358                        -6.157606 
#>                        z_1[1,94]                        z_1[1,95] 
#>                         0.163359                        -1.215103 
#>                        z_1[1,96]                        z_1[1,97] 
#>                        -2.892911                        -0.496275 
#>                        z_1[1,98]                        z_1[1,99] 
#>                        -2.250915                         1.082417 
#>                       z_1[1,100] 
#>                        -2.974943 
#> 
#> 
is_converged(model)
#> [1] FALSE
is_converged(model, threshold = 1.05)
#> [1] FALSE
# }
```
