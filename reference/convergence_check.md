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
model <- hbm(brms::bf(y ~ x1 + x2 + x3),
             data   = data_fhnorm,
             re     = ~ (1 | regency),    # area-level random effect
             chains = 2, iter = 2000, warmup = 1000,
             cores  = 1, seed = 123, refresh = 0)
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 50 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.68, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

diag <- convergence_check(model)
summary(diag)
#> 
#> ===== Convergence Diagnostics Summary =====
#> 
#> R-hat:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.012   1.118   1.179   1.188   1.249   1.682 
#> 
#> Bulk ESS:
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    3.251    9.256   25.208  279.384  321.038 1955.007 
#> 
#> Geweke test (Z-scores):
#> [[1]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                         0.825026                         2.237414 
#>                             b_x2                             b_x3 
#>                        -0.957252                        -0.033677 
#>            sd_regency__Intercept                            sigma 
#>                         0.309196                        -0.300126 
#>                        Intercept r_regency[regency_001,Intercept] 
#>                         0.496797                        -0.137524 
#> r_regency[regency_002,Intercept] r_regency[regency_003,Intercept] 
#>                        -1.368352                        -0.320668 
#> r_regency[regency_004,Intercept] r_regency[regency_005,Intercept] 
#>                        -1.123754                         1.188504 
#> r_regency[regency_006,Intercept] r_regency[regency_007,Intercept] 
#>                        -0.013475                        -0.390970 
#> r_regency[regency_008,Intercept] r_regency[regency_009,Intercept] 
#>                         0.855678                         0.453482 
#> r_regency[regency_010,Intercept] r_regency[regency_011,Intercept] 
#>                         0.583239                        -0.699164 
#> r_regency[regency_012,Intercept] r_regency[regency_013,Intercept] 
#>                         0.292829                        -0.240468 
#> r_regency[regency_014,Intercept] r_regency[regency_015,Intercept] 
#>                        -0.395443                         0.167977 
#> r_regency[regency_016,Intercept] r_regency[regency_017,Intercept] 
#>                        -0.236898                        -0.233585 
#> r_regency[regency_018,Intercept] r_regency[regency_019,Intercept] 
#>                        -0.346648                        -0.421454 
#> r_regency[regency_020,Intercept] r_regency[regency_021,Intercept] 
#>                         0.348021                         0.308242 
#> r_regency[regency_022,Intercept] r_regency[regency_023,Intercept] 
#>                        -1.415045                        -0.458239 
#> r_regency[regency_024,Intercept] r_regency[regency_025,Intercept] 
#>                         0.595674                        -0.077857 
#> r_regency[regency_026,Intercept] r_regency[regency_027,Intercept] 
#>                        -1.135765                        -0.268576 
#> r_regency[regency_028,Intercept] r_regency[regency_029,Intercept] 
#>                         0.236424                        -1.719036 
#> r_regency[regency_030,Intercept] r_regency[regency_031,Intercept] 
#>                        -0.575862                        -0.511576 
#> r_regency[regency_032,Intercept] r_regency[regency_033,Intercept] 
#>                        -1.936252                         0.023848 
#> r_regency[regency_034,Intercept] r_regency[regency_035,Intercept] 
#>                         0.393084                         0.406496 
#> r_regency[regency_036,Intercept] r_regency[regency_037,Intercept] 
#>                        -0.630431                         0.162426 
#> r_regency[regency_038,Intercept] r_regency[regency_039,Intercept] 
#>                         1.416618                         0.170280 
#> r_regency[regency_040,Intercept] r_regency[regency_041,Intercept] 
#>                         0.149803                         0.207201 
#> r_regency[regency_042,Intercept] r_regency[regency_043,Intercept] 
#>                         0.308668                         0.457061 
#> r_regency[regency_044,Intercept] r_regency[regency_045,Intercept] 
#>                        -0.174986                        -0.362960 
#> r_regency[regency_046,Intercept] r_regency[regency_047,Intercept] 
#>                         0.254487                         0.644039 
#> r_regency[regency_048,Intercept] r_regency[regency_049,Intercept] 
#>                        -0.541797                         0.010648 
#> r_regency[regency_050,Intercept] r_regency[regency_051,Intercept] 
#>                        -1.332480                         0.774569 
#> r_regency[regency_052,Intercept] r_regency[regency_053,Intercept] 
#>                        -0.200680                        -0.599829 
#> r_regency[regency_054,Intercept] r_regency[regency_055,Intercept] 
#>                        -0.496194                        -0.407020 
#> r_regency[regency_056,Intercept] r_regency[regency_057,Intercept] 
#>                         0.263655                         0.931913 
#> r_regency[regency_058,Intercept] r_regency[regency_059,Intercept] 
#>                        -0.626526                        -0.816375 
#> r_regency[regency_060,Intercept] r_regency[regency_061,Intercept] 
#>                        -0.526806                         0.345035 
#> r_regency[regency_062,Intercept] r_regency[regency_063,Intercept] 
#>                         0.228379                         0.486185 
#> r_regency[regency_064,Intercept] r_regency[regency_065,Intercept] 
#>                         0.349311                         0.575785 
#> r_regency[regency_066,Intercept] r_regency[regency_067,Intercept] 
#>                        -0.895744                         1.020576 
#> r_regency[regency_068,Intercept] r_regency[regency_069,Intercept] 
#>                         0.203060                        -0.416607 
#> r_regency[regency_070,Intercept] r_regency[regency_071,Intercept] 
#>                         0.002354                         0.534007 
#> r_regency[regency_072,Intercept] r_regency[regency_073,Intercept] 
#>                        -0.162584                        -0.502184 
#> r_regency[regency_074,Intercept] r_regency[regency_075,Intercept] 
#>                        -0.689608                        -1.252908 
#> r_regency[regency_076,Intercept] r_regency[regency_077,Intercept] 
#>                        -0.310791                         0.664086 
#> r_regency[regency_078,Intercept] r_regency[regency_079,Intercept] 
#>                        -0.656862                         0.234568 
#> r_regency[regency_080,Intercept] r_regency[regency_081,Intercept] 
#>                        -2.283114                        -0.916806 
#> r_regency[regency_082,Intercept] r_regency[regency_083,Intercept] 
#>                        -0.908293                        -1.716355 
#> r_regency[regency_084,Intercept] r_regency[regency_085,Intercept] 
#>                         0.969812                         0.420735 
#> r_regency[regency_086,Intercept] r_regency[regency_087,Intercept] 
#>                         0.615707                        -0.733634 
#> r_regency[regency_088,Intercept] r_regency[regency_089,Intercept] 
#>                         1.102253                         0.843281 
#> r_regency[regency_090,Intercept] r_regency[regency_091,Intercept] 
#>                        -1.259438                         0.127428 
#> r_regency[regency_092,Intercept] r_regency[regency_093,Intercept] 
#>                        -0.515409                        -0.385404 
#> r_regency[regency_094,Intercept] r_regency[regency_095,Intercept] 
#>                         0.341521                         0.120573 
#> r_regency[regency_096,Intercept] r_regency[regency_097,Intercept] 
#>                        -0.371734                         0.168801 
#> r_regency[regency_098,Intercept] r_regency[regency_099,Intercept] 
#>                        -1.327304                         0.390941 
#> r_regency[regency_100,Intercept]                           lprior 
#>                        -0.162486                         0.854437 
#>                             lp__                         z_1[1,1] 
#>                         0.079598                         0.123190 
#>                         z_1[1,2]                         z_1[1,3] 
#>                        -1.608334                        -0.718924 
#>                         z_1[1,4]                         z_1[1,5] 
#>                        -1.570294                         0.443978 
#>                         z_1[1,6]                         z_1[1,7] 
#>                        -0.144639                        -0.559694 
#>                         z_1[1,8]                         z_1[1,9] 
#>                         0.431956                         1.261381 
#>                        z_1[1,10]                        z_1[1,11] 
#>                         1.045263                        -0.945977 
#>                        z_1[1,12]                        z_1[1,13] 
#>                         0.315159                         0.309805 
#>                        z_1[1,14]                        z_1[1,15] 
#>                         0.099564                         0.734669 
#>                        z_1[1,16]                        z_1[1,17] 
#>                        -0.678188                        -0.756783 
#>                        z_1[1,18]                        z_1[1,19] 
#>                        -0.312449                        -0.667775 
#>                        z_1[1,20]                        z_1[1,21] 
#>                         1.282833                        -0.089686 
#>                        z_1[1,22]                        z_1[1,23] 
#>                        -1.029979                        -0.297776 
#>                        z_1[1,24]                        z_1[1,25] 
#>                        -0.212936                         0.377608 
#>                        z_1[1,26]                        z_1[1,27] 
#>                        -1.183847                        -0.006695 
#>                        z_1[1,28]                        z_1[1,29] 
#>                         0.520791                        -2.636982 
#>                        z_1[1,30]                        z_1[1,31] 
#>                        -1.169323                         0.107566 
#>                        z_1[1,32]                        z_1[1,33] 
#>                        -1.438105                         0.044766 
#>                        z_1[1,34]                        z_1[1,35] 
#>                         0.645369                         0.402397 
#>                        z_1[1,36]                        z_1[1,37] 
#>                        -0.014176                         0.423636 
#>                        z_1[1,38]                        z_1[1,39] 
#>                         0.155066                         0.112333 
#>                        z_1[1,40]                        z_1[1,41] 
#>                        -0.066260                         0.503694 
#>                        z_1[1,42]                        z_1[1,43] 
#>                         1.127974                         0.922584 
#>                        z_1[1,44]                        z_1[1,45] 
#>                        -1.174774                        -1.493085 
#>                        z_1[1,46]                        z_1[1,47] 
#>                         0.082574                         0.704715 
#>                        z_1[1,48]                        z_1[1,49] 
#>                        -1.159070                         0.466077 
#>                        z_1[1,50]                        z_1[1,51] 
#>                        -1.202044                         0.477604 
#>                        z_1[1,52]                        z_1[1,53] 
#>                        -1.132946                        -1.789651 
#>                        z_1[1,54]                        z_1[1,55] 
#>                        -0.583466                        -1.274268 
#>                        z_1[1,56]                        z_1[1,57] 
#>                         0.353812                         0.370133 
#>                        z_1[1,58]                        z_1[1,59] 
#>                        -0.150930                        -1.027197 
#>                        z_1[1,60]                        z_1[1,61] 
#>                        -0.773166                         0.077192 
#>                        z_1[1,62]                        z_1[1,63] 
#>                         0.825441                         1.204124 
#>                        z_1[1,64]                        z_1[1,65] 
#>                         0.483606                         1.343985 
#>                        z_1[1,66]                        z_1[1,67] 
#>                        -1.763566                         1.693185 
#>                        z_1[1,68]                        z_1[1,69] 
#>                         0.980742                        -0.088232 
#>                        z_1[1,70]                        z_1[1,71] 
#>                        -0.115788                         0.770690 
#>                        z_1[1,72]                        z_1[1,73] 
#>                        -0.139143                        -1.093017 
#>                        z_1[1,74]                        z_1[1,75] 
#>                        -0.356536                        -0.720085 
#>                        z_1[1,76]                        z_1[1,77] 
#>                        -0.757023                         0.362183 
#>                        z_1[1,78]                        z_1[1,79] 
#>                        -0.641318                         0.332860 
#>                        z_1[1,80]                        z_1[1,81] 
#>                        -0.638261                        -1.044423 
#>                        z_1[1,82]                        z_1[1,83] 
#>                        -0.747317                        -0.962651 
#>                        z_1[1,84]                        z_1[1,85] 
#>                         1.370371                        -0.099334 
#>                        z_1[1,86]                        z_1[1,87] 
#>                         0.939318                        -1.527647 
#>                        z_1[1,88]                        z_1[1,89] 
#>                         0.620220                         1.602933 
#>                        z_1[1,90]                        z_1[1,91] 
#>                        -1.373060                         0.022250 
#>                        z_1[1,92]                        z_1[1,93] 
#>                        -0.717740                        -1.162853 
#>                        z_1[1,94]                        z_1[1,95] 
#>                         0.050788                        -0.163333 
#>                        z_1[1,96]                        z_1[1,97] 
#>                         0.133506                         0.036382 
#>                        z_1[1,98]                        z_1[1,99] 
#>                        -1.867297                        -0.461148 
#>                       z_1[1,100] 
#>                         0.320806 
#> 
#> 
#> [[2]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                           3.1110                          -1.9068 
#>                             b_x2                             b_x3 
#>                          -0.4884                           7.3655 
#>            sd_regency__Intercept                            sigma 
#>                           3.0123                          -5.3304 
#>                        Intercept r_regency[regency_001,Intercept] 
#>                           3.2415                          -4.1730 
#> r_regency[regency_002,Intercept] r_regency[regency_003,Intercept] 
#>                         -10.0315                          -3.9192 
#> r_regency[regency_004,Intercept] r_regency[regency_005,Intercept] 
#>                           2.8101                           1.8269 
#> r_regency[regency_006,Intercept] r_regency[regency_007,Intercept] 
#>                           2.5011                          -3.0074 
#> r_regency[regency_008,Intercept] r_regency[regency_009,Intercept] 
#>                          -1.3157                          -4.5322 
#> r_regency[regency_010,Intercept] r_regency[regency_011,Intercept] 
#>                          -6.9097                          -3.4087 
#> r_regency[regency_012,Intercept] r_regency[regency_013,Intercept] 
#>                           3.1660                          -4.1955 
#> r_regency[regency_014,Intercept] r_regency[regency_015,Intercept] 
#>                          -6.9355                           2.6663 
#> r_regency[regency_016,Intercept] r_regency[regency_017,Intercept] 
#>                          -3.8819                          -4.1717 
#> r_regency[regency_018,Intercept] r_regency[regency_019,Intercept] 
#>                          -4.2430                          -4.4553 
#> r_regency[regency_020,Intercept] r_regency[regency_021,Intercept] 
#>                           5.0869                           3.0649 
#> r_regency[regency_022,Intercept] r_regency[regency_023,Intercept] 
#>                           2.4345                           3.5243 
#> r_regency[regency_024,Intercept] r_regency[regency_025,Intercept] 
#>                           2.6638                           3.4095 
#> r_regency[regency_026,Intercept] r_regency[regency_027,Intercept] 
#>                           3.2463                          -5.9762 
#> r_regency[regency_028,Intercept] r_regency[regency_029,Intercept] 
#>                           2.6830                          -6.0541 
#> r_regency[regency_030,Intercept] r_regency[regency_031,Intercept] 
#>                          -2.1238                           1.6166 
#> r_regency[regency_032,Intercept] r_regency[regency_033,Intercept] 
#>                          -2.4736                           2.8280 
#> r_regency[regency_034,Intercept] r_regency[regency_035,Intercept] 
#>                           5.7815                          -6.2310 
#> r_regency[regency_036,Intercept] r_regency[regency_037,Intercept] 
#>                          -4.1544                          -5.9371 
#> r_regency[regency_038,Intercept] r_regency[regency_039,Intercept] 
#>                          -2.2422                           5.2267 
#> r_regency[regency_040,Intercept] r_regency[regency_041,Intercept] 
#>                           3.1097                           4.1602 
#> r_regency[regency_042,Intercept] r_regency[regency_043,Intercept] 
#>                           2.6006                          -0.8047 
#> r_regency[regency_044,Intercept] r_regency[regency_045,Intercept] 
#>                           3.4940                          -4.7402 
#> r_regency[regency_046,Intercept] r_regency[regency_047,Intercept] 
#>                           3.4720                           2.0639 
#> r_regency[regency_048,Intercept] r_regency[regency_049,Intercept] 
#>                          -5.5519                           4.9004 
#> r_regency[regency_050,Intercept] r_regency[regency_051,Intercept] 
#>                          -3.0934                           4.8797 
#> r_regency[regency_052,Intercept] r_regency[regency_053,Intercept] 
#>                          -4.5894                          -4.3186 
#> r_regency[regency_054,Intercept] r_regency[regency_055,Intercept] 
#>                           2.7543                          -2.2715 
#> r_regency[regency_056,Intercept] r_regency[regency_057,Intercept] 
#>                          -7.7398                           6.3199 
#> r_regency[regency_058,Intercept] r_regency[regency_059,Intercept] 
#>                           4.6233                           1.4266 
#> r_regency[regency_060,Intercept] r_regency[regency_061,Intercept] 
#>                          -5.1891                          -0.6776 
#> r_regency[regency_062,Intercept] r_regency[regency_063,Intercept] 
#>                           3.0190                           2.9840 
#> r_regency[regency_064,Intercept] r_regency[regency_065,Intercept] 
#>                          -9.0931                          -6.5267 
#> r_regency[regency_066,Intercept] r_regency[regency_067,Intercept] 
#>                          -7.0954                           6.0486 
#> r_regency[regency_068,Intercept] r_regency[regency_069,Intercept] 
#>                          -4.3979                          -7.8398 
#> r_regency[regency_070,Intercept] r_regency[regency_071,Intercept] 
#>                          -4.6249                           2.5227 
#> r_regency[regency_072,Intercept] r_regency[regency_073,Intercept] 
#>                          -5.4789                          -3.9372 
#> r_regency[regency_074,Intercept] r_regency[regency_075,Intercept] 
#>                           8.2499                          -7.4234 
#> r_regency[regency_076,Intercept] r_regency[regency_077,Intercept] 
#>                           4.0242                          -7.3933 
#> r_regency[regency_078,Intercept] r_regency[regency_079,Intercept] 
#>                          -5.4406                           3.6955 
#> r_regency[regency_080,Intercept] r_regency[regency_081,Intercept] 
#>                          -0.6687                          -7.4404 
#> r_regency[regency_082,Intercept] r_regency[regency_083,Intercept] 
#>                          -6.5099                          -0.5569 
#> r_regency[regency_084,Intercept] r_regency[regency_085,Intercept] 
#>                          -7.5179                          -1.0257 
#> r_regency[regency_086,Intercept] r_regency[regency_087,Intercept] 
#>                           2.7243                          -1.0871 
#> r_regency[regency_088,Intercept] r_regency[regency_089,Intercept] 
#>                          -4.8544                           3.9610 
#> r_regency[regency_090,Intercept] r_regency[regency_091,Intercept] 
#>                         -11.3821                           2.5636 
#> r_regency[regency_092,Intercept] r_regency[regency_093,Intercept] 
#>                          -9.1515                          -8.3144 
#> r_regency[regency_094,Intercept] r_regency[regency_095,Intercept] 
#>                           4.4240                           3.6737 
#> r_regency[regency_096,Intercept] r_regency[regency_097,Intercept] 
#>                          -3.0039                           3.7477 
#> r_regency[regency_098,Intercept] r_regency[regency_099,Intercept] 
#>                          -2.8850                           5.3151 
#> r_regency[regency_100,Intercept]                           lprior 
#>                          -6.6042                           2.8900 
#>                             lp__                         z_1[1,1] 
#>                          10.4845                          -5.1790 
#>                         z_1[1,2]                         z_1[1,3] 
#>                          -6.5861                          -4.0433 
#>                         z_1[1,4]                         z_1[1,5] 
#>                           2.3952                           1.2164 
#>                         z_1[1,6]                         z_1[1,7] 
#>                           2.1387                          -4.1253 
#>                         z_1[1,8]                         z_1[1,9] 
#>                          -2.9261                          -3.8724 
#>                        z_1[1,10]                        z_1[1,11] 
#>                          -5.9402                          -2.8653 
#>                        z_1[1,12]                        z_1[1,13] 
#>                           3.3811                          -5.6060 
#>                        z_1[1,14]                        z_1[1,15] 
#>                          -7.5631                           3.7110 
#>                        z_1[1,16]                        z_1[1,17] 
#>                          -4.4361                          -4.5593 
#>                        z_1[1,18]                        z_1[1,19] 
#>                          -4.9049                          -3.8931 
#>                        z_1[1,20]                        z_1[1,21] 
#>                           7.5644                           3.8226 
#>                        z_1[1,22]                        z_1[1,23] 
#>                           2.0375                           2.4897 
#>                        z_1[1,24]                        z_1[1,25] 
#>                           1.8892                           3.6261 
#>                        z_1[1,26]                        z_1[1,27] 
#>                           2.8538                          -4.7470 
#>                        z_1[1,28]                        z_1[1,29] 
#>                           4.2779                          -5.6582 
#>                        z_1[1,30]                        z_1[1,31] 
#>                          -1.9994                           1.7178 
#>                        z_1[1,32]                        z_1[1,33] 
#>                          -2.5828                           3.2051 
#>                        z_1[1,34]                        z_1[1,35] 
#>                           5.3340                          -9.1848 
#>                        z_1[1,36]                        z_1[1,37] 
#>                          -1.9088                          -7.2243 
#>                        z_1[1,38]                        z_1[1,39] 
#>                          -2.6835                           5.3812 
#>                        z_1[1,40]                        z_1[1,41] 
#>                           3.9592                           3.2998 
#>                        z_1[1,42]                        z_1[1,43] 
#>                           2.8346                          -1.5922 
#>                        z_1[1,44]                        z_1[1,45] 
#>                           3.6561                          -6.7588 
#>                        z_1[1,46]                        z_1[1,47] 
#>                           3.1899                           1.6821 
#>                        z_1[1,48]                        z_1[1,49] 
#>                          -5.6495                           3.3548 
#>                        z_1[1,50]                        z_1[1,51] 
#>                          -2.4303                           5.1138 
#>                        z_1[1,52]                        z_1[1,53] 
#>                          -4.8171                          -9.0188 
#>                        z_1[1,54]                        z_1[1,55] 
#>                           2.3216                          -1.3639 
#>                        z_1[1,56]                        z_1[1,57] 
#>                          -7.9598                           4.7231 
#>                        z_1[1,58]                        z_1[1,59] 
#>                           3.7743                          -0.2808 
#>                        z_1[1,60]                        z_1[1,61] 
#>                          -7.1309                          -1.3295 
#>                        z_1[1,62]                        z_1[1,63] 
#>                           3.5428                           2.5354 
#>                        z_1[1,64]                        z_1[1,65] 
#>                          -9.1117                          -8.2333 
#>                        z_1[1,66]                        z_1[1,67] 
#>                          -4.8284                           7.8461 
#>                        z_1[1,68]                        z_1[1,69] 
#>                          -5.4619                          -7.4209 
#>                        z_1[1,70]                        z_1[1,71] 
#>                          -5.0463                           2.7190 
#>                        z_1[1,72]                        z_1[1,73] 
#>                          -8.2251                          -5.3693 
#>                        z_1[1,74]                        z_1[1,75] 
#>                           7.2369                          -6.4961 
#>                        z_1[1,76]                        z_1[1,77] 
#>                           4.8472                          -5.7978 
#>                        z_1[1,78]                        z_1[1,79] 
#>                          -5.1877                           4.8711 
#>                        z_1[1,80]                        z_1[1,81] 
#>                          -1.0491                         -10.5312 
#>                        z_1[1,82]                        z_1[1,83] 
#>                          -7.4778                           0.4322 
#>                        z_1[1,84]                        z_1[1,85] 
#>                          -5.3643                          -1.2229 
#>                        z_1[1,86]                        z_1[1,87] 
#>                           2.7682                          -0.7905 
#>                        z_1[1,88]                        z_1[1,89] 
#>                          -5.1930                           3.7406 
#>                        z_1[1,90]                        z_1[1,91] 
#>                          -8.1779                           2.4084 
#>                        z_1[1,92]                        z_1[1,93] 
#>                         -12.1434                          -5.6625 
#>                        z_1[1,94]                        z_1[1,95] 
#>                           5.0671                           3.8541 
#>                        z_1[1,96]                        z_1[1,97] 
#>                          -2.2516                           3.5768 
#>                        z_1[1,98]                        z_1[1,99] 
#>                          -3.2066                           5.9951 
#>                       z_1[1,100] 
#>                          -8.3779 
#> 
#> 
is_converged(model)
#> [1] FALSE
is_converged(model, threshold = 1.05)
#> [1] FALSE
# }
```
