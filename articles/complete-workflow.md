# Complete Workflow with hbsaems 1.1.0

``` r

library(hbsaems)
```

A typical Bayesian SAE workflow with `hbsaems` v1.1.0 follows seven
steps, each backed by a single primary function. This vignette walks
through the canonical pipeline.

## See also

For deeper coverage of topics beyond this introductory workflow, see the
**articles on the package website**:

<https://madsyair.github.io/hbsaems/articles/>

The articles cover:

- Distribution-specific walkthroughs (Beta logit-normal, binomial
  logit-normal, lognormal-lognormal)
- Spatial models (CAR / SAR / BYM2)
- Handling missing data (deletion, multiple imputation, joint Bayesian
  imputation)
- The interactive Shiny dashboard
- Advanced features (custom families, benchmarking, smooth terms)
- AST-based formula manipulation (internals)
- Migration guide for users coming from earlier versions

## 0. Inspect the data

``` r

data("data_fhnorm")
str(data_fhnorm, max.level = 1)
#> 'data.frame':    100 obs. of  9 variables:
#>  $ y         : num  6.98 9.01 6.22 10.47 10.62 ...
#>  $ D         : num  0.921 2.643 1.704 0.659 1.017 ...
#>  $ x1        : num  -0.9026 -0.7527 -0.2606 0.0759 0.2118 ...
#>  $ x2        : num  0.712 -0.878 -0.607 -1.077 -0.502 ...
#>  $ x3        : num  1.431 -0.318 -1.029 -1.071 -0.496 ...
#>  $ theta_true: num  8.5 9.73 8.54 9.68 11.86 ...
#>  $ u         : num  -0.8531 -0.0145 -1.2505 -0.5989 1.5845 ...
#>  $ regency   : chr  "regency_001" "regency_002" "regency_003" "regency_004" ...
#>  $ province  : chr  "province_01" "province_01" "province_01" "province_01" ...
#>  - attr(*, "datalabel")= chr "data_fhnorm"
#>  - attr(*, "var.labels")= chr [1:9] "" "" "" "" ...
head(data_fhnorm[, c("regency", "province", "y", "D", "x1", "x2", "x3")], 4)
#>       regency    province         y         D         x1         x2         x3
#> 1 regency_001 province_01  6.982249 0.9213655 -0.9026345  0.7116302  1.4305563
#> 2 regency_002 province_01  9.005801 2.6432183 -0.7526745 -0.8779797 -0.3182451
#> 3 regency_003 province_01  6.216509 1.7035092 -0.2605613 -0.6066989 -1.0293040
#> 4 regency_004 province_01 10.469687 0.6586857  0.0759456 -1.0766939 -1.0711613
```

`data_fhnorm` is a simulated Fay-Herriot dataset: 100 regencies nested
within 5 provinces, with a known sampling variance `D` per area and
three covariates.

## 1. Prior predictive check

``` r

# This is the production call.  Replace `chains`, `iter` with your
# usual values; the lighter settings below are a quick demonstration.
model_prior <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  data              = data_fhnorm,
  re                = ~ (1 | regency),     # area-level random effect (Fay-Herriot)
  sampling_variance = "D",                 # KNOWN sampling variance D_i
  sample_prior      = "only",
  prior             = c(
    brms::prior(normal(0, 1),  class = "b"),
    brms::prior(normal(10, 5), class = "Intercept"),
    brms::prior(normal(0, 2),  class = "sd")
  ),
  chains = 2, iter = 1000
)
pc <- prior_check(model_prior,
                  data         = data_fhnorm,
                  response_var = "y")
plot(pc)
```

The `sampling_variance = "D"` argument is the **defining feature** of a
Fay-Herriot model: the sampling variance is treated as **known** from
the design (it is not estimated), so brms pins the observation-level to
. Omitting this argument makes the model unidentified because the
residual and the area random-effect would compete to explain the same
variance, typically producing divergent transitions and very wide
credible intervals.

## 2. Fit the model

``` r

model <- hbm(
  formula           = brms::bf(y ~ x1 + x2 + x3),
  data              = data_fhnorm,
  re                = ~ (1 | regency),
  sampling_variance = "D",
  control           = list(adapt_delta = 0.99),
  chains            = 4, iter = 4000, warmup = 2000, cores = 4
)
summary(model)
```

Two settings deserve attention:

- **`sampling_variance = "D"`** – the column `D` in `data_fhnorm` holds
  the known sampling variance for each area; `hbsaems` translates this
  into an offset on the observation-level standard deviation so brms /
  Stan never tries to estimate it.
- **`adapt_delta = 0.99`** – Fay-Herriot likelihoods can produce mild
  funnel geometry between and the area random effects when many areas
  have small . As of v1.1.0 `hbsaems` already raises the default from
  brms’s `0.8` to `0.95`; pushing it further to `0.99` buys an even more
  conservative leapfrog step and eliminates the occasional divergent
  transition without re-parameterising the model.

### A small fitted model to demonstrate the rest of the workflow

For the remainder of the vignette we use a tiny model (very few
iterations) so that the diagnostic and prediction chunks below have a
real object to operate on. In your own analysis, **use the full
production settings shown above**, not the toy settings here.

``` r

# Mini fit -- iter = 200, chains = 1 -- for vignette demonstration only.
# Do NOT use these settings for inference: the chains have not
# converged at this length and the posterior will be biased.
fit_demo <- suppressWarnings(
  hbm(
    formula           = brms::bf(y ~ x1 + x2 + x3),
    data              = data_fhnorm,
    re                = ~ (1 | regency),
    sampling_variance = "D",
    chains  = 1,
    iter    = 200,
    warmup  = 100,
    refresh = 0,
    seed    = 1
  )
)
#> Running /opt/R/4.6.0/lib/R/bin/R CMD SHLIB foo.c
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG   -I"/home/runner/work/_temp/Library/Rcpp/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/unsupported"  -I"/home/runner/work/_temp/Library/BH/include" -I"/home/runner/work/_temp/Library/StanHeaders/include/src/"  -I"/home/runner/work/_temp/Library/StanHeaders/include/"  -I"/home/runner/work/_temp/Library/RcppParallel/include/"  -I"/home/runner/work/_temp/Library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include    -fpic  -g -O2  -c foo.c -o foo.o
#> In file included from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Core:19,
#>                  from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Dense:1,
#>                  from /home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22,
#>                  from <command-line>:
#> /home/runner/work/_temp/Library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: cmath: No such file or directory
#>   679 | #include <cmath>
#>       |          ^~~~~~~
#> compilation terminated.
#> make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:190: foo.o] Error 1
```

## 3. Convergence diagnostics

``` r

# Operate on the mini fit_demo above (NOT a substitute for production
# diagnostics on full chains).
diag <- convergence_check(fit_demo)
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> No divergences to plot.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
is_converged(fit_demo)
#> [1] FALSE
summary(diag)
#> 
#> ===== Convergence Diagnostics Summary =====
#> 
#> Divergent transitions: 0 (0.00%)
#> E-BFMI (min over chains): 0.869
#> Max-treedepth hit rate: 0.00%
#> 
#> R-hat:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.9900  0.9962  1.0055  1.0131  1.0214  1.1271 
#> 
#> Bulk ESS:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   45.82  148.45  200.00  172.38  200.00  200.00 
#> 
#> Tail ESS:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   17.48   54.39   71.29   70.78   87.24  116.78 
#> 
#> Geweke test (Z-scores):
#> [[1]]
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>                      b_Intercept                             b_x1 
#>                          0.02756                          1.63151 
#>                             b_x2                             b_x3 
#>                          0.72706                         -1.09421 
#>            sd_regency__Intercept                        Intercept 
#>                          0.31567                         -0.01188 
#> r_regency[regency_001,Intercept] r_regency[regency_002,Intercept] 
#>                         -0.41362                          0.24352 
#> r_regency[regency_003,Intercept] r_regency[regency_004,Intercept] 
#>                          1.05332                          0.03582 
#> r_regency[regency_005,Intercept] r_regency[regency_006,Intercept] 
#>                          0.21438                          0.49467 
#> r_regency[regency_007,Intercept] r_regency[regency_008,Intercept] 
#>                          0.13687                          0.63635 
#> r_regency[regency_009,Intercept] r_regency[regency_010,Intercept] 
#>                         -0.54708                          0.16036 
#> r_regency[regency_011,Intercept] r_regency[regency_012,Intercept] 
#>                          0.85271                          1.09389 
#> r_regency[regency_013,Intercept] r_regency[regency_014,Intercept] 
#>                          0.55080                          0.95422 
#> r_regency[regency_015,Intercept] r_regency[regency_016,Intercept] 
#>                          1.15718                         -2.25064 
#> r_regency[regency_017,Intercept] r_regency[regency_018,Intercept] 
#>                         -0.19592                         -1.10670 
#> r_regency[regency_019,Intercept] r_regency[regency_020,Intercept] 
#>                         -1.04465                         -0.32793 
#> r_regency[regency_021,Intercept] r_regency[regency_022,Intercept] 
#>                         -0.43215                         -0.44070 
#> r_regency[regency_023,Intercept] r_regency[regency_024,Intercept] 
#>                          3.28200                         -0.57884 
#> r_regency[regency_025,Intercept] r_regency[regency_026,Intercept] 
#>                         -0.99784                         -1.24640 
#> r_regency[regency_027,Intercept] r_regency[regency_028,Intercept] 
#>                         -1.15818                          0.59797 
#> r_regency[regency_029,Intercept] r_regency[regency_030,Intercept] 
#>                          1.49991                         -0.31399 
#> r_regency[regency_031,Intercept] r_regency[regency_032,Intercept] 
#>                          0.36533                          0.43370 
#> r_regency[regency_033,Intercept] r_regency[regency_034,Intercept] 
#>                         -0.43881                         -2.29111 
#> r_regency[regency_035,Intercept] r_regency[regency_036,Intercept] 
#>                          0.03546                          0.02348 
#> r_regency[regency_037,Intercept] r_regency[regency_038,Intercept] 
#>                         -0.82952                         -1.01048 
#> r_regency[regency_039,Intercept] r_regency[regency_040,Intercept] 
#>                          0.43409                         -1.14000 
#> r_regency[regency_041,Intercept] r_regency[regency_042,Intercept] 
#>                          0.30326                          0.09880 
#> r_regency[regency_043,Intercept] r_regency[regency_044,Intercept] 
#>                          0.51865                         -1.36104 
#> r_regency[regency_045,Intercept] r_regency[regency_046,Intercept] 
#>                          0.96704                          1.19243 
#> r_regency[regency_047,Intercept] r_regency[regency_048,Intercept] 
#>                          0.35681                          0.42429 
#> r_regency[regency_049,Intercept] r_regency[regency_050,Intercept] 
#>                          0.83179                         -1.30952 
#> r_regency[regency_051,Intercept] r_regency[regency_052,Intercept] 
#>                          0.87114                          0.04033 
#> r_regency[regency_053,Intercept] r_regency[regency_054,Intercept] 
#>                         -0.22456                         -1.73389 
#> r_regency[regency_055,Intercept] r_regency[regency_056,Intercept] 
#>                         -1.48915                          0.19948 
#> r_regency[regency_057,Intercept] r_regency[regency_058,Intercept] 
#>                         -0.42487                         -1.28317 
#> r_regency[regency_059,Intercept] r_regency[regency_060,Intercept] 
#>                          0.76388                         -2.42164 
#> r_regency[regency_061,Intercept] r_regency[regency_062,Intercept] 
#>                          1.01162                          0.63274 
#> r_regency[regency_063,Intercept] r_regency[regency_064,Intercept] 
#>                         -0.31138                          1.37668 
#> r_regency[regency_065,Intercept] r_regency[regency_066,Intercept] 
#>                         -1.23688                         -0.52075 
#> r_regency[regency_067,Intercept] r_regency[regency_068,Intercept] 
#>                         -0.19520                         -0.38100 
#> r_regency[regency_069,Intercept] r_regency[regency_070,Intercept] 
#>                         -0.03043                         -0.21890 
#> r_regency[regency_071,Intercept] r_regency[regency_072,Intercept] 
#>                         -1.67580                         -0.64859 
#> r_regency[regency_073,Intercept] r_regency[regency_074,Intercept] 
#>                         -0.42374                         -1.86281 
#> r_regency[regency_075,Intercept] r_regency[regency_076,Intercept] 
#>                         -1.25123                         -0.48981 
#> r_regency[regency_077,Intercept] r_regency[regency_078,Intercept] 
#>                         -1.19049                          0.15146 
#> r_regency[regency_079,Intercept] r_regency[regency_080,Intercept] 
#>                         -1.00369                         -0.20462 
#> r_regency[regency_081,Intercept] r_regency[regency_082,Intercept] 
#>                         -1.48465                         -1.57461 
#> r_regency[regency_083,Intercept] r_regency[regency_084,Intercept] 
#>                          0.19960                         -0.02181 
#> r_regency[regency_085,Intercept] r_regency[regency_086,Intercept] 
#>                          1.11567                          0.31935 
#> r_regency[regency_087,Intercept] r_regency[regency_088,Intercept] 
#>                         -0.93300                          1.05328 
#> r_regency[regency_089,Intercept] r_regency[regency_090,Intercept] 
#>                          3.16628                         -0.51418 
#> r_regency[regency_091,Intercept] r_regency[regency_092,Intercept] 
#>                         -0.97232                          1.05436 
#> r_regency[regency_093,Intercept] r_regency[regency_094,Intercept] 
#>                         -0.85499                          0.29396 
#> r_regency[regency_095,Intercept] r_regency[regency_096,Intercept] 
#>                         -3.19545                         -1.06784 
#> r_regency[regency_097,Intercept] r_regency[regency_098,Intercept] 
#>                         -0.12460                          0.23501 
#> r_regency[regency_099,Intercept] r_regency[regency_100,Intercept] 
#>                          0.01669                         -1.61013 
#>                           lprior                             lp__ 
#>                         -0.29467                          1.08164 
#>                         z_1[1,1]                         z_1[1,2] 
#>                         -0.03564                          0.42334 
#>                         z_1[1,3]                         z_1[1,4] 
#>                          1.16970                          0.21751 
#>                         z_1[1,5]                         z_1[1,6] 
#>                         -0.01693                          0.02817 
#>                         z_1[1,7]                         z_1[1,8] 
#>                          0.27423                          1.01744 
#>                         z_1[1,9]                        z_1[1,10] 
#>                         -1.05816                          0.27880 
#>                        z_1[1,11]                        z_1[1,12] 
#>                          1.08221                          0.82147 
#>                        z_1[1,13]                        z_1[1,14] 
#>                          0.74766                          0.65085 
#>                        z_1[1,15]                        z_1[1,16] 
#>                          0.85494                         -2.53132 
#>                        z_1[1,17]                        z_1[1,18] 
#>                          0.54385                         -1.05665 
#>                        z_1[1,19]                        z_1[1,20] 
#>                         -1.72126                         -0.50613 
#>                        z_1[1,21]                        z_1[1,22] 
#>                         -0.49393                         -0.61423 
#>                        z_1[1,23]                        z_1[1,24] 
#>                          3.42986                         -0.46628 
#>                        z_1[1,25]                        z_1[1,26] 
#>                         -1.11761                         -1.55551 
#>                        z_1[1,27]                        z_1[1,28] 
#>                         -1.11386                          0.49351 
#>                        z_1[1,29]                        z_1[1,30] 
#>                          1.99491                         -0.28087 
#>                        z_1[1,31]                        z_1[1,32] 
#>                          0.32523                          0.27193 
#>                        z_1[1,33]                        z_1[1,34] 
#>                         -1.54518                         -2.68978 
#>                        z_1[1,35]                        z_1[1,36] 
#>                          0.08098                          0.23147 
#>                        z_1[1,37]                        z_1[1,38] 
#>                         -1.40340                         -1.05409 
#>                        z_1[1,39]                        z_1[1,40] 
#>                          0.49250                         -1.42937 
#>                        z_1[1,41]                        z_1[1,42] 
#>                          0.50953                         -0.08669 
#>                        z_1[1,43]                        z_1[1,44] 
#>                          0.68367                         -1.63697 
#>                        z_1[1,45]                        z_1[1,46] 
#>                          1.00586                          1.30309 
#>                        z_1[1,47]                        z_1[1,48] 
#>                          0.26454                          0.70431 
#>                        z_1[1,49]                        z_1[1,50] 
#>                          1.36033                         -1.04000 
#>                        z_1[1,51]                        z_1[1,52] 
#>                          1.31181                          0.49279 
#>                        z_1[1,53]                        z_1[1,54] 
#>                         -0.37696                         -1.89306 
#>                        z_1[1,55]                        z_1[1,56] 
#>                         -2.60658                          0.49066 
#>                        z_1[1,57]                        z_1[1,58] 
#>                         -0.24313                         -0.83687 
#>                        z_1[1,59]                        z_1[1,60] 
#>                          0.61164                         -2.56502 
#>                        z_1[1,61]                        z_1[1,62] 
#>                          0.79802                          0.79710 
#>                        z_1[1,63]                        z_1[1,64] 
#>                         -0.29258                          0.76544 
#>                        z_1[1,65]                        z_1[1,66] 
#>                         -1.30421                         -0.58106 
#>                        z_1[1,67]                        z_1[1,68] 
#>                         -0.47530                          0.21051 
#>                        z_1[1,69]                        z_1[1,70] 
#>                         -0.09328                         -0.04452 
#>                        z_1[1,71]                        z_1[1,72] 
#>                         -2.09584                         -0.93970 
#>                        z_1[1,73]                        z_1[1,74] 
#>                         -0.90808                         -1.63375 
#>                        z_1[1,75]                        z_1[1,76] 
#>                         -1.31966                         -0.41465 
#>                        z_1[1,77]                        z_1[1,78] 
#>                         -1.01289                         -0.06445 
#>                        z_1[1,79]                        z_1[1,80] 
#>                         -1.05459                         -0.95162 
#>                        z_1[1,81]                        z_1[1,82] 
#>                         -1.34035                         -2.10182 
#>                        z_1[1,83]                        z_1[1,84] 
#>                          0.19060                          1.01905 
#>                        z_1[1,85]                        z_1[1,86] 
#>                          1.45897                          0.32820 
#>                        z_1[1,87]                        z_1[1,88] 
#>                         -0.64752                          1.56242 
#>                        z_1[1,89]                        z_1[1,90] 
#>                          3.50642                         -0.24357 
#>                        z_1[1,91]                        z_1[1,92] 
#>                         -1.06497                          1.67863 
#>                        z_1[1,93]                        z_1[1,94] 
#>                         -0.58284                         -0.20301 
#>                        z_1[1,95]                        z_1[1,96] 
#>                         -3.76394                         -1.01190 
#>                        z_1[1,97]                        z_1[1,98] 
#>                          0.09469                          0.08573 
#>                        z_1[1,99]                       z_1[1,100] 
#>                         -0.07840                         -2.86144
hbm_warnings(fit_demo)
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> Warning: The ESS has been capped to avoid unstable estimates.
#> [1] "R-hat > 1.1: model may not have converged."
```

The full set of diagnostics on your production fit would also include
trace plots, R-hat / ESS tables, and pp-checks. These all follow the
same calling convention:

``` r

plot(diag, type = "trace")
plot(diag, type = "rhat")
plot(diag, type = "ess")
```

## 4. Goodness-of-fit

``` r

gof <- model_compare(model)    # single-model: LOO, WAIC, pp_check
summary(gof)
plot(gof, type = "pp_check")
```

## 5. Compare alternatives

``` r

model2 <- hbm(brms::bf(y ~ x1 + x2), data = data_fhnorm,
              re = ~ (1 | regency),
              sampling_variance = "D",
              control = list(adapt_delta = 0.99),
              chains = 4, iter = 4000)
model3 <- hbm(brms::bf(y ~ x1),      data = data_fhnorm,
              re = ~ (1 | regency),
              sampling_variance = "D",
              control = list(adapt_delta = 0.99),
              chains = 4, iter = 4000)

model_compare(model, model2)
model_compare_all(full = model, medium = model2, simple = model3)
```

## 6. Small-area estimates

The simplest call uses the data the model was fit on – no `newdata`
argument needed:

``` r

# In-sample prediction (default): operates on the data stored
# inside the fitted object.
est <- sae_predict(fit_demo)
summary(est)
#> 
#> ===== Small Area Estimation Summary =====
#> 
#> Areas       : 100 
#> Overall RSE : 7.3 %
#> 
#> Predictions:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   6.322   8.964   9.789   9.860  10.655  13.573 
#> 
#> RSE by area:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   4.128   6.222   7.149   7.296   8.038  13.904
head(est$result_table, 5)
#>   Prediction        SD RSE_percent
#> 1   8.117171 0.6834583    8.419907
#> 2   9.221638 0.9479032   10.279120
#> 3   8.333632 0.9276971   11.131967
#> 4  10.361134 0.6228779    6.011677
#> 5  10.532557 0.6383649    6.060873
```

To predict at fresh data points, pass `newdata`. When you do, any
internal offset columns the original fit needed
(e.g. `.hbsaems_sigma_fixed = sqrt(D)` for the Fay-Herriot sugar) are
repopulated automatically when the new data frame has the same number of
rows as the training data – the standard “predict at the same areas with
updated covariates” use case.

``` r

# Out-of-sample prediction at new areas:
est_new <- sae_predict(fit_demo, newdata = data_new)
```

### Bayesian model averaging

For model averaging, fit several candidate models, then either let
[`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md)
weight them by stacking weights (LOO-based) or supply your own weights:

``` r

# Stacking weights (default, derived from LOO)
avg_stacked <- model_average(model, model2, model3)

# User-specified weights -- must sum to 1
avg_manual  <- model_average(model, model2, weights = c(0.7, 0.3))

# The returned object is an `hbsae_results`, identical in shape to
# what sae_predict() returns, so all post-processing helpers
# (sae_transform, sae_scale, sae_filter, sae_aggregate) work on it.
plot(avg_stacked, type = "predictions")
```

### Visualisations

``` r

# Visualisations (require a graphics device):
plot(est, type = "predictions")
plot(est, type = "uncertainty")
```

## 7. Post-processing predictions

All `sae_*` post-processing helpers operate on the `hbsae_results`
object returned by either
[`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md)
or
[`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md):

``` r

log_est  <- sae_transform(est, log)
sc_est   <- sae_scale(est)
hi_est   <- sae_filter(est, est$pred > stats::median(est$pred))
agg_est  <- sae_aggregate(sae_predict(model),
                          sae_predict(model2),
                          method = "mean")
```

## Session info

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] hbsaems_1.1.0
#> 
#> loaded via a namespace (and not attached):
#>   [1] tidyselect_1.2.1      dplyr_1.2.1           farver_2.1.2         
#>   [4] loo_2.9.0             S7_0.2.2              fastmap_1.2.0        
#>   [7] tensorA_0.36.2.1      rpart_4.1.27          digest_0.6.39        
#>  [10] lifecycle_1.0.5       StanHeaders_2.32.10   survival_3.8-6       
#>  [13] processx_3.9.0        magrittr_2.0.5        posterior_1.7.0      
#>  [16] compiler_4.6.0        rlang_1.2.0           sass_0.4.10          
#>  [19] tools_4.6.0           yaml_2.3.12           knitr_1.51           
#>  [22] bridgesampling_1.2-1  htmlwidgets_1.6.4     pkgbuild_1.4.8       
#>  [25] plyr_1.8.9            RColorBrewer_1.1-3    abind_1.4-8          
#>  [28] withr_3.0.2           purrr_1.2.2           desc_1.4.3           
#>  [31] nnet_7.3-20           grid_4.6.0            stats4_4.6.0         
#>  [34] jomo_2.7-6            mice_3.19.0           inline_0.3.21        
#>  [37] ggplot2_4.0.3         scales_1.4.0          iterators_1.0.14     
#>  [40] MASS_7.3-65           cli_3.6.6             mvtnorm_1.4-0        
#>  [43] rmarkdown_2.31        reformulas_0.4.4      ragg_1.5.2           
#>  [46] generics_0.1.4        otel_0.2.0            RcppParallel_5.1.11-2
#>  [49] reshape2_1.4.5        minqa_1.2.8           cachem_1.1.0         
#>  [52] rstan_2.32.7          stringr_1.6.0         splines_4.6.0        
#>  [55] bayesplot_1.15.0      parallel_4.6.0        matrixStats_1.5.0    
#>  [58] brms_2.23.0           vctrs_0.7.3           boot_1.3-32          
#>  [61] glmnet_5.0            Matrix_1.7-5          jsonlite_2.0.0       
#>  [64] callr_3.7.6           mitml_0.4-5           systemfonts_1.3.2    
#>  [67] foreach_1.5.2         jquerylib_0.1.4       tidyr_1.3.2          
#>  [70] glue_1.8.1            pan_1.9               nloptr_2.2.1         
#>  [73] pkgdown_2.2.0         codetools_0.2-20      ps_1.9.3             
#>  [76] distributional_0.7.0  stringi_1.8.7         gtable_0.3.6         
#>  [79] shape_1.4.6.1         QuickJSR_1.10.0       lme4_2.0-1           
#>  [82] tibble_3.3.1          pillar_1.11.1         htmltools_0.5.9      
#>  [85] Brobdingnag_1.2-9     R6_2.6.1              Rdpack_2.6.6         
#>  [88] textshaping_1.0.5     evaluate_1.0.5        lattice_0.22-9       
#>  [91] rbibutils_2.4.1       backports_1.5.1       broom_1.0.13         
#>  [94] bslib_0.11.0          rstantools_2.6.0      Rcpp_1.1.1-1.1       
#>  [97] coda_0.19-4.1         gridExtra_2.3         nlme_3.1-169         
#> [100] checkmate_2.3.4       xfun_0.57             fs_2.1.0             
#> [103] pkgconfig_2.0.3
```
