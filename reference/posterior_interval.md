# Compute Credible Intervals for an hbmfit Object

The
[`posterior_interval`](https://mc-stan.org/rstantools/reference/posterior_interval.html)
generic is re-exported from rstantools and an S3 method is provided that
dispatches on `hbmfit` objects. This lets users call
`posterior_interval(fit)` on the return value of
[`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) just as
they would on a `brmsfit`.

## Usage

``` r
# S3 method for class 'hbmfit'
posterior_interval(object, prob = 0.95, params = NULL, ...)
```

## Arguments

- object:

  An `hbmfit` object.

- prob:

  Coverage probability in \\(0, 1)\\ (default `0.95`; note that
  [`rstantools::posterior_interval`](https://mc-stan.org/rstantools/reference/posterior_interval.html)'s
  own default is `0.9`).

- params:

  Optional character vector of parameter names to keep.

- ...:

  Additional arguments forwarded to
  [`posterior_draws`](https://madsyair.github.io/hbsaems/reference/posterior_draws.md).

## Value

A matrix with two rows giving lower and upper bounds.

## See also

[`posterior_interval`](https://mc-stan.org/rstantools/reference/posterior_interval.html)

## Examples

``` r
# \donttest{
library(hbsaems)
library(brms)
data("data_fhnorm")
model <- hbm(brms::bf(y ~ x1), data = data_fhnorm,
             re = ~ (1 | regency),    # area-level random effect
             chains = 2, iter = 1000, warmup = 500,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 40 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.1, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
posterior_interval(model, prob = 0.90)
#>      variable
#>       b_Intercept      b_x1 sd_regency__Intercept    sigma Intercept
#>   5%     9.791176 0.7578318              0.117114 0.614227  9.635747
#>   95%   10.312155 1.2602805              1.492446 1.558393 10.139947
#>      variable
#>       r_regency[regency_001,Intercept] r_regency[regency_002,Intercept]
#>   5%                        -2.4804407                        -1.052669
#>   95%                        0.3114698                         0.845649
#>      variable
#>       r_regency[regency_003,Intercept] r_regency[regency_004,Intercept]
#>   5%                        -3.4315324                       -0.8109872
#>   95%                        0.1285436                        1.2118573
#>      variable
#>       r_regency[regency_005,Intercept] r_regency[regency_006,Intercept]
#>   5%                        -0.8275264                       -0.5496158
#>   95%                        1.1549429                        1.7918442
#>      variable
#>       r_regency[regency_007,Intercept] r_regency[regency_008,Intercept]
#>   5%                        -2.0745219                       -0.5519149
#>   95%                        0.2996904                        1.7544519
#>      variable
#>       r_regency[regency_009,Intercept] r_regency[regency_010,Intercept]
#>   5%                        -2.1523740                       -1.3644704
#>   95%                        0.4456685                        0.7858374
#>      variable
#>       r_regency[regency_011,Intercept] r_regency[regency_012,Intercept]
#>   5%                        -1.5301551                       -0.2713639
#>   95%                        0.5565693                        2.3201926
#>      variable
#>       r_regency[regency_013,Intercept] r_regency[regency_014,Intercept]
#>   5%                        -1.9401367                       -1.2435055
#>   95%                        0.4459114                        0.8495309
#>      variable
#>       r_regency[regency_015,Intercept] r_regency[regency_016,Intercept]
#>   5%                        -0.3445869                       -2.5082561
#>   95%                        2.2742529                        0.2788507
#>      variable
#>       r_regency[regency_017,Intercept] r_regency[regency_018,Intercept]
#>   5%                       -3.97221297                       -1.6837947
#>   95%                       0.05775981                        0.4403211
#>      variable
#>       r_regency[regency_019,Intercept] r_regency[regency_020,Intercept]
#>   5%                       -3.50850779                       -0.5178427
#>   95%                       0.09525293                        1.5709732
#>      variable
#>       r_regency[regency_021,Intercept] r_regency[regency_022,Intercept]
#>   5%                        -0.4300366                        -1.069474
#>   95%                        1.7528667                         1.204050
#>      variable
#>       r_regency[regency_023,Intercept] r_regency[regency_024,Intercept]
#>   5%                        -0.4511115                       -0.7241055
#>   95%                        1.6585717                        1.3251641
#>      variable
#>       r_regency[regency_025,Intercept] r_regency[regency_026,Intercept]
#>   5%                        -0.4106253                        -1.065478
#>   95%                        2.0038133                         1.224599
#>      variable
#>       r_regency[regency_027,Intercept] r_regency[regency_028,Intercept]
#>   5%                        -1.1231776                       -0.3574651
#>   95%                        0.9479077                        2.0848391
#>      variable
#>       r_regency[regency_029,Intercept] r_regency[regency_030,Intercept]
#>   5%                        -1.8993571                       -0.9466928
#>   95%                        0.5407782                        1.1251083
#>      variable
#>       r_regency[regency_031,Intercept] r_regency[regency_032,Intercept]
#>   5%                        -1.1210185                       -0.6791875
#>   95%                        0.8447592                        1.2737250
#>      variable
#>       r_regency[regency_033,Intercept] r_regency[regency_034,Intercept]
#>   5%                        -0.2008354                       -0.5307639
#>   95%                        2.7706796                        1.8958907
#>      variable
#>       r_regency[regency_035,Intercept] r_regency[regency_036,Intercept]
#>   5%                        -1.6379596                        -1.433251
#>   95%                        0.5314154                         0.705924
#>      variable
#>       r_regency[regency_037,Intercept] r_regency[regency_038,Intercept]
#>   5%                        -1.5997754                       -0.4570581
#>   95%                        0.6455377                        2.1524453
#>      variable
#>       r_regency[regency_039,Intercept] r_regency[regency_040,Intercept]
#>   5%                         -0.634865                       -0.3368931
#>   95%                         1.390759                        2.5410015
#>      variable
#>       r_regency[regency_041,Intercept] r_regency[regency_042,Intercept]
#>   5%                        -0.4826793                       -0.4581581
#>   95%                        1.6751515                        1.7888614
#>      variable
#>       r_regency[regency_043,Intercept] r_regency[regency_044,Intercept]
#>   5%                        -0.5924223                       -0.5785545
#>   95%                        1.9502767                        1.6225576
#>      variable
#>       r_regency[regency_045,Intercept] r_regency[regency_046,Intercept]
#>   5%                        -1.8268728                        -0.186519
#>   95%                        0.4473272                         2.995036
#>      variable
#>       r_regency[regency_047,Intercept] r_regency[regency_048,Intercept]
#>   5%                        -0.2890656                        -1.405101
#>   95%                        2.2260769                         0.703645
#>      variable
#>       r_regency[regency_049,Intercept] r_regency[regency_050,Intercept]
#>   5%                        -1.1642030                       -1.5487814
#>   95%                        0.7361391                        0.6796153
#>      variable
#>       r_regency[regency_051,Intercept] r_regency[regency_052,Intercept]
#>   5%                        -0.6662641                       -2.2778679
#>   95%                        1.2274964                        0.2681512
#>      variable
#>       r_regency[regency_053,Intercept] r_regency[regency_054,Intercept]
#>   5%                        -1.6415839                       -0.5102876
#>   95%                        0.5006712                        1.6494236
#>      variable
#>       r_regency[regency_055,Intercept] r_regency[regency_056,Intercept]
#>   5%                        -1.2151776                       -1.3962375
#>   95%                        0.8035161                        0.8645878
#>      variable
#>       r_regency[regency_057,Intercept] r_regency[regency_058,Intercept]
#>   5%                        -1.1937452                        -1.122144
#>   95%                        0.7099562                         1.146311
#>      variable
#>       r_regency[regency_059,Intercept] r_regency[regency_060,Intercept]
#>   5%                        -0.8546174                       -1.8254981
#>   95%                        1.1199018                        0.3632692
#>      variable
#>       r_regency[regency_061,Intercept] r_regency[regency_062,Intercept]
#>   5%                        -0.5006326                       -0.2396636
#>   95%                        1.6702791                        2.6064240
#>      variable
#>       r_regency[regency_063,Intercept] r_regency[regency_064,Intercept]
#>   5%                        -0.9563583                       -1.1993685
#>   95%                        1.3034885                        0.7904705
#>      variable
#>       r_regency[regency_065,Intercept] r_regency[regency_066,Intercept]
#>   5%                        -1.5886175                       -2.0963201
#>   95%                        0.5564596                        0.3400306
#>      variable
#>       r_regency[regency_067,Intercept] r_regency[regency_068,Intercept]
#>   5%                        -0.9065453                       -2.2482694
#>   95%                        1.0013818                        0.2728756
#>      variable
#>       r_regency[regency_069,Intercept] r_regency[regency_070,Intercept]
#>   5%                        -0.9966293                       -2.4842074
#>   95%                        1.0112389                        0.2726005
#>      variable
#>       r_regency[regency_071,Intercept] r_regency[regency_072,Intercept]
#>   5%                        -0.4063718                       -1.6952303
#>   95%                        1.7970591                        0.4984861
#>      variable
#>       r_regency[regency_073,Intercept] r_regency[regency_074,Intercept]
#>   5%                        -2.9036396                       -1.1861900
#>   95%                        0.1956183                        0.9471926
#>      variable
#>       r_regency[regency_075,Intercept] r_regency[regency_076,Intercept]
#>   5%                        -1.0664554                       -0.3556162
#>   95%                        0.9670795                        1.8526060
#>      variable
#>       r_regency[regency_077,Intercept] r_regency[regency_078,Intercept]
#>   5%                        -1.0487073                       -1.7073354
#>   95%                        0.9027422                        0.5059311
#>      variable
#>       r_regency[regency_079,Intercept] r_regency[regency_080,Intercept]
#>   5%                        -0.2826821                       -0.5920751
#>   95%                        2.1693350                        1.5774641
#>      variable
#>       r_regency[regency_081,Intercept] r_regency[regency_082,Intercept]
#>   5%                        -1.2129089                       -1.5878305
#>   95%                        0.8504595                        0.5838797
#>      variable
#>       r_regency[regency_083,Intercept] r_regency[regency_084,Intercept]
#>   5%                        -1.8639155                       -1.0620899
#>   95%                        0.3963295                        0.8275986
#>      variable
#>       r_regency[regency_085,Intercept] r_regency[regency_086,Intercept]
#>   5%                        -0.7155993                       -0.3118314
#>   95%                        1.2875845                        2.3455620
#>      variable
#>       r_regency[regency_087,Intercept] r_regency[regency_088,Intercept]
#>   5%                        -0.8186283                       -0.5134922
#>   95%                        1.4162188                        1.6754832
#>      variable
#>       r_regency[regency_089,Intercept] r_regency[regency_090,Intercept]
#>   5%                        -0.4585831                       -0.8222232
#>   95%                        1.8952935                        1.2771256
#>      variable
#>       r_regency[regency_091,Intercept] r_regency[regency_092,Intercept]
#>   5%                        -0.3383708                        -1.047881
#>   95%                        2.0784572                         1.051217
#>      variable
#>       r_regency[regency_093,Intercept] r_regency[regency_094,Intercept]
#>   5%                        -1.6213230                       -0.3569255
#>   95%                        0.5395307                        2.0131544
#>      variable
#>       r_regency[regency_095,Intercept] r_regency[regency_096,Intercept]
#>   5%                        -1.5624269                       -2.0864082
#>   95%                        0.5259262                        0.3776069
#>      variable
#>       r_regency[regency_097,Intercept] r_regency[regency_098,Intercept]
#>   5%                        -0.4827982                       -0.9341127
#>   95%                        1.6567630                        1.2343243
#>      variable
#>       r_regency[regency_099,Intercept] r_regency[regency_100,Intercept]
#>   5%                        -0.6478455                       -1.3341406
#>   95%                        1.5491703                        0.6084329
#>      variable
#>          lprior      lp__   z_1[1,1]  z_1[1,2]   z_1[1,3]  z_1[1,4]  z_1[1,5]
#>   5%  -4.669507 -333.5539 -2.1030284 -1.361241 -2.7201700 -1.344532 -1.113245
#>   95% -4.559042 -232.0233  0.7218117  1.258699  0.5665471  1.309346  1.296688
#>      variable
#>        z_1[1,6]   z_1[1,7]  z_1[1,8]  z_1[1,9] z_1[1,10]  z_1[1,11]  z_1[1,12]
#>   5%  -1.007728 -1.9531245 -1.006271 -1.973884 -1.397076 -1.5154941 -0.7952597
#>   95%  1.733931  0.7986381  1.608622  1.124674  1.089185  0.9689494  2.1513068
#>      variable
#>        z_1[1,13] z_1[1,14]  z_1[1,15] z_1[1,16]  z_1[1,17]  z_1[1,18]
#>   5%  -1.6939091 -1.451670 -0.8573337 -2.187791 -3.1496260 -1.6466358
#>   95%  0.9479207  1.148194  1.9370959  0.641112  0.3507781  0.9795855
#>      variable
#>        z_1[1,19] z_1[1,20] z_1[1,21] z_1[1,22]  z_1[1,23] z_1[1,24]  z_1[1,25]
#>   5%  -2.8069663 -1.052368 -0.911190 -1.259772 -0.9306953 -1.109451 -0.9637722
#>   95%  0.4689011  1.580900  1.740705  1.324709  1.7802289  1.464109  2.0128053
#>      variable
#>       z_1[1,26] z_1[1,27]  z_1[1,28] z_1[1,29] z_1[1,30] z_1[1,31] z_1[1,32]
#>   5%  -1.136668 -1.312775 -0.9048455 -1.847535 -1.319238 -1.437770 -1.073182
#>   95%  1.389412  1.261149  1.9068033  1.000905  1.390690  1.317074  1.357020
#>      variable
#>        z_1[1,33] z_1[1,34] z_1[1,35] z_1[1,36] z_1[1,37] z_1[1,38] z_1[1,39]
#>   5%  -0.5764584 -1.236623 -1.720334 -1.456377 -1.632076 -1.003659 -1.040419
#>   95%  2.4173636  1.678197  1.019726  1.138784  1.242941  1.801596  1.479043
#>      variable
#>       z_1[1,40] z_1[1,41]  z_1[1,42] z_1[1,43] z_1[1,44] z_1[1,45]  z_1[1,46]
#>   5%  -0.829730 -1.003871 -0.7898897 -1.014792 -0.988598 -1.777243 -0.6303144
#>   95%  2.043297  1.670507  1.7211002  1.643814  1.684950  0.905018  2.4238465
#>      variable
#>        z_1[1,47] z_1[1,48] z_1[1,49] z_1[1,50] z_1[1,51]  z_1[1,52]  z_1[1,53]
#>   5%  -0.7864359 -1.563410 -1.356515 -1.438200 -1.049208 -1.9399853 -1.6303196
#>   95%  2.0874556  1.091941  1.136300  1.081303  1.444480  0.7715843  0.9563147
#>      variable
#>       z_1[1,54] z_1[1,55] z_1[1,56] z_1[1,57] z_1[1,58] z_1[1,59]  z_1[1,60]
#>   5%  -1.053207 -1.331914 -1.388067 -1.438124 -1.308656 -1.111216 -1.7277789
#>   95%  1.594360  1.194044  1.062213  1.002794  1.359507  1.360753  0.7850869
#>      variable
#>        z_1[1,61]  z_1[1,62] z_1[1,63] z_1[1,64]  z_1[1,65]  z_1[1,66] z_1[1,67]
#>   5%  -0.9849661 -0.5948665 -1.280734 -1.361901 -1.6890461 -1.8941534 -1.199708
#>   95%  1.7313121  2.1633470  1.379218  1.193758  0.9873994  0.8275436  1.378899
#>      variable
#>        z_1[1,68] z_1[1,69]  z_1[1,70]  z_1[1,71]  z_1[1,72]  z_1[1,73]
#>   5%  -2.0688682 -1.236118 -2.1714826 -0.7555802 -1.6265910 -2.3912731
#>   95%  0.7618106  1.196763  0.7707903  1.7368166  0.9787573  0.7472268
#>      variable
#>       z_1[1,74] z_1[1,75]  z_1[1,76] z_1[1,77]  z_1[1,78]  z_1[1,79] z_1[1,80]
#>   5%  -1.414375 -1.237275 -0.8432432 -1.249071 -1.6277991 -0.7474098 -1.019544
#>   95%  1.104000  1.379937  1.8699117  1.205554  0.8576398  1.9521648  1.510895
#>      variable
#>       z_1[1,81] z_1[1,82]  z_1[1,83] z_1[1,84] z_1[1,85]  z_1[1,86] z_1[1,87]
#>   5%  -1.431989 -1.716145 -1.7661236 -1.350559 -1.116645 -0.8304605 -1.192772
#>   95%  1.227851  1.173225  0.9569526  1.218151  1.496819  2.1408602  1.580819
#>      variable
#>       z_1[1,88]  z_1[1,89] z_1[1,90]  z_1[1,91] z_1[1,92] z_1[1,93]  z_1[1,94]
#>   5%  -1.072718 -0.8861467 -1.165287 -0.7890753 -1.200791 -1.522167 -0.7780493
#>   95%  1.566868  1.6618806  1.351922  1.9787588  1.279442  1.094531  1.7904289
#>      variable
#>       z_1[1,95]  z_1[1,96] z_1[1,97] z_1[1,98]  z_1[1,99] z_1[1,100]
#>   5%  -1.560859 -1.9079649 -1.022418 -1.206322 -0.9889584  -1.587501
#>   95%  1.020145  0.7885501  1.602525  1.460978  1.6037149   1.052142
# }
```
