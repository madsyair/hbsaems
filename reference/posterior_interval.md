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
             chains = 4, iter = 2000, warmup = 1000,
             cores = 1, seed = 1, refresh = 0)
#> Compiling Stan program...
#> Start sampling
#> Warning: There were 32 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.44, indicating chains have not mixed.
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
#>       b_Intercept     b_x1 sd_regency__Intercept     sigma Intercept
#>   5%     9.733409 0.770820             0.1041003 0.1329507  9.571327
#>   95%   10.286432 1.263571             1.5804342 1.5662764 10.121574
#>      variable
#>       r_regency[regency_001,Intercept] r_regency[regency_002,Intercept]
#>   5%                         -2.401233                       -1.0889555
#>   95%                         0.279015                        0.8852009
#>      variable
#>       r_regency[regency_003,Intercept] r_regency[regency_004,Intercept]
#>   5%                        -3.5925534                       -0.7504843
#>   95%                        0.1463933                        1.0817428
#>      variable
#>       r_regency[regency_005,Intercept] r_regency[regency_006,Intercept]
#>   5%                        -0.7566513                       -0.4432796
#>   95%                        1.0819304                        1.7852899
#>      variable
#>       r_regency[regency_007,Intercept] r_regency[regency_008,Intercept]
#>   5%                        -2.2848932                       -0.4917826
#>   95%                        0.2597766                        1.8394768
#>      variable
#>       r_regency[regency_009,Intercept] r_regency[regency_010,Intercept]
#>   5%                        -2.0305868                       -1.0997710
#>   95%                        0.4200014                        0.7849404
#>      variable
#>       r_regency[regency_011,Intercept] r_regency[regency_012,Intercept]
#>   5%                         -1.420643                       -0.2517772
#>   95%                         0.578699                        2.5852451
#>      variable
#>       r_regency[regency_013,Intercept] r_regency[regency_014,Intercept]
#>   5%                        -1.8021251                       -1.0713848
#>   95%                        0.4462022                        0.7867288
#>      variable
#>       r_regency[regency_015,Intercept] r_regency[regency_016,Intercept]
#>   5%                        -0.3413133                       -2.4714429
#>   95%                        2.2837507                        0.2382759
#>      variable
#>       r_regency[regency_017,Intercept] r_regency[regency_018,Intercept]
#>   5%                       -4.49857898                       -1.6605686
#>   95%                       0.05659349                        0.4424408
#>      variable
#>       r_regency[regency_019,Intercept] r_regency[regency_020,Intercept]
#>   5%                        -3.7690378                       -0.4905167
#>   95%                        0.1077582                        1.6026975
#>      variable
#>       r_regency[regency_021,Intercept] r_regency[regency_022,Intercept]
#>   5%                        -0.4128254                       -0.8593918
#>   95%                        1.7348068                        1.0087542
#>      variable
#>       r_regency[regency_023,Intercept] r_regency[regency_024,Intercept]
#>   5%                        -0.4504253                       -0.6261126
#>   95%                        1.8138817                        1.4070394
#>      variable
#>       r_regency[regency_025,Intercept] r_regency[regency_026,Intercept]
#>   5%                        -0.3334575                       -0.9137556
#>   95%                        2.1020700                        0.9860068
#>      variable
#>       r_regency[regency_027,Intercept] r_regency[regency_028,Intercept]
#>   5%                        -0.9410380                       -0.3155173
#>   95%                        0.9421062                        2.2405751
#>      variable
#>       r_regency[regency_029,Intercept] r_regency[regency_030,Intercept]
#>   5%                        -1.6907546                       -0.8729571
#>   95%                        0.4383858                        1.0256455
#>      variable
#>       r_regency[regency_031,Intercept] r_regency[regency_032,Intercept]
#>   5%                        -1.0920458                        -0.691739
#>   95%                        0.7475649                         1.268374
#>      variable
#>       r_regency[regency_033,Intercept] r_regency[regency_034,Intercept]
#>   5%                        -0.2040619                       -0.5476931
#>   95%                        3.0067076                        1.5782589
#>      variable
#>       r_regency[regency_035,Intercept] r_regency[regency_036,Intercept]
#>   5%                        -1.6207446                       -1.1982334
#>   95%                        0.4432017                        0.7524107
#>      variable
#>       r_regency[regency_037,Intercept] r_regency[regency_038,Intercept]
#>   5%                        -1.5144146                        -0.459241
#>   95%                        0.5146388                         1.972734
#>      variable
#>       r_regency[regency_039,Intercept] r_regency[regency_040,Intercept]
#>   5%                        -0.5362565                       -0.2885942
#>   95%                        1.4244176                        2.4560123
#>      variable
#>       r_regency[regency_041,Intercept] r_regency[regency_042,Intercept]
#>   5%                        -0.4594529                       -0.4547891
#>   95%                        1.7652192                        1.8601064
#>      variable
#>       r_regency[regency_043,Intercept] r_regency[regency_044,Intercept]
#>   5%                        -0.4950369                       -0.5274896
#>   95%                        1.6308136                        1.6114450
#>      variable
#>       r_regency[regency_045,Intercept] r_regency[regency_046,Intercept]
#>   5%                         -1.789179                       -0.1732887
#>   95%                         0.405637                        3.2352001
#>      variable
#>       r_regency[regency_047,Intercept] r_regency[regency_048,Intercept]
#>   5%                        -0.2695222                        -1.303255
#>   95%                        2.5144580                         0.668779
#>      variable
#>       r_regency[regency_049,Intercept] r_regency[regency_050,Intercept]
#>   5%                        -1.0916343                        -1.276056
#>   95%                        0.6717805                         0.774288
#>      variable
#>       r_regency[regency_051,Intercept] r_regency[regency_052,Intercept]
#>   5%                        -0.6522476                        -2.169812
#>   95%                        1.3349703                         0.321461
#>      variable
#>       r_regency[regency_053,Intercept] r_regency[regency_054,Intercept]
#>   5%                        -1.5885510                       -0.5712904
#>   95%                        0.4865044                        1.7486542
#>      variable
#>       r_regency[regency_055,Intercept] r_regency[regency_056,Intercept]
#>   5%                        -1.0853989                       -1.2014513
#>   95%                        0.7571634                        0.6802977
#>      variable
#>       r_regency[regency_057,Intercept] r_regency[regency_058,Intercept]
#>   5%                        -1.2717548                       -0.9135611
#>   95%                        0.6804797                        1.0309680
#>      variable
#>       r_regency[regency_059,Intercept] r_regency[regency_060,Intercept]
#>   5%                        -0.8624002                       -1.9258997
#>   95%                        1.0746938                        0.3873318
#>      variable
#>       r_regency[regency_061,Intercept] r_regency[regency_062,Intercept]
#>   5%                        -0.4503556                       -0.2055982
#>   95%                        1.8032843                        2.8732038
#>      variable
#>       r_regency[regency_063,Intercept] r_regency[regency_064,Intercept]
#>   5%                        -0.7732058                       -1.1921068
#>   95%                        1.0673116                        0.7515556
#>      variable
#>       r_regency[regency_065,Intercept] r_regency[regency_066,Intercept]
#>   5%                        -1.5842160                       -2.0444678
#>   95%                        0.5527895                        0.3941948
#>      variable
#>       r_regency[regency_067,Intercept] r_regency[regency_068,Intercept]
#>   5%                        -0.8129541                       -2.4308545
#>   95%                        1.0299721                        0.2700321
#>      variable
#>       r_regency[regency_069,Intercept] r_regency[regency_070,Intercept]
#>   5%                        -0.9774569                       -2.4719052
#>   95%                        0.9119975                        0.2377517
#>      variable
#>       r_regency[regency_071,Intercept] r_regency[regency_072,Intercept]
#>   5%                        -0.4293281                       -1.6217188
#>   95%                        1.8702439                        0.4765958
#>      variable
#>       r_regency[regency_073,Intercept] r_regency[regency_074,Intercept]
#>   5%                        -2.9358476                        -1.148260
#>   95%                        0.2117917                         0.730387
#>      variable
#>       r_regency[regency_075,Intercept] r_regency[regency_076,Intercept]
#>   5%                        -0.9426874                       -0.4073616
#>   95%                        0.9171798                        1.9678612
#>      variable
#>       r_regency[regency_077,Intercept] r_regency[regency_078,Intercept]
#>   5%                        -1.0153972                       -1.5165009
#>   95%                        0.8559687                        0.5466559
#>      variable
#>       r_regency[regency_079,Intercept] r_regency[regency_080,Intercept]
#>   5%                        -0.2689795                        -0.516788
#>   95%                        2.5613471                         1.525764
#>      variable
#>       r_regency[regency_081,Intercept] r_regency[regency_082,Intercept]
#>   5%                        -1.1439176                       -1.5250915
#>   95%                        0.7659964                        0.4940346
#>      variable
#>       r_regency[regency_083,Intercept] r_regency[regency_084,Intercept]
#>   5%                        -1.8959669                       -0.9657439
#>   95%                        0.4290276                        0.7881775
#>      variable
#>       r_regency[regency_085,Intercept] r_regency[regency_086,Intercept]
#>   5%                        -0.6854984                       -0.2537162
#>   95%                        1.2388900                        2.6716193
#>      variable
#>       r_regency[regency_087,Intercept] r_regency[regency_088,Intercept]
#>   5%                        -0.6220721                       -0.6164877
#>   95%                        1.4612763                        1.5246780
#>      variable
#>       r_regency[regency_089,Intercept] r_regency[regency_090,Intercept]
#>   5%                        -0.4251226                       -0.7766951
#>   95%                        1.7676577                        1.1156169
#>      variable
#>       r_regency[regency_091,Intercept] r_regency[regency_092,Intercept]
#>   5%                         -0.300865                       -0.9397277
#>   95%                         2.284440                        0.9777499
#>      variable
#>       r_regency[regency_093,Intercept] r_regency[regency_094,Intercept]
#>   5%                         -1.339939                       -0.3434568
#>   95%                         0.569926                        2.0784489
#>      variable
#>       r_regency[regency_095,Intercept] r_regency[regency_096,Intercept]
#>   5%                        -1.4786456                       -1.9552372
#>   95%                        0.5769936                        0.3893045
#>      variable
#>       r_regency[regency_097,Intercept] r_regency[regency_098,Intercept]
#>   5%                         -0.484998                       -0.7376373
#>   95%                         1.697094                        1.2115195
#>      variable
#>       r_regency[regency_099,Intercept] r_regency[regency_100,Intercept]
#>   5%                        -0.5972142                       -1.3079461
#>   95%                        1.4226235                        0.5991122
#>      variable
#>          lprior       lp__   z_1[1,1]  z_1[1,2]  z_1[1,3]  z_1[1,4]  z_1[1,5]
#>   5%  -4.669981 -333.91671 -2.0832028 -1.349668 -2.731560 -1.154217 -1.080609
#>   95% -4.556847  -84.08301  0.8060169  1.191556  0.608765  1.333160  1.313812
#>      variable
#>         z_1[1,6]   z_1[1,7]   z_1[1,8]   z_1[1,9] z_1[1,10] z_1[1,11]
#>   5%  -0.9042602 -1.9842232 -0.9703484 -1.8396170 -1.355925 -1.590354
#>   95%  1.6822585  0.7134774  1.6346995  0.9062527  1.154916  1.034630
#>      variable
#>        z_1[1,12]  z_1[1,13] z_1[1,14]  z_1[1,15] z_1[1,16]  z_1[1,17]
#>   5%  -0.7069984 -1.7857016 -1.328830 -0.8735161 -2.080332 -3.2693482
#>   95%  2.0188204  0.9366266  1.139376  1.9580140  0.691904  0.3616728
#>      variable
#>        z_1[1,18]  z_1[1,19]  z_1[1,20]  z_1[1,21] z_1[1,22]  z_1[1,23]
#>   5%  -1.6108155 -2.8028662 -0.9549518 -0.8998044 -1.157533 -0.9352148
#>   95%  0.9876241  0.4963746  1.5701078  1.6565216  1.272422  1.6422729
#>      variable
#>       z_1[1,24]  z_1[1,25] z_1[1,26] z_1[1,27]  z_1[1,28]  z_1[1,29] z_1[1,30]
#>   5%  -1.019936 -0.7997091 -1.191710 -1.222995 -0.7785767 -1.6994429 -1.170577
#>   95%  1.487228  1.8366573  1.265265  1.262104  1.9056853  0.8971189  1.295240
#>      variable
#>       z_1[1,31] z_1[1,32]  z_1[1,33]  z_1[1,34]  z_1[1,35] z_1[1,36] z_1[1,37]
#>   5%  -1.308361 -1.095089 -0.6571443 -0.9968887 -1.6469875 -1.438281 -1.587629
#>   95%  1.149838  1.449103  2.3221914  1.5881529  0.9604803  1.139025  1.029658
#>      variable
#>        z_1[1,38] z_1[1,39]  z_1[1,40]  z_1[1,41]  z_1[1,42]  z_1[1,43]
#>   5%  -0.9612357 -1.024165 -0.7964382 -0.9858247 -0.9156996 -0.9877723
#>   95%  1.7606654  1.477007  2.0048592  1.7396236  1.6895299  1.6151860
#>      variable
#>       z_1[1,44]  z_1[1,45]  z_1[1,46]  z_1[1,47] z_1[1,48] z_1[1,49] z_1[1,50]
#>   5%  -1.008661 -1.7035302 -0.7119076 -0.7600451 -1.495829 -1.347402 -1.459372
#>   95%  1.556487  0.8045543  2.4009248  2.0026031  1.073252  1.081362  1.077474
#>      variable
#>       z_1[1,51] z_1[1,52] z_1[1,53]  z_1[1,54] z_1[1,55] z_1[1,56] z_1[1,57]
#>   5%  -1.048086 -1.954747 -1.585049 -0.9609586 -1.329656 -1.412271 -1.487004
#>   95%  1.433938  0.826465  1.030117  1.7084117  1.147312  1.150035  1.133311
#>      variable
#>       z_1[1,58] z_1[1,59]  z_1[1,60]  z_1[1,61]  z_1[1,62] z_1[1,63] z_1[1,64]
#>   5%  -1.232581 -1.172438 -1.7713509 -0.8851064 -0.6626854 -1.169219 -1.379726
#>   95%  1.340078  1.345838  0.8481793  1.6784451  2.2001263  1.337042  1.171499
#>      variable
#>       z_1[1,65]  z_1[1,66] z_1[1,67]  z_1[1,68] z_1[1,69]  z_1[1,70]  z_1[1,71]
#>   5%  -1.617536 -1.8788575 -1.217561 -2.0410044 -1.285158 -2.0839423 -0.9534745
#>   95%  1.004150  0.8620341  1.327728  0.6996618  1.345540  0.7083512  1.7268872
#>      variable
#>        z_1[1,72] z_1[1,73] z_1[1,74] z_1[1,75]  z_1[1,76] z_1[1,77]  z_1[1,78]
#>   5%  -1.6780584 -2.314192 -1.390630 -1.177502 -0.8625169 -1.340465 -1.5990426
#>   95%  0.9775441  0.754395  1.077038  1.211283  1.7948916  1.236560  0.9676809
#>      variable
#>        z_1[1,79]  z_1[1,80] z_1[1,81]  z_1[1,82]  z_1[1,83] z_1[1,84] z_1[1,85]
#>   5%  -0.7400079 -0.9101074 -1.345191 -1.5346579 -1.8064597 -1.274384 -1.115002
#>   95%  2.0005156  1.5552137  1.112254  0.9194448  0.8919871  1.142524  1.463394
#>      variable
#>        z_1[1,86] z_1[1,87] z_1[1,88]  z_1[1,89] z_1[1,90]  z_1[1,91] z_1[1,92]
#>   5%  -0.7631252 -1.023163 -1.090679 -0.9248782 -1.167483 -0.8059206 -1.217361
#>   95%  2.1170700  1.473692  1.612246  1.6796516  1.338843  1.8655945  1.225668
#>      variable
#>       z_1[1,93]  z_1[1,94]  z_1[1,95]  z_1[1,96]  z_1[1,97] z_1[1,98] z_1[1,99]
#>   5%  -1.474265 -0.8954907 -1.5662996 -1.7452024 -0.9279997 -1.110853 -1.007596
#>   95%  1.013114  1.7908739  0.9592165  0.9106774  1.6722879  1.333239  1.551477
#>      variable
#>       z_1[1,100]
#>   5%   -1.481560
#>   95%   1.066798
# }
```
