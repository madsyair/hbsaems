# hbsaems: Hierarchical Bayesian Area-Level Small Area Estimation Models

hbsaems fits **area-level** Hierarchical Bayesian Small Area Estimation
(HBSAE) models. Its methodological foundation follows the standard SAE
literature – primarily Rao and Molina (2015) – while computational
implementation adapts those models to the parameterisation and
prior-specification conventions of the brms package (Buerkner 2017),
which targets the Stan back-end.

## Scope

The package implements the standard **area-level** SAE models:

- Fay-Herriot normal (Fay & Herriot 1979),

- lognormal-lognormal for positive, right-skewed responses (Slud & Maiti
  2006; You & Chapman 2006),

- beta logit-normal for proportions (Liu 2009; Rao & Molina 2015, Sec.\\
  8.2),

- binomial logit-normal for counts out of trials.

Each can be augmented with spatial random effects (CAR / SAR / BYM2;
Besag et al.\\ 1991, Riebler et al.\\ 2016, Anselin 1988), nonlinear
smooth terms (thin-plate splines, Gaussian processes), and
survey-design-informed fixed parameters (precision parameter \\\phi_i =
n_i / \mathrm{deff}\_i - 1\\ for the beta model, \\\sigma_i =
\sqrt{\psi_i}\\ for the lognormal Fay-Herriot variant).

Unit-level SAE models (e.g.\\ the nested error model of Battese, Harter
& Fuller 1988) are *not* the focus of this package.

## Bayesian workflow support

To facilitate the principled Bayesian workflow advocated by Gelman et
al.\\ (2020), hbsaems provides:

- **Prior predictive checks** via
  [`prior_check`](https://madsyair.github.io/hbsaems/reference/prior_check.md);

- **MCMC convergence diagnostics** (Rhat, ESS, divergent transitions)
  via
  [`convergence_check`](https://madsyair.github.io/hbsaems/reference/convergence_check.md);

- **Posterior predictive checks** via
  [`brms::pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  on the underlying brmsfit;

- **Leave-one-out cross-validation** via loo;

- **Bayesian model comparison** via
  [`model_compare`](https://madsyair.github.io/hbsaems/reference/model_compare.md)
  /
  [`model_compare_all`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md)
  and Bayesian model averaging via
  [`model_average`](https://madsyair.github.io/hbsaems/reference/model_average.md);

- **Prior sensitivity analysis** via the optional priorsense
  integration;

- **Design-consistent benchmarking** of model-based estimates against
  direct estimates via
  [`sae_benchmark`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md);

- **Out-of-sample prediction** for unsampled areas via
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md).

## Layered Function Family

Three layers of entry points, picked by how much customisation is
needed:

1.  **Wrappers** –
    [`hbm_lnln`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md),
    [`hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md),
    [`hbm_binlogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md).
    SAE-friendly arguments (`auxiliary`, `group`, `n` + `deff`,
    `sampling_variance`).

2.  **Flexible factory** –
    [`hbm_flex`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md).
    Works with any registered family, exposes the generic `fixed_params`
    interface.

3.  **Universal entry point** –
    [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md).
    Accepts any
    [`brms::bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html)
    formula and any brms (or custom) family.

## Custom likelihoods

Two custom brms families are shipped (Loglogistic and Shifted
Loglogistic). Users can register their own via
[`register_hbsae_brms_custom`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md);
Stan code lives in `inst/stan/*.stan` as plain text and is loaded by
[`read_stan_function`](https://madsyair.github.io/hbsaems/reference/read_stan_function.md).

## Deprecated names (removal scheduled for v2.0.0)

The original v0.1.x short-form names continue to work but emit a
deprecation warning:
[`hbcc`](https://madsyair.github.io/hbsaems/reference/deprecated.md) -\>
[`convergence_check`](https://madsyair.github.io/hbsaems/reference/convergence_check.md),
[`hbmc`](https://madsyair.github.io/hbsaems/reference/deprecated.md) -\>
[`model_compare`](https://madsyair.github.io/hbsaems/reference/model_compare.md),
[`hbpc`](https://madsyair.github.io/hbsaems/reference/deprecated.md) -\>
[`prior_check`](https://madsyair.github.io/hbsaems/reference/prior_check.md),
[`hbsae`](https://madsyair.github.io/hbsaems/reference/deprecated.md)
-\>
[`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md).
The argument `predictors` is also deprecated in favour of `auxiliary`.
See
[`deprecated`](https://madsyair.github.io/hbsaems/reference/deprecated.md).

## Interactive use

A bilingual (English / Indonesian) shiny application, launched via
[`run_sae_app`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md),
exposes the same workflow to non-programmer analysts.

## Comprehensive examples reference

An all-in-one examples document covering every supported model family,
every advanced feature, and every utility is shipped at
`inst/examples/hbsaems-examples.Rmd`. Locate it with:


    system.file("examples", "hbsaems-examples.Rmd",
                package = "hbsaems")

and render with
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
for a 30+ page quick reference card.

## References

Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer
Academic Publishers.

Battese, G. E., Harter, R. M., & Fuller, W. A. (1988). An error
components model for prediction of county crop areas using survey and
satellite data. *Journal of the American Statistical Association*,
83(401), 28-36.

Besag, J., York, J., & Mollie, A. (1991). Bayesian image restoration,
with two applications in spatial statistics. *Annals of the Institute of
Statistical Mathematics*, 43, 1-20.

Buerkner, P.-C. (2017). brms: An R Package for Bayesian Multilevel
Models Using Stan. *Journal of Statistical Software*, 80(1), 1-28.
[doi:10.18637/jss.v080.i01](https://doi.org/10.18637/jss.v080.i01) .

Fay, R. E., & Herriot, R. A. (1979). Estimates of income for small
places: An application of James-Stein procedures to census data.
*Journal of the American Statistical Association*, 74(366), 269-277.

Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter, B.,
Yao, Y., Kennedy, L., Gabry, J., Buerkner, P.-C., & Modrak, M. (2020).
Bayesian workflow. *arXiv preprint*
[doi:10.48550/arXiv.2011.01808](https://doi.org/10.48550/arXiv.2011.01808)
.

Liu, B. (2009). *Hierarchical Bayes Estimation and Empirical Best
Prediction of Small-Area Proportions*. PhD thesis, University of
Maryland.

Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation* (2nd ed.).
Wiley.
[doi:10.1002/9781118735855](https://doi.org/10.1002/9781118735855) .

Riebler, A., Sorbye, S. H., Simpson, D., & Rue, H. (2016). An intuitive
Bayesian spatial model for disease mapping that accounts for scaling.
*Statistical Methods in Medical Research*, 25(4), 1145-1165.

Slud, E. V., & Maiti, T. (2006). Mean-squared error estimation in
transformed Fay-Herriot models. *JRSSB*, 68(2), 239-257.

You, Y., & Chapman, B. (2006). Small area estimation using area level
models and estimated sampling variances. *Survey Methodology*, 32(1),
97-103.

## See also

Useful links:

- <https://madsyair.github.io/hbsaems/>

- <https://github.com/madsyair/hbsaems>

- Report bugs at <https://github.com/madsyair/hbsaems/issues>

## Author

**Maintainer**: Achmad Syahrul Choir <madsyair@stis.ac.id>
([ORCID](https://orcid.org/0000-0001-7088-0646))

Authors:

- Saniyyah Sri Nurhayati

- Sofi Zamzanah

- Arsyka Laila Oktalia Siregar
