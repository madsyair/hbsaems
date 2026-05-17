# hbsaems: Model Pendugaan Area Kecil Bayesian Hierarki Berbasis Area

**🌏 [Read in
English](https://github.com/madsyair/hbsaems/blob/main/README.md)**

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![CRAN
status](https://www.r-pkg.org/badges/version/hbsaems)](https://CRAN.R-project.org/package=hbsaems)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/hbsaems)](https://CRAN.R-project.org/package=hbsaems)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub](https://img.shields.io/badge/GitHub-madsyair/hbsaems-blue.svg)](https://github.com/madsyair/hbsaems)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

## Ringkasan

**hbsaems** adalah paket R untuk **Pendugaan Area Kecil (Small Area
Estimation, SAE) Bayesian Hierarki Berbasis Area (HBSAE)**. Landasan
metodologinya mengikuti literatur SAE standar – terutama **Rao dan
Molina (2015)** – sementara implementasi komputasinya diadaptasi dari
konvensi parameterisasi dan spesifikasi prior pada paket **brms**
(Bürkner, 2017), yang menggunakan back-end **Stan**.

Paket ini dirancang untuk mendukung **alur kerja Bayesian** yang
prinsipil (Gelman dkk., 2020) sehingga pemodelan SAE menjadi sistematis:
prior predictive checks, diagnostik konvergensi MCMC, posterior
predictive checks, leave-one-out cross-validation, perbandingan dan
rata-rata model Bayesian, analisis sensitivitas prior, serta
benchmarking design-consistent semuanya menjadi bagian dari pipeline
standar.

> **Cakupan.** Paket ini fokus pada model SAE **berbasis area**. Model
> SAE berbasis unit (mis. model komponen galat dari Battese, Harter &
> Fuller, 1988) bukan fokus paket ini.

## Fitur utama

- **Model SAE berbasis area**: Fay-Herriot normal, lognormal-lognormal,
  beta logit-normal, dan binomial logit-normal – masing-masing
  diadaptasi dari Rao dan Molina (2015) ke parameterisasi brms.
- **Penamaan argumen idiomatik SAE** (v1.0.0): argumen dinamai sesuai
  konsep SAE (`auxiliary`, `area_var`, `spatial_var`, `spatial_model`,
  `sampling_variance`, `n`, `deff`).
- **Parameter tetap berbasis desain survei**:
  - Presisi beta: $`\phi_i = n_i / \mathrm{deff}_i - 1`$ disematkan
    sebagai offset.
  - Lognormal: $`\sigma_i = \sqrt{\psi_i}`$ disematkan sebagai offset
    (Fay-Herriot lognormal).
- **Mekanisme `fixed_params` generik** untuk pengguna lanjutan dan
  distribusi kustom.
- **Pengaruh acak spasial**: CAR (ICAR / proper / BYM2) dan SAR (lag /
  error).
- **Distribusi brms kustom**: Loglogistic dan Shifted Loglogistic
  bawaan, ditambah kerangka registrasi untuk family buatan pengguna.
- **Data hilang**: penghapusan listwise, imputasi ganda (`mice`), atau
  imputasi Bayesian gabungan
  ([`brms::mi()`](https://paulbuerkner.com/brms/reference/mi.html))
  dengan auto-pilih.
- **Alur kerja Bayesian komprehensif**: prior predictive checks,
  diagnostik konvergensi MCMC, posterior predictive checks, LOO-CV,
  perbandingan dan rata-rata model Bayesian, analisis sensitivitas prior
  melalui `priorsense`, benchmarking design-consistent.
- **Prior shrinkage**: Horseshoe dan R2D2 yang dapat dipilih melalui
  `prior_type`.
- **Suku mulus non-linier**: thin-plate spline dan Gaussian process.
- **Aplikasi Shiny interaktif**: GUI dwibahasa (Inggris/Indonesia)
  melalui
  [`run_sae_app()`](https://madsyair.github.io/hbsaems/reference/run_sae_app.md).
- **Sepuluh vignette komprehensif** yang mencakup setiap model dan topik
  lanjutan.

## Instalasi

Instal versi pengembangan dari GitHub:

``` r

# install.packages("devtools")
devtools::install_github("madsyair/hbsaems")
```

Atau dengan vignette:

``` r

devtools::install_github("madsyair/hbsaems", build_vignettes = TRUE)
```

## Mulai cepat

``` r

library(hbsaems)

# Muat data contoh
data("data_fhnorm")
str(data_fhnorm[, c("regency", "province", "y", "D", "x1", "x2")])

# Pas model Fay-Herriot Normal dasar dengan IID area effect
model <- hbm(
  formula     = bf(y ~ x1 + x2 + x3),
  hb_sampling = "gaussian",
  hb_link     = "identity",
  re          = ~(1 | regency),
  data        = data_fhnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)

# Periksa ringkasan model
summary(model)
```

## Keluarga fungsi tiga-lapis

``` R
                      hbm()
                      Universal: family brms apapun, kustomisasi penuh
                                       ▲
                                       |
                      hbm_flex()
                      Registry family + auxiliary + fixed_params
                                       ▲
                                       |
       hbm_lnln() / hbm_betalogitnorm() / hbm_binlogitnorm()
       Wrapper ramah-SAE: response, auxiliary, area_var, ...
```

Sebagian besar pengguna mulai dengan wrapper. Naik ke
[`hbm_flex()`](https://madsyair.github.io/hbsaems/reference/hbm_flex.md)
ketika butuh family kustom atau antarmuka `fixed_params` generik; naik
ke [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) ketika
butuh kontrol brms penuh.

## Fungsi inti

### Fungsi pemodelan utama

#### `hbm()` – fungsi pemodelan universal

``` r

model <- hbm(
  formula        = bf(y ~ x1 + x2 + x3),  # rumus brms
  hb_sampling    = "gaussian",            # family likelihood
  hb_link        = "identity",            # fungsi link
  re             = ~(1 | regency),        # IID area RE (rumus)
  spatial_var    = "province",            # kolom RE spasial
  spatial_model  = "car",                 # "car" atau "sar"
  M              = adjacency_matrix_car,  # matriks bobot spasial
  data           = data_fhnorm
)
```

#### Wrapper khusus distribusi

**Beta logit-normal** untuk proporsi dalam $`(0, 1)`$:

``` r

fit_beta <- hbm_betalogitnorm(
  response  = "y",
  auxiliary = c("x1", "x2", "x3"),
  n         = "n",                # kolom ukuran sampel
  deff      = "deff",             # kolom design effect
  area_var  = "regency",          # area random effect
  data      = data_betalogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

**Binomial logit-normal** untuk jumlah sukses dari percobaan:

``` r

fit_bin <- hbm_binlogitnorm(
  response  = "y",
  trials    = "n",
  auxiliary = c("x1", "x2", "x3"),
  area_var  = "district",         # 100 kecamatan pada data ini
  data      = data_binlogitnorm,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

**Lognormal-lognormal** (varian Fay-Herriot) untuk respons positif yang
miring ke kanan:

``` r

fit_lnln <- hbm_lnln(
  response     = "y_obs",
  auxiliary    = c("x1", "x2", "x3"),
  area_var     = "district",
  sampling_variance = "psi_i",          # variansi sampling (skala log)
  data         = data_lnln,
  chains = 4, iter = 4000, warmup = 2000, cores = 2, seed = 1
)
```

### Fungsi alur kerja

| Fungsi | Tujuan | Mengganti (lama) |
|----|----|----|
| [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md) | Diagnostik konvergensi MCMC: $`\hat R`$, ESS, Geweke, Heidelberger, Raftery | [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) |
| [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md) | Perbandingan model LOO/WAIC/Bayes-factor dan posterior predictive checks | [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) |
| [`model_compare_all()`](https://madsyair.github.io/hbsaems/reference/model_compare_all.md) | Pemeringkatan multi-model analog dengan `loo_compare` | – |
| [`model_average()`](https://madsyair.github.io/hbsaems/reference/model_average.md) | Rata-rata model Bayesian (bobot manual, stacking, atau pseudo-BMA+ via `loo`) | – |
| [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md) | Prior predictive checks | [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) |
| [`prior_sensitivity()`](https://madsyair.github.io/hbsaems/reference/prior_sensitivity.md) | Diagnostik sensitivitas prior dengan power-scaling (`priorsense`) | – |
| [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md) | Prediksi SAE in-sample dan out-of-sample | [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) |
| [`sae_benchmark()`](https://madsyair.github.io/hbsaems/reference/sae_benchmark.md) | Benchmarking design-consistent | – |

### Fungsi pendukung

| Fungsi | Tujuan |
|----|----|
| [`check_data()`](https://madsyair.github.io/hbsaems/reference/check_data.md) | Pemeriksaan integritas data (missing, duplikat, ukuran sampel) |
| [`check_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/check_spatial_weight.md) | Pemeriksaan kompatibilitas teoretis matriks bobot spasial |
| [`build_spatial_weight()`](https://madsyair.github.io/hbsaems/reference/build_spatial_weight.md) | Membangun matriks adjacency / row-standardised |
| [`is_converged()`](https://madsyair.github.io/hbsaems/reference/is_converged.md) | Pemeriksaan konvergensi ya/tidak cepat |
| [`posterior_interval()`](https://madsyair.github.io/hbsaems/reference/posterior_interval.md) | Selang kredibel dari draws posterior |
| [`posterior_draws()`](https://madsyair.github.io/hbsaems/reference/posterior_draws.md) | Ekstrak draws posterior sebagai data frame panjang |
| [`hbm_info()`](https://madsyair.github.io/hbsaems/reference/hbm_info.md) | Inspeksi spek model, prior, pengaturan sampler |
| [`hbm_warnings()`](https://madsyair.github.io/hbsaems/reference/hbm_warnings.md) | Tampilkan peringatan pas model |

## Distribusi kustom

`hbsaems` menyertakan dua family brms kustom untuk data positif yang
miring ke kanan:

- **Loglogistic** – parameterisasi Fisk kanonik mengikuti
  `flexsurv::dllogis` dan Wikipedia. Lihat
  [`?loglogistic`](https://madsyair.github.io/hbsaems/reference/loglogistic.md)
  dan
  [`?brms_custom_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_loglogistic.md).
- **Shifted Loglogistic** – parameterisasi gaya GEV dari Hosking &
  Wallis (1997). Lihat
  [`?shifted_loglogistic`](https://madsyair.github.io/hbsaems/reference/shifted_loglogistic.md)
  dan
  [`?brms_custom_shifted_loglogistic`](https://madsyair.github.io/hbsaems/reference/brms_custom_shifted_loglogistic.md).

Keduanya diregistrasi otomatis ke brms saat paket dimuat melalui
[`register_hbsae_brms_custom()`](https://madsyair.github.io/hbsaems/reference/register_hbsae_brms_custom.md).

## Aplikasi Shiny interaktif

Paket ini menyertakan aplikasi Shiny dwibahasa (Inggris/Indonesia):

``` r

run_sae_app()
```

Aplikasi menyediakan alur kerja terpandu: unggah data → eksplorasi →
pengaturan spasial → pemodelan → diagnostik → unduh hasil.

## Vignette

Paket ini berisi sepuluh vignette yang secara progresif memandu alur
pemodelan:

``` r

browseVignettes("hbsaems")
```

| Vignette | Topik |
|----|----|
| `complete-workflow` | Contoh Fay-Herriot end-to-end dengan alur Bayesian penuh |
| `hbsaems-modelling` | Konsep pemodelan dan API tiga-lapis |
| `hbsaems-betalogitnorm-model` | Beta logit-normal untuk proporsi |
| `hbsaems-binlogitnorm-model` | Binomial logit-normal untuk count |
| `hbsaems-lnln-model` | Varian Fay-Herriot lognormal-lognormal |
| `hbsaems-spatial` | Model spasial CAR / SAR / BYM2 |
| `hbsaems-handle-missing` | Tiga strategi data hilang |
| `advanced-features` | Prior shrinkage, spline, family kustom |
| `hbsaems-run_sae_app` | Menggunakan aplikasi Shiny |
| `migration-guide` | Migrasi dari v0.x ke v1.0.0 |

## Migrasi dari v0.x

Jika Anda berasal dari hbsaems v0.x, lihat vignette `migration-guide`.
Perubahan paling jelas:

| Lama (deprecated) | Baru (kanonik) |
|----|----|
| [`hbcc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) | [`convergence_check()`](https://madsyair.github.io/hbsaems/reference/convergence_check.md) |
| [`hbmc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) | [`model_compare()`](https://madsyair.github.io/hbsaems/reference/model_compare.md) |
| [`hbpc()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) | [`prior_check()`](https://madsyair.github.io/hbsaems/reference/prior_check.md) |
| [`hbsae()`](https://madsyair.github.io/hbsaems/reference/deprecated.md) | [`sae_predict()`](https://madsyair.github.io/hbsaems/reference/sae_predict.md) |
| `group =` | `area_var =` |
| `sre =` | `spatial_var =` |
| `sre_type =` | `spatial_model =` |
| `predictors =` | `auxiliary =` |
| `sampling_var =` | `sampling_variance =` |

Nama lama tetap berfungsi di v1.0.0 dengan peringatan deprecation dan
akan dihapus di v2.0.0.

## Sitasi

Jika Anda menggunakan **hbsaems** dalam riset terpublikasi, mohon
sitasi:

``` r

citation("hbsaems")
```

## Referensi

- Battese, G. E., Harter, R. M., & Fuller, W. A. (1988). An
  error-components model for prediction of county crop areas using
  survey and satellite data. *Journal of the American Statistical
  Association*, 83(401), 28-36.
- Bürkner, P.-C. (2017). brms: An R package for Bayesian multilevel
  models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
- Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter,
  B., Yao, Y., Kennedy, L., Gabry, J., Bürkner, P.-C., & Modrák, M.
  (2020). Bayesian workflow. *arXiv:2011.01808*.
- Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation* (2nd
  edition). Wiley.

## Lisensi

GPL (\>= 3)

## Penulis

- **Achmad Syahrul Choir** (maintainer, penulis) –
  <madsyair@stis.ac.id>, Politeknik Statistika STIS, Jakarta
- Saniyyah Sri Nurhayati (penulis)
- Sofi Zamzanah (penulis)
- Arsyka Laila Oktalia Siregar (penulis)
