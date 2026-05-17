# ==========================================================================
# 1. STAN-SIDE CROSS-CHECK (only when rstan installed)
# ==========================================================================

test_that("Stan loglogistic_lpdf matches R dloglogistic(log = TRUE)", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  
  stan_file <- system.file("stan", "loglogistic.stan", package = "hbsaems")
  if (stan_file == "") {
    stan_file <- file.path("../../inst/stan/loglogistic.stan")
  }
  
  if (!file.exists(stan_file)) {
    stop("Gagal: File loglogistic.stan tidak ditemukan")
  }
  
  fns_code <- paste(readLines(stan_file), collapse = "\n")
  # Tambahkan blok model {} kosong karena stan_model membutuhkannya
  stancode <- paste0("functions {\n", fns_code, "\n}\nmodel { }\n")
  
  # 1. Compile model TANPA tryCatch agar error C++/Stan tidak disembunyikan
  mod <- rstan::stan_model(model_code = stancode)
  
  # 2. Expose fungsi dari model yang sudah sukses dikompilasi
  rstan::expose_stan_functions(mod, envir = environment())
  
  # 3. Pengujian nilai
  y_vec   <- c(0.5, 1, 2, 5)
  mu_vec  <- c(0.5, 1, 2, 5)
  beta    <- 2.5
  
  stan_val <- loglogistic_lpdf(y_vec, mu_vec, beta)
  r_val    <- sum(dloglogistic(y_vec, mu = mu_vec, beta = beta, log = TRUE))
  
  expect_equal(stan_val, r_val, tolerance = 1e-8)
})

test_that("Stan shifted_loglogistic_lpdf matches R dshifted_loglogistic(log=TRUE)", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  
  stan_file <- system.file("stan", "shifted_loglogistic.stan", package = "hbsaems")
  if (stan_file == "") {
    stan_file <- file.path("../../inst/stan/shifted_loglogistic.stan")
  }
  
  if (!file.exists(stan_file)) {
    stop("Gagal: File shifted_loglogistic.stan tidak ditemukan")
  }
  
  fns_code <- paste(readLines(stan_file), collapse = "\n")
  stancode <- paste0("functions {\n", fns_code, "\n}\nmodel { }\n")
  
  # 1. Compile model
  mod <- rstan::stan_model(model_code = stancode)
  
  # 2. Expose fungsi
  rstan::expose_stan_functions(mod, envir = environment())
  
  # 3. Pengujian nilai
  y_vec    <- c(-1, 0, 1, 2)
  mu_vec   <- rep(0, length(y_vec))
  sigma    <- 1
  xi       <- 0.3
  
  stan_val <- shifted_loglogistic_lpdf(y_vec, mu_vec, sigma, xi)
  r_val    <- sum(dshifted_loglogistic(y_vec, mu = 0, sigma = sigma, xi = xi, log = TRUE))
  
  expect_equal(stan_val, r_val, tolerance = 1e-8)
})