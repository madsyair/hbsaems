# data-raw/data_fhnorm.R
# =============================================================================
# Simulate `data_fhnorm` -- the running Fay-Herriot Normal example dataset
# for hbsaems vignettes, examples, and tests.
#
# This is the canonical R script that should be re-run whenever the
# dataset is regenerated.  An equivalent Python (numpy) generator was
# used to bootstrap the shipped .rda when an R install was not available;
# running this R script will overwrite the .rda with a deterministic
# R-side equivalent (statistics will differ slightly because numpy's
# Mersenne Twister differs from R's, but the design is identical).
#
# Run from the package root:
#   Rscript data-raw/data_fhnorm.R
#
# Design principles
# -----------------
# The simulated dataset is engineered to fit cleanly with the brms / Stan
# back-end so that vignettes can run with default sampler settings WITHOUT
# divergent transitions or convergence warnings.  Key choices:
#
#   * Covariates are STANDARDISED to N(0, 1) so the regression coefficients
#     beta_j live on a unit scale.  This avoids the most common cause of
#     funnel geometry in Bayesian random-intercept models -- a mismatched
#     scale between fixed-effect coefficients and the random-effect SD.
#
#   * The true area random-effect SD `sigma_u` is set to 1.0 (well clear of
#     the funnel "danger zone" sigma_u << 1) so the centred parameterisation
#     used inside brms remains efficient.
#
#   * Sampling variances `D_i` are drawn from `Gamma(shape = 4, rate = 4)` so
#     they cluster around 1 with a realistic range (~ 0.2 to 3.0), reflecting
#     varying sample sizes across regencies.  Crucially, `D_i` are TREATED
#     AS KNOWN in any Fay-Herriot fit -- vignettes must pass them via
#     `sampling_variance = "D"` so brms PINS sigma_i instead of trying to
#     estimate a single residual sigma (which would be unidentified once a
#     full area random effect is in the model).
#
#   * Province grouping (5 provinces, ~ 20 regencies each) follows the
#     v1.0.0 naming convention so the same dataset can be used for the
#     spatial / hierarchical examples (`area_var = c("province", "regency")`).
#
# Reproducibility: set.seed(20260518L); identical output across R versions.
# =============================================================================

library(usethis)

set.seed(20260518L)

# ---- Design constants -------------------------------------------------------
n_regency  <- 100L                                     # number of areas
n_province <- 5L                                       # higher-level group
beta_true  <- c(`(Intercept)` = 10, x1 = 0.8, x2 = -0.5, x3 = 0.3)
sigma_u    <- 1.0                                      # area RE SD (true)
# Hyperparameters of the sampling-variance distribution
D_shape    <- 4
D_rate     <- 4

# ---- Hierarchical labels ----------------------------------------------------
regency  <- sprintf("regency_%03d", seq_len(n_regency))
# Assign regencies to provinces in contiguous blocks of 20
prov_idx <- ((seq_len(n_regency) - 1L) %/% (n_regency / n_province)) + 1L
province <- sprintf("province_%02d", prov_idx)

# ---- Covariates (standardised) ---------------------------------------------
# x1, x2, x3 are drawn from independent N(0, 1).  We DO NOT center them
# afterwards because they are already standardised by construction.
x1 <- stats::rnorm(n_regency)
x2 <- stats::rnorm(n_regency)
x3 <- stats::rnorm(n_regency)

# ---- True area random effect ------------------------------------------------
u <- stats::rnorm(n_regency, mean = 0, sd = sigma_u)

# ---- Latent true area parameter ---------------------------------------------
theta_true <- beta_true["(Intercept)"] +
              beta_true["x1"] * x1 +
              beta_true["x2"] * x2 +
              beta_true["x3"] * x3 +
              u

# ---- Sampling variances (KNOWN from survey design) --------------------------
# Drawn from Gamma so values are strictly positive, with mean 1 and modest
# variability mimicking realistic between-area sample-size differences.
D <- stats::rgamma(n_regency, shape = D_shape, rate = D_rate)

# ---- Direct estimator y_i ~ N(theta_i, D_i) --------------------------------
y <- stats::rnorm(n_regency, mean = theta_true, sd = sqrt(D))

# ---- Assemble the data frame ------------------------------------------------
data_fhnorm <- data.frame(
  y          = y,
  D          = D,
  x1         = x1,
  x2         = x2,
  x3         = x3,
  theta_true = theta_true,
  u          = u,
  regency    = regency,
  province   = province,
  stringsAsFactors = FALSE
)

# ---- Sanity report ----------------------------------------------------------
cat("data_fhnorm simulated.\n")
cat("  rows                : ", nrow(data_fhnorm), "\n", sep = "")
cat("  beta_true           : ",
    paste(sprintf("%s=%.2f", names(beta_true), beta_true),
          collapse = ", "), "\n", sep = "")
cat("  sigma_u (true)      : ", sigma_u, "\n", sep = "")
cat("  Range of D          : ",
    sprintf("[%.3f, %.3f]", min(D), max(D)), "\n", sep = "")
cat("  Mean of D           : ", round(mean(D), 3), "\n", sep = "")
cat("  Range of y          : ",
    sprintf("[%.2f, %.2f]", min(y), max(y)), "\n", sep = "")

# ---- Save -------------------------------------------------------------------
usethis::use_data(data_fhnorm, overwrite = TRUE, compress = "xz")
