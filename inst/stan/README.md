# Custom Distribution Stan Code (`inst/stan/`)

This folder is the **single source of truth** for the Stan-side
function definitions of custom distributions used by `hbsaems`.  Each
`.stan` file contains only the Stan `functions` block contents for one
distribution (lpdf, lcdf, lccdf, rng).

## Available distributions

| File                          | Distribution             | Parameters       |
|-------------------------------|--------------------------|------------------|
| `loglogistic.stan`            | Loglogistic              | mu, beta         |
| `shifted_loglogistic.stan`    | Shifted (3-par) Loglogistic | mu, sigma, xi |

## Convention

All Stan code in this folder follows the **neodistr-style vectorised
convention** (`loop = FALSE` when calling `brms::custom_family()`).  This
means lpdf/lcdf/lccdf signatures accept *vectors* of `y` and `mu` and
return a single accumulated log-density (or log-CDF / log-CCDF); brms
does not generate a per-observation loop.

The rng function remains scalar, because brms' post-processing path
already loops over observations in R when generating posterior
predictives.

This choice was made because:

1. It matches the convention used by the `neodistr` package, which is
   the inspiration for this layout.
2. Vectorised Stan code is typically 1.5--3x faster for large `N` due
   to memory locality and reduced overhead per observation.
3. It removes the per-observation function-call overhead that limits
   `loop = TRUE` performance.

If you need scalar signatures for compatibility with a third-party
brms extension, write the Stan with scalar signatures and pass
`loop = TRUE` to `build_brms_custom_family()` -- the helper supports
both conventions; only `loop = FALSE` is the *default*.

## Function signatures (vectorised, `loop = FALSE`)

Each `.stan` file MUST define four functions:

```stan
real <name>_lpdf (vector y, vector mu, ...);   // returns scalar (sum)
real <name>_lcdf (vector y, vector mu, ...);   // returns scalar (sum)
real <name>_lccdf(vector y, vector mu, ...);   // returns scalar (sum)
real <name>_rng  (real <par1>, real <par2>, ...);  // returns scalar
```

The first argument of every lpdf/lcdf/lccdf MUST be `vector y` and the
second `vector mu`.  Subsequent arguments are scalar auxiliary
parameters (sigma, shape, etc.) in the order declared in `dpars`.

## Adding a new distribution

To add a new distribution `<myname>`:

1. **Create `<myname>.stan`** in this folder containing the four
   functions above with vectorised signatures.  Use existing files as
   templates.
2. **Add d/p/q/r helpers and three post-processing functions** in
   `R/dist-<myname>.R`:

   ```r
   # R-level d/p/q/r helpers
   d<myname> <- function(x, mu, ...) { ... }
   p<myname> <- function(q, mu, ...) { ... }
   q<myname> <- function(p, mu, ...) { ... }
   r<myname> <- function(n, mu, ...) { ... }

   # brms post-processing hooks
   log_lik_<myname> <- function(i, prep) {
     mu <- brms::get_dpar(prep, "mu", i = i)
     # ... other dpars ...
     y  <- prep$data$Y[i]
     d<myname>(y, mu = mu, ..., log = TRUE)
   }
   posterior_predict_<myname> <- function(i, prep, ...) {
     mu <- brms::get_dpar(prep, "mu", i = i)
     # ... other dpars ...
     r<myname>(length(mu), mu = mu, ...)
   }
   posterior_epred_<myname> <- function(prep) {
     mu <- brms::get_dpar(prep, "mu")
     # ... other dpars ...
     # closed-form mean (or numerical approximation)
     mu * <factor>
   }

   # brms custom_family wrapper
   brms_custom_<myname> <- function() {
     hbsaems::build_brms_custom_family(
       name              = "<myname>",
       dpars             = c("mu", ...),
       links             = c("identity", ...),
       lb                = c(NA, ...),
       ub                = c(NA, ...),
       type              = "real",     # or "int" for discrete distributions
       loop              = FALSE,      # match the vectorised .stan signatures
       log_lik           = log_lik_<myname>,
       posterior_predict = posterior_predict_<myname>,
       posterior_epred   = posterior_epred_<myname>
     )
   }
   ```

3. **Optionally** add an auto-registration call in
   `R/zzz.R::.register_builtin_custom_families()`.
4. **Add tests** in `tests/testthat/test-dist-<myname>.R` covering at
   least: PDF normalisation, CDF--quantile inverse identity, K-S
   goodness-of-fit on `r<myname>()` draws, the closed-form mean
   identity, and the structure of the custom_family object (including
   the three post-processing hooks).

The Stan code is loaded at runtime via
`hbsaems::read_stan_function("<myname>")` -- no R-side string
concatenation required.

## Why this layout?

* **Editor support.**  Stan files get proper syntax highlighting,
  brace matching, and linting in editors that recognise `.stan`.
* **One source of truth.**  No risk of R-side comments drifting away
  from the actual code that runs in Stan.
* **Easier review.**  Stan code is reviewed in isolation, with no R
  string-escaping noise.
* **Reusability.**  The same `.stan` file can be sourced by other
  Stan programs outside of brms (e.g.\ raw `rstan` workflows).
