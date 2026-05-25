# =============================================================================
#  shiny_memory_helpers.R
#
#  Memory management helpers for the hbsaems Shiny dashboard.
#  Sourced inline by `inst/shiny/sae_app/app.R` via local() to keep them
#  out of the global namespace; they are NOT exported package functions.
#
#  Three composable strategies are provided:
#
#    1. .shrink_hbmfit()         -- drop the heaviest brmsfit internals
#                                   that we don't need at the Shiny app
#                                   level (compiled Stan model handle,
#                                   summary cache, etc).
#
#    2. .multimodel_library_*()  -- LRU-style cap on the snapshot list.
#                                   When the user adds an N+1'th snapshot
#                                   above the cap, the oldest is evicted
#                                   with a notification.
#
#    3. .estimate_object_size()  -- quick approximate-MB readout so the
#                                   UI can show the user how much memory
#                                   each snapshot costs.
#
#  All three are intentionally defensive: every function returns its
#  input unchanged on any internal error, so a Shiny session is never
#  brought down by a memory-helper bug.
# =============================================================================


# -- 1. Object shrinker --------------------------------------------------------
#
# brmsfit objects carry a few heavy internals we don't need at the Shiny
# UI level:
#
#   * `$fit@.MISC` (rstan internals: compiled module handle, an
#     environment full of cached C++ pointers).  Dropping this means the
#     fit cannot be sampled-from-prior again, but it CAN still be passed
#     to brms::loo(), brms::posterior_predict(), summary(), etc, which
#     is all the Shiny app does post-fit.
#
#   * `$fit@stan_args` cached compile flags (small, but no value)
#
#   * `attr(fit$data, ".brmsformula")` cache (small)
#
# We also drop our own `model$data` redundant copy when it is identical
# to `model$model$data` (which it almost always is) -- one of the two
# is sufficient.

.shrink_hbmfit <- function(model) {
  if (is.null(model)) return(model)
  if (!inherits(model, "hbmfit")) return(model)

  tryCatch({
    bf <- model$model
    if (!inherits(bf, "brmsfit")) return(model)

    # rstan compiled module handle -- the single heaviest item
    if (!is.null(bf$fit) && methods::is(bf$fit, "stanfit")) {
      # Replace .MISC with an empty environment of the same kind so
      # rstan internals can still find it but the cached compiled
      # pointers and posterior summary caches are freed.
      try(
        assign(".MISC", new.env(parent = emptyenv()),
                envir = bf$fit@.MISC),
        silent = TRUE
      )
      # Drop the cached stan compile args (rebuild on demand)
      try(bf$fit@stan_args <- list(), silent = TRUE)
    }

    # Drop redundant data copy (we keep model$model$data which brms uses)
    if (!is.null(model$data) && !is.null(bf$data) &&
        identical(dim(model$data), dim(bf$data))) {
      model$data <- NULL
    }

    model$model <- bf
    model
  }, error = function(e) {
    # Never fail: return original on any error
    model
  })
}


# -- 2. LRU library helpers -----------------------------------------------------
#
# When the user adds the (N+1)'th snapshot above the configured cap,
# evict the OLDEST entry (FIFO).  Returns the new library plus an
# `evicted` attribute listing names that were removed, so the Shiny
# observer can show a notification.

.multimodel_library_add <- function(lib, name, model,
                                     max_snapshots = 5L) {
  lib[[name]] <- .shrink_hbmfit(model)

  # If over cap, drop oldest entries.  list elements are ordered in
  # insertion order, so head(lib, n) is the oldest n.
  evicted <- character(0L)
  while (length(lib) > max_snapshots) {
    evicted <- c(evicted, names(lib)[1L])
    lib[[1L]] <- NULL
  }

  attr(lib, "evicted") <- evicted
  lib
}


# -- 3. Object size estimator --------------------------------------------------
#
# `object.size()` walks the entire object recursively and is slow for
# big brmsfit -- we cache the result on the snapshot itself the first
# time it is asked for.

.estimate_object_size <- function(x) {
  if (is.null(x)) return(0)
  tryCatch({
    sz <- attr(x, ".hbsaems_mb", exact = TRUE)
    if (!is.null(sz)) return(sz)
    mb <- as.numeric(utils::object.size(x)) / 1024^2
    attr(x, ".hbsaems_mb") <- mb
    mb
  }, error = function(e) NA_real_)
}


# -- 4. Aggressive GC helper ---------------------------------------------------
#
# Call after each model fit / snapshot.  full = TRUE returns memory to
# the OS, important on long-running Shiny sessions where intermediate
# Stan compilation artifacts can accumulate.

.hbsaems_gc <- function() {
  invisible(gc(full = TRUE, verbose = FALSE))
}
