# The hbmfit S3 Class

`hbmfit` is the result class returned by all model-fitting functions in
hbsaems: [`hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md),
[`hbm_betalogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_betalogitnorm.md),
[`hbm_binlogitnorm`](https://madsyair.github.io/hbsaems/reference/hbm_binlogitnorm.md),
and
[`hbm_lnln`](https://madsyair.github.io/hbsaems/reference/hbm_lnln.md).
It wraps a `brmsfit` object together with the original data and fitting
metadata.

## Slots

- `model`:

  A `brmsfit` (or `brmsfit_multiple`) object.

- `missing_method`:

  Character scalar – the missing-data strategy used (`"deleted"`,
  `"model"`, `"multiple"`) or `NULL` when the data were complete.

- `data`:

  The **original** `data.frame` passed to the fitting function before
  any row deletion or imputation. Downstream functions (e.g.\\
  [`sae_predict`](https://madsyair.github.io/hbsaems/reference/sae_predict.md))
  need all rows – including those with missing \\Y\\ – to produce
  area-level predictions.

- `handle_missing`:

  Backwards-compatibility alias for `missing_method`; identical value.
