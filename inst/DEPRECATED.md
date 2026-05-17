# Deprecated Functions in `hbsaems`

This document tracks the deprecation status of `hbsaems` functions.

There are two categories:

1. **Active deprecation** -- function is still callable but emits a
   `.Deprecated()` warning.  Existing code keeps working; only the
   warning tells you the recommended replacement.  Scheduled removal:
   v1.0.0.

2. **Removed** -- function was deprecated in an earlier release and is
   no longer present.  Existing code that uses it will now error with
   `could not find function ...`.  See the "Migration" section for the
   exact one-line substitutions.

---

## Currently active deprecations (v1.0.0)

These four functions were renamed in v1.0.0 and remain available as
deprecated aliases.  All four forward to their replacement, so
behaviour is identical to the new name.

| Deprecated  | Replacement           | Since  | Note |
|-------------|-----------------------|:------:|------|
| `hbcc()`    | `convergence_check()` | v1.0.0 | Match `verb_noun()` naming convention |
| `hbmc()`    | `model_compare()`     | v1.0.0 | Match `verb_noun()` naming convention |
| `hbpc()`    | `prior_check()`       | v1.0.0 | Match `verb_noun()` naming convention |
| `hbsae()`   | `sae_predict()`       | v1.0.0 | Disambiguate from the package name |

```r
# v1.0.0 style (deprecated -- warns once per session):
hbcc(model)        # diagnostics
hbmc(model)        # model comparison
hbpc(model, ...)   # prior predictive
hbsae(model, ...)  # prediction

# v1.0.0+ style (current):
convergence_check(model)
model_compare(model)
prior_check(model, ...)
sae_predict(model, ...)
```

---

## Removed in v1.0.0 (no longer callable)

The three aliases below were active deprecations in earlier versions
but have been **removed** as of v1.0.0.  Their replacements
(`hbm_flex()` and `hbm()`) provide identical functionality.

| Removed             | Use instead    | Original deprecation | Removed in |
|---------------------|----------------|:--------------------:|:----------:|
| `hbsae_wrapper()`   | `hbm_flex()`   | v1.0.0               | v1.0.0     |
| `hbm_generic()`     | `hbm_flex()`   | v1.0.0               | v1.0.0     |
| `hbm_call()`        | `hbm()`        | v1.0.0               | v1.0.0     |

Why three aliases pointing to two replacements?  Two reasons:

* The factory function evolved through two naming generations
  (`hbsae_wrapper` -> `hbm_generic` -> `hbm_flex`); both legacy names
  were preserved in `v1.0.0` -- `v1.0.0`, then removed together.
* `hbm_call` was a v1.0.0 helper that spliced configuration bundles
  into `hbm()`.  Since v1.0.0, `hbm()` does that splicing itself when
  passed objects of class `hbm_config`, so `hbm_call` became redundant.

### Migration

```r
# OLD                                          NEW
hbsae_wrapper("beta", ...)                     hbm_flex("beta", ...)
hbm_generic("beta", ...)                       hbm_flex("beta", ...)
hbm_call(formula, data,                        hbm(formula, data,
         hbm_control(...), hbm_priors(...))         hbm_control(...), hbm_priors(...))
```

Note the second example: `hbm()` now accepts configuration bundles
directly through `...`, so the syntax is otherwise unchanged.

---

## Bulk migration script

If you maintain a codebase that uses one or more of the affected
functions, this `sed` one-liner does the substitution in place:

```bash
# from the root of your project
find . -name '*.R' -o -name '*.Rmd' | xargs sed -i \
  -e 's/\bhbcc(/convergence_check(/g' \
  -e 's/\bhbmc(/model_compare(/g' \
  -e 's/\bhbpc(/prior_check(/g' \
  -e 's/\bhbsae(/sae_predict(/g' \
  -e 's/\bhbsae_wrapper(/hbm_flex(/g' \
  -e 's/\bhbm_generic(/hbm_flex(/g' \
  -e 's/\bhbm_call(/hbm(/g'
```

After running, re-run your tests.  Nothing should break:

* The four still-deprecated names (`hbcc`, `hbmc`, `hbpc`, `hbsae`) are
  pure forwarders to their new names.
* The three removed names had pure forwarders too, so the substitutions
  above are semantically identical to the originals.

---

## Deprecation policy

`hbsaems` follows a **conservative, two-stage deprecation policy**:

* **Stage 1: announce.**  When a function is renamed, the old name
  becomes a deprecated alias that forwards to the new name and emits a
  `.Deprecated()` warning.  Existing code keeps working.
* **Stage 2: remove.**  After at least two minor versions (or one major
  version), the alias is removed.

The four Group A functions (`hbcc`, `hbmc`, `hbpc`, `hbsae`) are
currently in Stage 1; their removal is scheduled for **v1.0.0**.

For the authoritative list at any moment, see `?deprecated` in an
installed copy of the package, or `R/deprecated.R` in the source tree.
