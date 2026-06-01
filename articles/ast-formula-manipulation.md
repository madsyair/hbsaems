# AST-based Formula Manipulation: How hbsaems Rewrites Your Formula Safely

This vignette explains, with worked examples, how `hbsaems` rewrites
user-supplied formulas behind the scenes – and why a regex-based
approach would silently corrupt many legitimate formulas.

If you have ever wondered *“how does `nonlinear = 'x2'` know not to
touch `I(x2/100)` somewhere else in the formula?”* this document is for
you. It is also a useful reference if you are extending `hbsaems` and
need to add a new sugar argument that rewrites the formula.

## Why does `hbsaems` need to rewrite formulas at all?

Users write simple, declarative formulas:

``` r

fml <- y ~ x1 + x2 + x3
```

But the actual `brms` formula passed to Stan often needs to be richer.
For instance, the high-level wrapper

``` r

hbm(
  formula           = bf(y ~ x1 + x2 + x3),
  data              = data_fhnorm,
  hb_sampling       = "gaussian",
  area_var          = c("province", "regency"),
  nonlinear         = "x2",
  measurement_error = list(x3 = "se_x3"),
  sampling_variance = "D"
)
```

needs to construct, internally:

``` r

bf(
  y ~ x1 + s(x2) + mi(x3, se_x3) + (1 | province/regency),
  sigma ~ 0 + offset(.hbsaems_sigma_fixed)
)
```

`hbsaems` does this by reading the *sugar* arguments (`nonlinear`,
`measurement_error`, `area_var`, `sampling_variance`, `handle_missing`,
…) and **transforming the formula’s AST**.

## Naive regex vs. AST: a cautionary tale

Suppose you wanted to “replace `x2` with `s(x2)`” using a regular
expression:

``` r

fml_str <- "y ~ x1 + x2 + x3"
gsub("x2", "s(x2)", fml_str)
#> [1] "y ~ x1 + s(x2) + x3"
```

That works for the simplest case. But now try:

``` r

gsub("x2", "s(x2)", "y ~ x1 + x2 + x21 + I(x2/100)")
#> [1] "y ~ x1 + s(x2) + s(x2)1 + I(s(x2)/100)"
```

The output `y ~ x1 + s(x2) + s(x2)1 + I(s(x2)/100)` is broken in
**three** ways:

- `x21` was corrupted to `s(x2)1` (substring collision)
- `I(x2/100)` was rewritten even though the user explicitly wrapped it
- The semantics of [`I()`](https://rdrr.io/r/base/AsIs.html) were
  destroyed in the process

A naive regex cannot solve all three at once, because they each require
*understanding* the structure of the formula – which characters belong
to which variable, which subexpression is wrapped in a function call,
where operator precedence places implicit grouping.

This understanding is exactly what an **AST (abstract syntax tree)**
gives you.

## Formulas as language trees

A formula in R is not a string – it is a *call object*, i.e. a node in
the language tree:

``` r

fml <- y ~ x1 + x2 + x3

class(fml)
#> [1] "formula"
typeof(fml)
#> [1] "language"
length(fml)
#> [1] 3
```

`length(fml) == 3` because the formula `~` is a binary operator with a
LHS and an RHS. We can walk the tree:

``` r

fml[[1]]           # the root operator
#> `~`
fml[[2]]           # the LHS
#> y
fml[[3]]           # the RHS
#> x1 + x2 + x3
```

The RHS is itself a call to `+`, and that call also has length 3:

``` r

fml[[3]][[1]]      # operator
#> `+`
fml[[3]][[2]]      # left subtree:  x1 + x2
#> x1 + x2
fml[[3]][[3]]      # right operand: x3
#> x3
```

Visually:

                        `~`
                       /   \
                      y     `+`           <- RHS root
                           /   \
                         `+`    x3        <- nested +
                        /   \
                      x1    x2

In principle you could write the formula rewriter as a manual recursive
walk over this tree. In practice we lean on
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) – which already
implements the walk for us and returns a clean list of top-level term
labels.

## `stats::terms()` as a tokeniser

[`terms()`](https://rdrr.io/r/stats/terms.html) decomposes the RHS into
top-level terms while leaving each term’s internal expression unchanged:

``` r

tt <- terms(y ~ x1 + I(x2 / 100) + s(x3) + x4:x5,
            keep.order = TRUE)
attr(tt, "term.labels")
#> [1] "x1"        "I(x2/100)" "s(x3)"     "x4:x5"
attr(tt, "intercept")
#> [1] 1
```

Key insight: each term label is the **string form of the term’s
expression**. `I(x2/100)` is *one* token, not three: when we later ask
“is this term equal to ‘x2’?”, the comparison is between strings, so
collisions are impossible.

This is the core trick that powers all of `hbsaems`’s formula sugar.

## Worked example: `.replace_nl_in_formula()`

The helper `.replace_nl_in_formula()` (called when you set
`nonlinear =`) rewrites bare variable names as smooth terms. Here is the
algorithm:

    INPUT:  fml, nonlinear (character), nonlinear_type ("spline" or "gp")

    STEP 1: tt <- stats::terms(fml, keep.order = TRUE)
            Extract:  term.labels, intercept, offset positions, response

    STEP 2: For each tl in term.labels:
              if tl is exactly one of the names in `nonlinear`:
                replace tl with `s(tl)` or `gp(tl, ...)`
              else:
                keep tl unchanged

    STEP 3: Splice the offset() terms back in (terms() strips them into
            a separate attribute).

    STEP 4: Reassemble via as.formula(paste(..., "~", ...)) preserving env.

Let us see this end-to-end:

``` r

# Round 1: simple
hbsaems:::.replace_nl_in_formula(
  fml            = y ~ x1 + x2 + x3,
  nonlinear      = "x2",
  nonlinear_type = "spline"
)
#> y ~ x1 + s(x2) + x3
```

``` r

# Round 2: substring-name safety -- x1 is a prefix of x10 and x11
hbsaems:::.replace_nl_in_formula(
  fml            = y ~ x1 + x10 + x11,
  nonlinear      = "x1",
  nonlinear_type = "spline"
)
#> y ~ s(x1) + x10 + x11
```

Note that `x10` and `x11` are **not** corrupted – they are different
terms, with different string labels.

``` r

# Round 3: wrapped variables are left alone
hbsaems:::.replace_nl_in_formula(
  fml            = y ~ x1 + I(x2/100) + x3,
  nonlinear      = "x2",
  nonlinear_type = "spline"
)
#> y ~ x1 + I(x2/100) + x3
```

The user explicitly wrapped `x2` in
[`I()`](https://rdrr.io/r/base/AsIs.html) to suppress operator
interpretation; `hbsaems` respects that choice and does not
double-rewrite it.

``` r

# Round 4: interactions left alone
hbsaems:::.replace_nl_in_formula(
  fml            = y ~ x1 + x1:x2 + x3,
  nonlinear      = "x1",
  nonlinear_type = "spline"
)
#> y ~ s(x1) + x1:x2 + x3
```

The interaction `x1:x2` is preserved bare – `brms` cannot reliably fit
`s(x1):x2`, so the conservative choice is to leave the user’s
interaction intact.

``` r

# Round 5: GP (Gaussian process) instead of spline
hbsaems:::.replace_nl_in_formula(
  fml            = y ~ x1 + x2 + x3,
  nonlinear      = "x2",
  nonlinear_type = "gp",
  gp_k           = 20,
  gp_cov         = "exp_quad"
)
#> y ~ x1 + gp(x2, k = 20, c = 1.25) + x3
```

``` r

# Round 6: offset() preserved across the rewrite
hbsaems:::.replace_nl_in_formula(
  fml            = y ~ x1 + offset(log(pop)) + x2,
  nonlinear      = "x1",
  nonlinear_type = "spline"
)
#> y ~ s(x1) + x2 + offset(log(pop))
```

This last case is subtle.
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) strips
[`offset()`](https://rdrr.io/r/stats/offset.html) into a separate
`attr(., "offset")` rather than including it in `term.labels` – meaning
that earlier versions of `hbsaems` silently **lost** offsets on rewrite.
The v1.0.0 fix re-extracts those positions and splices them back in.

## Walking the LHS: `.extract_response_names()`

For multivariate or `brms`-flavoured formulas, the LHS is itself a
mini-language with addition operators (`|`), distributional wrappers
([`mi()`](https://paulbuerkner.com/brms/reference/mi.html),
[`me()`](https://paulbuerkner.com/brms/reference/me.html), `trials()`,
`cens()`), and constructors
([`cbind()`](https://rdrr.io/r/base/cbind.html),
[`mvbind()`](https://paulbuerkner.com/brms/reference/mvbind.html)). We
need to recover only the **response** column names – not, for instance,
the `n` in `y | trials(n)`.

``` r

hbsaems:::.extract_response_names(quote(y))
#> [1] "y"
hbsaems:::.extract_response_names(quote(y | mi()))
#> [1] "y"
hbsaems:::.extract_response_names(quote(y | trials(n)))
#> [1] "y"
hbsaems:::.extract_response_names(quote(y | cens(c)))
#> [1] "y"
hbsaems:::.extract_response_names(quote(cbind(s, f) | trials(n)))
#> [1] "s" "f"
```

The implementation is a small AST walker:

``` r

while (is.call(lhs) && identical(lhs[[1L]], as.name("|"))) {
  lhs <- lhs[[2L]]
}
all.vars(lhs)
```

It recursively peels off the `|` operator (keeping the LEFT branch each
time, because the RIGHT branch holds the addition-wrappers we want to
discard), then asks [`all.vars()`](https://rdrr.io/r/base/allnames.html)
for the bare names that remain.

## Building a hierarchical RE term: `.build_area_re_formula()`

When you pass `area_var = c("province", "regency")` to
[`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md), the
helper builds the random-effect specification:

``` r

hbsaems:::.build_area_re_formula("province")
#> ~(1 | province)
#> <environment: 0x55c35a0a29c8>

hbsaems:::.build_area_re_formula(
  c("province", "regency"),
  structure = "nested"
)
#> ~(1 | province/regency)
#> <environment: 0x55c35a0e4460>

hbsaems:::.build_area_re_formula(
  c("province", "regency"),
  structure = "crossed"
)
#> ~(1 | province) + (1 | regency)
#> <environment: 0x55c35a12eba8>

hbsaems:::.build_area_re_formula(
  c("province", "regency", "district", "village"),
  structure = "nested"
)
#> ~(1 | province/regency/district/village)
#> <environment: 0x55c35a177130>
```

This formula is then
[`update()`](https://rdrr.io/r/stats/update.html)-ed onto the main
formula by `brms`, producing the final fitted model:

    y ~ x1 + x2 + (1 | province/regency/district/village)

## Combining the pieces: end-to-end example

Putting it all together, the rewriter pipeline for a request like

``` r

hbm(
  formula           = bf(y ~ x1 + offset(log(pop)) + x2 + x3),
  data              = data_fhnorm,
  area_var          = c("province", "regency"),
  nonlinear         = "x2",
  measurement_error = list(x3 = "se_x3")
)
```

walks through the following internal sequence (sketched):

``` r

fml_main <- bf(y ~ x1 + offset(log(pop)) + x2 + x3)$formula

# 1. measurement-error sugar
fml_main <- hbsaems:::.apply_measurement_error(
  fml_main, list(x3 = "se_x3")
)$formula
#> y ~ x1 + mi(x3, se_x3) + x2 + offset(log(pop))

# 2. nonlinear sugar
fml_main <- hbsaems:::.replace_nl_in_formula(
  fml_main, "x2", "spline"
)
#> y ~ x1 + mi(x3, se_x3) + s(x2) + offset(log(pop))

# 3. area random effects (built separately, then update()d in)
re_part <- hbsaems:::.build_area_re_formula(
  c("province", "regency"), "nested"
)
#> ~(1 | province/regency)

# Final formula handed to brms:
#> y ~ x1 + mi(x3, se_x3) + s(x2) + offset(log(pop)) +
#>     (1 | province/regency)
```

At every step the offset survives, the wrapped
[`mi()`](https://paulbuerkner.com/brms/reference/mi.html) survives,
substring collisions are impossible, and the original environment of the
formula is preserved. None of those guarantees would hold with a
regex-based rewriter.

## Why this matters in practice

1.  **You can mix syntaxes freely.** If you already know `brms` idioms,
    write them directly in the formula – `me(x, se_x)`, `poly(x, 2)`,
    `I(log(x))`, `offset(...)`, `(1 | g)`, `s(x)`, etc. `hbsaems` will
    leave them alone and only add the sugar you asked for.

2.  **No surprising substring corruption.** A variable called `x10` is
    never confused with `x1`, even when `x1` is being rewritten. This is
    invisible until it isn’t – the failure mode of a naive rewriter is
    silent corruption, not an error.

3.  **Predictable composition.** Sugar arguments compose: passing
    `nonlinear` *and* `measurement_error` in the same call produces the
    formula you expect, because each pass operates on independent AST
    tokens.

4.  **Future-proof.** When `brms` adds new addition operators or
    smooth-term constructors, the AST approach generally continues to
    work without modification, because we treat each top-level term as
    an opaque token.

## Extending `hbsaems`: adding your own sugar

If you contribute a new sugar argument to `hbsaems`, the convention is:

``` r

.apply_my_sugar <- function(formula, my_arg) {
  if (is.null(my_arg)) return(formula)

  rewrite_one <- function(fml) {
    tt <- tryCatch(stats::terms(fml, keep.order = TRUE),
                    error = function(e) NULL)
    if (is.null(tt)) return(fml)                # safer to skip than corrupt

    term_labels   <- attr(tt, "term.labels")
    has_intercept <- attr(tt, "intercept") == 1L
    response_lang <- if (length(fml) == 3L) fml[[2L]] else NULL
    env           <- environment(fml)

    # Preserve offset() (terms() strips it)
    offset_idx <- attr(tt, "offset")
    offset_strs <- if (length(offset_idx)) {
      vars <- attr(tt, "variables")
      vapply(offset_idx, function(i)
        deparse(vars[[i + 1L]], width.cutoff = 500L),
        character(1L))
    } else character(0L)

    # YOUR REWRITE RULE per term label
    new_labels <- vapply(term_labels, function(tl) {
      # ... transform tl if it matches your criterion ...
      tl
    }, character(1L))

    all_labels <- c(new_labels, offset_strs)
    rhs_str <- if (length(all_labels) == 0L) {
      if (has_intercept) "1" else "0"
    } else {
      paste(all_labels, collapse = " + ")
    }
    if (!has_intercept) rhs_str <- paste(rhs_str, "- 1")

    if (!is.null(response_lang)) {
      stats::as.formula(
        paste(deparse(response_lang, width.cutoff = 500L), "~", rhs_str),
        env = env
      )
    } else {
      stats::as.formula(paste("~", rhs_str), env = env)
    }
  }

  # Dispatch by formula class -- bf(), bf() + bf(), or plain formula
  if (inherits(formula, "brmsformula")) {
    formula$formula <- rewrite_one(formula$formula)
    return(formula)
  }
  if (inherits(formula, "mvbrmsformula")) {
    formula$forms <- lapply(formula$forms, function(f) {
      f$formula <- rewrite_one(f$formula)
      f
    })
    return(formula)
  }
  rewrite_one(formula)
}
```

This template appears verbatim in `.replace_nl_in_formula()` and
`.apply_measurement_error()` because it is the right pattern for *every*
top-level term rewrite.

## Summary

| Pattern | Tool |
|----|----|
| Rewrite bare predictors | [`stats::terms()`](https://rdrr.io/r/stats/terms.html) + map term labels |
| Detect response columns | Recursive walk via [`is.call()`](https://rdrr.io/r/base/call.html) |
| Preserve [`offset()`](https://rdrr.io/r/stats/offset.html) | Splice from `attr(., "offset")` |
| Preserve [`mi()`](https://paulbuerkner.com/brms/reference/mi.html), [`me()`](https://paulbuerkner.com/brms/reference/me.html), [`I()`](https://rdrr.io/r/base/AsIs.html) | Automatic (they are single tokens) |
| Build hierarchical `(1 \| a/b)` | String construct + [`as.formula()`](https://rdrr.io/r/stats/formula.html) |
| Multivariate (`mvbrmsformula`) | Apply rewrite per `$forms[[i]]` |
| Environment preservation | `as.formula(..., env = env)` |

The take-away is that R’s language objects are *first-class data*.
Treating them as such – rather than as strings to be sliced and glued –
buys you correctness for free in cases that a string-based rewriter
would silently get wrong.

## See also

- [`vignette("complete-workflow")`](https://madsyair.github.io/hbsaems/articles/complete-workflow.md)
  – the canonical end-to-end SAE pipeline using
  [`hbm()`](https://madsyair.github.io/hbsaems/reference/hbm.md) with
  sugar arguments.
- `vignette("advanced-features")` – spatial, benchmarking, custom
  families, and other extensions.
- [`?hbm`](https://madsyair.github.io/hbsaems/reference/hbm.md) –
  argument-by-argument reference for the sugar passed through to these
  rewriters.
