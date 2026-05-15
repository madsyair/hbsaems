# Translate a UI String for the Shiny SAE App

Looks up a translation key in the hbsaems translation dictionary. When
the requested language does not contain the key, it falls back to
English; when English also lacks the key, it returns the key itself
wrapped in brackets so missing strings stand out during development.

## Usage

``` r
tr(key, lang = "en")
```

## Arguments

- key:

  Character. Translation key (e.g. `"menu_home"`).

- lang:

  Character. Language code; currently `"en"` or `"id"` (default `"en"`).

## Value

A character scalar with the translated UI string.

## See also

[`tr_langs`](https://madsyair.github.io/hbsaems/reference/tr_langs.md),
[`tr_keys`](https://madsyair.github.io/hbsaems/reference/tr_keys.md)

## Examples

``` r
tr("menu_home", "en")    # "Home"
#> [1] "Home"
tr("menu_home", "id")    # "Beranda"
#> [1] "Beranda"
tr("nonexistent", "id")  # "[nonexistent]"
#> [1] "[nonexistent]"
```
