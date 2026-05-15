# Publishing the pkgdown Site to GitHub Pages

This guide walks through the complete process to publish the **hbsaems**
package documentation website at <https://madsyair.github.io/hbsaems/>.

---

## Overview

The pkgdown site is built automatically by GitHub Actions and deployed
to the `gh-pages` branch.  GitHub Pages serves that branch at
`https://madsyair.github.io/hbsaems/`.

```
              push to main / new release
                          │
                          ▼
                  GitHub Actions
                  (.github/workflows/pkgdown.yaml)
                          │
                          ▼
                  pkgdown::build_site_github_pages()
                          │
                          ▼
                  docs/ folder (HTML site)
                          │
                          ▼
                  Deploy to gh-pages branch
                          │
                          ▼
       https://madsyair.github.io/hbsaems/
```

All of this happens automatically once you complete the **one-time
setup** described below.

---

## Prerequisites

You should already have:

- A GitHub account with write access to `madsyair/hbsaems`
- Git installed and configured locally
- R (>= 4.0.0) with `pkgdown`, `devtools`, and `usethis` installed
- The files from this release already prepared:
  - `_pkgdown.yml` — site configuration
  - `.github/workflows/pkgdown.yaml` — CI workflow
  - `DESCRIPTION` with `URL:` field including the pkgdown URL
  - `.gitignore` with `docs/` listed

Install the R prerequisites:

```r
install.packages(c("pkgdown", "devtools", "usethis"))
```

---

## One-Time Setup

You only need to do this **once** per repository.  After this, every
push to `main` (and every release) will rebuild and redeploy the site
automatically.

### Step 1: Verify the package URL field in DESCRIPTION

Open `DESCRIPTION` and confirm the `URL:` line includes the pkgdown
site URL **first**, then the GitHub URL:

```yaml
URL: https://madsyair.github.io/hbsaems/, https://github.com/madsyair/hbsaems
BugReports: https://github.com/madsyair/hbsaems/issues
```

The pkgdown URL goes first because R's `help()` system uses the first
URL as the primary documentation link.

### Step 2: Verify `_pkgdown.yml` has the site URL

Confirm the top of `_pkgdown.yml` reads:

```yaml
url: https://madsyair.github.io/hbsaems/
```

This is what tells pkgdown how to build absolute links and the
canonical `<link rel="canonical">` tag for SEO.

### Step 3: Push these files to GitHub

From your local checkout of `madsyair/hbsaems`, sync the files from
the v1.0.0 release tree, then commit and push:

```bash
cd /path/to/hbsaems

# After copying the prepared files
git add DESCRIPTION _pkgdown.yml .github/workflows/pkgdown.yaml \
        .gitignore .Rbuildignore

git commit -m "Set up pkgdown site at madsyair.github.io/hbsaems"

git push origin main
```

### Step 4: Initialise the `gh-pages` branch (first run)

The first time the workflow runs, it will fail with a message like
"branch `gh-pages` does not exist".  You need to **create an empty
`gh-pages` branch** to bootstrap deployment:

```bash
# Create an orphan branch (no history) for gh-pages
git checkout --orphan gh-pages

# Remove all working-tree files (gh-pages is just the rendered site)
git rm -rf .

# Create a placeholder so the branch can be pushed
echo "Placeholder for pkgdown deployment" > index.html
git add index.html
git commit -m "Initialise gh-pages branch"
git push origin gh-pages

# Switch back to main
git checkout main
```

Alternatively, use the `usethis` helper which does all this for you:

```r
# From an R session, inside the hbsaems project root
usethis::use_pkgdown_github_pages()
```

This single command:

1. Adds `_pkgdown.yml` if missing
2. Creates the `gh-pages` branch
3. Adds the GitHub Actions workflow
4. Configures repository settings via the GitHub API (requires a
   `GITHUB_PAT` environment variable with `repo` scope)

If `usethis::use_pkgdown_github_pages()` has already been run, this
step is unnecessary.

### Step 5: Enable GitHub Pages in repository settings

Open the repository on GitHub: <https://github.com/madsyair/hbsaems/settings/pages>

Under **Build and deployment**:

- **Source**: select **Deploy from a branch**
- **Branch**: select **`gh-pages`** and the **`/ (root)`** folder
- Click **Save**

After a few minutes, the site will be live at
<https://madsyair.github.io/hbsaems/>.

### Step 6: Verify GitHub Actions permissions

For the deployment step to work, the workflow needs write access to
push commits to `gh-pages`.  Open:

<https://github.com/madsyair/hbsaems/settings/actions>

Under **Workflow permissions**:

- Select **Read and write permissions**
- Check **Allow GitHub Actions to create and approve pull requests**
- Click **Save**

This is a repository-wide setting and only needs to be done once.

---

## Triggering the First Build

Once the setup is complete, the workflow runs automatically on:

- Every push to `main` or `master`
- Every release publication
- Every pull request to `main` or `master` (build only, no deploy)
- Manual trigger via the **Actions** tab > **pkgdown** > **Run workflow**

To trigger it now, you can either:

**A.  Push any change to `main`:**

```bash
git commit --allow-empty -m "Trigger pkgdown rebuild"
git push origin main
```

**B.  Trigger manually from the Actions tab:**

1. Go to <https://github.com/madsyair/hbsaems/actions/workflows/pkgdown.yaml>
2. Click **Run workflow**
3. Select the `main` branch and click **Run workflow**

You can watch progress in real time.  The build typically takes 3-7
minutes (depending on dependency installation time).

---

## Building the Site Locally (Recommended Preview)

Before pushing changes, you can build the site locally to preview:

```r
# From the hbsaems project root
pkgdown::build_site()

# Or preview in your browser
pkgdown::preview_site()
```

This creates a `docs/` folder (gitignored) with the rendered HTML.
Open `docs/index.html` in a browser to inspect the result.

For faster iteration on a single page, you can rebuild only that page:

```r
pkgdown::build_home()         # Just the landing page
pkgdown::build_reference()    # Just the function reference
pkgdown::build_articles()     # Just the vignettes
pkgdown::build_news()         # Just the NEWS.md page
```

If you change `_pkgdown.yml`, run `pkgdown::init_site()` first to
refresh the assets.

---

## Customising the Site

The site configuration lives in `_pkgdown.yml`.  The current setup
includes:

- **Bootstrap 5** template with custom primary colour (`#1976D2` blue)
- **Source Sans Pro** for body text and headings (Google Fonts)
- **JetBrains Mono** for code
- **Navbar**: intro, reference, articles, news, GitHub link
- **Articles menu**: grouped into "Model walkthroughs", "Advanced
  topics", and "Reference"
- **Reference index**: organised into 8 topical groups (Model fitting,
  Diagnostics, Prediction, Spatial weights, Custom families,
  Configuration bundles, Object methods, Shiny dashboard, Datasets,
  Deprecated)

Common customisations:

```yaml
# Change the primary colour
template:
  bootstrap: 5
  bslib:
    primary: "#YOUR_HEX_HERE"

# Add a logo
home:
  links:
    - text: Learn more
      href: https://your-link.example.com
```

For the full list of options, see <https://pkgdown.r-lib.org/articles/customise.html>.

---

## Troubleshooting

### "Permission denied" on `gh-pages` push

The workflow can't write to the `gh-pages` branch.  Fix by enabling
write permissions in the repository settings (Step 6 above).

### "Branch `gh-pages` does not exist"

You skipped Step 4 of the one-time setup.  Run
`usethis::use_pkgdown_github_pages()` from R, or create the branch
manually.

### The site URL returns 404

GitHub Pages may take 1-2 minutes to propagate after the first
deploy.  Wait a few minutes and try again.  Also verify in
**Settings > Pages** that the source is set to `gh-pages` and `/ (root)`.

### Google Fonts not loading

The current `_pkgdown.yml` uses Google-hosted fonts.  If your local
build environment can't reach `fonts.googleapis.com`, the local
preview will warn.  This **does not affect the deployed site** -- GitHub
Actions has full internet access and will fetch the fonts correctly.

To avoid the local issue, you can temporarily comment out the
`base_font` / `heading_font` / `code_font` lines and rely on system
fonts when previewing offline.

### A vignette doesn't appear in the articles menu

Check `_pkgdown.yml` under `navbar > articles > menu`; the
`href:` must match the vignette filename (without `.Rmd`).  Example:

```yaml
- text: Spatial models (CAR / SAR / BYM2)
  href: articles/hbsaems-spatial.html
```

### CI build is slow

The first build of any R package CI typically takes 5-10 minutes
because all dependencies must be installed.  Subsequent builds are
faster (1-3 minutes) thanks to dependency caching via
`r-lib/actions/setup-r-dependencies@v2`.

### Workflow fails with "Error: pkgdown not found"

The CI installs pkgdown via `extra-packages: any::pkgdown` in the
workflow.  If this fails, check the `setup-r-dependencies` step
output -- it usually indicates a temporary CRAN or RSPM mirror issue;
re-running the workflow typically fixes it.

---

## Re-deploying After Changes

After the one-time setup, the site updates automatically:

- **Push to `main`** -> site rebuilds and redeploys
- **Publish a release** -> site rebuilds and redeploys with the new
  version banner
- **Open a pull request** -> CI builds the site to verify, but does
  not deploy (so PRs can't accidentally publish unfinished docs)

You don't need to commit `docs/` -- the workflow regenerates it on
every build.

---

## Manual Local Deployment (Emergency Backup)

If GitHub Actions is down and you need to deploy manually:

```r
# Build the site locally
pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)

# Deploy via pkgdown's built-in helper (uses gert + token)
pkgdown::deploy_to_branch(pkg = ".")
```

`pkgdown::deploy_to_branch()` will commit `docs/` to the `gh-pages`
branch and push.  You'll need a `GITHUB_PAT` set in your R environment
(see `?gh::gh_token`).

---

## Summary Checklist

Before your first push:

- [x] `DESCRIPTION` has `URL: https://madsyair.github.io/hbsaems/, ...`
- [x] `_pkgdown.yml` exists at repository root with `url:` set
- [x] `.github/workflows/pkgdown.yaml` exists
- [x] `.Rbuildignore` excludes `^docs$` and `^_pkgdown\.yml$`
- [x] `.gitignore` excludes `docs/`
- [ ] `gh-pages` branch exists on remote (run
  `usethis::use_pkgdown_github_pages()` once)
- [ ] Repository **Settings > Pages** source set to `gh-pages / (root)`
- [ ] Repository **Settings > Actions** has Read/Write permissions

After the first successful build:

- [ ] Site visible at <https://madsyair.github.io/hbsaems/>
- [ ] README badges link correctly
- [ ] Articles render with vignette content
- [ ] Function reference pages render correctly

You're done!  Future changes to `main` will rebuild and redeploy the
site automatically.

---

## References

- **pkgdown documentation**: <https://pkgdown.r-lib.org/>
- **r-lib/actions examples**: <https://github.com/r-lib/actions/tree/v2/examples>
- **GitHub Pages docs**: <https://docs.github.com/en/pages>
- **usethis::use_pkgdown_github_pages()**: <https://usethis.r-lib.org/reference/use_pkgdown.html>
