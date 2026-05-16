# Lokasi File YAML di Repo hbsaems

Panduan lengkap di mana setiap file YAML/YML harus diletakkan di
repository GitHub Anda untuk berfungsi dengan benar.

---

## Ringkasan Cepat

Ada **4 file YAML** yang Anda perlu push ke GitHub:

```
hbsaems/                                  ← root repository
├── _pkgdown.yml                          ← konfigurasi pkgdown site
└── .github/
    └── workflows/                        ← folder GitHub Actions
        ├── R-CMD-check.yaml              ← CI: R CMD check multi-OS
        ├── pkgdown.yaml                  ← CI: deploy website
        └── vignettes.yaml                ← CI: build vignettes
```

Total ukuran: ~9 KB (sangat kecil).

---

## Detail Per File

### 1. `_pkgdown.yml` — Konfigurasi Website Dokumentasi

**Lokasi**: **`/_pkgdown.yml`** (root repository, sejajar dengan
`DESCRIPTION`)

**Tujuan**: Mendefinisikan struktur dan tampilan website dokumentasi
yang dibangun oleh paket `pkgdown` dan di-deploy ke
<https://madsyair.github.io/hbsaems/>.

**Berisi**:
- URL website canonical
- Theme (Bootstrap 5, warna primer, font)
- Struktur navbar (intro, reference, articles, news)
- Pengelompokan articles (model walkthroughs, advanced topics)
- Pengelompokan reference index (modeling functions, diagnostics,
  prediction, spatial, custom families, dll)

**Konvensi**: Ekstensi `.yml` (bukan `.yaml`) — ini standar pkgdown
sejak v2.0.

**Catatan**: File ini **tidak masuk** ke tarball CRAN karena ada di
`.Rbuildignore`. Hanya dipakai di GitHub Actions saat build website.

### 2. `.github/workflows/R-CMD-check.yaml` — Continuous Integration

**Lokasi**: **`.github/workflows/R-CMD-check.yaml`**

**Tujuan**: Menjalankan `R CMD check --as-cran` otomatis setiap kali
Anda push ke branch `main` atau buka pull request. Ini memastikan
paket tetap CRAN-ready.

**Trigger**:
- Push ke `main` atau `master`
- Pull request ke `main` atau `master`

**Matrix testing**:
- Linux + R-release (utama)
- Linux + R-devel (sebelum CRAN check)
- Linux + R-oldrel (backward compat)
- Windows + R-release (Windows-only issues)
- macOS + R-release (cross-platform validation)

**Output**: Status badge merah/hijau di README + email notification kalau
ada error.

### 3. `.github/workflows/pkgdown.yaml` — Deploy Website

**Lokasi**: **`.github/workflows/pkgdown.yaml`**

**Tujuan**: Otomatis build website dari `_pkgdown.yml` dan deploy ke
GitHub Pages (branch `gh-pages`). Setelah deploy pertama, setiap push
ke `main` akan otomatis update website di
<https://madsyair.github.io/hbsaems/>.

**Trigger**:
- Push ke `main` atau `master`
- Release published
- Pull request (build only, tidak deploy)
- Manual trigger via Actions tab

**Output**: Website fresh dengan dokumentasi terbaru.

### 4. `.github/workflows/vignettes.yaml` — Vignette Build Check

**Lokasi**: **`.github/workflows/vignettes.yaml`**

**Tujuan**: Build hanya vignettes (lebih cepat dibanding full `R CMD
check`). Berguna untuk verify vignette tidak rusak setelah ubah
dokumentasi tanpa harus tunggu full check yang lama.

**Trigger**:
- Push yang touch `vignettes/` folder
- Push yang touch `R/` folder (karena vignettes load library)

**Output**: Pass/fail per vignette.

---

## Cara Push ke GitHub (Step-by-Step)

### Step 1: Verifikasi struktur folder di local repo

```bash
cd /path/to/hbsaems

# Struktur yang harus ada:
ls -la
# DESCRIPTION
# NAMESPACE
# _pkgdown.yml          ← INI di root
# README.md
# ...

ls -la .github/workflows/
# R-CMD-check.yaml      ← INI di .github/workflows/
# pkgdown.yaml          ← INI di .github/workflows/
# vignettes.yaml        ← INI di .github/workflows/
```

### Step 2: Stage + commit + push

```bash
git add _pkgdown.yml \
        .github/workflows/R-CMD-check.yaml \
        .github/workflows/pkgdown.yaml \
        .github/workflows/vignettes.yaml

git commit -m "Add CI workflows and pkgdown configuration"

git push origin main
```

### Step 3: Verifikasi di GitHub

Setelah push, cek:

1. **Workflows tab**: <https://github.com/madsyair/hbsaems/actions>
   - Anda akan melihat 3 workflow running

2. **Settings → Pages**: Aktifkan GitHub Pages
   - Source: `gh-pages` branch
   - Root folder

3. **Settings → Actions → General**: Permissions
   - Pilih "Read and write permissions"

4. Setelah workflows selesai (3-7 menit):
   - Site live di: <https://madsyair.github.io/hbsaems/>
   - Status badges muncul di README

---

## Apa yang Terjadi Setelah Push?

### Workflow Lifecycle

```
Anda: git push origin main
        │
        ▼
GitHub Actions trigger ──┬──> R-CMD-check.yaml (3-7 menit)
                         │      └─> Status badge updated
                         │
                         ├──> pkgdown.yaml (3-7 menit)
                         │      └─> Website rebuild + deploy
                         │
                         └──> vignettes.yaml (2-3 menit)
                                └─> Vignette validation
```

### Sebelum Push Pertama

Lakukan **one-time setup** ini sebelum push pertama (lihat
`inst/PKGDOWN_DEPLOY_GUIDE.md` untuk detail lengkap):

```r
# Dari R session, di project root
install.packages(c("pkgdown", "usethis"))
usethis::use_pkgdown_github_pages()
```

Atau manual:

```bash
# Buat orphan branch gh-pages
git checkout --orphan gh-pages
git rm -rf .
echo "Placeholder" > index.html
git add index.html
git commit -m "Initialise gh-pages branch"
git push origin gh-pages
git checkout main
```

---

## Apakah YAML Files Ini Masuk ke Tarball CRAN?

**Tidak**. Semua YAML files di-exclude dari tarball CRAN via
`.Rbuildignore`:

```bash
$ cat .Rbuildignore | grep -E "github|pkgdown"
^\.github$
^\.github/workflows$
^_pkgdown\.yml$
^pkgdown$
```

Ini **disengaja**:
- CRAN tidak pakai GitHub Actions, jadi workflows tidak relevan
- pkgdown config hanya dipakai di GitHub, bukan di CRAN
- Tarball CRAN harus minimalist (hanya hal yang R butuh untuk run)

Hasilnya: tarball clean ~361 KB, sementara repo GitHub punya semua
metadata + CI/CD.

---

## Common Mistakes & Troubleshooting

### Mistake 1: Letakkan workflow di tempat salah

❌ **Salah**: `workflows/R-CMD-check.yaml` (di root)  
❌ **Salah**: `.github/R-CMD-check.yaml` (tanpa workflows folder)  
✅ **Benar**: `.github/workflows/R-CMD-check.yaml`

GitHub Actions HANYA mendeteksi workflows di `.github/workflows/`.

### Mistake 2: Salah ekstensi

✅ **`.yaml`** bekerja  
✅ **`.yml`** juga bekerja

GitHub menerima keduanya. Tapi untuk **konsistensi**:
- pkgdown convention: `_pkgdown.yml`
- GitHub Actions convention: `*.yaml`

### Mistake 3: Workflow file tidak executable

YAML files **tidak perlu** executable permission (chmod). Git tracks
permission tapi GitHub Actions tidak peduli.

### Mistake 4: Lupa permissions di GitHub Settings

Workflow gagal push ke `gh-pages` karena permissions:
- Solusi: **Settings → Actions → General → Workflow permissions**
- Pilih "Read and write permissions"

### Mistake 5: gh-pages branch belum ada

`pkgdown.yaml` fail di first run kalau branch `gh-pages` belum ada:
- Solusi: Jalankan `usethis::use_pkgdown_github_pages()` dulu

---

## Inspect File Sebelum Push

Verifikasi semua YAML files valid:

```bash
# Validasi syntax (butuh yamllint, optional)
yamllint .github/workflows/R-CMD-check.yaml
yamllint .github/workflows/pkgdown.yaml
yamllint .github/workflows/vignettes.yaml
yamllint _pkgdown.yml

# Atau via R
R -e 'yaml::read_yaml("_pkgdown.yml")'
R -e 'yaml::read_yaml(".github/workflows/R-CMD-check.yaml")'
```

Kalau parse OK, syntax valid.

---

## Ringkasan Lokasi (Cheat Sheet)

```
File                            Lokasi di Repo                                   Tujuan
──────────────────────────────  ──────────────────────────────────────────────  ─────────────────────
_pkgdown.yml                    /_pkgdown.yml (root)                            Website config
R-CMD-check.yaml                /.github/workflows/R-CMD-check.yaml             CI: R CMD check
pkgdown.yaml                    /.github/workflows/pkgdown.yaml                 CI: Deploy website
vignettes.yaml                  /.github/workflows/vignettes.yaml               CI: Build vignettes
```

🎯 **Hanya 4 file YAML untuk sebuah R-package modern dengan CI/CD lengkap.**
