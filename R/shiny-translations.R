# R/shiny-translations.R
# =============================================================================
# Translation dictionary for the Shiny SAE app.
#
# Layout:
#   .hbsaems_translations[[lang]][[key]] = translated string
#
# Supported languages:
#   "en"  -- English (default; reference language)
#   "id"  -- Bahasa Indonesia
#
# Helper:
#   tr(key, lang) -- safe translator with English fallback.
#     If a key is missing in the chosen language, falls back to the
#     English value (never crashes); if also missing in English, returns
#     the key itself wrapped in [brackets] so unset strings stand out
#     visually during development.
#
# Adding a new language:
#   1. Add a new top-level element to .hbsaems_translations.
#   2. Provide translations for every key in the "en" reference.
#   3. Add the language code to the dropdown in inst/shiny/sae_app/app.R.
# =============================================================================


# ---- internal storage -------------------------------------------------------

.hbsaems_translations <- list(

  # =========================================================================
  # ENGLISH (reference language)
  # =========================================================================
  en = list(

    # ---- App-wide ----
    app_title        = "HBSAE Dashboard",
    app_subtitle     = "Hierarchical Bayesian Small Area Estimation",
    language_label   = "Language",

    # ---- Sidebar menu ----
    menu_home        = "Home",
    menu_data        = "Data",
    menu_modeling    = "Modeling",
    menu_diagnostics = "Diagnostics",
    menu_results     = "Results",
    menu_benchmark   = "Benchmark",
    menu_help        = "Help",
    menu_spatial     = "Spatial Setup",

    # ---- Spatial setup tab (v0.5.0) ----
    spatial_intro_title    = "About Spatial Setup",
    spatial_intro_text     = paste0(
      "Configure the spatial neighbourhood structure used by CAR and ",
      "SAR random effects.  Upload a shapefile or a pre-built weight ",
      "matrix here, preview the geometry, inspect the matrix, and ",
      "verify theoretical compatibility before fitting the model."
    ),
    spatial_subtab_source     = "Source",
    spatial_subtab_map        = "Map Preview",
    spatial_subtab_matrix     = "Weight Matrix",
    spatial_subtab_diagnostic = "Diagnostic",
    spatial_subtab_reference  = "Reference",
    spatial_box_upload     = "Source Selection",
    spatial_box_params     = "Construction Parameters",
    spatial_box_summary    = "Summary",
    spatial_box_heatmap    = "Weight Matrix Heatmap",
    spatial_box_degree     = "Degree Distribution",
    spatial_box_diag       = "Theoretical Diagnostic",
    spatial_box_advisor    = "CAR/SAR Compatibility Advisor",
    spatial_box_reference  = "Reference: When to use CAR vs SAR",
    spatial_no_matrix      = "No weight matrix loaded yet.  Use the Source tab to upload or build one.",
    spatial_no_shape       = "No shapefile loaded.  Upload one in the Source tab to see the map preview.",
    spatial_n_areas        = "Number of areas",
    spatial_n_links        = "Number of neighbour links",
    spatial_density        = "Density",
    spatial_mean_degree    = "Mean neighbours per area",
    spatial_max_degree     = "Maximum neighbours",
    spatial_isolated       = "Isolated areas",
    spatial_components     = "Connected components",
    spatial_style_detected = "Detected style",
    spatial_ref_car        = paste0(
      "CAR (Besag 1974) requires a symmetric binary matrix and is the ",
      "standard choice for disease mapping and discrete administrative ",
      "areas.  Sub-types ICAR, ESCAR, ESICAR, and BYM2 are available."
    ),
    spatial_ref_sar        = paste0(
      "SAR (Whittle 1954, Anselin 1988) typically uses a row-standardised ",
      "matrix and is preferred in spatial econometrics for continuous ",
      "spillover modelling.  It does not require symmetry."
    ),
    spatial_btn_apply      = "Apply to Model",
    spatial_apply_notice   = "This matrix will be used when sre_type is set in the Modeling tab.",
    spatial_status_none      = "No spatial data loaded yet.",
    spatial_status_shape     = "Active source: Shapefile (with geometry preview).",
    spatial_status_matrix    = "Active source: Pre-built weight matrix (no geometry).",
    spatial_map_unavailable  = paste0(
      "Map preview is only available when the source is a shapefile. ",
      "You uploaded a pre-built weight matrix, so geometry is not ",
      "available. Use the 'Weight Matrix' sub-tab instead."
    ),

    # ---- Common buttons ----
    btn_load_data    = "Load Data",
    btn_run_model    = "Run Model",
    btn_check_data   = "Check Data",
    btn_build_weight = "Build Weight Matrix",
    btn_inspect_weight = "Inspect Weight Matrix",
    btn_predict      = "Predict",
    btn_benchmark    = "Apply Benchmark",
    btn_download     = "Download",
    btn_reset        = "Reset",
    btn_help         = "Help",

    # ---- Box titles ----
    box_intro        = "Introduction",
    box_workflow     = "Example Workflow",
    box_families     = "Available Distribution Families",
    box_data_upload  = "Upload Data",
    box_data_preview = "Data Preview",
    box_data_check   = "Data Quality Check",
    box_model_spec   = "Model Specification",
    box_spatial      = "Spatial Random Effects",
    box_priors       = "Prior Settings",
    box_mcmc         = "MCMC Settings",
    box_convergence  = "Convergence Diagnostics",
    box_predictions  = "Area Predictions",
    box_benchmark    = "Benchmarking",

    # ---- Form labels ----
    label_response   = "Response variable",
    label_predictors = "Predictor variables",
    label_family     = "Distribution family",
    label_link       = "Link function",
    label_sre        = "Spatial area variable (optional)",
    label_sre_type   = "Spatial model type",
    label_M          = "Spatial weight matrix",
    label_chains     = "Number of chains",
    label_iter       = "Iterations per chain",
    label_warmup     = "Warmup iterations",
    label_seed       = "Random seed (optional)",
    label_prior_type = "Prior type",
    label_target     = "Benchmark target value",
    label_weights    = "Area weights",
    label_method     = "Benchmark method",
    label_posterior  = "Use fully Bayesian benchmark",

    # ---- Helper / info text ----
    help_response    = "The dependent variable to be modeled.",
    help_predictors  = "Auxiliary variables used to borrow strength.",
    help_family      = "Probability distribution of the response.",
    help_link        = "Function linking the linear predictor to the mean.",
    help_sre         = "Variable identifying the spatial area (if any).",
    help_check_data  = "Run a pre-fit check: missing values, types, and shape.",
    help_build_weight = "Construct W from a shapefile (queen/rook/knn/distance).",
    help_inspect_weight = "Verify W is theoretically compatible with CAR/SAR.",
    help_posterior   = paste0(
      "When checked, the benchmark is applied to every posterior draw ",
      "and credible intervals are recomputed from the adjusted draws."
    ),
    help_horseshoe   = "Regularised horseshoe prior (Piironen & Vehtari 2017).",
    help_r2d2        = "R2-D2 prior on the model R-squared (Zhang et al. 2022).",

    # ---- Tab names ----
    tab_workflow     = "Workflow",
    tab_data_summary = "Summary",
    tab_data_check   = "Quality Check",
    tab_model_fit    = "Fit Model",
    tab_model_compare = "Compare Models",
    tab_rhat         = "R-hat & ESS",
    tab_ppc          = "Posterior Predictive",
    tab_loo          = "LOO-CV",
    tab_table        = "Estimates Table",
    tab_map          = "Choropleth Map",

    # ---- Status / notifications ----
    msg_data_loaded     = "Data loaded successfully.",
    msg_data_invalid    = "Data validation failed: %s",
    msg_model_fitting   = "Fitting model... this may take a few minutes.",
    msg_model_done      = "Model fitted successfully.",
    msg_model_failed    = "Model fitting failed: %s",
    msg_converged       = "All chains have converged (Rhat < 1.05).",
    msg_not_converged   = "Convergence problems detected: see diagnostics.",
    msg_benchmark_done  = "Benchmark applied successfully.",
    msg_no_data         = "No data loaded yet. Upload a dataset first.",
    msg_no_model        = "No model fitted yet.",
    msg_select_response = "Please select a response variable.",
    msg_select_predictors = "Please select at least one predictor.",

    # ---- Static text ----
    text_intro_p1 = paste0(
      "Welcome to the hbsaems Shiny dashboard. This interactive ",
      "interface lets you fit Hierarchical Bayesian Small Area ",
      "Estimation models without writing R code."
    ),
    text_intro_p2 = paste0(
      "Use the sidebar to navigate through the workflow: upload data, ",
      "specify the model, run diagnostics, and benchmark the results."
    ),
    text_families_help = paste0(
      "hbsaems v0.5.0 ships with a registry of distributions usable ",
      "via the hbm() family argument or the convenience wrappers ",
      "(hbm_lnln, hbm_binlogitnorm, hbm_betalogitnorm)."
    ),
    text_families_advanced = paste0(
      "Advanced: register custom distributions via ",
      "register_hbsae_model() in your R session before launching the app."
    ),

    # ---- Workflow steps ----
    step_1 = "Upload a dataset with area-level direct estimates and auxiliary variables.",
    step_2 = "Specify the model structure (response, predictors, random effects, spatial effects).",
    step_3 = "Choose distribution family, link function, and define priors if needed.",
    step_4 = "Conduct prior predictive checking to evaluate the chosen priors.",
    step_5 = "Set MCMC configurations and fit the model.",
    step_6 = "Inspect convergence diagnostics and extract estimation results."
  ),

  # =========================================================================
  # BAHASA INDONESIA
  # =========================================================================
  id = list(

    # ---- App-wide ----
    app_title        = "Dasbor HBSAE",
    app_subtitle     = "Pendugaan Area Kecil Hirarkis Bayesian",
    language_label   = "Bahasa",

    # ---- Sidebar menu ----
    menu_home        = "Beranda",
    menu_data        = "Data",
    menu_modeling    = "Pemodelan",
    menu_diagnostics = "Diagnostik",
    menu_results     = "Hasil",
    menu_benchmark   = "Benchmark",
    menu_help        = "Bantuan",
    menu_spatial     = "Pengaturan Spasial",

    # ---- Spatial setup tab (v0.5.0) ----
    spatial_intro_title    = "Tentang Pengaturan Spasial",
    spatial_intro_text     = paste0(
      "Konfigurasi struktur ketetanggaan spasial yang digunakan oleh ",
      "efek acak CAR dan SAR.  Unggah shapefile atau matriks bobot ",
      "yang sudah dibangun di sini, lihat pratinjau geometri, periksa ",
      "matriks, dan verifikasi kesesuaian teoretis sebelum fitting model."
    ),
    spatial_subtab_source     = "Sumber",
    spatial_subtab_map        = "Pratinjau Peta",
    spatial_subtab_matrix     = "Matriks Bobot",
    spatial_subtab_diagnostic = "Diagnostik",
    spatial_subtab_reference  = "Referensi",
    spatial_box_upload     = "Pemilihan Sumber",
    spatial_box_params     = "Parameter Konstruksi",
    spatial_box_summary    = "Ringkasan",
    spatial_box_heatmap    = "Heatmap Matriks Bobot",
    spatial_box_degree     = "Distribusi Derajat",
    spatial_box_diag       = "Diagnostik Teoretis",
    spatial_box_advisor    = "Penasihat Kompatibilitas CAR/SAR",
    spatial_box_reference  = "Referensi: Kapan menggunakan CAR vs SAR",
    spatial_no_matrix      = "Matriks bobot belum dimuat.  Gunakan tab Sumber untuk mengunggah atau membangunnya.",
    spatial_no_shape       = "Shapefile belum dimuat.  Unggah di tab Sumber untuk melihat pratinjau peta.",
    spatial_n_areas        = "Jumlah area",
    spatial_n_links        = "Jumlah link ketetanggaan",
    spatial_density        = "Densitas",
    spatial_mean_degree    = "Rata-rata tetangga per area",
    spatial_max_degree     = "Maksimum tetangga",
    spatial_isolated       = "Area terisolasi",
    spatial_components     = "Komponen terhubung",
    spatial_style_detected = "Style terdeteksi",
    spatial_ref_car        = paste0(
      "CAR (Besag 1974) memerlukan matriks biner simetris dan merupakan ",
      "pilihan standar untuk disease mapping dan area administratif diskret. ",
      "Sub-tipe ICAR, ESCAR, ESICAR, dan BYM2 tersedia."
    ),
    spatial_ref_sar        = paste0(
      "SAR (Whittle 1954, Anselin 1988) umumnya menggunakan matriks ",
      "row-standardised dan lebih disukai dalam ekonometrika spasial ",
      "untuk pemodelan spillover kontinu.  Tidak memerlukan kesimetrisan."
    ),
    spatial_btn_apply      = "Terapkan ke Model",
    spatial_apply_notice   = "Matriks ini akan digunakan saat sre_type diatur di tab Pemodelan.",
    spatial_status_none      = "Data spasial belum dimuat.",
    spatial_status_shape     = "Sumber aktif: Shapefile (dengan pratinjau geometri).",
    spatial_status_matrix    = "Sumber aktif: Matriks bobot yang sudah dibangun (tanpa geometri).",
    spatial_map_unavailable  = paste0(
      "Pratinjau peta hanya tersedia ketika sumber berupa shapefile. ",
      "Anda mengunggah matriks bobot yang sudah dibangun, sehingga ",
      "geometri tidak tersedia. Gunakan sub-tab 'Matriks Bobot' sebagai gantinya."
    ),

    # ---- Common buttons ----
    btn_load_data    = "Muat Data",
    btn_run_model    = "Jalankan Model",
    btn_check_data   = "Periksa Data",
    btn_build_weight = "Bangun Matriks Bobot",
    btn_inspect_weight = "Periksa Matriks Bobot",
    btn_predict      = "Prediksi",
    btn_benchmark    = "Terapkan Benchmark",
    btn_download     = "Unduh",
    btn_reset        = "Atur Ulang",
    btn_help         = "Bantuan",

    # ---- Box titles ----
    box_intro        = "Pendahuluan",
    box_workflow     = "Contoh Alur Kerja",
    box_families     = "Distribusi yang Tersedia",
    box_data_upload  = "Unggah Data",
    box_data_preview = "Pratinjau Data",
    box_data_check   = "Pemeriksaan Kualitas Data",
    box_model_spec   = "Spesifikasi Model",
    box_spatial      = "Efek Acak Spasial",
    box_priors       = "Pengaturan Prior",
    box_mcmc         = "Pengaturan MCMC",
    box_convergence  = "Diagnostik Konvergensi",
    box_predictions  = "Prediksi per Area",
    box_benchmark    = "Benchmarking",

    # ---- Form labels ----
    label_response   = "Variabel respons",
    label_predictors = "Variabel prediktor",
    label_family     = "Distribusi keluarga",
    label_link       = "Fungsi link",
    label_sre        = "Variabel area spasial (opsional)",
    label_sre_type   = "Tipe model spasial",
    label_M          = "Matriks bobot spasial",
    label_chains     = "Jumlah chain",
    label_iter       = "Iterasi per chain",
    label_warmup     = "Iterasi warmup",
    label_seed       = "Random seed (opsional)",
    label_prior_type = "Tipe prior",
    label_target     = "Nilai target benchmark",
    label_weights    = "Bobot area",
    label_method     = "Metode benchmark",
    label_posterior  = "Gunakan benchmark Bayesian penuh",

    # ---- Helper / info text ----
    help_response    = "Variabel terikat yang akan dimodelkan.",
    help_predictors  = "Variabel auxiliary untuk meminjam kekuatan informasi.",
    help_family      = "Distribusi probabilitas dari variabel respons.",
    help_link        = "Fungsi yang menghubungkan predictor linear ke mean.",
    help_sre         = "Variabel yang mengidentifikasi area spasial (jika ada).",
    help_check_data  = "Jalankan pemeriksaan sebelum fitting: missing values, tipe, bentuk.",
    help_build_weight = "Bangun W dari shapefile (queen/rook/knn/distance).",
    help_inspect_weight = "Verifikasi W sesuai secara teoretis dengan CAR/SAR.",
    help_posterior   = paste0(
      "Jika dicentang, benchmark diterapkan pada setiap posterior draw ",
      "dan credible interval dihitung ulang dari draw yang telah disesuaikan."
    ),
    help_horseshoe   = "Prior horseshoe teregulasi (Piironen & Vehtari 2017).",
    help_r2d2        = "Prior R2-D2 pada R-squared model (Zhang et al. 2022).",

    # ---- Tab names ----
    tab_workflow     = "Alur Kerja",
    tab_data_summary = "Ringkasan",
    tab_data_check   = "Pemeriksaan Kualitas",
    tab_model_fit    = "Fit Model",
    tab_model_compare = "Bandingkan Model",
    tab_rhat         = "R-hat & ESS",
    tab_ppc          = "Posterior Predictive",
    tab_loo          = "LOO-CV",
    tab_table        = "Tabel Estimasi",
    tab_map          = "Peta Tematik",

    # ---- Status / notifications ----
    msg_data_loaded     = "Data berhasil dimuat.",
    msg_data_invalid    = "Validasi data gagal: %s",
    msg_model_fitting   = "Menjalankan model... mohon tunggu beberapa menit.",
    msg_model_done      = "Model berhasil di-fit.",
    msg_model_failed    = "Fitting model gagal: %s",
    msg_converged       = "Semua chain telah konvergen (Rhat < 1.05).",
    msg_not_converged   = "Terdeteksi masalah konvergensi: lihat diagnostik.",
    msg_benchmark_done  = "Benchmark berhasil diterapkan.",
    msg_no_data         = "Data belum dimuat. Unggah dataset terlebih dahulu.",
    msg_no_model        = "Model belum di-fit.",
    msg_select_response = "Silakan pilih variabel respons.",
    msg_select_predictors = "Silakan pilih minimal satu variabel prediktor.",

    # ---- Static text ----
    text_intro_p1 = paste0(
      "Selamat datang di dasbor Shiny hbsaems. Antarmuka interaktif ini ",
      "memungkinkan Anda untuk fitting model Hierarchical Bayesian Small ",
      "Area Estimation tanpa menulis kode R."
    ),
    text_intro_p2 = paste0(
      "Gunakan sidebar untuk navigasi alur kerja: unggah data, spesifikasi ",
      "model, jalankan diagnostik, dan benchmark hasil."
    ),
    text_families_help = paste0(
      "hbsaems v0.5.0 menyediakan registry distribusi yang dapat digunakan ",
      "melalui argumen family dari hbm() atau wrapper khusus ",
      "(hbm_lnln, hbm_binlogitnorm, hbm_betalogitnorm)."
    ),
    text_families_advanced = paste0(
      "Lanjutan: registrasi distribusi kustom melalui ",
      "register_hbsae_model() di session R sebelum menjalankan aplikasi."
    ),

    # ---- Workflow steps ----
    step_1 = "Unggah dataset dengan estimasi langsung tingkat area dan variabel auxiliary.",
    step_2 = "Spesifikasikan struktur model (respons, prediktor, efek acak, efek spasial).",
    step_3 = "Pilih distribusi keluarga, fungsi link, dan tentukan prior jika diperlukan.",
    step_4 = "Lakukan prior predictive checking untuk mengevaluasi prior yang dipilih.",
    step_5 = "Atur konfigurasi MCMC dan jalankan model.",
    step_6 = "Periksa diagnostik konvergensi dan ekstraksi hasil estimasi."
  )
)


# ---- public helpers ---------------------------------------------------------

#' Translate a UI String for the Shiny SAE App
#'
#' Looks up a translation key in the hbsaems translation dictionary.
#' When the requested language does not contain the key, it falls back
#' to English; when English also lacks the key, it returns the key
#' itself wrapped in brackets so missing strings stand out during
#' development.
#'
#' @param key Character.  Translation key (e.g. \code{"menu_home"}).
#' @param lang Character.  Language code; currently \code{"en"} or
#'   \code{"id"} (default \code{"en"}).
#'
#' @return A character scalar with the translated UI string.
#'
#' @examples
#' tr("menu_home", "en")    # "Home"
#' tr("menu_home", "id")    # "Beranda"
#' tr("nonexistent", "id")  # "[nonexistent]"
#'
#' @seealso \code{\link{tr_langs}}, \code{\link{tr_keys}}
#' @export
tr <- function(key, lang = "en") {
  stopifnot(is.character(key), length(key) == 1L,
            is.character(lang), length(lang) == 1L)

  trans <- .hbsaems_translations[[lang]]
  if (!is.null(trans) && !is.null(trans[[key]]))
    return(trans[[key]])

  # Fallback to English
  if (lang != "en") {
    en <- .hbsaems_translations[["en"]]
    if (!is.null(en[[key]])) return(en[[key]])
  }

  # Last resort: visible marker so the missing key shows up
  paste0("[", key, "]")
}


#' List Available Languages
#'
#' @return Character vector of supported language codes.
#'
#' @examples
#' tr_langs()
#'
#' @export
tr_langs <- function() names(.hbsaems_translations)


#' List All Translation Keys (for a Reference Language)
#'
#' Useful when adding a new language: enumerates every key that needs a
#' translation.
#'
#' @param lang Character.  Reference language (default \code{"en"}).
#' @return Sorted character vector of translation keys.
#'
#' @examples
#' head(tr_keys())
#'
#' @export
tr_keys <- function(lang = "en") {
  trans <- .hbsaems_translations[[lang]]
  if (is.null(trans))
    stop(sprintf("Unknown language code: '%s'. Available: %s",
                 lang, paste(tr_langs(), collapse = ", ")),
         call. = FALSE)
  sort(names(trans))
}
