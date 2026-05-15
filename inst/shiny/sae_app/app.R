# =============================================================================
# inst/shiny/sae_app/app.R
# Shiny application for Hierarchical Bayesian Small Area Estimation (HBSAE)
#
# v0.3.0 changes:
#   - All API calls migrated to snake_case primary names:
#       hbcc()  -> convergence_check()
#       hbmc()  -> model_compare()
#       hbpc()  -> prior_check()      (now requires `data` argument)
#       hbsae() -> sae_predict()
#   - Local var renamed to avoid shadowing prior_check() function.
#
# Earlier changes (preserved):
#   - Removed XICOR dependency (Chatterjee's Xi correlation removed)
#   - Replaced deprecated ggplot2::aes_string() with ggplot2::aes(.data[[]])
#   - Removed mixed-language (Indonesian) inline comments
#   - Updated missing data UI note to reflect that "multiple" imputes X only
#   - Added prior_type UI (horseshoe / R2D2)
#   - Added nonlinear variable selector (spline / GP)
#   - Added observer to sync nonlinear_vars choices with linear_aux_vars
# =============================================================================

# The hbsaems package loads all required dependencies via its NAMESPACE.
# Only library(shiny) is needed explicitly to start the app before the
# package namespace is attached.
library(shiny)
library(hbsaems)


# =============================================================================
# UI
# =============================================================================
ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
        title = textOutput("ui_app_title", inline = TRUE),
        # -- Language selector (v0.5.0) ---------------------------------------
        # Custom dropdown in the header right showing the active language.
        # Initial state is English; user can switch any time without restart.
        tags$li(
            class = "dropdown",
            style = "padding: 12px 15px;",
            tags$div(
                style = "color: white; font-size: 13px;",
                tags$span(
                    icon("language"),
                    style = "margin-right: 6px;"
                ),
                selectInput(
                    inputId  = "app_lang",
                    label    = NULL,
                    choices  = c("English" = "en", "Bahasa Indonesia" = "id"),
                    selected = "en",
                    width    = "180px"
                )
            )
        )
    ),
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            id = "sidebar_menu",
            shinydashboard::menuItemOutput("menu_home_out"),
            shinydashboard::menuItemOutput("menu_upload_out"),
            shinydashboard::menuItemOutput("menu_explore_out"),
            shinydashboard::menuItemOutput("menu_modeling_out"),
            shinydashboard::menuItemOutput("menu_spatial_out"),
            shinydashboard::menuItemOutput("update_menu"),
            shinydashboard::menuItemOutput("menu_results_out")
        )
    ),
    shinydashboard::dashboardBody(
        # -- Optional-dependency banner (rendered server-side) ----------------
        # Shows a yellow warning at the top of every page when the user
        # launched the app via run_sae_app() and one or more OPTIONAL
        # Suggests packages are not installed.  Disappears entirely when
        # everything is available.
        uiOutput("dep_banner"),

        shinydashboard::tabItems(
            
            # -- HOME TAB -----------------------------------------------------
            shinydashboard::tabItem(
                tabName = "home",
                # v0.5.0: content rendered server-side so it reacts to
                # language changes.  See output$home_content in the server.
                uiOutput("home_content")
            ),
            
            # -- DATA UPLOAD TAB -----------------------------------------------
            shinydashboard::tabItem(
                tabName = "data_upload",
                fluidRow(
                    shinydashboard::box(
                        title = "Upload CSV or Excel File", width = 12, solidHeader = TRUE, status = "primary",
                        tags$ul(
                            tags$li("Accepted formats: .csv, .xls, .xlsx."),
                            tags$li(
                                "The dataset must be in a ", tags$strong("tabular format"),
                                ", where each row represents an observation or area."
                            ),
                            tags$li("Ensure the data structure follows these guidelines:"),
                            tags$ul(
                                tags$li("The ", tags$strong("first row"), " must contain column names (headers)."),
                                tags$li(
                                    "Include a ", tags$strong("column for direct estimates"),
                                    " -- typically survey-based or observed values for each area."
                                ),
                                tags$li("Include one or more ", tags$strong("auxiliary variables"),
                                        " (predictors) that can help explain variation in the direct estimates."),
                                tags$li(
                                    "Optionally, include a ", tags$strong("unique identifier column"),
                                    " for each area (e.g., area code or name)."
                                ),
                                tags$li(tags$strong("Important notes on file formats:")),
                                tags$ul(
                                    tags$li(".csv files should use a comma (`,`) as the field separator and a period (`.`) for decimal numbers."),
                                    tags$li(".xlsx/.xls files might use a comma (`,`) as the decimal separator, especially in European-style regional settings."),
                                    tags$li("Make sure numeric columns are not interpreted as text due to incorrect decimal separators.")
                                )
                            ),
                            tags$li("The application will automatically detect ", tags$strong("missing values"),
                                    ", and you can choose a handling strategy during the modeling stage."),
                            tags$li("If spatial modeling is intended, you may also prepare a corresponding ",
                                    tags$strong("spatial weight matrix"), " (uploaded separately).")
                        ),
                        fileInput("data_file", "Upload file", accept = c(".csv", ".xlsx", ".xls", ".rda", ".RData")),
                        uiOutput("data_upload_error")
                    ),
                    shinydashboard::box(
                        title = "Data Preview", width = 12, solidHeader = TRUE, status = "primary",
                        DT::DTOutput("data_preview")
                    )
                )
            ),
            
            # -- DATA EXPLORATION TAB ------------------------------------------
            shinydashboard::tabItem(
                tabName = "data_exploration",
                fluidRow(
                    shinydashboard::box(
                        title = "Data Exploration", width = 12, solidHeader = TRUE, status = "primary",
                        tags$p("This section allows you to explore your dataset to better understand
                               the variables and their relationships before modeling."),
                        tags$ul(
                            tags$li(strong("Summary Statistics:"),
                                    " Displays basic descriptive statistics (mean, median, min, max, quartiles)
                                     for the selected numeric variable."),
                            tags$li(strong("Histogram:"),
                                    " Shows the frequency distribution and a density curve to visualize the
                                     shape and spread of a variable."),
                            tags$li(strong("Boxplot:"),
                                    " Provides a graphical summary highlighting the median, quartiles, and
                                     potential outliers."),
                            tags$li(
                                strong("Scatter Plot & Correlation:"),
                                " Visualizes the relationship between two variables with optional axis
                                  transformations. Four correlation metrics are computed:",
                                tags$ul(
                                    tags$li(strong("Pearson's r:"),
                                            " Measures linear association. Values close to \u00b11 indicate a strong
                                              linear relationship. The p-value tests H\u2080: no linear relationship."),
                                    tags$li(strong("Spearman's rho:"),
                                            " Non-parametric rank-based measure capturing monotonic relationships.
                                              The p-value tests H\u2080: no monotonic relationship."),
                                    tags$li(strong("Distance Correlation:"),
                                            " Captures both linear and nonlinear associations. A value of 0
                                              implies statistical independence. The p-value comes from a
                                              permutation test."),
                                    tags$li(strong("Maximal Information Coefficient (MIC):"),
                                            " Detects a wide variety of associations, linear and nonlinear.
                                              Ranges from 0 (independent) to 1 (perfect functional relationship).
                                              No direct p-value is available.")
                                )
                            )
                        ),
                        tags$p(em("Note: A low p-value (typically < 0.05) indicates strong evidence against
                                   the null hypothesis of no association."))
                    ),
                    shinydashboard::box(
                        title = "Summary Statistics", width = 12, solidHeader = TRUE, status = "primary",
                        selectInput("explore_var_summary", "Select Variable for Summary:", choices = NULL),
                        verbatimTextOutput("numeric_summary")
                    )
                ),
                fluidRow(
                    shinydashboard::box(
                        title = "Histogram", width = 6, solidHeader = TRUE, status = "primary",
                        selectInput("explore_var_hist", "Select Variable for Histogram:", choices = NULL),
                        plotOutput("histogram_plot")
                    ),
                    shinydashboard::box(
                        title = "Boxplot", width = 6, solidHeader = TRUE, status = "primary",
                        selectInput("explore_var_boxplot", "Select Variable for Boxplot:", choices = NULL),
                        plotOutput("boxplot_plot")
                    )
                ),
                fluidRow(
                    shinydashboard::box(
                        title = "Scatter Plot & Correlation", width = 12, solidHeader = TRUE, status = "primary",
                        fluidRow(
                            column(
                                width = 6,
                                selectInput("explore_var_scatter_x", "X-axis Variable:", choices = NULL),
                                selectInput("explore_transform_x", "X-axis Transformation:",
                                            choices = c("None" = "none", "Log" = "log", "Z-score" = "zscore"),
                                            selected = "none"),
                                selectInput("explore_var_scatter_color", "Color by (Categorical, optional):",
                                            choices = c("None" = ""), selected = "")
                            ),
                            column(
                                width = 6,
                                selectInput("explore_var_scatter_y", "Y-axis Variable:", choices = NULL),
                                selectInput("explore_transform_y", "Y-axis Transformation:",
                                            choices = c("None" = "none", "Log" = "log",
                                                        "Logit" = "logit", "Z-score" = "zscore"),
                                            selected = "none")
                            )
                        ),
                        uiOutput("correlation_results"),
                        plotOutput("scatter_plot_exploration")
                    )
                ),
                # -- Data Check (calls hbsaems::check_data) --------------------
                fluidRow(
                    shinydashboard::box(
                        title  = "Data Quality Check",
                        width  = 12, solidHeader = TRUE, status = "warning",
                        tags$p("Inspect the data for missing values and dimensional ",
                               "consistency before fitting the model. The check ",
                               "recommends an appropriate ",
                               tags$code("handle_missing"), " strategy."),
                        fluidRow(
                            column(4, selectInput("dc_response", "Response Variable",
                                                   choices = NULL)),
                            column(4, selectizeInput("dc_predictors", "Predictor Variable(s)",
                                                       choices = NULL, multiple = TRUE)),
                            column(4, selectInput("dc_sre", "Spatial Grouping (optional)",
                                                   choices = NULL))
                        ),
                        actionButton("run_data_check", "Run Data Check",
                                     icon = icon("check-circle"),
                                     class = "btn-primary"),
                        br(), br(),
                        verbatimTextOutput("data_check_output"),
                        uiOutput("data_check_recommendation")
                    )
                )
            ),
            
            # -- MODELING TAB --------------------------------------------------
            shinydashboard::tabItem(
                tabName = "modeling",
                tabsetPanel(
                    # Sub-tab: Model Configuration
                    tabPanel(
                        title = "Model Configuration",
                        fluidRow(
                            shinydashboard::box(
                                title = "Overview", width = 12, solidHeader = TRUE, status = "primary",
                                tags$p("Specify the variables and settings for hierarchical Bayesian modeling."),
                                tags$ul(
                                    tags$li(strong("Response Variable:"),
                                            " The outcome variable being modeled. Predicted using auxiliary
                                              variables and may follow a specific distribution (e.g., Lognormal, Beta)."),
                                    tags$li(strong("Auxiliary Variables:"),
                                            " Predictor variables that help explain variation in the response."),
                                    tags$li(strong("Nonlinear Variables:"),
                                            " Subset of auxiliary variables to be modelled with a smooth term
                                              (spline or Gaussian process) instead of a linear term."),
                                    tags$li(strong("Group Variables:"),
                                            " Variables that define the hierarchical (random-effect) grouping
                                              structure (e.g., area, region)."),
                                    tags$li(strong("Distribution Type:"),
                                            " The sampling distribution for the response variable
                                              (e.g., Lognormal-Lognormal, Beta-Logitnormal, Custom)."),
                                    tags$li(strong("Prior Settings:"),
                                            " Optionally apply a global-local shrinkage prior (Horseshoe or R2D2)
                                              to regression coefficients."),
                                    tags$li(strong("Spatial Modeling:"),
                                            " Optionally add spatial random effects (CAR or SAR)."),
                                    tags$li(strong("Missing Data Handling:"),
                                            " Choose a strategy for missing values in the response or
                                              auxiliary variables: deletion, multiple imputation (predictors only),
                                              or model-based imputation.")
                                )
                            ),
                            shinydashboard::box(
                                title = "Variable Selection", width = 12, solidHeader = TRUE, status = "primary",
                                column(6, selectInput("response_var", "Response Variable", choices = NULL)),
                                column(6, selectizeInput("linear_aux_vars", "Auxiliary Variables",
                                                         choices = NULL, multiple = TRUE)),
                                # Nonlinear variable selector
                                # Choices are populated dynamically from linear_aux_vars (see server observer)
                                column(6,
                                    selectizeInput(
                                        inputId  = "nonlinear_vars",
                                        label    = "Nonlinear Variables (optional)",
                                        choices  = NULL,
                                        multiple = TRUE,
                                        options  = list(
                                            placeholder = "Select variables for smooth (nonlinear) modelling...",
                                            plugins     = list("remove_button")
                                        )
                                    ),
                                    helpText(
                                        "Selected variables will be entered as",
                                        tags$code("s(var)"), "(spline) or",
                                        tags$code("gp(var)"), "(Gaussian process).",
                                        "All other auxiliary variables remain linear."
                                    )
                                ),
                                # Smooth-type and knot controls -- visible only when at least one NL var is selected
                                column(6,
                                    conditionalPanel(
                                        condition = "input.nonlinear_vars.length > 0",
                                        selectInput(
                                            inputId  = "nonlinear_type",
                                            label    = "Smooth Term Type",
                                            choices  = c(
                                                "Thin-plate regression spline  [s()]" = "spline",
                                                "Gaussian process              [gp()]" = "gp"
                                            ),
                                            selected = "spline"
                                        ),
                                        conditionalPanel(
                                            condition = "input.nonlinear_type == 'spline'",
                                            numericInput(
                                                inputId = "spline_k",
                                                label   = "Basis dimension k  (-1 = automatic)",
                                                value   = -1,
                                                min     = -1,
                                                step    = 1
                                            ),
                                            helpText(
                                                "k = -1 lets mgcv choose the number of basis functions automatically.",
                                                "Increase k to allow more flexible curves (requires more data)."
                                            )
                                        )
                                    )
                                ),
                                # Overlap warning -- shown when a variable is listed in both selectors
                                column(12, uiOutput("nonlinear_overlap_note")),
                                column(6, selectizeInput("re_groups", "Group Variables",
                                                         choices = NULL, multiple = TRUE)),
                                column(6, selectInput("sre_type", "Spatial Type",
                                                      choices = c("None" = "none", "SAR" = "sar", "CAR" = "car"))),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type == 'sar'",
                                    selectInput("sar_type", "SAR Type",
                                                choices = c("Lag" = "lag", "Error" = "error"), selected = "lag")
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type == 'car'",
                                    selectInput("car_type", "CAR Type",
                                                choices = c("eSCAR" = "escar", "eSICAR" = "esicar",
                                                            "ICAR" = "icar", "BYM2" = "bym2"), selected = "escar")
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type == 'sar' || input.sre_type == 'car'",
                                    selectInput("sre", "Spatial Random Effects", choices = NULL),
                                    radioButtons(
                                        "spatial_source",
                                        "Spatial Weight Source:",
                                        choices = c(
                                            "Upload pre-built matrix (.csv/.xlsx/.rda)" = "matrix",
                                            "Build from shapefile (.shp via sf)"        = "shp"
                                        ),
                                        selected = "matrix"
                                    ),
                                    conditionalPanel(
                                        condition = "input.spatial_source == 'matrix'",
                                        fileInput("spatial_weights",
                                                   "Upload Spatial Weights Matrix",
                                                   accept = c(".csv", ".xls", ".xlsx",
                                                              ".rda", ".RData"))
                                    ),
                                    conditionalPanel(
                                        condition = "input.spatial_source == 'shp'",
                                        fileInput("shp_file",
                                                   "Upload Shapefile (.shp + .shx + .dbf)",
                                                   accept = c(".shp", ".shx", ".dbf",
                                                              ".prj", ".cpg"),
                                                   multiple = TRUE),
                                        # Theoretical guidance hint
                                        tags$div(class = "alert alert-info",
                                            style = "padding: 8px; font-size: 90%;",
                                            tags$strong("Recommended for:"),
                                            tags$ul(style = "margin-bottom: 0;",
                                                tags$li(tags$strong("CAR"),
                                                    " (Besag 1974): type = queen or rook, style = B (binary)"),
                                                tags$li(tags$strong("SAR"),
                                                    " (Anselin 1988): type = knn or distance, style = W (row-standardised)")
                                            )
                                        ),
                                        selectInput("shp_type",
                                                     "Neighbour Type",
                                                     choices = c(
                                                         "Queen contiguity (CAR)" = "queen",
                                                         "Rook contiguity (CAR)"  = "rook",
                                                         "K-nearest (SAR)"        = "knn",
                                                         "Distance band (SAR)"    = "distance"
                                                     ),
                                                     selected = "queen"),
                                        selectInput("shp_style",
                                                     "Matrix Style",
                                                     choices = c(
                                                         "Binary 0/1 (CAR)"             = "B",
                                                         "Row-standardised (SAR)"        = "W"
                                                     ),
                                                     selected = "B"),
                                        conditionalPanel(
                                            condition = "input.shp_type == 'knn'",
                                            numericInput("shp_k", "k (neighbours)",
                                                          value = 4, min = 1, step = 1)
                                        ),
                                        conditionalPanel(
                                            condition = "input.shp_type == 'distance'",
                                            numericInput("shp_threshold", "Distance threshold",
                                                          value = NA, min = 0)
                                        ),
                                        textInput("shp_id_col",
                                                   "ID column (optional)",
                                                   value = ""),
                                        actionButton("build_spatial_btn",
                                                      textOutput("lbl_build_weight",
                                                                  inline = TRUE),
                                                      icon = icon("hammer"),
                                                      class = "btn-primary")
                                    )
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type != 'none'",
                                    uiOutput("spatial_weights_error"),
                                    # v0.4.0: theoretical diagnostic on demand
                                    conditionalPanel(
                                        condition = "input.sre_type == 'car' || input.sre_type == 'sar'",
                                        actionButton("show_spatial_diag",
                                                      textOutput("lbl_inspect_weight",
                                                                  inline = TRUE),
                                                      icon = icon("magnifying-glass-chart"),
                                                      class = "btn-default btn-sm"),
                                        verbatimTextOutput("spatial_diag_output")
                                    )
                                ))
                            ),
                            shinydashboard::box(
                                title = "Distribution Settings", width = 12, solidHeader = TRUE, status = "primary",
                                column(6, selectInput("dist_type", "Distribution Type",
                                                      choices = c("Custom", "Beta-Logitnormal",
                                                                  "Binomial-Logitnormal", "Lognormal-Lognormal"),
                                                      selected = "Custom")),
                                column(6, conditionalPanel(
                                    condition = "input.dist_type == 'Binomial-Logitnormal'",
                                    selectInput("trials_logit", "Trials Variable", choices = NULL)
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.dist_type == 'Beta-Logitnormal'",
                                    selectInput("n_beta", "Sample Size (n)", choices = NULL),
                                    selectInput("deff_beta", "Design Effect (deff)", choices = NULL)
                                )),
                                conditionalPanel(
                                    condition = "input.dist_type == 'Custom'",
                                    column(6, selectInput("hb_family", "HB Family", choices = c(
                                        "gaussian", "binomial", "poisson", "gamma", "beta", "lognormal",
                                        "skew_normal", "shifted_lognormal", "negbinomial", "beta_binomial",
                                        "logistic_normal", "hurdle_poisson", "hurdle_negbinomial",
                                        "hurdle_gamma", "hurdle_lognormal", "zero_inflated_beta",
                                        "zero_inflated_poisson", "zero_inflated_negbinomial",
                                        "zero_inflated_binomial", "zero_inflated_beta_binomial",
                                        "weibull", "frechet", "gen_extreme_value"
                                    ))),
                                    column(6, conditionalPanel(
                                        condition = "input.hb_family == 'binomial'",
                                        selectInput("trials_var", "Trials Variable", choices = NULL)
                                    )),
                                    column(6, selectInput("hb_link", "HB Link",
                                                          choices = c("identity", "log", "logit", "probit", "cloglog")))
                                )
                            ),
                            # Prior Settings box -- collapsed by default (advanced option)
                            shinydashboard::box(
                                title       = "Prior Settings for Regression Coefficients",
                                width       = 12,
                                solidHeader = TRUE,
                                status      = "info",
                                collapsible = TRUE,
                                collapsed   = TRUE,
                                fluidRow(
                                    column(4,
                                        selectInput(
                                            inputId  = "prior_type",
                                            label    = "Shrinkage Prior",
                                            choices  = c(
                                                "Default (brms defaults)" = "default",
                                                "Horseshoe"               = "horseshoe",
                                                "R2D2"                    = "r2d2"
                                            ),
                                            selected = "default"
                                        ),
                                        helpText(
                                            tags$strong("Horseshoe:"), "promotes sparsity; allows a few large signals.",
                                            tags$br(),
                                            tags$strong("R2D2:"), "places a prior on total model R\u00b2."
                                        )
                                    ),
                                    # Horseshoe parameter controls
                                    column(8,
                                        conditionalPanel(
                                            condition = "input.prior_type == 'horseshoe'",
                                            fluidRow(
                                                column(4,
                                                    numericInput("hs_df", "Local half-t df",
                                                                 value = 1, min = 0.5, step = 0.5),
                                                    helpText("df = 1 gives a half-Cauchy prior (recommended).")
                                                ),
                                                column(4,
                                                    numericInput("hs_df_global", "Global half-t df",
                                                                 value = 1, min = 0.5, step = 0.5)
                                                ),
                                                column(4,
                                                    numericInput("hs_df_slab", "Slab df",
                                                                 value = 4, min = 1, step = 1)
                                                ),
                                                column(4,
                                                    numericInput("hs_scale_slab", "Slab scale",
                                                                 value = 2, min = 0.1, step = 0.5)
                                                ),
                                                column(4,
                                                    numericInput("hs_par_ratio",
                                                                 label = "par_ratio (optional)",
                                                                 value = NA, min = 0, step = 0.05),
                                                    helpText("Expected proportion of non-zero coefficients.",
                                                             "Leave blank for automatic.")
                                                ),
                                                column(4,
                                                    numericInput("hs_scale_global",
                                                                 label = "Global scale (optional)",
                                                                 value = NA, min = 0, step = 0.1),
                                                    helpText("Leave blank to let brms compute automatically.")
                                                )
                                            )
                                        ),
                                        # R2D2 parameter controls
                                        conditionalPanel(
                                            condition = "input.prior_type == 'r2d2'",
                                            fluidRow(
                                                column(4,
                                                    sliderInput("r2d2_mean_R2",
                                                                label = "Prior mean R\u00b2",
                                                                min = 0.05, max = 0.95, value = 0.5, step = 0.05),
                                                    helpText("Expected proportion of variance explained by the predictors.")
                                                ),
                                                column(4,
                                                    numericInput("r2d2_prec_R2",
                                                                 label = "Precision of R\u00b2",
                                                                 value = 2, min = 0.5, step = 0.5),
                                                    helpText("Higher values concentrate mass around the prior mean.")
                                                ),
                                                column(4,
                                                    numericInput("r2d2_cons_D2",
                                                                 label = "Dirichlet concentration D2 (optional)",
                                                                 value = NA, min = 0.01, step = 0.1),
                                                    helpText("NULL corresponds to 0.5 (uniform simplex).",
                                                             "Leave blank for automatic.")
                                                )
                                            )
                                        )
                                    )
                                )
                            ),
                            # Missing data handling panel
                            conditionalPanel(
                                condition = "output.missing_detected == true",
                                shinydashboard::box(
                                    title = "Missing Data Handling", width = 12, solidHeader = TRUE, status = "warning",
                                    column(6, uiOutput("missing_vars_list")),
                                    conditionalPanel(
                                        condition = "output.missing_aux_response_detected == true",
                                        column(6, selectInput("missing_data_method", "Handle Missing Data",
                                                              choices = c("None"     = "none",
                                                                          "Deleted"  = "deleted",
                                                                          "Model"    = "model",
                                                                          "Multiple" = "multiple")))
                                    ),
                                    column(12, uiOutput("missing_data_note")),
                                    column(6, conditionalPanel(
                                        condition = "input.missing_data_method == 'multiple'",
                                        numericInput("m_value", "Number of Imputations", value = 5, min = 1)
                                    )),
                                    column(12, uiOutput("missing_re_warning")),
                                    column(12, uiOutput("missing_sre_warning")),
                                    column(12, uiOutput("missing_trials_var_error")),
                                    column(12, uiOutput("missing_trials_logit_error")),
                                    column(12, uiOutput("missing_n_beta_error")),
                                    column(12, uiOutput("missing_deff_beta_error"))
                                )
                            )
                        )
                    ),
                    
                    # Sub-tab: Prior Checking
                    tabPanel(
                        title = "Prior Checking",
                        fluidRow(
                            shinydashboard::box(
                                title = "Overview", width = 12, solidHeader = TRUE, status = "primary",
                                tags$ul(
                                    tags$li(
                                        p(
                                            "To specify a prior, use valid syntax supported by the ", strong("brms"), " package. Examples:",
                                            br(), code('prior(normal(0, 5), class = "b")'), " for fixed effects,",
                                            br(), code('prior(cauchy(0, 2), class = "sd")'), " for group-level standard deviations."
                                        )
                                    ),
                                    tags$li(p("Click ", strong("'Add Prior'"), " to include the prior; use ", strong("'Clear Priors'"), " to remove all."))
                                )
                            ),
                            shinydashboard::box(
                                title = "Prior Specification", width = 12, solidHeader = TRUE, status = "primary",
                                textInput("prior_input", "Enter Prior:", placeholder = 'prior(normal(0, 1), class = "b")'),
                                fluidRow(
                                    column(6, actionButton("add_prior",   "Add Prior",    icon = icon("plus"),  class = "btn-success")),
                                    column(6, actionButton("clear_priors","Clear Priors", icon = icon("trash"), class = "btn-danger"))
                                ),
                                br(),
                                verbatimTextOutput("prior_list_display")
                            ),
                            shinydashboard::box(
                                title = "Prior Predictive Check", width = 12, solidHeader = TRUE, status = "primary",
                                actionButton("run_prior_check", "Run Prior Predictive Check",
                                             icon = icon("play"), class = "btn-primary"),
                                br(), br(),
                                plotOutput("prior_predictive_plot")
                            )
                        )
                    ),
                    
                    # Sub-tab: MCMC Settings
                    tabPanel(
                        title = "MCMC Settings",
                        fluidRow(
                            shinydashboard::box(
                                title = "Sampling Configuration", width = 12, solidHeader = TRUE, status = "primary",
                                column(4, numericInput("num_seed",   "Seed",        value = 123,  min = 1)),
                                column(4, numericInput("num_chains", "Chains",      value = 4,    min = 1)),
                                column(4, numericInput("num_cores",  "Cores",       value = 1,    min = 1)),
                                column(4, numericInput("num_thin",   "Thinning Rate", value = 1,  min = 1)),
                                column(4, numericInput("num_iter",   "Iterations",  value = 4000, min = 1)),
                                column(4, numericInput("num_warmup", "Warm-up",     value = 2000, min = 1)),
                                column(4, sliderInput("adapt_delta", "Adapt Delta",
                                                      min = 0.8, max = 0.99, value = 0.95, step = 0.01)),
                                br(),
                                actionButton("fit_model", "Fit Model", icon = icon("play"), class = "btn-primary")
                            )
                        )
                    )
                )
            ),

            # -- SPATIAL SETUP TAB (v0.5.0) -----------------------------------
            # Dedicated tab for configuring and exploring the spatial
            # neighbourhood structure used by CAR/SAR random effects.
            # Provides a Source / Map / Matrix / Diagnostic / Reference
            # workflow.  The reactive `spatial_weights()` is the single
            # source of truth -- the Modeling tab reads from it.
            shinydashboard::tabItem(
                tabName = "spatial_setup",
                uiOutput("spatial_setup_body")
            ),

            # -- RESULTS TAB ---------------------------------------------------
            shinydashboard::tabItem(
                tabName = "results",
                tabsetPanel(
                    tabPanel(
                        "Model Summary",
                        shinydashboard::box(
                            title = "Model Summary", width = 12, solidHeader = TRUE, status = "primary",
                            verbatimTextOutput("model_summary")
                        )
                    ),
                    tabPanel(
                        "Convergence Diagnostics",
                        tabsetPanel(
                            tabPanel(
                                "Numerical Diagnostics",
                                fluidRow(
                                    column(3, wellPanel(
                                        shinyWidgets::pickerInput("diag_tests", "Select Diagnostic Tests:",
                                                                  choices = c("R-hat" = "rhat", "Geweke" = "geweke",
                                                                              "Raftery-Lewis" = "raftery",
                                                                              "Heidelberger-Welch" = "heidel"),
                                                                  multiple = TRUE, selected = "rhat")
                                    )),
                                    column(9, verbatimTextOutput("convergence_output"))
                                )
                            ),
                            tabPanel(
                                "Diagnostic Plots",
                                fluidRow(
                                    column(3, wellPanel(
                                        shinyWidgets::pickerInput("plot_types", "Select Diagnostic Plot Type:",
                                                                  choices = c(
                                                                      "Trace Plot"                     = "trace",
                                                                      "Density Plot"                   = "dens",
                                                                      "Autocorrelation (ACF) Plot"     = "acf",
                                                                      "NUTS Energy"                    = "nuts_energy",
                                                                      "R-hat Plot"                     = "rhat",
                                                                      "Effective Sample Size (ESS) Plot" = "neff"
                                                                  ),
                                                                  multiple = FALSE, selected = "trace")
                                    )),
                                    column(9,
                                        uiOutput("plot_definition"),
                                        plotOutput("diag_plots", height = "600px")
                                    )
                                )
                            )
                        )
                    ),
                    tabPanel(
                        "Model Checking (Goodness of Fit)",
                        value = "model_checking_panel",
                        tabsetPanel(
                            tabPanel(
                                "Numerical Checks",
                                fluidRow(
                                    column(3, wellPanel(
                                        shinyWidgets::pickerInput("gof_metrics_select", "Select Metric:",
                                                                  choices = c("LOO" = "loo", "WAIC" = "waic"),
                                                                  multiple = FALSE, selected = "loo"),
                                        actionButton("run_model_check", "Run Check",
                                                     icon = icon("play"), class = "btn-primary")
                                    )),
                                    column(9,
                                        uiOutput("metrics_definition_box"),
                                        verbatimTextOutput("model_check_output")
                                    )
                                )
                            ),
                            tabPanel(
                                "Posterior Predictive Check (PPC)",
                                fluidRow(
                                    column(3, wellPanel(
                                        shinyWidgets::pickerInput("pp_check_type_select", "Select PPC Plot Type:",
                                                                  choices = c(
                                                                      "Density Overlay"          = "dens_overlay",
                                                                      "Ribbon"                   = "ribbon",
                                                                      "Intervals"                = "intervals",
                                                                      "Error Scatter (Mean)"     = "error_scatter_avg",
                                                                      "Bar (Discrete)"           = "bars",
                                                                      "Scatter (y vs yrep)"      = "scatter_avg",
                                                                      "Statistic vs Data"        = "stat"
                                                                  ),
                                                                  multiple = FALSE, selected = "dens_overlay"),
                                        sliderInput("pp_samples_slider",
                                                    "Number of Posterior Predictive Samples:",
                                                    min = 10, max = 200, value = 50, step = 10),
                                        conditionalPanel(
                                            condition = "input.pp_check_type_select == 'stat'",
                                            selectInput("pp_check_stat_select", "Statistic for PPC:",
                                                        choices = c("mean", "median", "sd", "min", "max"),
                                                        selected = "mean")
                                        )
                                    )),
                                    column(9,
                                        uiOutput("ppc_definition_box"),
                                        plotOutput("model_check_plot_output", height = "600px")
                                    )
                                )
                            )
                        )
                    ),
                    tabPanel(
                        "Prior Sensitivity Analysis",
                        value = "prior_sensitivity_panel",
                        shinydashboard::box(
                            title = "Prior Sensitivity Analysis",
                            width = 12, solidHeader = TRUE, status = "primary",
                            tagList(
                                p(
                                    "This analysis evaluates how sensitive the posterior distribution is to
                                     the choice of prior. It uses power-scaling perturbations implemented in the ",
                                    code("priorsense"), " package."
                                ),
                                p(
                                    "Sensitivity is quantified using the ",
                                    strong("Cumulative Jensen-Shannon (CJS) distance"),
                                    ", a symmetric metric measuring divergence between cumulative distribution
                                     functions (CDFs). High CJS distance indicates prior sensitivity."
                                )
                            ),
                            fluidRow(
                                column(4,
                                    selectizeInput("sensitivity_params_results",
                                                   "Select Parameters for Sensitivity Analysis:",
                                                   choices = NULL, multiple = TRUE, options = list(maxItems = 5)),
                                    actionButton("run_sensitivity", "Run Sensitivity Analysis",
                                                 icon = icon("play"), class = "btn-primary")
                                ),
                                column(8,
                                    verbatimTextOutput("prior_sensitivity_output_results"),
                                    plotOutput("prior_sensitivity_plot_results", height = "400px")
                                )
                            )
                        )
                    ),
                    tabPanel(
                        "Prediction",
                        fluidRow(
                            shinydashboard::box(
                                title = "Data Prediction", width = 12, solidHeader = TRUE, status = "primary",
                                DT::DTOutput("prediction_results"),
                                downloadButton("download_results", "Download")
                            ),
                            shinydashboard::box(
                                title = "Input Data for Prediction", width = 12, solidHeader = TRUE, status = "primary",
                                actionButton("add_row",    "Add Row",    icon = icon("plus"),  class = "btn-success"),
                                actionButton("remove_row", "Remove Row", icon = icon("trash"), class = "btn-danger"),
                                br(), br(),
                                DT::DTOutput("prediction_input_table"),
                                br(), br(),
                                actionButton("predict_new", "Predict", icon = icon("play"), class = "btn-primary")
                            )
                        )
                    ),
                    # -- BENCHMARKING TAB ----------------------------------
                    tabPanel(
                        "Benchmark",
                        shinydashboard::box(
                            title  = "Benchmark Small-Area Estimates",
                            width  = 12, solidHeader = TRUE, status = "primary",
                            tags$p("Adjust area-level predictions so that their ",
                                   "weighted sum matches a known aggregate total ",
                                   "(e.g. official provincial or national figure)."),
                            fluidRow(
                                column(4, selectInput(
                                    "bm_method", "Benchmark Method",
                                    choices = c(
                                        "Ratio (multiplicative)" = "ratio",
                                        "Difference (additive)"  = "difference",
                                        "Raking (multi-target)"  = "raking"
                                    ),
                                    selected = "ratio")),
                                column(4, conditionalPanel(
                                    condition = "input.bm_method != 'raking'",
                                    numericInput("bm_target_single",
                                                  "Benchmark Total",
                                                  value = NA, min = 0)
                                )),
                                column(4, selectInput(
                                    "bm_weights_var",
                                    "Weights Variable (optional)",
                                    choices = c("Equal weights (1/n)" = ""),
                                    selected = ""))
                            ),
                            conditionalPanel(
                                condition = "input.bm_method == 'raking'",
                                tags$div(class = "alert alert-info",
                                    tags$strong("Multi-domain benchmarking. "),
                                    "Use this when each predicted area belongs to a ",
                                    "single higher-level domain and you have an ",
                                    "official total for each domain. ",
                                    tags$br(),
                                    tags$em("Example: SAE per kecamatan, benchmark to ",
                                            "kabupaten totals. The 'Group Variable' is ",
                                            "the kabupaten ID column; 'Group Targets' is ",
                                            "the vector of kabupaten totals in the same ",
                                            "order as ", tags$code("levels(group_var)"), ".")),
                                fluidRow(
                                    column(6, selectInput(
                                        "bm_groups_var",
                                        "Group Variable (e.g. 'kabupaten')",
                                        choices = NULL)),
                                    column(6, textInput(
                                        "bm_targets_multi",
                                        "Group Targets (comma-separated)",
                                        placeholder = "e.g. 110, 145, 145"))
                                ),
                                # Live preview of group levels + sums
                                verbatimTextOutput("bm_group_preview"),
                                actionButton("bm_template_targets",
                                              "Auto-fill targets with current group sums",
                                              icon = icon("magic"),
                                              class = "btn-default btn-sm"),
                                br(), br()
                            ),
                            actionButton("run_benchmark", "Apply Benchmark",
                                          icon = icon("balance-scale"),
                                          class = "btn-primary"),
                            br(), br(),
                            verbatimTextOutput("benchmark_summary"),
                            DT::DTOutput("benchmark_table"),
                            br(),
                            plotOutput("benchmark_plot", height = "400px"),
                            br(),
                            downloadButton("download_benchmark",
                                            "Download Benchmarked Predictions (CSV)")
                        )
                    ),
                    tabPanel(
                        "Save Outputs",
                        downloadButton("save_model",     "Save Model (RDS)"),
                        downloadButton("save_stan_code", "Save Stan Code"),
                        downloadButton("save_coda",      "Save CODA Samples"),
                        downloadButton("save_plots",     "Save Plots (PDF)")
                    )
                )
            )
        )
    )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

    # -- Dependency banner -----------------------------------------------------
    # Reads the missing-optional list set by run_sae_app() via getOption().
    # When the app is launched directly (e.g. shiny::runApp() instead of
    # run_sae_app()), the option is NULL and the banner is hidden.
    output$dep_banner <- renderUI({
        miss <- getOption("hbsaems.shiny_missing_optional", default = NULL)
        if (is.null(miss) || length(miss) == 0L) return(NULL)

        # Re-check live so a manual install during the session clears the
        # banner without needing to relaunch.
        still_missing <- miss[
            !vapply(miss, requireNamespace, logical(1L), quietly = TRUE)
        ]
        if (length(still_missing) == 0L) {
            options(hbsaems.shiny_missing_optional = character(0L))
            return(NULL)
        }

        cmd <- sprintf(
            'install.packages(c(%s))',
            paste(sprintf('"%s"', still_missing), collapse = ", ")
        )

        div(
            class = "alert alert-warning",
            style = "margin: 10px 15px;",
            tags$h4(
                tags$i(class = "fa fa-exclamation-triangle"),
                "  Some optional packages are missing"),
            tags$p(
                "The application will continue to work, but the panels ",
                "depending on the following packages will be disabled or ",
                "fall back to simpler alternatives:"),
            tags$ul(lapply(still_missing, tags$li)),
            tags$p(tags$strong("To enable all features, run:")),
            tags$pre(cmd),
            tags$p(
                tags$em(
                    "After installing, restart R and call ",
                    tags$code("hbsaems::run_sae_app()"),
                    " again."))
        )
    })

    # -- Reactive values -------------------------------------------------------
    data            <- reactiveVal()
    new_data        <- reactiveVal(NULL)
    spatial_weights <- reactiveVal(NULL)
    model_fit       <- reactiveVal(NULL)
    prior_fit       <- reactiveVal(NULL)
    prior_list      <- reactiveVal(character())
    pred_data       <- reactiveVal(NULL)
    check_results   <- reactiveVal(NULL)
    ppc_result      <- reactiveVal(NULL)

    # -- v0.5.0: Bilingual support (en / id) ------------------------------------
    # Active language is read from the header dropdown.  Helper `t_()`
    # is a closure that wraps tr() with the current language so call
    # sites stay compact:   t_("menu_home")  ->  "Home" or "Beranda"
    lang <- reactive({
        l <- input$app_lang
        if (is.null(l) || !(l %in% hbsaems::tr_langs())) "en" else l
    })
    t_ <- function(key) hbsaems::tr(key, lang())

    # App title (in header)
    output$ui_app_title <- renderText({ t_("app_title") })

    # Sidebar menu items (re-rendered when language changes)
    output$menu_home_out <- shinydashboard::renderMenu({
        shinydashboard::menuItem(t_("menu_home"), tabName = "home",
                                  icon = icon("home"))
    })
    output$menu_upload_out <- shinydashboard::renderMenu({
        shinydashboard::menuItem(
            t_("box_data_upload"), tabName = "data_upload",
            icon = icon("upload"))
    })
    output$menu_explore_out <- shinydashboard::renderMenu({
        shinydashboard::menuItem(
            t_("menu_data"), tabName = "data_exploration",
            icon = icon("search"))
    })
    output$menu_modeling_out <- shinydashboard::renderMenu({
        shinydashboard::menuItem(t_("menu_modeling"), tabName = "modeling",
                                  icon = icon("cogs"))
    })
    output$menu_spatial_out <- shinydashboard::renderMenu({
        shinydashboard::menuItem(t_("menu_spatial"), tabName = "spatial_setup",
                                  icon = icon("globe"))
    })
    output$menu_results_out <- shinydashboard::renderMenu({
        shinydashboard::menuItem(t_("menu_results"), tabName = "results",
                                  icon = icon("chart-bar"))
    })

    # Reactive labels for action buttons (used by textOutput inside
    # actionButton labels above)
    output$lbl_build_weight   <- renderText({ t_("btn_build_weight")   })
    output$lbl_inspect_weight <- renderText({ t_("btn_inspect_weight") })

    # ==========================================================================
    # SPATIAL SETUP TAB (v0.5.0)
    # ==========================================================================
    # Three reactive outputs power this tab:
    #   * spatial_weights()       -- the W matrix (already exists, reused)
    #   * spatial_shape_obj()     -- the loaded sf polygon object (NEW)
    #   * spatial_neighbours()    -- spdep neighbour list (NEW, derived)
    # All visualisations and summaries read from these reactives, so the
    # tab stays consistent with whatever the user has built so far.

    # Helper: derive a degree vector (#neighbours per area) from W
    .deg_from_W <- function(M) {
        if (is.null(M) || !is.matrix(M)) return(NULL)
        # Count nonzero off-diagonal entries per row
        rowSums(M != 0, na.rm = TRUE)
    }

    # Main render: 4-tab layout
    output$spatial_setup_body <- renderUI({
        # v0.5.0: Status indicator — adapts to whether source is shape/matrix/none
        status_box <- {
            shape <- spatial_shape_obj()
            M     <- spatial_weights()
            if (!is.null(shape)) {
                # Source = shapefile
                shinydashboard::valueBox(
                    value    = paste0(nrow(shape), " areas"),
                    subtitle = t_("spatial_status_shape"),
                    icon     = icon("draw-polygon"),
                    color    = "green",
                    width    = 12
                )
            } else if (!is.null(M) && is.matrix(M)) {
                # Source = pre-built matrix
                shinydashboard::valueBox(
                    value    = paste0(nrow(M), " x ", ncol(M)),
                    subtitle = t_("spatial_status_matrix"),
                    icon     = icon("th"),
                    color    = "blue",
                    width    = 12
                )
            } else {
                # No source yet
                shinydashboard::valueBox(
                    value    = "—",
                    subtitle = t_("spatial_status_none"),
                    icon     = icon("circle-info"),
                    color    = "yellow",
                    width    = 12
                )
            }
        }

        fluidRow(
            shinydashboard::box(
                title = t_("spatial_intro_title"),
                width = 12, solidHeader = TRUE, status = "info",
                p(t_("spatial_intro_text"))
            ),
            # Status indicator
            status_box,
            column(12,
                tabBox(
                    width = 12,
                    # --- Sub-tab 1: Source -----------------------------------
                    tabPanel(
                        title = t_("spatial_subtab_source"),
                        icon  = icon("upload"),
                        fluidRow(
                            column(6,
                                shinydashboard::box(
                                    title = t_("spatial_box_upload"),
                                    width = 12, solidHeader = TRUE,
                                    status = "primary",
                                    radioButtons(
                                        "spatial_source_main",
                                        label = NULL,
                                        choices = setNames(
                                            c("matrix", "shp"),
                                            c("Upload pre-built matrix (.csv/.xlsx/.rda)",
                                              "Build from shapefile (.shp via sf)")
                                        ),
                                        selected = "shp"
                                    ),
                                    conditionalPanel(
                                        condition = "input.spatial_source_main == 'matrix'",
                                        fileInput("spatial_weights_main",
                                                   "Upload Weight Matrix",
                                                   accept = c(".csv", ".xls", ".xlsx",
                                                              ".rda", ".RData"))
                                    ),
                                    conditionalPanel(
                                        condition = "input.spatial_source_main == 'shp'",
                                        fileInput("shp_file_main",
                                                   "Upload Shapefile (.shp + .shx + .dbf)",
                                                   accept = c(".shp", ".shx", ".dbf",
                                                              ".prj", ".cpg"),
                                                   multiple = TRUE)
                                    )
                                )
                            ),
                            column(6,
                                shinydashboard::box(
                                    title = t_("spatial_box_params"),
                                    width = 12, solidHeader = TRUE,
                                    status = "primary",
                                    conditionalPanel(
                                        condition = "input.spatial_source_main == 'shp'",
                                        selectInput("shp_type_main",
                                                     "Neighbour Type",
                                                     choices = c(
                                                         "Queen contiguity (CAR)" = "queen",
                                                         "Rook contiguity (CAR)"  = "rook",
                                                         "K-nearest (SAR)"        = "knn",
                                                         "Distance band (SAR)"    = "distance"
                                                     ),
                                                     selected = "queen"),
                                        selectInput("shp_style_main",
                                                     "Matrix Style",
                                                     choices = c(
                                                         "Binary 0/1 (CAR)"        = "B",
                                                         "Row-standardised (SAR)"   = "W"
                                                     ),
                                                     selected = "B"),
                                        conditionalPanel(
                                            condition = "input.shp_type_main == 'knn'",
                                            numericInput("shp_k_main",
                                                          "k (neighbours)",
                                                          value = 4, min = 1, step = 1)
                                        ),
                                        conditionalPanel(
                                            condition = "input.shp_type_main == 'distance'",
                                            numericInput("shp_threshold_main",
                                                          "Distance threshold",
                                                          value = NA, min = 0)
                                        ),
                                        textInput("shp_id_col_main",
                                                   "ID column (optional)",
                                                   value = "")
                                    ),
                                    actionButton("build_spatial_main",
                                                  t_("btn_build_weight"),
                                                  icon = icon("hammer"),
                                                  class = "btn-primary"),
                                    br(), br(),
                                    div(class = "alert alert-info",
                                        style = "padding: 8px; font-size: 90%;",
                                        tags$strong("Recommended for:"),
                                        tags$ul(style = "margin-bottom: 0;",
                                            tags$li(tags$strong("CAR"),
                                                " (Besag 1974): type = queen or rook, style = B"),
                                            tags$li(tags$strong("SAR"),
                                                " (Anselin 1988): type = knn or distance, style = W")
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    # --- Sub-tab 2: Map preview -------------------------------
                    tabPanel(
                        title = t_("spatial_subtab_map"),
                        icon  = icon("map"),
                        # v0.5.0: Adaptive content -- show map only when a
                        # shapefile is loaded; show an informative banner
                        # when the user has uploaded a pre-built matrix
                        # (no geometry to display).
                        fluidRow(
                            column(12,
                                if (is.null(spatial_shape_obj()) &&
                                    !is.null(spatial_weights())) {
                                    # Matrix source -- map not applicable
                                    div(class = "alert alert-warning",
                                        style = "margin-top: 10px;",
                                        icon("triangle-exclamation"),
                                        tags$strong(" Map preview unavailable. "),
                                        t_("spatial_map_unavailable"))
                                } else {
                                    # Either shape loaded or nothing loaded:
                                    # show the plot (which renders its own
                                    # "no shape" fallback if needed).
                                    shinydashboard::box(
                                        title = t_("spatial_subtab_map"),
                                        width = 12, solidHeader = TRUE,
                                        status = "primary",
                                        plotOutput("spatial_map_plot",
                                                    height = "500px")
                                    )
                                }
                            )
                        )
                    ),
                    # --- Sub-tab 3: Weight matrix -----------------------------
                    tabPanel(
                        title = t_("spatial_subtab_matrix"),
                        icon  = icon("th"),
                        fluidRow(
                            column(7,
                                shinydashboard::box(
                                    title = t_("spatial_box_heatmap"),
                                    width = 12, solidHeader = TRUE,
                                    status = "primary",
                                    plotOutput("spatial_heatmap",
                                                height = "450px")
                                )
                            ),
                            column(5,
                                shinydashboard::box(
                                    title = t_("spatial_box_summary"),
                                    width = 12, solidHeader = TRUE,
                                    status = "info",
                                    verbatimTextOutput("spatial_summary")
                                ),
                                shinydashboard::box(
                                    title = t_("spatial_box_degree"),
                                    width = 12, solidHeader = TRUE,
                                    status = "primary",
                                    plotOutput("spatial_degree_plot",
                                                height = "200px")
                                )
                            )
                        )
                    ),
                    # --- Sub-tab 4: Diagnostic --------------------------------
                    tabPanel(
                        title = t_("spatial_subtab_diagnostic"),
                        icon  = icon("stethoscope"),
                        fluidRow(
                            column(6,
                                shinydashboard::box(
                                    title = t_("spatial_box_diag"),
                                    width = 12, solidHeader = TRUE,
                                    status = "primary",
                                    verbatimTextOutput("spatial_diag_main")
                                )
                            ),
                            column(6,
                                shinydashboard::box(
                                    title = t_("spatial_box_advisor"),
                                    width = 12, solidHeader = TRUE,
                                    status = "warning",
                                    htmlOutput("spatial_advisor")
                                )
                            )
                        )
                    ),
                    # --- Sub-tab 5: Reference ---------------------------------
                    tabPanel(
                        title = t_("spatial_subtab_reference"),
                        icon  = icon("book"),
                        shinydashboard::box(
                            title = t_("spatial_box_reference"),
                            width = 12, solidHeader = TRUE,
                            status = "info",
                            h4("CAR (Conditional Autoregressive)"),
                            p(t_("spatial_ref_car")),
                            tags$pre(style = "background-color: #f5f5f5;",
                                "s ~ N(0, sigma^2 * (D - rho*W)^{-1})\n\n",
                                "Requirements:\n",
                                "  - W is square, symmetric, binary (0/1)\n",
                                "  - Zero diagonal\n",
                                "  - Connected graph (no isolated areas)\n"),
                            h4("SAR (Simultaneous Autoregressive)"),
                            p(t_("spatial_ref_sar")),
                            tags$pre(style = "background-color: #f5f5f5;",
                                "s = rho*W*s + epsilon, epsilon ~ N(0, sigma^2*I)\n\n",
                                "Requirements:\n",
                                "  - W is square, row-standardised (each row sums to 1)\n",
                                "  - Zero diagonal\n",
                                "  - Symmetry NOT required\n"),
                            h4("References"),
                            tags$ul(
                                tags$li("Besag, J. (1974). Spatial Interaction and the Statistical Analysis of Lattice Systems. JRSS-B 36(2), 192-225."),
                                tags$li("Whittle, P. (1954). On Stationary Processes in the Plane. Biometrika 41(3/4), 434-449."),
                                tags$li("Anselin, L. (1988). Spatial Econometrics: Methods and Models. Kluwer."),
                                tags$li("Morris, M. et al. (2019). Bayesian Hierarchical Spatial Models: Implementing the Besag York Mollie Model in Stan.")
                            )
                        )
                    )
                )
            )
        )
    })

    # -- Spatial reactives: shapefile object + neighbours list ------------------
    # Triggered when user clicks "Build Weight Matrix" in the spatial tab.
    spatial_shape_obj <- reactiveVal(NULL)

    observeEvent(input$build_spatial_main, {
        req(input$spatial_source_main)
        src <- input$spatial_source_main

        if (src == "shp") {
            files <- input$shp_file_main
            if (is.null(files) || nrow(files) == 0L) {
                showNotification("Please upload shapefile files first.",
                                  type = "warning")
                return(invisible(NULL))
            }
            # Move files to a temp dir so sf can read the .shp
            tmp_dir <- tempfile("shp_main_")
            dir.create(tmp_dir)
            for (i in seq_len(nrow(files)))
                file.copy(files$datapath[i],
                          file.path(tmp_dir, files$name[i]))
            shp_path <- list.files(tmp_dir, pattern = "\\.shp$",
                                    full.names = TRUE)[1L]
            if (is.na(shp_path)) {
                showNotification(".shp file not found in upload.",
                                  type = "error")
                return(invisible(NULL))
            }
            shape <- tryCatch(sf::st_read(shp_path, quiet = TRUE),
                              error = function(e) e)
            if (inherits(shape, "error")) {
                showNotification(sprintf("Cannot read shapefile: %s",
                                          shape$message), type = "error")
                return(invisible(NULL))
            }
            spatial_shape_obj(shape)

            M <- tryCatch(
                hbsaems::build_spatial_weight(
                    shp       = shape,
                    type      = input$shp_type_main,
                    style     = input$shp_style_main,
                    k         = input$shp_k_main %||% 4L,
                    threshold = if (is.na(input$shp_threshold_main))
                                  NULL else input$shp_threshold_main,
                    id_col    = if (nzchar(input$shp_id_col_main %||% ""))
                                  input$shp_id_col_main else NULL,
                    for_model = NULL
                ),
                error = function(e) e
            )
            if (inherits(M, "error")) {
                showNotification(sprintf("build_spatial_weight failed: %s",
                                          M$message), type = "error")
                return(invisible(NULL))
            }
            spatial_weights(M)
            showNotification(sprintf("Weight matrix built (%d x %d).",
                                      nrow(M), ncol(M)),
                              type = "message")
        } else {
            # Matrix upload via the main tab
            up <- input$spatial_weights_main
            if (is.null(up)) {
                showNotification("Please upload a weight matrix file.",
                                  type = "warning")
                return(invisible(NULL))
            }
            ext <- tolower(tools::file_ext(up$name))
            M <- tryCatch({
                if (ext == "csv") {
                    as.matrix(utils::read.csv(up$datapath,
                                                row.names = 1))
                } else if (ext %in% c("xls", "xlsx")) {
                    as.matrix(readxl::read_excel(up$datapath))
                } else if (ext %in% c("rda", "rdata")) {
                    env <- new.env(); load(up$datapath, envir = env)
                    obj <- get(ls(env)[1L], envir = env)
                    if (is.matrix(obj)) obj else as.matrix(obj)
                } else {
                    stop("Unsupported file type: ", ext)
                }
            }, error = function(e) e)
            if (inherits(M, "error")) {
                showNotification(sprintf("Could not load matrix: %s",
                                          M$message), type = "error")
                return(invisible(NULL))
            }
            spatial_weights(M)
            spatial_shape_obj(NULL)
            showNotification(sprintf("Weight matrix loaded (%d x %d).",
                                      nrow(M), ncol(M)),
                              type = "message")
        }
    })

    # -- Visualisations ---------------------------------------------------------

    output$spatial_map_plot <- renderPlot({
        shape <- spatial_shape_obj()
        if (is.null(shape)) {
            plot.new()
            text(0.5, 0.5, t_("spatial_no_shape"), cex = 1.2)
            return(invisible(NULL))
        }
        M <- spatial_weights()
        if (!is.null(M)) {
            # Compute degree per polygon and colour by it
            deg <- .deg_from_W(M)
            if (length(deg) == nrow(shape)) {
                shape$.degree <- deg
                p <- ggplot2::ggplot(shape) +
                    ggplot2::geom_sf(ggplot2::aes(fill = .data$.degree),
                                      colour = "white", linewidth = 0.3) +
                    ggplot2::scale_fill_viridis_c(
                        name = "Neighbours", option = "C") +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(
                        title = "Polygon Map (coloured by neighbour count)"
                    )
                return(p)
            }
        }
        # Fallback: plain map without colour
        ggplot2::ggplot(shape) +
            ggplot2::geom_sf(fill = "lightblue", colour = "white",
                              linewidth = 0.3) +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = "Polygon Map")
    })

    output$spatial_heatmap <- renderPlot({
        M <- spatial_weights()
        if (is.null(M) || !is.matrix(M)) {
            plot.new()
            text(0.5, 0.5, t_("spatial_no_matrix"), cex = 1.2)
            return(invisible(NULL))
        }
        n <- nrow(M)
        # For very large matrices, downsample to keep render fast
        if (n > 200L) {
            idx <- round(seq(1L, n, length.out = 200L))
            M <- M[idx, idx]
            n <- 200L
        }
        df <- expand.grid(i = seq_len(n), j = seq_len(n))
        df$w <- as.vector(M)
        ggplot2::ggplot(df, ggplot2::aes(.data$j, .data$i,
                                          fill = .data$w)) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_viridis_c(name = "W") +
            ggplot2::scale_y_reverse() +
            ggplot2::coord_fixed() +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = NULL, y = NULL,
                           title = sprintf("W matrix (%d x %d)",
                                            nrow(spatial_weights()),
                                            ncol(spatial_weights())))
    })

    output$spatial_degree_plot <- renderPlot({
        M <- spatial_weights()
        deg <- .deg_from_W(M)
        if (is.null(deg)) {
            plot.new()
            text(0.5, 0.5, t_("spatial_no_matrix"), cex = 1)
            return(invisible(NULL))
        }
        ggplot2::ggplot(data.frame(deg = deg),
                         ggplot2::aes(x = .data$deg)) +
            ggplot2::geom_histogram(
                bins = max(5L, min(20L, length(unique(deg)))),
                fill = "#3c8dbc", colour = "white") +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "Number of neighbours",
                           y = "Count of areas",
                           title = NULL)
    })

    output$spatial_summary <- renderPrint({
        M <- spatial_weights()
        if (is.null(M) || !is.matrix(M)) {
            cat(t_("spatial_no_matrix"), "\n")
            return(invisible(NULL))
        }
        n   <- nrow(M)
        deg <- .deg_from_W(M)
        nbr_total <- sum(M != 0, na.rm = TRUE)
        dens <- nbr_total / (n * (n - 1L))
        cat(t_("spatial_n_areas"),     ":", n,                       "\n")
        cat(t_("spatial_n_links"),     ":", nbr_total,                "\n")
        cat(t_("spatial_density"),     ":",
            sprintf("%.4f", dens),                                     "\n")
        cat(t_("spatial_mean_degree"), ":",
            sprintf("%.2f", mean(deg, na.rm = TRUE)),                  "\n")
        cat(t_("spatial_max_degree"),  ":", max(deg, na.rm = TRUE),   "\n")
        cat(t_("spatial_isolated"),    ":", sum(deg == 0L),           "\n")
        # Detected style
        is_binary <- all(M %in% c(0, 1), na.rm = TRUE)
        is_rownorm <- all(abs(rowSums(M, na.rm = TRUE) - 1) < 1e-8 |
                            rowSums(M, na.rm = TRUE) == 0)
        style_str <- if (is_binary) "B (binary)" else
                     if (is_rownorm) "W (row-standardised)" else
                     "Other"
        cat(t_("spatial_style_detected"), ":", style_str, "\n")
    })

    output$spatial_diag_main <- renderPrint({
        M <- spatial_weights()
        if (is.null(M) || !is.matrix(M)) {
            cat(t_("spatial_no_matrix"), "\n")
            return(invisible(NULL))
        }
        sre_t <- input$sre_type
        if (is.null(sre_t) || !(sre_t %in% c("car", "sar"))) {
            cat("Run checks for: CAR\n\n")
            sre_t <- "car"
        }
        chk <- tryCatch(
            hbsaems::check_spatial_weight(M, sre_type = sre_t,
                                            verbose = FALSE),
            error = function(e) NULL
        )
        if (is.null(chk)) {
            cat("Could not run check_spatial_weight().\n")
            return(invisible(NULL))
        }
        print(chk)
    })

    output$spatial_advisor <- renderUI({
        M <- spatial_weights()
        if (is.null(M) || !is.matrix(M)) {
            return(p(em(t_("spatial_no_matrix"))))
        }
        # Detect style
        is_symmetric <- isSymmetric(unname(M))
        is_binary    <- all(M %in% c(0, 1), na.rm = TRUE)
        rs           <- rowSums(M, na.rm = TRUE)
        is_rownorm   <- all(abs(rs - 1) < 1e-8 | rs == 0)

        recommend <- if (is_symmetric && is_binary) {
            tags$strong(style = "color: green;",
                        "Compatible with CAR (Besag 1974)")
        } else if (is_rownorm) {
            tags$strong(style = "color: green;",
                        "Compatible with SAR (Anselin 1988)")
        } else {
            tags$strong(style = "color: orange;",
                        "Atypical structure -- check style manually")
        }

        tagList(
            p("Based on the matrix properties:"),
            tags$ul(
                tags$li(tags$strong("Symmetric: "),
                        if (is_symmetric) "Yes" else "No"),
                tags$li(tags$strong("Binary 0/1: "),
                        if (is_binary) "Yes" else "No"),
                tags$li(tags$strong("Row-standardised: "),
                        if (is_rownorm) "Yes" else "No")
            ),
            recommend
        )
    })

    # Home tab content -- re-renders when language changes
    output$home_content <- renderUI({
        # v0.5.0: bilingual; falls back to English via tr() if a key missing.
        # Cache the current language in a local variable for clarity.
        intro_struct <- if (lang() == "id") list(
            home = "Beranda: Gambaran umum dan tujuan aplikasi.",
            up   = "Unggah Data: Unggah dan pratinjau dataset (CSV/Excel), deteksi missing values.",
            mdl  = "Pemodelan: Spesifikasi struktur model, prior, pengaturan MCMC, dan fitting model.",
            res  = "Hasil: Review diagnostik model, ringkasan, dan output prediksi."
        ) else list(
            home = "Home: Overview and purpose of the application.",
            up   = "Data Upload: Upload and preview datasets (CSV/Excel), detect missing values.",
            mdl  = "Modeling: Specify model structure, priors, MCMC settings, and fit the model.",
            res  = "Results: Review model diagnostics, summaries, and prediction outputs."
        )

        fluidRow(
            shinydashboard::box(
                title = t_("box_intro"), width = 12,
                solidHeader = TRUE, status = "primary",
                p(t_("text_intro_p1")),
                p(t_("text_intro_p2"))
            ),
            shinydashboard::box(
                title = if (lang() == "id") "Struktur Aplikasi"
                        else                "App Structure",
                width = 12, solidHeader = TRUE, status = "primary",
                tags$ul(
                    tags$li(intro_struct$home),
                    tags$li(intro_struct$up),
                    tags$li(intro_struct$mdl),
                    tags$li(intro_struct$res)
                )
            ),
            shinydashboard::box(
                title = t_("box_workflow"), width = 12,
                solidHeader = TRUE, status = "primary",
                tags$ol(
                    tags$li(t_("step_1")),
                    tags$li(t_("step_2")),
                    tags$li(t_("step_3")),
                    tags$li(t_("step_4")),
                    tags$li(t_("step_5")),
                    tags$li(t_("step_6"))
                )
            ),
            shinydashboard::box(
                title  = t_("box_families"), width = 12,
                solidHeader = TRUE, status = "info",
                tags$p(t_("text_families_help")),
                verbatimTextOutput("families_summary"),
                tags$p(tags$em(t_("text_families_advanced")))
            )
        )
    })
    spatial_diag    <- reactiveVal(NULL)

    # -- v0.4.0: Show registered families on Home tab ------------------------
    output$families_summary <- renderPrint({
        fams <- list_hbsae_models()
        cat("Registered families (", length(fams), "):\n", sep = "")
        for (k in fams) {
            spec <- get_hbsae_model(k)
            tag  <- if (isTRUE(spec$discrete))           "[discrete] "
                    else                                 "[continuous]"
            mi   <- if (isTRUE(spec$supports_mi))        " mi=TRUE"
                    else                                 " mi=FALSE"
            aux  <- if (is.function(spec$aux_param_hyperprior))
                                                          " +aux"
                    else                                 ""
            cat(sprintf("  %-30s %s link=%-9s%s%s\n",
                        k, tag, spec$link %||% "?", mi, aux))
        }
    })

    # -- v0.4.0: On-demand spatial diagnostic display ------------------------
    # Triggered by [Inspect Weight Matrix] button under spatial weights UI.
    # Renders the full check_spatial_weight() output so users can verify
    # theoretical compatibility (CAR/Besag, SAR/Anselin) at a glance.
    observeEvent(input$show_spatial_diag, {
        M  <- spatial_weights()
        st <- input$sre_type
        if (is.null(M) || !is.matrix(M)) {
            spatial_diag("No weight matrix loaded.")
            return(invisible(NULL))
        }
        if (is.null(st) || !(st %in% c("car", "sar"))) {
            spatial_diag("Choose sre_type = 'car' or 'sar' first.")
            return(invisible(NULL))
        }
        chk <- tryCatch(
            check_spatial_weight(M, sre_type = st, verbose = FALSE),
            error = function(e) NULL
        )
        if (is.null(chk)) {
            spatial_diag("Could not run check_spatial_weight().")
            return(invisible(NULL))
        }
        spatial_diag(chk)
    })

    output$spatial_diag_output <- renderPrint({
        d <- spatial_diag()
        if (is.null(d)) {
            cat("Click [Inspect Weight Matrix] to see CAR/SAR theoretical ",
                "diagnostics.\n", sep = "")
            return(invisible(NULL))
        }
        if (is.character(d)) {
            cat(d, "\n")
            return(invisible(NULL))
        }
        # d is an hbsaems_spatial_check object
        print(d)
    })
    
    # -- Dynamic "Update Model" menu item --------------------------------------
    observe({
        if (!is.null(model_fit())) {
            output$update_menu <- shinydashboard::renderMenu({
                shinydashboard::menuItem(
                    if (lang() == "id") "Perbarui Model" else "Update Model",
                    tabName = "update_model", icon = icon("sync"))
            })
            if (!"update_model" %in% sapply(shiny::isolate(input$tabs), as.character)) {
                insertUI(
                    selector = ".tab-content",
                    where    = "beforeEnd",
                    ui = shinydashboard::tabItem(
                        tabName = "update_model",
                        fluidRow(
                            shinydashboard::box(
                                title = "Update Model", width = 12, solidHeader = TRUE, status = "primary",
                                fileInput("update_newdata", "Upload New Data (optional):",
                                          accept = c("text/csv", ".csv", ".xlsx", ".xls", ".rda", ".RData")),
                                numericInput("update_num_chains", "Chains",     value = 1,    min = 1),
                                numericInput("update_num_cores",  "Cores",      value = 1,    min = 1),
                                numericInput("update_num_iter",   "Iterations", value = 4000, min = 1),
                                numericInput("update_num_warmup", "Warm-up",    value = 2000, min = 1),
                                sliderInput("update_adapt_delta", "Adapt Delta",
                                            min = 0.8, max = 0.99, value = 0.95),
                                actionButton("update_run", "Run Model Update",
                                             icon = icon("refresh"), class = "btn-primary")
                            )
                        )
                    )
                )
            }
        } else {
            output$update_menu <- shinydashboard::renderMenu(NULL)
        }
    })
    
    
    # -- DATA UPLOAD -----------------------------------------------------------
    observeEvent(input$data_file, {
        req(input$data_file)
        ext <- tools::file_ext(input$data_file$name)
        output$data_upload_error <- renderUI(NULL)
        withProgress(message = "Uploading data...", {
            tryCatch(
                withCallingHandlers(
                    {
                        df <- if (ext %in% c("xls", "xlsx")) {
                            readxl::read_excel(input$data_file$datapath)
                        } else if (ext == "csv") {
                            utils::read.csv(input$data_file$datapath, header = TRUE, sep = ",")
                        } else if (ext %in% c("rda", "RData")) {
                            env <- new.env()
                            load(input$data_file$datapath, envir = env)
                            obj_names <- ls(env)
                            if (length(obj_names) == 0) stop("No objects found in the file.")
                            df_candidates <- obj_names[sapply(obj_names, function(n) is.data.frame(env[[n]]))]
                            if (length(df_candidates) == 0) stop("No data frame found in the file.")
                            env[[df_candidates[1]]]
                        } else {
                            stop("Unsupported file format.")
                        }
                        df <- as.data.frame(df)
                        data(df)
                        output$data_preview <- DT::renderDT({
                            DT::datatable(df, options = list(scrollX = TRUE, scrollY = "370px",
                                                             pageLength = 10))
                        })
                        all_vars     <- names(df)
                        numeric_vars <- all_vars[sapply(df, is.numeric)]
                        updateSelectInput(session, "explore_var_summary",       choices = numeric_vars)
                        updateSelectInput(session, "explore_var_hist",           choices = numeric_vars)
                        updateSelectInput(session, "explore_var_boxplot",        choices = numeric_vars)
                        updateSelectInput(session, "explore_var_scatter_x",      choices = numeric_vars)
                        updateSelectInput(session, "explore_var_scatter_y",      choices = numeric_vars)
                        updateSelectInput(session, "explore_var_scatter_color",  choices = c("None" = "", all_vars))
                        updateSelectInput(session, "response_var",               choices = all_vars)
                        updateSelectizeInput(session, "linear_aux_vars",         choices = all_vars, server = TRUE)
                        updateSelectizeInput(session, "re_groups",               choices = all_vars, server = TRUE)
                        updateSelectInput(session, "trials_var",   choices = c("none", all_vars))
                        updateSelectInput(session, "sre",          choices = c("none", all_vars))
                        updateSelectInput(session, "trials_logit", choices = c("none", all_vars))
                        updateSelectInput(session, "n_beta",       choices = c("none", all_vars))
                        updateSelectInput(session, "deff_beta",    choices = c("none", all_vars))
                        showNotification("Data uploaded successfully!", type = "message", duration = 5)
                    },
                    warning = function(w) {
                        showNotification(paste("Upload warning:", conditionMessage(w)),
                                         type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) { invokeRestart("muffleMessage") }
                ),
                error = function(e) {
                    showNotification(paste("Data Upload Error:", e$message), type = "error", duration = 10)
                    data(NULL)
                    output$data_preview <- DT::renderDT(NULL)
                    output$data_upload_error <- renderUI(
                        div(class = "alert alert-danger", paste("Upload Error:", e$message))
                    )
                    for (id in c("explore_var_summary", "explore_var_hist", "explore_var_boxplot",
                                 "explore_var_scatter_x", "explore_var_scatter_y",
                                 "explore_var_scatter_color", "response_var", "trials_var",
                                 "sre", "trials_logit", "n_beta", "deff_beta")) {
                        updateSelectInput(session, id, choices = NULL)
                    }
                    for (id in c("linear_aux_vars", "re_groups", "sensitivity_params_results")) {
                        updateSelectizeInput(session, id, choices = NULL, server = TRUE)
                    }
                }
            )
        })
    })
    
    
    # -- RESET OBSERVERS -------------------------------------------------------
    observeEvent(input$dist_type, {
        updateSelectInput(session, "trials_var",   selected = "none")
        updateSelectInput(session, "trials_logit", selected = "none")
        updateSelectInput(session, "n_beta",       selected = "none")
        updateSelectInput(session, "deff_beta",    selected = "none")
    })
    observeEvent(input$sre_type, {
        updateSelectInput(session, "sre", selected = "none")

        # v0.4.0+: auto-suggest shp_type / shp_style based on theory.
        # User can still override afterwards (this only fires on sre_type change).
        if (isTRUE(input$sre_type == "car")) {
            updateSelectInput(session, "shp_type",  selected = "queen")
            updateSelectInput(session, "shp_style", selected = "B")
        } else if (isTRUE(input$sre_type == "sar")) {
            updateSelectInput(session, "shp_type",  selected = "knn")
            updateSelectInput(session, "shp_style", selected = "W")
        }
    })
    observeEvent(input$hb_family, {
        updateSelectInput(session, "trials_var", selected = "none")
    })
    
    # -- NONLINEAR VARS: sync choices with linear_aux_vars --------------------
    # When the user changes the auxiliary variable selection, the nonlinear
    # selector is refreshed to contain only the currently selected aux vars.
    # Previously selected nonlinear vars no longer in aux_vars are dropped.
    observe({
        req(input$linear_aux_vars)
        updateSelectizeInput(
            session,
            inputId  = "nonlinear_vars",
            choices  = input$linear_aux_vars,
            selected = intersect(
                isolate(input$nonlinear_vars),
                input$linear_aux_vars
            )
        )
    })
    
    
    # -- DATA EXPLORATION: Summary ---------------------------------------------
    output$numeric_summary <- renderPrint({
        req(data(), input$explore_var_summary)
        summary(data()[[input$explore_var_summary]])
    })
    
    # -- DATA EXPLORATION: Histogram -------------------------------------------
    output$histogram_plot <- renderPlot({
        req(data(), input$explore_var_hist)
        var <- input$explore_var_hist
        p <- ggplot2::ggplot(data(), ggplot2::aes(x = .data[[var]])) +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                    bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
            ggplot2::geom_density(alpha = 0.2, fill = "#FF6666") +
            ggplot2::labs(title = paste("Histogram of", var), x = var, y = "Density") +
            ggplot2::theme_minimal()
        withCallingHandlers(print(p),
            warning = function(w) {
                showNotification(paste("Histogram warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) { invokeRestart("muffleMessage") }
        )
    })
    
    # -- DATA EXPLORATION: Boxplot ---------------------------------------------
    output$boxplot_plot <- renderPlot({
        req(data(), input$explore_var_boxplot)
        var <- input$explore_var_boxplot
        p <- ggplot2::ggplot(data(), ggplot2::aes(y = .data[[var]])) +
            ggplot2::geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
            ggplot2::labs(title = paste("Boxplot of", var), y = var) +
            ggplot2::theme_minimal() +
            ggplot2::coord_flip()
        withCallingHandlers(print(p),
            warning = function(w) {
                showNotification(paste("Boxplot warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) { invokeRestart("muffleMessage") }
        )
    })
    
    # -- DATA EXPLORATION: Scatter -- data transformation -----------------------
    scatter_data_transformed <- reactive({
        req(data(), input$explore_var_scatter_x, input$explore_var_scatter_y)
        df      <- data()
        x_col   <- input$explore_var_scatter_x
        y_col   <- input$explore_var_scatter_y
        epsilon <- 1e-6
        
        if (input$explore_transform_y == "log") {
            if (any(df[[y_col]] <= 0, na.rm = TRUE))
                showNotification("Some Y values \u2264 0; replaced with a small positive value before log transform.",
                                 type = "message", duration = 10)
            df[[paste0(y_col, "_log")]] <- log(pmax(df[[y_col]], epsilon))
            y_col <- paste0(y_col, "_log")
        } else if (input$explore_transform_y == "logit") {
            if (any(df[[y_col]] <= 0 | df[[y_col]] >= 1, na.rm = TRUE))
                showNotification("Y values outside (0, 1) adjusted before logit transform.",
                                 type = "message", duration = 10)
            df[[paste0(y_col, "_logit")]] <- stats::qlogis(pmin(pmax(df[[y_col]], epsilon), 1 - epsilon))
            y_col <- paste0(y_col, "_logit")
        } else if (input$explore_transform_y == "zscore") {
            mu <- mean(df[[y_col]], na.rm = TRUE)
            sg <- stats::sd(df[[y_col]], na.rm = TRUE)
            df[[paste0(y_col, "_std")]] <- (df[[y_col]] - mu) / sg
            y_col <- paste0(y_col, "_std")
        }
        
        if (input$explore_transform_x == "log") {
            if (any(df[[x_col]] <= 0, na.rm = TRUE))
                showNotification("Some X values \u2264 0; replaced with a small positive value before log transform.",
                                 type = "message", duration = 10)
            df[[paste0(x_col, "_log")]] <- log(pmax(df[[x_col]], epsilon))
            x_col <- paste0(x_col, "_log")
        } else if (input$explore_transform_x == "zscore") {
            mu <- mean(df[[x_col]], na.rm = TRUE)
            sg <- stats::sd(df[[x_col]], na.rm = TRUE)
            df[[paste0(x_col, "_std")]] <- (df[[x_col]] - mu) / sg
            x_col <- paste0(x_col, "_std")
        }
        list(data = df, x = x_col, y = y_col)
    })
    
    # -- DATA EXPLORATION: Scatter plot ----------------------------------------
    output$scatter_plot_exploration <- renderPlot({
        s <- scatter_data_transformed()
        req(s$data, s$x, s$y)
        if (s$x == s$y &&
            input$explore_transform_x == "none" &&
            input$explore_transform_y == "none") {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, "Select different variables or apply a transformation.", cex = 1.2)
            return()
        }
        p <- ggplot2::ggplot(s$data, ggplot2::aes(x = .data[[s$x]], y = .data[[s$y]]))
        color_var <- input$explore_var_scatter_color
        if (!is.null(color_var) && nzchar(color_var)) {
            p <- p +
                ggplot2::geom_point(ggplot2::aes(color = as.factor(.data[[color_var]])),
                                    alpha = 0.7, size = 2.5) +
                ggplot2::labs(color = color_var)
        } else {
            p <- p + ggplot2::geom_point(color = "darkcyan", alpha = 0.6, size = 2.5)
        }
        p <- p +
            ggplot2::geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
            ggplot2::labs(title = paste("Scatter Plot:", s$y, "vs", s$x), x = s$x, y = s$y) +
            ggplot2::theme_minimal()
        withCallingHandlers(suppressMessages(print(p)),
            warning = function(w) {
                showNotification(paste("Scatter Plot warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) { invokeRestart("muffleMessage") }
        )
    })
    
    # -- DATA EXPLORATION: Correlation -----------------------------------------
    # Pearson, Spearman, Distance Correlation, MIC.
    # Chatterjee's Xi (XICOR) has been removed to reduce external dependencies.
    output$correlation_results <- renderUI({
        s_data_list <- scatter_data_transformed()
        req(s_data_list$data, s_data_list$y, s_data_list$x)
        
        df_corr <- s_data_list$data
        y_vec   <- df_corr[[s_data_list$y]]
        x_vec   <- df_corr[[s_data_list$x]]
        ok      <- stats::complete.cases(x_vec, y_vec)
        x_vec   <- x_vec[ok]; y_vec <- y_vec[ok]
        
        if (length(x_vec) < 3L)
            return(h4("Not enough complete observations to compute correlations."))
        if (s_data_list$x == s_data_list$y &&
            input$explore_transform_x == "none" &&
            input$explore_transform_y == "none")
            return(h4("Please select different variables or apply a transformation."))
        
        results <- list()
        
        results[["Pearson's r"]] <- tryCatch({
            r <- stats::cor.test(x_vec, y_vec, method = "pearson")
            c(sprintf("%.4f", r$estimate), sprintf("%.4f", r$p.value))
        }, error = function(e) c("Error", e$message))
        
        results[["Spearman's rho"]] <- tryCatch({
            r <- suppressWarnings(stats::cor.test(x_vec, y_vec, method = "spearman", exact = FALSE))
            c(sprintf("%.4f", r$estimate), sprintf("%.4f", r$p.value))
        }, error = function(e) c("Error", e$message))
        
        results[["Distance Correlation"]] <- tryCatch({
            dc <- energy::dcor.test(x_vec, y_vec, R = 199)
            c(sprintf("%.4f", dc$estimate[[1L]]), sprintf("%.4f", dc$p.value))
        }, error = function(e) c("Error", e$message))
        
        results[["Maximal Information Coefficient (MIC)"]] <- tryCatch({
            mic <- minerva::mine(x_vec, y_vec)
            c(sprintf("%.4f", mic$MIC), "N/A")
        }, error = function(e) c("Error", e$message))
        
        df_out <- data.frame(
            Metric    = names(results),
            Value     = vapply(results, `[[`, character(1L), 1L),
            `p-value` = vapply(results, `[[`, character(1L), 2L),
            check.names = FALSE
        )
        output$correlation_table <- renderTable(df_out)
        tagList(
            h5(paste0("Correlation: ", s_data_list$y, " vs ", s_data_list$x)),
            tableOutput("correlation_table")
        )
    })
    
    
    # -- MISSING DATA CHECKING -------------------------------------------------
    observe({
        req(data(), input$dist_type)
        df             <- data()
        available_vars <- names(df)
        
        observeEvent(
            c(input$response_var, input$linear_aux_vars, input$nonlinear_vars,
              input$trials_var, input$trials_logit, input$n_beta,
              input$deff_beta, input$re_groups, input$sre),
            {
                selected_vars <- unique(c(
                    input$response_var, input$linear_aux_vars, input$nonlinear_vars,
                    input$trials_var, input$trials_logit, input$n_beta,
                    input$deff_beta, input$re_groups, input$sre
                ))
                selected_vars <- selected_vars[selected_vars %in% available_vars]
                
                missing_info <- vapply(selected_vars, function(col) {
                    sum(is.na(df[[col]]), na.rm = TRUE)
                }, integer(1L))
                missing_info <- missing_info[missing_info > 0L]
                
                missing_aux_response <- intersect(
                    names(missing_info),
                    c(input$response_var, input$linear_aux_vars, input$nonlinear_vars)
                )
                missing_re            <- intersect(names(missing_info), input$re_groups)
                missing_sre           <- intersect(names(missing_info), input$sre)
                missing_trials_var    <- intersect(names(missing_info), input$trials_var)
                missing_trials_logit  <- intersect(names(missing_info), input$trials_logit)
                missing_n_beta        <- intersect(names(missing_info), input$n_beta)
                missing_deff_beta     <- intersect(names(missing_info), input$deff_beta)
                
                output$missing_detected <- reactive({ length(missing_info) > 0L })
                outputOptions(output, "missing_detected", suspendWhenHidden = FALSE)
                output$missing_aux_response_detected <- reactive({ length(missing_aux_response) > 0L })
                outputOptions(output, "missing_aux_response_detected", suspendWhenHidden = FALSE)
                
                output$missing_vars_list <- renderUI({
                    if (length(missing_info) > 0L)
                        div(h5("Variables with Missing Values:"),
                            HTML(paste0("<ul>",
                                        paste(sprintf("<li>%s [%d missing]</li>",
                                                      names(missing_info), missing_info), collapse = ""),
                                        "</ul>")))
                })
                
                output$missing_data_note <- renderUI({
                    method <- input$missing_data_method
                    response_has_na <- input$response_var %in% names(missing_info)
                    if (is.null(method) || method == "none") return(NULL)
                    switch(method,
                        deleted  = div(class = "alert alert-info",
                                       strong("Deletion (complete-case analysis):"),
                                       " Rows where the response variable is missing are removed before
                              model fitting. All predictor (X) variables must be complete; if
                              any predictor has missing values, please use 'Multiple' instead."),
                        multiple = div(class = "alert alert-warning",
                                       strong("Multiple Imputation \u2014 predictors (X) only:"),
                                       " Multiple imputation via ", code("mice"), " is applied exclusively
                              to ", strong("auxiliary predictor variables"), ".",
                                       if (response_has_na) tagList(
                                           br(), strong("\u26a0 Warning: "),
                                           " The response variable has missing values but will ",
                                           strong("NOT"), " be imputed. In a Bayesian model, missing
                                      outcomes are naturally integrated out through the posterior
                                      predictive distribution. Imputing Y would introduce
                                      unjustified bias. Rows with missing Y will be excluded from
                                      model fitting but retained for prediction via sae_predict()."
                                       )),
                        model    = div(class = "alert alert-info",
                                       strong("Model-based imputation (mi()):"),
                                       " Missing values in predictors are estimated jointly with the model
                              parameters using the ", code("mi()"), " function from ",
                                       code("brms"), ". Only available for continuous outcomes."),
                        NULL
                    )
                })
                
                output$missing_re_warning <- renderUI({
                    if (length(missing_re) > 0L)
                        div(class = "alert alert-warning", strong("Warning:"),
                            " Missing values in group variable(s): ", paste(missing_re, collapse = ", "), ".")
                })
                output$missing_sre_warning <- renderUI({
                    if (length(missing_sre) > 0L)
                        div(class = "alert alert-warning", strong("Warning:"),
                            " Missing values in spatial variable(s): ", paste(missing_sre, collapse = ", "), ".")
                })
                output$missing_trials_var_error <- renderUI({
                    if (length(missing_trials_var) > 0L)
                        div(class = "alert alert-danger", strong("Error:"),
                            " Missing values in trials variable: ", paste(missing_trials_var, collapse = ", "), ".")
                })
                output$missing_trials_logit_error <- renderUI({
                    if (length(missing_trials_logit) > 0L)
                        div(class = "alert alert-danger", strong("Error:"),
                            " Missing values in logit trials variable: ", paste(missing_trials_logit, collapse = ", "), ".")
                })
                output$missing_n_beta_error <- renderUI({
                    if (length(missing_n_beta) > 0L)
                        div(class = "alert alert-danger", strong("Error:"),
                            " Missing values in sample size variable (n): ", paste(missing_n_beta, collapse = ", "), ".")
                })
                output$missing_deff_beta_error <- renderUI({
                    if (length(missing_deff_beta) > 0L)
                        div(class = "alert alert-danger", strong("Error:"),
                            " Missing values in design effect variable (deff): ", paste(missing_deff_beta, collapse = ", "), ".")
                })
                
                default_method <- if (input$dist_type %in% c("Beta-Logitnormal", "Binomial-Logitnormal"))
                    "multiple" else "model"
                updateSelectInput(session, "missing_data_method", selected = default_method)
            }
        )
    })
    
    
    # -- SPATIAL WEIGHTS UPLOAD ------------------------------------------------
    observeEvent(input$spatial_weights, {
        req(input$spatial_weights)
        ext <- tools::file_ext(input$spatial_weights$name)
        tryCatch({
            M <- if (ext %in% c("xls", "xlsx")) {
                as.matrix(readxl::read_excel(input$spatial_weights$datapath, col_names = FALSE))
            } else {
                as.matrix(utils::read.csv(input$spatial_weights$datapath, header = FALSE))
            }
            spatial_weights(M)
            output$spatial_weights_error <- renderUI(
                div(class = "alert alert-success", "Spatial weights matrix uploaded successfully.")
            )
        }, error = function(e) {
            output$spatial_weights_error <- renderUI(
                div(class = "alert alert-danger", paste("Error reading spatial weights:", e$message))
            )
        })
    })
    
    
    # -- PRIORS ----------------------------------------------------------------
    observeEvent(input$add_prior, {
        req(input$prior_input)
        prior_list(c(prior_list(), trimws(input$prior_input)))
        updateTextInput(session, "prior_input", value = "")
    })
    observeEvent(input$clear_priors, { prior_list(character()) })
    
    output$prior_list_display <- renderPrint({
        p <- prior_list()
        if (length(p) == 0L) cat("No priors specified (brms defaults will be used).\n")
        else cat(paste0(seq_along(p), ". ", p, collapse = "\n"), "\n")
    })
    
    reactive_prior <- reactive({
        p <- prior_list()
        if (length(p) == 0L) return(NULL)
        tryCatch(
            do.call(c, lapply(p, function(x) eval(parse(text = x)))),
            error = function(e) {
                showNotification(paste("Prior parsing error:", e$message), type = "error", duration = 10)
                NULL
            }
        )
    })
    
    
    # -- FORMULA CONSTRUCTION --------------------------------------------------
    reactive_formula <- reactive({
        req(data(), input$response_var)
        output$model_fitting_error <- renderUI(NULL)
        
        y_var    <- input$response_var
        aux_vars <- input$linear_aux_vars
        nl_vars  <- input$nonlinear_vars %||% character(0L)
        lin_vars <- setdiff(aux_vars, nl_vars)
        nl_type  <- input$nonlinear_type %||% "spline"
        nl_k     <- { k <- suppressWarnings(as.integer(input$spline_k)); if (is.na(k)) -1L else k }
        
        make_nl_term <- function(v) {
            if (nl_type == "spline") {
                if (nl_k == -1L) paste0("s(", v, ")") else paste0("s(", v, ", k = ", nl_k, ")")
            } else {
                paste0("gp(", v, ")")
            }
        }
        
        if (!is.null(input$missing_data_method) && input$missing_data_method == "model") {
            d         <- data()
            all_vars  <- c(y_var, aux_vars)
            missing_v <- all_vars[sapply(d[all_vars], function(x) any(is.na(x)))]
            resp_lhs  <- if (y_var %in% missing_v) paste0(y_var, " | mi()") else y_var
            lin_terms <- vapply(lin_vars, function(v)
                if (v %in% missing_v) paste0("mi(", v, ")") else v, character(1L))
            nl_terms  <- vapply(nl_vars, function(v) {
                inner <- if (v %in% missing_v) paste0("mi(", v, ")") else v
                if (nl_type == "spline") {
                    if (nl_k == -1L) paste0("s(", inner, ")") else paste0("s(", inner, ", k = ", nl_k, ")")
                } else paste0("gp(", inner, ")")
            }, character(1L))
            all_terms   <- c(lin_terms, nl_terms)
            rhs         <- if (length(all_terms) > 0L) paste(all_terms, collapse = " + ") else "1"
            formula_str <- paste(resp_lhs, "~", rhs)
        } else {
            nl_terms    <- vapply(nl_vars, make_nl_term, character(1L))
            all_terms   <- c(lin_vars, nl_terms)
            rhs         <- if (length(all_terms) > 0L) paste(all_terms, collapse = " + ") else "1"
            formula_str <- paste(y_var, "~", rhs)
        }
        
        if (!is.null(input$hb_family) && input$hb_family == "binomial") {
            req(input$trials_var)
            formula_str <- paste0(y_var, " | trials(", input$trials_var, ") ~ ",
                                  sub(paste0(y_var, " ~ "), "", formula_str, fixed = TRUE))
        }
        brms::bf(stats::as.formula(formula_str))
    })
    
    reactive_formula_re <- reactive({
        if (!is.null(input$re_groups) && length(input$re_groups) > 0L)
            stats::as.formula(paste("~", paste(paste0("(1 | ", input$re_groups, ")"), collapse = " + ")))
        else NULL
    })
    
    # -- SHRINKAGE PRIOR ARGUMENT COLLECTOR ------------------------------------
    get_prior_args <- function() {
        pt <- input$prior_type
        if (is.null(pt) || pt == "default") return(list(prior_type = "default"))
        if (pt == "horseshoe") {
            return(list(
                prior_type      = "horseshoe",
                hs_df           = as.numeric(input$hs_df),
                hs_df_global    = as.numeric(input$hs_df_global),
                hs_df_slab      = as.numeric(input$hs_df_slab),
                hs_scale_slab   = as.numeric(input$hs_scale_slab),
                hs_scale_global = if (!is.na(input$hs_scale_global)) as.numeric(input$hs_scale_global) else NULL,
                hs_par_ratio    = if (!is.na(input$hs_par_ratio))    as.numeric(input$hs_par_ratio)    else NULL
            ))
        }
        if (pt == "r2d2") {
            return(list(
                prior_type   = "r2d2",
                r2d2_mean_R2 = as.numeric(input$r2d2_mean_R2),
                r2d2_prec_R2 = as.numeric(input$r2d2_prec_R2),
                r2d2_cons_D2 = if (!is.na(input$r2d2_cons_D2)) as.numeric(input$r2d2_cons_D2) else NULL
            ))
        }
        list(prior_type = "default")
    }
    
    # -- MODEL FITTING ---------------------------------------------------------
    fit_model_function <- function(sample_prior_mode = "no") {
        nl_vars <- input$nonlinear_vars %||% character(0L)
        nl_type <- input$nonlinear_type %||% "spline"
        nl_k    <- { k <- suppressWarnings(as.integer(input$spline_k)); if (is.na(k)) -1L else k }
        
        common_args <- list(
            data           = data(),
            handle_missing = if (input$missing_data_method == "none") NULL else input$missing_data_method,
            m              = input$m_value,
            chains         = input$num_chains,
            iter           = input$num_iter,
            warmup         = input$num_warmup,
            cores          = input$num_cores,
            seed           = input$num_seed,
            sample_prior   = sample_prior_mode,
            sre            = if (input$sre_type == "none") NULL else input$sre,
            sre_type       = if (input$sre_type == "none") NULL else input$sre_type,
            car_type       = if (input$sre_type == "car")  input$car_type else NULL,
            sar_type       = if (input$sre_type == "sar")  input$sar_type else NULL,
            M              = if (input$sre_type != "none" && !is.null(spatial_weights()))
                               as.matrix(spatial_weights()) else NULL,
            control        = list(adapt_delta = input$adapt_delta),
            refresh        = 0L
        )
        common_args <- c(common_args, get_prior_args())
        
        if (input$dist_type == "Custom") {
            common_args$re          <- reactive_formula_re()
            common_args$formula     <- reactive_formula()
            common_args$hb_sampling <- input$hb_family
            common_args$hb_link     <- input$hb_link
            fit <- do.call(hbm, common_args)
            
        } else if (input$dist_type == "Lognormal-Lognormal") {
            common_args$response   <- input$response_var
            common_args$predictors <- setdiff(input$linear_aux_vars, nl_vars)
            common_args$group      <- input$re_groups
            if (length(nl_vars) > 0L) {
                common_args$nonlinear      <- nl_vars
                common_args$nonlinear_type <- nl_type
                common_args$spline_k       <- nl_k
            }
            fit <- do.call(hbm_lnln, common_args)
            
        } else if (input$dist_type == "Binomial-Logitnormal") {
            common_args$trials     <- if (input$trials_logit == "none") NULL else input$trials_logit
            common_args$response   <- input$response_var
            common_args$predictors <- setdiff(input$linear_aux_vars, nl_vars)
            common_args$group      <- input$re_groups
            if (length(nl_vars) > 0L) {
                common_args$nonlinear      <- nl_vars
                common_args$nonlinear_type <- nl_type
                common_args$spline_k       <- nl_k
            }
            fit <- do.call(hbm_binlogitnorm, common_args)
            
        } else if (input$dist_type == "Beta-Logitnormal") {
            common_args$n          <- if (input$n_beta    == "none") NULL else input$n_beta
            common_args$deff       <- if (input$deff_beta == "none") NULL else input$deff_beta
            common_args$response   <- input$response_var
            common_args$predictors <- setdiff(input$linear_aux_vars, nl_vars)
            common_args$group      <- input$re_groups
            if (length(nl_vars) > 0L) {
                common_args$nonlinear      <- nl_vars
                common_args$nonlinear_type <- nl_type
                common_args$spline_k       <- nl_k
            }
            fit <- do.call(hbm_betalogitnorm, common_args)
        }
        return(fit)
    }
    
    observeEvent(input$fit_model, {
        req(input$fit_model)
        withProgress(message = "Fitting model...", {
            tryCatch(
                withCallingHandlers(
                    {
                        invisible(capture.output(
                            suppressMessages(fit <- fit_model_function(sample_prior_mode = "no"))
                        ))
                        model_fit(fit)
                        showNotification("Model fitting completed!", type = "message", duration = 10)
                        fit_params <- tryCatch(
                            colnames(brms::as_draws_matrix(fit$model))[
                                !startsWith(colnames(brms::as_draws_matrix(fit$model)), "r_") &
                                    !startsWith(colnames(brms::as_draws_matrix(fit$model)), "lp")
                            ],
                            error = function(e) character()
                        )
                        updateSelectizeInput(session, "sensitivity_params_results",
                                             choices = fit_params, server = TRUE)
                    },
                    warning = function(w) {
                        showNotification(paste("Fitting warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) { invokeRestart("muffleMessage") }
                ),
                error = function(e) {
                    showNotification(paste("Fitting Error:", e$message), type = "error", duration = 15)
                    output$model_fitting_error <- renderUI(
                        div(class = "alert alert-danger", paste("Model Error:", e$message))
                    )
                }
            )
        })
    })
    
    
    # -- PRIOR PREDICTIVE CHECK ------------------------------------------------
    observeEvent(input$run_prior_check, {
        req(data(), input$response_var)
        withProgress(message = "Running prior predictive check...", {
            tryCatch(
                withCallingHandlers(
                    {
                        invisible(capture.output(
                            suppressMessages(fit_prior <- fit_model_function(sample_prior_mode = "only"))
                        ))
                        prior_fit(fit_prior)
                        # NOTE: rename local var to avoid shadowing prior_check()
                        # function. v0.3.0 prior_check() requires the `data`
                        # argument explicitly (was implicit in hbpc()).
                        pc_result <- prior_check(
                            model        = fit_prior,
                            data         = data(),
                            response_var = input$response_var
                        )
                        showNotification("Prior check completed!", type = "message", duration = 10)
                        output$prior_predictive_plot <- renderPlot({
                            withCallingHandlers(print(pc_result$prior_predictive_plot),
                                warning = function(w) {
                                    showNotification(paste("Prior plot warning:", conditionMessage(w)),
                                                     type = "warning", duration = 10)
                                    invokeRestart("muffleWarning")
                                },
                                message = function(m) { invokeRestart("muffleMessage") }
                            )
                        })
                    },
                    warning = function(w) {
                        showNotification(paste("Prior check warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) { invokeRestart("muffleMessage") }
                ),
                error = function(e) {
                    showNotification(paste("Prior Check Error:", e$message), type = "error", duration = 15)
                }
            )
        })
    })
    
    
    # -- MODEL SUMMARY ---------------------------------------------------------
    output$model_summary <- renderPrint({
        req(model_fit())
        summary(model_fit())
    })
    
    
    # -- CONVERGENCE DIAGNOSTICS -----------------------------------------------
    output$convergence_output <- renderPrint({
        req(model_fit(), input$diag_tests)
        withCallingHandlers(
            { res <- convergence_check(model_fit(), diag_tests = input$diag_tests); print(res) },
            warning = function(w) {
                showNotification(paste("Diagnostics warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) { invokeRestart("muffleMessage") }
        )
    })
    
    
    # -- DIAGNOSTIC PLOTS -----------------------------------------------------
    output$plot_definition <- renderUI({
        switch(input$plot_types,
               "trace"       = shinydashboard::box(title = "Trace Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                   p("Trace plots show sampled parameter values across MCMC iterations for each chain. Well-mixed, stationary chains indicate good convergence.")),
               "dens"        = shinydashboard::box(title = "Density Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                   p("Density plots show the posterior distribution of each parameter. Overlapping densities across chains indicate good mixing.")),
               "acf"         = shinydashboard::box(title = "Autocorrelation (ACF) Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                   p("ACF plots show autocorrelation at increasing lags. Low autocorrelation at small lags indicates efficient sampling.")),
               "nuts_energy" = shinydashboard::box(title = "NUTS Energy Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                   p("Energy transition plots visualize NUTS sampler efficiency. Large discrepancies between energy and momentum can indicate step-size problems.")),
               "rhat"        = shinydashboard::box(title = "R-hat Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                   p("R-hat values close to 1.00 indicate convergence. Values above 1.05 suggest chains have not fully mixed.")),
               "neff"        = shinydashboard::box(title = "Effective Sample Size (ESS) Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                   p("ESS reflects how many independent draws are equivalent to the correlated MCMC samples. Higher values indicate better sampling efficiency."))
        )
    })
    
    output$diag_plots <- renderPlot({
        req(model_fit(), input$plot_types)
        withCallingHandlers(
            {
                res <- convergence_check(model_fit(), plot_types = input$plot_types)
                if (!is.null(res$plots[[input$plot_types]])) print(res$plots[[input$plot_types]])
            },
            warning = function(w) {
                showNotification(paste("Diagnostic plot warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) { invokeRestart("muffleMessage") }
        )
    })
    
    
    # -- MODEL CHECKING --------------------------------------------------------
    output$metrics_definition_box <- renderUI({
        switch(input$gof_metrics_select,
               "loo"  = shinydashboard::box(title = "LOO (Leave-One-Out Cross-Validation)",
                                            width = NULL, solidHeader = TRUE, status = "primary",
                                            p("LOO estimates out-of-sample predictive accuracy using Pareto-smoothed importance sampling (PSIS). The ", strong("Pareto-k diagnostic"), " measures reliability; values above 0.7 indicate unstable approximations.")),
               "waic" = shinydashboard::box(title = "WAIC (Widely Applicable Information Criterion)",
                                            width = NULL, solidHeader = TRUE, status = "primary",
                                            p("WAIC is a fully Bayesian criterion approximating leave-one-out cross-validation. Lower values indicate better predictive accuracy."))
        )
    })
    
    observeEvent(input$run_model_check, {
        req(model_fit())
        withProgress(message = "Running model check...", {
            tryCatch(
                withCallingHandlers(
                    {
                        res <- model_compare(model_fit(), comparison_metrics = input$gof_metrics_select)
                        check_results(res)
                        showNotification("Model check completed!", type = "message", duration = 10)
                    },
                    warning = function(w) {
                        showNotification(paste("Model check warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) { invokeRestart("muffleMessage") }
                ),
                error = function(e) {
                    showNotification(paste("Model Check Error:", e$message), type = "error", duration = 15)
                }
            )
        })
    })
    
    output$model_check_output <- renderPrint({ req(check_results()); print(check_results()) })
    
    output$ppc_definition_box <- renderUI({
        switch(input$pp_check_type_select,
               "dens_overlay"      = shinydashboard::box(title = "Density Overlay", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("Overlays the density of the observed data against densities from multiple replicated datasets. Close alignment indicates good fit.")),
               "ribbon"            = shinydashboard::box(title = "Ribbon Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("Displays a credible interval ribbon of replicated values compared to the observed data.")),
               "intervals"         = shinydashboard::box(title = "Interval Plot", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("Shows posterior predictive intervals per observation. Points outside the intervals suggest model inadequacy.")),
               "error_scatter_avg" = shinydashboard::box(title = "Error Scatter (Mean)", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("Plots prediction errors against observed values. Patterns suggest systematic misfit.")),
               "bars"              = shinydashboard::box(title = "Frequency Bar Plot (Discrete)", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("For discrete outcomes, compares observed frequencies against the posterior predictive distribution.")),
               "scatter_avg"       = shinydashboard::box(title = "Scatter Plot (y vs yrep)", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("Plots observed values against average replicated values. Points near the identity line indicate good accuracy.")),
               "stat"              = shinydashboard::box(title = "Statistic vs Data", width = NULL, solidHeader = TRUE, status = "primary",
                                                         p("Compares a chosen summary statistic from replicated data to the same statistic in observed data."))
        )
    })
    
    output$model_check_plot_output <- renderPlot({
        req(model_fit(), input$pp_check_type_select)
        withCallingHandlers(
            {
                m      <- if (inherits(model_fit(), "hbmfit")) model_fit()$model else model_fit()
                ndraws <- input$pp_samples_slider
                stat   <- if (input$pp_check_type_select == "stat") input$pp_check_stat_select else NULL
                print(brms::pp_check(m, type = input$pp_check_type_select, ndraws = ndraws, stat = stat))
            },
            warning = function(w) {
                showNotification(paste("PPC warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) { invokeRestart("muffleMessage") }
        )
    })
    
    
    # -- PRIOR SENSITIVITY ANALYSIS --------------------------------------------
    prior_sens_result <- reactiveVal(NULL)
    
    observeEvent(input$run_sensitivity, {
        req(model_fit(), input$sensitivity_params_results)
        withProgress(message = "Running sensitivity analysis...", {
            tryCatch(
                withCallingHandlers(
                    {
                        res <- model_compare(model_fit(), run_prior_sensitivity = TRUE,
                                    sensitivity_vars = input$sensitivity_params_results)
                        prior_sens_result(res)
                        showNotification("Sensitivity analysis completed!", type = "message", duration = 10)
                    },
                    warning = function(w) {
                        showNotification(paste("Sensitivity warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) { invokeRestart("muffleMessage") }
                ),
                error = function(e) {
                    showNotification(paste("Sensitivity Error:", e$message), type = "error", duration = 15)
                }
            )
        })
    })
    
    output$prior_sensitivity_output_results <- renderPrint({
        res <- prior_sens_result()
        validate(need(!is.null(res), "No sensitivity results available yet."))
        print(res$prior_sensitivity_results$model1$result)
    })
    output$prior_sensitivity_plot_results <- renderPlot({
        res <- prior_sens_result()
        validate(need(!is.null(res), "No sensitivity results available yet."))
        print(res$prior_sensitivity_results$model1$plot)
    })
    
    
    # -- PREDICTION ------------------------------------------------------------
    output$prediction_results <- DT::renderDT({
        req(model_fit())
        withProgress(message = "Generating predictions...", {
            withCallingHandlers(
                {
                    preds <- sae_predict(model_fit())
                    DT::datatable(round(preds$result_table, 4L),
                                  options = list(scrollX = TRUE, scrollY = "370px"))
                },
                warning = function(w) {
                    showNotification(paste("Prediction warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) { invokeRestart("muffleMessage") }
            )
        })
    })
    
    output$download_results <- downloadHandler(
        filename = function() paste0("sae_predictions_", Sys.Date(), ".csv"),
        content  = function(file) {
            preds <- sae_predict(model_fit())
            utils::write.csv(preds$result_table, file, row.names = FALSE)
        }
    )
    
    
    # -- NEW DATA PREDICTION ---------------------------------------------------
    observe({
        req(input$response_var)
        pred_data(NULL)
        model_vars <- unique(stats::na.omit(c(
            input$linear_aux_vars, input$nonlinear_vars, input$re_groups,
            if (!is.null(input$trials_var)   && input$trials_var   != "none") input$trials_var,
            if (!is.null(input$trials_logit) && input$trials_logit != "none") input$trials_logit,
            if (!is.null(input$n_beta)       && input$n_beta       != "none") input$n_beta,
            if (!is.null(input$deff_beta)    && input$deff_beta    != "none") input$deff_beta
        )))
        df <- stats::setNames(
            data.frame(matrix(NA_real_, nrow = 1L, ncol = length(model_vars) + 1L)),
            c(model_vars, "prediction_result")
        )
        pred_data(df)
    })
    
    observeEvent(input$add_row, {
        req(model_fit())
        df <- pred_data()
        df <- rbind(df, stats::setNames(
            data.frame(matrix(NA_real_, nrow = 1L, ncol = ncol(df))), names(df)
        ))
        pred_data(df)
    })
    
    observeEvent(input$remove_row, {
        req(model_fit())
        df <- pred_data()
        pred_data(if (nrow(df) > 1L) df[-nrow(df), ] else df)
    })
    
    output$prediction_input_table <- DT::renderDT({
        req(pred_data())
        DT::datatable(pred_data(), selection = "none",
                      editable = list(target = "cell",
                                      disable = list(columns = ncol(pred_data()) - 1L)))
    })
    
    observeEvent(input$prediction_input_table_cell_edit, {
        info <- input$prediction_input_table_cell_edit
        df   <- pred_data()
        df[info$row, info$col + 1L] <- DT::coerceValue(info$value, df[info$row, info$col + 1L])
        pred_data(df)
    })
    
    observeEvent(input$predict_new, {
        req(model_fit(), pred_data())
        new_df <- pred_data()
        new_df[["prediction_result"]] <- NULL
        tryCatch(
            withCallingHandlers(
                {
                    preds <- sae_predict(model_fit(), newdata = new_df)
                    df    <- pred_data()
                    df[["prediction_result"]] <- preds$result_table$Mean
                    pred_data(df)
                },
                warning = function(w) {
                    showNotification(paste("Prediction warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) { invokeRestart("muffleMessage") }
            ),
            error = function(e) {
                showNotification(paste("Prediction Error:", e$message), type = "error", duration = 15)
            }
        )
    })
    
    
    # -- UPDATE MODEL ----------------------------------------------------------
    observeEvent(input$update_run, {
        req(model_fit())
        withProgress(message = "Updating model...", {
            tryCatch(
                withCallingHandlers(
                    {
                        new_d <- if (!is.null(input$update_newdata)) {
                            ext <- tools::file_ext(input$update_newdata$name)
                            if (ext %in% c("xls", "xlsx"))
                                as.data.frame(readxl::read_excel(input$update_newdata$datapath))
                            else
                                utils::read.csv(input$update_newdata$datapath, header = TRUE)
                        } else NULL
                        updated <- update_hbm(
                            model_fit(), newdata = new_d,
                            chains  = input$update_num_chains,
                            cores   = input$update_num_cores,
                            iter    = input$update_num_iter,
                            warmup  = input$update_num_warmup,
                            control = list(adapt_delta = input$update_adapt_delta)
                        )
                        model_fit(updated)
                        showNotification("Model updated successfully!", type = "message", duration = 10)
                    },
                    warning = function(w) {
                        showNotification(paste("Update warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) { invokeRestart("muffleMessage") }
                ),
                error = function(e) {
                    showNotification(paste("Update Error:", e$message), type = "error", duration = 15)
                }
            )
        })
    })
    
    
    # -- NONLINEAR OVERLAP WARNING ---------------------------------------------
    # Shown when the same variable appears in both the auxiliary and nonlinear
    # selectors (it will be modelled nonlinearly only in that case).
    output$nonlinear_overlap_note <- renderUI({
        nl      <- input$nonlinear_vars
        aux     <- input$linear_aux_vars
        overlap <- intersect(nl, aux)
        if (length(overlap) > 0L)
            div(class = "alert alert-warning",
                icon("exclamation-triangle"),
                " Variable(s) ", tags$strong(paste(overlap, collapse = ", ")),
                " appear in both Auxiliary Variables and Nonlinear Variables. ",
                "They will be modelled nonlinearly only ",
                "(removed from the linear part of the formula).")
    })
    
    
    # -- FORMULA PREVIEW -------------------------------------------------------
    output$formula_preview <- renderText({
        req(data(), input$response_var)
        tryCatch(
            paste("Formula:", paste(deparse(reactive_formula()$formula), collapse = " ")),
            error = function(e) "Formula not yet available."
        )
    })
    
    
    # -- SAVE OUTPUTS ----------------------------------------------------------
    output$save_model <- downloadHandler(
        filename = function() paste0("hbsae_model_", Sys.Date(), ".rds"),
        content  = function(file) {
            withProgress(message = "Saving model...", {
                tryCatch(
                    withCallingHandlers(
                        { saveRDS(model_fit(), file); showNotification("Model saved!", type = "message", duration = 10) },
                        warning = function(w) {
                            showNotification(paste("Save warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) { invokeRestart("muffleMessage") }
                    ),
                    error = function(e) {
                        showNotification(paste("Save Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )
    
    output$save_stan_code <- downloadHandler(
        filename = function() "stan_code.txt",
        content  = function(file) {
            withProgress(message = "Exporting Stan code...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            m <- if (inherits(model_fit(), "hbmfit")) model_fit()$model else model_fit()
                            writeLines(brms::stancode(m), file)
                            showNotification("Stan code exported!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Export warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) { invokeRestart("muffleMessage") }
                    ),
                    error = function(e) {
                        showNotification(paste("Export Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )
    
    output$save_coda <- downloadHandler(
        filename = function() paste0("coda_samples_", Sys.Date(), ".rds"),
        content  = function(file) {
            withProgress(message = "Exporting CODA samples...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            m <- if (inherits(model_fit(), "hbmfit")) model_fit()$model else model_fit()
                            saveRDS(coda::as.mcmc(m), file)
                            showNotification("CODA samples exported!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Export warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) { invokeRestart("muffleMessage") }
                    ),
                    error = function(e) {
                        showNotification(paste("Export Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )
    
    output$save_plots <- downloadHandler(
        filename = function() paste0("diagnostic_plots_", Sys.Date(), ".pdf"),
        content  = function(file) {
            withProgress(message = "Exporting diagnostic plots...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            grDevices::pdf(file, width = 10, height = 8)
                            res <- convergence_check(model_fit(), plot_types = c("trace", "dens", "acf",
                                                                    "nuts_energy", "rhat", "neff"))
                            for (p in res$plots) if (!is.null(p)) print(p)
                            grDevices::dev.off()
                            showNotification("Plots exported!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Export warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) { invokeRestart("muffleMessage") }
                    ),
                    error = function(e) {
                        grDevices::dev.off()
                        showNotification(paste("Export Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )
    
    # =========================================================================
    # NEW v0.3.0 FEATURES (Data Check / Spatial Weight Builder / Benchmarking)
    # =========================================================================

    # -- Update select inputs whenever data() changes --------------------------
    observe({
        d <- data()
        req(d)
        nm <- names(d)
        updateSelectInput(session,    "dc_response",      choices = nm)
        updateSelectizeInput(session, "dc_predictors",    choices = nm)
        updateSelectInput(session,    "dc_sre",
                           choices = c("(none)" = "", nm))
        updateSelectInput(session,    "bm_groups_var",    choices = nm)
        # Restrict weights variable to numeric columns
        num_cols <- nm[vapply(d, is.numeric, logical(1L))]
        updateSelectInput(session,    "bm_weights_var",
                           choices = c("Equal weights (1/n)" = "", num_cols))
    })

    # -- DATA CHECK -----------------------------------------------------------
    observeEvent(input$run_data_check, {
        req(data(), input$dc_response, input$dc_predictors)
        tryCatch({
            sre_arg <- if (nzchar(input$dc_sre %||% "")) input$dc_sre else NULL
            chk <- check_data(
                data       = data(),
                response   = input$dc_response,
                predictors = input$dc_predictors,
                sre        = sre_arg,
                M          = spatial_weights()
            )
            check_results(chk)
            output$data_check_output <- renderPrint({ summary(chk) })
            output$data_check_recommendation <- renderUI({
                if (length(chk$issues) > 0L) {
                    div(class = "alert alert-danger",
                        tags$strong("Issues found:"),
                        tags$ul(lapply(chk$issues, tags$li)))
                } else if (!is.na(chk$recommended_method)) {
                    msg <- paste0(
                        "Recommended handle_missing: '",
                        chk$recommended_method, "'. ",
                        chk$recommendation_text
                    )
                    div(class = "alert alert-warning",
                        tags$strong("Recommendation: "), msg,
                        if (!is.null(chk$non_sample_warning))
                          tags$pre(chk$non_sample_warning)
                        else NULL)
                } else {
                    div(class = "alert alert-success",
                        "Data look complete and consistent.")
                }
            })
        }, error = function(e) {
            output$data_check_output <- renderPrint({
                cat("Error running check_data():\n", e$message, "\n")
            })
            output$data_check_recommendation <- renderUI(NULL)
        })
    })

    # -- BUILD SPATIAL WEIGHT FROM SHAPEFILE ----------------------------------
    observeEvent(input$build_spatial_btn, {
        req(input$shp_file)
        tryCatch({
            files <- input$shp_file
            # Multi-file upload: shp + shx + dbf must coexist in same dir.
            shp_idx <- grep("\\.shp$", files$name, ignore.case = TRUE)
            if (length(shp_idx) == 0L)
              stop("No .shp file found in upload (need .shp + .shx + .dbf).",
                   call. = FALSE)
            tmp_dir <- dirname(files$datapath[shp_idx[1L]])
            for (i in seq_len(nrow(files))) {
                file.rename(files$datapath[i],
                             file.path(tmp_dir, files$name[i]))
            }
            shp_path <- file.path(tmp_dir, files$name[shp_idx[1L]])

            id_col <- if (nzchar(input$shp_id_col %||% ""))
              input$shp_id_col else NULL

            # v0.4.0+: derive for_model from current sre_type so type/style
            # defaults match the model's theoretical requirements.
            for_model <- if (!is.null(input$sre_type) &&
                              input$sre_type %in% c("car", "sar"))
              input$sre_type else NULL

            M <- build_spatial_weight(
                shp       = shp_path,
                for_model = for_model,
                type      = input$shp_type,
                style     = input$shp_style,
                k         = input$shp_k         %||% 4L,
                threshold = if (is.null(input$shp_threshold) ||
                                is.na(input$shp_threshold))
                              NULL else input$shp_threshold,
                id_col    = id_col,
                validate  = TRUE
            )
            spatial_weights(M)

            # Surface the validation diagnostics from the matrix attribute
            chk <- attr(M, "hbsae_check")
            warn_html <- if (!is.null(chk) && length(chk$warnings) > 0L)
              tags$ul(lapply(chk$warnings, tags$li))
            else NULL

            output$spatial_weights_error <- renderUI(
                tagList(
                    div(class = "alert alert-success",
                        sprintf(paste0(
                            "Built %d x %d spatial weight matrix from ",
                            "shapefile (type = %s, style = %s)."
                        ), nrow(M), ncol(M),
                          attr(M, "hbsae_type"),
                          attr(M, "hbsae_style")),
                        if (!is.null(chk))
                            tags$div(
                                tags$strong("Theoretical check: "),
                                if (chk$compatible)
                                  tags$span("compatible.",
                                              style = "color: green;")
                                else
                                  tags$span("INCOMPATIBLE.",
                                              style = "color: red;"),
                                tags$br(),
                                sprintf(paste0(
                                  "Symmetric: %s, zero-diagonal: %s, ",
                                  "components: %s, isolated: %s"
                                ), chk$is_symmetric, chk$has_zero_diag,
                                  chk$n_components, chk$n_isolated)
                            )),
                    if (!is.null(warn_html))
                      div(class = "alert alert-warning",
                          tags$strong("Spatial-weight warnings:"),
                          warn_html)
                )
            )
        }, error = function(e) {
            output$spatial_weights_error <- renderUI(
                div(class = "alert alert-danger",
                    paste("Failed to build weight matrix:", e$message))
            )
        })
    })

    # v0.4.0+: also validate user-uploaded matrices.  Fires after the
    # existing fileInput observer has populated spatial_weights().
    observe({
        M <- spatial_weights()
        if (is.null(M) || !is.matrix(M)) return(invisible(NULL))
        # Only validate if we have a sre_type selected
        st <- input$sre_type
        if (is.null(st) || !(st %in% c("car", "sar"))) return(invisible(NULL))
        # Skip if matrix already came from build_spatial_weight() (has attr)
        if (!is.null(attr(M, "hbsae_check"))) return(invisible(NULL))

        chk <- tryCatch(
            check_spatial_weight(M, sre_type = st, verbose = FALSE),
            error = function(e) NULL
        )
        if (is.null(chk)) return(invisible(NULL))

        if (!chk$compatible) {
            showNotification(
                paste("Uploaded matrix is INCOMPATIBLE with sre_type = '",
                      st, "':\n",
                      paste(chk$issues, collapse = "; ")),
                type = "error", duration = 15
            )
        } else if (length(chk$warnings) > 0L) {
            showNotification(
                paste("Spatial weight warnings:",
                      paste(chk$warnings, collapse = "; ")),
                type = "warning", duration = 12
            )
        }
    })

    # -- BENCHMARKING ---------------------------------------------------------
    benchmark_result <- reactiveVal(NULL)

    # Live preview: how groups partition the kecamatan + what sums look like
    output$bm_group_preview <- renderPrint({
        d <- data()
        req(d, input$bm_groups_var)
        if (!nzchar(input$bm_groups_var)) return(invisible(NULL))
        g <- d[[input$bm_groups_var]]
        if (is.null(g)) {
            cat("Group variable not in data.\n")
            return(invisible(NULL))
        }
        cat("Group levels (in order):\n")
        tab <- table(g)
        print(tab)
        cat(sprintf("\n=> Provide %d target value(s) above, in the same order.\n",
                    length(tab)))
        if (!is.null(model_fit())) {
            preds <- tryCatch(sae_predict(model_fit()), error = function(e) NULL)
            if (!is.null(preds) && length(preds$pred) == length(g)) {
                wts <- if (nzchar(input$bm_weights_var %||% ""))
                  d[[input$bm_weights_var]][seq_along(preds$pred)]
                else rep(1 / length(preds$pred), length(preds$pred))
                grp_sums <- tapply(wts * preds$pred, g, sum)
                cat("\nCurrent weighted sums per group (BEFORE benchmark):\n")
                print(round(grp_sums, 4))
            }
        }
    })

    # Auto-fill targets text with current group sums (a sensible starting point)
    observeEvent(input$bm_template_targets, {
        req(model_fit(), input$bm_groups_var, data())
        d <- data()
        g <- d[[input$bm_groups_var]]
        preds <- tryCatch(sae_predict(model_fit()), error = function(e) NULL)
        req(preds)
        wts <- if (nzchar(input$bm_weights_var %||% ""))
          d[[input$bm_weights_var]][seq_along(preds$pred)]
        else rep(1 / length(preds$pred), length(preds$pred))
        grp_sums <- tapply(wts * preds$pred, g, sum)
        updateTextInput(session, "bm_targets_multi",
                         value = paste(round(grp_sums, 4), collapse = ", "))
        showNotification(
            "Targets pre-filled with current group sums. ",
            "Adjust to your official figures.",
            type = "message", duration = 8)
    })

    observeEvent(input$run_benchmark, {
        req(model_fit())
        tryCatch({
            # 1. Get baseline SAE predictions
            preds <- sae_predict(model_fit())

            # 2. Resolve weights
            wts <- if (nzchar(input$bm_weights_var %||% "")) {
                w <- data()[[input$bm_weights_var]]
                if (is.null(w))
                  stop("Weights variable not found in data.", call. = FALSE)
                w[seq_len(length(preds$pred))]
            } else NULL

            # 3. Resolve target depending on method
            if (input$bm_method == "raking") {
                req(input$bm_targets_multi, input$bm_groups_var)
                target_vec <- as.numeric(strsplit(
                    gsub("\\s+", "", input$bm_targets_multi), ","
                )[[1L]])
                if (anyNA(target_vec))
                  stop("Could not parse 'Group Targets' as numeric vector.",
                       call. = FALSE)
                groups_vec <- data()[[input$bm_groups_var]]
                bm <- sae_benchmark(
                    predictions = preds,
                    target      = target_vec,
                    weights     = wts,
                    groups      = groups_vec[seq_len(length(preds$pred))],
                    method      = "raking"
                )
            } else {
                req(input$bm_target_single)
                bm <- sae_benchmark(
                    predictions = preds,
                    target      = as.numeric(input$bm_target_single),
                    weights     = wts,
                    method      = input$bm_method
                )
            }

            benchmark_result(list(original = preds, benchmarked = bm))

            # 4. Render outputs
            output$benchmark_summary <- renderPrint({
                cat(sprintf("Method            : %s\n", bm$benchmark_info$method))
                if (length(bm$benchmark_info$target) == 1L)
                  cat(sprintf("Target total      : %.4f\n",
                              bm$benchmark_info$target))
                else
                  cat(sprintf("Target totals     : %s\n",
                              paste(round(bm$benchmark_info$target, 4),
                                    collapse = ", ")))
                if (!is.null(bm$benchmark_info$adjustment))
                  cat(sprintf("Adjustment        : %.6f\n",
                              bm$benchmark_info$adjustment))
                if (!is.na(bm$benchmark_info$converged))
                  cat(sprintf("Converged         : %s\n",
                              bm$benchmark_info$converged))
                cat(sprintf("Sum |delta|       : %.4f\n",
                            sum(abs(bm$pred - preds$pred))))
                cat(sprintf("Mean |delta|      : %.4f\n",
                            mean(abs(bm$pred - preds$pred))))
            })

            output$benchmark_table <- DT::renderDT({
                df <- data.frame(
                    Area        = seq_along(preds$pred),
                    Original    = round(preds$pred, 4),
                    Benchmarked = round(bm$pred,    4),
                    Delta       = round(bm$pred - preds$pred, 4),
                    RSE_pct     = round(bm$result_table$RSE_percent, 2)
                )
                DT::datatable(df, options = list(pageLength = 10),
                               rownames = FALSE)
            })

            output$benchmark_plot <- renderPlot({
                df <- data.frame(
                    Area        = seq_along(preds$pred),
                    Original    = preds$pred,
                    Benchmarked = bm$pred
                )
                df_long <- rbind(
                    data.frame(Area = df$Area,
                               Estimate = df$Original,
                               Type = "Original"),
                    data.frame(Area = df$Area,
                               Estimate = df$Benchmarked,
                               Type = "Benchmarked")
                )
                ggplot2::ggplot(
                    df_long,
                    ggplot2::aes(x = .data$Area, y = .data$Estimate,
                                 colour = .data$Type)
                ) +
                    ggplot2::geom_point(size = 2) +
                    ggplot2::geom_line(alpha = 0.6) +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(title = "Original vs Benchmarked Estimates",
                                  x = "Area", y = "Estimate") +
                    ggplot2::scale_colour_manual(
                        values = c("Original" = "#185FA5",
                                    "Benchmarked" = "#D85A30"))
            })

            showNotification("Benchmark applied successfully.",
                              type = "message", duration = 8)

        }, error = function(e) {
            showNotification(paste("Benchmark error:", e$message),
                              type = "error", duration = 15)
        })
    })

    output$download_benchmark <- downloadHandler(
        filename = function()
          paste0("sae_benchmarked_", Sys.Date(), ".csv"),
        content  = function(file) {
            br <- benchmark_result()
            if (is.null(br)) return(invisible(NULL))
            df <- data.frame(
                Area        = seq_along(br$original$pred),
                Original    = br$original$pred,
                Benchmarked = br$benchmarked$pred,
                RSE_pct     = br$benchmarked$result_table$RSE_percent
            )
            utils::write.csv(df, file, row.names = FALSE)
        }
    )

}   # end server


# =============================================================================
# RUN APP
# =============================================================================
shinyApp(ui, server)
