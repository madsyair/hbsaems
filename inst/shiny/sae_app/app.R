# ==== LIBRARIES (Implicitly required by the code) ====

library(shiny)
# library(shinydashboard)
# library(DT)
# library(ggplot2)
# library(shinyWidgets)
# library(tools)
# library(readxl)
# library(stats)
# library(XICOR)
# library(energy)
# library(minerva)
# library(brms)
# library(posterior)
# library(priorsense)
# library(bayesplot)
# library(grDevices)
library(hbsaems)

# ===== UI ====
# Defines the user interface for the application.
ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Small Area Estimation App"),
    shinydashboard::dashboardSidebar(
        # Defines the navigation menu in the sidebar.
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Home", tabName = "home", icon = icon("home")),
            shinydashboard::menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
            shinydashboard::menuItem("Data Exploration", tabName = "data_exploration", icon = icon("search")),
            shinydashboard::menuItem("Modeling", tabName = "modeling", icon = icon("cogs")),
            shinydashboard::menuItemOutput("update_menu"),
            shinydashboard::menuItem("Results", tabName = "results", icon = icon("chart-bar"))
        )
    ),
    # Defines the main body of the dashboard.
    shinydashboard::dashboardBody(
        shinydashboard::tabItems(
            ## ===== HOME TAB =====
            # Content for the Home tab, providing an introduction to the app.
            shinydashboard::tabItem(
                tabName = "home",
                fluidRow(
                    shinydashboard::box(
                        title = "Introduction", width = 12, solidHeader = TRUE, status = "primary",
                        p(
                            "The ", strong("run_sae_app()"), " function in the ", code("hbsaems"),
                            " package provides an interactive ", strong("Shiny Dashboard"),
                            " for ", em("Hierarchical Bayesian Small Area Estimation (HBSAE)"),
                            " using ", code("brms"), " for Bayesian inference with Stan."
                        ),
                        p("This application offers a user-friendly interface to upload data, define models,
              and obtain estimation results without requiring extensive R coding.")
                    ),
                    shinydashboard::box(
                        title = "App Structure", width = 12, solidHeader = TRUE, status = "primary",
                        tags$ul(
                            tags$li(strong("Home:"), " Overview and purpose of the application."),
                            tags$li(strong("Data Upload:"), " Upload and preview datasets (CSV/Excel), detect missing values."),
                            tags$li(strong("Modeling:"), " Specify model structure, priors, MCMC settings, and fit the model."),
                            tags$li(strong("Results:"), " Review model diagnostics, summaries, and prediction outputs.")
                        )
                    ),
                    shinydashboard::box(
                        title = "Example Workflow", width = 12, solidHeader = TRUE, status = "primary",
                        tags$ol(
                            tags$li("Upload a dataset with area-level direct estimates and auxiliary variables."),
                            tags$li("Specify the modeling structure including response variable, auxiliary variables, random effects, and spatial effects."),
                            tags$li("Choose distribution, family, link function, and define priors if needed."),
                            tags$li("Conduct prior predictive checking to evaluate whether the chosen priors and model structure produce plausible simulated data."),
                            tags$li("Set MCMC configurations and fit the model."),
                            tags$li("Inspect convergence diagnostics and extract estimation results.")
                        )
                    )
                )
            ),
            ## ===== DATA UPLOAD TAB =====
            # Content for the Data Upload tab.
            shinydashboard::tabItem(
                tabName = "data_upload",
                fluidRow(
                    # UI element for file input.
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
                                    " — typically survey-based or observed values for each area."
                                ),
                                tags$li("Include one or more ", tags$strong("auxiliary variables"), "
                                            (predictors) that can help explain variation in the direct estimates."),
                                tags$li(
                                    "Optionally, include a ", tags$strong("unique identifier column"),
                                    " for each area (e.g., area code or name)."
                                ),
                                tags$li(tags$strong("Important notes on file formats:")),
                                tags$ul(
                                    tags$li(".csv files should use a comma (`,`) as the field separator and a period (`.`) for decimal numbers."),
                                    tags$li(".xlsx/.xls files might use a comma (`,`) as the decimal separator, especially in European-style regional settings."),
                                    tags$li("Make sure numeric columns are not interpreted as text due to wrong decimal separators.")
                                ),
                            ),
                            tags$li("The application will automatically detect ", tags$strong("missing values"), ", and you can choose the method for handling them during the modeling stage."),
                            tags$li("If spatial modeling is intended, you may also prepare a corresponding ", tags$strong("spatial weight matrix"), " (uploaded separately).")
                        ),
                        fileInput("data_file", "Upload file", accept = c(".csv", ".xlsx", ".xls", ".rda", ".RData"))
                    ),
                    # UI element for displaying a preview of the uploaded data.
                    shinydashboard::box(
                        title = "Data Preview", width = 12, solidHeader = TRUE, status = "primary",
                        DT::DTOutput("data_preview")
                    )
                )
            ),
            ## ===== DATA EXPLORATION TAB =====
            # Content for the Data Exploration tab.
            shinydashboard::tabItem(
                tabName = "data_exploration",
                fluidRow(
                    # Overview of the data exploration tools available.
                    shinydashboard::box(
                        title = "Data Exploration", width = 12, solidHeader = TRUE, status = "primary",
                        tags$p("This section allows you to explore your dataset to better understand the variables and their relationships before modeling. The following tools are available:"),
                        tags$ul(
                            tags$li(strong("Summary Statistics:"), " Displays basic descriptive statistics like mean, median, min, max, and quartiles for the selected numeric variable."),
                            tags$li(strong("Histogram:"), " Shows the distribution of the selected variable including density curve to visualize the shape and spread."),
                            tags$li(strong("Boxplot:"), " Provides a graphical summary of variable distribution highlighting median, quartiles, and potential outliers."),
                            tags$li(
                                strong("Scatter Plot & Correlation:"), " Visualizes the relationship between two variables with optional transformations on axes. Additionally, calculates several correlation metrics:",
                                tags$ul(
                                    tags$li(strong("Pearson's Correlation:"), " Measures linear association between variables. Values close to ±1 indicate strong linear relationship, while 0 indicates no linear correlation. P-value tests the null hypothesis of no linear relationship."),
                                    tags$li(strong("Spearman's Rho:"), " Non-parametric measure based on ranks. Captures monotonic (increasing or decreasing) relationships. P-value tests the null hypothesis of no monotonic relationship."),
                                    tags$li(strong("Chatterjee's Xi:"), " Measures the predictability of one variable from another. Values close to 1 suggest strong predictive association. P-value (available via the XICOR package) tests the null hypothesis of independence."),
                                    tags$li(strong("Distance Correlation:"), " Captures both linear and nonlinear relationships. A value of 0 implies independence. P-value (from permutation test) tests the null hypothesis of independence."),
                                    tags$li(strong("Maximal Information Coefficient (MIC):"), " Detects a wide variety of associations, both linear and nonlinear. Ranges from 0 (independent) to 1 (perfect functional relationship). No direct p-value is available")
                                )
                            )
                        ),
                        tags$p(em("Note: A low p-value (typically < 0.05) indicates strong evidence against the null hypothesis, suggesting a statistically significant relationship between the variables."))
                    ),
                    # UI for displaying summary statistics.
                    shinydashboard::box(
                        title = "Summary Statistics", width = 12, solidHeader = TRUE, status = "primary",
                        selectInput("explore_var_summary", "Select Variable for Summary:", choices = NULL),
                        verbatimTextOutput("numeric_summary")
                    )
                ),
                fluidRow(
                    # UI for Histogram plot.
                    shinydashboard::box(
                        title = "Histogram", width = 6, solidHeader = TRUE, status = "primary",
                        selectInput("explore_var_hist", "Select Variable for Histogram:", choices = NULL),
                        plotOutput("histogram_plot")
                    ),
                    # UI for Boxplot.
                    shinydashboard::box(
                        title = "Boxplot", width = 6, solidHeader = TRUE, status = "primary",
                        selectInput("explore_var_boxplot", "Select Variable for Boxplot:", choices = NULL),
                        plotOutput("boxplot_plot")
                    )
                ),
                fluidRow(
                    # UI for Scatter plot and correlation analysis.
                    shinydashboard::box(
                        title = "Scatter Plot & Correlation", width = 12, solidHeader = TRUE, status = "primary",
                        fluidRow(
                            column(
                                width = 6,
                                selectInput("explore_var_scatter_x", "X-axis Variable:", choices = NULL),
                                selectInput("explore_transform_x", "X-axis Transformation:", choices = c("None" = "none", "Log" = "log", "Z-score" = "zscore"), selected = "none"),
                                selectInput("explore_var_scatter_color", "Color by (Categorical, optional):", choices = c("None" = ""), selected = "")
                            ),
                            column(
                                width = 6,
                                selectInput("explore_var_scatter_y", "Y-axis Variable:", choices = NULL),
                                selectInput("explore_transform_y", "Y-axis Transformation:", choices = c("None" = "none", "Log" = "log", "Logit" = "logit", "Z-score" = "zscore"), selected = "none")
                            )
                        ),
                        uiOutput("correlation_results"),
                        plotOutput("scatter_plot_exploration")
                    )
                )
            ),
            ## ===== MODELING TAB =====
            # Content for the Modeling tab, which contains sub-tabs for configuration, prior checking, and MCMC settings.
            shinydashboard::tabItem(
                tabName = "modeling",
                tabsetPanel(
                    # Sub-tab for Model Configuration.
                    tabPanel(
                        title = "Model Configuration",
                        fluidRow(
                            shinydashboard::box(
                                title = "Overview", width = 12, solidHeader = TRUE, status = "primary",
                                tags$p("This section allows you to specify the variables and model settings used for hierarchical Bayesian modeling."),
                                tags$ul(
                                    tags$li(
                                        strong("Response Variable:"),
                                        " The outcome variable being modeled. This variable is predicted using auxiliary variables and may follow a specific distribution (e.g., Lognormal, Beta)."
                                    ),
                                    tags$li(
                                        strong("Auxiliary Variables:"),
                                        " A set of explanatory (independent) variables forming the fixed effects in the model. These are used to explain variability in the response."
                                    ),
                                    tags$li(
                                        strong("Group Variables:"),
                                        " The name of the grouping variable (e.g., area, cluster, region) used to define the hierarchical structure for random effects. This variable should correspond to a column in the input ⁠ data ⁠ and is typically used to model area-level variation through random intercepts. If omitted, each row is treated as its own group."
                                    ),
                                    tags$li(
                                        strong("Spatial Type:"),
                                        " Indicates whether spatial random effects are modeled. Options include SAR (Spatial Autoregressive) and CAR (Conditional Autoregressive)."
                                    ),
                                    tags$li(
                                        strong("SAR Type / CAR Type:"),
                                        " Specifies the subtype of the selected spatial model. For SAR: 'lag' or 'error'. For CAR: 'icar', 'escar', 'esicar', or 'bym2'."
                                    ),
                                    tags$li(
                                        strong("Spatial Random Effects & Weights:"),
                                        " If spatial effects are included, upload a square spatial weights matrix (CSV) whose row and column names match the spatial grouping variable."
                                    ),
                                    tags$li(
                                        strong("Distribution Type:"),
                                        " Defines the assumed distribution of the response variable. Options may include Lognormal, Beta, Logitnormal, or custom families for flexible modeling."
                                    ),
                                    tags$li(
                                        strong("HB Family and Link:"),
                                        " Selects the Bayesian hierarchical family (e.g., lognormal, beta) and associated link function (e.g., log, logit)."
                                    ),
                                    tags$li(
                                        strong("Missing Data Handling:"),
                                        " Choose a strategy for handling missing values in the response or auxiliary variables: deletion (complete case), multiple imputation, or model-based imputation."
                                    )
                                )
                            ),
                            # UI for selecting variables for the model.
                            shinydashboard::box(
                                title = "Variable Selection", width = 12, solidHeader = TRUE, status = "primary",
                                column(6, selectInput("response_var", "Response Variable", choices = NULL)),
                                # column(6, selectizeInput("linear_aux_vars", "Linear Auxiliary Variables", choices = NULL, multiple = TRUE)),
                                column(6, selectizeInput("linear_aux_vars", "Auxiliary Variables", choices = NULL, multiple = TRUE)),
                                # conditionalPanel(
                                #   condition = "input.dist_type == 'Custom'",
                                #   column(6, selectizeInput("nonlinear_aux_vars", "Nonlinear Auxiliary Variables", choices = NULL, multiple = TRUE))
                                # ),
                                column(6, selectizeInput("re_groups", "Group Variables", choices = NULL, multiple = TRUE)),
                                column(6, selectInput("sre_type", "Spatial Type", choices = c("None" = "none", "SAR" = "sar", "CAR" = "car"))),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type == 'sar'",
                                    selectInput("sar_type", "SAR Type", choices = c("Lag" = "lag", "Error" = "error"), selected = "lag")
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type == 'car'",
                                    selectInput("car_type", "CAR Type", choices = c("eSCAR" = "escar", "eSICAR" = "esicar", "ICAR" = "icar", "BYM2" = "bym2"), selected = "escar")
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type == 'sar' || input.sre_type == 'car'",
                                    selectInput("sre", "Spatial Random Effects", choices = NULL),
                                    fileInput("spatial_weights", "Upload Spatial Weights",
                                              accept = c(".csv", ".xls", ".xlsx", ".rda", ".RData"))
                                    
                                )),
                                column(6, conditionalPanel(
                                    condition = "input.sre_type != 'none'",
                                    uiOutput("spatial_weights_error")
                                ))
                            ),
                            # UI for distribution settings.
                            shinydashboard::box(
                                title = "Distribution Settings", width = 12, solidHeader = TRUE, status = "primary",
                                column(6, selectInput("dist_type", "Distribution Type", choices = c("Custom", "Beta-Logitnormal", "Binomial-Logitnormal",  "Lognormal-Lognormal"), selected = "Custom")),
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
                                    # column(6, selectInput("model_type", "Model Type for Nonlinear Terms", choices = c("Spline", "Gaussian Process"))),
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
                                    column(6, selectInput("hb_link", "HB Link", choices = c("identity", "log", "logit", "probit", "cloglog")))
                                )
                            ),
                            # UI for handling missing data, appears conditionally.
                            conditionalPanel(
                                condition = "output.missing_detected == true",
                                shinydashboard::box(
                                    title = "Missing Data Handling", width = 12, solidHeader = TRUE, status = "warning",
                                    column(6, uiOutput("missing_vars_list")),
                                    conditionalPanel(
                                        condition = "output.missing_aux_response_detected == true",
                                        column(6, selectInput("missing_data_method", "Handle Missing Data", choices = c("None" = "none", "Deleted" = "deleted", "Model" = "model", "Multiple" = "multiple")))
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
                    ### ===== PRIOR CHECKING SUB-TAB =====
                    tabPanel(
                        title = "Prior Checking",
                        fluidRow(
                            # Overview of prior specification and checking.
                            shinydashboard::box(
                                title = "Overview", width = 12, solidHeader = TRUE, status = "primary",
                                tags$ul(
                                    tags$li(
                                        p(
                                            "To specify a prior, use valid syntax supported by the ", strong("brms"), " package. Examples include:",
                                            br(), code('prior(normal(0, 5), class = "b")'), " for fixed effects, or ",
                                            code('prior(cauchy(0, 2), class = "sd")'), " for standard deviations of group-level effects."
                                        )
                                    ),
                                    tags$li(p("Click ", strong("'Add Prior'"), " to include the specified prior into the model, and use ", strong("'Clear Priors'"), " to remove all existing priors.")),
                                    tags$li(p("After defining priors, click ", strong("'Prior Predictive Checking'"), " to simulate data from the prior predictive distribution. This helps assess whether your priors lead to reasonable outcomes, before fitting the model.")),
                                    tags$li(p(strong("Prior Summary Output"), " (", code("prior_summary"), ") displays a list of all currently specified priors, grouped by parameter class.")),
                                    tags$li(p(strong("Parameter Draws Output"), " (", code("prior_parameter_draw"), ") shows random draws from the prior distributions, helping to understand the scale and variability implied by your prior choices.")),
                                    tags$li(
                                        p(
                                            strong("Prior Predictive Plot"), " (", code("prior_predictive_plot"), ") visualizes the distribution of simulated outcomes generated from your priors. ",
                                            "You can use this to check whether the prior alone can generate data that fall within a plausible range of the response variable."
                                        )
                                    )
                                )
                            ),
                            # UI for defining and checking priors.
                            shinydashboard::box(
                                title = "Define Priors", width = 12, solidHeader = TRUE, status = "primary",
                                textInput("prior_input", "Enter Prior:", placeholder = 'e.g., prior(normal(0,5), class = "b")'),
                                actionButton("add_prior", "Add Prior", icon = icon("plus"), class = "btn-success"),
                                actionButton("clear_prior", "Clear Priors", icon = icon("trash"), class = "btn-danger"),
                                br(), br(),
                                verbatimTextOutput("prior_list"),
                                actionButton("prior_check", "Prior Predictive Checking", icon = icon("play"), class = "btn-info")
                            ),
                            # UI for displaying results of prior predictive checking.
                            shinydashboard::box(
                                title = "Prior Predictive Checking Results", width = 12, solidHeader = TRUE, status = "primary",
                                verbatimTextOutput("prior_summary"),
                                verbatimTextOutput("prior_parameter_draw"),
                                plotOutput("prior_predictive_plot")
                            )
                        )
                    ),
                    ### ===== MCMC SETTING SUB-TAB ====
                    tabPanel(
                        title = "MCMC Setting",
                        fluidRow(
                            # Overview of MCMC settings.
                            shinydashboard::box(
                                title = "Overview", width = 12, solidHeader = TRUE, status = "primary",
                                tagList(
                                    p(
                                        "This section allows you to configure the Markov Chain Monte Carlo (MCMC) settings used by the ",
                                        code("brms"), " package to fit the hierarchical Bayesian model. These settings impact model accuracy, convergence, and computational performance."
                                    ),
                                    tags$ul(
                                        tags$li(p(
                                            strong("Seed:"),
                                            " Sets the random seed for reproducibility. Use NA for random behavior."
                                        )),
                                        tags$li(p(
                                            strong("Chains:"),
                                            " Number of MCMC chains to run in parallel."
                                        )),
                                        tags$li(p(
                                            strong("Cores:"),
                                            " Number of CPU cores to use. Ideally equal to the number of chains for efficient parallel computation."
                                        )),
                                        tags$li(p(
                                            strong("Thinning Rate:"),
                                            " Frequency of sample retention. A value of 1 means no thinning; increase only if memory constraints arise."
                                        )),
                                        tags$li(p(
                                            strong("Iterations:"),
                                            " Total number of iterations per chain, including warmup. Determines the total number of samples drawn."
                                        )),
                                        tags$li(p(
                                            strong("Warmup:"),
                                            " Number of initial iterations for sampler adaptation. These are discarded from posterior inference."
                                        )),
                                        tags$li(p(
                                            strong("Adapt Delta:"),
                                            " Target acceptance probability for the NUTS sampler. Higher values (e.g., 0.99) reduce divergent transitions but may slow sampling."
                                        )),
                                        tags$li(p(
                                            "After configuring these settings, click ", strong("'Fit Model'"),
                                            " to begin the estimation. Any issues encountered during fitting will be displayed below the interface."
                                        ))
                                    )
                                )
                            ),
                            # UI for MCMC settings inputs.
                            shinydashboard::box(
                                title = "MCMC Settings", width = 12, solidHeader = TRUE, status = "primary",
                                column(6, numericInput("num_seed", "Seed:", value = NA, step = 1L)),
                                column(6, numericInput("num_chains", "Chains", value = 1, min = 1)),
                                column(6, numericInput("num_cores", "Cores", value = 1, min = 1)),
                                column(6, numericInput("num_thin", "Thinning rate", value = 1, min = 1)),
                                column(6, numericInput("num_iter", "Iterations", value = 4000, min = 1)),
                                column(6, numericInput("num_warmup", "Warmup", value = 2000, min = 1)),
                                column(6, sliderInput("adapt_delta", "Adapt Delta", min = 0.8, max = 0.99, value = 0.95))
                            ),
                            # UI for the "Fit Model" button.
                            fluidRow(
                                column(
                                    12,
                                    div(
                                        style = "margin-left: 20px;",
                                        actionButton("fit_model", "Fit Model", icon = icon("play"), class = "btn-info")
                                    )
                                )
                            ),
                            uiOutput("model_fitting_error")
                        )
                    )
                )
            ),
            
            ## ===== RESULTS TAB =====
            # Content for the Results tab, containing multiple sub-tabs for different result types.
            shinydashboard::tabItem(
                tabName = "results",
                tabsetPanel(
                    id = "results_tabs",
                    ### ===== MODEL SUMMARY SUB-TAB =====
                    tabPanel(
                        "Model Summary",
                        fluidRow(
                            verbatimTextOutput("model_summary"),
                            verbatimTextOutput("model_prior")
                        )
                    ),
                    ### ===== CONVERGENCE DIAGNOSTICS SUB-TAB =====
                    tabPanel(
                        "Convergence Diagnostics",
                        tabsetPanel(
                            tabPanel(
                                "Diagnostics Tests",
                                fluidRow(
                                    column(3, wellPanel(
                                        shinyWidgets::pickerInput("diag_tests", "Select Diagnostic Test Types:",
                                            choices = c(
                                                "R-hat" = "rhat",
                                                "Geweke" = "geweke",
                                                "Raftery-Lewis" = "raftery",
                                                "Heidelberger-Welch" = "heidel"
                                            ),
                                            multiple = FALSE, selected = "rhat"
                                        )
                                    )),
                                    column(
                                        9,
                                        uiOutput("diag_definition"),
                                        verbatimTextOutput("diag_numerical")
                                    )
                                )
                            ),
                            tabPanel(
                                "Diagnostics Plots",
                                fluidRow(
                                    column(3, wellPanel(
                                        shinyWidgets::pickerInput("plot_types", "Select Diagnostic Plot Types:",
                                            choices = c(
                                                "Trace Plot" = "trace",
                                                "Density Plot" = "dens",
                                                "Autocorrelation Function (acf) Plot" = "acf",
                                                "NUTS Energy" = "nuts_energy",
                                                "R-hat Plot" = "rhat",
                                                "Effective Sample Size (ESS) Plot" = "neff"
                                            ),
                                            multiple = FALSE, selected = "trace"
                                        )
                                    )),
                                    column(
                                        9,
                                        uiOutput("plot_definition"),
                                        plotOutput("diag_plots", height = "600px")
                                    )
                                )
                            )
                        )
                    ),
                    ### ===== MODEL CHECKING SUB-TAB =====
                    tabPanel(
                        "Model Checking (Goodness of Fit)",
                        value = "model_checking_panel",
                        tabsetPanel(
                            tabPanel(
                                "Numerical Checks",
                                fluidRow(
                                    column(
                                        3,
                                        wellPanel(
                                            # actionButton("run_model_check", "▶ Run Model Checking", icon = icon("play"), class = "btn-primary"),
                                            shinyWidgets::pickerInput(inputId = "gof_metrics_select", label = "Select Goodness-of-Fit Metric to Display:", choices = c("Leave-One-Out (LOO)" = "loo", "WAIC" = "waic"), multiple = FALSE, selected = "loo"),
                                            checkboxInput("moment_match_check", "Use Moment Matching (recommended for complex models)", value = FALSE),
                                        )
                                    ),
                                    column(
                                        9,
                                        uiOutput("metrics_definition_box"),
                                        verbatimTextOutput("model_check_metrics_output")
                                    )
                                )
                            ),
                            tabPanel(
                                "Graphical Checks",
                                fluidRow(
                                    column(3, wellPanel(
                                        selectInput(
                                            "pp_check_type_select",
                                            "Select Posterior Predictive Checking Plot Type:",
                                            choices = c(
                                                "Density Overlay" = "dens_overlay",
                                                "Boxplot" = "boxplot",
                                                "Frequency Plot (Discrete)" = "bars",
                                                "Scatter Plot (y vs yrep)" = "scatter_avg",
                                                "Statistic vs Data" = "stat"
                                            ),
                                            selected = "dens_overlay"
                                        ),
                                        sliderInput(
                                            "pp_samples_slider",
                                            "Number of Posterior Predictive Samples:",
                                            min = 10, max = 200, value = 50, step = 10
                                        ),
                                        conditionalPanel(
                                            condition = "input.pp_check_type_select == 'stat'",
                                            selectInput(
                                                "pp_check_stat_select",
                                                "Statistic for PPC:",
                                                choices = c("mean", "median", "sd", "min", "max"),
                                                selected = "mean"
                                            )
                                        )
                                    )),
                                    column(
                                        9,
                                        uiOutput("ppc_definition_box"),
                                        plotOutput("model_check_plot_output", height = "600px")
                                    )
                                )
                            )
                        )
                    ),
                    ### ===== PRIOR SENSITIVITY SUB-TAB =====
                    tabPanel(
                        "Prior Sensitivity Analysis",
                        value = "prior_sensitivity_panel",
                        shinydashboard::box(
                            title = "Prior Sensitivity Analysis",
                            width = 12,
                            solidHeader = TRUE,
                            status = "primary",
                            # Detailed explanation of Prior Sensitivity Analysis.
                            tagList(
                                p(
                                    "This analysis helps to understand how sensitive the posterior distribution is to the choice of prior. ",
                                    "It is performed using the method of power-scaling perturbations implemented in the ",
                                    code("priorsense"), " package. This technique evaluates how much the posterior changes as we ",
                                    "adjust the influence of the prior and likelihood through power-scaling. "
                                ),
                                p(
                                    "The sensitivity is quantified using the ", strong("Cumulative Jensen-Shannon distance (CJS distance)"),
                                    ", a symmetric metric that measures the divergence between cumulative distribution functions (CDFs). ",
                                    "High sensitivity values indicate that the posterior is influenced by changes in the prior and/or likelihood."
                                ),
                                p(
                                    strong("Diagnostic interpretation: "), "Based on the calculated sensitivity values, diagnostics are provided:",
                                    tags$ul(
                                        tags$li(strong("Potential prior-data conflict: "), "Both prior and likelihood sensitivities are high, indicating possible disagreement between them. "),
                                        tags$li(strong("Potential strong prior / weak likelihood: "), "Prior sensitivity is high while likelihood sensitivity is low, suggesting that the prior dominates the posterior. "),
                                        tags$li(strong("- (dash): "), "No notable sensitivity detected. This often implies robustness to prior specification.")
                                    )
                                ),
                                p("For more detail see", a(href = "https://doi.org/10.1007/s11222-023-10366-5", "https://doi.org/10.1007/s11222-023-10366-5", target = "_blank")),
                                p(
                                    "These diagnostics are not definitive signs of modeling issues, but rather informative tools. ",
                                    "If you intended your prior to be informative, its strong influence might be acceptable. ",
                                    "However, if the priors were chosen arbitrarily or without much consideration, these results may prompt a reassessment of the prior settings."
                                ),
                                p(
                                    strong("Visualization: "), "The ", code("powerscale_plot_dens()"), " function shows how the posterior density shifts ",
                                    "as the influence of the prior and likelihood are adjusted. Overlapping density lines suggest low sensitivity, ",
                                    "while widely separated lines indicate higher sensitivity. Dashed lines warn that estimates may be unreliable due to high Pareto k values."
                                ),
                                p(strong("References:")),
                                p(
                                    "Kallioinen N, Paananen T, Bürkner P, Vehtari A (2023). ", em("“Detecting and diagnosing prior and likelihood sensitivity with power-scaling.”"),
                                    " Statistics and Computing, 34. ",
                                    a(href = "https://doi.org/10.1007/s11222-023-10366-5", "https://doi.org/10.1007/s11222-023-10366-5", target = "_blank")
                                )
                            ),
                            selectizeInput("sensitivity_params_results", "Select Parameters for Sensitivity Analysis:", choices = NULL, multiple = TRUE, width = "100%", options = list(maxItems = 5, placeholder = "Select up to 5 parameters"))
                        ),
                        verbatimTextOutput("prior_sensitivity_output_results"),
                        plotOutput("prior_sensitivity_plot_results")
                    ),
                    ### ===== PREDICTION =====
                    tabPanel(
                        "Predictions",
                        style = "overflow-y: auto; max-height: 85vh; padding-right: 6px;",
                        # UI for displaying predictions from the original data.
                        shinydashboard::box(
                            title = "Data Prediction", width = 12, solidHeader = TRUE, status = "primary",
                            DT::DTOutput("prediction_results"),
                            downloadButton("download_results", "Download")
                        ),
                        # UI for predicting on new, user-entered data.
                        shinydashboard::box(
                            title = "Input Data for Prediction", width = 12, solidHeader = TRUE, status = "primary",
                            actionButton("add_row", "Add Row", icon = icon("plus"), class = "btn-success"),
                            actionButton("remove_row", "Remove Row", icon = icon("trash"), class = "btn-danger"),
                            br(), br(),
                            DT::DTOutput("prediction_input_table"),
                            br(), br(),
                            actionButton("predict_new", "Predict", icon = icon("play"), class = "btn-primary")
                        )
                    ),
                    ## ===== SAVE OUTPUTS SUB-TAB ====
                    tabPanel(
                        "Save Outputs",
                        downloadButton("save_model", "Save Model (RDS)"),
                        downloadButton("save_stan_code", "Save Stan Code"),
                        downloadButton("save_coda", "Save CODA Samples"),
                        downloadButton("save_plots", "Save Plots (PDF)")
                    )
                )
            )
        )
    )
)


# ==== SERVER =====
# Defines the server-side logic of the Shiny application.
server <- function(input, output, session) {
    # ==== REACTIVE VALUES INITIALIZATION ====
    # These reactive values store the application's state and data.
    data <- reactiveVal() # Stores the uploaded dataset.
    new_data <- reactiveVal(NULL)
    spatial_weights <- reactiveVal(NULL) # Stores the spatial weights matrix.
    model_fit <- reactiveVal(NULL) # Stores the fitted brms model object.
    prior_fit <- reactiveVal(NULL) # Stores the prior predictive checking model object.
    prior_list <- reactiveVal(character()) # Stores user-defined prior strings.
    pred_data <- reactiveVal(NULL) # Stores the data for new predictions.
    check_results <- reactiveVal(NULL)
    ppc_result <- reactiveVal(NULL)

    observe({
        if (!is.null(model_fit())) {
            output$update_menu <- shinydashboard::renderMenu({
                shinydashboard::menuItem("Update Model", tabName = "update_model", icon = icon("sync"))
            })

            if (!"update_model" %in% sapply(shiny::isolate(input$tabs), as.character)) {
                insertUI(
                    selector = ".tab-content", # selector CSS valid di dashboardBody
                    where = "beforeEnd",
                    ui = shinydashboard::tabItem(
                        tabName = "update_model",
                        fluidRow(
                            shinydashboard::box(
                                title = "Update Model", width = 12, solidHeader = TRUE, status = "primary",
                                fileInput("update_newdata", "Upload New Data *Optional:", accept = c("text/csv", ".csv", ".xlsx", ".xls", ".rda", ".RData")),
                                numericInput("update_num_chains", "Chains", value = 1, min = 1),
                                numericInput("update_num_cores", "Cores", value = 1, min = 1),
                                numericInput("update_num_iter", "Iterations", value = 4000, min = 1),
                                numericInput("update_num_warmup", "Warmup", value = 2000, min = 1),
                                sliderInput("update_adapt_delta", "Adapt Delta", min = 0.8, max = 0.99, value = 0.95),
                                actionButton("update_run", "Run Model Update", icon = icon("refresh"), class = "btn-primary")
                            )
                        )
                    )
                )
            }
        } else {
            output$update_menu <- shinydashboard::renderMenu(NULL)
        }
    })



    # ==== DATA UPLOAD HANDLING ====
    # This observer triggers when a file is uploaded.
    observeEvent(input$data_file, {
        req(input$data_file) # Ensure a file is uploaded before proceeding
        ext <- tools::file_ext(input$data_file$name) # Get file extension
        output$data_upload_error <- renderUI(NULL) # Reset any previous error messages
        withProgress(message = "Uploading data...", {
            # Try to read the file based on its extension.
            tryCatch(
                withCallingHandlers(
                    {
                        # Read file based on extension
                        df <- switch(ext,
                            "csv" = read.csv(input$data_file$datapath,
                                header = TRUE,
                                sep = ",", quote = "", fill = TRUE, check.names = FALSE,
                                na.strings = c("", "NA", "NULL")
                            ),
                            "xls" = readxl::read_excel(input$data_file$datapath),
                            "xlsx" = readxl::read_excel(input$data_file$datapath),
                            "rda" = {
                              e <- new.env()
                              load(input$data_file$datapath, envir = e)
                              obj_names <- ls(envir = e)
                              e[[obj_names[1]]]  # Ambil objek pertama
                            },
                            "RData" = {
                              e <- new.env()
                              load(input$data_file$datapath, envir = e)
                              obj_names <- ls(envir = e)
                              e[[obj_names[1]]]
                            }
                        )

                        # Clean column names to be valid R variable names.
                        colnames(df) <- gsub("[^A-Za-z0-9_]", "", colnames(df))
                        df_numeric <- df[, sapply(df, is.numeric)]

                        # Update all UI selection inputs with the columns from the new data.
                        updateSelectInput(session, "explore_var_summary", choices = names(df_numeric))
                        updateSelectInput(session, "explore_var_hist", choices = names(df_numeric))
                        updateSelectInput(session, "explore_var_boxplot", choices = names(df_numeric))
                        updateSelectInput(session, "explore_var_scatter_x", choices = names(df_numeric))
                        updateSelectInput(session, "explore_var_scatter_y", choices = names(df_numeric))
                        updateSelectInput(session, "explore_var_scatter_color", choices = c("None" = "", names(df)))
                        updateSelectInput(session, "response_var", choices = names(df_numeric))
                        updateSelectizeInput(session, "linear_aux_vars", choices = names(df_numeric), server = TRUE)
                        updateSelectizeInput(session, "nonlinear_aux_vars", choices = names(df_numeric), server = TRUE)
                        updateSelectizeInput(session, "re_groups", choices = names(df), server = TRUE)
                        updateSelectInput(session, "trials_var", choices = c("None" = "none", names(df_numeric)))
                        updateSelectInput(session, "sre", choices = c("None" = "none", names(df)))
                        updateSelectInput(session, "trials_logit", choices = c("None" = "none", names(df_numeric)))
                        updateSelectInput(session, "n_beta", choices = c("None" = "none", names(df_numeric)))
                        updateSelectInput(session, "deff_beta", choices = c("None" = "none", names(df_numeric)))

                        # Store the data and display a preview.
                        data(df)
                        output$data_preview <- DT::renderDT({
                            DT::datatable(df, options = list(scrollX = TRUE, scrollY = "370px"))
                        })
                    },
                    warning = function(w) {
                        showNotification(paste("Data Upload Warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning") # Supaya warning tidak muncul di console
                    },
                    message = function(m) {
                        showNotification(paste("Data Upload Message:", conditionMessage(m)), type = "message", duration = 10)
                        invokeRestart("muffleMessage")
                    }
                ),
                # Handle errors during file upload and processing.
                error = function(e) {
                    # Display error notification
                    showNotification(paste("Data Upload Error:", e$message), type = "error", duration = 10)
                    # Clear reactive data and UI elements
                    data(NULL)
                    output$data_preview <- DT::renderDT(NULL)
                    output$data_upload_error <- renderUI(
                        div(class = "alert alert-danger", paste("Upload Error:", e$message))
                    )
                    # Reset all UI inputs if an error occurs.
                    updateSelectInput(session, "explore_var_summary", choices = NULL)
                    updateSelectInput(session, "explore_var_hist", choices = NULL)
                    updateSelectInput(session, "explore_var_boxplot", choices = NULL)
                    updateSelectInput(session, "explore_var_scatter_x", choices = NULL)
                    updateSelectInput(session, "explore_var_scatter_y", choices = NULL)
                    updateSelectInput(session, "explore_var_scatter_color", choices = NULL)
                    updateSelectInput(session, "response_var", choices = NULL)
                    updateSelectizeInput(session, "linear_aux_vars", choices = NULL, server = TRUE)
                    updateSelectizeInput(session, "nonlinear_aux_vars", choices = NULL, server = TRUE)
                    updateSelectizeInput(session, "re_groups", choices = NULL, server = TRUE)
                    updateSelectInput(session, "trials_var", choices = NULL)
                    updateSelectInput(session, "sre", choices = NULL)
                    updateSelectInput(session, "trials_logit", choices = NULL)
                    updateSelectInput(session, "n_beta", choices = NULL)
                    updateSelectInput(session, "deff_beta", choices = NULL)
                    updateSelectizeInput(session, "sensitivity_params_results", choices = NULL, server = TRUE)
                }
            )
        })
    })


    # ==== RESET OBSERVERS ====
    # Reset specific inputs when related settings change to avoid invalid states.
    # Reset variables when distribution type changes
    observeEvent(input$dist_type, {
        updateSelectInput(session, "trials_var", selected = "none")
        updateSelectInput(session, "trials_logit", selected = "none")
        updateSelectInput(session, "n_beta", selected = "none")
        updateSelectInput(session, "deff_beta", selected = "none")
    })

    # Reset spatial random effects (SRE) variable when spatial type changes
    observeEvent(input$sre_type, {
        updateSelectInput(session, "sre", selected = "none")
    })

    # Reset trials variable when HB family changes
    observeEvent(input$hb_family, {
        updateSelectInput(session, "trials_var", selected = "none")
    })


    # ==== DATA EXPLORATION ====
    # Renders the summary statistics for the selected variable.
    ## ===== SUMMARY =====
    output$numeric_summary <- renderPrint({
        req(data(), input$explore_var_summary)
        summary(data()[[input$explore_var_summary]])
    })
    ## ===== HISTOGRAM =====
    # Renders a histogram for the selected variable.
    output$histogram_plot <- renderPlot({
        req(data(), input$explore_var_hist)
        p <- ggplot2::ggplot(data(), ggplot2::aes_string(x = input$explore_var_hist)) +
            ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
            ggplot2::geom_density(alpha = 0.2, fill = "#FF6666") +
            ggplot2::labs(title = paste("Histogram of", input$explore_var_hist), x = input$explore_var_hist, y = "Density") +
            ggplot2::theme_minimal()
        withCallingHandlers(
            {
                print(p)
            },
            warning = function(w) {
                showNotification(paste("Histogram Warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning") # Supaya warning tidak muncul di console
            },
            message = function(m) {
                showNotification(paste("Histogram Message:", conditionMessage(m)), type = "message", duration = 10)
                invokeRestart("muffleMessage")
            }
        )
    })
    ## ===== BOXPLOT ====
    # Renders a boxplot for the selected variable.
    output$boxplot_plot <- renderPlot({
        req(data(), input$explore_var_boxplot)
        p <- ggplot2::ggplot(data(), ggplot2::aes_string(y = input$explore_var_boxplot)) +
            ggplot2::geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
            ggplot2::labs(title = paste("Boxplot of", input$explore_var_boxplot), y = input$explore_var_boxplot) +
            ggplot2::theme_minimal() +
            ggplot2::coord_flip()
        withCallingHandlers(
            {
                print(p)
            },
            warning = function(w) {
                showNotification(paste("Boxplot Warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning") # Supaya warning tidak muncul di console
            },
            message = function(m) {
                showNotification(paste("Boxplot Message:", conditionMessage(m)), type = "message", duration = 10)
                invokeRestart("muffleMessage")
            }
        )
    })
    ## ===== TRANSFORMASI DATA =====
    # Reactive expression to transform data for the scatter plot based on user selection (log, logit).
    scatter_data_transformed <- reactive({
        req(data(), input$explore_var_scatter_x, input$explore_var_scatter_y)
        df <- data()
        x <- input$explore_var_scatter_x
        y <- input$explore_var_scatter_y

        epsilon <- 1e-6

        # Y-axis transformation
        if (input$explore_transform_y == "log") {
            if (any(df[[y]] <= 0, na.rm = TRUE)) {
                showNotification("Some Y values ≤ 0 replaced with small positive value before log transform.", type = "message", duration = 10)
            }
            df[[paste0(y, "_log")]] <- log(pmax(df[[y]], epsilon))
            y <- paste0(y, "_log")
        } else if (input$explore_transform_y == "logit") {
            if (any(df[[y]] <= 0 | df[[y]] >= 1, na.rm = TRUE)) {
                showNotification("Y values outside (0,1) adjusted before logit transform.", type = "message", duration = 10)
            }
            df[[paste0(y, "_logit")]] <- qlogis(pmin(pmax(df[[y]], epsilon), 1 - epsilon))
            y <- paste0(y, "_logit")
        } else if (input$explore_transform_y == "zscore") {
            mu <- mean(df[[y]], na.rm = TRUE)
            sigma <- sd(df[[y]], na.rm = TRUE)
            df[[paste0(y, "_std")]] <- (df[[y]] - mu) / sigma
            y <- paste0(y, "_std")
        }

        # X-axis transformation
        if (input$explore_transform_x == "log") {
            if (any(df[[x]] <= 0, na.rm = TRUE)) {
                showNotification("Some X values ≤ 0 replaced with small positive value before log transform.", type = "message", duration = 10)
            }
            df[[paste0(x, "_log")]] <- log(pmax(df[[x]], epsilon))
            x <- paste0(x, "_log")
        } else if (input$explore_transform_x == "zscore") {
            mu <- mean(df[[x]], na.rm = TRUE)
            sigma <- sd(df[[x]], na.rm = TRUE)
            df[[paste0(x, "_std")]] <- (df[[x]] - mu) / sigma
            x <- paste0(x, "_std")
        }
        list(data = df, x = x, y = y)
    })


    ## ===== SCATTER PLOT =====
    # Renders the scatter plot.
    output$scatter_plot_exploration <- renderPlot({
        s <- scatter_data_transformed()
        req(s$data, s$x, s$y)
        # Avoid plotting a variable against itself without transformation.
        if (s$x == s$y && input$explore_transform_x == "none" && input$explore_transform_y == "none") {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, "Select different variables or apply a transformation", cex = 1.2)
            return()
        }
        p <- ggplot2::ggplot(s$data, ggplot2::aes_string(x = s$x, y = s$y))
        # Add color aesthetic if a categorical variable is selected.
        if (!is.null(input$explore_var_scatter_color) && input$explore_var_scatter_color != "") {
            p <- p + ggplot2::geom_point(ggplot2::aes_string(color = paste0("as.factor(", input$explore_var_scatter_color, ")")),
                alpha = 0.7, size = 2.5
            ) + ggplot2::labs(color = input$explore_var_scatter_color)
        } else {
            p <- p + ggplot2::geom_point(color = "darkcyan", alpha = 0.6, size = 2.5)
        }
        p <- p + ggplot2::geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "dashed") +
            ggplot2::labs(title = paste("Scatter Plot:", s$y, "vs", s$x), x = s$x, y = s$y) +
            ggplot2::theme_minimal()
        withCallingHandlers(
            {
                suppressMessages(print(p))
            },
            warning = function(w) {
                showNotification(paste("Scatter Plot Warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning") # Supaya warning tidak muncul di console
            },
            message = function(m) {
                showNotification(paste("Scatter Plot Message:", conditionMessage(m)), type = "message", duration = 10)
                invokeRestart("muffleMessage")
            }
        )
    })

    ## ===== CORRELATION ====
    # Calculates and renders various correlation coefficients.
    output$correlation_results <- renderUI({
        s_data_list <- scatter_data_transformed()
        req(s_data_list$data, s_data_list$y, s_data_list$x)

        df_corr <- s_data_list$data
        y_vec <- df_corr[[s_data_list$y]]
        x_vec <- df_corr[[s_data_list$x]]

        complete_cases <- stats::complete.cases(x_vec, y_vec)
        x_vec <- x_vec[complete_cases]
        y_vec <- y_vec[complete_cases]

        if (length(x_vec) < 3 || length(y_vec) < 3) {
            return(h4("Not enough complete observations to compute correlations."))
        }

        if (s_data_list$x == s_data_list$y &&
            input$explore_transform_x == "none" &&
            input$explore_transform_y == "none") {
            return(h4("Please select different variables or apply a transformation."))
        }

        # Compute correlations
        results <- list()
        results[["Pearson's r"]] <- tryCatch(
            {
                pearson <- stats::cor.test(x_vec, y_vec, method = "pearson")
                c(sprintf("%.4f", pearson$estimate), sprintf("%.4f", pearson$p.value))
            },
            error = function(e) c("Error", e$message)
        )

        results[["Spearman's rho"]] <- tryCatch(
            {
                spearman <- suppressWarnings(stats::cor.test(x_vec, y_vec, method = "spearman", exact = FALSE))
                c(sprintf("%.4f", spearman$estimate), sprintf("%.4f", spearman$p.value))
            },
            error = function(e) c("Error", e$message)
        )

        results[["Chatterjee's Xi"]] <- tryCatch(
            {
                xi <- XICOR::xicor(x_vec, y_vec, pvalue = TRUE)
                c(sprintf("%.4f", xi$xi), sprintf("%.4f", xi$pval))
            },
            error = function(e) c("Error", e$message)
        )

        results[["Distance Correlation"]] <- tryCatch(
            {
                dcor <- energy::dcor.test(x_vec, y_vec, R = 199)
                c(sprintf("%.4f", dcor$estimate[1]), sprintf("%.4f", dcor$p.value))
            },
            error = function(e) c("Error", e$message)
        )

        results[["Maximal Information Coefficient (MIC)"]] <- tryCatch(
            {
                mic <- minerva::mine(x_vec, y_vec)
                c(sprintf("%.4f", mic$MIC), "N/A")
            },
            error = function(e) c("Error", e$message)
        )

        df_out <- data.frame(
            Metric = names(results),
            Value = sapply(results, `[[`, 1),
            `p-value` = sapply(results, `[[`, 2),
            check.names = FALSE
        )

        # Output combined: dynamic title + table
        tagList(
            h5(paste0("Correlation between variables ", s_data_list$y, " and ", s_data_list$x)),
            tableOutput("correlation_table")
        ) -> output_ui

        output$correlation_table <- renderTable(df_out)
        output_ui
    })


    # ==== MISSING DATA CHECKING ====
    # This complex observer watches all variable selection inputs.
    observe({
        req(data(), input$dist_type)
        df <- data()
        available_vars <- names(df)

        # It re-evaluates whenever a model variable selection changes.
        observeEvent(
            c(
                input$response_var, input$linear_aux_vars, input$nonlinear_aux_vars,
                input$trials_var, input$trials_logit, input$n_beta,
                input$deff_beta, input$re_groups, input$sre
            ),
            {
                # Collect all unique variables selected by the user.
                selected_vars <- unique(c(
                    input$response_var, input$linear_aux_vars,
                    input$nonlinear_aux_vars, input$trials_var,
                    input$trials_logit, input$n_beta,
                    input$deff_beta, input$re_groups, input$sre
                ))
                selected_vars <- selected_vars[selected_vars %in% available_vars]

                # Check for missing values (NA) in the selected variables.
                missing_info <- sapply(selected_vars, function(col) sum(is.na(df[[col]]), na.rm = TRUE))
                missing_info <- missing_info[missing_info > 0]

                # Categorize variables with missing data based on their role.
                missing_aux_response <- intersect(names(missing_info), c(input$response_var, input$linear_aux_vars, input$nonlinear_aux_vars))
                missing_re <- intersect(names(missing_info), input$re_groups)
                missing_sre <- intersect(names(missing_info), input$sre)
                missing_trials_var <- intersect(names(missing_info), input$trials_var)
                missing_trials_logit <- intersect(names(missing_info), input$trials_logit)
                missing_n_beta <- intersect(names(missing_info), input$n_beta)
                missing_deff_beta <- intersect(names(missing_info), input$deff_beta)

                # Set reactive flags to conditionally show UI elements.
                output$missing_detected <- reactive({
                    length(missing_info) > 0
                })
                outputOptions(output, "missing_detected", suspendWhenHidden = FALSE)

                output$missing_aux_response_detected <- reactive({
                    length(missing_aux_response) > 0
                })
                outputOptions(output, "missing_aux_response_detected", suspendWhenHidden = FALSE)

                # Render a list of variables that contain missing values.
                output$missing_vars_list <- renderUI({
                    if (length(missing_info) > 0) {
                        div(
                            h5("Selected Variables with Missing Values:"),
                            HTML(paste0("<ul>", paste(sprintf("<li>%s [ %d Observations ]</li>", names(missing_info), missing_info), collapse = " "), " </ul>"))
                        )
                    }
                })

                # Set a default method for handling missing data based on the chosen distribution.
                default_method <- "none"
                if (length(missing_aux_response) > 0) {
                    default_method <- switch(input$dist_type,
                        "Lognormal-Lognormal"   = "model",
                        "Binomial-Logitnormal" = "multiple",
                        "Beta-Logitnormal"        = "model",
                        "none"
                    )
                }
                updateSelectInput(session, "missing_data_method", selected = default_method)

                # Display a note about the default missing data handling method.
                output$missing_data_note <- renderUI({
                    if (input$dist_type %in% c("Lognormal-Lognormal", "Binomial-Logitnormal", "Beta-Logitnormal")) {
                        msg <- switch(input$dist_type,
                            "Lognormal-Lognormal"   = "Default: Model-based imputation",
                            "Binomial-Logitnormal" = "Default: Multiple imputation",
                            "Beta-Logitnormal"        = "Default: Model-based imputation"
                        )
                        div(style = "color: blue; font-weight: bold;", msg)
                    }
                })

                # Display specific warnings or errors if crucial variables have missing data.
                output$missing_re_warning <- renderUI({
                    if (length(missing_re) > 0) {
                        div(style = "color: orange; font-weight: bold;", "Warning: Missing values detected in grouping variable. Rows with missing values will be automatically removed.")
                    }
                })
                output$missing_sre_warning <- renderUI({
                    if (length(missing_sre) > 0) {
                        div(style = "color: orange; font-weight: bold;", "Warning: Missing values detected in spatial random effects (SRE). Rows with missing values will be automatically removed.")
                    }
                })
                output$missing_trials_var_error <- renderUI({
                    if (length(missing_trials_var) > 0) {
                        div(style = "color: red; font-weight: bold;", "Error: The count of trials for the Binomial distribution contains missing values. This information is required and must be fully specified.")
                    }
                })
                output$missing_trials_logit_error <- renderUI({
                    if (length(missing_trials_logit) > 0) {
                        div(style = "color: red; font-weight: bold;", "Error: The count of trials for the Logit distribution contains missing values. This information is required and must be fully specified.")
                    }
                })
                output$missing_n_beta_error <- renderUI({
                    if (length(missing_n_beta) > 0) {
                        div(style = "color: red; font-weight: bold;", "Error: The sample size (n) for the Beta distribution contains missing values. This information is required and must be fully specified.")
                    }
                })
                output$missing_deff_beta_error <- renderUI({
                    if (length(missing_deff_beta) > 0) {
                        div(style = "color: red; font-weight: bold;", "Error: The design effect contains missing values. This information is required and must be fully specified.")
                    }
                })
            },
            ignoreNULL = FALSE,
            ignoreInit = FALSE
        )
    })


    # ==== INPUT DATA SPATIAL ====
    observeEvent(input$spatial_weights, {
        req(input$spatial_weights)
        output$spatial_error <- renderUI(NULL) # Reset any previous error messages

        tryCatch(
            {
                ext <- tools::file_ext(input$spatial_weights$name)

                # Baca file sesuai ekstensi
                mat_df <- switch(ext,
                                 "csv" = read.csv(input$spatial_weights$datapath,
                                                  header = TRUE, row.names = 1, check.names = FALSE),
                                 "xls" = readxl::read_excel(input$spatial_weights$datapath),
                                 "xlsx" = readxl::read_excel(input$spatial_weights$datapath),
                                 "rda" = {
                                   e <- new.env()
                                   load(input$spatial_weights$datapath, envir = e)
                                   obj_names <- ls(envir = e)
                                   obj <- e[[obj_names[1]]]
                                   obj
                                 },
                                 "RData" = {
                                   e <- new.env()
                                   load(input$spatial_weights$datapath, envir = e)
                                   obj_names <- ls(envir = e)
                                   obj <- e[[obj_names[1]]]
                                   obj
                                 },
                                 stop("Unsupported file type: must be .csv, .xls, .xlsx, .rda, or .RData")
                )

                # Jika Excel, atur rownames dari kolom pertama
                if (ext %in% c("xls", "xlsx")) {
                    mat_df <- as.data.frame(mat_df)
                    rownames(mat_df) <- as.character(mat_df[[1]])
                    mat_df[[1]] <- NULL
                }

                # Konversi ke matriks
                M <- as.matrix(mat_df)

                # Simpan matriks valid
                spatial_weights(M)
                showNotification("Spatial weights successfully loaded.", type = "message", duration = 10)
            },
            error = function(e) {
                spatial_weights(NULL)
                output$spatial_error <- renderUI(
                    div(class = "alert alert-danger", paste("Upload Error:", e$message))
                )
                showNotification(paste("Spatial Weights Error:", e$message), type = "error", duration = 15)
            }
        )
    })



    # ==== PRIOR HANDLING ====
    # Function to validate the syntax of a user-entered prior string.
    validate_prior_format <- function(prior_str) {
        prior_str <- trimws(prior_str)
        if (prior_str == "") {
            return("Prior string cannot be empty.")
        }
        if (!grepl("^prior\\(.*\\)$", prior_str) && !grepl("^set_prior\\(.*\\)$", prior_str)) {
            return("Invalid prior format. Use 'prior(...)' or 'set_prior(...)'")
        }
        return(NULL)
    }

    # Adds a validated prior to the list of priors.
    observeEvent(input$add_prior, {
        req(input$prior_input)
        new_prior <- trimws(input$prior_input)
        error_message <- validate_prior_format(new_prior)
        if (!is.null(error_message)) {
            showNotification(error_message, type = "error", duration = 15)
            return()
        }
        current_priors <- prior_list()
        if (new_prior %in% current_priors) {
            showNotification("Duplicate prior detected!", type = "warning", duration = 10)
            return()
        }
        updated_priors <- c(current_priors, new_prior)
        prior_list(updated_priors)
        updateTextInput(session, "prior_input", value = "")
    })

    # Clears all defined priors.
    observeEvent(input$clear_prior, {
        prior_list(character(0))
        showNotification("All priors cleared.", type = "message", duration = 10)
    })

    # Displays the current list of priors.
    output$prior_list <- renderText({
        priors <- prior_list()
        if (length(priors) == 0) {
            return("No priors defined. Model will use brms defaults.")
        }
        paste(priors, collapse = "\n")
    })

    # Reactive expression that combines the prior strings into a single brms prior object.
    reactive_prior <- reactive({
        priors <- prior_list()
        if (length(priors) == 0) {
            return(NULL)
        }
        tryCatch(
            {
                combined_expr <- paste(priors, collapse = " + ")
                eval(parse(text = combined_expr))
            },
            error = function(e) {
                showNotification(paste("Error parsing priors:", e$message), type = "error", duration = 15)
                NULL
            }
        )
    })


    # ==== PRIOR PREDICTIVE CHECKING RESULTS ====
    # Renders all outputs for the prior predictive checking sub-tab.
    observe({
        req(prior_fit())
        tryCatch(
            {
                # Calls a custom function to process prior check results.
                prior_check <- hbpc(prior_fit(), data(), input$response_var)

                # Renders the prior summary.
                output$prior_summary <- renderPrint({
                    withCallingHandlers(
                        {
                            cat("================================== Prior Summary ==================================\n")
                            print(prior_check$prior_summary)
                        },
                        warning = function(w) {
                            showNotification(paste("Prior Summary Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Prior Summary Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    )
                })

                # Renders a summary of the draws from the prior distributions.
                output$prior_parameter_draw <- renderPrint({
                    withCallingHandlers(
                        {
                            cat("================================== Summary of Parameter Draws from Prior ==================================\n")
                            print(prior_check$prior_draws_summary)
                        },
                        warning = function(w) {
                            showNotification(paste("Parameter Draws Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Parameter Draws Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    )
                })

                # Renders the prior predictive plot.
                output$prior_predictive_plot <- renderPlot({
                    withCallingHandlers(
                        {
                            print(prior_check$prior_predictive_plot)
                        },
                        warning = function(w) {
                            showNotification(paste("Prior Predictive Plot Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Prior Predictive Plot Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    )
                })
            },
            error = function(e) {
                showNotification(paste("Prior Check Error:", e$message), type = "error", duration = 15)
            }
        )
    })


    # ==== FORMULA CONSTRUCTION ====
    # Reactive expression to dynamically build the model formula based on user inputs.
    reactive_formula <- reactive({
        req(data(), input$response_var)
        output$model_fitting_error <- renderUI(NULL)

        y_var <- input$response_var

        # Start with the base formula: response ~ predictors.
        formula_str <- paste(y_var, "~")
        formula_str <- if (length(input$linear_aux_vars) > 0) {
            paste(formula_str, paste(input$linear_aux_vars, collapse = "+"))
        } else {
            "1" # Intercept-only model if no predictors.
        }

        # # Nonlinear terms (splines or Gaussian processes)
        # if (length(input$nonlinear_aux_vars) > 0) {
        #     nonlinear_terms <- if (input$model_type == "Spline") {
        #         paste0("s(", input$nonlinear_aux_vars, ")") # For splines
        #     } else {
        #         paste0("gp(", input$nonlinear_aux_vars, ")") # For Gaussian processes
        #     }
        #     formula_str <- paste(formula_str, "+", paste(nonlinear_terms, collapse = "+"))
        # }

        # Special syntax for binomial family with a 'trials' variable.
        if (input$hb_family == "binomial") {
            req(input$trials_var)
            formula_str <- paste0(y_var, "|trials(", input$trials_var, ") ~ ", gsub(paste0(y_var, " ~ "), "", formula_str))
        }

        # If model-based imputation is selected, modify the formula for brms.
        if (input$missing_data_method == "model") {
            selected_vars <- c(input$response_var, input$linear_aux_vars, input$nonlinear_aux_vars)
            missing_vars <- selected_vars[sapply(data()[selected_vars], function(x) any(is.na(x)))]

            # Use `mi()` syntax for variables with missing data.
            response_with_mi <- ifelse(input$response_var %in% missing_vars, paste0(input$response_var, " | mi()"), input$response_var)
            all_predictors <- c(input$linear_aux_vars, input$nonlinear_aux_vars)
            predictor_with_mi <- sapply(all_predictors, function(x) {
                if (x %in% missing_vars) {
                    return(paste0("mi(", x, ")"))
                } else {
                    return(x)
                }
            })

            # Create the main formula.
            main_formula <- brms::bf(stats::as.formula(paste0(response_with_mi, " ~ ", paste(predictor_with_mi, collapse = " + "))))

            # Create auxiliary formulas to model the missing predictors themselves.
            auxiliary_formulas <- lapply(missing_vars, function(var) {
                if (var == input$response_var) {
                    return(NULL)
                } # Don't model the response variable.
                predictors_for_var <- setdiff(all_predictors, var)
                predictors_for_var <- predictors_for_var[!sapply(data()[predictors_for_var], function(x) any(is.na(x)))]
                if (length(predictors_for_var) > 0) {
                    return(brms::bf(stats::as.formula(paste0(var, " | mi() ~ ", paste(predictors_for_var, collapse = " + ")))))
                } else {
                    return(brms::bf(stats::as.formula(paste0(var, " | mi() ~ 1"))))
                }
            })
            auxiliary_formulas <- Filter(Negate(is.null), auxiliary_formulas)

            # Combine main and auxiliary formulas.
            if (length(auxiliary_formulas) > 0) {
                all_formulas <- Reduce("+", c(list(main_formula), auxiliary_formulas))
            } else {
                all_formulas <- main_formula
            }
        } else {
            # If no imputation, use the standard formula.
            all_formulas <- brms::bf(formula_str)
        }
    })

    # Reactive expression for the random effects part of the formula.
    reactive_formula_re <- reactive({
        if (!is.null(input$re_groups)) {
            stats::as.formula(paste("~", paste("(1 | ", input$re_groups, ")", collapse = " + ")))
        } else {
            NULL
        }
    })

    # ==== MODEL FITTING & PRIOR CHECKING ====
    # A single function to handle model fitting for different scenarios.
    fit_model_function <- function(sample_prior_mode) {
        # Collect all common arguments for the model fitting functions.
        common_args <- list(
            sre = if (input$sre == "none") NULL else input$sre,
            sre_type = if (input$sre_type == "none") NULL else input$sre_type,
            sar_type = if (input$sre_type == "none") NULL else input$sar_type,
            car_type = if (input$sre_type == "none") NULL else input$car_type,
            data = data(),
            M = if (input$sre_type == "none") NULL else spatial_weights(),
            handle_missing = if (input$missing_data_method == "none") NULL else input$missing_data_method,
            m = input$m_value,
            prior = reactive_prior(),
            chains = input$num_chains,
            seed = input$num_seed,
            iter = input$num_iter,
            warmup = input$num_warmup,
            cores = input$num_cores,
            thin = input$num_thin,
            control = list(adapt_delta = input$adapt_delta),
            refresh = 0, # Suppress printing MCMC progress to the console.
            sample_prior = sample_prior_mode, # "only" for prior check, "no" for model fit.
            save_model = "model.stan"
        )

        # Call the appropriate modeling function from the hbsaems package based on distribution type.
        if (input$dist_type == "Custom") {
            common_args$re <- reactive_formula_re()
            common_args$formula <- reactive_formula()
            common_args$hb_sampling <- input$hb_family
            common_args$hb_link <- input$hb_link
            fit <- do.call(hbm, common_args)
        } else if (input$dist_type == "Lognormal-Lognormal") {
            common_args$response <- input$response_var
            common_args$predictors <- input$linear_aux_vars
            common_args$group <- input$re_groups
            fit <- do.call(hbm_lnln, common_args)
        } else if (input$dist_type == "Binomial-Logitnormal") {
            common_args$trials <- if (input$trials_logit == "none") NULL else input$trials_logit
            common_args$response <- input$response_var
            common_args$predictors <- input$linear_aux_vars
            common_args$group <- input$re_groups
            fit <- do.call(hbm_binlogitnorm, common_args)
        } else if (input$dist_type == "Beta-Logitnormal") {
            common_args$n <- if (input$n_beta == "none") NULL else input$n_beta
            common_args$deff <- if (input$deff_beta == "none") NULL else input$deff_beta
            common_args$response <- input$response_var
            common_args$predictors <- input$linear_aux_vars
            common_args$group <- input$re_groups
            fit <- do.call(hbm_betalogitnorm, common_args)
        }
        return(fit)
    }

    ## ==== MODEL FITTING ====
    # Observer to trigger the main model fitting process.
    observeEvent(input$fit_model, {
        req(input$fit_model)
        withProgress(message = "Fitting model...", {
            tryCatch(withCallingHandlers(
                {
                    # Set sample_prior to 'no' for posterior sampling.
                    invisible(capture.output(
                        suppressMessages(
                            fit <- fit_model_function(sample_prior_mode = "no")
                        )
                    ))
                    model_fit(fit) # Store the fitted model.
                    showNotification("Model fitting completed!", type = "message", duration = 10)
                    # Update parameter list for sensitivity analysis.
                    updateSelectizeInput(session, "sensitivity_params_results", choices = posterior::variables(model_fit()$model), server = TRUE)
                },
                warning = function(w) {
                    showNotification(paste("Model Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Model Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Model Error:", e$message), type = "error", duration = 15)
                model_fit(NULL)
            })
        })
    })

    ## ==== PRIOR CHECKING ====
    # Observer to trigger prior predictive checking.
    observeEvent(input$prior_check, {
        req(input$prior_check)
        withProgress(message = "Prior predictive checking...", {
            tryCatch(withCallingHandlers(
                {
                    # Set sample_prior to 'only' to sample from the prior.
                    invisible(capture.output(
                        suppressMessages(
                            fit <- fit_model_function(sample_prior_mode = "only")
                        )
                    ))
                    prior_fit(fit) # Store the prior fit object.
                    showNotification("Prior Predictive Checking completed!", type = "message", duration = 10)
                },
                warning = function(w) {
                    showNotification(paste("Prior Predictive Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Prior Predictive Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Prior Predictive Checking Error:", e$message), type = "error", duration = 15)
                prior_fit(NULL)
            })
        })
    })


    # ==== RESULT =====
    # ==== MODEL SUMMARY ====
    # Renders the main model summary.
    output$model_summary <- renderPrint({
        req(model_fit())
        withCallingHandlers(
            {
                cat("================================ Model Summary ================================\n")
                print(model_fit()$model)
            },
            warning = function(w) {
                showNotification(paste("Model Summary Warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) {
                showNotification(paste("Model Summary Message:", conditionMessage(m)), type = "message", duration = 10)
                invokeRestart("muffleMessage")
            }
        )
    })

    # Renders the summary of the priors used in the final model.
    output$model_prior <- renderPrint({
        req(model_fit())
        withCallingHandlers(
            {
                cat("================================ Prior Summary ================================\n")
                print(brms::prior_summary(model_fit()$model))
            },
            warning = function(w) {
                showNotification(paste("Model Prior Warning:", conditionMessage(w)), type = "warning", duration = 10)
                invokeRestart("muffleWarning")
            },
            message = function(m) {
                showNotification(paste("Model Prior Message:", conditionMessage(m)), type = "message", duration = 10)
                invokeRestart("muffleMessage")
            }
        )
    })


    # ==== CONVERGENCE DIAGNOSTICS ====
    convergence_result <- reactive({ # Name changed for clarity
        req(model_fit())
        withProgress(message = "Convergence test running...", {
            tryCatch(withCallingHandlers(
                {
                    diag_results <- hbcc(model = model_fit())
                },
                warning = function(w) {
                    showNotification(paste("Convergence Warning:", conditionMessage(w)), type = "warning", duration = 15)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Convergence Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Error in Convergent:", e$message), type = "error", duration = 15)
                return(NULL)
            })
        })
    })

    # ==== CONVERGENCE NUMERIC ====
    output$diag_numerical <- renderPrint({
        diag_results <- convergence_result()
        switch(input$diag_tests,
            "rhat" = {
                cat("\n==================== R-hat Statistics ====================\n")
                print(diag_results$rhat_ess)
            },
            "geweke" = {
                cat("\n================== Geweke Diagnostics ====================\n")
                print(diag_results$geweke)
            },
            "raftery" = {
                cat("\n================ Raftery-Lewis Diagnostics ===============\n")
                print(diag_results$raftery)
            },
            "heidel" = {
                cat("\n============= Heidelberger-Welch Diagnostics =============\n")
                print(diag_results$heidel)
            },
            { # default case
                cat("No diagnostics selected.")
            }
        )
    })


    # ====CONVERGENCE PLOT ====
    # Renders graphical convergence diagnostics (trace plots, density plots, etc.)
    output$diag_plots <- renderPlot({
        req(convergence_result())
        diag_results <- convergence_result()
        selected_plot <- input$plot_types
        if (!is.null(diag_results$plots[[selected_plot]])) {
            suppressMessages(
                print(diag_results$plots[[selected_plot]])
            )
        } else {
            plot.new()
            text(0.5, 0.5, "No plot available for this selection", cex = 1.5)
            return(NULL)
        }
    })


    # ==== MODEL CHECKING ====
    check_results <- reactive({ # Name changed for clarity
        req(model_fit())
        withProgress(message = "Checking model goodness of fit...", value = 0.5, {
            tryCatch(withCallingHandlers(
                {
                    result <- suppressWarnings(
                        hbmc(model = model_fit(), ndraws_ppc = 10, moment_match = input$moment_match_check)
                    )
                },
                warning = function(w) {
                    showNotification(paste("Model Checking Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Model Checkin Message:", conditionMessage(m)), type = "message", duration = 5)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Model Checking Error:", e$message), type = "error", duration = 10)
                check_results(NULL)
            })
        })
    })

    # ==== MODEL RESULT ====
    output$model_check_metrics_output <- renderPrint({
        req(input$results_tabs == "model_checking_panel")
        req(check_results())
        res <- check_results()
        if ("loo" %in% input$gof_metrics_select && !is.null(res$primary_model_diagnostics$loo)) {
            cat("==================== Leave-One-Out (LOO) ====================\n")
            cat(res$primary_model_diagnostics$loo$warnings)
            cat("\n\n")
            print(res$primary_model_diagnostics$loo$estimates)
        }
        if ("waic" %in% input$gof_metrics_select && !is.null(res$primary_model_diagnostics$waic)) {
            cat("=============== Widely Applicable Information Criteria (WAIC) ===============\n")
            print(res$primary_model_diagnostics$waic$estimates)
        }
    })

    # ==== MODEL PLOT ====
    # Renders graphical posterior predictive checks.
    observe({
        req(model_fit())
        output$model_check_plot_output <- renderPlot({
            selected_plot <- input$pp_check_type_select
            y_obs <- model_fit()$model$data[[input$response_var]]
            # Generate replicated data from the posterior predictive distribution.
            y_rep <- brms::posterior_predict(model_fit()$model, ndraws = input$pp_samples_slider)

            if (is.null(y_obs) || is.null(y_rep)) {
                plot.new()
                text(0.5, 0.5, "Missing y_obs or yrep", cex = 1.5)
                return()
            }
            withProgress(message = "Making Plot...", {
                tryCatch(withCallingHandlers(
                    {
                        # Use bayesplot functions to create the selected plot.
                        plot <- switch(selected_plot,
                            "dens_overlay" = bayesplot::ppc_dens_overlay(y = y_obs, yrep = y_rep),
                            "boxplot" = bayesplot::ppc_boxplot(y = y_obs, yrep = y_rep, notch = FALSE),
                            "bars" = {
                                if (all(y_obs == floor(y_obs))) {
                                    bayesplot::ppc_bars(y = y_obs, yrep = y_rep)
                                } else {
                                    plot.new()
                                    text(0.5, 0.5, "'ppc_bars' requires discrete response", cex = 1.5)
                                    return(NULL)
                                }
                            },
                            "scatter_avg" = bayesplot::ppc_scatter_avg(y = y_obs, yrep = y_rep),
                            "stat" = {
                                req(input$pp_check_stat_select)
                                bayesplot::ppc_stat(y = y_obs, yrep = y_rep, stat = input$pp_check_stat_select)
                            },
                            {
                                plot.new()
                                text(0.5, 0.5, "Unknown or unsupported plot type", cex = 1.5)
                            }
                        )
                        suppressMessages(print(plot))
                        ppc_result(plot)
                    },
                    warning = function(w) {
                        showNotification(paste("PPC Warning:", conditionMessage(w)), type = "warning", duration = 8)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) {
                        showNotification(paste("PPC Message:", conditionMessage(m)), type = "message", duration = 8)
                        invokeRestart("muffleMessage")
                    }
                ), error = function(e) {
                    showNotification(paste("PPC Error:", conditionMessage(e)), type = "error", duration = 10)
                    plot.new()
                    text(0.5, 0.5, "Error generating plot", cex = 1.5)
                })
            })
        })
    })


    # ===== PRIOR SENSITIVITY =====
    # Reactive expression to run prior sensitivity analysis once
    prior_sens_result <- reactive({
        req(input$sensitivity_params_results)
        withProgress(message = "Prior sensitivity running...", {
            tryCatch(withCallingHandlers(
                {
                    hbmc(
                        model = model_fit(),
                        plot_types = c(),
                        comparison_metrics = c(),
                        run_prior_sensitivity = TRUE,
                        sensitivity_vars = input$sensitivity_params_results
                    )
                },
                warning = function(w) {
                    showNotification(paste("Prior Sensitivity Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Prior Sensitivity Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Error during prior sensitivity analysis:", e$message), type = "error", duration = 15)
                return(NULL)
            })
        })
    })

    # Output: numerical result
    output$prior_sensitivity_output_results <- renderPrint({
        res <- prior_sens_result()
        if (is.null(res)) {
            cat("Please choose at least one parameter for sensitivity analysis.")
        } else {
            print(res$prior_sensitivity_results$model1$result)
        }
    })

    # Output: plot result
    output$prior_sensitivity_plot_results <- renderPlot({
        res <- prior_sens_result()
        validate(need(!is.null(res), "No result available."))
        print(res$prior_sensitivity_results$model1$plot)
    })


    # ==== PREDICTION ====
    # Renders the prediction results for the original data.
    output$prediction_results <- DT::renderDT({
        req(model_fit())
        withProgress(message = "Load prediction data...", {
            withCallingHandlers(
                {
                    # Calls a custom function to get predictions.
                    preds <- hbsae(model_fit())
                    DT::datatable(round(preds$result_table,4), options = list(scrollX = TRUE, scrollY = "370px"))
                },
                warning = function(w) {
                    showNotification(paste("Prediction Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Prediction Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            )
        })
    })


    # ==== PREDICTION FOR NEW DATA ====
    # Initializes an empty, editable data frame for new predictions.
    observe({
        req(input$response_var)
        pred_data(NULL)
        model_vars <- unique(stats::na.omit(c(
            input$linear_aux_vars, input$nonlinear_aux_vars, input$re_groups,
            if (input$trials_var != "none") input$trials_var else NULL,
            if (input$trials_logit != "none") input$trials_logit else NULL,
            if (input$n_beta != "none") input$n_beta else NULL,
            if (input$deff_beta != "none") input$deff_beta else NULL
        )))
        df <- data.frame(matrix(NA, nrow = 1, ncol = length(model_vars) + 1))
        colnames(df) <- c(model_vars, "prediction_result")
        pred_data(df)
    })

    # Adds a new empty row to the prediction input table.
    observeEvent(input$add_row, {
        req(model_fit())
        df <- pred_data()
        df <- rbind(df, stats::setNames(data.frame(matrix(NA, ncol = ncol(df))), names(df)))
        pred_data(df)
    })

    # Removes the last row from the prediction input table.
    observeEvent(input$remove_row, {
        req(model_fit())
        df <- pred_data()
        pred_data(if (nrow(df) > 1) df[-nrow(df), ] else df * NA)
    })

    # Renders the editable table for new data input.
    output$prediction_input_table <- DT::renderDT({
        req(pred_data())
        DT::datatable(pred_data(),
            selection = "none",
            editable = list(target = "cell", disable = list(columns = ncol(pred_data())))
        )
    })

    # Saves user edits from the editable table back to the reactive value.
    observeEvent(input$prediction_input_table_cell_edit, {
        info <- input$prediction_input_table_cell_edit
        df <- pred_data()
        df[info$row, info$col] <- as.numeric(info$value)
        pred_data(df)
    })

    # Triggers prediction on the new data when the "Predict" button is clicked.
    observeEvent(input$predict_new, {
        req(model_fit())
        withProgress(message = "Prediction start...", {
            tryCatch(withCallingHandlers(
                {
                    df <- pred_data()[, !names(pred_data()) %in% "prediction_result", drop = FALSE]
                    df$prediction_result <- hbsae(model_fit(), newdata = df)$pred
                    pred_data(round(df,4))
                },
                warning = function(w) {
                    showNotification(paste("Prediction Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Prediction Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Prediction Error:", e$message), type = "error", duration = 15)
            })
        })
    })


    # ==== DOWNLOAD HANDLERS ====
    output$download_results <- downloadHandler(
        filename = function() {
            paste0("predictions_.csv")
        },
        content = function(file) {
            req(model_fit())
            withProgress(message = "Downloading table...", {
                tryCatch(withCallingHandlers(
                    {
                        preds <- hbsae(model_fit()) # Fungsi kamu untuk prediksi
                        write.csv(preds$result_table, file, row.names = FALSE)
                    },
                    warning = function(w) {
                        showNotification(paste("Download Warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning")
                    },
                    message = function(m) {
                        showNotification(paste("Download Message:", conditionMessage(m)), type = "message", duration = 10)
                        invokeRestart("muffleMessage")
                    }
                ), error = function(e) {
                    showNotification(paste("Download Error:", e$message), type = "error", duration = 15)
                })
            })
        }
    )

    output$save_model <- downloadHandler(
        filename = function() {
            "model_fit.rds"
        },
        content = function(file) {
            withProgress(message = "Downloading model...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            saveRDS(model_fit()$model, file)
                            showNotification("Download completed!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Download Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Download Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    ),
                    error = function(e) {
                        showNotification(paste("Download Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )

    output$save_stan_code <- downloadHandler(
        filename = function() {
            "stan_code.txt"
        },
        content = function(file) {
            withProgress(message = "Downloading Stan code...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            writeLines(brms::stancode(model_fit()$model), file)
                            showNotification("Download completed!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Download Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Download Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    ),
                    error = function(e) {
                        showNotification(paste("Download Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )

    output$save_coda <- downloadHandler(
        filename = function() {
            "coda_samples.rds"
        },
        content = function(file) {
            withProgress(message = "Downloading CODA samples...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            saveRDS(brms::as.mcmc(model_fit()$model), file)
                            showNotification("Download completed!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Download Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Download Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    ),
                    error = function(e) {
                        showNotification(paste("Download Error:", e$message), type = "error", duration = 15)
                    }
                )
            })
        }
    )
    output$save_plots <- downloadHandler(
        filename = function() {
            "diagnostic_plots.pdf"
        },
        content = function(file) {
            withProgress(message = "Generating diagnostic plots...", {
                tryCatch(
                    withCallingHandlers(
                        {
                            suppressMessages(
                                grDevices::pdf(file),
                                print(ppc_result()),
                                print(prior_sens_result()$prior_sensitivity_results$model1$plot),
                                print(convergence_result()$plots),
                                grDevices::dev.off()
                            )
                            showNotification("Download completed!", type = "message", duration = 10)
                        },
                        warning = function(w) {
                            showNotification(paste("Download Warning:", conditionMessage(w)), type = "warning", duration = 10)
                            invokeRestart("muffleWarning")
                        },
                        message = function(m) {
                            showNotification(paste("Download Message:", conditionMessage(m)), type = "message", duration = 10)
                            invokeRestart("muffleMessage")
                        }
                    ),
                    error = function(e) {
                        showNotification(paste("Download Error:", e$message), type = "error", duration = 15)
                        return(NULL)
                    }
                )
            })
        }
    )


    # ==== UPDATING MODEL ====
    observeEvent(input$update_newdata, {
        req(input$update_newdata)
        ext <- tools::file_ext(input$update_newdata$name)
        output$newdata_upload_error <- renderUI(NULL) # Reset any previous error messages
        withProgress(message = "Uploading newdata...", {
            # Try to read the file based on its extension.
            tryCatch(
                withCallingHandlers(
                    {
                        # Read file based on extension
                      # Read file based on extension
                      df <- switch(ext,
                                   "csv" = read.csv(input$update_newdata$datapath,
                                                    header = TRUE,
                                                    sep = ",", quote = "", fill = TRUE, check.names = FALSE,
                                                    na.strings = c("", "NA", "NULL")
                                   ),
                                   "xls" = readxl::read_excel(input$update_newdata$datapath),
                                   "xlsx" = readxl::read_excel(input$update_newdata$datapath),
                                   "rda" = {
                                     e <- new.env()
                                     load(input$update_newdata$datapath, envir = e)
                                     obj_names <- ls(envir = e)
                                     obj <- e[[obj_names[1]]]
                                     obj
                                   },
                                   "RData" = {
                                     e <- new.env()
                                     load(input$update_newdata$datapath, envir = e)
                                     obj_names <- ls(envir = e)
                                     obj <- e[[obj_names[1]]]
                                     obj
                                   },
                                   stop("Unsupported file type: must be .csv, .xls, .xlsx, .rda, or .RData")
                      )
                      

                        # Clean column names to be valid R variable names.
                        colnames(df) <- gsub("[^A-Za-z0-9_]", "", colnames(df))
                        # Store the data and display a preview.
                        new_data(as.data.frame(df))
                        showNotification("New data loaded successfully", type = "message", duration = 10)
                    },
                    warning = function(w) {
                        showNotification(paste("Newdata Upload Warning:", conditionMessage(w)), type = "warning", duration = 10)
                        invokeRestart("muffleWarning") # Supaya warning tidak muncul di console
                    },
                    message = function(m) {
                        showNotification(paste("Newdata Upload Message:", conditionMessage(m)), type = "message", duration = 10)
                        invokeRestart("muffleMessage")
                    }
                ),
                # Handle errors during file upload and processing.
                error = function(e) {
                    # Display error notification
                    showNotification(paste("Newdata Upload Error:", e$message), type = "error", duration = 10)
                    # Clear reactive data and UI elements
                    new_data(NULL)
                    output$newdata_upload_error <- renderUI(
                        div(class = "alert alert-danger", paste("Upload Error:", e$message))
                    )
                }
            )
        })
    })


    observeEvent(input$update_run, {
        req(model_fit())

        withProgress(message = "Updating model...", {
            tryCatch(withCallingHandlers(
                {
                    updated_model <- suppressMessages(update_hbm(
                        model   = model_fit(),
                        newdata = new_data(), # will be NULL if not uploaded
                        iter    = input$update_num_iter,
                        warmup  = input$update_num_warmup,
                        chains  = input$update_num_chains,
                        cores   = input$update_num_cores,
                        control = list(adapt_delta = input$update_adapt_delta)
                    ))

                    # Store updated model
                    model_fit(updated_model)
                    showNotification("Model update completed successfully!", type = "message", duration = 10)
                },
                warning = function(w) {
                    showNotification(paste("Model Update Warning:", conditionMessage(w)), type = "warning", duration = 10)
                    invokeRestart("muffleWarning")
                },
                message = function(m) {
                    showNotification(paste("Model Update Message:", conditionMessage(m)), type = "message", duration = 10)
                    invokeRestart("muffleMessage")
                }
            ), error = function(e) {
                showNotification(paste("Model Update Error:", e$message), type = "error", duration = NULL)
            })
        })
    })


    # ==== DYNAMIC UI TEXT ====
    # These blocks render descriptive text in the UI based on user selections.
    ## ===== DIAGNOTICS NUMERIC =====
    output$diag_definition <- renderUI({
        switch(input$diag_tests,
            "rhat" = shinydashboard::box(
                title = "R-hat Diagnostic", width = NULL, solidHeader = TRUE, status = "info",
                p(
                    "The ", strong("R-hat (Gelman-Rubin)"),
                    " diagnostic measures the convergence of MCMC chains by comparing between- and within-chain variances. ",
                    "If chains have not mixed well (i.e., the estimates differ across chains), ", strong("R-hat"), " will be greater than ", code("1"), ". ",
                    "Values below ", code("1.05"), " are generally considered acceptable. ",
                    "We recommend running at least four chains to compute R-hat reliably."
                )
            ),
            "geweke" = shinydashboard::box(
                title = "Geweke Diagnostic", width = NULL, solidHeader = TRUE, status = "info",
                p(
                    "The ", strong("Geweke"),
                    " diagnostic compares the mean of the first 10% and the last 50% of a Markov chain. ",
                    "If the chain has reached its stationary distribution, the two means should be similar, resulting in a Z-score near ", code("0"), ". ",
                    "Z-scores outside the range ", code("[-1.96, 1.96]"), " may indicate lack of convergence at the 5% significance level."
                )
            ),
            "raftery" = shinydashboard::box(
                title = "Raftery-Lewis Diagnostic", width = NULL, solidHeader = TRUE, status = "info",
                p(
                    "The ", strong("Raftery-Lewis"),
                    " diagnostic estimates the number of iterations required to estimate a specified posterior quantile to a desired level of accuracy and confidence. ",
                    "We recommend ensuring that the total number of iterations (", code("N"), ") for each parameter meets or exceeds the value suggested by this diagnostic—",
                    "especially for parameters that are critical to your inference."
                )
            ),
            "heidel" = shinydashboard::box(
                title = "Heidelberger-Welch Diagnostic", width = NULL, solidHeader = TRUE, status = "info",
                p(
                    "The ", strong("Heidelberger-Welch"),
                    " diagnostic tests for stationarity and convergence using statistical methods applied to the MCMC sample. ",
                    "It is typically evaluated at a significance level of ", code("0.05"), ". ",
                    "If a parameter fails the stationarity test, it suggests that the Markov chain has not yet reached a stable distribution. ",
                    "In such cases, you may need to run the chain longer to achieve reliable inference."
                )
            ),
            shinydashboard::box(
                title = "Information", width = NULL, solidHeader = TRUE, status = "warning",
                p("Please select a diagnostic test to see the explanation here.")
            )
        )
    })

    ## ===== DIAGNOTICS PLOTS =====
    ## ===== DIAGNOSTIC PLOTS =====
    output$plot_definition <- renderUI({
        switch(input$plot_types,
            "trace" = shinydashboard::box(
                title = "Trace Plot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "Trace plots display the sampled values of each parameter across iterations for each MCMC chain. ",
                    "They are used to assess ", strong("mixing"), " and ", strong("convergence"), " — well-mixed chains appear as 'hairy caterpillars' without long-term trends."
                )
            ),
            "dens" = shinydashboard::box(
                title = "Density Plot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "Density plots show the estimated ", strong("posterior distributions"), " of parameters from the MCMC samples. ",
                    "They help visualize the central tendency and spread of each parameter."
                )
            ),
            "acf" = shinydashboard::box(
                title = "Autocorrelation Plot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "Autocorrelation plots display the correlation of sampled values with their lagged values. ",
                    "High autocorrelation indicates poor mixing and a lower ", strong("effective sample size"), ". ",
                    "Ideally, autocorrelation should drop quickly with increasing lag."
                )
            ),
            "nuts_energy" = shinydashboard::box(
                title = "NUTS Energy Plot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "NUTS (No-U-Turn Sampler) energy plots are used to diagnose ", strong("Hamiltonian Monte Carlo"), " performance. ",
                    "They show the distribution of energy transitions between states. ",
                    "Significant discrepancies between energy and momentum can indicate sampling problems such as poor step size or divergences."
                )
            ),
            "rhat" = shinydashboard::box(
                title = "R-hat Plot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "R-hat plots visualize the ", strong("Gelman-Rubin R-hat diagnostic"), " across parameters. ",
                    "Values close to ", code("1.00"), " suggest convergence. ",
                    "Values above ", code("1.05"), " may indicate that the chains have not fully mixed."
                )
            ),
            "neff" = shinydashboard::box(
                title = "Effective Sample Size (ESS) Plot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "ESS plots show the ", strong("effective sample size"), " for each parameter, reflecting how many independent draws are equivalent to the correlated samples. ",
                    "Higher ESS values indicate better sampling efficiency and more reliable estimates."
                )
            ),
            shinydashboard::box(
                title = "Information", width = NULL, solidHeader = TRUE, status = "warning",
                p("Please select a diagnostic plot type to see the explanation here.")
            )
        )
    })

    ## ===== MODEL CHECKING NUMERIC =====
    output$metrics_definition_box <- renderUI({
        switch(input$gof_metrics_select,
            "loo" = shinydashboard::box(
                title = "LOO (Leave-One-Out Cross-Validation)",
                width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "LOO estimates out-of-sample predictive accuracy by systematically leaving out one observation at a time, ",
                    "then computing the log-predictive density for the omitted point. ",
                    "It is computed efficiently using ", strong("Pareto-smoothed importance sampling (PSIS)"), "."
                ),
                p(
                    "An important diagnostic in LOO is the ", strong("Pareto k diagnostic"), ", which measures the reliability of the importance sampling approximation. ",
                    "Values of ", code("k > 0.7"), " indicate that the approximation may be unstable for certain observations."
                ),
                p(
                    "If high k values are present, we recommend enabling ", code("moment_match = TRUE"), " to improve the stability of the estimate using moment matching. ",
                    "In this package, if k values remain high after moment matching, it will automatically fall back to ", code("reloo = TRUE"),
                    " which recomputes LOO using full model refitting for those problematic observations."
                )
            ),
            "waic" = shinydashboard::box(
                title = "WAIC (Widely Applicable Information Criterion)",
                width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "WAIC evaluates model predictive performance by adjusting the log pointwise predictive density (lppd) ",
                    "using the effective number of parameters to account for overfitting. "
                ),
                p("It is a fully Bayesian criterion, asymptotically equivalent to LOO, but can be less robust in small samples or with weak priors. "),
                p("Lower ", strong("WAIC"), " values indicate better expected predictive performance.")
            )
        )
    })

    ## ===== MODEL CHECKING PLOTS =====
    output$ppc_definition_box <- renderUI({
        switch(input$pp_check_type_select,
            "dens_overlay" = shinydashboard::box(
                title = "Density Overlay", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "This plot overlays the density of observed data with densities from the posterior predictive simulations. ",
                    "Good overlap between the two indicates that the model can capture the overall shape and spread of the observed data distribution."
                )
            ),
            "boxplot" = shinydashboard::box(
                title = "Boxplot", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "Boxplots compare the distribution of observed data against replicated (simulated) datasets. ",
                    "It is useful for assessing central tendency, spread, and outliers—especially when the data contains skewness or variability across groups."
                )
            ),
            "bars" = shinydashboard::box(
                title = "Frequency Plot (Discrete)", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "For discrete outcomes, this bar plot compares the frequency of observed values with the frequencies ",
                    "predicted by the posterior predictive distribution. A close match suggests the model captures the categorical structure well."
                )
            ),
            "scatter_avg" = shinydashboard::box(
                title = "Scatter Plot (y vs yrep)", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "Plots the observed values (y) against the average of the replicated values (yrep) from the posterior. ",
                    "Points close to the identity line (diagonal) suggest the model has good predictive accuracy for individual observations."
                )
            ),
            "stat" = shinydashboard::box(
                title = "Statistic vs Data", width = NULL, solidHeader = TRUE, status = "primary",
                p(
                    "Displays the distribution of a summary statistic (such as mean, standard deviation, min, max) ",
                    "computed from replicated data (yrep), and compares it to the same statistic calculated from the observed data. ",
                    "Helps assess whether the model reproduces specific aspects of the data's distribution."
                )
            )
        )
    })
}


# ==== RUN APP ====
# Combines the UI and Server logic to launch the Shiny application.
shinyApp(ui, server)
