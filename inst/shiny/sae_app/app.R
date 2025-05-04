ui <- dashboardPage(
    dashboardHeader(title = "Small Area Estimation App"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
            menuItem("Modeling", tabName = "modeling", icon = icon("cogs")),
            menuItem("Results", tabName = "results", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "data_upload",
                box(
                    title = "Upload CSV or Excel File", width = 12, solidHeader = TRUE, status = "primary",
                    fileInput("data_file", " ",
                        accept = c("text/csv", ".csv", ".xlsx", ".xls")
                    )
                ),
                box(
                    title = "Data Preview", width = 12, solidHeader = TRUE, status = "primary",
                    DT::DTOutput("data_preview")
                ),
                uiOutput("missing_data_alert"),
                uiOutput("data_upload_error")
            ),
            tabItem(
                tabName = "modeling",
                  style = "overflow-y: auto; max-height: 90vh; padding-right: 6px;",
                    box(
                        title = "Model Variables", width = 12, solidHeader = TRUE,
                        column(6, selectInput("direct_var", "Direct Estimation Variable", choices = NULL)),
                        column(6, selectizeInput("linear_aux_vars", "Linear Auxiliary Variables", choices = NULL, multiple = TRUE)),
                        conditionalPanel(
                            condition = "input.dist_type == 'Custom'",
                            column(6, selectizeInput("nonlinear_aux_vars", "Nonlinear Auxiliary Variables", choices = NULL, multiple = TRUE))
                        ),
                        column(6, selectizeInput("re_groups", "Random Effect Group Variables", choices = NULL, multiple = TRUE)),
                        column(6, selectInput("sre_type", "Spatial Type",
                            choices = c(
                                "None" = "none",
                                "SAR" = "sar",
                                "CAR" = "car"
                            )
                        )),
                        column(6, conditionalPanel(
                            condition = "input.sre_type == 'sar'",
                            selectInput("sar_type", "SAR Type",
                                choices = c("Lag" = "lag", "Error" = "error"),
                                selected = "lag"
                            )
                        )),
                        column(6, conditionalPanel(
                            condition = "input.sre_type == 'car'",
                            selectInput("car_type", "CAR Type",
                                choices = c(
                                    "eSCAR" = "escar", "eSICAR" = "esicar",
                                    "ICAR" = "icar", "BYM2" = "bym2"
                                ),
                                selected = "escar"
                            )
                        )),
                        column(6, conditionalPanel(
                            condition = "input.sre_type == 'sar' || input.sre_type == 'car'",
                            selectInput("sre", "Spatial Random Effects", choices = NULL),
                            fileInput("spatial_weights", "Upload Spatial Weights (CSV)")
                        )),
                        column(6, conditionalPanel(
                            condition = "input.sre_type != 'none'",
                            uiOutput("spatial_weights_error")
                        ))
                    ),
                    box(
                        title = "Model Constructions", width = 12, solidHeader = TRUE,
                        column(6, selectInput("dist_type", "Distribution Type",
                            choices = c("Custom", "Lognormal", "Logitnormal", "Beta"),
                            selected = "Custom"
                        )),
                        column(6, conditionalPanel(
                            condition = "input.dist_type == 'Logitnormal'",
                            selectInput("trials_logit", "Trials Variable", choices = NULL)
                        )),
                        column(6, conditionalPanel(
                            condition = "input.dist_type == 'Beta'",
                            selectInput("n_beta", "Sample Size (n)", choices = NULL, multiple = FALSE),
                            selectInput("deff_beta", "Design Effect (deff)", choices = NULL, multiple = FALSE)
                        )),
                        conditionalPanel(
                            condition = "input.dist_type == 'Custom'",
                            column(6, selectInput("model_type", "Model Type for Nonlinear Terms", choices = c("Spline", "Gaussian Process"), selected = "Spline")),
                            column(6, selectInput("hb_family", "HB Family",
                                choices = c("gaussian", "binomial", "poisson", "gamma", "beta", "lognormal", "skew_normal", "shifted_lognormal", "negbinomial", "beta_binomial", "logistic_normal", "hurdle_poisson", "hurdle_negbinomial", "hurdle_gamma", "hurdle_lognormal", "zero_inflated_beta", "zero_inflated_poisson", "zero_inflated_negbinomial", "zero_inflated_binomial", "zero_inflated_beta_binomial", "weibull", "frechet", "gen_extreme_value")
                            )),
                            column(6, conditionalPanel(
                                condition = "input.hb_family == 'binomial'",
                                selectInput("trials_var", "Trials Variable", choices = NULL)
                            )),
                            column(6, selectInput("hb_link", "HB Link", choices = c("identity", "log", "logit", "probit", "cloglog"))),
                        ),
                        conditionalPanel(
                            condition = "output.missing_detected == true",
                            box(
                                title = "Missing Detected", width = 12, solidHeader = TRUE, status = "warning",
                                column(6, uiOutput("missing_vars_list")),
                                conditionalPanel(
                                  condition = "output.missing_aux_response_detected == true",
                                  column(6, selectInput("missing_data_method", "Handle Missing Data",
                                                        choices = c(
                                                          "None" = "none",
                                                          "Deleted" = "deleted",
                                                          "Model" = "model",
                                                          "Multiple" = "multiple"
                                                        )
                                  ))
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
                    ),
                    box(
                        title = "MCMC Settings", width = 12, solidHeader = TRUE,
                        column(6, numericInput("num_chains", "Chains", value = 4, min = 1)),
                        column(6, numericInput("num_cores", "Cores", value = 1, min = 1)),
                        column(6, numericInput("num_thin", "Thinning rate", value = 1, min = 1)),
                        column(6, numericInput("num_iter", "Iterations", value = 4000, min = 1)),
                        column(6, numericInput("num_warmup", "Warmup", value = 2000, min = 1)),
                        column(6, sliderInput("adapt_delta", "Adapt Delta", 0.8, 0.99, 0.95))
                    ),
                    box(
                        title = "Define Priors", width = 12, solidHeader = TRUE,
                        textInput("prior_input", "Enter Prior:", placeholder = 'e.g., prior(normal(0,5), class = "b")'),
                        actionButton("add_prior", "Add Prior", icon = icon("plus"), class = "btn-success"),
                        actionButton("clear_prior", "Clear Priors", icon = icon("trash"), class = "btn-danger"),
                        br(), br(),
                        verbatimTextOutput("prior_list"),
                        actionButton("prior_check", "Prior Predictive Checking", icon = icon("play"), class = "btn-info")
                    ),
                    fluidRow(
                        column(
                            12,
                            div(
                                style = "margin-left: 20px;",
                                actionButton("fit_model", "Fit Model", icon = icon("play"), class = "btn-primary")
                            )
                        )
                    ),
                    uiOutput("model_fitting_error")
            ),
            tabItem(
                tabName = "results",
                # Results After Prior Predictive Checking
                conditionalPanel(
                    condition = "output.show_prior_check == 'true'",
                    tabPanel(
                        "Prior predictive checking",
                        verbatimTextOutput("prior_summary"),
                        verbatimTextOutput("prior_convergent"),
                        plotOutput("data_vs_prior_dist")
                    )
                ),
                # Results After Model Fitting
                conditionalPanel(
                    condition = "output.show_model_fit == 'true'",
                    tabsetPanel(
                        tabPanel(
                            "Model Summary",
                            verbatimTextOutput("model_summary"),
                            verbatimTextOutput("model_prior")
                        ),
                        tabPanel(
                            "Convergence Diagnostics",
                            tabsetPanel(
                                tabPanel("Diagnostics Tests", fluidRow(
                                    column(3, wellPanel(pickerInput("diag_tests", "Select Diagnostic Test Types:",
                                        choices = c(
                                            "R-hat" = "rhat",
                                            "Geweke" = "geweke",
                                            "Raftery-Lewis" = "raftery",
                                            "Heidelberger-Welch" = "heidel"
                                        ),
                                        multiple = FALSE, selected = "rhat"
                                    ))),
                                    column(9, verbatimTextOutput("diag_numerical"))
                                )),
                                tabPanel("Diagnostics Plots", fluidRow(
                                    column(3, wellPanel(pickerInput("plot_types", "Select Diagnostic Plot Types:",
                                        choices = c(
                                            "Trace Plot" = "trace",
                                            "Density Plot" = "dens",
                                            "Autocorrelation Function (acf) Plot" = "acf",
                                            "NUTS Energy" = "nuts_energy",
                                            "R-hat Plot" = "rhat",
                                            "Effective Sample Size (ESS) Plot" = "neff"
                                        ),
                                        multiple = FALSE, selected = "trace"
                                    ))),
                                    column(9, plotOutput("diag_plots", height = "600px"))
                                ))
                            )
                        ),
                        tabPanel(
                            "Model Checking",
                            tabsetPanel(
                                tabPanel("Numerical Model Checks", fluidRow(
                                    column(3, wellPanel(pickerInput("num_check_methods", "Select Model Check Types:",
                                        choices = c(
                                            "LOO" = "loo",
                                            "WAIC" = "waic"
                                        ),
                                        multiple = FALSE, selected = "loo"
                                    ))),
                                    column(9, verbatimTextOutput("model_metrics"))
                                )),
                                tabPanel("Graphical Model Checks", fluidRow(
                                    column(3, wellPanel(
                                        pickerInput("graph_check_methods", "Select Model Check Plots:",
                                            choices = c(
                                                "Posterior Predictive Check" = "pp_check",
                                                "Parameter Distributions Plot" = "params"
                                            ),
                                            multiple = FALSE, selected = "pp_check"
                                        ),
                                        sliderInput("pp_samples", "Number of PP samples:", min = 10, max = 500, value = 50, step = 10)
                                    )),
                                    column(9, plotOutput("model_check_plot", height = "600px"))
                                ))
                            )
                        ),
                        tabPanel(
                            "Predictions",
                            style = "overflow-y: auto; max-height: 85vh; padding-right: 6px;",
                            column(12, wellPanel(pickerInput("scale_types", "Select Prediction Scale Types:",
                                                            choices = c(
                                                              "Response" = "response",
                                                              "Linear" = "linear",
                                                              "Linear Inverse" = "linear_inverse"
                                                            ),
                                                            multiple = FALSE, selected = "response"
                            ))),
                            box(
                              title = "Data Prediction", width = 12, solidHeader = TRUE, status = "primary",
                              DT::DTOutput("prediction_results"),
                              downloadButton("download_results", "Download")
                            ),
                            box(
                              title = "Input Data for Prediction", width = 12, solidHeader = TRUE, status = "primary",
                              actionButton("add_row", "Add Row", icon = icon("plus"), class = "btn-success"),
                              actionButton("remove_row", "Remove Row", icon = icon("trash"), class = "btn-danger"),
                              br(), br(),
                              DT::DTOutput("prediction_input_table"),
                              br(), br(),
                              actionButton("predict_new", "Predict", icon = icon("play"), class = "btn-primary")
                            )
                         
                        ),
                        tabPanel(
                            "Save Outputs",
                            downloadButton("save_model", "Save Model (RDS)"),
                            downloadButton("save_stan_code", "Save Stan Code"),
                            downloadButton("save_coda", "Save CODA Samples"),
                            downloadButton("save_plots", "Save Plots (PDF)")
                        )
                    )
                ),

                # Default before run model
                conditionalPanel(
                    condition = "output.show_prior_check == 'false' && output.show_model_fit == 'false'",
                    h4("No results can be displayed")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    # ==== Active Parameter ====
    data <- reactiveVal()
    spatial_weights <- reactiveVal(NULL)
    model_fit <- reactiveVal(NULL)
    convergence_plots <- reactiveVal(list())
    prior_list <- reactiveVal(character())
    pred_data <- reactiveVal(NULL)



    # ==== View Results handling ====
    # Reactive values to track state
    reactive_state <- reactiveValues(prior_check = FALSE, fit_model = FALSE)

    # When the "Prior Predictive Checking" button is clicked
    observeEvent(input$prior_check, {
        reactive_state$prior_check <- TRUE # Activate prior predictive checking
        reactive_state$fit_model <- FALSE # Ensure "Fit Model" is deactivated
    })

    # When the "Fit Model" button is clicked
    observeEvent(input$fit_model, {
        reactive_state$fit_model <- TRUE # Activate model fitting
        reactive_state$prior_check <- FALSE # Ensure "Prior Predictive Checking" is deactivated
    })

    # Output for controlling UI visibility based on the state
    output$show_prior_check <- reactive({
        if (reactive_state$prior_check) "true" else "false" # Returns TRUE if prior predictive checking is active
    })
    output$show_model_fit <- reactive({
        if (reactive_state$fit_model) "true" else "false" # Returns TRUE if model fitting is active
    })

    # Ensure outputOptions is used so that conditionalPanel can read the values correctly
    outputOptions(output, "show_prior_check", suspendWhenHidden = FALSE)
    outputOptions(output, "show_model_fit", suspendWhenHidden = FALSE)



    # ==== Data upload handling ====
    observeEvent(input$data_file, {
        req(input$data_file) # Ensure a file is uploaded before proceeding
        ext <- tools::file_ext(input$data_file$name) # Get file extension
        output$data_upload_error <- renderUI(NULL) # Reset any previous error messages

        tryCatch(
            {
                # Read file based on extension
                df <- switch(ext,
                    "csv"  = read.csv(input$data_file$datapath, header = TRUE, sep = ",", quote = "", fill = TRUE, check.names = FALSE, na.strings = c("", "NA")),
                    "xls"  = readxl::read_excel(input$data_file$datapath),
                    "xlsx" = readxl::read_excel(input$data_file$datapath)
                )

                # Clean column names by removing special characters
                colnames(df) <- gsub("[^A-Za-z0-9_]", "", colnames(df))

                # Update variable selection inputs
                updateSelectInput(session, "direct_var", choices = names(df))
                updateSelectizeInput(session, "linear_aux_vars", choices = names(df), server = TRUE)
                updateSelectizeInput(session, "nonlinear_aux_vars", choices = names(df), server = TRUE)
                updateSelectizeInput(session, "re_groups", choices = names(df), server = TRUE)
                updateSelectInput(session, "trials_var", choices = c("None" = "none", names(df)))
                updateSelectInput(session, "sre", choices = c("None" = "none", names(df)))
                updateSelectInput(session, "trials_logit", choices = c("None" = "none", names(df)))
                updateSelectInput(session, "n_beta", choices = c("None" = "none", names(df)))
                updateSelectInput(session, "deff_beta", choices = c("None" = "none", names(df)))

                # Store the uploaded data in a reactive value
                data(df)

                # Display a preview of the uploaded data
                output$data_preview <- DT::renderDT(datatable(df))
            },
            error = function(e) {
                # Display error notification
                showNotification(paste("Data Error:", e$message), type = "error")

                # Clear reactive data and UI elements
                data(NULL)
                output$data_preview <- DT::renderDT(NULL)
                output$data_upload_error <- renderUI(
                    div(class = "alert alert-danger", paste("Upload Error:", e$message))
                )

                # Reset variable selection inputs
                updateSelectInput(session, "direct_var", choices = NULL)
                updateSelectizeInput(session, "linear_aux_vars", choices = NULL)
                updateSelectizeInput(session, "nonlinear_aux_vars", choices = NULL)
                updateSelectizeInput(session, "re_groups", choices = NULL)
                updateSelectInput(session, "trials_var", choices = NULL)
                updateSelectInput(session, "sre", choices = NULL)
                updateSelectInput(session, "trials_logit", choices = NULL)
                updateSelectizeInput(session, "n_beta", choices = NULL)
                updateSelectizeInput(session, "deff_beta", choices = NULL)
            }
        )
    })
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



    # ==== Checking Missing Data ====
    observe({
      req(data(), input$dist_type)
      df <- data()
      available_vars <- names(df)
      
      observeEvent(
        c(
          input$direct_var, input$linear_aux_vars, input$nonlinear_aux_vars,
          input$trials_var, input$trials_logit, input$n_beta,
          input$deff_beta, input$re_groups, input$sre
        ),
        {
          # Collect selected variables that are present in the dataset
          selected_vars <- unique(c(
            input$direct_var, input$linear_aux_vars,
            input$nonlinear_aux_vars, input$trials_var,
            input$trials_logit, input$n_beta,
            input$deff_beta, input$re_groups, input$sre
          ))
          selected_vars <- selected_vars[selected_vars %in% available_vars]
          
          # Identify missing values in selected variables
          missing_info <- sapply(selected_vars, function(col) sum(is.na(df[[col]]), na.rm = TRUE))
          missing_info <- missing_info[missing_info > 0]
          
          # Categorize missing variables based on their role in the model
          missing_aux_response <- intersect(names(missing_info), c(input$direct_var, input$linear_aux_vars, input$nonlinear_aux_vars))
          missing_re <- intersect(names(missing_info), input$re_groups)
          missing_sre <- intersect(names(missing_info), input$sre)
          missing_trials_var <- intersect(names(missing_info), input$trials_var)
          missing_trials_logit <- intersect(names(missing_info), input$trials_logit)
          missing_n_beta <- intersect(names(missing_info), input$n_beta)
          missing_deff_beta <- intersect(names(missing_info), input$deff_beta)
          
          # Flag if any missing values are detected
          output$missing_detected <- reactive({
            length(missing_info) > 0
          })
          outputOptions(output, "missing_detected", suspendWhenHidden = FALSE)
          
          output$missing_aux_response_detected <- reactive({
            length(missing_aux_response) > 0
          })
          outputOptions(output, "missing_aux_response_detected", suspendWhenHidden = FALSE)
          
          # Display list of variables containing missing values
          output$missing_vars_list <- renderUI({
            if (length(missing_info) > 0) {
              div(
                h5("Selected Variables with Missing Values:"),
                HTML(paste0("<ul>", paste(
                  sprintf(
                    "<li>%s [ %d Observations ]</li>",
                    names(missing_info), missing_info
                  ),
                  collapse = " "
                ), " </ul>"))
              )
            }
          })
          
          # Set default missing data handling method based on distribution type
          default_method <- "none"
          if (length(missing_aux_response) > 0) {
            default_method <- switch(input$dist_type,
                                     "Lognormal"   = "model",   # Model-based imputation for Lognormal distribution
                                     "Logitnormal" = "multiple", # Multiple imputation for Logitnormal distribution
                                     "Beta"        = "model",   # Model-based imputation for Beta distribution
                                     "none"
            )
          }
          updateSelectInput(session, "missing_data_method", selected = default_method)
          
          # Display notes on missing data handling for specific distributions
          output$missing_data_note <- renderUI({
            if (input$dist_type %in% c("Lognormal", "Logitnormal", "Beta")) {
              msg <- switch(input$dist_type,
                            "Lognormal"   = "Default: Model-based imputation",
                            "Logitnormal" = "Default: Multiple imputation",
                            "Beta"        = "Default: Model-based imputation"
              )
              div(style = "color: blue; font-weight: bold;", msg)
            }
          })
          
          # Warning for missing values in random effect variables
          output$missing_re_warning <- renderUI({
            if (length(missing_re) > 0) {
              div(
                style = "color: orange; font-weight: bold;",
                "Warning: Missing values detected in random effect variables. Rows with missing values will be automatically removed."
              )
            }
          })
          
          # Warning for missing values in spatial random effects (SRE)
          output$missing_sre_warning <- renderUI({
            if (length(missing_sre) > 0) {
              div(
                style = "color: orange; font-weight: bold;",
                "Warning: Missing values detected in spatial random effects (SRE). Rows with missing values will be automatically removed."
              )
            }
          })
          
          # Error messages for missing required variables
          output$missing_trials_var_error <- renderUI({
            if (length(missing_trials_var) > 0) {
              div(
                style = "color: red; font-weight: bold;",
                "Error: The count of trials for the Binomial distribution contains missing values. This information is required and must be fully specified."
              )
            }
          })
          
          output$missing_trials_logit_error <- renderUI({
            if (length(missing_trials_logit) > 0) {
              div(
                style = "color: red; font-weight: bold;",
                "Error: The count of trials for the Logit distribution contains missing values. This information is required and must be fully specified."
              )
            }
          })
          
          output$missing_n_beta_error <- renderUI({
            if (length(missing_n_beta) > 0) {
              div(
                style = "color: red; font-weight: bold;",
                "Error: The sample size (n) for the Beta distribution contains missing values. This information is required and must be fully specified."
              )
            }
          })
          
          output$missing_deff_beta_error <- renderUI({
            if (length(missing_deff_beta) > 0) {
              div(
                style = "color: red; font-weight: bold;",
                "Error: The design effect contains missing values. This information is required and must be fully specified."
              )
            }
          })
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )
    })



    # ==== Spatial weights handling ====
    observeEvent(input$spatial_weights, {
        req(input$spatial_weights) # Ensure the spatial weights file is uploaded

        # Try to read and process the spatial weights matrix
        spatial_matrix <- tryCatch(
            {
                # Read CSV file with row names
                M <- read.csv(input$spatial_weights$datapath, row.names = 1, check.names = FALSE)

                # Convert the data frame to a numeric matrix
                M <- as.matrix(M)

                # Validate if matrix is numeric
                if (!is.numeric(M)) {
                    stop("The spatial weights matrix must contain only numeric values.")
                }

                # Validate if matrix is square
                if (nrow(M) != ncol(M)) {
                    stop("The spatial weights matrix must be square (rows = columns).")
                }

                # Return valid matrix
                M
            },
            error = function(e) {
                showNotification(paste("Error in spatial weights matrix:", e$message), type = "error")
                return(NULL) # Return NULL if an error occurs
            }
        )

        # Store the valid spatial weights matrix in a reactive value if not NULL
        if (!is.null(spatial_matrix)) {
            spatial_weights(spatial_matrix)
        }
    })




    # ==== Prior Input Handling ====
    # Function to validate user-defined priors
    validate_prior <- function(prior) {
        prior <- trimws(prior)

        # Attempt to parse the prior and catch any parsing errors
        parsed_prior <- tryCatch(
            eval(parse(text = prior)),
            error = function(e) {
                return(e$message)
            }
        )

        # Return an error message if parsing fails
        if (is.character(parsed_prior)) {
            return(paste("Invalid brms prior! Parsing error:", parsed_prior))
        }

        # Check if the parsed expression is a valid brms prior
        is_valid <- tryCatch(
            is.brmsprior(parsed_prior),
            error = function(e) {
                return(paste("is.brmsprior() error:", e$message))
            }
        )

        # Return any error from is.brmsprior()
        if (is.character(is_valid)) {
            return(is_valid)
        }

        # If the parsed expression is not a valid brms prior
        if (!is_valid) {
            return("Parsed expression is not a valid brms prior!")
        }

        return(NULL) # Jika semua validasi lolos
    }


    # Observe event when adding a new prior
    observeEvent(input$add_prior, {
        new_prior <- trimws(input$prior_input)
        error_message <- validate_prior(new_prior)

        if (!is.null(error_message)) {
            showNotification(error_message, type = "error")
            return(NULL)
        }

        current_priors <- prior_list()
        new_parsed_prior <- eval(parse(text = new_prior))

        # Check for duplicate priors using is.brmsprior()
        duplicate_prior <- any(sapply(current_priors, function(p) {
            existing_prior <- eval(parse(text = p))
            is.brmsprior(existing_prior) && identical(existing_prior, new_parsed_prior)
        }))

        if (duplicate_prior) {
            showNotification("Duplicate prior detected!", type = "error")
            return(NULL)
        }

        # Add prior if it's not a duplicate
        updated_priors <- c(current_priors, new_prior)
        prior_list(updated_priors)
        updateTextInput(session, "prior_input", value = "")
    })

    # Observe event to clear all priors
    observeEvent(input$clear_prior, {
        prior_list(character())
    })

    # Render the list of priors
    output$prior_list <- renderText({
        priors <- prior_list()
        if (length(priors) == 0) {
            return("No priors defined.")
        }
        paste(priors, collapse = "\n")
    })

    # Reactive expression to combine all priors
    reactive_prior <- reactive({
        priors <- prior_list()
        if (length(priors) == 0) {
            return(NULL) # Return NULL if no priors are defined
        }

        # Attempt to evaluate each prior and handle errors
        eval_priors <- lapply(priors, function(p) {
            tryCatch(
                eval(parse(text = p)),
                error = function(e) {
                    showNotification(paste("Invalid prior:", p), type = "error")
                    return(NULL)
                }
            )
        })

        # Combine priors using the '+' operator
        combined_prior <- eval_priors[[1]]
        if (length(eval_priors) > 1) {
            for (i in 2:length(eval_priors)) {
                combined_prior <- combined_prior + eval_priors[[i]]
            }
        }
        return(combined_prior)
    })



    # ==== Formula Construction ====
    reactive_formula <- reactive({
        req(data(), input$direct_var) # Make sure data is available and direct variable is selected
        output$model_fitting_error <- renderUI(NULL) # Clear any previous error messages

        # Direct variable (response variable)
        y_var <- input$direct_var
        formula_str <- paste(y_var, "~")

        # Linear terms (predictor variables)
        if (length(input$linear_aux_vars) > 0) {
            formula_str <- paste(formula_str, paste(input$linear_aux_vars, collapse = "+"))
        }

        # Nonlinear terms (splines or Gaussian processes)
        if (length(input$nonlinear_aux_vars) > 0) {
            nonlinear_terms <- if (input$model_type == "Spline") {
                paste0("s(", input$nonlinear_aux_vars, ")") # For splines
            } else {
                paste0("gp(", input$nonlinear_aux_vars, ")") # For Gaussian processes
            }
            formula_str <- paste(formula_str, "+", paste(nonlinear_terms, collapse = "+"))
        }

        # Binomial family handling (for binomial response with trials)
        if (input$hb_family == "binomial") {
            req(input$trials_var)
            formula_str <- paste0(y_var, "|trials(", input$trials_var, ") ~ ", gsub(paste0(y_var, " ~ "), "", formula_str))
        }

        # Handle Missing Data
        if (input$missing_data_method == "model") {
            selected_vars <- c(input$direct_var, input$linear_aux_vars, input$nonlinear_aux_vars)
            missing_vars <- selected_vars[sapply(data()[selected_vars], function(x) any(is.na(x)))]

            # Create the main formula for the response variable with missing data handling
            response_with_mi <- ifelse(input$direct_var %in% missing_vars,
                paste0(input$direct_var, " | mi()"),
                input$direct_var
            )

            # Combine all predictors
            all_predictors <- c(input$linear_aux_vars, input$nonlinear_aux_vars)

            # Create a formula for predictors with missing values
            predictor_with_mi <- sapply(all_predictors, function(x) {
                if (x %in% missing_vars) {
                    return(paste0("mi(", x, ")"))
                } else {
                    return(x)
                }
            })

            # The main formula that combines the response and predictors
            main_formula <- brms::bf(as.formula(paste0(response_with_mi, " ~ ", paste(predictor_with_mi, collapse = " + "))))

            # Create additional formulas for variables that have missing values
            auxiliary_formulas <- lapply(missing_vars, function(var) {
                if (var == input$direct_var) {
                    return(NULL)
                } # Do not create an additional model for the response

                # Only use variables from the initial formula as predictors
                predictors_for_var <- setdiff(all_predictors, var)
                predictors_for_var <- predictors_for_var[!sapply(data()[predictors_for_var], function(x) any(is.na(x)))]

                # If there are no valid predictors, use the intercept (1)
                if (length(predictors_for_var) > 0) {
                    return(brms::bf(as.formula(paste0(var, " | mi() ~ ", paste(predictors_for_var, collapse = " + ")))))
                } else {
                    return(brms::bf(as.formula(paste0(var, " | mi() ~ 1"))))
                }
            })

            # Remove NULL values from auxiliary formulas
            auxiliary_formulas <- Filter(Negate(is.null), auxiliary_formulas)

            # Combine all formulas (main formula + auxiliary formulas)
            if (length(auxiliary_formulas) > 0) {
                all_formulas <- Reduce("+", c(list(main_formula), auxiliary_formulas))
            } else {
                all_formulas <- main_formula
            }
        } else {
            # No missing data handling, just return the formula for the model
            all_formulas <- brms::bf(formula_str)
        }
    })



    # ==== Formula for random effect ====
    reactive_formula_re <- reactive({
        if (!is.null(input$re_groups)) {
            formula_re <- as.formula(paste("~", paste("(1 | ", input$re_groups, ")", collapse = " + ")))
        } else {
            formula_re <- NULL
        }
    })



    # === Model Fitting ===
    observeEvent(input$fit_model, {
      req(input$fit_model)
      
      withProgress(message = "Fitting model...", {
        tryCatch(
          {
            sample_prior_mode <- "no"  
            fit <- fit_model_function(sample_prior_mode)  
            model_fit(fit$model)
            showNotification("Model fitting completed!", type = "message")
          },
          error = function(e) {
            showNotification(paste("Model Error:", e$message), type = "error")
            model_fit(NULL)
          }
        )
      })
    })
    
    # === Prior Predictive Checking ===
    observeEvent(input$prior_check, {
      req(input$prior_check)
      
      withProgress(message = "Fitting prior predictive checking...", {
        tryCatch(
          {
            sample_prior_mode <- "only"  
            fit <- fit_model_function(sample_prior_mode)
            model_fit(fit$model)  
            showNotification("Prior Predictive Checking completed!", type = "message")
          },
          error = function(e) {
            showNotification(paste("Prior Predictive Checking Error:", e$message), type = "error")
            model_fit(NULL)
          }
        )
      })
    })
    
    fit_model_function <- function(sample_prior_mode) {

                    if (input$dist_type == "Custom") {
                        fit <- hbm(
                            formula = reactive_formula(),
                            re = reactive_formula_re(),
                            sre = if (input$sre == "none") NULL else input$sre,
                            sre_type = if (input$sre_type == "none") NULL else input$sre_type,
                            sar_type = if (input$sre_type == "none") NULL else input$sar_type,
                            car_type = if (input$sre_type == "none") NULL else input$car_type,
                            hb_sampling = input$hb_family,
                            hb_link = input$hb_link,
                            data = data(),
                            M = if (input$sre_type == "none") NULL else spatial_weights(),
                            handle_missing = if (input$missing_data_method == "none") NULL else input$missing_data_method,
                            m = input$m_value,
                            prior = reactive_prior(),
                            chains = input$num_chains,
                            iter = input$num_iter,
                            warmup = input$num_warmup,
                            cores = input$num_cores,
                            thin = input$num_thin,
                            control = list(adapt_delta = input$adapt_delta),
                            refresh = 0,
                            sample_prior = sample_prior_mode,
                            save_model = "model.stan"
                        )
                    } else if (input$dist_type == "Lognormal") {
                        fit <- hbm_lognormal(
                            response = input$direct_var,
                            predictors = input$linear_aux_vars,
                            re = input$re_groups,
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
                            iter = input$num_iter,
                            warmup = input$num_warmup,
                            cores = input$num_cores,
                            thin = input$num_thin,
                            control = list(adapt_delta = input$adapt_delta),
                            refresh = 0,
                            sample_prior = sample_prior_mode,
                            save_model = "model.stan"
                        )
                    } else if (input$dist_type == "Logitnormal") {
                        fit <- hbm_logitnormal(
                            response = input$direct_var,
                            trials = if (input$trials_logit == "none") NULL else input$trials_logit,
                            predictors = input$linear_aux_vars,
                            re = input$re_groups,
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
                            iter = input$num_iter,
                            warmup = input$num_warmup,
                            cores = input$num_cores,
                            thin = input$num_thin,
                            control = list(adapt_delta = input$adapt_delta),
                            refresh = 0,
                            sample_prior = sample_prior_mode,
                            save_model = "model.stan"
                        )
                    } else if (input$dist_type == "Beta") {
                      if (input$n_beta == "none") { 
                        n <- NULL 
                      } else {
                        n <- input$n_beta
                      }
                      if (input$deff_beta == "none") { 
                        deff <- NULL 
                      } else {
                        deff <- input$deff_beta
                      }
                        fit <- hbm_beta(
                            response = input$direct_var,
                            n = n,
                            deff = deff,
                            predictors = input$linear_aux_vars,
                            re = input$re_groups,
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
                            iter = input$num_iter,
                            warmup = input$num_warmup,
                            cores = input$num_cores,
                            thin = input$num_thin,
                            control = list(adapt_delta = input$adapt_delta),
                            refresh = 0,
                            sample_prior = sample_prior_mode,
                            save_model = "model.stan"
                        )
                    }
    }



    # ==== Convergence Diagnostics ====
    # Convergence Diagnostic : Numerical diagnostics
    observe({
        req(model_fit())
        diag_results <- hbcc(
            model = model_fit(),
            diag_tests = input$diag_tests
        )
        output$diag_numerical <- renderPrint({
            if ("rhat" %in% input$diag_tests && !is.null(diag_results$rhat_ess)) {
                cat("==================== R-hat Statistics ====================\n")
                print(diag_results$rhat_ess)
            }
            if ("geweke" %in% input$diag_tests && !is.null(diag_results$geweke)) {
                cat("========================== Geweke Diagnostics ==========================\n")
                print(diag_results$geweke)
            }
            if ("raftery" %in% input$diag_tests && !is.null(diag_results$raftery)) {
                cat("===================== Raftery-Lewis Diagnostics ====================\n")
                print(diag_results$raftery)
            }
            if ("heidel" %in% input$diag_tests && !is.null(diag_results$heidel)) {
                cat("=================== Heidelberger-Welch Diagnostics ===================\n")
                print(diag_results$heidel)
            }
        })
    })

    # Convergence Diagnostic : Visual diagnostics
    observe({
        req(model_fit())
        diag_results <- hbcc(
            model = model_fit(),
            plot_types = input$plot_types
        )
        output$diag_plots <- renderPlot({
            selected_plot <- input$plot_types
            if (!is.null(diag_results$plots[[selected_plot]])) {
                diag_results$plots[[selected_plot]]
            } else {
                plot.new()
                text(0.5, 0.5, "No plot available for this selection", cex = 1.5)
            }
        })
    })



    # ==== Model Checking ====
    # Model Checking : Numerical Model Checking
    observe({
        req(model_fit())
        check_results <- hbmc(
            model = model_fit(),
            ndraws = input$pp_samples
        )
        output$model_metrics <- renderPrint({
            if ("loo" %in% input$num_check_methods) {
                cat("==================== Leave-One-Out (LOO) ====================\n")
                print(check_results$loo1)
                cat("\n")
            }
            if ("waic" %in% input$num_check_methods) {
                cat("=============== Widely Applicable Information Criteria (WAIC) ===============\n")
                print(check_results$waic1)
                cat("\n")
            }
        })
    })

    # Model Checking : Grapical Model Checking
    observe({
        req(model_fit())
        check_plots <- hbmc(
            model = model_fit(),
            ndraws = input$pp_samples
        )
        output$model_check_plot <- renderPlot({
            selected_plot <- input$graph_check_methods
            if (!is.null(check_plots[[selected_plot]])) {
              check_plots[[selected_plot]]
            } else {
                plot.new()
                text(0.5, 0.5, "No plot available for this selection", cex = 1.5)
            }
        })
    })



    # ==== Output Model Prior Predictive Checking ====
    output$prior_summary <- renderPrint({
        req(model_fit())
        cat("================================== Prior Summary ==================================\n")
        prior_summary(model_fit())
    })
    output$prior_convergent <- renderPrint({
        req(model_fit())
        hasil <- hbcc(model = model_fit())
        cat("=========================== R-hat Statistics ============================\n")
        print(hasil$rhat_ess)
    })
    output$data_vs_prior_dist <- renderPlot({
      req(model_fit())
      
      # Extract observed data and prior predictive draws
      observed_data <- model_fit()$data[[input$direct_var]]
      prior_draws <- as.vector(posterior_predict(model_fit(), draw = TRUE))
      
      observed_data <- na.omit(observed_data)
      prior_draws <- na.omit(prior_draws)
      
      shift_amount <- abs(min(prior_draws, na.rm = TRUE)) + 1e-3
      if (min(prior_draws, na.rm = TRUE) < 0) {
        prior_draws <- prior_draws + shift_amount
        observed_data <- observed_data + shift_amount
      }
      
      # Transformasi log
      transformed_observed <- log(observed_data)
      transformed_prior <- log(prior_draws)
      
      if (length(transformed_observed) < 2 || length(transformed_prior) < 2) {
        plot.new()
        text(0.5, 0.5, "Not enough data for density plot", cex = 1.2)
        return()
      }
      
      # Plot density untuk data observasi
      plot(density(transformed_observed),
           col = rgb(1, 0, 0, 0.7), lwd = 2,
           main = "Comparison: Observed Data vs Prior Predictive (Log Scale)",
           xlab = "log(Value)", ylab = "Density",
           xlim = range(c(transformed_observed, transformed_prior), na.rm = TRUE)
      )
      
      # Overlay density from prior predictive distribution
      lines(density(transformed_prior),
            col = rgb(0, 0, 1, 0.7), lwd = 2, lty = 2)
      
      grid()
      
      legend("topright",
             legend = c("Observed Data", "Prior Predictive"),
             col = c("red", "blue"), lty = c(1, 2), lwd = 2, bty = "n"
      )
    })
    



    # ==== Output Model Summary ====
    output$model_summary <- renderPrint({
        req(model_fit())
        cat("================================ Model Summary ================================\n")
        summary(model_fit())
    })
    output$model_prior <- renderPrint({
        req(model_fit())
        cat("================================ Prior Summary ================================\n")
        prior_summary(model_fit())
    })



    # ==== Output Prediction ====
    output$prediction_results <- DT::renderDT({
        req(model_fit())
        preds <- hbsae(model_fit(),scale = input$scale_types)
        datatable(preds$result_table)
    })



    # === Prediction New Data Handling ===
    observe({
        req(input$direct_var)
        pred_data(NULL)

        model_vars <- unique(
            na.omit(c(
                input$linear_aux_vars, input$nonlinear_aux_vars,input$re_groups,
                if (input$trials_var != "none") input$trials_var else NULL,
                if (input$trials_logit != "none") input$trials_logit else NULL,
                if (input$n_beta != "none") input$n_beta else NULL,
                if (input$deff_beta != "none") input$deff_beta else NULL
            ))
        )
        df <- data.frame(matrix(NA, nrow = 1, ncol = length(model_vars) + 1))
        colnames(df) <- c(model_vars, "prediction_result")

        pred_data(df)
    })

    # Add new row
    observeEvent(input$add_row, {
        df <- pred_data()
        df <- rbind(df, setNames(data.frame(matrix(NA, ncol = ncol(df))), names(df)))
        pred_data(df)
    })

    # Remove row (ensure at least one row remains)
    observeEvent(input$remove_row, {
        df <- pred_data()
        pred_data(if (nrow(df) > 1) df[-nrow(df), ] else df * NA)
    })

    # Render the editable prediction input table
    output$prediction_input_table <- DT::renderDT(
        {
            req(pred_data())
            datatable(
                pred_data(),
                options = list(
                    dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE, autoWidth = TRUE,
                    columnDefs = list(
                        list(className = "dt-center", targets = "_all"),
                        list(width = "100px", targets = "_all")
                    )
                ),
                selection = "none",
                editable = list(target = "cell", disable = list(columns = ncol(pred_data()))) # Lock prediction column
            ) %>% formatStyle(columns = "prediction_result", backgroundColor = "lightgray")
        },
        server = FALSE
    )

    # Save user edits
    observeEvent(input$prediction_input_table_cell_edit, {
        info <- input$prediction_input_table_cell_edit
        df <- pred_data()
        df[info$row, info$col] <- as.numeric(info$value)
        pred_data(df)
    })

    # Perform prediction when the button is clicked
    observeEvent(input$predict_new, {
        req(model_fit()) # Ensure model exists
        tryCatch(
            {
                df <- pred_data()[, !names(pred_data()) %in% "prediction_result", drop = FALSE] # Exclude prediction column
                df$prediction_result <- hbsae(model_fit(), newdata = df)$pred # Run prediction
                pred_data(df) # Update table with predictions
            },
            error = function(e) {
                showNotification(paste("Prediction Error:", e$message), type = "error")
            }
        )
    })



    # ==== Download Handlers ====
    output$download_results <- downloadHandler(
        filename = function() paste0("predictions_", Sys.Date(), ".csv"),
        content = function(file) write.csv(posterior_predict(model_fit()), file)
    )
    output$save_model <- downloadHandler(
        filename = function() "model_fit.rds",
        content = function(file) saveRDS(model_fit(), file)
    )
    output$save_stan_code <- downloadHandler(
        filename = function() "stan_code.txt",
        content = function(file) writeLines(stancode(model_fit()), file)
    )
    output$save_coda <- downloadHandler(
        filename = function() "coda_samples.rds",
        content = function(file) saveRDS(as.mcmc(model_fit()), file)
    )
    output$save_plots <- downloadHandler(
        filename = function() "diagnostic_plots.pdf",
        content = function(file) {
            req(model_fit())
            pdf(file)
            print(mcmc_trace(as.array(model_fit())))
            print(mcmc_dens_overlay(as.array(model_fit())))
            print(pp_check(model_fit()))
            dev.off()
        }
    )
}


shinyApp(ui, server)