# tests/testthat/test-sae-app.R

library(shinytest2)
options(shiny.testmode = TRUE)

test_that("{shinytest2} recording: sae_app", {
  skip_on_cran()
  
  app <- AppDriver$new(name = "sae_app", height = 797, width = 1169)
  
  # Initial data upload and preview settings
  app$upload_file(data_file = "data_lnln.rda")
  app$set_inputs(data_preview_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(data_preview_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                           13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                                           32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
                                           51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                                           70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,
                                           89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100), allow_no_input_binding_ = TRUE)
  app$set_inputs(data_preview_state = c(1751255337675, 0, 10, "", TRUE, FALSE, TRUE,
                                        c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "",
                                                                                                          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
                                                                                                                                                                TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE),
                                        c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "",
                                                                                                          TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
                                                                                                                                                                TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
                 allow_no_input_binding_ = TRUE)
  
  # You could add an expect_values here after data load to check if data_preview output is as expected
  # app$expect_values(output = "data_preview_output_id")
  
  # Explore tab interactions
  app$set_inputs(explore_var_summary = "y_obs", wait_ = FALSE)
  app$set_inputs(explore_var_hist = "y_obs", wait_ = FALSE)
  app$set_inputs(explore_var_boxplot = "y_obs", wait_ = FALSE)
  app$set_inputs(explore_var_scatter_x = "x1", wait_ = FALSE)
  app$set_inputs(explore_var_scatter_y = "y_obs", wait_ = FALSE)
  app$set_inputs(explore_transform_y = "log", wait_ = FALSE)
  
  # Model specification
  app$set_inputs(response_var = "y_obs", wait_ = FALSE)
  app$set_inputs(linear_aux_vars = "x1", wait_ = FALSE)
  app$set_inputs(linear_aux_vars = c("x1", "x2"), wait_ = FALSE)
  app$set_inputs(linear_aux_vars = c("x1", "x2", "x3"), wait_ = FALSE)
  app$set_inputs(re_groups = "group", wait_ = FALSE)
  app$set_inputs(sre_type = "car", wait_ = FALSE)
  app$set_inputs(car_type = "icar", wait_ = FALSE)
  app$set_inputs(sre = "sre")
  app$upload_file(spatial_weights = "spatial_weight.rda")
  app$set_inputs(dist_type = "Lognormal-Lognormal")
  
  # Prior settings
  app$set_inputs(prior_input = "prior(normal(0, 5), class = \"b\")")
  app$click("add_prior", wait_ = FALSE)
  app$click("prior_check", wait_ = FALSE)
  
  # MCMC settings for initial fit
  app$set_inputs(num_seed = 1, wait_ = FALSE)
  app$set_inputs(num_chains = 2, wait_ = FALSE)
  app$set_inputs(num_cores = 2, wait_ = FALSE)
  app$set_inputs(num_cores = 1, wait_ = FALSE)
  app$set_inputs(num_thin = 2, wait_ = FALSE)
  app$set_inputs(num_thin = 1, wait_ = FALSE)
  app$set_inputs(num_chains = 1, wait_ = FALSE)
  app$set_inputs(num_iter = 0, wait_ = FALSE)
  app$set_inputs(num_iter = 5000, wait_ = FALSE)
  app$set_inputs(num_warmup = 200, wait_ = FALSE)
  app$set_inputs(num_warmup = 2500, wait_ = FALSE)
  app$set_inputs(adapt_delta = 0.952, wait_ = FALSE)
  
  # Fit the model
  app$click("fit_model")
  app$wait_for_idle() # Wait for model fitting to complete and UI to update
  
  # --- Assertions after initial model fit ---
  # Navigate to Model Summary tab to check results
  app$set_inputs(results_tabs = "Model Summary")
  app$wait_for_idle()
  
  app$expect_values(output = c("model_summary_output_id", "some_coefficient_output_id")) # Replace with actual output IDs
  
  # Interaction with Update Parameters
  app$set_inputs(DataTables_Table_0_length = "10", wait_ = FALSE)
  app$set_inputs(update_adapt_delta = 0.95, wait_ = FALSE)
  app$set_inputs(update_num_chains = 1, wait_ = FALSE)
  app$set_inputs(update_num_cores = 1, wait_ = FALSE)
  app$set_inputs(update_num_iter = 4000, wait_ = FALSE)
  app$set_inputs(update_num_warmup = 2000, wait_ = FALSE)
  
  # --- Assertions after model update ---
  # You might want to re-check model summary or other outputs to see changes.
  app$expect_values(output = c("model_summary_output_id", "some_coefficient_output_id")) # Check updated values
  # Or compare before/after if you stored initial values
  
  # Navigate to Convergence Diagnostics
  app$set_inputs(results_tabs = "Convergence Diagnostics", wait_ = FALSE)
  app$wait_for_idle()
  
  # Example: Expecting convergence plots or metrics
  app$set_inputs(diag_tests_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(diag_tests = "geweke", wait_ = FALSE)
  app$set_inputs(diag_tests_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # app$expect_values(output = "geweke_plot_id") # If you have an output for the plot
  # app$expect_values(output = "geweke_summary_text_id") # If text output
  
  app$set_inputs(diag_tests_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(diag_tests = "raftery", wait_ = FALSE)
  app$set_inputs(diag_tests_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  
  app$set_inputs(diag_tests_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(diag_tests = "heidel", wait_ = FALSE)
  app$set_inputs(diag_tests_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  
  app$set_inputs(plot_types_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(plot_types = "dens", wait_ = FALSE)
  app$set_inputs(plot_types_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # app$expect_values(output = "density_plot_id")
  
  app$set_inputs(plot_types_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(plot_types = "acf", wait_ = FALSE)
  app$set_inputs(plot_types_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  
  app$set_inputs(plot_types_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(plot_types = "nuts_energy", wait_ = FALSE)
  app$set_inputs(plot_types_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  
  app$set_inputs(plot_types_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(plot_types = "rhat", wait_ = FALSE)
  app$set_inputs(plot_types_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  
  app$set_inputs(plot_types_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(plot_types = "neff", wait_ = FALSE)
  app$set_inputs(plot_types_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  
  # Model Checking
  app$set_inputs(results_tabs = "model_checking_panel", wait_ = FALSE)
  app$wait_for_idle()
  
  # Example: Expecting GOF metrics or PP check plots
  app$set_inputs(gof_metrics_select_open = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_inputs(gof_metrics_select = "waic", wait_ = FALSE)
  app$set_inputs(gof_metrics_select_open = FALSE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # app$expect_values(output = "waic_output_id")
  
  app$set_inputs(pp_check_type_select = "boxplot", wait_ = FALSE)
  # app$expect_values(output = "pp_check_boxplot_id")
  app$set_inputs(pp_check_type_select = "bars", wait_ = FALSE)
  app$set_inputs(pp_check_type_select = "scatter_avg", wait_ = FALSE)
  app$set_inputs(pp_check_type_select = "stat", wait_ = FALSE)
  
  # Prior Sensitivity
  app$set_inputs(results_tabs = "prior_sensitivity_panel", wait_ = FALSE)
  app$wait_for_idle()
  
  # Example: Expecting sensitivity results table/plot
  app$set_inputs(sensitivity_params_results = "b_Intercept", wait_ = FALSE)
  # app$expect_values(output = "sensitivity_plot_id")
  
  # Predictions
  app$set_inputs(results_tabs = "Predictions", wait_ = FALSE)
  app$wait_for_idle()
  
  # Example: Expecting prediction table content
  app$set_inputs(prediction_results_rows_current = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(prediction_results_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                                                 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                                                 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                                                 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68,
                                                 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,
                                                 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100), allow_no_input_binding_ = TRUE)
  app$set_inputs(prediction_results_state = c(1751255603179, 0, 10, "", TRUE, FALSE,
                                              TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
                                                                                                                      "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE,
                                                                                                                                                                                FALSE, TRUE)), allow_no_input_binding_ = TRUE)
})