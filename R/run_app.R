#' @title Launch the Shiny App for Small Area Estimation using Hierarchical Bayesian
#' 
#' @description
#' This function launches an interactive **Shiny application** for performing 
#' **Hierarchical Bayesian Small Area Estimation (HBSAE)** using the `brms` package 
#' with `Stan` as the backend for Bayesian inference.
#' 
#' @import shinyWidgets shinydashboard readxl DT
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem tabItems tabItem
#' @importFrom readxl read_excel
#' @importFrom DT datatable renderDT DTOutput dataTableOutput renderDataTable
#' @importFrom minerva mine
#' @importFrom XICOR xicor
#' @importFrom energy dcor.test
#' @importFrom grDevices dev.off pdf 
#' @importFrom tools file_ext

#' 
#' @return
#' Opens a Shiny app in the web browser. The function does not return a value.
#' 
#' @examples
#' # Launch the HBSAE Shiny application (run interactively only)
#' if (interactive()) {
#'   run_sae_app()
#' }
#' 
#' # The function will open an interactive web application in your default browser
#' # where you can:
#' # 1. Upload your small area data
#' # 2. Specify model parameters 
#' # 3. Run Hierarchical Bayesian analysis
#' # 4. View and download results
#' # Note: This function requires an interactive R session
#' # and will open a web browser to display the Shiny application
#' 
#' 
#' @export
#' @author Achmad Syahrul Choir and Arsyka Laila Oktalia Siregar
#'
#' @references 
#' Rao, J. N. K., & Molina, I. (2015). *Small Area Estimation*. John Wiley & Sons. 
#'
run_sae_app <- function() {
  # Find the directory for the Shiny app inside the 'hbsaems' package
  app_dir <- system.file("shiny/sae_app", package = "hbsaems")
  
  # If the app directory is not found, stop execution and show an error message
  if (app_dir == "") {
    stop("App directory not found. Please reinstall `hbsaems`.", call. = FALSE)
  }
  
  # Launch the Shiny app from the found directory
  shiny::runApp(app_dir)
}
