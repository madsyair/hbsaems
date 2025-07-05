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
