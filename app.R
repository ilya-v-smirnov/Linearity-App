# Entry point for shiny::runApp() and for deployment (shinyapps.io, Posit Connect).
# For local use the installed package is preferred: LinearityApp::run_app()

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
LinearityApp::run_app()
