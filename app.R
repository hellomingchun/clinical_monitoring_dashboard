# Launch the Shiny App (Golem entry point for RStudio / Connect / Shiny Server)
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
clinicalmonitoringdashboard::run_app()
