# Building a Prod-Ready, Robust Shiny Application.
# 
# Deploying the application

## Test the app

### Run R CMD check
devtools::check()

## Deploy

### Posit Connect / shinyapps.io
# rsconnect::deployApp(
#   appName = "clinical_monitoring_dashboard",
#   appTitle = "Clinical Monitoring Dashboard"
# )

### Dockerfile
# golem::add_dockerfile_with_renv()
# golem::add_dockerfile()
