# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step of the dev files is optional.

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("shiny")
usethis::use_package("bslib")
usethis::use_package("bsicons")
usethis::use_package("plotly")
usethis::use_package("DT")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("lubridate")
usethis::use_package("ggplot2")
usethis::use_package("scales")
usethis::use_package("htmltools")

## Add modules ----
## Create a module with golem::add_module(name = "my_module")
# golem::add_module(name = "overview", with_test = TRUE)
# golem::add_module(name = "ae_safety", with_test = TRUE)
# golem::add_module(name = "labs", with_test = TRUE)
# golem::add_module(name = "patient_profile", with_test = TRUE)
# golem::add_module(name = "site_performance", with_test = TRUE)

## Add helper functions ----
# golem::add_fct("synthetic_data", with_test = FALSE)
# golem::add_utils("helpers", with_test = FALSE)

## External resources
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")

## Tests ----
## Add one line by test you want to create
# usethis::use_test("app")

# Documentation
devtools::document()
