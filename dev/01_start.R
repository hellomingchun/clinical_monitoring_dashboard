# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you can also replace
# by your own code.
# 
# 1. Set function "golem" options
# 
golem::fill_desc(
  pkg_name = "clinicalmonitoringdashboard",
  pkg_title = "Clinical Trial Safety & Operations Monitoring Dashboard",
  pkg_description = "A Golem-based Shiny application for clinical trial safety and operations monitoring.",
  author_first_name = "Clinical",
  author_last_name = "Team",
  author_email = "clinical@example.com",
  repo_url = NULL
)

## Set {golem} options
golem::set_golem_options()

## Create Common Files
usethis::use_mit_license("Clinical Team")
usethis::use_readme_rmd(open = FALSE)
usethis::use_code_of_conduct(contact = "Clinical Team")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Use git
usethis::use_git()

## Init Testing Infrastructure
usethis::use_testthat()

## Recommended packages
golem::use_recommended_tests()
golem::use_recommended_deps()

# You're now ready to go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
