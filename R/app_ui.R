#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @noRd
app_ui <- function(request) {
  clinical_theme <- bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1b4965",
    secondary = "#62b6cb",
    success = "#2a9d8f",
    warning = "#e76f51",
    danger = "#e63946",
    base_font = font_google("Inter"),
    heading_font = font_google("Outfit")
  )

  tagList(
    # External resources & golem assets
    golem_add_external_resources(),
    # Application UI
    page_navbar(
      title = "Clinical Trial Operations & Patient Journey Dashboard",
      theme = clinical_theme,
      fillable = FALSE,
      sidebar = sidebar(
        title = "Trial Filters",
        width = 300,
        selectInput(
          "site_filter", "Clinical Site:",
          choices = c(
            "All Sites",
            "Site 101 - Boston",
            "Site 102 - Chicago",
            "Site 103 - Houston",
            "Site 104 - Seattle",
            "Site 105 - Atlanta"
          ),
          selected = "All Sites"
        ),
        selectInput(
          "arm_filter", "Treatment Arm:",
          choices = c(
            "All Arms",
            "Arm A (Drug 100mg)",
            "Arm B (Drug 200mg)",
            "Placebo"
          ),
          selected = "All Arms"
        ),
        checkboxGroupInput(
          "status_filter", "Subject Status / Stage:",
          choices = c(
            "On Treatment" = "On Treatment",
            "Treatment Completed" = "Treatment Completed",
            "Discontinued - AE" = "Discontinued - AE",
            "Discontinued - Progression" = "Discontinued - Disease Progression",
            "Discontinued - Other" = "Discontinued - Consent Withdrawn",
            "Not Treated" = "Not Treated",
            "Screen Failed" = "Screen Failed"
          ),
          selected = c(
            "On Treatment",
            "Treatment Completed",
            "Discontinued - AE",
            "Discontinued - Progression",
            "Not Treated",
            "Screen Failed"
          )
        ),
        radioButtons(
          "rescreen_filter", "Rescreening Status:",
          choices = c("All Subjects", "Rescreened Only", "Never Rescreened"),
          selected = "All Subjects"
        ),
        dateRangeInput(
          "date_range", "ICF Consent Date Range:",
          start = as.Date("2025-01-15"),
          end = as.Date("2025-12-31"),
          min = as.Date("2025-01-01"),
          max = as.Date("2026-12-31")
        )
      ),

      # Tab 1: Overview & Disposition
      nav_panel(
        title = "Overview & Disposition",
        icon = bs_icon("speedometer2"),
        mod_disposition_ui("disposition_1")
      ),

      # Tab 2: Swimmer Plot Tab
      nav_panel(
        title = "Swimmer Plot (Patient Journeys)",
        icon = bs_icon("water"),
        mod_swimmer_ui("swimmer_1")
      ),

      # Tab 3: Detailed Patient Listing Tab
      nav_panel(
        title = "Participant Listing & Details",
        icon = bs_icon("table"),
        mod_patient_listing_ui("patient_listing_1")
      ),

      # Tab 4: Adverse Event Safety & Toxicity Grades
      nav_panel(
        title = "AE Safety & Toxicity",
        icon = bs_icon("shield-exclamation"),
        mod_ae_safety_ui("ae_safety_1")
      ),

      # Tab 5: Demographics & Study Overview
      nav_panel(
        title = "Demographics & Overview",
        icon = bs_icon("pie-chart"),
        mod_overview_ui("overview_1")
      ),

      # Tab 6: Individual Subject Clinical Profile
      nav_panel(
        title = "Subject Profile",
        icon = bs_icon("person-lines-fill"),
        mod_patient_profile_ui("patient_profile_1")
      ),

      # Tab 7: Risk-Based Site Monitoring & KRIs
      nav_panel(
        title = "Site Risk & KRIs",
        icon = bs_icon("hospital"),
        mod_site_performance_ui("site_performance_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "clinicalmonitoringdashboard"
    )
  )
}
