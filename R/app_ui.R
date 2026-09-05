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
        ),
        hr(),
        tags$h6("Swimmer Display Options"),
        sliderInput(
          "swimmer_top_n", "Subjects to display in Swimmer:",
          min = 10, max = 50, value = 25, step = 5
        ),
        selectInput(
          "swimmer_sort", "Sort Swimmer By:",
          choices = c(
            "Longest Duration" = "duration",
            "Subject ID" = "id",
            "Treatment Arm" = "arm"
          )
        )
      ),

      # Tab 1: Overview & Disposition
      nav_panel(
        title = "Overview & Disposition",
        icon = bs_icon("speedometer2"),
        layout_columns(
          fill = FALSE,
          value_box(
            title = "ICF Obtained",
            value = textOutput("kpi_icf"),
            showcase = bsicons::bs_icon("file-earmark-medical-fill"),
            theme = "primary"
          ),
          value_box(
            title = "Screening Pass Rate",
            value = textOutput("kpi_screen"),
            showcase = bsicons::bs_icon("check2-circle"),
            theme = "success"
          ),
          value_box(
            title = "Rescreened",
            value = textOutput("kpi_rescreen"),
            showcase = bsicons::bs_icon("arrow-repeat"),
            theme = "info"
          ),
          value_box(
            title = "Randomized",
            value = textOutput("kpi_rand"),
            showcase = bsicons::bs_icon("shuffle"),
            theme = "secondary"
          ),
          value_box(
            title = "Treated vs Not Treated",
            value = textOutput("kpi_treated"),
            showcase = bsicons::bs_icon("capsule"),
            theme = "warning"
          ),
          value_box(
            title = "Currently On Treatment",
            value = textOutput("kpi_on_treatment"),
            showcase = bsicons::bs_icon("heart-pulse-fill"),
            theme = "danger"
          )
        ),
        br(),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header(class = "bg-light", strong("Subject Disposition Flow (CONSORT Funnel)")),
            card_body(plotlyOutput("disposition_funnel_plot", height = "380px"))
          ),
          card(
            card_header(class = "bg-light", strong("Enrollment & Milestone Distribution by Site")),
            card_body(plotlyOutput("site_milestone_plot", height = "380px"))
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-light", strong("Screening Failure Reasons")),
            card_body(plotlyOutput("screen_failure_plot", height = "280px"))
          ),
          card(
            card_header(class = "bg-light", strong("Randomized but Not Treated Reasons")),
            card_body(plotlyOutput("not_treated_plot", height = "280px"))
          )
        )
      ),

      # Tab 2: Swimmer Plot Tab
      nav_panel(
        title = "Swimmer Plot (Patient Journeys)",
        icon = bs_icon("water"),
        card(
          card_header(
            class = "bg-light d-flex justify-content-between align-items-center",
            strong("Subject Treatment Timeline & Milestone Events (Swimmer Plot)"),
            span(class = "badge bg-secondary", "Interactive Plotly")
          ),
          card_body(
            p(class = "text-muted", "Each horizontal bar represents a participant's time on study from ICF consent. Milestone markers denote First Dose, Confirmed Tumor Responses (PR/CR), Severe Adverse Events (Grade 3+), and ongoing treatment status."),
            plotlyOutput("swimmer_plot", height = "650px")
          )
        )
      ),

      # Tab 3: Detailed Patient Listing Tab
      nav_panel(
        title = "Participant Listing & Details",
        icon = bs_icon("table"),
        card(
          card_header(
            class = "bg-light d-flex justify-content-between align-items-center",
            strong("Individual Participant Records (ADSL / Clinical Traceability)"),
            downloadButton("download_csv", "Export CSV", class = "btn-sm btn-outline-primary")
          ),
          card_body(
            DT::DTOutput("patient_table")
          )
        )
      ),

      # Tab 4: Adverse Event Safety & Toxicity Grades
      nav_panel(
        title = "AE Safety & Toxicity",
        icon = bs_icon("shield-exclamation"),
        mod_ae_safety_ui("ae_safety_1")
      ),

      # Tab 5: Laboratory Hepatotoxicity & Outliers
      nav_panel(
        title = "Hy's Law & Labs",
        icon = bs_icon("activity"),
        mod_labs_ui("labs_1")
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
