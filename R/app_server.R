#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialize clinical trial data generator
  raw_data <- generate_clinical_trial_data()
  df_all_subjects <- raw_data$subjects
  df_all_events <- raw_data$events

  # Synchronize filter choices dynamically with generated data
  updateSelectInput(
    session, "site_filter",
    choices = c("All Sites", sort(unique(df_all_subjects$site))),
    selected = "All Sites"
  )
  updateSelectInput(
    session, "arm_filter",
    choices = c("All Arms", sort(unique(df_all_subjects$arm))),
    selected = "All Arms"
  )
  updateDateRangeInput(
    session, "date_range",
    start = min(df_all_subjects$icf_date),
    end = max(df_all_subjects$icf_date),
    min = min(df_all_subjects$icf_date),
    max = max(df_all_subjects$icf_date)
  )

  # Reactive filtered dataset
  filtered_subjects <- reactive({
    df <- df_all_subjects

    # Filter: Site
    if (!is.null(input$site_filter) && input$site_filter != "All Sites") {
      df <- df %>% dplyr::filter(site == input$site_filter)
    }

    # Filter: Arm
    if (!is.null(input$arm_filter) && input$arm_filter != "All Arms") {
      df <- df %>% dplyr::filter(arm == input$arm_filter)
    }

    # Filter: Status
    if (!is.null(input$status_filter) && length(input$status_filter) > 0) {
      df <- df %>% dplyr::filter(current_status %in% input$status_filter)
    }

    # Filter: Rescreen
    if (!is.null(input$rescreen_filter)) {
      if (input$rescreen_filter == "Rescreened Only") {
        df <- df %>% dplyr::filter(rescreen_flag == "Yes")
      } else if (input$rescreen_filter == "Never Rescreened") {
        df <- df %>% dplyr::filter(rescreen_flag == "No")
      }
    }

    # Filter: Date range
    if (!is.null(input$date_range) && length(input$date_range) == 2 && !any(is.na(input$date_range))) {
      df <- df %>% dplyr::filter(icf_date >= input$date_range[1] & icf_date <= input$date_range[2])
    }

    df
  })

  # ----------------------------------------------------------------------------
  # Module: Overview & Disposition
  # ----------------------------------------------------------------------------
  mod_disposition_server("disposition_1", filtered_subjects)

  # ----------------------------------------------------------------------------
  # Module: Swimmer Plot (Patient Journeys)
  # ----------------------------------------------------------------------------
  mod_swimmer_server("swimmer_1", filtered_subjects, df_all_events)

  # ----------------------------------------------------------------------------
  # Module: Participant Listing & Traceability
  # ----------------------------------------------------------------------------
  mod_patient_listing_server("patient_listing_1", filtered_subjects)

  # ----------------------------------------------------------------------------
  # CDISC Clinical Safety & Operational Modules
  # ----------------------------------------------------------------------------
  cdisc_data <- generate_clinical_data()
  mod_overview_server("overview_1", cdisc_data)
  mod_ae_safety_server("ae_safety_1", cdisc_data)
  mod_patient_profile_server("patient_profile_1", cdisc_data)
  mod_site_performance_server("site_performance_1", cdisc_data)
}
