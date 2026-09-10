#' patient_listing UI Function
#'
#' @description A shiny Module for detailed clinical trial participant records listing and CSV data export.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList downloadButton downloadHandler moduleServer is.reactive reactive strong
#' @importFrom bslib card card_header card_body
#' @importFrom DT DTOutput renderDT datatable formatStyle styleEqual
#' @importFrom dplyr select
#' @importFrom utils write.csv
mod_patient_listing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        class = "bg-light d-flex justify-content-between align-items-center",
        strong("Individual Participant Records (ADSL / Clinical Traceability)"),
        downloadButton(ns("download_csv"), "Export CSV", class = "btn-sm btn-outline-primary")
      ),
      card_body(
        DT::DTOutput(ns("patient_table"))
      )
    )
  )
}

#' patient_listing Server Functions
#'
#' @noRd
mod_patient_listing_server <- function(id, filtered_subjects) {
  shiny::moduleServer(id, function(input, output, session) {
    get_df <- if (shiny::is.reactive(filtered_subjects)) filtered_subjects else shiny::reactive(filtered_subjects)

    output$patient_table <- DT::renderDT({
      df <- get_df() %>%
        dplyr::select(
          `Subject ID` = usubjid,
          `Site` = site,
          `ICF Date` = icf_date,
          `Screen Status` = screen_status,
          `Rescreened` = rescreen_flag,
          `Randomized` = randomized_flag,
          `Arm` = arm,
          `Treatment Status` = treatment_status,
          `Current Status` = current_status,
          `Weeks on Study` = duration_weeks
        )

      DT::datatable(
        df,
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = "Bfrtip"
        ),
        rownames = FALSE,
        filter = "top",
        selection = "single"
      ) %>%
        DT::formatStyle(
          "Current Status",
          backgroundColor = DT::styleEqual(
            c("On Treatment", "Treatment Completed", "Discontinued - AE", "Screen Failed", "Not Treated"),
            c("#d8f3dc", "#e0f2fe", "#fee2e2", "#f3f4f6", "#fecaca")
          ),
          color = DT::styleEqual(
            c("On Treatment", "Treatment Completed", "Discontinued - AE", "Screen Failed", "Not Treated"),
            c("#1b4332", "#0369a1", "#991b1b", "#374151", "#7f1d1d")
          ),
          fontWeight = "bold"
        )
    })

    # CSV Downloader
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("clinical_trial_subjects_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(get_df(), file, row.names = FALSE)
      }
    )
  })
}
