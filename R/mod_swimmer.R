#' swimmer UI Function
#'
#' @description A shiny Module for participant journey timeline and milestone event visualization (Swimmer Plot).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sliderInput selectInput p span strong
#' @importFrom bslib card card_header card_body layout_column_wrap
#' @importFrom plotly plotlyOutput renderPlotly ggplotly layout
#' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_color_manual scale_shape_manual labs theme_minimal theme element_blank element_text
#' @importFrom dplyr arrange desc filter mutate
mod_swimmer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        class = "bg-light d-flex justify-content-between align-items-center",
        strong("Subject Treatment Timeline & Milestone Events (Swimmer Plot)"),
        span(class = "badge bg-secondary", "Interactive Plotly")
      ),
      card_body(
        layout_column_wrap(
          width = 1/2,
          fill = FALSE,
          sliderInput(
            ns("swimmer_top_n"), "Subjects to display in Swimmer:",
            min = 10, max = 50, value = 25, step = 5
          ),
          selectInput(
            ns("swimmer_sort"), "Sort Swimmer By:",
            choices = c(
              "Longest Duration" = "duration",
              "Subject ID" = "id",
              "Treatment Arm" = "arm"
            )
          )
        ),
        p(class = "text-muted", "Each horizontal bar represents a participant's time on study from ICF consent. Milestone markers denote First Dose, Confirmed Tumor Responses (PR/CR), Severe Adverse Events (Grade 3+), and ongoing treatment status."),
        plotlyOutput(ns("swimmer_plot"), height = "620px")
      )
    )
  )
}

#' swimmer Server Functions
#'
#' @noRd
mod_swimmer_server <- function(id, filtered_subjects, df_all_events) {
  shiny::moduleServer(id, function(input, output, session) {
    get_df <- if (shiny::is.reactive(filtered_subjects)) filtered_subjects else shiny::reactive(filtered_subjects)

    output$swimmer_plot <- plotly::renderPlotly({
      df <- get_df()
      if (nrow(df) == 0) {
        return(NULL)
      }

      top_n_val <- if (!is.null(input$swimmer_top_n)) input$swimmer_top_n else 25
      sort_val <- if (!is.null(input$swimmer_sort)) input$swimmer_sort else "duration"

      # Limit number of subjects for readability
      if (sort_val == "duration") {
        df_sub <- df %>%
          dplyr::arrange(dplyr::desc(duration_weeks)) %>%
          head(top_n_val)
      } else if (sort_val == "arm") {
        df_sub <- df %>%
          dplyr::arrange(arm, dplyr::desc(duration_weeks)) %>%
          head(top_n_val)
      } else {
        df_sub <- df %>%
          dplyr::arrange(usubjid) %>%
          head(top_n_val)
      }

      target_ids <- df_sub$usubjid
      df_sub$usubjid <- factor(df_sub$usubjid, levels = rev(df_sub$usubjid))

      # Filter milestones for selected subjects
      events_sub <- df_all_events %>%
        dplyr::filter(usubjid %in% target_ids) %>%
        dplyr::mutate(usubjid = factor(usubjid, levels = levels(df_sub$usubjid)))

      # Base bars & glyphs
      p <- ggplot2::ggplot() +
        ggplot2::geom_segment(
          data = df_sub,
          ggplot2::aes(
            y = usubjid,
            yend = usubjid,
            x = 0,
            xend = duration_weeks,
            color = current_status,
            text = paste0(
              "Subject: ", usubjid,
              "\nSite: ", site,
              "\nArm: ", arm,
              "\nStatus: ", current_status,
              "\nDuration: ", duration_weeks, " weeks",
              "\nRescreened: ", rescreen_flag
            )
          ),
          linewidth = 4.5,
          lineend = "round"
        ) +
        ggplot2::geom_point(
          data = events_sub,
          ggplot2::aes(
            y = usubjid,
            x = event_week,
            shape = event_type,
            text = paste0(usubjid, " - ", event_type, " at Wk ", event_week)
          ),
          size = 3,
          color = "black",
          fill = "yellow"
        ) +
        ggplot2::scale_color_manual(
          name = "Subject Status",
          values = c(
            "On Treatment" = "#2a9d8f",
            "Treatment Completed" = "#457b9d",
            "Discontinued - AE" = "#e63946",
            "Discontinued - Disease Progression" = "#f4a261",
            "Discontinued - Consent Withdrawn" = "#8d99ae",
            "Not Treated" = "#d90429",
            "Screen Failed" = "#6c757d",
            "Pending Randomization" = "#90e0ef"
          )
        ) +
        ggplot2::scale_shape_manual(
          name = "Milestones & Events",
          values = c(
            "ICF Obtained" = 21,
            "Randomized" = 22,
            "First Dose" = 23,
            "Severe AE (Grade 3+)" = 24,
            "Objective Tumor Response" = 8
          )
        ) +
        ggplot2::labs(
          x = "Weeks from Informed Consent (ICF)",
          y = "Participant ID",
          title = NULL
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 9, face = "bold"),
          legend.position = "right"
        )

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          legend = list(orientation = "h", y = -0.15),
          margin = list(l = 80, r = 20, t = 20, b = 60)
        )
    })
  })
}
