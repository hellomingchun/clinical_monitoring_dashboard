#' disposition UI Function
#'
#' @description A shiny Module for trial disposition KPIs, CONSORT funnel, site milestones, and screening/treatment failure analysis.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput renderText moduleServer is.reactive br strong
#' @importFrom bslib layout_columns value_box card card_header card_body
#' @importFrom bsicons bs_icon
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plot_ly layout config
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_fill_brewer scale_fill_manual scale_x_continuous theme_minimal labs theme element_blank element_text coord_flip annotate theme_void expansion
#' @importFrom dplyr group_by summarise n filter count mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stats reorder
mod_disposition_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      fill = FALSE,
      value_box(
        title = "ICF Obtained",
        value = textOutput(ns("kpi_icf")),
        showcase = bsicons::bs_icon("file-earmark-medical-fill"),
        theme = "primary"
      ),
      value_box(
        title = "Screening Pass Rate",
        value = textOutput(ns("kpi_screen")),
        showcase = bsicons::bs_icon("check2-circle"),
        theme = "success"
      ),
      value_box(
        title = "Rescreened",
        value = textOutput(ns("kpi_rescreen")),
        showcase = bsicons::bs_icon("arrow-repeat"),
        theme = "info"
      ),
      value_box(
        title = "Randomized",
        value = textOutput(ns("kpi_rand")),
        showcase = bsicons::bs_icon("shuffle"),
        theme = "secondary"
      ),
      value_box(
        title = "Treated vs Not Treated",
        value = textOutput(ns("kpi_treated")),
        showcase = bsicons::bs_icon("capsule"),
        theme = "warning"
      ),
      value_box(
        title = "Currently On Treatment",
        value = textOutput(ns("kpi_on_treatment")),
        showcase = bsicons::bs_icon("heart-pulse-fill"),
        theme = "danger"
      )
    ),
    br(),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(class = "bg-light", strong("Subject Disposition Flow (CONSORT Funnel)")),
        card_body(plotlyOutput(ns("disposition_funnel_plot"), height = "380px"))
      ),
      card(
        card_header(class = "bg-light", strong("Enrollment & Milestone Distribution by Site")),
        card_body(plotlyOutput(ns("site_milestone_plot"), height = "380px"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(class = "bg-light", strong("Screening Failure Reasons")),
        card_body(plotlyOutput(ns("screen_failure_plot"), height = "280px"))
      ),
      card(
        card_header(class = "bg-light", strong("Randomized but Not Treated Reasons")),
        card_body(plotlyOutput(ns("not_treated_plot"), height = "280px"))
      )
    )
  )
}

#' disposition Server Functions
#'
#' @noRd
mod_disposition_server <- function(id, filtered_subjects) {
  moduleServer(id, function(input, output, session) {
    # Ensure reactive data accessor
    get_df <- if (shiny::is.reactive(filtered_subjects)) filtered_subjects else reactive(filtered_subjects)

    # KPI 1: ICF
    output$kpi_icf <- renderText({
      nrow(get_df())
    })

    # KPI 2: Screening Pass
    output$kpi_screen <- renderText({
      df <- get_df()
      if (nrow(df) == 0) {
        return("0%")
      }
      n_passed <- sum(df$screen_status == "Screen Success")
      pct <- round((n_passed / nrow(df)) * 100, 1)
      sprintf("%d (%s%%)", n_passed, pct)
    })

    # KPI 3: Rescreened
    output$kpi_rescreen <- renderText({
      df <- get_df()
      sum(df$rescreen_flag == "Yes")
    })

    # KPI 4: Randomized
    output$kpi_rand <- renderText({
      df <- get_df()
      n_rand <- sum(df$randomized_flag == "Yes")
      sprintf("%d", n_rand)
    })

    # KPI 5: Treated vs Not Treated
    output$kpi_treated <- renderText({
      df <- get_df()
      n_tx <- sum(df$treatment_status == "Treatment Started")
      n_not_tx <- sum(df$treatment_status == "Not Treated")
      sprintf("%d / %d", n_tx, n_not_tx)
    })

    # KPI 6: On Treatment
    output$kpi_on_treatment <- renderText({
      df <- get_df()
      sum(df$current_status == "On Treatment")
    })

    # ----------------------------------------------------------------------------
    # Plot 1: Disposition Funnel
    # ----------------------------------------------------------------------------
    output$disposition_funnel_plot <- plotly::renderPlotly({
      df <- get_df()
      if (nrow(df) == 0) {
        return(NULL)
      }

      stages <- c(
        "1. ICF Obtained",
        "2. Screen Success",
        "3. Randomized",
        "4. Treatment Started",
        "5. Currently On Treatment"
      )

      counts <- c(
        nrow(df),
        sum(df$screen_status == "Screen Success"),
        sum(df$randomized_flag == "Yes"),
        sum(df$treatment_status == "Treatment Started"),
        sum(df$current_status == "On Treatment")
      )

      funnel_df <- data.frame(
        Stage = factor(stages, levels = rev(stages)),
        Count = counts,
        Percentage = round(counts / nrow(df) * 100, 1)
      )

      p <- ggplot2::ggplot(
        funnel_df,
        ggplot2::aes(
          x = Count,
          y = Stage,
          fill = Stage,
          text = paste0(Stage, "\nCount: ", Count, "\n% of Consented: ", Percentage, "%")
        )
      ) +
        ggplot2::geom_col(width = 0.55, show.legend = FALSE) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(Count, " (", Percentage, "%)")),
          hjust = -0.15,
          size = 3.5,
          fontface = "bold"
        ) +
        ggplot2::scale_fill_brewer(palette = "Blues", direction = -1) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x = "Number of Subjects", y = NULL) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(face = "bold")
        )

      plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = FALSE)
    })

    # ----------------------------------------------------------------------------
    # Plot 2: Site Milestone Breakdown
    # ----------------------------------------------------------------------------
    output$site_milestone_plot <- plotly::renderPlotly({
      df <- get_df()
      if (nrow(df) == 0) {
        return(NULL)
      }

      site_summary <- df %>%
        dplyr::group_by(site) %>%
        dplyr::summarise(
          Consented = dplyr::n(),
          ScreenPassed = sum(screen_status == "Screen Success"),
          Randomized = sum(randomized_flag == "Yes"),
          Treated = sum(treatment_status == "Treatment Started"),
          .groups = "drop"
        ) %>%
        tidyr::pivot_longer(
          cols = c("Consented", "ScreenPassed", "Randomized", "Treated"),
          names_to = "Milestone",
          values_to = "Count"
        )

      p <- ggplot2::ggplot(site_summary, ggplot2::aes(x = site, y = Count, fill = Milestone)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.9) +
        ggplot2::scale_fill_manual(
          values = c(
            "Consented" = "#3a86ff",
            "ScreenPassed" = "#06d6a0",
            "Randomized" = "#ffd166",
            "Treated" = "#e76f51"
          )
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = "Participants") +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
    })

    # ----------------------------------------------------------------------------
    # Plot 3: Screen Failure Breakdown
    # ----------------------------------------------------------------------------
    output$screen_failure_plot <- plotly::renderPlotly({
      df <- get_df() %>% dplyr::filter(screen_status == "Screen Failure")

      if (nrow(df) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "No Screen Failures in Filtered View") +
          ggplot2::theme_void()
        return(plotly::ggplotly(p))
      }

      sf_df <- df %>%
        dplyr::count(screen_fail_reason, name = "Count") %>%
        dplyr::mutate(pct = round(Count / sum(Count) * 100, 1))

      plotly::plot_ly(
        sf_df,
        labels = ~screen_fail_reason,
        values = ~Count,
        type = "pie",
        textposition = "inside",
        textinfo = "label+percent",
        marker = list(colors = c("#e63946", "#f4a261", "#e76f51", "#2a9d8f", "#264653")),
        hole = 0.45
      ) %>%
        plotly::layout(showlegend = FALSE, margin = list(l = 20, r = 20, t = 20, b = 20)) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ----------------------------------------------------------------------------
    # Plot 4: Not Treated Breakdown
    # ----------------------------------------------------------------------------
    output$not_treated_plot <- plotly::renderPlotly({
      df <- get_df() %>% dplyr::filter(treatment_status == "Not Treated")

      if (nrow(df) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 1, y = 1, label = "Zero 'Not Treated' Participants in Filtered View") +
          ggplot2::theme_void()
        return(plotly::ggplotly(p))
      }

      nt_df <- df %>% dplyr::count(not_treated_reason, name = "Count")

      p <- ggplot2::ggplot(
        nt_df,
        ggplot2::aes(x = stats::reorder(not_treated_reason, Count), y = Count, fill = not_treated_reason)
      ) +
        ggplot2::geom_col(show.legend = FALSE, fill = "#d90429", width = 0.5) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = "Count")

      plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
    })
  })
}
