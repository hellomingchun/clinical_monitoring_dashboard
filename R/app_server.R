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

  # KPI 1: ICF
  output$kpi_icf <- renderText({
    nrow(filtered_subjects())
  })

  # KPI 2: Screening Pass
  output$kpi_screen <- renderText({
    df <- filtered_subjects()
    if (nrow(df) == 0) {
      return("0%")
    }
    n_passed <- sum(df$screen_status == "Screen Success")
    pct <- round((n_passed / nrow(df)) * 100, 1)
    sprintf("%d (%s%%)", n_passed, pct)
  })

  # KPI 3: Rescreened
  output$kpi_rescreen <- renderText({
    df <- filtered_subjects()
    sum(df$rescreen_flag == "Yes")
  })

  # KPI 4: Randomized
  output$kpi_rand <- renderText({
    df <- filtered_subjects()
    n_rand <- sum(df$randomized_flag == "Yes")
    sprintf("%d", n_rand)
  })

  # KPI 5: Treated vs Not Treated
  output$kpi_treated <- renderText({
    df <- filtered_subjects()
    n_tx <- sum(df$treatment_status == "Treatment Started")
    n_not_tx <- sum(df$treatment_status == "Not Treated")
    sprintf("%d / %d", n_tx, n_not_tx)
  })

  # KPI 6: On Treatment
  output$kpi_on_treatment <- renderText({
    df <- filtered_subjects()
    sum(df$current_status == "On Treatment")
  })

  # ----------------------------------------------------------------------------
  # Plot 1: Disposition Funnel
  # ----------------------------------------------------------------------------
  output$disposition_funnel_plot <- plotly::renderPlotly({
    df <- filtered_subjects()
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
    df <- filtered_subjects()
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
    df <- filtered_subjects() %>% dplyr::filter(screen_status == "Screen Failure")

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
    df <- filtered_subjects() %>% dplyr::filter(treatment_status == "Not Treated")

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

  # ----------------------------------------------------------------------------
  # Plot 5: Swimmer Plot
  # ----------------------------------------------------------------------------
  output$swimmer_plot <- plotly::renderPlotly({
    df <- filtered_subjects()
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

  # ----------------------------------------------------------------------------
  # Data Table (DT)
  # ----------------------------------------------------------------------------
  output$patient_table <- DT::renderDT({
    df <- filtered_subjects() %>%
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
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("clinical_trial_subjects_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(filtered_subjects(), file, row.names = FALSE)
    }
  )

  # ----------------------------------------------------------------------------
  # CDISC Clinical Safety & Operational Modules
  # ----------------------------------------------------------------------------
  cdisc_data <- generate_clinical_data()
  mod_ae_safety_server("ae_safety_1", cdisc_data)
  mod_labs_server("labs_1", cdisc_data)
  mod_patient_profile_server("patient_profile_1", cdisc_data)
  mod_site_performance_server("site_performance_1", cdisc_data)
}
