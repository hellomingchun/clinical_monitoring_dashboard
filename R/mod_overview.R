#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList textOutput
#' @importFrom bslib layout_column_wrap layout_columns value_box card card_header
#' @importFrom bsicons bs_icon
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_step geom_point geom_bar geom_boxplot scale_y_continuous scale_fill_brewer scale_fill_manual labs theme_minimal theme element_text
#' @importFrom dplyr arrange mutate row_number n_distinct
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      value_box(
        title = "Randomized Patients",
        value = textOutput(ns("total_rand")),
        showcase = bsicons::bs_icon("people-fill"),
        theme = "primary"
      ),
      value_box(
        title = "Ongoing Treatment",
        value = textOutput(ns("total_ongoing")),
        showcase = bsicons::bs_icon("activity"),
        theme = "teal"
      ),
      value_box(
        title = "Serious Adverse Events (SAEs)",
        value = textOutput(ns("total_saes")),
        showcase = bsicons::bs_icon("exclamation-triangle-fill"),
        theme = "danger"
      ),
      value_box(
        title = "Active Clinical Sites",
        value = textOutput(ns("total_sites")),
        showcase = bsicons::bs_icon("geo-alt-fill"),
        theme = "secondary"
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Cumulative Subject Enrollment Timeline"),
        plotlyOutput(ns("plot_enrollment"), height = "360px")
      ),
      card(
        card_header("Subject Disposition by Treatment Arm"),
        plotlyOutput(ns("plot_disposition"), height = "360px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Demographics: Age & Sex Distribution"),
        plotlyOutput(ns("plot_demographics"), height = "300px")
      ),
      card(
        card_header("Racial Demographics Breakdown"),
        plotlyOutput(ns("plot_race"), height = "300px")
      )
    )
  )
}

#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    adsl <- data$adsl
    adae <- data$adae
    
    output$total_rand <- renderText({ nrow(adsl) })
    output$total_ongoing <- renderText({ sum(adsl$EOSSTT == "Ongoing") })
    output$total_saes <- renderText({ sum(adae$AESER == "Y") })
    output$total_sites <- renderText({ dplyr::n_distinct(adsl$SITEID) })
    
    output$plot_enrollment <- renderPlotly({
      df_enroll <- adsl %>% dplyr::arrange(RANDDT) %>% dplyr::mutate(Cumulative = dplyr::row_number())
      p <- ggplot(df_enroll, aes(x = RANDDT, y = Cumulative)) +
        geom_step(color = "#1E3A8A", linewidth = 1.2) +
        geom_point(color = "#0D9488", size = 1.5, alpha = 0.6) +
        labs(x = "Randomization Date", y = "Total Enrolled Patients") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$plot_disposition <- renderPlotly({
      p <- ggplot(adsl, aes(x = ARM, fill = EOSSTT)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_brewer(palette = "Blues") +
        labs(x = "", y = "Proportion", fill = "Disposition") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 15, hjust = 1))
      ggplotly(p)
    })
    
    output$plot_demographics <- renderPlotly({
      p <- ggplot(adsl, aes(x = ARM, y = AGE, fill = SEX)) +
        geom_boxplot(alpha = 0.7, outlier.shape = 21) +
        scale_fill_manual(values = c("M" = "#3B82F6", "F" = "#EC4899")) +
        labs(x = "", y = "Age (Years)", fill = "Sex") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$plot_race <- renderPlotly({
      p <- ggplot(adsl, aes(x = RACE, fill = ARM)) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Set2") +
        labs(x = "", y = "Number of Subjects") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
      ggplotly(p)
    })
  })
}
