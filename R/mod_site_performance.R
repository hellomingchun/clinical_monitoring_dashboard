#' site_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_columns card card_header
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual geom_point geom_text labs theme_minimal
#' @importFrom dplyr count inner_join
mod_site_performance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Site Recruitment Performance (Enrolled vs Target)"),
        plotlyOutput(ns("plot_site_enrollment"), height = "360px")
      ),
      card(
        card_header("Site Key Risk Indicators (KRI): Queries vs Protocol Deviations"),
        plotlyOutput(ns("plot_site_risk"), height = "360px")
      )
    ),
    card(
      card_header("Site Performance Matrix"),
      DTOutput(ns("table_site_summary"))
    )
  )
}

#' site_performance Server Functions
#'
#' @noRd 
mod_site_performance_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    site_metrics <- reactive({
      enrolled <- data$adsl %>% dplyr::count(SITEID, name = "ACTUAL_ENROLLED")
      dplyr::inner_join(data$adpd, enrolled, by = "SITEID")
    })
    
    output$plot_site_enrollment <- renderPlotly({
      df <- site_metrics()
      p <- ggplot(df, aes(x = SITEID)) +
        geom_col(aes(y = PLANNED_ENROLL, fill = "Target"), alpha = 0.4) +
        geom_col(aes(y = ACTUAL_ENROLLED, fill = "Actual"), width = 0.5) +
        scale_fill_manual(values = c("Target" = "#94A3B8", "Actual" = "#1E3A8A")) +
        labs(x = "Site ID", y = "Subjects", fill = "") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$plot_site_risk <- renderPlotly({
      df <- site_metrics()
      p <- ggplot(df, aes(x = PROTOCOL_DEVIATIONS, y = OPEN_QUERIES, label = SITEID, size = ACTUAL_ENROLLED)) +
        geom_point(color = "#DC2626", alpha = 0.7) +
        geom_text(vjust = -1, size = 3) +
        labs(x = "Protocol Deviations", y = "Open Data Queries", size = "Enrolled") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$table_site_summary <- renderDT({
      DT::datatable(site_metrics(), options = list(pageLength = 8), rownames = FALSE, class = "compact")
    })
  })
}
