#' labs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList p selectInput req
#' @importFrom bslib layout_columns card card_header
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom DT DTOutput renderDT datatable formatRound
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_hline annotate geom_boxplot scale_x_log10 scale_y_log10 labs theme_minimal theme element_text
#' @importFrom dplyr filter group_by summarise inner_join arrange desc select
mod_labs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Hy\'s Law Quadrant Analysis (Peak ALT vs Peak BILI)"),
        p(class = "text-muted small", "Subjects in top-right quadrant (ALT \u2265 3\u00d7ULN & BILI \u2265 2\u00d7ULN) indicate potential Drug-Induced Liver Injury (DILI)."),
        plotlyOutput(ns("plot_hys_law"), height = "400px")
      ),
      card(
        card_header("Laboratory Parameter Distribution Across Visits"),
        selectInput(ns("sel_lab_param"), "Select Parameter:", choices = c("ALT", "AST", "BILI", "CREAT")),
        plotlyOutput(ns("plot_lab_trajectory"), height = "330px")
      )
    ),
    card(
      card_header("Marked Lab Outliers (Values > 2\u00d7 Upper Limit of Normal)"),
      DTOutput(ns("table_lab_outliers"))
    )
  )
}

#' labs Server Functions
#'
#' @noRd 
mod_labs_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    adlb <- data$adlb
    
    output$plot_hys_law <- renderPlotly({
      peak_alt <- adlb %>% dplyr::filter(PARAMCD == "ALT") %>% dplyr::group_by(USUBJID, ARM) %>% dplyr::summarise(max_alt_uln = max(ULN_RATIO, na.rm = TRUE), .groups = "drop")
      peak_bili <- adlb %>% dplyr::filter(PARAMCD == "BILI") %>% dplyr::group_by(USUBJID) %>% dplyr::summarise(max_bili_uln = max(ULN_RATIO, na.rm = TRUE), .groups = "drop")
      hys_df <- dplyr::inner_join(peak_alt, peak_bili, by = "USUBJID")
      
      p <- ggplot(hys_df, aes(x = max_alt_uln, y = max_bili_uln, color = ARM, text = USUBJID)) +
        geom_point(size = 3, alpha = 0.8) +
        geom_vline(xintercept = 3, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
        annotate("rect", xmin = 3, xmax = max(hys_df$max_alt_uln, 6), ymin = 2, ymax = max(hys_df$max_bili_uln, 4), alpha = 0.15, fill = "red") +
        scale_x_log10() + scale_y_log10() +
        labs(x = "Peak ALT (\u00d7 ULN)", y = "Peak Total Bilirubin (\u00d7 ULN)") +
        theme_minimal()
      ggplotly(p, tooltip = c("text", "x", "y", "color"))
    })
    
    output$plot_lab_trajectory <- renderPlotly({
      req(input$sel_lab_param)
      df_param <- adlb %>% dplyr::filter(PARAMCD == input$sel_lab_param)
      p <- ggplot(df_param, aes(x = reorder(VISIT, VISITNUM), y = AVAL, fill = ARM)) +
        geom_boxplot(outlier.size = 1) +
        labs(x = "Study Visit", y = unique(df_param$PARAM)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 15, hjust = 1))
      ggplotly(p)
    })
    
    output$table_lab_outliers <- renderDT({
      outliers <- adlb %>% dplyr::filter(ULN_RATIO >= 2.0) %>% dplyr::select(USUBJID, ARM, VISIT, PARAM, AVAL, ANRHI, ULN_RATIO) %>% dplyr::arrange(desc(ULN_RATIO))
      DT::datatable(outliers, options = list(pageLength = 6), rownames = FALSE, class = "compact") %>% DT::formatRound(columns = c("AVAL", "ANRHI", "ULN_RATIO"), digits = 2)
    })
  })
}
