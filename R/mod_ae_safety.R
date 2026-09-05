#' ae_safety UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectInput req reactive
#' @importFrom bslib layout_column_wrap layout_columns card card_header
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom ggplot2 ggplot aes geom_bar geom_col scale_fill_manual scale_fill_brewer labs theme_minimal
#' @importFrom dplyr filter count group_by mutate ungroup slice_max select
mod_ae_safety_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1/3,
      fill = FALSE,
      selectInput(ns("sel_arm"), "Filter Treatment Arm:", choices = c("All Arms", "Active Drug 100mg", "Active Drug 200mg", "Placebo")),
      selectInput(ns("sel_ser"), "Seriousness:", choices = c("All", "Serious Only (SAE)", "Non-Serious")),
      selectInput(ns("sel_rel"), "Relationship to Study Drug:", choices = c("All", "Related Only", "Not Related"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Adverse Events by System Organ Class (SOC) & CTCAE Toxicity Grade"),
        plotlyOutput(ns("plot_soc_grade"), height = "380px")
      ),
      card(
        card_header("Top Preferred Terms (PT) by Incidence"),
        plotlyOutput(ns("plot_top_pt"), height = "380px")
      )
    ),
    card(
      card_header("Adverse Event Listing & Incidence Summary"),
      DTOutput(ns("table_ae"))
    )
  )
}

#' ae_safety Server Functions
#'
#' @noRd 
mod_ae_safety_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    adae_filtered <- reactive({
      df <- data$adae
      if (input$sel_arm != "All Arms") df <- df %>% dplyr::filter(ARM == input$sel_arm)
      if (input$sel_ser == "Serious Only (SAE)") df <- df %>% dplyr::filter(AESER == "Y")
      if (input$sel_ser == "Non-Serious") df <- df %>% dplyr::filter(AESER == "N")
      if (input$sel_rel != "All") df <- df %>% dplyr::filter(AEREL == input$sel_rel)
      df
    })
    
    output$plot_soc_grade <- renderPlotly({
      df <- adae_filtered()
      req(nrow(df) > 0)
      p <- ggplot(df, aes(y = AEBODSYS, fill = factor(AETOXGR))) +
        geom_bar(position = "stack") +
        scale_fill_manual(values = c("1" = "#93C5FD", "2" = "#FCD34D", "3" = "#F97316", "4" = "#DC2626"), name = "CTCAE Grade") +
        labs(y = "", x = "Count of AEs") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$plot_top_pt <- renderPlotly({
      df <- adae_filtered()
      req(nrow(df) > 0)
      top_pts <- df %>%
        dplyr::count(AEDECOD, ARM, sort = TRUE) %>%
        dplyr::group_by(AEDECOD) %>%
        dplyr::mutate(total = sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::slice_max(total, n = 25)
      
      p <- ggplot(top_pts, aes(x = n, y = reorder(AEDECOD, total), fill = ARM)) +
        geom_col(position = "dodge") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "AE Count", y = "") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$table_ae <- renderDT({
      df <- adae_filtered() %>% dplyr::select(USUBJID, ARM, AEBODSYS, AEDECOD, AESEV, AETOXGR, AESER, AEREL, ASTDT, AENDT)
      DT::datatable(df, extensions = "Buttons", options = list(pageLength = 8, dom = "Bfrtip", buttons = c("copy", "csv")), rownames = FALSE, class = "compact stripe hover")
    })
  })
}
