#' patient_profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectizeInput hr uiOutput tags updateSelectizeInput reactive req renderUI
#' @importFrom bslib layout_columns card card_header
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plotly_empty layout
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_line facet_wrap labs theme_minimal
#' @importFrom dplyr filter
mod_patient_profile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Select Patient"),
        selectizeInput(ns("sel_subject"), "USUBJID:", choices = NULL),
        hr(),
        uiOutput(ns("patient_demog_card"))
      ),
      card(
        card_header("Patient Adverse Events Timeline"),
        plotlyOutput(ns("plot_patient_ae_timeline"), height = "280px")
      )
    ),
    card(
      card_header("Longitudinal Lab Trends"),
      plotlyOutput(ns("plot_patient_trends"), height = "340px")
    )
  )
}

#' patient_profile Server Functions
#'
#' @noRd 
mod_patient_profile_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    updateSelectizeInput(session, "sel_subject", choices = data$adsl$USUBJID, server = TRUE)
    
    selected_adsl <- reactive({
      req(input$sel_subject)
      data$adsl %>% dplyr::filter(USUBJID == input$sel_subject)
    })
    
    output$patient_demog_card <- renderUI({
      sub <- selected_adsl()
      req(nrow(sub) > 0)
      tagList(
        tags$h5(paste("Subject:", sub$USUBJID)),
        tags$p(tags$b("Treatment Arm: "), sub$ARM),
        tags$p(tags$b("Age / Sex: "), paste(sub$AGE, "yrs /", sub$SEX)),
        tags$p(tags$b("Site: "), sub$SITEID),
        tags$p(tags$b("Status: "), tags$span(class = "badge bg-primary", sub$EOSSTT))
      )
    })
    
    output$plot_patient_ae_timeline <- renderPlotly({
      req(input$sel_subject)
      sub_aes <- data$adae %>% dplyr::filter(USUBJID == input$sel_subject)
      
      if (nrow(sub_aes) == 0) {
        return(plotly_empty() %>% layout(title = "No Adverse Events recorded for this subject."))
      }
      
      p <- ggplot(sub_aes) +
        geom_segment(
          aes(y = AEDECOD, yend = AEDECOD, x = ASTDT, xend = AENDT, color = AESEV),
          linewidth = 3
        ) +
        geom_point(aes(x = ASTDT, y = AEDECOD, color = AESEV), size = 3) +
        geom_point(aes(x = AENDT, y = AEDECOD, color = AESEV), size = 3) +
        labs(x = "Date", y = "", color = "Severity") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$plot_patient_trends <- renderPlotly({
      req(input$sel_subject)
      sub_labs <- data$adlb %>% dplyr::filter(USUBJID == input$sel_subject)
      p <- ggplot(sub_labs, aes(x = reorder(VISIT, VISITNUM), y = AVAL, group = PARAMCD, color = PARAMCD)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(~PARAM, scales = "free_y") +
        labs(x = "Visit", y = "Value") +
        theme_minimal()
      ggplotly(p)
    })
  })
}
