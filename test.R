library(shiny)
library(teal.code)
library(ggplot2)

ui <- fluidPage(
  titlePanel("teal.code in Shiny Example"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:", choices = levels(iris$Species)),
      selectInput("y_var", "Select Y-axis:", choices = c("Sepal.Length", "Petal.Length")),
      actionButton("show_code", "Show R Code", icon = icon("code"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive qenv that records user selections
  computed_qenv <- reactive({
    q <- qenv()
    
    # Evaluate data filtering and plotting code dynamically
    eval_code(
      q,
      code = bquote({
        library(ggplot2)
        plot_data <- subset(iris, Species == .(input$species))
        p <- ggplot(plot_data, aes(x = Sepal.Width, y = .data[[.(input$y_var)]])) +
          geom_point(color = "steelblue", size = 3) +
          theme_minimal() +
          labs(title = paste("Plot for", .(input$species)))
      })
    )
  })

  # Render the plot produced in the qenv
  output$plot <- renderPlot({
    computed_qenv()[["p"]]
  })

  # Modal popup displaying the reproducible code
  observeEvent(input$show_code, {
    showModal(modalDialog(
      title = "Reproducible R Code",
      tags$pre(get_code(computed_qenv())),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

shinyApp(ui, server)