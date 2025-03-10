minimal_test_ui <- function(id) {

  ns <- NS(id)

  tagList(
    sliderInput(ns("point_size"), label = "Point size", value = 0.3, min = 0.1, max = 4),
    plotOutput(ns("plot"))
  )

}


#' @import ggplot2
minimal_test_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$plot <- renderPlot({
      iris %>%
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point(size = input$point_size, alpha = 0.7) +
        labs(
          title = "Sepal Length vs Sepal Width",
          x = "Sepal Length (cm)",
          y = "Sepal Width (cm)",
          color = "Species"
        )
    })

  })

}