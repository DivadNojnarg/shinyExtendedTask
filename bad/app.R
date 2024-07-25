library(shiny) # at least version 1.8.1
library(ggplot2)
library(bslib) # at least version 0.7.0
library(palmerpenguins)
library(waiter)

# Function to retrieve stock data
create_plot <- function(y_col, w) {
  # simulate long retrieval time
  w$start()
  w$auto(value = 10, ms = 1000) # increase by 10 % every seconds
  Sys.sleep(10)
  w$close()
  ggplot(
    data = penguins,
    mapping = aes(
      x = flipper_length_mm, 
      y = .data[[y_col]], 
      color = species
    )
  ) +
    geom_point()
}

card_with_sidebar <- function(sidebar, main, title) {
  card(
    full_screen = TRUE,
    card_header(title),
    layout_sidebar(
      sidebar = sidebar(
        sidebar
      ),
      main
    )
  )
}

ui <- page_sidebar(
  fillable = FALSE,
  title = "Laggy app",
  useWaitress(),
  sidebar = sidebar(
    actionButton("task", "Get plot")
  ),
  card_with_sidebar(
    selectInput(
      "y_col", "Y-axis col:",
      colnames(penguins),
      selected = "body_mass_g"
    ),
    plotOutput("plot"),
    "My plot"
  ),
  card_with_sidebar(
    selectInput(
      "cols", "Columns:",
      colnames(penguins),
      selected = colnames(penguins)[1:3],
      multiple = TRUE
    ),
    tableOutput("data"),
    "My table"
  )
)

server <- function(input, output, session) {
  
  waitress <- Waitress$new("#task", theme = "overlay-opacity") # call the waitress
  
  output$data <- renderTable({
    validate(need(input$cols > 0, "Invalid choice: select at least one column ..."))
    head(penguins[, input$cols, drop = FALSE])
  }, rownames = TRUE)
  
  output$plot <- renderPlot({
    validate(need(input$task > 0, "Click on 'Get plot'"))
    create_plot(input$y_col, waitress)
  })
}

shinyApp(ui = ui, server = server)
