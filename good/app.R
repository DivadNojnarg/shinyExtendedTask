library(shiny) # at least version 1.8.1
library(ggplot2)
library(bslib) # at least version 0.7.0
library(future)
library(promises)
library(palmerpenguins)

# Options for asynchronous strategies: multisession,
# multicore (not Windows/RStudio), cluster
plan(multisession)

# Function to retrieve stock data
create_plot <- function(y_col) {
  # simulate long retrieval time
  Sys.sleep(10)
  
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
  title = "Extended tasks with Shiny 1.8.1, bslib 0.7.0",
  sidebar = sidebar(
    input_task_button("task", "Get plot"),
    textOutput("status")
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
  
  output$data <- renderTable({
    validate(need(input$cols > 0, "Invalid choice: select at least one column ..."))
    head(penguins[, input$cols, drop = FALSE])
  }, rownames = TRUE)
  
  # reactive values
  reactive_status <- reactiveVal("No task submitted yet")
  # outputs
  output$plot <- renderPlot({
    validate(need(input$task > 0, "Click on 'Get plot'"))
    my_task$result()
  })
  output$status <- renderText(reactive_status())
  
  # Session level task: one per user
  my_task <- ExtendedTask$new(function(y_col) {
    # Need a promise
    future_promise({
      create_plot(y_col)
    })
  }) |> bind_task_button("task")
  
  
  # From the doc, we need to call task$invoke
  observeEvent(input$task, {
    reactive_status("Running 🏃")
    my_task$invoke(input$y_col)
  })
  
  observeEvent(my_task$result(), {
    reactive_status("Task completed ✅")
  })
}

shinyApp(ui = ui, server = server)
