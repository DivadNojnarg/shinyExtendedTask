library(shiny)
library(deSolve)
library(waiter)

van_der_pol <- function(t, y, mu) {
  d_x <- y[2]
  d_y <- mu * (1 - y[1]^2) * y[2] - y[1]
  list(c(X = d_x, Y = d_y))
}

times <- seq(0, 1000, .001)

server <- function(input, output) {
  
  # Will stop updating as soon as one of the simulation run
  output$clock <- renderText({
    invalidateLater(1000)
    format(Sys.time(), "%a %b %d %X %Y")
  })
  
  w <- Waiter$new(id = c("table", "trajectories", "phase"))
  waitress <- Waitress$new("#multi_phase", infinite = TRUE) # call the waitress
  
  # Slow but ok and cached
  sim <- reactive({
    w$show()
    on.exit({
      w$hide()
    })
    y0 <- c(X = input$X, Y = input$Y)
    ode(y0, times, van_der_pol, input$mu)
  }) |>
    bindCache(input$X, input$Y, input$mu) |>
    bindEvent(input$run)
  
  # Very very slow and can't be cached because random
  random_initcond_sim <- reactive({
    waitress$start()
    on.exit({
      waitress$close()
    })
    conds <- replicate(2, sample(-1:1, size = 2), simplify = FALSE)
    lapply(conds, \(cond) {
      names(cond) <- c("X", "Y")
      ode(cond, times, van_der_pol, input$mu)
    })
  }) |>
    bindEvent(input$run_random)
  
  output$table <- DT::renderDataTable({
    as.data.frame(sim())
  })
  
  output$solutions <- renderPlot({
    validate(need(input$run> 0, "Click on run"))
    out <- sim()
    plot(out[, 1], out[, "X"], type = "l", xlab = "time", ylab = "X", main = "Solutions", col = "blue")
    lines(out[, 1], out[, "Y"], xlab = "time", ylab = "Y", col = "red")
    legend(
      "topright",
      c("X(t)", "Y(t)"),
      col = c("blue", "red"),
      lty = 1
    )
  })
  
  output$multi_phase <- renderPlot({
    validate(need(input$run> 0, "Click on run random"))
    out <- random_initcond_sim()
    plot(out[[1]][, 2:3], type = "l", xlab = "X", ylab = "Y", main = "multi init state diagram")
    points(out[[1]][1, 2], out[[1]][1, 3])
    
    # Other trajectories
    lapply(out[-1], \(dat) {
      lines(dat[, 2:3], xlab = "X", ylab = "Y")
      points(dat[1, 2], dat[1, 3])
    })
  })
}

ui <- fluidPage(
  useWaiter(),
  useWaitress(),
  headerPanel("Van der Pol oscillator"),
  sidebarLayout(
    sidebarPanel(
      textOutput("clock"),
      h3("Init values"),
      numericInput("X", label = "X", min = 0.0, max = 5,  value = 1, step = 0.2),
      numericInput("Y", label = "Y", min = 0.0, max = 5,  value = 1, step = 0.2),
      
      h3("Parameters"),
      numericInput("mu", label = "mu", min = 0.0, max = 500,  value = 10, step = 1),
      actionButton("run", "Run"),
      actionButton("run_random", "Simulate 1000 random initial conditions")
    ),
    mainPanel(
      h3("Simulation results"),
      DT::dataTableOutput("table"),
      plotOutput("solutions"),
      h3("Random initial values"),
      plotOutput("multi_phase")
    )
  )
)

shinyApp(ui = ui, server = server)
