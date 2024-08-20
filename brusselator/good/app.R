library(shiny)
library(bslib)
library(deSolve)
library(mirai)

van_der_pol <- function(t, y, mu) {
  d_x <- y[2]
  d_y <- mu * (1 - y[1]^2) * y[2] - y[1]
  list(c(X = d_x, Y = d_y))
}

plot_trajectories <- function(res) {
  plot(res[, 1], res[, "X"], type = "l", xlab = "time", ylab = "X", main = "Solutions", col = "blue")
  lines(res[, 1], res[, "Y"], xlab = "time", ylab = "Y", col = "red")
  legend(
    "topright",
    c("X(t)", "Y(t)"),
    col = c("blue", "red"),
    lty = 1
  )
}

plot_phase <- function(res) {
  plot(res[[1]][, 2:3], type = "l", xlab = "X", ylab = "Y", main = "multi init state diagram")
  points(res[[1]][1, 2], res[[1]][1, 3])
  # Other trajectories
  lapply(res[-1], \(dat) {
    lines(dat[, 2:3], xlab = "X", ylab = "Y")
    points(dat[1, 2], dat[1, 3])
  })
}

times <- seq(0, 1000, .001)

server <- function(input, output) {
  
  # Will stop updating as soon as one of the simulation run
  output$clock <- renderText({
    invalidateLater(1000)
    format(Sys.time(), "%a %b %d %X %Y")
  })
  
  # Slow but ok and cached
  sim_task <- ExtendedTask$new(
    function(...) mirai({
      deSolve::ode(
        y = c(X = x, Y = y), 
        times = times, 
        func = van_der_pol, 
        parms = mu
      )
    }, ...)
  ) |> bind_task_button("run")
  
  observe({
    sim_task$invoke(
      x = input$X,
      y = input$Y,
      mu = input$mu,
      times = times,
      van_der_pol = van_der_pol
    )
  }) |>
    bindEvent(input$run)
  
  output$table <- DT::renderDataTable({
    as.data.frame(sim_task$result())
  })
  
  output$solutions <- renderPlot({
    validate(need(input$run> 0, "Click on run"))
    plot_trajectories(sim_task$result())
  })
  
  sim_slow_task <- ExtendedTask$new(
    function(...) {
      mirai(
        {
          conds <- replicate(10, sample(seq(-1, 1, by = 0.01), size = 2), simplify = FALSE)
          lapply(conds, \(cond) {
            names(cond) <- c("X", "Y")
            deSolve::ode(cond, times, van_der_pol, mu)
          })
        }, ...)
    }
  ) |> bind_task_button("run_random")
  
  observe({
    sim_slow_task$invoke(
      mu = input$mu,
      times = times,
      van_der_pol = van_der_pol
    )
  }) |>
    bindEvent(input$run_random)

  # Very very slow and can't be cached because random
  
  output$multi_phase <- renderPlot({
    validate(need(input$run_random > 0, "Click on run random"))
    plot_phase(sim_slow_task$result())
  })
}

ui <- fluidPage(
  headerPanel("Van der Pol oscillator"),
  sidebarLayout(
    sidebarPanel(
      textOutput("clock"),
      h3("Init values"),
      numericInput("X", label = "X", min = 0.0, max = 5,  value = 1, step = 0.2),
      numericInput("Y", label = "Y", min = 0.0, max = 5,  value = 1, step = 0.2),
      
      h3("Parameters"),
      numericInput("mu", label = "mu", min = 0.0, max = 500,  value = 10, step = 1),
      input_task_button("run", "Run"),
      input_task_button("run_random", "Simulate 10 random initial conditions")
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

app <- shinyApp(ui = ui, server = server)

# run app using 2 local daemons
with(daemons(2), runApp(app))
