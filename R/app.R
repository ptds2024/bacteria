# load libraries
# library(shiny)
# library(shinythemes)


# Define UI for application that draws a histogram
#' @import shiny
#' @importFrom shinythemes shinytheme
ui <- function(...){
  fluidPage(theme = shinytheme("slate"),

                withMathJax(),

                # Application title
                titlePanel("Bacteria Motility App"),

                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 30pt !important; }")),
                    numericInput("nbr_bacteria", "Number of Bacteria:", 20, 1, 50),
                    checkboxInput("move_in_sugar", label = "Bacteria keep moving inside the sugar", value = TRUE),
                    numericInput("constant_kappa",   label = paste('\\( \\kappa \\)', "constant", '\\( c \\)'), 0, 0, 100),
                    numericInput("input_seed", "Simulation seed", 123456, 1, 1000000),
                    actionButton("run_simulation", h4("Compute simulation"), icon = icon("calculator", lib = "font-awesome")),
                    sliderInput("animation_id", h4("Run animation of simulation"),
                                min = 0, max = 100,
                                value = 1, step = 1,
                                animate = animationOptions(interval = 300, loop = FALSE))
                  ),

                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("animated_simulation")
                  )
                )
)}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # compute all trajectories
  array_all_positions <- eventReactive(input$run_simulation, {
    compute_all_positions(circle_coordinate = c(0,0),
                          circle_radius = 3,
                          move_in_sugar = input$move_in_sugar,
                          N = input$nbr_bacteria,
                          M = 100,
                          initial_seed = input$input_seed,
                          constant_kappa = input$constant_kappa)
  })

  # if recomputed simulation, then reset animation to 0
  observeEvent(input$run_simulation,{
    updateSliderInput(session,'animation_id',value = 1)
  })

  sliderValues <- reactive({ (input$animation_id)})
  output$animated_simulation <- renderPlot({
    compute_animation_path_one_step(circle_coordinate = c(0,0), circle_radius = 3,
                                    array_all_positions = array_all_positions(), step_i = sliderValues())
  }, height = 540)
}

# Run the application
#' @title App for bacteria mobility
#' @export
run_app <- function(...){
  shinyApp(ui = ui, server = server, ...)
}
