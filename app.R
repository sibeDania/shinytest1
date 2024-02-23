library(shiny)

logo <- "https://eadania.com/media/3920/dania-academy-denmark-official-logo2.png"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    title = "EA Dania 2024",

    # Application title
    titlePanel("Datavisualisering"),
    
    theme = shinythemes::shinytheme("cosmo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          div(img(height = 65, width = 80, src = logo),
              style = "text-align: center;"),
          
          br(),
          hr(),
          br(),
          
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Min tab",
              plotOutput("distPlot")),
            tabPanel("Min anden tab"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
