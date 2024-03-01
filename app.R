library(shiny)
library(palmerpenguins)
library(tidyverse)
library(bslib)
library(bsicons)

head(penguins)

peng <- penguins

day <- readRDS("~/GitHub/shinytest1/model/day.Rds")

#logo <- "https://eadania.com/media/3920/dania-academy-denmark-official-logo2.png"

theme_set(theme_minimal())

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    title = "EA Dania 2024",

    # Application title
    titlePanel("Datavisualisering"),
    
    theme = shinythemes::shinytheme("cosmo"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          div(img(height = 65, width = 80, src = "dania.png"),
              style = "text-align: center;"),
          
          br(),
          hr(),
          br(),
          
          numericInput("sel_day",
                       "Select a number to return a day",
                       min = "1",
                       max = "7",
                       value = "1"),
          
          radioButtons("sel_peng",
                       "Select penguins",
                       choices = c(unique(peng$species))),
          
          checkboxInput("sel_color",
                        "Tick for colors between sexes"),
          
          selectInput("sel_island",
                             "Select which island",
                             choices = unique(peng$island),
                             selected = head(peng$island))
          
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Min tab",
                     br(),
                     fluidRow(column(width = 3,
                                     uiOutput("bl_vb")),
                              column(width = 3,
                                     value_box(title = "Bill depth",
                                               value = "50",
                                               showcase = div(
                                                 img(height = 60, width = 60,
                                                     src = "sibe_no_bg.PNG")
                                               ),
                                               class = "bg-danger")),
                              column(width = 3,
                                     uiOutput("day_vb")),
                              column(width = 3,
                                     value_box(title = "Bill depth",
                                               value = "50",
                                               showcase = bs_icon("bank2"),
                                               class = "bg-danger"))),
                     plotOutput("peng_plot")),
            tabPanel("Min anden tab"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

# Calculation -------------------------------------------------------------

  bill_length <- reactive({
    
    bl <- peng %>% 
      filter(species == input$sel_peng) %>% 
      summarise(mean = round(mean(bill_length_mm, na.rm = TRUE), digits = 1))
    
    return(bl$mean)
    
  })  
  

# Value boxes -------------------------------------------------------------

  output$bl_vb <- renderUI({
    
    value_box(title = "Bill depth",
              value = bill_length(),
              showcase = bs_icon("bank2"),
              class = "bg-danger")
    
  })
  
  output$day_vb <- renderUI({
    
    weekday <- day(input$sel_day)
    
    value_box(title = "The chosen day is a:",
              value = weekday,
              showcase = bs_icon("bank2"),
              class = "bg-danger")
    
  })


# Plots -------------------------------------------------------------------

    

    output$peng_plot <- renderPlot({
      
      peng1 <- peng %>% 
        filter(species == input$sel_peng)
      
      if(input$sel_color) {
      
        ggplot(peng1, aes(x = bill_length_mm, y = bill_depth_mm)) +
          geom_point(aes(color = peng1$sex)) +
          labs(x = "Bill length",
               y = "Bill depth",
               title = "Plot",
               subtitle = input$sel_peng) +
          scale_color_manual(values = c("male" = "blue", "female" = "pink"),
                             name = "Gender")
        
      }
      
      else {
        
        ggplot(peng1, aes(x = bill_length_mm,
                          y = bill_depth_mm)) +
          geom_point() +
          labs(title = input$sel_peng)
          
        
      }
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
