library(shiny)
library(palmerpenguins)
library(tidyverse)
library(bslib)
library(bsicons)


peng <- penguins

logo <- "https://eadania.com/media/3920/dania-academy-denmark-official-logo2.png"

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
          
          div(img(height = 65, width = 80, src = logo),
              style = "text-align: center;"),
          
          br(),
          hr(),
          br(),
          
          radioButtons("sel_peng",
                       "Select penguins",
                       choices = c(unique(peng$species)))
          
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Min tab",
                     br(),
                     fluidRow(column(width = 3,
                                     uiOutput("bill_length_vb")),
                              
                              column(width = 3,
                                     uiOutput("bill_depth_vb")),

                              column(width = 3,
                                     uiOutput("flipper_length_vb")),
                     
                              column(width = 3,
                                     uiOutput("body_mass_vb")),
                              
                     plotOutput("peng_plot"))),
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
  
  bill_depth <- reactive({
    
    bd <- peng %>% 
      filter(species == input$sel_peng) %>% 
      summarise(mean = round(mean(bill_depth_mm, na.rm = TRUE), digits = 1))
    
    return(bd$mean)
    
  })
  
  flipper_length <- reactive({
    
    fl <- peng %>% 
      filter(species == input$sel_peng) %>% 
      summarise(mean = round(mean(flipper_length_mm, na.rm = TRUE), digits = 1))
    
    return(fl$mean)
    
  })
  
  bill_depth <- reactive({
    
    bd <- peng %>% 
      filter(species == input$sel_peng) %>% 
      summarise(mean = round(mean(bill_depth_mm, na.rm = TRUE), digits = 1))
    
    return(bd$mean)
    
  })

# Value boxes -------------------------------------------------------------

  output$bill_length_vb <- renderUI({
    
    value_box(title = "Avg. Bill length",
              value = bill_length(),
              showcase = icon("ruler"),
              class = "bs-danger")
    
  })
  
  output$bill_depth_vb <- renderUI({
    
    value_box(title = "Avg. Bill depth",
              value = bill_depth(),
              showcase = icon("magnifying-glass"),
              class = "bg-danger")
   
  })
  
  output$flipper_length_vb <- renderUI({
    
    value_box(title = "Avg. Flipper Length",
              value = flipper_length(),
              showcase = bs_icon("3-circle-fill"),
              class = "bg-danger")
    
  })
  
  output$body_mass_vb <- renderUI({
    
    value_box(title = "Avg. Body Mass",
              value = body_mass(),
              showcase = bs_icon("4-circle-fill"),
              class = "bg-danger")
    
  })
# Plots -------------------------------------------------------------------

    

    output$peng_plot <- renderPlot({
      
      peng1 <- peng %>% 
        filter(species == input$sel_peng)
      
      ggplot(peng1, aes(x = bill_length_mm,
                       y = bill_depth_mm)) +
        geom_point() +
        labs(title = input$sel_peng)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
