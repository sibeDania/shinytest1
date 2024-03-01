library(shiny)
library(palmerpenguins)
library(bslib)
library(bsicons)
library(tidyverse)

head(penguins)

peng <- penguins

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
          
            radioButtons("sel_peng",
                         "Select penguins",
                         choices = c(unique(peng$species))),
          
          checkboxInput("sel_color",
                        "Tick for colores between sexes"),
          
          actionButton("show_labels",
                       "Show labels")),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Min tab",
                     
                     br(),
                     
                     fluidRow(column(width = 3, 
                                     uiOutput("bl_vb")),
                              column(width = 3, 
                                     value_box(
                                       title = "Bill depth",
                                       value = "$5,000",
                                       showcase = div(
                                         img(height = 60, width = 60, src = "sibe_no_bg.PNG")),
                                       class = "bg-danger"
                                     )),
                              column(width = 3, 
                                     value_box(
                                       title = "Flipper length",
                                       value = "$5,000",
                                       showcase = bsicons::bs_icon("bank2"),
                                       class = "bg-success"
                                     )),
                              column(width = 3, 
                                     value_box(
                                       title = "Body mass",
                                       value = "$5,000",
                                       showcase = bsicons::bs_icon("bank2"),
                                       class = "bg-warning",
                                       
                                     ))),
                     
                     br(),
                     
              plotOutput("peng_plot")),
            
            tabPanel("Min anden tab"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bl_vb <- renderUI({
  
    value_box(
      title = "Bill length",
      value = bill_length(),
      p(paste0("For ", input$sel_peng)),
      showcase = bsicons::bs_icon("rulers"),
      class = "bg-primary"
    )
    
  })
  
  bill_length <- reactive({
    
    bl <- peng %>% 
      filter(species == input$sel_peng) %>% 
      summarise(mean = round(mean(bill_length_mm, na.rm = TRUE), digits = 1))
    
    return(bl$mean)
    
  })
  
  
  observeEvent(input$show_labels, {
    
    peng <- peng %>% 
      filter(species == input$sel_peng)
    
  })

    output$peng_plot <- renderPlot({
      
      peng <- peng %>% 
        filter(species == input$sel_peng)
  
      theme_set(theme_minimal())
      
      if(input$sel_color) {
        
      ggplot(peng, aes(x = bill_length_mm, y = bill_depth_mm)) +
        geom_point(aes(color = peng$sex)) +
        labs(x = "Bill length",
             y = "Bill depth",
             title = "Plot",
             subtitle = input$sel_peng) +
          scale_color_manual(values = c("male" = "blue", "female" = "pink"),
                             name = "Gender") +
          geom_text(
            aes(label = ifelse(input$show_labels > 0, as.character(island), "")),
            vjust = -0.5
          )
      }
      
      else {
        
        ggplot(peng, aes(x = bill_length_mm, y = bill_depth_mm)) +
          geom_point() +
          labs(x = "Bill length",
               y = "Bill depth",
               title = "Plot",
               subtitle = input$sel_peng) +
          geom_text(
            aes(label = ifelse(input$show_labels > 0, as.character(island), "")),
            vjust = -0.5
          )
        
      }
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
