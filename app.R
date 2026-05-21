# Load libraries and source code
library(shiny)
source("fitness_landscape.R")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Fitness Landscape"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            # Sliders for relative fitness for each genotype
            sliderInput("w11",
                        "w11:",
                        min = 0,
                        max = 1,
                        value = 1),
            sliderInput("w12",
                        "w12:",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("w22",
                        "w22:",
                        min = 0,
                        max = 1,
                        value = 1),
            # Button for whether to include the legend
            radioButtons("add_legend",
                         "Include Legend:",
                         choiceNames = c("Yes","No"),
                         choiceValues = c(TRUE,FALSE)),
            # Button for whether to treat as relative or absolute fitness
            radioButtons("rel_fit",
                         "Fitness:",
                         choiceNames = c("Relative","Absolute"),
                         choiceValues = c(TRUE,FALSE)),
            width = 2
            
        ),

        # Display the output plot
        mainPanel(
           plotOutput("myPlot")
        )
    )
)

# Define server logic required to create the output plot
server <- function(input, output) {

    output$myPlot <- renderPlot({
        
      # See source code for fl_plot function
      fl_plot(w11 = as.numeric(input$w11), 
              w12 = as.numeric(input$w12), 
              w22 = as.numeric(input$w22),
              plot_type = "plot_window",
              add_legend = input$add_legend,
              rel_fit = input$rel_fit)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
