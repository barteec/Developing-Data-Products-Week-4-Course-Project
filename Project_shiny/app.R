
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(plotly)
library(colourpicker)
library(ggplot2)
library(gapminder)
library(shinycustomloader)
library(DT)

ui <- fluidPage(
    
    # App title ----
    titlePanel("Developing Data Products Assign-4 w/ Gapminder Data"),
    p("Created by Bently Bartee"), 
    p(paste("created 2021-02-10")),
    p("An excerpt of the data available at", a(href="https://www.gapminder.org/data/", "Gapminder.org"), 
      "For each of 142 countries, the package provides values for life expectancy, Gross Domestic Product (GDP), and population, 
    every five years, from 1952 to 2007. Data can be reused freely but please attribute the original data source 
    (where applicable) and Gapminder."),
    p("My source code: ",
      a(href="https://github.com/barteec/Developing-Data-Products-Week-4-Course-Project", 
        "published on GitHub.")),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select the random distribution type ----
            textInput("title", "Project Title", "GDP vs Life Expectancy"),
            numericInput("size", "Point size", 1, 1),
            checkboxInput("fit", "Add line of best fit", FALSE),
            colourInput("color", "Point color", value = "blue"),
            
            
            selectInput("continent", "Continent",
                        choices = c("All", levels(gapminder$continent))),
            
            sliderInput(inputId = "life", label = "Life expectancy",
                        min = 0, max = 120,
                        value = c(30, 50)),
            downloadButton("download_data")
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Gapminder Plot", withLoader(plotlyOutput("plot")) ),
                        tabPanel("Gapminder Table", withLoader(DT::dataTableOutput("table")))
                        
                        
            )
            
        )
    )
)


server <- function(input, output) {
    filtered_data <- reactive({
        data <- gapminder
        data <- subset(
            data,
            lifeExp >= input$life[1] & lifeExp <= input$life[2]
        )
        if (input$continent != "All") {
            data <- subset(
                data,
                continent == input$continent
            )
        }
        data
    })
    
    output$table <- DT::renderDataTable({
        data <- filtered_data()
        data
    })
    
    output$download_data <- downloadHandler(
        filename = function() {
            paste("gapminder-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(filtered_data(), file)
        }
    )
    
    
    output$plot <- renderPlotly({
        
        ggplotly({
            data <- filtered_data()
            
            p <- ggplot(data, aes(gdpPercap, lifeExp)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle(input$title) + 
                labs(y="Life Expectancy", x = "Gross Domestic Product (GDP) per Capita")+
                
                
                if (input$fit) {
                    p <- p + geom_smooth(method = "lm")
                }
            p
        })
    })
}

shinyApp(ui = ui, server = server)