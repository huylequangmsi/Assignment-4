# Load necessary package
library(shiny)


## User interface

ui <- fluidPage(
    titlePanel("Analysis of the Attitudes to Democracy, News consumption and Attitudes to Science"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Select country for your analysis", choices = country.name)
        ),
        mainPanel(
            textOutput("text")
        )
    )
)

server <- shinyServer(function(input, output, session) {
    country <- reactive({
        get(input$country, country.name)
    })
    output$text <- renderText({
        "This application helps you analyze the Attitudes to Democracy, News consumption and Attitudes to Science 
        using the World Value Survey (Wave 6). Please select the country in the drop-down list, and see the results
        in the tables and plots below."
    })
})

shinyApp(ui=ui, server=server)

