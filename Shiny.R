# Load necessary package
library(shiny)

## User interface

ui <- fluidPage(
    headerPanel("Analysis of the attitudes to democracy, news consumption and science"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Select country for your analysis", choices = country.name,
                        multiple = FALSE),
        ),
        mainPanel(
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    title = "Overview",
                    "This application helps you analyze the Attitudes to Democracy, News consumption and Attitudes to Science 
            using the World Value Survey (Wave 6) (2010 - 2014). In the next tabs, please select the country in the drop-down list, 
            and see the results in the tables and plots below. The names of the variables correspond to the numbers in the questionnaire."
                ),
                tabPanel(
                    title = "Democracy",
                    plotOutput("plot_democracy", width = "800px"),
                    dataTableOutput("democracy"),
                    dataTableOutput("democracy_all")
                ),
                tabPanel(
                    title = "News consumption",
                    plotOutput("plot_news", width = "800px"),
                    dataTableOutput("news"),
                    dataTableOutput("news_all")
                ),
                tabPanel(
                    title = "Science",
                    plotOutput("plot_science", width = "800px"),
                    dataTableOutput("science"),
                    dataTableOutput("science_all")
                    
                )
            ))))

## Server

server <- shinyServer(function(input, output, session) {
    country <- reactive({
        get(input$country, country.name)
    })
    output$plot_democracy <- renderPlot({
        democracy[complete.cases(democracy),] %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = country))+
            geom_point(aes(y = m.V228A, colour = "Votes are counted fairly"))+
            geom_point(aes(y = m.V228B, colour = "Opposition candidates are prevented from running"))+
            geom_point(aes(y = m.V228C, colour = "TV news favors the governing party"))+
            geom_point(aes(y = m.V228D, colour = "Voters are bribed"))+
            geom_point(aes(y = m.V228E, colour = "Journalists provide fair coverage of elections"))+
            geom_point(aes(y = m.V228F, colour = "Election officials are fair"))+
            geom_point(aes(y = m.V228G, colour = "Rich people buy elections"))+
            geom_point(aes(y = m.V228H, colour = "Voters are threatened with violence"))+
            geom_point(aes(y = m.V228I, colour = "Voters are offered a genuine choice"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                  text = element_text(size = 12)) + 
            xlab("Country") + 
            ylab("Mean Reported Attitude to Democracy") + 
            labs(title = "Self-reported Attitude to Democracy", 
                 subtitle = "(1: Very often, 4: Not at all often)",
                 caption = "Source: World Values Survey, Wave 6, 2010-14",
                 colour = "How often do the following things occur in this country’s elections?")
    })
    output$democracy <- renderDataTable(democracy %>% 
                                            filter(country == input$country))
    output$democracy_all <- renderDataTable(democracy,
                                            options = list(pageLength = 5))
    output$plot_news <- renderPlot({
        news_long[complete.cases(news_long),] %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = variable, y = as.factor(value), fill = as.factor(value)))+
            geom_bar(stat = "identity")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                  text = element_text(size = 12)) + 
            xlab("Media")+
        ylab("Proportion") + 
            labs(title = "News consumption", 
                 subtitle = "(1: Daily, 2: Weekly, 3: Monthly, 4: < Monthly, 5: Never)",
                 caption = "Source: World Values Survey, Wave 6, 2010-14",
                 fill = "Frequency")
    })
    output$news <- renderDataTable(news %>% 
                                       filter(country == input$country))
    output$news_all <- renderDataTable(news,
                                       options = list(pageLength = 5))
    output$plot_science <- renderPlot({
        science[complete.cases(science),] %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x=country)) +
            geom_point(aes(y = m.V192, colour = "Science makes our lives better"))+
            geom_point(aes(y = m.V193, colour = "Science brings more opportunities"))+
            geom_point(aes(y = m.V194, colour = "We depend too much on science"))+
            geom_point(aes(y = m.V195, colour = "Science breaks down people’s ideas of right and wrong"))+
            geom_point(aes(y = m.V196, colour = "Science is not important"))+
            geom_point(aes(y = m.V197, colour = "The world is better off thanks to science"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1), 
                  text = element_text(size = 12)) + 
            xlab("Country") + 
            ylab("Mean Reported Attitude to Science") + 
            labs(title = "Self-reported Attitude to Science", 
                 subtitle = "(1: Completely disagree, 10: Completely agree)",
                 caption = "Source: World Values Survey, Wave 6, 2010-14",
                 colour = "How much do you agree with the following statements?")
    })
    
    output$science <- renderDataTable(science %>% 
                                          filter(country == input$country))
    output$science_all <- renderDataTable(science,
                                          options = list(pageLength = 5))
})

## Run App

shinyApp(ui=ui, server=server)