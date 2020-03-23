library(shiny)
library(highcharter)

#Global Settings
campaign_variable <- "Media Campaign"
campaign_values <- original_data[[eval(campaign_variable)]] %>% unique() %>% sort()

# Define UI for application that draws a histogram
ui <- navbarPage("Campaign Efficency Analysis",
                 inverse = T,
                 theme = shinytheme('flatly'),
                 tabPanel("Descriptives",
                          fluidRow(
                              column(12,
                              column(9,
                              h1("Exploring Data")),
                              column(3,
                              sliderInput("sel1", "Adstock Value:", min = 0, max = 1, value = 0))),
                              #selectInput("uno", "example", campaign_values)),
                              column(9,
                                     wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 600px;",
                                               highchartOutput("search",height = "300px"),
                                               highchartOutput("spend",height = "300px"))),
                              column(3,
                                     #uiOutput("correlation"),
                                     h4("Summary by Year and Campaign"),
                                     wellPanel(style = "background-color: #AFC7D0; border-color: #AFC7D0; height: 250px;",
                                               tableOutput("descriptive_data")),
                                     h4("Correlation with Sales"),
                                     wellPanel(style = "background-color: #AFC7D0; border-color: #AFC7D0; height: 150px;",
                                               uiOutput("correlation"))
                                               ))

                          ),
                 tabPanel("Summary",
                          column(12,
                                 column(9,
                                        h1("Linear Mixed Model")),
                                 column(3,
                                        sliderInput("sel2", "Adstock Value:", min = 0, max = 1, value = 0))),
                          column(8,
                                 h4("Model Results"),
                                 wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 300px;",
                                           column(12,
                                          column(6,
                                           uiOutput("total_accuracy")),
                                          column(6,
                                           uiOutput("accuracy"),
                                           tableOutput("roi")
                                          )
                                           )
                                           ),
                                 highchartOutput("estimated",height = "300px")
                          ),
                          column(4,
                                           fluidRow(plotlyOutput("linear1", height = "200px"),
                                                    br(),
                                                    br(),
                                                    plotlyOutput("linear2", height = "200px"),
                                                    br(),
                                                    br(),
                                                    plotlyOutput("linear3", height = "200px")
                                           )
                                 )
                 ),
                 navbarMenu("More",
                            tabPanel("Table",
                                     DT::dataTableOutput("table")
                            )
                 )
)
