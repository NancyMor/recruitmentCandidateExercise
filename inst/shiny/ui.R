library(shiny)
library(highcharter)
library(shinydashboard)


#Global Settings
campaign_variable <- "Media Campaign"
campaign_values <- original_data[[eval(campaign_variable)]] %>% unique() %>% sort()

# Define UI for application that draws a histogram
ui <- #fluidPage(#theme = "www/css/style.css",
  navbarPage("Campaign Efficency Analysis",
                 inverse = T,
                 theme = shinytheme('flatly'),
                 tabPanel("About",
                          column(1),
                          column(9,
                                 h2("Recruitment Candidate Exercise"),
                                 h3("Exercise for Solutions Analytics Director role"),
                                 br(),
                                 wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 600px;",
                                 uiOutput('textWithHTML')
                                 ))
                          ),
                 tabPanel("Descriptives",
                          fluidRow(
                              column(12,
                              column(9,
                              h1("Exploring Data"),
                              h4("Srongs correlation between Media Spend and Search Volume")),
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
                                     wellPanel(style = "background-color: #DCDCDC; border-color: #2c3e50; height: 250px;",
                                               tableOutput("descriptive_data")),
                                     h4("Correlation with Sales"),
                                     wellPanel(style = "background-color: #7DCEA0; border-color: #2c3e50; height: 150px;",
                                               uiOutput("correlation"))
                                               ))

                          ),
                 tabPanel("Model",
                          column(12,
                                 column(9,
                                        h1("Linear Mixed Model"),
                                        h4("Model Results")),
                                 column(3,
                                        sliderInput("sel2", "Adstock Value:", min = 0, max = 1, value = 0))),
                          column(8,
                                 wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 300px;",
                                           column(12,
                                         # column(1),
                                          column(7,
                                                 uiOutput("accuracy"),
                                                 tableOutput("roi")),
                                          column(5,
                                           uiOutput("total_accuracy"))

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
                            tabPanel("Documentacion",
                                     h1("Documentation"),
                                     h4("You can find more information in the links below:"),
                                     br(),
                                     h4("Linear Mixed Models, John Fox, 2002"),
                                     tags$a(href="https://www.statpower.net/Content/MLRM/Lab/appendix-mixed-models.pdf", "See Paper"),
                                     br(),
                                     h4("Linear mixed-effect models in R (tutorial), 2017"),
                                     tags$a(href="https://www.researchgate.net/publication/321748226_Linear_mixed-effect_models_in_R_tutorial", "See Tutorial")


                            )
                 )
)

#)
