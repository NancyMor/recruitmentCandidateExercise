library(shiny)
library(highcharter)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #Reactive events to Adstock upgrade.
    observeEvent(
    input$sel2,
    updateSelectInput(session, input = 'sel1', selected = input$sel2))

    observeEvent(
        input$sel1,
        updateSelectInput(session, input = 'sel2', selected = input$sel1))


    observeEvent(input$sel1,{
        search_data <- original_data
        ignoreInit=T
    })


    #Data update with adstocks
    search_data <- reactive({
      media_spend <- original_data$`Media Spend (USD)`
              for(i in 2:nrow(original_data)){
              media_spend[i] <-  media_spend[i] + input$sel1 * media_spend[i - 1]
              }
              adstock_data <- original_data
              adstock_data$`Media Spend (USD)` <- media_spend
              return(adstock_data)
              })

    #Model Results
    linear_model <- reactive({req(search_data)
                              search_data <- search_data()
                              mixed_linear_model(search_data)$data_results %>% as.data.table()})
    model <- reactive({req(search_data)
                       search_data <- search_data()
                       mixed_linear_model(search_data)$model})

    output$spend <- renderHighchart({
        req(search_data())
        descriptive_plot <- highchart() %>%
            hc_add_series(
                data = search_data(),
                "line",
                hcaes(
                    x = dmy(`Date (Week)`),
                    y = `Media Spend (USD)`,
                    group = `Media Campaign`
                )
            ) %>%
          hc_title(text = "Media Investment (USD)") %>%
          hc_xAxis(type = 'datetime') %>%
            hc_yAxis(title = list(text = "Media Spend (USD)"))

        return(descriptive_plot)

    })

    output$search <- renderHighchart({
        req(search_data)
        search_data <- search_data()
        descriptive_plot <- highchart() %>%
            hc_add_series(
                data = search_data,
                "line",
                hcaes(
                    x = dmy(`Date (Week)`),
                    y = `Search Volume`
                )
            ) %>%
            hc_title(text = "Google Search Volume") %>%
            hc_xAxis(type = 'datetime') %>%
            hc_yAxis(title = list(text = "Search Volume")) %>%
            hc_legend(enabled = FALSE)

        return(descriptive_plot)

    })


    output$descriptive_data <- renderTable({
      descriptive <- search_data() %>% .[, Year := year(dmy(`Date (Week)`))] %>% .[, .(Year, `Media Spend (USD)`,`Media Campaign`, `Search Volume`)] %>%
                                       .[, .(`Spend(USD)` = sum(`Media Spend (USD)`),
                                                 `Search Volume` = sum(`Search Volume`)),by = .(Year, `Media Campaign`)]

      return(descriptive)

    })

    output$correlation <- renderUI({
        cor_base <- dcast.data.table(search_data(), . ~ "Media Campaign", value.var = "Media Spend (USD)", fun.aggregate = sum, fill = NA)
        camp1 <- search_data() %>% .[`Media Campaign` == 1, ] %>% .[, .(Campign1 = `Media Spend (USD)`, `Search Volume`)] %>% cor() %>% .[1,2] %>% round(2)
        camp2 <- search_data() %>% .[`Media Campaign` == 2, ] %>% .[, .(Campign2 = `Media Spend (USD)`, `Search Volume`)] %>% cor() %>% .[1,2] %>% round(2)
        camp3 <- search_data() %>% .[`Media Campaign` == 3, ] %>% .[, .(Campign3 = `Media Spend (USD)`, `Search Volume`)] %>% cor() %>% .[1,2] %>% round(2)


        box(width = 12,
            splitLayout(
              box(title = h2(camp1), "Campaing1", solidHeader = TRUE),
              box(title = h2(camp2), "Campaing2", solidHeader = TRUE),
              box(title = h2(camp3), "Campaing3", solidHeader = TRUE)
            ))

        })

    output$estimated <- renderHighchart({
        req(search_data)
        req(linear_model)
        descriptive_plot <- highchart() %>%
            hc_title(text = "Search Volume vs Estimated") %>%
            hc_add_series(data = linear_model(), "line", name = "Search Volume", hcaes(x = dmy(`Date (Week)`), y = `Search Volume`)) %>%
            hc_add_series(data = linear_model(), "line", name = "Estimated Volume",  hcaes(x = dmy(`Date (Week)`), y = `Estimated Search Volume`)) %>%
            hc_yAxis(title = list(text = "Media Spend (USD)"))%>%
            hc_xAxis(type = 'datetime')

        return(descriptive_plot)

    })


  output$linear1 <- renderPlotly({
        linear_model <- linear_model()
        data_campaign1 <- linear_model %>% as.data.table() %>% .[`Media Campaign`  == 1, ]
        linear_plot <- plot_ly(data_campaign1)
        linear_plot <- linear_plot %>% add_markers(x = ~  `Media Spend (USD)` , y = ~ `Search Volume`)
        linear_plot <- linear_plot %>% add_lines(x = ~  `Media Spend (USD)`, y = ~ `Estimated Search Volume`,
                                 line = list(color = '#07A4B5'),
                                 name = "Loess Smoother",
                                 showlegend = FALSE) %>%
                       layout(title = 'Media Campaign 1')

       return(linear_plot)

    })

    output$linear2 <- renderPlotly({
        linear_model <- linear_model()
        data_campaign2 <- linear_model %>% as.data.table() %>% .[`Media Campaign`  == 2, ]

        linear_plot <- plot_ly(data_campaign2)
        linear_plot <- linear_plot %>% add_markers(x = ~  `Media Spend (USD)` , y = ~ `Search Volume`)
        linear_plot <- linear_plot %>% add_lines(x = ~  `Media Spend (USD)`, y = ~ `Estimated Search Volume`,
                                         line = list(color = '#07A4B5'),
                                         name = "Loess Smoother",
                                         showlegend = FALSE) %>%
                        layout(title = 'Media Campaign 2')
        return(linear_plot)

    })

    output$linear3 <- renderPlotly({
        linear_model <- linear_model()
        data_campaign3 <- linear_model %>% as.data.table() %>% .[`Media Campaign`  == 3, ]

        linear_plot <- plot_ly(data_campaign3)
        linear_plot <- linear_plot %>% add_markers(x = ~  `Media Spend (USD)` , y = ~ `Search Volume`)
        linear_plot <- linear_plot %>% add_lines(x = ~  `Media Spend (USD)`, y = ~ `Estimated Search Volume`,
                                         line = list(color = '#07A4B5'),
                                         name = "Loess Smoother",
                                         showlegend = FALSE) %>%
                       layout(title = 'Media Campaign 3')
        return(linear_plot)

    })

    output$total_accuracy <- renderUI({

      linear_model <- linear_model()
      model <- model()

      results_summary <- summary(model)
      r_square <- function(x,y){cor(x,y)^2}

      #Getting result for total and each campaign
      total <- linear_model %>% .[,r_square( `Search Volume`, `Estimated Search Volume`)]

      box(title = h2(round(total,3), br(), h4("Total Accuracy(R-Squared)")))

    })

    output$accuracy <- renderTable({

        linear_model <- linear_model()
        model <- model()

        results_summary <- summary(model)
        r_square <- function(x,y){cor(x,y)^2}

        #Getting result for total and each campaign
        camp1 <- linear_model[`Media Campaign`  == 1,] %>% .[,r_square( `Search Volume`, `Estimated Search Volume`)]
        camp2 <- linear_model[`Media Campaign`  == 2,] %>% .[,r_square( `Search Volume`, `Estimated Search Volume`)]
        camp3 <- linear_model[`Media Campaign`  == 3,] %>% .[,r_square( `Search Volume`, `Estimated Search Volume`)]
        total <- linear_model %>% .[,r_square( `Search Volume`, `Estimated Search Volume`)]

        #R squared result
        r_square_results <- data.table("R2" = c(camp1, camp2, camp3),
                                       "Media Campaign" = c(1, 2, 3))

        #Paremeters Results
        parameters_table <- linear_model[, .(`Media Campaign`, `Beta`, `Intercept`)] %>% unique()

        #Merging table
        table_results <- merge(r_square_results, parameters_table, by = "Media Campaign")

        return(table_results)

    })

    output$roi <- renderTable({
      req(search_data)
      req(input$sel1)
      descriptive <- search_data() %>% .[, Year := year(dmy(`Date (Week)`))] %>% .[, .(Year, `Media Spend (USD)`,`Media Campaign`, `Search Volume`)] %>%
        .[, .(`Spend(USD)` = sum(`Media Spend (USD)`),
              `Search Volume` = sum(`Search Volume`)),by = .(`Media Campaign`)] %>% .[, ROI := `Search Volume`/`Spend(USD)`]

      return(descriptive)

    })

    output$model_results <- renderPrint({
        model <- model()
       summary_results <- summary(model)
       print(summary_results)
       # lmerTest::rand(model)

    })




}
