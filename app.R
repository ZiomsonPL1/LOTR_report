#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(visNetwork)
library(plotly)
library(prophet)
library(xgboost)
library(randomForest)
library(leaflet)
library(stringr)
library(bslib)
library(shinycssloaders)
library(shinydashboard)
library(rnaturalearth)
library(sf)


european_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Denmark",
  "Estonia", "Finland", "France", "Greece", "Spain", "Netherlands",
  "Ireland", "Lithuania", "Luxembourg", "Latvia", "Germany", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Sweden", "Hungary", "Italy"
)

set.seed(123)

BANK_CUSTOMERS <- read.csv("C:/Users/Paawel/Desktop/BANKS/BankCustomers.csv") %>%
  select(-RowNumber)

BANK_CUSTOMERS <- BANK_CUSTOMERS %>%
  mutate(Date = sample(
    seq.Date(from = as.Date("2023-01-01"), to = as.Date("2025-08-01"), by = "day"),
    size = nrow(BANK_CUSTOMERS),
    replace = TRUE
  )) %>%
  mutate(Geography = sample(
    european_countries, nrow(BANK_CUSTOMERS), replace = TRUE)) %>%
  arrange(Date)

BANK_GENDER <- BANK_CUSTOMERS %>%
  group_by(Gender) %>%
  summarise(mean_balance = mean(Balance), .groups = "drop")

europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name_en %in% european_countries) %>% 
  select(name_en, geometry)


ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(title = "BANK CUSTOMERS"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Clients", tabName = "about" , icon = icon("users")),
      menuItem("Churn Analysis", tabName = "churn", icon = icon("chart-line")),
      menuItem("Balance", tabName = "balance", icon = icon("dollar-sign")),
      menuItem("Client segmentation", tabName = "segmentation", icon = icon("th")),
      menuItem("Map", tabName = "map", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      #Zakładka1
      tabItem(tabName = "about",
              fluidRow(
                box(
                  
                  width = 3,
                  solidHeader = TRUE,
                  background = "green",
                  tags$div(
                    style = "text-align: center",
                    tags$h4("Clients number"),
                    tags$h2(nrow(BANK_CUSTOMERS)),
                    tags$i(class = "fa fa-users fa-3x", style = "margin-top:10px;")
                  )
                ),
                
                box(
                  width = 3,
                  solidHeader = TRUE,
                  background = "olive",
                  tags$div(
                    style = "text-align: center",
                    tags$h4("Mean age"),
                    tags$h2(round(mean(BANK_CUSTOMERS$Age, na.rm = TRUE), 2)),
                    tags$i(class = "fa fa-user-clock fa-3x", style = "margin-top:10px;")
                  )
                ),
                
                box(
                  width = 3,
                  solidHeader = TRUE,
                  background = "orange",
                  tags$div(
                    style = "text-align: center",
                    tags$h4("Satisfaction"),
                    tags$h2(round(mean(BANK_CUSTOMERS$Satisfaction.Score, na.rm = TRUE), 2)),
                    tags$i(class = "fa fa-meh fa-3x", style = "margin-top:10px;")
                  )
                ),
                
                box(
                  width = 3,
                  solidHeader = TRUE,
                  background = "lime",
                  tags$div(
                    style = "text-align: center",
                    tags$h4("% of cards"),
                    tags$h2((sum(BANK_CUSTOMERS$HasCrCard) / nrow(BANK_CUSTOMERS)) * 100),
                    tags$i(class = "fa fa-credit-card fa-3x", style = "margin-top:10px;")
                  )
                )
                
              ),
              
              fluidRow(
                column(
                  width = 8,
                  withSpinner(plotOutput("satisfactionHist"), type = 6, color = "#509B51")
                ),
                column(
                  width = 4,
                  selectInput("SEX", "Choose sex",
                              choices = c(unique(BANK_CUSTOMERS$Gender), "Both"),
                              multiple = FALSE,
                              selected = c("Both")),
                  sliderInput("AGE", "Choose range of age",
                              min = min(BANK_CUSTOMERS$Age),
                              max = max(BANK_CUSTOMERS$Age),
                              value = c(18, 92),
                              step = 10),
                  withSpinner(plotlyOutput("complainPie", height = "225px"), type = 6, color = "#509B51")
                )
              )
              ),
      
      #Zakładka2
      tabItem(tabName = "churn",
              fluidRow(
          
                box(
                  width = 3,
                  solidHeader = TRUE,
                  background = "red",
                  tags$div(
                    style = "text-align: center",
                    tags$h4("% of exits"),
                    tags$h2((sum(BANK_CUSTOMERS$Exited) / nrow(BANK_CUSTOMERS)) * 100),
                    tags$i(class = "fa fa-sign-out-alt fa-3x", style = "margin-top:10px;")
                  )
                  ),
                  
                box(
                  width = 9,
                  withSpinner(plotOutput("churnBar", height = "143px"), type = 6, color = "#509B51")
                )
              ),
              
              fluidRow(
                column(
                  width = 6,
                  box(
                    width = NULL,
                    title = "Random Forest Confusion Matrix",
                    solidHeader = TRUE,
                    status = "primary",
                    verbatimTextOutput("rf_conf_matrix"),
                    
                    box(
                      width = NULL,
                      solidHeader = TRUE,
                      withSpinner(plotlyOutput("trendPlot"), type = 6, color = "#509B51")
                    )
                  )
                ),
                
                column(
                  width = 6,
                  box(
                    width = NULL,
                    title = "Forecast of exited clients",
                    solidHeader = TRUE,
                    withSpinner(plotlyOutput("forecastPlot"), type = 6, color = "#509B51")
                  )
                )
              )
              
              
              
              ),
      
      #Zakładka3
      tabItem(tabName = "balance",
              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    background = "blue",
                    tags$div(style = "text-align: center",
                             tags$h4("Average balance of customers"),
                             tags$h2(round(mean(BANK_CUSTOMERS$Balance), 2)),
                             tags$i(class = "fa fa-users fa-3x", style = "margin-top:10px;"))
              )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(width = NULL,
                      solidHeader = TRUE,
                      background = "teal",
                      tags$div(style = "text-align: center",
                               tags$h4("Average balance of men"),
                               tags$h2(round(mean(BANK_GENDER$mean_balance[2]), 2)),
                               tags$i(class = "fa fa-male fa-3x", style = "margin-top:10px;"))
                    
                  )
                ),
                column(
                  width = 6,
                  box(width = NULL,
                      solidHeader = TRUE,
                      background = "purple",
                      tags$div(style = "text-align: center",
                               tags$h4("Average balance of women"),
                               tags$h2(round(mean(BANK_GENDER$mean_balance[1]), 2)),
                               tags$i(class = "fa fa-female fa-3x", style = "margin-top:10px;"))
                      
                  )
                )
              ),
              fluidRow(
                withSpinner(visNetworkOutput("relationGraph"), type = 6, color = "#509B51")
              )
      
      
    ),
    #Zakładka4
    tabItem(tabName = "segmentation",
            fluidRow(
              box(
                width = 3,
                sliderInput("clusters", "Number of clusters:",
                            min = 2, max = 8, step = 1, value = 3),
                actionButton("runSegmentation", "Run Segmentation", icon = icon("play"))
              ),
              box(
                width = 9,
                withSpinner(plotlyOutput("segmentPlot"), type = 6, color = "#509B51")
              )
            ),
            fluidRow(
              box(
                width = 12,
                withSpinner(tableOutput("segmentSummary"), type = 6, color = "#509B51")
              )
            )),
    
   
    
    #Zakładka5
    tabItem(tabName = "map",
            fluidRow(
              column(
                width = 4,
                textOutput("infoText"),
                selectInput("selectGender", "Select Gender",
                            choices = c(unique(BANK_CUSTOMERS$Gender), "Both"),
                            multiple = FALSE,
                            selected = "Both"),
                sliderInput("selectAge", "Select Age",
                            min = min(BANK_CUSTOMERS$Age),
                            max = max(BANK_CUSTOMERS$Age),
                            value = c(18, 92),
                            step = 10)
              ),
              column(
                width = 8,
                withSpinner(leafletOutput("europeMap"), type = 6, color = "#509B51")
              )
            ),
            fluidRow(
              column(
                width = 6,
                tableOutput("statTable")
              )
            ),
            fluidRow(
              column(
                width = 12,
                withSpinner(plotlyOutput("satisfactionTrend"), type = 6, color = "#509B51")
              )
            ))
  )
  
)
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$satisfactionHist <- renderPlot({
      
      plot_filtered <- BANK_CUSTOMERS %>%
        filter(Age >= input$AGE[1],
               Age <= input$AGE[2])
      
      
      if (input$SEX != "Both") {
        plot_filtered <- plot_filtered %>%
          filter(Gender == input$SEX)
      }
      
      validate(need(nrow(plot_filtered) > 0, "Lack of data for these filters."))
      
      plot1 <- plot_filtered %>%
        mutate(bins = cut(Age, breaks = seq(18, 92, 10), right = FALSE)) %>%
        ggplot(aes(x = Satisfaction.Score, fill = bins)) +
        geom_histogram(binwidth = 1, position = "dodge", color = "black") +
        labs(title = "Satisfaction histogram in respect of age groups",
             x = "Satisfaction",
             y = "Number of clients") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "lightgrey", color = "lightgrey"),
              panel.background = element_rect(fill = "lightgrey", color = "lightgrey")) +
        scale_y_continuous(expand = c(0,0))
      
      plot1
      
    })
    
    
    output$complainPie <- renderPlotly({
      
      pie_filtered <- BANK_CUSTOMERS %>%
        filter(Age >= input$AGE[1],
               Age <= input$AGE[2])
      
      
      if (input$SEX != "Both") {
        pie_filtered <- pie_filtered %>%
          filter(Gender == input$SEX)
      }
      
      validate(need(nrow(pie_filtered) > 0, "Lack of data for these filters."))
      
      plotly1 <- pie_filtered %>%
        mutate(bins = cut(Age, breaks = seq(18, 92, 10), right = FALSE)) %>%
        group_by(bins) %>%
        summarise(number_of_complaints = sum(Complain), .groups = "drop") %>%
        plot_ly(labels = ~bins,
                values = ~number_of_complaints,
                type = "pie",
                hole = 0.6,
                marker = list(colors = c("#B3E0A6", "#8CCE7D", "#76BD6A", "#60A855", "#418E4D", "#308344", "#24693D")),
                textinfo = "percent",
                hoverinfo = "label+value+percent") %>%
        layout(showlegend = FALSE,
               title = list(text = "Complaints by age", x = 0.5),
               plot_bgcolor = "lightgrey",
               paper_bgcolor = "lightgrey")
      
      plotly1
      
    })
    
    output$churnBar <- renderPlot({
      
      plot2 <- BANK_CUSTOMERS %>%
        group_by(Gender) %>%
        summarise(exited_number = sum(Exited), .groups = "drop") %>%
        mutate(percent = (exited_number / 10000) * 100) %>%
        ggplot(aes(x = Gender, y = percent)) +
        geom_bar(stat = "identity", position = "dodge", fill = "#60A855") +
        coord_flip() +
        labs(title = "Percent of exited clients",
             x = "Gender",
             y = "% of clients") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "lightgrey", color = "lightgrey"),
              panel.background = element_rect(fill = "lightgrey", color = "lightgrey"))
      
      plot2
    })
    
    output$rf_conf_matrix <- renderPrint({
      
      data_model <- BANK_CUSTOMERS %>%
        select(Exited, Age, Balance, CreditScore, Geography, Gender, HasCrCard, IsActiveMember) %>%
        mutate(across(where(is.character), as.factor))
      
      data_transformed <- model.matrix(Exited ~ ., data = data_model)[, -1]
      data_transformed <- as.data.frame(data_transformed)
      data_transformed$Exited <- as.factor(data_model$Exited)
      
      index <- sample(seq_len(nrow(data_transformed)), size = 0.8 * nrow(data_transformed))
      trainData <- data_transformed[index, ]
      testData <- data_transformed[-index, ]
      
      rf_model <- randomForest(Exited ~ ., data = trainData)
      pred_rf <- predict(rf_model, newdata = testData)
      table(Predicted = pred_rf, Actual = testData$Exited)
      
    })
    
    output$forecastPlot <- renderPlotly({
      
      plotly2 <- BANK_CUSTOMERS %>%
        group_by(Date) %>%
        summarise(num_exited = sum(Exited),
                  remain = n() - sum(Exited)) %>%
        arrange(Date)
      
      plotly2_prophet <- plotly2 %>%
        select(ds = Date, y = num_exited)
      
      model <- prophet(plotly2_prophet, daily.seasonality = TRUE)
      future <- make_future_dataframe(model, periods = 365)
      forecast <- predict(model, future)
      
      plot_ly() %>%
        add_lines(x = forecast$ds, y = forecast$yhat, name = "Forecast") %>%
        add_ribbons(x = forecast$ds,
                    ymin = forecast$yhat_lower,
                    ymax = forecast$yhat_upper,
                    name = "Confidence Interval",
                    fillcolor = 'rgba(96, 168, 85, 0.3)',
                    line = list(color = 'transparent')) %>%
        layout(showlegend = FALSE,
               xaxis = list(title = "Date"),
               yaxis = list(title = "Number of Clients"),
               plot_bgcolor = "lightgrey",
               paper_bgcolor = "lightgrey")
      
      
    })
    
    output$trendPlot <- renderPlotly({
      
      trend_data <- BANK_CUSTOMERS %>%
        group_by(Date) %>%
        summarise(num_of_exited = sum(Exited),
                  num_of_products = sum(NumOfProducts),
                  .groups = "drop") 
      
      trend_data <- trend_data %>%
        pivot_longer(cols = c(num_of_exited, num_of_products),
                     names_to = "variable", values_to = "value") 
      
      plotly2 <- ggplot(trend_data, aes(x = Date, y = value, color = variable)) +
        geom_point() +
        geom_smooth(data = trend_data %>% filter(variable == "num_of_exited"),
                    method = "lm", se = FALSE, color = "orange", linetype = "dashed"
                    ) +
        labs(title = "Exited-Products relationship",
             x = "Date",
             y = "value") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "lightgrey", color = "lightgrey"),
              panel.background = element_rect(fill = "lightgrey", color = "lightgrey"),
              legend.position = "none")
      
      ggplotly(plotly2)
      
      
    })
    
    
    output$relationGraph <- renderVisNetwork({
      
      rel_Graph <- BANK_CUSTOMERS %>%
        select(from = Geography, to = Gender, to_to = Card.Type) %>%
        filter(!is.na(from) & !is.na(to) & !is.na(to_to))
      
      nodes <- rel_Graph %>%
        select(label = from) %>%
        bind_rows(rel_Graph %>% select(label = to)) %>%
        bind_rows(rel_Graph %>% select(label = to_to)) %>%
        distinct() %>%
        mutate(id = row_number())
      
      edges <- rel_Graph %>%
        left_join(nodes, by = c("from" = "label")) %>%
        rename(from_id = id) %>%
        left_join(nodes, by = c("to" = "label")) %>%
        rename(to_id = id) %>%
        left_join(nodes, by = c("to_to" = "label")) %>%
        rename(to_to_id = id)
      
      edges_country_sex <- edges %>%
        select(from = from_id, to = to_id)
      
      edges_sex_card <- edges %>%
        select(from = to_id, to = to_to_id)
      
      edges_final <- bind_rows(edges_country_sex, edges_sex_card) %>%
        distinct()
      
      nodes <- nodes %>%
        mutate(group = case_when(
                label %in% rel_Graph$from ~ "Country",
                label %in% rel_Graph$to ~ "Sex",
                label %in% rel_Graph$to_to ~ "Card"
        ),
               color = case_when(
                 label %in% rel_Graph$from ~ "red",
                 label %in% rel_Graph$to ~ "orange",
                 label %in% rel_Graph$to_to ~ "gold"
               ))
      
      visNetwork(nodes, edges_final, main = "Country-Sex-Card Links", background = "lightgrey") %>%
        visOptions(highlightNearest = TRUE) %>%
        visInteraction(hover = TRUE) %>%
        visPhysics(stabilization = TRUE) %>%
        visNodes(shape = "dot", size = 10, color = list(highlight = "darkorange"), font = list(color = "white")) %>%
        visEdges(smooth = list(enabled = TRUE, type = "curvedCCW")) %>%
        visLegend(addNodes = data.frame(
          label = c("Country", "Sex", "Card"),
          color = c("red", "orange", "gold"),
          shape = "dot"
        ))
      
      
    })
  
    output$infoText <- renderText({
      "These are overall statistics for all countries. If you want more detailed information please click on a country."
    })
    
    selected_country <- reactiveVal(NULL)
    
    
    
    output$europeMap <- renderLeaflet({
      
      leaflet(europe) %>%
        addTiles() %>%
        setView(lng = 15, lat = 50, zoom = 3) %>%
        addPolygons(
          fillColor = "lightgreen",
          color = "black",
          weight = 1,
          layerId = ~name_en,
          label = ~name_en,
          highlightOptions = highlightOptions(color = "darkgreen", weight = 2, bringToFront = TRUE)
        )
        
      
      
    })
    
    observeEvent(input$europeMap_shape_click, {
      clicked_country <- input$europeMap_shape_click$id
      selected_country(clicked_country)
    })
    
    output$statTable <- renderTable({
      
      req(selected_country())
      
      filtered <- BANK_CUSTOMERS %>%
        filter(Geography == selected_country())
      
      if (input$selectGender != "Both") {
        filtered <- filtered %>%
          filter(Gender == input$selectGender)
      }
      
      filtered <- filtered %>%
        filter(Age >= input$selectAge[1],
               Age <= input$selectAge[2])
      
      if (nrow(filtered) == 0) {
        return(data.frame(Message = "No data available for selected filters."))
      }
      
      data.frame(
        Country = selected_country(),
        Gender = input$selectGender,
        AgeRange = paste(input$selectAge[1], "-", input$selectAge[2]),
        AvgBalance = round(mean(filtered$Balance, na.rm = TRUE), 2),
        AvgCreditScore = round(mean(filtered$CreditScore, na.rm = TRUE), 2),
        AvgSalary = round(mean(filtered$EstimatedSalary, na.rm = TRUE), 1),
        AvgSatisfaction = round(mean(filtered$Satisfaction.Score, na.rm = TRUE), 2)
      )
      
      
    })
    
    output$satisfactionTrend <- renderPlotly({
      
      req(selected_country())
      
      grouped <- BANK_CUSTOMERS %>%
        filter(Geography == selected_country()) %>%
        group_by(Date) %>%
        summarise(Avg_sat = mean(Satisfaction.Score), .groups = "drop") %>%
        arrange(Date)
      
      grouped_prophet <- grouped %>%
        select(ds = Date, y = Avg_sat)
      
      model <- prophet(grouped_prophet, daily.seasonality = TRUE)
      grouped_future <- make_future_dataframe(model, periods = 365)
      forecast <- predict(model, grouped_future)
      
      plot_ly() %>%
        add_lines(x = forecast$ds, y= forecast$yhat, name = "Forecast of satisfaction") %>%
        add_ribbons(x = forecast$ds,
                    ymin = forecast$yhat_lower,
                    ymax = forecast$yhat_upper,
                    name = "Confidence Interval",
                    fillcolor = 'rgba(96, 168, 85, 0.3)',
                    line = list(color = 'transparent')) %>%
        layout(showlegend = FALSE,
               xaxis = list(title = "Date"),
               yaxis = list(title = "Satisfaction"),
               plot_bgcolor = "lightgrey",
               paper_bgcolor = "lightgrey")
    
      
    })
    
    
    
    observeEvent(input$runSegmentation, {
      
      seg_data <- BANK_CUSTOMERS %>%
        select(Age, Balance, CreditScore, NumOfProducts, Satisfaction.Score) %>%
        mutate(across(where(is.factor), as.numeric)) %>%
        na.omit()
      
      
      seg_scaled <- scale(select(seg_data, where(is.numeric)))
      
      kmeans_model <- kmeans(seg_scaled, centers = input$clusters, nstart = 25)
      
      pca_res <- prcomp(seg_scaled)
      pca_df <- data.frame(pca_res$x[,1:2]) %>%
        rename(PC1 = PC1, PC2 = PC2) %>%
        mutate(Segment = factor(kmeans_model$cluster))
      
      output$segmentPlot <- renderPlotly({
        
        plot_ly(data = pca_df, x = ~PC1, y = ~PC2,
                color = ~Segment, colors = "Set1",
                type = "scatter", mode = "markers",
                marker = list(size = 8),
                hoverinfo = "text",
                text = ~paste("Segment: ", Segment,
                              "<br>PC1: ", round(PC1, 2),
                              "<br>PC2: ", round(PC2, 2))) %>%
          layout(plot_bgcolor = "lightgrey",
                 paper_bgcolor = "lightgrey")
        
      })
      
      segment_summary <- seg_data %>%
        mutate(Segment = factor(kmeans_model$cluster)) %>%
        group_by(Segment) %>%
        summarise(across(where(is.numeric), mean, .names = "avg_{.col}"), .groups = "drop")
      
      output$segmentSummary <- renderTable({
        segment_summary %>% mutate(across(where(is.numeric), round, 2))
      })
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)




