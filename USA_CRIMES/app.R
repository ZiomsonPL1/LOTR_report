#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(visNetwork)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(prophet)
library(maps)
library(bslib)
library(shinycssloaders)
library(lubridate)
library(leaflet)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)



states_sf <- tigris::states(cb = TRUE, year = 2020) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) %>%
  st_transform(crs = 4326) 


USA <- map_data("state")
USA_cities <- us.cities

state_abbreviations <- data.frame(
  state_name = state.name,
  state_abb = state.abb,
  stringsAsFactors = FALSE
)

state_abbreviations <- state_abbreviations %>%
  mutate(state_name = str_to_lower(state_name))

USA <- USA %>%
  left_join(state_abbreviations, by = c("region" = "state_name"))

USA <- USA %>%
  left_join(USA_cities, by = c("state_abb" = "country.etc"))

USA <- USA %>%
  rename(long = long.x,
         lat = lat.x,
         long_city = long.y,
         lat_city = lat.y) 

USA <- USA %>% 
  mutate(name = str_remove(name, "\\s+[A-Z]{2}$"))

US_CRIMES <- read.csv("C:/Users/Paawel/Desktop/USA_CRIMES/crime_safety_dataset.csv")

US_CRIMES$czas <- as.POSIXct(paste(US_CRIMES$date, US_CRIMES$time),
                             format = "%Y-%m-%d %H:%M:%S")

US_CRIMES <- US_CRIMES %>%
  arrange(czas)

US_CRIMES <- US_CRIMES %>%
  left_join(USA, by = c("city" = "name"))

US_CRIMES <- US_CRIMES %>%
  distinct(time, .keep_all = TRUE) %>%
  select(-c(order, subregion, state_abb, lat_city, long_city, capital))

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
  theme = bs_theme(bootswatch = "darkly"),
    titlePanel("US CRIMES VISUALIZATION"),

 tabPanel("Dashboard", 
  fluidRow(
    column(
      width = 3,
      dateRangeInput("DATA",
                     "Give date: ",
                     start = US_CRIMES$date[1],
                     end = US_CRIMES$date[nrow(US_CRIMES)],
                     min = "2020-01-01",
                     max = "2025-12-31",
                     format = "yyyy-mm-dd",
                     separator = " do "),
      sliderInput("GODZINA",
                  "Give an hour: ",
                  min = 0,
                  max = 23,
                  value = c(8, 20),
                  step = 1)
    ),
    column(
      width = 6,
      withSpinner(
        leafletOutput("USAPlot", height = "450px", width = "100%"),
        image = "https://www.gify.net/data/media/930/kogut-policyjny-ruchomy-obrazek-0005.gif"
      )
    ),
    column(
      width = 3,
      h4("Crimes in cities"),
      tableOutput("CityTable")
    )
  ),
  
  fluidRow(
    column(
      width = 6,
      uiOutput("stateTitle"),
      plotOutput("crimeBarPlot", height = "300px", width = "100%")
    ),
    column(
      width = 6,
      plotOutput("trendPlot", height = "300px", width = "100%")
    )
  )
),


 tabPanel("Victims",
            fluidRow(
              column(
                width = 3,
                selectInput("GENDER", "Choose gender", 
                            choices = c("All", sort(unique(US_CRIMES$victim_gender)))),
                selectInput("RACE", "Choose race",
                            choices = c("All", sort(unique(US_CRIMES$victim_race)))),
                actionButton("RESET", "Reset filters", icon = icon("redo"))
                
              ),
              column(
                width = 6,
                h3("Information about victims"),
                visNetworkOutput("graphPlot", height = "450px", width = "100%")
              ),
              column(
                width = 3,
                plotlyOutput("piePlot", height = "450px")
              )
            )
          
          )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    clicked_state <- reactiveVal(NULL)  
  
    output$USAPlot <- renderLeaflet({
      
      validate(
        need(input$DATA[1] <= input$DATA[2], "❌ Given date is inaccurate. Please provide correct date.")
      )
      
        US_CRIMES_filtered <- US_CRIMES %>%
          filter(date >= input$DATA[1],
                 date <= input$DATA[2],
                 lubridate::hour(czas) >= input$GODZINA[1],
                 lubridate::hour(czas) <= input$GODZINA[2])
          
      
        US_CRIMES1 <- US_CRIMES_filtered %>%
          group_by(region) %>%
          summarise(crime_number = n(),
                    state_abb = first(state),
                    .groups = "drop")
        
        states_merged <- states_sf %>%
          left_join(US_CRIMES1, by = c("STUSPS" = "state_abb"))
         
        validate(
          need(nrow(states_merged) > 0, "❌ No data for this period"),
          need(sum(states_merged$crime_number, na.rm = TRUE) > 0, "❌ No crime data for this period")
        )
        
        pal <- colorNumeric(palette = "Blues", domain = states_merged$crime_number)
        
        leaflet(data = states_merged) %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addPolygons(
            fillColor = ~pal(crime_number),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            smoothFactor = 0.5,
            layerId = ~STUSPS,
            popup = ~paste0(
              "<strong>", str_to_title(region), "</strong><br>",
              "Number of violations: ", ifelse(is.na(crime_number), 0, crime_number)
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~crime_number,
            title = "Number of Violations",
            opacity = 1
          ) %>%
          fitBounds(
            lng1 = -125, lat1 = 24,
            lng2 = -66.9, lat2 = 49.5
          )
          
    })
    
    output$CityTable <- renderTable({
      validate(need(clicked_state(), "⚠️ Click on state to get data"))
      
      city_crime <- US_CRIMES %>%
        filter(date >= input$DATA[1],
               date <= input$DATA[2],
               lubridate::hour(czas) >= input$GODZINA[1],
               lubridate::hour(czas) <= input$GODZINA[2],
               state == clicked_state()) %>%
        group_by(city) %>%
        summarise(`crime number` = n(), .groups = "drop") %>%
        arrange(desc(`crime number`))
      
      if(nrow(city_crime) == 0){
        return(data.frame(Message = "No data for this period"))
      }
      
      city_crime
      
    })
    
    observeEvent(input$USAPlot_shape_click, {
      clicked_state(input$USAPlot_shape_click$id)
    })
    
    output$stateTitle <- renderUI({
      req(clicked_state())
      h4(paste("Crime types in", clicked_state()))
    })
    
    output$crimeBarPlot <- renderPlot({
      
      validate(
        need(clicked_state(), "⚠️ Click on state to get data")
      )
      
      filtered_crimes <- US_CRIMES %>%
        filter(date >= input$DATA[1],
               date <= input$DATA[2],
               lubridate::hour(czas) >= input$GODZINA[1],
               lubridate::hour(czas) <= input$GODZINA[2],
               state == clicked_state())
      
      plot_crimes <- filtered_crimes %>%
        group_by(crime_type) %>%
        summarise(liczba_przestepstw = n(), .groups = "drop") %>%
        ggplot(aes(x = crime_type, y = liczba_przestepstw)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(
             x = "Type of crime",
             y = "Crime number") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "#222222", color = "#222222"),
              plot.background = element_rect(fill = "#222222", color = "#222222"),
              axis.text.x = element_text(angle = 30, size = 10, color = "white"),
              axis.text.y = element_text(color = "white"),
              axis.title = element_text(color = "white"))
      
      plot_crimes
      })
    
    output$trendPlot <- renderPlot({
      
      validate(need(clicked_state(), "⚠️ Click on state to get data"))
      
      yearly_trend <- US_CRIMES %>%
        filter(state == clicked_state()) %>%
        mutate(year = lubridate::year(czas)) %>%
        group_by(year) %>%
        summarise(crimes = n(), .groups = "drop")
        
      trend_plot <- ggplot(yearly_trend, aes(x = year, y = crimes)) +
        geom_line(color = "skyblue", linewidth = 1) +
        geom_point(color = "steelblue", size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "orange", linetype = "dashed") +
        labs(title = paste("Annual crime trend in", clicked_state()),
             x = "Year",
             y = "Number of crimes") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "#222222", color = "#222222"),
              plot.background = element_rect(fill = "#222222", color = "#222222"),
              axis.title = element_text(color = "white"),
              axis.text = element_text(color = "white"),
              plot.title = element_text(color = "white", size = 14))
      trend_plot
    })
    
    observeEvent(input$RESET, {
      updateSelectInput(inputId = "GENDER", selected = "All")
      updateSelectInput(inputId = "RACE", selected = "All")
    })
    
    output$graphPlot <- renderVisNetwork({
      
      if (input$GENDER != "All") {
        US_CRIMES <- US_CRIMES %>%
          filter(victim_gender == input$GENDER)
      }
      
      if (input$RACE != "All") {
        US_CRIMES <- US_CRIMES %>%
          filter(victim_race == input$RACE)
      }
      
      victims_graph <- US_CRIMES %>%
        select(from = victim_gender, to = victim_race, to_to = crime_type)
      
      wezly <- victims_graph %>%
        select(label = from) %>%
        bind_rows(victims_graph %>% select(label = to)) %>%
        bind_rows(victims_graph %>% select(label = to_to)) %>%
        distinct() %>%
        mutate(id = row_number())
      
      krawedzie <- victims_graph %>%
        left_join(wezly, by = c("from" = "label")) %>%
        rename(from_id = id) %>%
        left_join(wezly, by = c("to" = "label")) %>%
        rename(to_id = id) %>%
        left_join(wezly, by = c("to_to" = "label")) %>%
        rename(to_to_id = id)
      
      
      krawezie_gender_race <- krawedzie %>%
        select(from = from_id, to = to_id)
      
      krawedze_race_crime <- krawedzie %>%
        select(from = to_id, to = to_to_id)
      
      krawedzie_final <- bind_rows(krawezie_gender_race, krawedze_race_crime) %>%
        distinct()
      
      wezly <- wezly %>%
        mutate(group = case_when(
          label %in% victims_graph$from ~ "gender",
          label %in% victims_graph$to ~ "race",
          label %in% victims_graph$to_to ~ "crime"
        ))
      
      visNetwork(wezly, krawedzie_final, main = "Relation between gender, race and crime", background = "#222222") %>%
        visOptions(highlightNearest = TRUE) %>%
        visLayout(randomSeed = 123) %>%
        visInteraction(hover = TRUE) %>%
        visPhysics(stabilization = TRUE) %>%
        visNodes(shape = "dot", size = 10, color = list(highlight = "#2E5B88", hover = "#8DBBDC"), font = list(color = "white")) %>%
        visEdges(smooth = list(enabled = TRUE, type = "curvedCW")) %>%
        visGroups(
          groupname = "gender",
          color = list(background = "#3690AE", border = "#3480A2")
        ) %>%
        visGroups(
          groupname = "race",
          color = list(background = "#B3D5E9", border = "#699CC5")
        ) %>%
        visGroups(
          groupname = "crime",
          color = list(background = "#645A9D", border = "#433191")
        ) %>%
        visLegend(addNodes = list(
          list(label = "Gender", shape = "dot", color = "#3690AE"),
          list(label = "Race", shape = "dot", color = "#B3D5E9"),
          list(label = "Crime", shape = "dot", color = "#645A9D")
        ),
        width = 0.1,
        position = "right")
      
    })
    
    output$piePlot <- renderPlotly({
      
      req(clicked_state())
      
      victim_data <- US_CRIMES %>%
        filter(state == clicked_state(),
               date >= input$DATA[1], 
               date <= input$DATA[2],
               lubridate::hour(czas) >= input$GODZINA[1],
               lubridate::hour(czas) <= input$GODZINA[2]) %>%
        group_by(victim_race) %>%
        summarise(liczba_ofiar = n(), .groups = "drop") %>%
        mutate(procent_ofiar = liczba_ofiar / sum(liczba_ofiar) * 100)
      
      labels <- victim_data$victim_race
      values <- victim_data$procent_ofiar
      
      validate(need(input$DATA[1] <= input$DATA[2], "❌ Given date is inaccurate. Please provide correct date."),
               need(nrow(victim_data) > 0, "❌ No victim data for the selected state and time period")
      )
      
      piePlot <- plot_ly(
        labels = labels,
        values = values,
        type = "pie",
        hole = 0.6,
        marker = list(colors = colorRampPalette(c("#B9DDF1", "#2A5783"))(5)),
        textinfo = "label",
        hoverinfo = "percent"
      )
      
      piePlot <- piePlot %>%layout(
        title = "Race of victims",
        showlegend = FALSE,
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "white")
      )
      
      piePlot
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)





