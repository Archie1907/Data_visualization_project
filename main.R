  # import library
  library(shiny)
  library(shinyWidgets)
  library(shinythemes)
  library(tidyverse)
  library(plotly)
  library(leaflet)
  library(dplyr)
  library(htmlwidgets)
  
  # olynmpic dataset for plotting map
  olympic <- read.csv("noc.csv")
  
  #load data
  olynmpic <- read_csv("nocathletes.csv")
  
  # filter data
  olynmpic <- olynmpic %>% filter(Year != 2020)
  
  
  # preprocess data
  agg_data <- olynmpic %>%
    group_by(Year, Season, Sport) %>%
    summarise(peo_count = n_distinct(ID)) %>%
    arrange(Year, Season)
  
  agg_data1 <- olynmpic %>%
    group_by(Year, Season) %>%
    summarise(SportList = list(unique(Sport)),
              SportCount = n_distinct(Sport)) %>%
    arrange(Year, Season)
  
  agg_data1 <- agg_data1 %>%
    group_by(Season) %>%
    mutate(NewSports = map2(SportList, lag(SportList), setdiff)) %>%  
    ungroup()
  
  agg_data1$NewSportsText <- map_chr(agg_data1$NewSports, paste, collapse = ', ')
  
  # load data for athletic characteristic
  olympic_athlete <- read_csv("athletes.csv")
  
  # Use the filter function to remove rows in the Year column with a value of 2020
  olympic_athlete<- olympic_athlete %>%
    filter(Year != 2020)
  
  # Calculation of the number of male and female participants per year
  gender_data <- olympic_athlete %>%
    group_by(Year, Season, Sex) %>%
    summarise(Count = n_distinct(ID))
  
  # Calculate the total number of entries
  total_participants <- gender_data %>%
    group_by(Year, Season) %>%
    summarise(TotalCount = sum(Count))
  
  # Consolidation of data
  gender_data <- left_join(gender_data, total_participants, by = c("Year", "Season"))
  
  # Calculation of the proportion of men and women in the total
  gender_data <- gender_data %>%
    mutate(Percentage = (Count / TotalCount) * 100)
  
  # Create interactive line charts, organized by seasonal facets
  gg2 <- ggplot(gender_data, aes(x = Year, y = Count, color = Sex, shape = Sex, 
                                 text = paste("Year:", Year, "<br>Count:", Count, "<br>Percentage:", round(Percentage, 2), "%"))) +
    geom_point() +
    labs(title = "Olympic Participation Trend by Gender",
         x = "Year", y = "Number of Participants",
         text = "Percentage") +
    scale_color_manual(values = c("M" = "blue", "F" = "pink")) +
    scale_shape_manual(values = c("M" = 3, "F" = 19)) +  # Use shapes 3 and 19 to distinguish between men and women
    facet_wrap(~ Season) +
    theme_minimal()
  
  # Adding regression lines with geom_smooth()
  gg2 <- gg2 + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(group = Sex))
  
  # Adding hover interactions with the plotly package
  gg2 <- ggplotly(gg2, tooltip = "text")
  

  # Calculation of the average of the heights of male and female athletes per year
  height_summary <- olympic_athlete %>%
    group_by(Year, Season, Sex) %>%
    summarise(Mean_Height = mean(Height, na.rm = TRUE))
  
  # Create line charts, with facets by season
  height_trend <- ggplot(height_summary, aes(x = Year, y = Mean_Height, color = Sex)) +
    geom_line() +
    geom_point() +
    labs(title = "Average Height of Olympic Athletes Over the Years",
         x = "Year", y = "Height") +
    scale_color_manual(values = c("M" = "blue", "F" = "pink")) +
    facet_wrap(~ Season) +  
    theme_minimal()
  
  height_trend_plotly <- ggplotly(height_trend, tooltip = "text")
  
  
  # Calculation of the average weight of male and female athletes per year
  weight_summary <- olympic_athlete %>%
    group_by(Year, Season, Sex) %>%
    summarise(Mean_Weight = mean(Weight, na.rm = TRUE))
  
  # Create line charts, with facets by season
  weight_trend <- ggplot(weight_summary, aes(x = Year, y = Mean_Weight, color = Sex)) +
    geom_line() +
    geom_point() +
    labs(title = "Average Weight of Olympic Athletes Over the Years",
         x = "Year", y = "Weight") +
    scale_color_manual(values = c("M" = "blue", "F" = "pink")) +
    facet_wrap(~ Season) +  
    theme_minimal()
  
  weight_trend_plotly <- ggplotly(weight_trend, tooltip = "text")
  
  
  # define UI
  ui <- fluidPage(imageOutput("Olympic"), navbarPage(
    "Olympics Insights from Historical Data Analysis",
    theme = shinytheme("spacelab"),
    tags$head(tags$style(HTML("
                               .navbar {font-size: 20px;
                               background-color: #3498db; }
                               .tab-content {padding: 20px;}
                               "))),
    
    # Page 1: Background
    tabPanel("Introduction", icon = icon("info-circle"),
             fluidPage(
               titlePanel("Background Introduction"),
               textOutput("background")
             )
    ),
    
    tabPanel("Evolution of Sporting Events", icon = icon("chart-line"),
             fluidPage(
               titlePanel("Olympic Sports Participation Over Time"),
               
               fluidRow(
                 column(width = 12,
                        sidebarLayout(
                          sidebarPanel(
                            pickerInput(
                              inputId = "sport",
                              label = "Select a sport:",
                              choices = unique(agg_data$Sport),
                              multiple = TRUE,
                              options = pickerOptions(actionsBox = TRUE)
                            )
                          ),
                          mainPanel(
                            plotlyOutput("plot")
                          )
                        )
                 )
               ),
               
               fluidRow(
                 column(width = 12,
                        plotlyOutput("new_sports_plot")
                 )
               ),
               
               fluidRow(
                 column(width = 12,
                        HTML(
                          "<br>
                          From the above plots, we can see that over the years, the sports disciplines in the Olympics have seen significant changes and evolution. 
                          From a few initial sports to a diverse array of choices today, the Olympics has continually expanded and innovated.
                          <br><br>

                          <strong>Evolution of Summer Olympics:</strong> The Summer Olympics has grown from its initial 9 sports disciplines to 34 today. This growth is not just numeric but also reflects a diversified global sports culture and varied demands of the participants.
                          <br><br>

                          <strong>Evolution of Winter Olympics:</strong> In contrast to the Summer Olympics, the Winter Olympics has seen growth from 10 to 15 disciplines. While the growth pace is slower, it reflects the global popularity of winter sports.
                          <br><br>

                          <strong>Stability and Change in Sports Disciplines:</strong> Even as the number of Olympic sports has been increasing, the number has stabilized in recent years. This might suggest that the Olympic Committee has found a balance that meets the needs of nations while maintaining the quality of the games.
                          <br><br>

                          <strong>Enduring Popularity of Key Sports:</strong> Athletics and swimming have consistently been the crowd-pullers in the Olympics. Particularly, athletics has drawn a massive number of participants both historically and in contemporary times. Additionally, the popularity of swimming has been on an ascent since the 1980s.
                          <br><br>                         

                          <strong>Rise of Emerging Sports:</strong> Beyond the traditional disciplines, the Olympics has incorporated emerging sports like skateboarding and climbing to attract a younger audience and participants.
                          <br><br>                         

                          In conclusion, the sports disciplines in the Olympics over time have displayed a balance of enduring tradition and fresh innovation. This balance ensures the Olympics' sustained appeal and impact on a global scale.
                          <br><br>"
                        )
                 )
               )
             )
    ),
    
               
    tabPanel("Performance of Countries in Olympic History", icon = icon("flag"),
             fluidPage(
               titlePanel("Olympic Medals by Country"),
               
               # Create a sidebar layout with input controls for selecting the year and an output panel for displaying the map
               sidebarLayout(
                 sidebarPanel(
                   
                   # Create a drop-down menu to allow users to select a year
                   selectInput("year", "Select Year:", 
                               choices = sort(unique(olympic$Year)),
                               selected = max(unique(olympic$Year)))  # The latest year is selected by default
                   
                 ),
                 
                 # display map
                 mainPanel(
                   leafletOutput("map"),
                   
                   fluidRow(
                     column(width = 12,
                            HTML(
                              "<strong>Finding One: Dominance of the United States</strong><br>

<strong>Highest Medal Count:</strong> The United States has consistently been the leader in the Olympic medal tally, with a significantly higher number of medals than other countries.
Outperformance: The performance of the United States in the Olympics far surpasses that of other nations, not only in total medal count but also in the number of gold and silver medals.
<br><br>
<strong>Finding Two: Rise of China</strong><br>

<strong>Swift Ascendance:</strong> China has rapidly risen in the Olympics, emerging as a significant competitor. China has achieved remarkable success not only in the total number of medals but also in the count of gold medals.
<br><br>
<strong>Finding Three: Advantages of Developed Countries</strong><br>

<strong>Prominence of Developed Nations:</strong> Most of the countries and regions ranking high in the medal standings are economically developed nations. These countries allocate more resources to nurture and support athletes, resulting in outstanding performances at the Olympics.
<br><br>
<strong>Performance Gap in Asia, Africa, and Latin America:</strong> In contrast, countries in Asia, Africa, and Latin America have shown comparatively weaker performances at the Olympics. This may reflect the influence of economic conditions on sports development, as economically developed regions typically provide better training facilities and support.
<br><br>
These findings highlight the varying performances of nations at the Olympics and underscore the significance of economic prosperity in sporting achievements. The dominance of the United States and the advantages of developed countries emphasize the pivotal role of sporting prowess and resources, while countries in Asia, Africa, and Latin America may need to address a range of challenges to enhance their competitiveness at the Olympics. These findings hold vital implications for a deeper understanding of national Olympic performances and the impact on sports development."
                            )
                     )
                   )
                 )
               )
             )
    ),
    
    
    
    # Fourth Page: Changes in Athlete Characteristics
    tabPanel("Changes in Athlete Characteristics", icon = icon("users"),
             fluidPage(
               titlePanel("Changes in Athlete Characteristics Over Time"),
               fluidRow(
                 column(width = 12, plotlyOutput("height_trend_plot")),
                 column(width = 12, 
                        plotlyOutput("weight_trend_plot"),
                        HTML("<strong>Finding: Changes in Athlete Characteristics</strong><br>
                              From 1890 to 2016, the height and weight of Olympic athletes have shown certain trends of change.
                              <br><br>
                              <strong>Development in Height:</strong><br>

                              <strong>Male Athletes:</strong> Whether in the Summer or Winter Olympics, it can be seen that the height of male athletes increased throughout the 20th century. The growth is especially notable after the 1950s.
                              <br><br>
                              <strong>Female Athletes:</strong> The height of female athletes in both the Summer and Winter Olympics has been on an upward trend since 1890, particularly after the 1950s.
                              <br><br>
                              <strong>Changes in Weight:</strong><br>
                              
                              <strong>Male Athletes:</strong> The weight of male athletes in the Summer Olympics remained relatively stable in the first half of the 20th century, but began to increase in the latter half. The weight of male athletes in the Winter Olympics has been gradually increasing since 1890.
                              <br><br>
                              <strong>Female Athletes:</strong> The weight of female athletes in both the Summer and Winter Olympics has been on an upward trend since 1890.
                              <br><br>
                              <strong>Differences Between Genders:</strong><br>
                              
                              Over the past 120 years, the height and weight of male athletes have generally been higher than that of females. However, as time has passed, the difference between the two seems to be narrowing, especially in terms of weight.
                              <br><br>
                              <strong>Comparison between Summer and Winter Olympics:</strong><br>
                              
                              Overall, athletes in the Winter Olympics seem to have slightly higher height and weight than those in the Summer Olympics.
                              <br><br>
                              These changes may be related to various factors, including advancements in training methods, improvements in nutritional status, and adjustments to the selection criteria for high-level competitive athletes.
                              <br><br>")
                 ),
                 column(width = 12, 
                        plotlyOutput("gender_trend_plot"),
                        HTML("Over time, there have been significant shifts in athlete characteristics within the Olympic Games:
                              <br><br>
                              <strong>Narrowing Gender Gap:</strong> While early editions of the Olympics saw a predominance of male participants, there has been a marked uptick in female athlete participation in recent years. This is likely indicative of the growing equal rights and opportunities women are gaining in the sporting domain.
                              <br><br>
                              <strong>Increased Participation in Summer Olympics:</strong> The number of participants in the Summer Olympics has shown a consistent increase over time, while the Winter Olympics has witnessed a slower growth rate. This might be attributed to the variety of events in the Summer Olympics, the diversity of participating nations, and the global popularity of summer sports.
                              
                              <br><br>
                              <strong>Factors Affecting Participation:</strong> Some years have seen deviations in actual participation numbers from the expected trend. This can be attributed to global events like wars, economic crises, and other factors that influenced participation. For instance, during the times of World War I and II, the Olympic Games of 1916, 1940, and 1944 were cancelled. Additionally, during the Cold War era, the Olympics of 1976, 1980, and 1984 faced boycotts from various countries.
                              <br><br>
                              These findings highlight the dynamic nature of athlete characteristics in the Olympic Games. The increasing gender parity and evolving physical attributes of athletes reflect the changing landscape of sports and the broader societal shifts towards inclusivity and improved physical fitness.")
                 )
               )
             )
    )
  )
)
  
  # define sever
  server <- function(input, output) {
    
    output$Olympic <- renderImage(
      {
        list(src = "pexels-pixabay-236937.jpg",
             width = "100%",
             height = 430
          
        )
      }
    )
    
    # background introduction
    output$background <- renderText({
      "Through studying the historical data of the Olympic Games from 1896 to 2016, 
      I've made many interesting discoveries. I will present these findings in the 
      following three pages. You can explore the insights you're interested in by 
      clicking on the respective interfaces. Additionally, you can use the sliders 
      to select and explore the aspects of the data that intrigue you the most."
    })
    
    # server logic for the first chart on the second page
    output$plot <- renderPlotly({
      
      # Ensure input$sport is a vector and does not contain any irregular values
      selected_sports <- if (is.vector(input$sport) && !is.null(input$sport) && length(input$sport) > 0) {
        input$sport
      } else {
        # Return a ggplot chart with title and axis labels but without data
        empty_plot <- ggplot() +
          labs(title = "Trends in the number of participants in sports", x = "Year", y = "Number of participants") +
          theme_minimal()
        return(ggplotly(empty_plot))
      }
      
      selected_data <- agg_data %>% filter(Sport %in% selected_sports)
      p <- ggplot(selected_data, aes(x = Year, y = peo_count, color = Sport, group = Sport)) +
        geom_line() +
        geom_point() +
        labs(title = "Trends in the number of participants in sports", x = "Year", y = "Number of participants") +
        theme_minimal()
      ggplotly(p, tooltip = c("Year", "number of athletes"))
    })
    
    
    
    # server logic for the second chart on the second page
    output$new_sports_plot <- renderPlotly({
      interactive_plot <- plot_ly(data = agg_data1, 
                                  x = ~factor(Year), 
                                  y = ~SportCount, 
                                  color = ~Season, 
                                  type = 'bar', 
                                  text = ~NewSportsText,
                                  hoverinfo = 'x+y+text',
                                  name = ~Season) %>%
        layout(title = "Trends in the Number of Sports by Year",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of Sports"))
      interactive_plot
    })
    
    # Create a reactive object to filter data based on the year selected by the user
    filtered_data <- reactive({
      olympic %>%
        filter(Year == input$year) %>%
        group_by(Country, Event, Medal) %>%  # Grouping by country, event and medal in order to count unique medals by each event
        summarise(
          Latitude = mean(Latitude, na.rm = TRUE),
          Longitude = mean(Longitude, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        group_by(Country) %>%
        summarise(
          medal_count = sum(!is.na(Medal)),
          gold_count = sum(Medal == "Gold", na.rm = TRUE),
          silver_count = sum(Medal == "Silver", na.rm = TRUE),
          bronze_count = sum(Medal == "Bronze", na.rm = TRUE),
          Latitude = mean(Latitude, na.rm = TRUE),
          Longitude = mean(Longitude, na.rm = TRUE)
        ) %>%
        filter(medal_count > 0)  # Only medal-winning countries will be retained
    })
    
    # Rendering Maps
    output$map <- renderLeaflet({
      medal_counts <- filtered_data()
      color_pal <- colorQuantile("OrRd", medal_counts$medal_count, n = 5)
      
      leaflet(data = medal_counts) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          color = ~color_pal(medal_count),
          radius = ~sqrt(medal_count) / max(sqrt(medal_count)) * 20,
          popup = ~paste("<strong>Country:</strong>", Country,
                         "<br><strong>Medal Count:</strong>", medal_count,
                         "<br><strong>Gold:</strong>", gold_count,
                         "<br><strong>Silver:</strong>", silver_count,
                         "<br><strong>Bronze:</strong>", bronze_count),
          group = "circles"
        ) %>%
        onRender("
      function(el) {
        var map = this;
        var markers = [];
        
        map.eachLayer(function(layer) {
          if (layer.options.group && layer.options.group === 'circles') {
            markers.push(layer);

            layer.on('click', function() {
              markers.forEach(function(marker) {
                if (marker !== layer) {
                  marker.setStyle({fillOpacity: 0.2});
                } else {
                  marker.setStyle({fillOpacity: 1});
                }
              });
            });
          }
        });
        
        map.on('click', function(e) {
          if (!e.layer) {
            markers.forEach(function(marker) {
              marker.setStyle({fillOpacity: 1});
            });
          }
        });
      }
    ")
    })
    
    
    
    
    
    # Height trend plot
    output$height_trend_plot <- renderPlotly({
      height_trend_plotly
    })
    
    # Weight trend plot
    output$weight_trend_plot <- renderPlotly({
      weight_trend_plotly
    })
    
    # Gender trend plot
    output$gender_trend_plot <- renderPlotly({
      gg2
    })
  }
  
  # run app
  shinyApp(ui = ui, server = server)
