# load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(countrycode)

# read in data
fifa <- read_csv(paste("https://storage.googleapis.com/kaggle-datasets/19728/29747/WorldCupMatches.csv",
                       "?GoogleAccessId=web-data@kaggle-161607.iam.gserviceaccount.com&Expires=15439484",
                       "40&Signature=fQT7U3x7xLbn0G9v141DO5yNFmNloixUgZigiYcqu86PGZGwb192DB71gfa3x6LrxB",
                       "UatqOsq%2Bz33orBvw3pNsWLfqL%2Bxg37gc5Sbl4%2BD%2BH0KMO8xn59XgSufLLxQ8zkmQ0d7z2KMM",
                       "zZlu6dgHbAlmEWw7DyHaxVYrH6kPxTiLDJNCbYtgIJ8XXpUFCYM2o8ZRQnLvgcdNhY8CiubtE1ISoYKmS",
                       "e2Ia6Uomc20IVpf2SD4tloYDrW7TLg6bR%2F7IcCvY3pYLT2Pl%2FfBjX03JhoODntvkSe3eELAkGL8BKF",
                       "J6Me74ytJ4fwmPYkSUjoW5lBzBT2fyodljq7f4a9TIWUA%3D%3D", sep = ""))

# project theme
project2_theme <- theme_bw() +
  theme(axis.text = element_text(size = 14, color = "black"),
        rect = element_rect(color = "black"),
        text = element_text(size = 14, color = "black"),
        panel.background = element_rect(fill = "white"))

# preprocessing
fifa$Stage <- ifelse(grepl("Group", fifa$Stage), "Group Stage", fifa$Stage)
fifa$Stage <- ifelse(fifa$Stage == "First round", "Group Stage", fifa$Stage)
fifa$Stage <- ifelse(grepl("place", fifa$Stage), "Third Place", fifa$Stage)
fifa <- fifa[fifa$Stage != "Preliminary round",]
fifa <- fifa[!is.na(fifa$Stage),]
fifa <- fifa[!is.na(fifa$`Home Team Name`),]
fifa <- fifa[!is.na(fifa$`Away Team Name`),]
fifa$`Home Team Name` <- ifelse(grepl("Arab Emirates", fifa$`Home Team Name`),
                                "United Arab Emirates", fifa$`Home Team Name`)
fifa$`Home Team Name` <- ifelse(grepl("of Ireland", fifa$`Home Team Name`),
                                "Republic of Ireland", fifa$`Home Team Name`)
fifa$`Home Team Name` <- ifelse(grepl("Tobago", fifa$`Home Team Name`),
                                "Trinidad and Tobago", fifa$`Home Team Name`)
fifa$`Home Team Name` <- ifelse(grepl("and Montenegro", fifa$`Home Team Name`),
                                "Serbia and Montenegro", fifa$`Home Team Name`)
fifa$`Home Team Name` <- ifelse(grepl("d'Ivoire", fifa$`Home Team Name`),
                                "Cote d'Ivoire", fifa$`Home Team Name`)
fifa$`Home Team Name` <- ifelse(grepl("Herzegovina", fifa$`Home Team Name`),
                                "Bosnia and Herzegovina", fifa$`Home Team Name`)
fifa$`Away Team Name` <- ifelse(grepl("Arab Emirates", fifa$`Away Team Name`),
                                "United Arab Emirates", fifa$`Away Team Name`)
fifa$`Away Team Name` <- ifelse(grepl("of Ireland", fifa$`Away Team Name`),
                                "Republic of Ireland", fifa$`Away Team Name`)
fifa$`Away Team Name` <- ifelse(grepl("Tobago", fifa$`Away Team Name`),
                                "Trinidad and Tobago", fifa$`Away Team Name`)
fifa$`Away Team Name` <- ifelse(grepl("and Montenegro", fifa$`Away Team Name`),
                                "Serbia and Montenegro", fifa$`Away Team Name`)
fifa$`Away Team Name` <- ifelse(grepl("d'Ivoire", fifa$`Away Team Name`),
                                "Cote d'Ivoire", fifa$`Away Team Name`)
fifa$`Away Team Name` <- ifelse(grepl("Herzegovina", fifa$`Away Team Name`),
                                "Bosnia and Herzegovina", fifa$`Away Team Name`)
goals <- data.frame(first = c(fifa$`Half-time Home Goals`, fifa$`Half-time Away Goals`),
                    second = c(fifa$`Home Team Goals` - fifa$`Half-time Home Goals`,
                               fifa$`Away Team Goals` - fifa$`Half-time Away Goals`),
                    total = c(fifa$`Home Team Goals`, fifa$`Away Team Goals`),
                    homeGoals = c(fifa$`Home Team Goals`),
                    awayGoals = c(fifa$`Away Team Goals`),
                    homeCode = countrycode(fifa$`Home Team Name`, 'country.name', 'iso3c'),
                    awayCode = countrycode(fifa$`Away Team Name`, 'country.name', 'iso3c'),
                    round = c(fifa$Stage, fifa$Stage),
                    team = c(fifa$`Home Team Name`, fifa$`Away Team Name`),
                    year = c(fifa$Year, fifa$Year))

goals$continent <- ifelse(goals$team %in% c("France", "Yugoslavia", "Romania",
                                            "Austria", "Hungary", "Switzerland",
                                            "Sweden", "Germany", "Spain",
                                            "Italy", "Czechoslovakia", "England",
                                            "Germany FR", "Turkey", "Soviet Union",
                                            "Northern Ireland", "Wales", "Portugal",
                                            "Belgium", "Bulgaria", "German DR", "Poland",
                                            "Scotland", "Netherlands", "Denmark",
                                            "Republic of Ireland", "Norway", "Russia",
                                            "Greece", "Croatia", "Slovenia",
                                            "Serbia and Montenegro", "Czech Republic", "Ukraine",
                                            "Serbia", "Slovakia", "Bosnia and Herzegovina",
                                            "Israel"),
                          "UEFA",
                          ifelse(goals$team %in% c("Korea DPR", "Australia", "Korea Republic",
                                                   "Iraq", "United Arab Emirates", "Saudi Arabia",
                                                   "Japan", "China PR", "Iran",
                                                   "IR Iran", "Kuwait"),
                                 "AFC",
                                 ifelse(goals$team %in% c("USA", "Cuba", "Mexico",
                                                          "Haiti", "Honduras", "Canada",
                                                          "Costa Rica", "Jamaica", "Trinidad and Tobago",
                                                          "El Salvador"),
                                        "CONCACAF",
                                        ifelse(goals$team %in% c("Zaire", "Tunisia", "Algeria",
                                                                 "Morocco", "Cameroon", "Nigeria",
                                                                 "South Africa", "Senegal", "Angola",
                                                                 "Togo", "Cote d'Ivoire", "Ghana",
                                                                 "Egypt"),
                                               "CAF",
                                               ifelse(goals$team %in% c("Argentina", "Chile", "Uruguay",
                                                                        "Brazil", "Paraguay", "Peru",
                                                                        "Colombia", "Bolivia", "Ecuador"),
                                                      "CONMEBOL",
                                                      "OFC")))))


server <- function(input, output) {
  output$map_goals_scored <- renderPlotly({
    goals_sub <- goals
    if(!input$scored_group) {
      goals_sub <- goals_sub[goals_sub$round != "Group Stage",]
    }
    if(!input$scored_ro16) {
      goals_sub <- goals_sub[goals_sub$round != "Round of 16",]
    }
    if(!input$scored_quarter) {
      goals_sub <- goals_sub[goals_sub$round != "Quarter-finals",]
    }
    if(!input$scored_semi) {
      goals_sub <- goals_sub[goals_sub$round != "Semi-finals",]
    }
    if(!input$scored_third) {
      goals_sub <- goals_sub[goals_sub$round != "Third Place",]
    }
    if(!input$scored_final) {
      goals_sub <- goals_sub[goals_sub$round != "Final",]
    }
    
    homeCountries <- as.character(unique(goals_sub$homeCode))
    meanGoals <- c()
    for (country in homeCountries) {
      countryGoals <- goals_sub %>% filter(homeCode == country)
      meanGoals <- c(meanGoals, mean(countryGoals$homeGoals))
    }
    goalsScored <- data.frame(code = homeCountries,
                              goal = meanGoals)
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), 
              width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    l2 <- list(font = list(size = 4))
    
    plot_geo(goalsScored) %>%
      add_trace(
        z = ~goal, color = ~goal,
        text = ~code, locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Goals', thickness = 10) %>%
      layout(
        title = 'Mean Number of Goals Scored per Country',
        geo = g,
        legend = l2
      )
  })
  output$map_goals_conceded <- renderPlotly({
    goals_sub <- goals
    if(!input$conceded_group) {
      goals_sub <- goals_sub[goals_sub$round != "Group Stage",]
    }
    if(!input$conceded_ro16) {
      goals_sub <- goals_sub[goals_sub$round != "Round of 16",]
    }
    if(!input$conceded_quarter) {
      goals_sub <- goals_sub[goals_sub$round != "Quarter-finals",]
    }
    if(!input$conceded_semi) {
      goals_sub <- goals_sub[goals_sub$round != "Semi-finals",]
    }
    if(!input$conceded_third) {
      goals_sub <- goals_sub[goals_sub$round != "Third Place",]
    }
    if(!input$conceded_final) {
      goals_sub <- goals_sub[goals_sub$round != "Final",]
    }
    meanGoals <- c()
    for (country in homeCountries) {
      countryGoals <- goals %>% filter(homeCode == country)
      meanGoals <- c(meanGoals, mean(countryGoals$awayGoals))
    }
    goalsConceded <- data.frame(code = homeCountries,
                                goal = meanGoals)
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), 
              width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    l2 <- list(font = list(size = 4))
    
    plot_geo(goalsConceded) %>%
      add_trace(
        z = ~goal, color = ~goal,
        text = ~code, locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Goals', thickness = 10) %>%
      layout(
        title = 'Mean Number of Goals Conceded per Country',
        geo = g,
        legend = l2
      )
  })
  output$time_series_attendance <- renderPlot({
    # code for time series of attendance
  })
  output$network_teams_matches <- renderPlot({
    # code for network plot with teams as nodes and matches as edges
  })
  output$first_second_scatterplot <- renderPlot({
    goals_sub <- goals
    if(!input$scatter_group) {
      goals_sub <- goals_sub[goals_sub$round != "Group Stage",]
    }
    if(!input$scatter_ro16) {
      goals_sub <- goals_sub[goals_sub$round != "Round of 16",]
    }
    if(!input$scatter_quarter) {
      goals_sub <- goals_sub[goals_sub$round != "Quarter-finals",]
    }
    if(!input$scatter_semi) {
      goals_sub <- goals_sub[goals_sub$round != "Semi-finals",]
    }
    if(!input$scatter_third) {
      goals_sub <- goals_sub[goals_sub$round != "Third Place",]
    }
    if(!input$scatter_final) {
      goals_sub <- goals_sub[goals_sub$round != "Final",]
    }
    if(input$line) {
      if(input$error_bars) {
        ggplot(data = goals_sub, aes(x = jitter(first),
                                     y = jitter(second),
                                     color = round)) +
          geom_point(alpha = 0.2) +
          geom_smooth(method = "lm", se = TRUE) +
          labs(
            title = "Second Half versus First Half Goals",
            x = "First Half Goals",
            y = "Second Half Goals",
            color = "Tournament Round"
          ) +
          project2_theme
      } else {
        ggplot(data = goals_sub, aes(x = jitter(first),
                                     y = jitter(second),
                                     color = round)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          labs(
            title = "Second Half versus First Half Goals",
            x = "First Half Goals",
            y = "Second Half Goals",
            color = "Tournament Round"
          ) +
          project2_theme
      }
    } else {
      ggplot(data = goals_sub, aes(x = jitter(first),
                                   y = jitter(second),
                                   color = round)) +
        geom_point() +
        labs(
          title = "Second Half versus First Half Goals",
          x = "First Half Goals",
          y = "Second Half Goals",
          color = "Tournament Round"
        ) +
        project2_theme
    }
  })
  output$cor_mat_attendance_goals <- renderPlot({
    # code for correlation matrix of attendance and total match goals
  })
  output$hist_total_goals <- renderPlot({
    # code for histogram of total match goals
  })
  output$bar_appearances <- renderPlot({
    goals_sub <- goals
    if(!input$bar_group) {
      goals_sub <- goals_sub[goals_sub$round != "Group Stage",]
    }
    if(!input$bar_ro16) {
      goals_sub <- goals_sub[goals_sub$round != "Round of 16",]
    }
    if(!input$bar_quarter) {
      goals_sub <- goals_sub[goals_sub$round != "Quarter-finals",]
    }
    if(!input$bar_semi) {
      goals_sub <- goals_sub[goals_sub$round != "Semi-finals",]
    }
    if(!input$bar_third) {
      goals_sub <- goals_sub[goals_sub$round != "Third Place",]
    }
    if(!input$bar_final) {
      goals_sub <- goals_sub[goals_sub$round != "Final",]
    }
    ggplot(data = goals_sub, aes(x = continent, fill = round)) +
      geom_bar() +
      labs(
        title = "Tournament Appearances",
        x = "Continent",
        y = "Matches Played",
        fill = "Tournament Round"
      ) +
      project2_theme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "FIFA World Cup Matches"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Goals by Country", tabName = "goal_map", icon = icon("futbol")),
      menuItem("Attendance", tabName = "attend", icon = icon("futbol")),
      menuItem("Matches", tabName = "matches", icon = icon("futbol")),
      menuItem("Goals", tabName = "goals", icon = icon("futbol")),
      menuItem("Tournament Record", tabName = "tournament", icon = icon("futbol"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "goal_map", tabsetPanel(type = "tabs", tabPanel(title = "Goals Scored",
                                                                  fluidRow(
                                                                    box(plotlyOutput(outputId = "map_goals_scored")),
                                                                    box(
                                                                      checkboxInput(inputId = "scored_group",
                                                                                    label = "Group Stage",
                                                                                    value = TRUE),
                                                                      checkboxInput(inputId = "scored_ro16",
                                                                                    label = "Round of 16",
                                                                                    value = TRUE),
                                                                      checkboxInput(inputId = "scored_quarter",
                                                                                    label = "Quarter-finals",
                                                                                    value = TRUE),
                                                                      checkboxInput(inputId = "scored_semi",
                                                                                    label = "Semi-finals",
                                                                                    value = TRUE),
                                                                      checkboxInput(inputId = "scored_third",
                                                                                    label = "Third Place",
                                                                                    value = TRUE),
                                                                      checkboxInput(inputId = "scored_final",
                                                                                    label = "Final",
                                                                                    value = TRUE))
                                                                  
                                                                  )
        ),
        tabPanel(title = "Goals Conceded",
                 fluidRow(
                   box(plotlyOutput(outputId = "map_goals_conceded")),
                   box(
                     checkboxInput(inputId = "conceded_group",
                                   label = "Group Stage",
                                   value = TRUE),
                     checkboxInput(inputId = "conceded_ro16",
                                   label = "Round of 16",
                                   value = TRUE),
                     checkboxInput(inputId = "conceded_quarter",
                                   label = "Quarter-finals",
                                   value = TRUE),
                     checkboxInput(inputId = "conceded_semi",
                                   label = "Semi-finals",
                                   value = TRUE),
                     checkboxInput(inputId = "conceded_third",
                                   label = "Third Place",
                                   value = TRUE),
                     checkboxInput(inputId = "conceded_final",
                                   label = "Final",
                                   value = TRUE))
                 )))),
      tabItem(
        tabName = "attend", tabsetPanel(type = "tabs", tabPanel(title = "Attendance over Time",
                                                                fluidRow(
                                                                  # code for attendance over time
                                                                )
        ),
        tabPanel(title = "Attendance and Goals",
                 fluidRow(
                   # code for correlation matrix of attendance and total match goals
                 )))),
      tabItem(
        tabName = "matches",
        fluidRow(
          # code for network plot with nodes as teams and edges as matches
        )
      ),
      tabItem(
        tabName = "goals",
        tabsetPanel(type = "tabs", tabPanel(title = "First versus Second Half",
                                            fluidRow(
                                              box(plotOutput(outputId = "first_second_scatterplot", height = "500px")),
                                              box(
                                                checkboxInput(inputId = "scatter_group",
                                                              label = "Group Stage",
                                                              value = TRUE),
                                                checkboxInput(inputId = "scatter_ro16",
                                                              label = "Round of 16",
                                                              value = TRUE),
                                                checkboxInput(inputId = "scatter_quarter",
                                                              label = "Quarter-finals",
                                                              value = TRUE),
                                                checkboxInput(inputId = "scatter_semi",
                                                              label = "Semi-finals",
                                                              value = TRUE),
                                                checkboxInput(inputId = "scatter_third",
                                                              label = "Third Place",
                                                              value = TRUE),
                                                checkboxInput(inputId = "scatter_final",
                                                              label = "Final",
                                                              value = TRUE),
                                                checkboxInput(inputId = "line",
                                                              label = "Include trend lines",
                                                              value = FALSE),
                                                conditionalPanel(condition = "input.line == true",
                                                                 checkboxInput(inputId = "error_bars",
                                                                               label = "Include error bars",
                                                                               value = FALSE))
                                              )
                                            )),
                    tabPanel(title = "Distribution")) # add code for histogram of goals
      ),
      tabItem(
        tabName = "tournament",
        fluidRow(
          box(plotOutput(outputId = "bar_appearances", height = "600px")),
          box(
            checkboxInput(inputId = "bar_group",
                          label = "Group Stage",
                          value = TRUE),
            checkboxInput(inputId = "bar_ro16",
                          label = "Round of 16",
                          value = TRUE),
            checkboxInput(inputId = "bar_quarter",
                          label = "Quarter-finals",
                          value = TRUE),
            checkboxInput(inputId = "bar_semi",
                          label = "Semi-finals",
                          value = TRUE),
            checkboxInput(inputId = "bar_third",
                          label = "Third Place",
                          value = TRUE),
            checkboxInput(inputId = "bar_final",
                          label = "Final",
                          value = TRUE)
          )
        )
      )
    )
  )
)

shinyApp(ui = ui, server = server)
