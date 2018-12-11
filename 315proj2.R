# load libraries
library(shiny)
library(tidyverse)
library(shinydashboard)
library(countrycode)
library(plotly)
library(dygraphs)
library(reshape2)

# read in data
fifa <- read_csv("https://raw.githubusercontent.com/rcserran/315project2/master/WorldCupMatches.csv")

# project theme
project2_theme <- theme_bw() +
  theme(axis.text = element_text(size = 14, color = "black"),
        rect = element_rect(color = "black"),
        text = element_text(size = 14, color = "black"),
        panel.background = element_rect(fill = "white"))
col.pal <- c("blue", "plum", "yellow", "red", "orange", "gold")

# preprocessing
fifa$Stage <- ifelse(grepl("Group", fifa$Stage) | grepl("Preliminary", fifa$Stage),
                     "Group Stage", fifa$Stage)
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
    goalsScored$code <- as.character(goalsScored$code)
    isos = codelist$iso3c
    isos = isos[!is.na(isos)]
    for (a in isos) {
      if (!(a %in% goalsScored$code)) {
        goalsScored = rbind(goalsScored, c(a, NaN))
      }
    }
    
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
    
    plot_geo(goalsScored, zmin = 0, zmax = 5) %>%
      add_trace(
        z = ~goal, color = ~goal,
        text = ~code, locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Goals', thickness = 10) %>%
      layout(
        title = 'Mean Number of Goals Scored by Country',
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
    homeCountries <- as.character(unique(goals_sub$homeCode))
    meanGoals <- c()
    for (country in homeCountries) {
      countryGoals <- goals %>% filter(homeCode == country)
      meanGoals <- c(meanGoals, mean(countryGoals$awayGoals))
    }
    goalsConceded <- data.frame(code = homeCountries,
                                goal = meanGoals)
    goalsConceded$code <- as.character(goalsConceded$code)
    
    isos = codelist$iso3c
    isos = isos[!is.na(isos)]
    for (a in isos) {
      if (!(a %in% goalsConceded$code)) {
        goalsConceded = rbind(goalsConceded, c(a, NaN))
      }
    }
    
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
    
    plot_geo(goalsConceded, zmin = 0, zmax = 7) %>%
      add_trace(
        z = ~goal, color = ~goal,
        text = ~code, locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Goals', thickness = 10) %>%
      layout(
        title = 'Mean Number of Goals Against by Country',
        geo = g,
        legend = l2
      )
  })
  output$time_series_attendance <- renderDygraph({
    # code for time series of attendance
    attendance <- fifa[!is.na(fifa$Attendance),
                       c("Year", "Stage", "Attendance")]
    attndn_sub <- aggregate(attendance$Attendance,
                            by = list(Stage = attendance$Stage,
                                      Year = attendance$Year),
                            FUN = mean)
    if(!input$attndn_group) {
      attndn_sub <- attndn_sub[attndn_sub$Stage != "Group Stage",]
    }
    if(!input$attndn_ro16) {
      attndn_sub <- attndn_sub[attndn_sub$Stage != "Round of 16",]
    }
    if(!input$attndn_quarter) {
      attndn_sub <- attndn_sub[attndn_sub$Stage != "Quarter-finals",]
    }
    if(!input$attndn_semi) {
      attndn_sub <- attndn_sub[attndn_sub$Stage != "Semi-finals",]
    }
    if(!input$attndn_third) {
      attndn_sub <- attndn_sub[attndn_sub$Stage != "Third Place",]
    }
    if(!input$attndn_final) {
      attndn_sub <- attndn_sub[attndn_sub$Stage != "Final",]
    }
    attndn_sub <- aggregate(attndn_sub$x,
                            by = list(Year = attndn_sub$Year),
                            FUN = mean)
    dygraph(attndn_sub, main = "Average World Cup Game Attendance")
  })
  output$winning_margin_round <- renderPlot({
    fifa$margin <- abs(fifa$`Home Team Goals` - fifa$`Away Team Goals`)
    fifa_sub <- fifa[fifa$Stage != "Preiliminary round",]
    if("wc1930" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1930,]
    }
    if("wc1934" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1934,]
    }
    if("wc1938" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1938,]
    }
    if("wc1950" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1950,]
    }
    if("wc1954" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1954,]
    }
    if("wc1958" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1958,]
    }
    if("wc1962" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1962,]
    }
    if("wc1966" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1966,]
    }
    if("wc1970" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1970,]
    }
    if("wc1974" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1974,]
    }
    if("wc1978" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1978,]
    }
    if("wc1982" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1982,]
    }
    if("wc1986" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1986,]
    }
    if("wc1990" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1990,] 
    }
    if("wc1994" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1994,]
    }
    if("wc1998" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 1998,]
    }
    if("wc2002" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 2002,] 
    }
    if("wc2006" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 2006,]
    }
    if("wc2010" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 2010,]
    }
    if("wc2014" %in% input$years) {
      fifa_sub <- fifa[fifa$Year != 2014,]
    }
    ggplot(data = fifa_sub, aes(x = Stage, y = margin,
                                fill = Stage)) +
      geom_boxplot() +
      labs(
        title = "Winning Margin by Round",
        x = "Round",
        y = "Winning Margin",
        fill = "Round"
      ) +
      project2_theme +
      scale_fill_manual(values = c("Group Stage" = col.pal[1],
                                   "Round of 16" = col.pal[2],
                                   "Quarter-finals" = col.pal[3],
                                   "Semi-finals" = col.pal[4],
                                   "Third Place" = col.pal[5],
                                   "Final" = col.pal[6])) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
          project2_theme +
          scale_color_manual(values = c("Group Stage" = col.pal[1],
                                        "Round of 16" = col.pal[2],
                                        "Quarter-finals" = col.pal[3],
                                        "Semi-finals" = col.pal[4],
                                        "Third Place" = col.pal[5],
                                        "Final" = col.pal[6]))
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
          project2_theme +
          scale_color_manual(values = c("Group Stage" = col.pal[1],
                                        "Round of 16" = col.pal[2],
                                        "Quarter-finals" = col.pal[3],
                                        "Semi-finals" = col.pal[4],
                                        "Third Place" = col.pal[5],
                                        "Final" = col.pal[6]))
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
        project2_theme +
        scale_color_manual(values = c("Group Stage" = col.pal[1],
                                      "Round of 16" = col.pal[2],
                                      "Quarter-finals" = col.pal[3],
                                      "Semi-finals" = col.pal[4],
                                      "Third Place" = col.pal[5],
                                      "Final" = col.pal[6]))
    }
  })
  output$cor_mat_attendance_goals <- renderPlot({
    # code for correlation matrix of attendance and total match goals
    total <- fifa$`Home Team Goals` + fifa$`Away Team Goals`
    fifa_t <- cbind(total, fifa)
    is.na(fifa_t) <- sapply(fifa_t, is.infinite)
    fifa_t[is.na(fifa_t)] <- 0
    corr_sub <- fifa_t[,c("Attendance", "total", "Home Team Goals", "Away Team Goals")]
    reorder_cormat <- function(cormat) {
      # Use correlation between variables as distance
      dd <- as.dist((1-cormat)/2)
      hc <- hclust(dd)
      cormat <- cormat[hc$order, hc$order]
    }    
    
    if(!input$corr_0) {
      corr_sub <- corr_sub[corr_sub$total != 0 & corr_sub$total != 1,]
    }
    if(!input$corr_2) {
      corr_sub <- corr_sub[corr_sub$total != 3 & corr_sub$total != 4,]
    }
    if(!input$corr_4) {
      corr_sub <- corr_sub[corr_sub$total != 5 & corr_sub$total != 6,]
    }
    if(!input$corr_6) {
      corr_sub <- corr_sub[corr_sub$total != 7 & corr_sub$total != 8,]
    }
    if(!input$corr_8) {
      corr_sub <- corr_sub[corr_sub$total != 9 & corr_sub$total != 10,]
    }
    if(!input$corr_10) {
      corr_sub <- corr_sub[corr_sub$total != 11 & corr_sub$total != 12,]
    }
    cormat <- cor(corr_sub)
    #cormat <- reorder_cormat(cormat)
    cormat[upper.tri(cormat)] <- NA
    melted_cormat <- melt(cormat, na.rm = TRUE)
    melted_cormat$value <- round(melted_cormat$value, 2)
    
    ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
      geom_tile() + 
      labs(
        title = "Correlations",
        x = "",
        y = "",
        fill = "Correlation"
      ) +
      geom_text(aes(label = value), color = "black", fontface = "bold") +
      project2_theme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_gradient2(low = "orange", mid = "ivory", high = "blue",
                           midpoint = 0, limit = c(-1,1))
  })
  output$hist_total_goals <- renderPlot({
    goals_sub <- goals
    if(!input$hist_group) {
      goals_sub <- goals_sub[goals_sub$round != "Group Stage",]
    }
    if(!input$hist_ro16) {
      goals_sub <- goals_sub[goals_sub$round != "Round of 16",]
    }
    if(!input$hist_quarter) {
      goals_sub <- goals_sub[goals_sub$round != "Quarter-finals",]
    }
    if(!input$hist_semi) {
      goals_sub <- goals_sub[goals_sub$round != "Semi-finals",]
    }
    if(!input$hist_third) {
      goals_sub <- goals_sub[goals_sub$round != "Third Place",]
    }
    if(!input$hist_final) {
      goals_sub <- goals_sub[goals_sub$round != "Final",]
    }
    ggplot(goals_sub, aes(x = total, fill = round)) +
      geom_histogram(binwidth = 1) +
      labs(title = "Total Goals in Match",
           x = "total goals",
           y = "frequency",
           fill = "round") +
      project2_theme +
      scale_fill_manual(values = c("Group Stage" = col.pal[1],
                                   "Round of 16" = col.pal[2],
                                   "Quarter-finals" = col.pal[3],
                                   "Semi-finals" = col.pal[4],
                                   "Third Place" = col.pal[5],
                                   "Final" = col.pal[6]))
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
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = c("Group Stage" = col.pal[1],
                                   "Round of 16" = col.pal[2],
                                   "Quarter-finals" = col.pal[3],
                                   "Semi-finals" = col.pal[4],
                                   "Third Place" = col.pal[5],
                                   "Final" = col.pal[6]))
  })
}

ui <- dashboardPage(
  skin = "green",
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
        tabPanel(title = "Goals Against",
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
        tabName = "attend", tabsetPanel(type = "tabs",
                                        tabPanel(title = "Attendance over Time",
                                                 fluidRow(
                                                   box(dygraphOutput(outputId = "time_series_attendance")),
                                                   box(
                                                     checkboxInput(inputId = "attndn_group",
                                                                   label = "Group Stage",
                                                                   value = TRUE),
                                                     checkboxInput(inputId = "attndn_ro16",
                                                                   label = "Round of 16",
                                                                   value = TRUE),
                                                     checkboxInput(inputId = "attndn_quarter",
                                                                   label = "Quarter-finals",
                                                                   value = TRUE),
                                                     checkboxInput(inputId = "attndn_semi",
                                                                   label = "Semi-finals",
                                                                   value = TRUE),
                                                     checkboxInput(inputId = "attndn_third",
                                                                   label = "Third Place",
                                                                   value = TRUE),
                                                     checkboxInput(inputId = "attndn_final",
                                                                   label = "Final",
                                                                   value = TRUE))
                                                 )
                                        ),
                                        tabPanel(title = "Attendance and Goals",
                                                 fluidRow(box(plotOutput(outputId = "cor_mat_attendance_goals")),
                                                          box(
                                                            checkboxInput(inputId = "corr_0",
                                                                          label = "0-2 Goals",
                                                                          value = TRUE),
                                                            checkboxInput(inputId = "corr_2",
                                                                          label = "3-4 Goals",
                                                                          value = TRUE),
                                                            checkboxInput(inputId = "corr_4",
                                                                          label = "5-6 Goals",
                                                                          value = TRUE),
                                                            checkboxInput(inputId = "corr_6",
                                                                          label = "7-8 Goals",
                                                                          value = TRUE),
                                                            checkboxInput(inputId = "corr_8",
                                                                          label = "9-10 Goals",
                                                                          value = TRUE),
                                                            checkboxInput(inputId = "corr_10",
                                                                          label = "11-12 Goals",
                                                                          value = TRUE))
                                                 )))),
      tabItem(
        tabName = "matches",
        fluidRow(
          box(plotOutput(outputId = "winning_margin_round", height = "500px")),
          box(
            selectInput("years", "Select years",
                        c("1930" = "wc1930",
                          "1934" = "wc1934",
                          "1938" = "wc1938",
                          "1950" = "wc1950",
                          "1954" = "wc1954",
                          "1958" = "wc1958",
                          "1962" = "wc1962",
                          "1966" = "wc1966",
                          "1970" = "wc1970",
                          "1974" = "wc1974",
                          "1978" = "wc1978",
                          "1982" = "wc1982",
                          "1986" = "wc1986",
                          "1990" = "wc1990",
                          "1994" = "wc1994",
                          "1998" = "wc1998",
                          "2002" = "wc2002",
                          "2006" = "wc2006",
                          "2010" = "wc2010",
                          "2014" = "wc2014"),
                        multiple = TRUE)
          )
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
                    tabPanel(title = "Distribution",
                             fluidRow(box(plotOutput(outputId = "hist_total_goals")),
                                      box(
                                        checkboxInput(inputId = "hist_group",
                                                      label = "Group Stage",
                                                      value = TRUE),
                                        checkboxInput(inputId = "hist_ro16",
                                                      label = "Round of 16",
                                                      value = TRUE),
                                        checkboxInput(inputId = "hist_quarter",
                                                      label = "Quarter-finals",
                                                      value = TRUE),
                                        checkboxInput(inputId = "hist_semi",
                                                      label = "Semi-finals",
                                                      value = TRUE),
                                        checkboxInput(inputId = "hist_third",
                                                      label = "Third Place",
                                                      value = TRUE),
                                        checkboxInput(inputId = "hist_final",
                                                      label = "Final",
                                                      value = TRUE))
                             )
                    )
        ) # add code for histogram of goals
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
