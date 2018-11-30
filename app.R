library(datasets)
library(shiny)
library(tidyverse)
library(shinydashboard)
data(faithful)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    if(input$density) {
      if(input$individual_obs) {
        ggplot(faithful, aes(x = eruptions)) +
          geom_histogram(bins = as.numeric(input$n_breaks),
                         aes(x = eruptions, stat(density))) +
          labs(
            title = "Geyser eruption duration",
            x = "Duration (minutes)"
          ) +
          geom_rug() +
          geom_density(adjust = input$bw_adjust, color = "blue")
      } else {
        ggplot(faithful, aes(x = eruptions)) +
          geom_histogram(bins = as.numeric(input$n_breaks),
                         aes(x = eruptions, stat(density))) +
          labs(
            title = "Geyser eruption duration",
            x = "Duration (minutes)"
          ) +
          geom_density(adjust = input$bw_adjust, color = "blue")
      }
    } else {
      if(input$individual_obs) {
        ggplot(faithful, aes(x = eruptions)) +
          geom_histogram(bins = as.numeric(input$n_breaks),
                         aes(x = eruptions, stat(density))) +
          labs(
            title = "Geyser eruption duration",
            x = "Duration (minutes)"
          ) +
          geom_rug()
      } else {
        ggplot(faithful, aes(x = eruptions)) +
          geom_histogram(bins = as.numeric(input$n_breaks),
                         aes(x = eruptions, stat(density))) +
          labs(
            title = "Geyser eruption duration",
            x = "Duration (minutes)"
          )
      }
    }
  })
  output$plot2 <- renderPlot({
    if(input$trend_line) {
      if(input$error_bars) {
        ggplot(faithful, aes(x = waiting, y = eruptions)) +
          geom_point(size = input$pt_size1) +
          labs(
            title = "Eruption Time versus Waiting Time",
            x = "Waiting Time (minutes)",
            y = "Eruption Time"
          ) +
          geom_smooth(method = "lm", se = TRUE)
      } else {
        ggplot(faithful, aes(x = waiting, y = eruptions)) +
          geom_point(size = input$pt_size1) +
          labs(
            title = "Eruption Time versus Waiting Time",
            x = "Waiting Time (minutes)",
            y = "Eruption Time"
          ) +
          geom_smooth(method = "lm", se = FALSE)
      }
    } else {
      ggplot(faithful, aes(x = waiting, y = eruptions)) +
        geom_point(size = input$pt_size1) +
        labs(
          title = "Eruption Time versus Waiting Time",
          x = "Waiting Time (minutes)",
          y = "Eruption Time"
        )
    }
  })
  output$plot3 <- renderPlot({
    if(input$points) {
      if(input$contour_lines) {
        ggplot(faithful, aes(x = waiting, y = eruptions)) +
          geom_point(size = input$pt_size2) +
          geom_density2d(h = c(input$bw_wait, input$bw_erupt)) +
          labs(
            title = "Eruption Time and Waiting Time",
            x = "Waiting Time (minutes)",
            y = "Eruption Time"
          )
      } else {
        ggplot(faithful, aes(x = waiting, y = eruptions)) +
          geom_point(size = input$pt_size2) +
          labs(
            title = "Eruption Time versus Waiting Time",
            x = "Waiting Time (minutes)",
            y = "Eruption Time"
          )
      }
    } else {
      if(input$contour_lines) {
        ggplot(faithful, aes(x = waiting, y = eruptions)) +
          geom_density2d(h = c(input$bw_wait, input$bw_erupt)) +
          labs(
            title = "Eruption Time and Waiting Time",
            x = "Waiting Time (minutes)",
            y = "Eruption Time"
          )
      } else {
        ggplot(faithful) + geom_point(aes(x = NA, y = NA))
      }
    }
  })
}
ui <- dashboardPage(
  dashboardHeader(title = "Old Faithful Eruptions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Part a", tabName = "a", icon = icon("th")),
      menuItem("Part b", tabName = "b", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "a",
        fluidRow(
          box(
            selectInput(inputId = "n_breaks",
                        label = "Number of bins in histogram (approximate):",
                        choices = c(10, 20, 35, 50),
                        selected = 20),
            checkboxInput(inputId = "individual_obs",
                          label = strong("Show individual observations"),
                          value = FALSE),
            checkboxInput(inputId = "density",
                          label = strong("Show density estimate"),
                          value = FALSE),
            plotOutput(outputId = "plot1", height = "300px"),
            
            # Display this only if the density is shown
            conditionalPanel(condition = "input.density == true",
                             sliderInput(inputId = "bw_adjust",
                                         label = "Bandwidth adjustment:",
                                         min = 0.2, max = 2, value = 1, step = 0.2))
          )
      )
    ),
    tabItem(tabName = "b",
            fluidRow(
              box(
                checkboxInput(inputId = "trend_line",
                                        label = strong("Include trend line"),
                                        value = FALSE),
                sliderInput(inputId = "pt_size1",
                                      label = "Point size",
                                      min = 0.2, max = 2, value = 1, step = 0.2),
                plotOutput(outputId = "plot2", height = "300px"),
                conditionalPanel(condition = "input.trend_line == true",
                                           checkboxInput(inputId = "error_bars",
                                                         label = strong("Include error bars"),
                                                         value = FALSE)
                          )
                        )
                      ),
              fluidRow(
                  box(
                          checkboxInput(inputId = "contour_lines",
                                        label = strong("Include contour lines"),
                                        value = TRUE),
                          checkboxInput(inputId = "points",
                                        label = strong("Include points"),
                                        value = TRUE),
                          plotOutput(outputId = "plot3", height = "300px"),
                          conditionalPanel(condition = "input.contour_lines == true",
                                           sliderInput(inputId = "bw_wait",
                                                       label = strong("Bandwidth for Waiting Time"),
                                                       min = 0.2, max = 3, value = 1.5, step = 0.2)
                          ),
                          conditionalPanel(condition = "input.contour_lines == true",
                                           sliderInput(inputId = "bw_erupt",
                                                       label = strong("Bandwidth for Eruption Time"),
                                                       min = 0.2, max = 3, value = 1.5, step = 0.2)
                          ),
                          conditionalPanel(condition = "input.points == true",
                                           sliderInput(inputId = "pt_size2",
                                                       label = strong("Point size"),
                                                       min = 0.2, max = 2, value = 1, step = 0.2)
                          )
                        )
                      )
             )
    )
  )
)
shinyApp(ui = ui, server = server)
