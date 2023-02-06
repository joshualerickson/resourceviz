#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ksanka Ranger District Gaging Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            pickerInput("site","Pick a Stream",choices = sort(unique(final_data$Stream))),
            pickerInput('year',
                        choices = sort(unique(final_data$year)),
                         multiple = TRUE,
                         'Select a year',
                        selected = max(final_data$year))
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotly::plotlyOutput("sitePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sitePlot <- plotly::renderPlotly({

      dh <- final_data %>%
        filter(Stream %in% input$site) %>%
        filter(year %in% input$year, type %in% c("Daily Q", "Hourly Q")) %>%
        filter(!is.na(q_pred))

      obs <- final_data %>% filter(Stream %in% input$site) %>%
        filter(year %in% input$year, type %in% c("Observed Q")) %>%
        filter(!is.na(Q))

print(obs)

       if(nrow(dh)<1){
              plotly::ggplotly(
         obs %>%
           ggplot(aes(dt, Q))+
           geom_point(aes(shape = 'Observed Q'),
                      color = 'blue',
                      size = 2.5) +
           geom_line(alpha = 0.75, size = 1) +
           scale_shape_manual(values = 8) +
           labs(y = 'Q ft^3 /s', x = 'Date', shape = '', linetype = '',
                title = paste0("Gaging Station: ", obs[1,]$Stream),
                subtitle = paste0('Years: ', min(obs$year), ':', max(obs$year)))  +
           guides(color = 'none', linetype = guide_legend(override.aes = list(color = c('red', 'black')))) +
           custom_theme())

       } else {
         plotly::ggplotly(
          dh %>%
          ggplot(aes(dt, q_pred))+
          geom_line(aes(color = type, linetype = type, alpha = type)) +
          scale_linetype_manual(values = c(1,3),
                                labels = c('Daily Q', 'Hourly Q')) +
          scale_alpha_manual(values = c(1, .75), guide = 'none') + scale_color_manual(values = c('red', 'black')) +
          geom_point(data = obs,
                     aes(dt, y = Q, shape = 'Observed Q'),
                     color = 'blue',
                     size = 2.5) +
            geom_line(data = obs,
                      alpha = 0.75,
                       aes(dt, y = Q, shape = 'Observed Q'),
                       color = 'black',
                       size = 1) +
          scale_shape_manual(values = 8) +
          labs(y = 'Q ft^3 /s', x = 'Date', shape = '', linetype = '',
               title = paste0("Gaging Station: ", obs[1,]$Stream),
               subtitle = paste0('Years: ', min(obs$year), ':', max(obs$year)))  +
          guides(color = 'none', linetype = guide_legend(override.aes = list(color = c('red', 'black')))) +
          custom_theme()
      )
      }

    })
}

# Run the application
shinyApp(ui = ui, server = server)
