## Packages
library(ggplot2)
library(dplyr)

## Data needed
plot_df = readRDS("plot_df.rds")
plot_df = plot_df %>%
  dplyr::filter(record_wet_year == year | record_dry_year == year)
mainland_df = readRDS("mainland_df.rds")
tas_df = readRDS("tas_df.rds")

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    plotOutput("distPlot"),
    sliderInput("year", "Year:",
                min = 1910, max = 2017, value = 1974)
  )
  
}

if (interactive()) {
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      ggplot() +
        # geom_point(data = plot_df,
        #            aes(x= x, y = y, group = year), size = 0.05, alpha = 0.0) +
        geom_path(data=mainland_df, aes(x = Long, y = Lat)) +
        geom_path(data=tas_df, aes(x = Long, y = Lat)) +
        geom_point(data = plot_df %>%
                     filter(record_wet_year == input$year),
                   aes(x= x, y =y, group = year), size = 0.75, col = "mediumblue") +
        geom_point(data = plot_df %>%
                     filter(record_dry_year == input$year),
                   aes(x= x, y =y, group = year), size = 0.75, col = "red") +
        coord_fixed() +
        xlab("Longitude") +
        ylab("Latitude") +
        ggtitle(input$year) +
        theme_bw()
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  
}