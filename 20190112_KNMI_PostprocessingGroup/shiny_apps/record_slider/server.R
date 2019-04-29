## Packages
library(ggplot2)
library(dplyr)

## Data needed
plot_df = readRDS("/Users/saundersk1/Documents/Git/completionSeminar/shiny_apps/record_slider/plot_df.rds")
# plot_df = plot_df %>%
#   dplyr::filter(record_wet_year == year | record_dry_year == year)
mainland_df = readRDS("/Users/saundersk1/Documents/Git/completionSeminar/shiny_apps/record_slider/mainland_df.rds")
tas_df = readRDS("/Users/saundersk1/Documents/Git/completionSeminar/shiny_apps/record_slider/tas_df.rds")

server <- function(input, output){
  output$recordPlot <- renderPlot({
    ggplot() +
      # geom_point(data = plot_df,
      #            aes(x= x, y = y, group = input$year), col = "lightgray", size = 0.1, alpha = 0.1) +
      geom_path(data=mainland_df, aes(x = Long, y = Lat)) +
      geom_path(data=tas_df, aes(x = Long, y = Lat)) +
      geom_point(data = plot_df %>%
                   filter(record_wet_year == input$year),
                 aes(x= x, y =y, group = year), size = 1, col = "mediumblue") +
      geom_point(data = plot_df %>%
                   filter(record_dry_year == input$year),
                 aes(x= x, y =y, group = year), size = 1, col = "red") +
      coord_fixed() +
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(paste("Year:", input$year)) +
      theme_bw()
  })
}