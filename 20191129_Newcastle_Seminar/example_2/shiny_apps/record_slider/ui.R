# ui <- fluidPage(
#   plotOutput("distPlot"),
#   sliderInput("year", "Year:",
#                 min = 1910, max = 2017, value = 1974)
# )

ui <- fluidPage(
  sidebarLayout(
  sidebarPanel(
      # sliderInput("year", "Year:",
      #               min = 1910, max = 2017, value = 1974), 
    numericInput('year', 'Year:', 1974,
                 min = 1910, max = 2017),
     # <font color="white"> white space </font> <font color="blue"> Largest Annual Maxima </font> -->
    h4("Annual Maxima:\n\n"),
    # add_text(slides[0], "my-title", "How Do We Model Rainfall Extremes?")
    h5("Driest  (red)"),
    h5("Wettest (blue)"),
      width = 4
  ),
  mainPanel(
    plotOutput("recordPlot")
  )
  )
)