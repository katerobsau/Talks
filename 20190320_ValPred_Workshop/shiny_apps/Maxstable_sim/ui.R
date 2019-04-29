  ui <- fluidPage(
    
    mainPanel("",
              fluidRow(
                splitLayout(cellWidths = c("25%", "42.5%", "42.5%"), 
                            plotOutput("poiPlot"), 
                            plotOutput("shapePlot"), 
                            plotOutput("simPlot"))
              )
    ),
    # fluidRow(column(2, 
    #   plotOutput("poiPlot", width = "200px"),
    #   plotOutput("simPlot", width = "200px"))),
    
    sliderInput("simVal", "Index i",
                min = 1, max = 20, value = 1)
  )

