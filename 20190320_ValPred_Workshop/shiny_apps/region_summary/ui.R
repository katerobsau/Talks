ui <- fluidPage(
  
      plotOutput("recordPlot"),
      
      fluidRow(
        
        column(4, 
          selectInput('region', "Region:",
                  choices = list("Tasmania" = "TAS", 
                                 "Southwest Australia" = "SWWA", 
                                 "Southeast Australia" = "SEA", 
                                 "Eastern Australia" = "EA", 
                                 "Northern Australia" = "NA", 
                                 "Regional Australia" = " R"),
                  selected = "SWWA") 
        ),
        column(4, offset = 1,
        sliderInput('cuth', "Cut Height:",
                  min = 0.11, max = 0.17, value = 0.12, step = 0.001, animate = animationOptions(interval = 1750))
        )
      )
)