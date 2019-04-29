## Packages
library(ggplot2)
library(dplyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(latex2exp)

set.seed(20)
n = 10
w = rexp(n, rate = 1)
p = sort(1/w, decreasing = TRUE)
num_signif = 2
x_lower = -3
x_upper = 3
s = 6
u = round(runif(n)*(x_upper - x_lower) - x_upper, digits = num_signif) 
x_pnts = seq(x_lower - s, x_upper + s, by = 10^(-num_signif))
y_norm = dnorm(x_pnts)

plot(rep(0, n), p)

# plot(0,0, xlim = c(x_lower,x_upper), ylim = c(0, max(p)*dnorm(0) + 0.1), col  ="white")
df = NULL
for(i in 1:n){
  
  x_i = x_pnts + u[i]
  y_i = y_norm*p[i]
  
  df_i = data.frame(x = x_i, y = y_i) %>% 
    mutate(sim = i)
  
  df = rbind(df, df_i)
  # lines(x_i, y_i, col = "gray")
  
}


df_spread = df %>% 
  mutate(x = round(x, digits = num_signif)) %>%
  spread(key = sim, value = y)

pmax_which = apply(df_spread %>% select(-x), 1, which.max)
pmax_vec = apply(df_spread %>% select(-x), 1, max, na.rm = TRUE)

df_shape = df %>% 
  right_join(data.frame(sim  = 1:n, p), by = "sim") 

df_max = data.frame(x = df_spread$x, y = pmax_vec, sim = pmax_which) %>%
    right_join(data.frame(sim  = 1:n, p), by = "sim") 

## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    
    
    mainPanel("Max-Stable Process: Example Simulation",
              fluidRow(
                splitLayout(cellWidths = c("10%", "45%", "45%"), 
                            plotOutput("poiPlot"), 
                            plotOutput("shapePlot"), 
                            plotOutput("simPlot"))
              )
    ),
    # fluidRow(column(2, 
    #   plotOutput("poiPlot", width = "200px"),
    #   plotOutput("simPlot", width = "200px"))),
    
    sliderInput("simVal", "Sample:",
                min = 1, max = n, value = 1)
  )
  
}

if (interactive()) {
  # Server logic
  server <- function(input, output){
    
    output$poiPlot <- renderPlot({
      ggplot() + 
        geom_point(data = NULL, aes(x = rep(0, length(p)), y = p), col = "gray") +
        geom_point(data = NULL, aes(x = 0), y = p[input$simVal], col = "blue") +
        ylab("Intensity") +
        xlab("") +
        theme_bw() +
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) 
    })

    
    output$shapePlot <- renderPlot({
      ggplot() + 
        geom_line(data = df_shape, aes(x= x, y = y/p, group = sim), col = "gray") + 
        geom_line(data = df_shape %>% filter(sim == input$simVal), aes(x= x, y =y/p), col = "blue") +
        theme_bw() + 
        xlim(limits = c(x_lower, x_upper)) +
        ylab("Storm Shape")
    })
    
    output$simPlot <- renderPlot({
      ggplot() + 
        geom_line(data = df_shape, aes(x= x, y =y, group = sim), col = "gray") +
        geom_line(data = df_shape %>% filter(sim == input$simVal), aes(x= x, y =y), col = "blue") +
        geom_line(data = df_max, aes(x= x, y = y), size = 1.5, col = "red", 
                  linetype = "dotted") +
        theme_bw() + 
        xlim(limits = c(x_lower, x_upper)) +
        ylab("Max-Stable Process")
    })
    
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  
}
