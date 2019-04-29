## Packages
library(ggplot2)
library(dplyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(latex2exp)

set.seed(25)
n = 20
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

df = NULL
for(i in 1:n){
  
  x_i = x_pnts + u[i]
  y_i = y_norm*p[i]
  
  df_i = data.frame(x = x_i, y = y_i) %>% 
    mutate(sim = i)
  
  df = rbind(df, df_i)
  
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

server <- function(input, output){
    
    output$poiPlot <- renderPlot({
      ggplot() + 
        geom_point(data = NULL, aes(x = rep(0, length(p)), y = p), col = "gray") +
        geom_point(data = NULL, aes(x = 0), y = p[input$simVal], col = "blue") +
        ylab("Intensity") +
        xlab("") +
        ggtitle(TeX("$\\left{\\zeta\\right}_{i=1}^{\\infty}$"), TeX("PPP $\\sim \\zeta ^{-2}$")) +
        theme_bw() +
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title=element_text(size = 14),
              plot.subtitle=element_text(size = 20)) 
    })

    
    output$shapePlot <- renderPlot({
      ggplot() + 
        geom_line(data = df_shape, aes(x= x, y = y/p, group = sim), col = "gray") + 
        geom_line(data = df_shape %>% filter(sim == input$simVal), aes(x= x, y =y/p), col = "blue") +
        theme_bw() + 
        xlim(limits = c(x_lower, x_upper)) +
        ylab("`Storm Shape'") +
        ggtitle(TeX("$Y_i(x) = W(x - U_i)$"), TeX("$W$ is a standard Gaussian)", "randomly centred"))+
        theme(plot.title=element_text(size = 20),
              plot.subtitle=element_text(size = 14))
    })
    
    output$simPlot <- renderPlot({
      ggplot() + 
        geom_line(data = df_shape, aes(x= x, y =y, group = sim), col = "gray") +
        geom_line(data = df_shape %>% filter(sim == input$simVal), aes(x= x, y =y), col = "blue") +
        geom_line(data = df_max, aes(x= x, y = y), size = 1.5, col = "red")+#, 
                  # linetype = "dotted") +
        geom_point(data = df_max[seq(1, nrow(df_max), by = 10), ], aes(x= x, y = y), size = 1.5, col = "red", 
                  shape = 20) +
        theme_bw() + 
        xlim(limits = c(x_lower, x_upper)) +
        ylab("") +
        ggtitle(TeX("Smith Process"), TeX("$Z(x) = \\max_{i\\geq 1} \\zeta_i Y_i(x)$")) +
        theme(plot.title=element_text(size = 20),
              plot.subtitle=element_text(size = 14))
    })
    
}

