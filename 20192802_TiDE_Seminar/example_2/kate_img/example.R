library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(10)
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

# plot(0,0, xlim = c(x_lower,x_upper), ylim = c(0, max(p)*dnorm(0) + 0.1), col  ="white")
pmax_vec = rep(0, length(x))
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

pmax_which = apply(df_spread %>% select(-x), 1, which.max) %>% unique()
pmax_vec = apply(df_spread %>% select(-x), 1, max, na.rm = TRUE)

# points(df_spread$x, pmax_vec, col = "red")

ggplot() + 
  geom_line(data = df %>% filter(sim %in% 1:15), aes(x= x, y =y, group = sim), col = "gray") + 
  geom_line(data = NULL, aes(x = df_spread$x, y = pmax_vec), linetype = "dotted", col = "red", size = 2) + 
  theme_bw() + 
  xlim(limits = c(x_lower, x_upper))
