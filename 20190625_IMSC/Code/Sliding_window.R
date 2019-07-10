library(plotly)
library(extRemes)
library(tidyverse)

plot_df <- tmax_gev_pars
min_temp = fit_data$TMAX %>% min(na.rm = TRUE)
min_temp = min_temp - 5
upper_support = tmax_gev_pars$location - 
  tmax_gev_pars$scale/tmax_gev_pars$shape
max_temp = max(upper_support, na.rm = TRUE)
x = seq(min_temp, max_temp, 0.25)
y <- apply(tmax_gev_pars, 1, function(row){
  loc = row[1]
  scale = row[2]
  shape = row[3]
  year = row[4]
  y <- devd(x, loc, scale, shape)
  y[x > loc - scale/shape] = NA
  x <- c(x, loc - scale/shape)
  y <- c(y , 0)
  len = length(x)
  df <- data.frame(x, y, YEAR = rep(year, len))
  return(df)
}) %>% do.call(rbind, .)

plot_df <- y %>% 
  mutate(YEAR = YEAR %>% as.numeric()) %>%
  left_join(fit_data %>% select(YEAR, TMEAN), by = "YEAR")

sliding_col_gev <- ggplot(data = plot_df) +
  geom_line(aes(x = x, y = y, col = TMEAN, group = YEAR)) +
  geom_point(data = plot_df %>% filter(y == 0), 
             aes(x = x, y = y, col = TMEAN %>% as.numeric())) + 
  scale_colour_distiller(palette = "RdBu") +
  theme(legend.position = "none")
sliding_col_gev 

# sliding_col_gev + transition_time(YEAR) +
  # labs(title = "Year: {frame_time}") + 
  # view_follow(fixed_y = TRUE)

sliding_gev <- ggplot() +
  geom_line(data = plot_df, aes(x = x, y = y, group = YEAR), col = "gray", alpha = 0.1) +
  geom_line(data = plot_df, aes(x = x, y = y, col = TMEAN %>% as.numeric(), frame = YEAR), size = 1.25) + 
  geom_point(data = plot_df %>% filter(y == 0), aes(x = x, y = y, col = TMEAN %>% as.numeric(), frame = YEAR), size = 1.25) + 
  scale_colour_distiller("Annual Mean", palette = "RdYlBu") +
  ylab("Annual Maximum") + 
  theme_bw()

ggplotly(sliding_gev)

