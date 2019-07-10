# install packages
library(ggplot2)
library(lubridate)
library(dplyr)
library(plyr)
library(gtools)
library(tidyr)
library(extRemes)
library(cowplot)

# get the files with temperature data
file_dir = "/Users/katesaunders/Dropbox/TU Delft/2019/R_Temperature_Bivariate_Dependence/"
files_in_folder = list.files(file_dir)
climate_var = "TEMP"
var_files = files_in_folder[grepl(climate_var, files_in_folder)]
stn_names = sapply(var_files, function(str){strsplit(str, split = "_")[[1]][1]}) %>%
  as.character()
num_stns = length(stn_names)

# ----------------------------------------------------------------

# get coordinates
coords <- data.frame(long = NULL, lat = NULL)
for(i in 1:num_stns){
  file_name = paste(stn_names[i], "_TEMP.txt", sep = "")
  temp_file = paste(file_dir, file_name, sep ="")
  temp_data = readLines(temp_file)[2]
  split_str = strsplit(temp_data, split = "\\s+")[[1]]
  lat = readr::parse_number(split_str[3]) %>% as.numeric()
  long = readr::parse_number(split_str[4])  %>% as.numeric()
  coords <- rbind(coords, data.frame(long, lat))
}

coords <- coords %>%
  mutate(NAME = stn_names)

coord_plot <- ggplot(data = coords, aes(x = long, y = lat)) +
  geom_point() + 
  geom_label(aes(label = NAME))

coord_plot

mp <- NULL
mapWorld <- borders("world", colour ="black", fill = "forestgreen", 
                    xlim = range(coords$long) + c(-2,2), 
                    ylim = range(coords$lat) + c(-2,2))    # create a layer of borders
mp <- ggplot() + 
  mapWorld +
  geom_point(data = coords %>% filter(NAME %in% c("DenHelder", "DeBilt")), 
             aes(x= long, y= lat) , color="blue", size = 3, shape = 21) +
  geom_text(data = coords  %>% filter(NAME %in% c("DenHelder", "DeBilt")), 
            aes(x= long, y= lat, label = NAME), hjust = 1.2, vjust = 0.3, size = 4) + 
  coord_fixed(xlim = c(3, 8), ylim = c(49,54)) + 
  xlab("Longitude") +
  ylab("Latitude") + 
  ggtitle("Map of the Netherlands")
mp