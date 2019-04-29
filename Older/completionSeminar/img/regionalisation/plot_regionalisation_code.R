library(clusterExtremes)

# Initialise necessary parameters
source("/Users/saundersk1/Documents/Git/clusterExtremes/helper/0-initialise.R")

coords = region_coords

# Maximum data
max_data = select(fmado_data, coords$id)
dim(max_data); dim(coords)
if(ncol(max_data) != nrow(coords)){
  stop("Dimensions of max_data and coords do not match")
}

# Get cluster distance
print("Dist file reference!!!")
clust_dist <- get_dist(x = max_data,
                       coords = coords %>% select(-id),
                       min_common_years = min_common_years,
                       max_euclid = max_euclid)

full_tree = hclust(clust_dist, method = linkage_method)
cut_tree = cutree(full_tree, h = 0.133)

grid_space = 0.1

grid_domain = get_grid_for_classification(coords = coords,
                                          grid_space = grid_space,
                                          min_dist = min_dist)

classify_grid = classify_with_kknn(coords = coords,
                   cluster_ids = cut_tree,
                   points_classify = grid_domain,
                   knn_value = knn_value)

grid_info = classify_grid %>%
  mutate(z = class_id %>% as.numeric(), xc = x, yc=y) %>%
  select(-class_id)

z = grid_info$z %>% as.numeric()
x = grid_info$xc %>% as.numeric()
y = grid_info$yc %>% as.numeric()

grd <- data.frame(z = z, xc = x, yc = y)
sp::coordinates(grd) <-~ xc + yc
sp::gridded(grd) <- TRUE
grd <- as(grd, "SpatialGridDataFrame")
at <- 1:ceiling(max(z, na.rm = TRUE))
plys <- inlmisc::Grid2Polygons(grd, level = FALSE, at = at)

# get the classes from the map data
map <- plys
map@data$id = rownames(map@data)
map.points = fortify(map) #, region="id")
map.df = full_join(map.points, map@data, by="id")

poly_df = map.df

poly_plot <- ggplot() +
  geom_raster(data = classify_grid,
              aes(x=x, y=y, fill = as.factor(class_id), alpha = prob_summary)) +
  geom_polygon(data = poly_df,
               aes(long, lat, group = group),
               color = "black", fill = NA) +
  coord_equal() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(legend.position = "none")

mainland_df <- utils_mainland()
tas_df <- utils_tasmania()
poly_plot <- poly_plot +
    geom_path(data = tas_df, aes(x = Long, y =Lat), col = "gray") +
    geom_path(data = mainland_df, aes(x = Long, y =Lat), col = "gray") +
    scale_x_continuous(limits = range(x) + c(-0.25, 0.25)) +
    scale_y_continuous(limits = range(y) + c(-0.25, 0.25))

poly_plot



