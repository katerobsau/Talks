### Plot to compare distances

# --------------------------------------------------------

# File details
thesis_dir = "/Users/saundersk1/Dropbox/Hard Drive/Thesis/"
chap_dir = "chapters/06_cluster/sections/img/"
plot_dir = paste(thesis_dir, chap_dir, sep = "")
compare_distance_plot = "compare_distance_plot.pdf"
compare_distance_path = paste(plot_dir, compare_distance_plot, sep = "")

# --------------------------------------------------------

# for plotting
text.type.large <- element_text(size = 12)
text.type.small <- element_text(size = 11)
source("/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/R/utils_dist.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_fitting.R")
source("/Users/saundersk1/Documents/Git/clusterExtremes/R/utils_aus_outline.R")
mainland_df <- utils_mainland()
tas_df <- utils_tasmania()

# --------------------------------------------------------

### Get region coords and data

# inputs needed
wa_id = 2; #wa_k = 7
print("Warning: May need to update region_id if we update the clustering in the working directory!!!")
region_id = wa_id
working_dir = "/Users/saundersk1/Dropbox/Hard Drive/R/2018/ChapterCluster/"

# data read
fmado_data = readRDS(paste(working_dir, "Data/fmado_data.rds", sep = ""))
region_coords = readRDS(paste(working_dir, "Data/region_coords.rds", sep = ""))

# data processing
test_coords = region_coords[[region_id]]
test_max = select(fmado_data, test_coords$id)
dim(test_max); dim(test_coords)

# --------------------------------------------------------

#### For distance interpolation

# Inputs
min_common_years = 20
max_euclid = 1

# get distances
x = test_max

# update fmado distances for clustering
DD_fmado <- get_fmado_dist(x)

# cap the maximum fmado distances
DD_fmado_cap <- cap_fmado_dist(DD_fmado)

# count the overlapping observations between pairs
DD_common <- get_num_common_obs(x)

# if overlapping observations are too few, set to NA
DD_fmado_min <- apply_min_obs(DD_fmado = DD_fmado_cap,
                              DD_common, min_common_years)

# get euclid for the region
DD_euclid = dist(test_coords %>% select(-id), diag = TRUE, upper = TRUE)

# restrict the range of distances to the theoretical range
DD_fmado_range <- range_infill_missing_fmado(DD_euclid = DD_euclid,
                                             DD_fmado = DD_fmado_min, max_euclid)

DD_fmado_infill <- DD_fmado_range

DD_fmado_all <- crude_infill_missing_fmado(DD_euclid = DD_euclid,
                                           DD_fmado = DD_fmado_infill,
                                           max_euclid)

print("This interpolation definitiely needs review")

# --------------------------------------------------------

# Get other distances

# Euclidean
meta_data = readRDS(paste(working_dir, "Data/AS_meta_data.rds", sep = ""))
coords = meta_data %>% filter(id %in% test_coords$id) %>%
  select(longitude, latitude)
DD_euclid = dist(coords, method = "manhattan", diag = TRUE, upper = TRUE)

# --------------------------------------------------------

## Clustering
cluster_method = "Hierarchical"
linkage_method = "average"

k = 10
fmado_dendrogram = hclust(DD_fmado_all, method = linkage_method)
fmado_clusters = fmado_dendrogram %>%
  cutree(k = k)
len = length(fmado_dendrogram$height)
fmado_height = fmado_dendrogram$height[len - k + 1] %>%
  signif(2)


library(cluster)
pam_k = 7
pam_clusters = pam(DD_fmado_all, k = pam_k)$clustering
# --------------------------------------------------------

# tidy cluster info
pam_df = data.frame(test_coords, cluster_id = pam_clusters) %>%
  mutate(distance_type = paste("K-medoids, k =", pam_k))

fmado_df = data.frame(test_coords, cluster_id = fmado_clusters) %>%
  mutate(distance_type = paste("Hierarchical k = ", length(unique(fmado_clusters))))

### ---------------------------------------------------------------------------

### Plot the euclidean vs fmadogram clustering
plot_coords <- rbind(pam_df, fmado_df)

plot_title = "Comparing cluster methods"

cluster_plot <- ggplot() +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude,
                 col = as.factor(cluster_id),
                 shape = as.factor(cluster_id%%6),
                 group = distance_type)) +
  coord_fixed() +
  facet_wrap(~distance_type, ncol = 2) +
  geom_path(data = mainland_df, aes(x = Long, y = Lat)) +
  geom_path(data = tas_df, aes(x = Long, y = Lat)) +
  scale_x_continuous(limits = range(plot_coords$longitude) + c(-0.1, 0.1)) +
  scale_y_continuous(limits = range(plot_coords$latitude) + c(-0.1, 0.1)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle(plot_title) +
  theme(legend.position = "none",
        legend.text = text.type.small,
        strip.text.x = text.type.large,
        axis.text = text.type.small,
        plot.title = text.type.large,
        axis.title = text.type.large)

cluster_plot


#### --------------------

grid_space = 0.05
min_dist = 0.3
names(coords) = c("x", "y")

grid_domain = get_grid_for_classification(coords = coords,
                                          grid_space = grid_space,
                                          min_dist = min_dist)

knn_value = 15
classify_grid = classify_with_kknn(coords = coords,
                                   cluster_ids = fmado_clusters,
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

poly_df = map.df %>% 
  mutate(plot_type = "1. Classification") 

hclust_df <- data.frame(test_coords, cluster_id = fmado_clusters) %>%
  mutate(plot_type = "2. Clustering") 

plot_df <- rbind(poly_df, hclust_df)

poly_plot <- ggplot() +
  geom_raster(data = classify_grid %>% 
                mutate(plot_type = "1. Classification"),
              aes(x=x, y=y, fill = as.factor(class_id), alpha = prob_summary)) +
  geom_polygon(data = poly_df,
               aes(long, lat, group = group),
               color = "black", fill = NA) +
  geom_point(data = plot_coords,
             aes(x = longitude, y = latitude,
                 fill = as.factor(cluster_id)
                 ), shape = 21) + 
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

