library(ggplot2)
library(ggmap)
library(dplyr)
library(ggmap)
library(ggplot2)
library(sf)
library(dplyr)
library(osmdata)

####################
###### Setup #######
####################

# Load the data
listings <- read.csv('./Data/listings_may_23_with_features_added.csv')





############
############

# Load library

library(ggmap)
library(ggplot2)
library(sf)
library(dplyr)
library(osmdata)

# Get Rotterdam boundary
rotterdam_boundary <- getbb("Rotterdam, Netherlands", format_out = "polygon") %>%
  opq() %>%
  add_osm_feature(key = "admin_level", value = c("7", "8", "9")) %>%
  osmdata_sf()

# Get basemap
rotterdam_map <- get_stadiamap(
  bbox = c(left = 4.3, bottom = 51.85, right = 4.65, top = 52),
  zoom = 11,
  maptype = "stamen_toner_lite"
)

# Assuming your data is in a dataframe called 'listings'
ggmap(rotterdam_map) +
  geom_sf(data = rotterdam_boundary$osm_multipolygons,
          inherit.aes = FALSE,
          fill = NA,
          color = "darkgray",
          size = 0.5) +
  geom_point(data = listings,
             aes(x = longitude, y = latitude, color = house_type),
             alpha = 0.6,
             size = 1) +
  scale_color_manual(values = c("appartement" = "#4A90E2", "huis" = "#E55934"),
                     labels = c("Apartment", "House")) +
  labs(title = "Property Listings in Rotterdam, Netherlands",
       color = "Property Type") +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_sf(crs = st_crs(4326)) +
  annotate("text", x = 4.48, y = 51.88, label = "Rotterdam", fontface = "bold", size = 4)

ggsave("rotterdam_property_listings.pdf", width = 10, height = 7, units = "in")

#######################################################################
###### Property Listings Map colored by type (house, apartment) #######
#######################################################################

library(ggmap)
library(ggplot2)
library(sf)
library(dplyr)
library(osmdata)
library(ggspatial)  # for scale bar

# Get Rotterdam boundary
rotterdam_boundary <- getbb("Rotterdam, Netherlands", format_out = "polygon") %>%
  opq() %>%
  add_osm_feature(key = "admin_level", value = c("8", "10")) %>%
  osmdata_sf()

# Get basemap
rotterdam_map <- get_stadiamap(
  bbox = c(left = 4.3, bottom = 51.855, right = 4.70, top = 51.998),
  zoom = 12,
  maptype = "stamen_toner_lite"
)

# Assuming your data is in a dataframe called 'listings'
ggmap(rotterdam_map) +
  geom_sf(data = rotterdam_boundary$osm_multipolygons,
          inherit.aes = FALSE,
          fill = NA,
          color = "darkgray",
          size = 0.5) +
  geom_point(data = listings,
             aes(x = longitude, y = latitude, color = house_type),
             alpha = 0.2,
             size = 3.2) +
  scale_color_manual(values = c("appartement" = "#FF6F61", "huis" = "#6B5B95"),
                     labels = c("Apartment", "House")) +
  labs(color = "Property Type",
       x = "Longitude",
      y = "Latitude") +
  theme_minimal() +
  theme(
    legend.position = c(0.1, 0.9),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(face = "bold", size = 14), # Change legend title size
    legend.text = element_text(size = 12), # Change legend text size
    axis.text = element_text(size = 14), # Change axis text size
    axis.title = element_text(size = 16) # Change axis title size
  ) +
  coord_sf(crs = st_crs(4326)) +
  annotate("text", x = 4.4777, y = 51.9243, label = "Rotterdam", 
           fontface = "bold", 
           size = 3) +
  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.2, "cm"))

ggsave("./Writing/Thesis/resources/rotterdam_property_listings.png", width = 10, height = 7, units = "in", dpi = 300, bg = "white")


#############################################
###### Greenery Index Map with points #######
#############################################

library(ggmap)
library(ggplot2)
library(sf)
library(dplyr)
library(osmdata)
library(ggspatial)  # for scale bar

# Load the data
panoids <- st_read('/Users/wolff/Desktop/delft_download/data/Rotterdam_NL/panoids/panoids.geojson')
greenery_data <- read.csv('./Data/greenery_index_results_big.csv')

# Clean the Greenery_Index column
greenery_data$Greenery_Index <- as.numeric(gsub("\\[|\\]", "", greenery_data$Greenery_Index))

# Aggregate by the panoid column, not image name (mean)
greenery_data <- greenery_data %>%
  group_by(panoid) %>%
  summarise(Greenery_Index = mean(Greenery_Index)) %>%
  ungroup()

# Merge the datasets on the panoid column
df <- merge(panoids, greenery_data, by = 'panoid')

# Get the Rotterdam boundary
# Get the basemap <-- we do these two above

# Create the plot
ggmap(rotterdam_map) +
  geom_sf(data = rotterdam_boundary$osm_multipolygons,
          inherit.aes = FALSE,
          fill = NA,
          color = "darkgray",
          size = 0.5) +
  geom_point(data = df,
             aes(x = lng, y = lat, color = Greenery_Index),
             alpha = 0.2,
             size = 3.2) +
  scale_color_gradient(low = "#eef0c0", high = "#006400", 
                       name = "Greenery",
                      #  breaks = c(0.2, 0.4, 0.6, 0.8, 1.0), # Customize breaks
                      #  labels = c("Low", "Medium-Low", "Medium", "Medium-High", "High")
                      ) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  #guides(colour = guide_colourbar(direction = "horizontal")) +
  theme(
    legend.position = c(0.07, 0.83),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(face = "bold", size = 14), # Change legend title size
    legend.text = element_text(size = 12), # Change legend text size
    axis.text = element_text(size = 14), # Change axis text size
    axis.title = element_text(size = 16), # Change axis title size
    plot.title = element_text(hjust = 0.5, size = 16) # Center the title
  ) +
  coord_sf(crs = st_crs(4326)) +
  annotate("text", x = 4.4777, y = 51.9243, label = "Rotterdam", 
           fontface = "bold", 
           size = 3) +
  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.2, "cm"))

ggsave("./Writing/Thesis/resources/rotterdam_greenery_map.png", width = 10, height = 7, units = "in", dpi = 300, bg = "white")


#################
### average greenery index per neighborhood (not working as supposed to) ####
############

library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggspatial)

# Fetch Rotterdam city boundary
rotterdam_city <- getbb("Rotterdam, Netherlands", format_out = "polygon") %>%
  opq() %>%
  add_osm_feature(key = "admin_level", value = "8") %>%
  osmdata_sf()

# Fetch Rotterdam neighborhoods using multiple tags
rotterdam_neighborhoods <- getbb("Rotterdam, Netherlands", format_out = "polygon") %>%
  opq() %>%
  add_osm_features(features = list(
    "place" = "neighbourhood",
    "admin_level" = "10",
    "boundary" = "administrative"
  )) %>%
  osmdata_sf()

# Transform to a suitable projected CRS for the Netherlands (EPSG:28992 - Amersfoort / RD New)
rotterdam_city_projected <- st_transform(rotterdam_city$osm_multipolygons, 28992)
rotterdam_neighborhoods_projected <- st_transform(rotterdam_neighborhoods$osm_multipolygons, 28992)

# Clip neighborhoods to city boundary
rotterdam_neighborhoods_clipped <- st_intersection(rotterdam_neighborhoods_projected, rotterdam_city_projected)

# Convert df to sf object and project
df_sf <- st_as_sf(df, coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(28992)

# Spatially join the greenery index data with the neighborhood boundaries
joined_data <- st_join(df_sf, rotterdam_neighborhoods_clipped, join = st_intersects)

# Calculate the average greenery index per neighborhood
average_greenery <- joined_data %>%
  group_by(name) %>%
  summarise(Average_Greenery = mean(Greenery_Index, na.rm = TRUE),
            .groups = 'drop') %>%
  st_drop_geometry()

# Join the average greenery index back to the neighborhood boundaries
neighborhoods_greenery <- rotterdam_neighborhoods_clipped %>%
  left_join(average_greenery, by = "name")

# Transform back to WGS84 for mapping
neighborhoods_greenery <- st_transform(neighborhoods_greenery, 4326)

# Plot the average greenery index per neighborhood
ggplot() +
  geom_sf(data = neighborhoods_greenery, aes(fill = Average_Greenery), color = "white", size = 0.2) +
  scale_fill_viridis(option = "viridis", name = "Average\nGreenery Index", 
                     na.value = "grey80", direction = -1) +
  geom_sf_text(data = neighborhoods_greenery, aes(label = name), size = 2, check_overlap = TRUE) +
  geom_sf(data = st_transform(rotterdam_city$osm_multipolygons, 4326), fill = NA, color = "black", size = 0.5) +
  labs(title = "Average Greenery Index per Neighborhood in Rotterdam",
       caption = "Data source: OpenStreetMap contributors") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 8, color = "grey50")
  ) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(location = "bl", width_hint = 0.3)

ggsave("./Writing/Thesis/resources/rotterdam_neighborhoods_greenery_improved.png", width = 10, height = 7, units = "in", dpi = 300, bg = "white")

