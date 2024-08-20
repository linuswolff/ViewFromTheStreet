library(tidyverse)
library(sf)
library(RColorBrewer)

raw_df <- read_csv("./Data/final_data_for_modeling.csv")

# only keep relevant columns for the area maps
df <- raw_df %>% 
  select(Area, price_sold, living_area, room, year_built, energy_label, insulation, heating, house_type,
         UGI_250m, people_250m, cars_250m, bicycles_250m, UGI_500m, people_500m, cars_500m, bicycles_500m,
         UGI_1000m, people_1000m, cars_1000m, bicycles_1000m, longitude, latitude)

str(df)
table(df$Area)

# create the summary dataframe

# Create the summarized dataframe
summary_df <- df %>%
  group_by(Area) %>%
  summarize(
    avg_UGI_250m = mean(UGI_250m, na.rm = TRUE),
    avg_people_250m = mean(people_250m, na.rm = TRUE),
    avg_cars_250m = mean(cars_250m, na.rm = TRUE),
    avg_bicycles_250m = mean(bicycles_250m, na.rm = TRUE),
    avg_UGI_500m = mean(UGI_500m, na.rm = TRUE),
    avg_people_500m = mean(people_500m, na.rm = TRUE),
    avg_cars_500m = mean(cars_500m, na.rm = TRUE),
    avg_bicycles_500m = mean(bicycles_500m, na.rm = TRUE),
    avg_UGI_1000m = mean(UGI_1000m, na.rm = TRUE),
    avg_people_1000m = mean(people_1000m, na.rm = TRUE),
    avg_cars_1000m = mean(cars_1000m, na.rm = TRUE),
    avg_bicycles_1000m = mean(bicycles_1000m, na.rm = TRUE)
  )

# View the summarized dataframe
print(summary_df)


# Define the areas you care about
areas_of_interest <- c(
  "Charlois", "Delfshaven", "Feijenoord", "Hillegersberg-Schiebroek", 
  "IJsselmonde", "Kralingen-Crooswijk", "Noord", "Overschie", 
  "Prins Alexander", "Rotterdam Centrum"
)

### MAPPING

# import the geojson file 
# Read the file as a single string
library(jsonlite)
geojson_string <- readLines("rotterdam_areas.geojson", warn = FALSE)

# Parse the JSON string
geojson_data <- fromJSON(geojson_string, simplifyVector = FALSE)

# Convert to an sf object
sf_data <- st_read(geojson_data, crs = 28992, quiet = TRUE) %>%
  st_transform(sf_data, crs = 4326) %>% 
  mutate(geometry = st_make_valid(geometry)) %>%
  group_by(naamGebied) %>%
  summarize(geometry = st_union(geometry))


### Joining the summary df and the sf data

# Perform the left join
joined_df <- sf_data %>%
  left_join(summary_df, by = c("naamGebied" = "Area")) %>%
  filter(naamGebied %in% c(areas_of_interest))

# Print the resulting dataframe
print(joined_df)

############
### Maps ###
############

### Just base map of all rotterdam with the areas of interest highlighted
# Add a new column to classify areas as of interest or not
sf_data <- sf_data %>%
  mutate(interest = ifelse(naamGebied %in% areas_of_interest, "Yes", "No"))
# Add centroids to the dataframe
centroids <- st_centroid(sf_data)

centroids_subset <- centroids %>%
  filter(naamGebied %in% areas_of_interest)


# Plot Base map
base_map_plot <- ggplot(data = sf_data) +
  geom_sf(aes(fill = interest), color = "black", size = 0.2) +
  geom_sf_label(data = centroids, aes(label = naamGebied), size = 2.5, check_overlap = T) +
  scale_fill_manual(values = c("Yes" = "orange", "No" = "lightgray"), na.value = "gray") + 
  #labs(title = "Rotterdam Areas of Interest") +
  theme_bw() +
    theme(legend.position = "none",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  plot.background = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank()
)

base_map_plot

# save plot as an svg
ggsave("base_map.png", base_map_plot, width = 20, height = 20)

### Greenery index map
greenery_map_plot <- ggplot(data = joined_df) +
  geom_sf(aes(fill = avg_UGI_500m), color = "white", size = 0.2) +
  geom_sf_text(data = centroids_subset, aes(label = naamGebied), size = 1.5, check_overlap = T) +
  scale_fill_distiller(palette = "Greens", na.value = "gray", direction = 1,
                       breaks = c(0.175, 0.275,0.375)) +   
  labs(fill = "Avg. UGI",
       title = "Greenery") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        #legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        plot.margin = margin(0, 0, 0, 0)
)

people_map_plot <- ggplot(data = joined_df) +
  geom_sf(aes(fill = avg_people_500m), color = "white", size = 0.2) +
    geom_sf_text(data = centroids_subset, aes(label = naamGebied), size = 1.5, check_overlap = T) +
  scale_fill_distiller(palette = "Reds", na.value = "gray", direction = 1) + 
  labs(fill = "Avg. People",
       title = "People") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        #legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        #aspect.ratio = 3,
        plot.margin = margin(0, 0, 0, 0)
)

cars_map_plot <- ggplot(data = joined_df) +
  geom_sf(aes(fill = avg_cars_500m), color = "white", size = 0.2) +
    geom_sf_text(data = centroids_subset, aes(label = naamGebied), size = 1.5, check_overlap = T) +
  scale_fill_distiller(palette = "Purples", na.value = "gray", direction = 1) + 
  labs(fill = "Avg. Cars",
        title = "Cars") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        #legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        plot.margin = margin(0, 0, 0, 0)
)

bicycles_map_plot <- ggplot(data = joined_df) +
  geom_sf(aes(fill = avg_bicycles_500m), color = "white", size = 0.2) +
    geom_sf_text(data = centroids_subset, aes(label = naamGebied), size = 1.5, check_overlap = T) +
  scale_fill_distiller(palette = "Blues", na.value = "gray", direction = 1) + 
  labs(fill = "Avg. Bicycles",
       title = "Bicycles") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        #legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        plot.margin = margin(0, 0, 0, 0)
)

library(patchwork)

# Display the combined plot
combined_maps_plot <- (greenery_map_plot | people_map_plot | cars_map_plot | bicycles_map_plot)

# Display the combined plot
combined_maps_plot

# save the combined plot as a ggplot object in Rdata file
save(combined_maps_plot, file = "combined_maps_plot.Rdata")
