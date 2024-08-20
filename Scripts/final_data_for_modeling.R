library(tidyverse)

# Read in the raw data
raw_data <- read_csv('Data/listings_may_23_with_features_added.csv') 

# First, let's create a mapping of neighborhoods to areas
neighborhood_to_area <- list(
  "Rotterdam Centrum" = c("Cool", "Cs Kwartier", "Stadsdriehoek", "Oude Westen", "Dijkzigt", "Nieuwe Werk"),
  "Delfshaven" = c("Bospolder", "Delfshaven", "Middelland", "Nieuwe Westen", "Oud Mathenesse", "Schiemond", "Spangen", "Tussendijken"),
  "Overschie" = c("Kleinpolder", "Noord Kethel", "Overschie", "Schieveen", "Zestienhoven"),
  "Noord" = c("Agniesebuurt", "Bergpolder", "Blijdorp", "Liskwartier", "Oude Noorden", "Provenierswijk"),
  "Hillegersberg-Schiebroek" = c("Hillegersberg Noord", "Hillegersberg Zuid", "Molenlaankwartier", "Schiebroek", "Terbregge"),
  "Kralingen-Crooswijk" = c("De Esch", "Kralingen Oost", "Kralingen West", "Nieuw Crooswijk", "Oud Crooswijk", "Rubroek", "Struisenburg"),
  "Prins Alexander" = c("Het Lage Land", "'s-Gravenland", "Kralingseveer", "Nesselande", "Ommoord", "Oosterflank", "Prinsenland", "Zevenkamp"),
  "Feijenoord" = c("Afrikaanderwijk", "Bloemhof", "Feijenoord", "Hillesluis", "Katendrecht", "Kop van Zuid", "Kop van Zuid - Entrepot", "Noordereiland", "Vreewijk"),
  "IJsselmonde" = c("Beverwaard", "Groot IJsselmonde", "Lombardijen", "Oud IJsselmonde"),
  "Charlois" = c("Carnisse", "Charlois Zuidrand", "Oud Charlois", "Pendrecht", "Tarwewijk", "Zuiderpark", "Wielewaal", "Zuidplein", "Zuidwijk", "Heijplaat"),
  "Pernis" = c("Pernis"),  # Assuming Pernis is its own neighborhood
  "Hoogvliet" = c("Hoogvliet"),  # Assuming Hoogvliet is its own neighborhood
  "Hoek van Holland" = c("Hoek van Holland"),  # Assuming Hoek van Holland is its own neighborhood
  "Rozenburg" = c("Rozenburg")  # Assuming Rozenburg is its own neighborhood
)

# Create a function to find the area for a given neighborhood
find_area <- function(neighborhood) {
for (area in names(neighborhood_to_area)) {
if (neighborhood %in% neighborhood_to_area[[area]]) {
 return(area)
}
}
return(NA)  # Return NA if no matching area is found
}

# Apply the function to create a new column 'area' in raw_data
raw_data$Area <- sapply(raw_data$neighborhood_name, find_area)


# Read in the socio-demographic data
socio_demographic_data <- read_csv('./Data/socio_demographic_data.csv')

# Make final socio-demographic data frame
socio_demographic_df <- socio_demographic_data %>%
       select(Area,
              income_low_pct, income_mid_pct, income_high_pct,
              education_low_pct, education_mid_pct, education_high_pct,
              schools_within_3km, dist_to_large_supermarket_km,
              crimes_per_year_count, net_labor_force_participation_pct) %>%
       filter((Area %in% c("Rotterdam Centrum", "Delfshaven", "Overschie", "Noord", "Hillegersberg-Schiebroek", "Kralingen-Crooswijk", "Prins Alexander", "Feijenoord", "IJsselmonde", "Charlois"))) %>%
       mutate(across(ends_with("_pct"), ~ as.numeric(str_remove(., "%"))),
              schools_within_3km = as.numeric(schools_within_3km),
              dist_to_large_supermarket_km = as.numeric(dist_to_large_supermarket_km),
              crimes_per_year_count = as.numeric(crimes_per_year_count))


#write_csv(socio_demographic_df, 'socio_demographic_df_for_data_section.csv')


################################################################################
## connect raw_data to the socio_demographic data.
################################################################################


# Now, let's merge the datasets
merged_data <- raw_data %>%
  left_join(socio_demographic_df, by = "Area")

# Write the merged data to a CSV file
#write_csv(merged_data, "merged_data_with_socio_demographics.csv")


################################################################################
# FINAL DATA FOR MODELING
################################################################################

final_data_for_modeling <- merged_data %>%
  
  # Select relevant columns
  select(
    # Listings Data
    price_sold, living_area, energy_label, room, year_built, insulation, heating, house_type,
    # Socio-Demographic Data
    income_low_pct, income_mid_pct, income_high_pct, education_low_pct, education_mid_pct, education_high_pct,
    schools_within_3km, dist_to_large_supermarket_km, crimes_per_year_count, net_labor_force_participation_pct,
    # Image Features
    average_greenery_index_100m, average_greenery_index_250m, average_greenery_index_500m, average_greenery_index_1000m,
    average_people_count_100m, average_people_count_250m, average_people_count_500m, average_people_count_1000m,
    average_cars_count_100m, average_cars_count_250m, average_cars_count_500m, average_cars_count_1000m,
    average_bicycles_count_100m, average_bicycles_count_250m, average_bicycles_count_500m, average_bicycles_count_1000m,
    assoc_panoid_count_100m, assoc_panoid_count_250m, assoc_panoid_count_500m, assoc_panoid_count_1000m,
    # Other Data
    full_address, Area, latitude, longitude, building_type, date_listed, date_sold, url
    ) %>%
  
  # Filter rows based on price_sold and living_area conditions
  filter(price_sold >= 50000,
         price_sold <= 2500000,
         living_area > 0) %>%
  
  # Apply KNN imputation for missing values in energy_label
  VIM::kNN(k = 5, variable = c("energy_label"), imp_var = FALSE) %>%
  
  # Clean and categorize insulation column
  mutate(energy_label = ifelse(energy_label %in% c(">A+", "A", "B", "C"), "Higher", "Lower"),
         insulation = case_when(
            str_detect(insulation, regex("Volledig geïsoleerd", ignore_case = TRUE)) ~ "Fully Insulated",
            str_detect(insulation, regex("dubbel glas", ignore_case = TRUE)) ~ "Double Glass",
            TRUE ~ "Other"
          ),
         # Clean and categorize heating column
         heating = case_when(
            str_detect(heating, regex("Stadsverwarming", ignore_case = TRUE)) ~ "District Heating",
            str_detect(heating, regex("Cv-ketel", ignore_case = TRUE)) ~ "Boiler",
            TRUE ~ "Other"
          )) %>%
  
  # Convert columns to factors
  mutate(building_type = as.factor(building_type),
         house_type = as.factor(house_type),
         insulation = as.factor(insulation),
         heating = as.factor(heating)) %>%
  
  # Simplify variable names
  rename(
    # Socio-Demographic Data
    income_low = income_low_pct,
    income_mid = income_mid_pct,
    income_high = income_high_pct,
    edu_low = education_low_pct,
    edu_mid = education_mid_pct,
    edu_high = education_high_pct,
    schools_3km = schools_within_3km,
    supermarket_km = dist_to_large_supermarket_km,
    crimes = crimes_per_year_count,
    net_LFP = net_labor_force_participation_pct,
    # Image Features
    UGI_100m = average_greenery_index_100m,
    UGI_250m = average_greenery_index_250m,
    UGI_500m = average_greenery_index_500m,
    UGI_1000m = average_greenery_index_1000m,
    people_100m = average_people_count_100m,
    people_250m = average_people_count_250m,
    people_500m = average_people_count_500m,
    people_1000m = average_people_count_1000m,
    cars_100m = average_cars_count_100m,
    cars_250m = average_cars_count_250m,
    cars_500m = average_cars_count_500m,
    cars_1000m = average_cars_count_1000m,
    bicycles_100m = average_bicycles_count_100m,
    bicycles_250m = average_bicycles_count_250m,
    bicycles_500m = average_bicycles_count_500m,
    bicycles_1000m = average_bicycles_count_1000m,
    associated_panoids_100m = assoc_panoid_count_100m,
    associated_panoids_250m = assoc_panoid_count_250m,
    associated_panoids_500m = assoc_panoid_count_500m,
    associated_panoids_1000m = assoc_panoid_count_1000m
  )

# Save the final data to ./Data path
write_csv(final_data_for_modeling, "Data/final_data_for_modeling.csv")
