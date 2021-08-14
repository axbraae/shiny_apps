#geospatial graphs code

#load libraries
library(tidyverse)
library(Rcpp)
library(sf)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)


# greenspace data ---------------------------------------------------------

#read in greenspace data
greenspace_council_names_only <- read_csv(here::here("data/clean_data/greenspace_council_names.csv")) %>% 
  filter(str_detect(area_code, "^S120"))

#read in spatial local authority data and simplify to 1km
la_zones <- st_read(here::here("data/raw_data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")) %>% 
  st_simplify(preserveTopology = FALSE, dTolerance = 1000)

#use merge to merge health data to shape file
greenspace_council_names_only_zones <- la_zones %>% 
  merge(greenspace_council_names_only, by.x = "code", by.y = "area_code")

#plot geospatial graph with example filters
greenspace %>% 
  group_by(area_code, date_code, age) %>% 
  slice_max(value_percent)
  filter(date_code == "2019",
         distance_to_nearest_green_or_blue_space == "A 5 minute walk or less",
         age == "All", 
         gender == "All",
         urban_rural_classification == "All",
         simd_quintiles == "All",
         type_of_tenure == "All",
         household_type == "All",
         ethnicity == "All") %>% 
  ggplot() +
  geom_sf(aes(fill = value_percent), colour = "black") +
  theme_minimal()
  
  
        distance_to_nearest_green_or_blue_space == "A 5 minute walk or less",
        gender == "All",
        urban_rural_classification == "All",
        simd_quintiles == "All",
        type_of_tenure == "All",
        household_type == "All",
        ethnicity == "All") %>% 
      filter(date_code == input$year_input |is.na(date_code),
             age == input$age_input |is.na(age)) %>% 
      ggplot() +
      geom_sf(aes(fill = value_percent), colour = "black") +
      theme_minimal() +
      labs(title = "Percentage of people less than 5 min away from Green space")


greenspace_map <- greenspace %>% 
  filter(
    str_detect(area_code, "^S120"),
    date_code >= 2016,
    distance_to_nearest_green_or_blue_space == "A 5 minute walk or less",
    gender == "All",
    urban_rural_classification == "All",
    simd_quintiles == "All",
    type_of_tenure == "All",
    household_type == "All",
    ethnicity == "All") %>% 
  group_by(
    area_code, ca_name, age
  ) %>% 
  summarise(
    mean_percent = mean(value_percent)
  )

# health_survey_local data ------------------------------------------------

#read in scotland_health_survey_local_clean
scotland_health_survey_local_clean <- read_csv(
  here::here("data/clean_data/scotland_health_survey_local_clean.csv")) %>% 
  filter(str_detect(area_code, "^S120")) 

#use merge to merge health data to shape file, keep all columns
scotland_health_survey_local_clean_zones <- la_zones %>% 
  merge(scotland_health_survey_local_clean, by.x = "code", by.y = "area_code", all = TRUE)


#plot geospatial graph with example filters
#need to filter for NA columns to keep empty council/la zones
scotland_health_survey_local_clean_zones %>% 
  filter(scottish_health_survey_indicator == "Life satisfaction: Mode (8)" | 
           is.na(scottish_health_survey_indicator),
         sex == "All" | is.na(sex)) %>% 
  mutate(centres = st_centroid(geometry)) %>%
  # pull out the lat/longs from centres and create new columns with these
  mutate(lat = st_coordinates(centres)[,1],
         long = st_coordinates(centres)[,2]) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage), colour = "black") +
  geom_text(aes(x = lat, y = long, label = ca_name),
            color = "black", fontface = "bold", size = 4,
            check_overlap = TRUE)

spdf_uk <- ne_countries(country = 'united kingdom',
                        scale = "medium", returnclass = "sf")
class(spdf_uk)

ggplot(data = spdf_uk) +
  geom_sf()