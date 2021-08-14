library(sf)
library(tidyverse)
library(shiny)
library(classInt)

#figure out a way to do better breaks for the map to turn it into a choropleth
breaks_greenspace <- classIntervals(c(min(greenspace$value_percent) - 0.00001,
                                      greenspace$value_percent),
                                    n = 15, style = "quantile")

greenspace_la <- greenspace %>%
  filter(
    str_detect(area_code, "^S120"),
    date_code >= 2016,
    distance_to_nearest_green_or_blue_space != "Don't Know",
    gender == "All",
    urban_rural_classification == "All",
    simd_quintiles == "All",
    type_of_tenure == "All",
    household_type == "All",
    ethnicity == "All") %>%
  group_by(
    area_code, ca_name, age, distance_to_nearest_green_or_blue_space
  ) %>%
  summarise(mean_percent = mean(value_percent)) %>% 
  mutate(mean_percent = cut(mean_percent, breaks_greenspace$brks))

#read in spatial local authority data and simplify to 1km
la_zones <- st_read(here::here("data/raw_data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 1000)

#use merge health data to shape file, all = TRUE to keep areas with no data
greenspace_la_geo <- la_zones %>%
  merge(greenspace_la, by.x = "code", by.y = "area_code", all = TRUE)

scottish_survey_la_geo <- la_zones %>%
  merge(scottish_survey_local, by.x = "code", by.y = "area_code", all = TRUE)