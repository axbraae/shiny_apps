
# read in packages
library(tidyverse)
library(janitor)
library(here)
library(ggthemes)
library(sf)
library(DT)
library(shinythemes)


# read in survey data
scottish_survey <- read.csv(here("data/clean_data/scotland_health_survey_clean.csv"))
scottish_survey_local <- read_csv(here("data/clean_data/scotland_health_survey_local_clean.csv"))
scottish_survey_local_stats <- read_csv(here("data/clean_data/summary_stat_scotland_health_2016_2019.csv"))
greenspace <- read_csv(here("data/clean_data/greenspace_council_names.csv"))

#read in geospatial data
greenspace_la <- greenspace %>%
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
  summarise(mean_percent = mean(value_percent))

#read in spatial local authority data and simplify to 1km
la_zones <- st_read(here::here("data/raw_data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")) %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 1000)

#use merge health data to shape file
greenspace_la_geo <- la_zones %>%
  merge(greenspace_la, by.x = "code", by.y = "area_code", all = TRUE)

scottish_survey_la_geo <- la_zones %>%
    merge(scottish_survey_local, by.x = "code", by.y = "area_code", all = TRUE)

# filter scottish surveys
scottish_survey_local <- scottish_survey_local %>%
  filter(scottish_health_survey_indicator %in% c("Any cardiovascular condition: Has a cardiovascular condition", "Life satisfaction: Below the mode (0-Extremely dissatisfied to 7)", "Obesity: Obese", "Overweight: Overweight (including obese)", "Summary activity levels: Low activity", "Summary activity levels: Very low activity"))


scottish_survey <- scottish_survey %>%
  filter(scottish_health_survey_indicator %in% c("Any cardiovascular condition: Has a cardiovascular condition", "Life satisfaction: Below the mode (0-Extremely dissatisfied to 7)", "Obesity: Obese", "Overweight: Overweight (including obese)", "Summary activity levels: Low activity", "Summary activity levels: Very low activity"))

# read in the life expectancy data
life <- read_csv(here("data/clean_data/life_expectancy_clean.csv"))
# reformatting and sorting the age categories
life <- life %>%
  mutate(age_new = str_remove(age, " years"))
#simd_codes <- distinct(.data = life, simd_quintiles)
simd_codes <- c("All", "5 - least deprived", "4", "3", "2", "1 - most deprived")

#adds summary table for Health Indicators and Greenspace tab

local_greenspace <- read_csv(here("data/clean_data/local_greenspace.csv"))
