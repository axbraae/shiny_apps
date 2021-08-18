# Data cleaning for health app
# Aug 2021

library(tidyverse)
library(janitor)
library(here)

# Read in area code and local authority name detail
area_names <- read_csv(here("data/clean_data/council codes.csv")) %>% 
  clean_names() %>% 
  rename(area_code = ca)


# Scottish Health Survey Overview -----------------------------------------

scotland_health_survey <- read_csv(here("data/raw_data/scotland_health_survey.csv")) %>%
  clean_names()

# Filter health indicators for those related to activity
scotland_health_survey_clean <- scotland_health_survey %>% 
  filter(scottish_health_survey_indicator %in% c(
    "Any cardiovascular condition: Has a cardiovascular condition", 
    "Any cardiovascular condition: No cardiovascular condition", 
    "Life satisfaction: Above the mode (9 to 10-Extremely satisfied)", 
    "Life satisfaction: Below the mode (0-Extremely dissatisfied to 7)", 
    "Life satisfaction: Mode (8)", 
    "Obesity: Not obese", 
    "Obesity: Obese", 
    "Overweight: Not overweight or obese", 
    "Overweight: Overweight (including obese)", 
    "Overweight: Overweight (including obese)", 
    "Summary activity levels: Low activity", 
    "Summary activity levels: Meets recommendations", 
    "Summary activity levels: Some activity", 
    "Summary activity levels: Very low activity"),
         measurement == "Percent")


scotland_health_survey_clean <- scotland_health_survey_clean %>% 
  rename(year = date_code, 
         percentage = value,
         area_code = feature_code) %>% 
  select(area_code, year, scottish_health_survey_indicator, sex, percentage) %>% 
  arrange(year, scottish_health_survey_indicator, sex)

write_csv(scotland_health_survey_clean, 
          here("data/clean_data/scotland_health_survey_clean.csv"))


# Greenspace  -------------------------------------------------------------

greenspace_clean <- read_csv(here("data/raw_data/greenspace.csv")) %>%
  clean_names() %>%
  filter(measurement == "Percent") %>%
  rename("value_percent" = "value", 
         "area_code" = "feature_code") %>%
  select(-"measurement", -"units") %>%
  filter(!str_detect(area_code, pattern = "S08")) %>% 
  replace_na(list(ca_name =  "Scotland"))

# Add council names to greenspace file
greenspace_council_names <- area_names %>% 
  select(area_code, ca_name) %>% 
  distinct() %>% 
  right_join(greenspace_clean, by = "area_code")

write_csv(greenspace_council_names, 
          here("data/clean_data/greenspace_council_names.csv"))


# Healthy life expectancy -------------------------------------------------

healthy_life_expectancy_clean <- read_csv(here("data/raw_data/healthy_life_expectancy.csv")) %>%
  clean_names() %>%
  filter(measurement == "Count",
         date_code == "2016-2018") %>%
  rename("years_of_quality_life" = "value",
         "area_code" = "feature_code") %>%
  select(-"measurement", -"units")  %>%
  filter(!str_detect(area_code, pattern = "S08")) %>% 
  replace_na(list(ca_name =  "Scotland"))

write_csv(healthy_life_expectancy_clean, 
          here("data/clean_data/healthy_life_expectancy_clean.csv"))

# Life expectancy -------------------------------------------------------

life_expectancy_clean <- read_csv(here("data/raw_data/life_expectancy.csv")) %>%
  clean_names() %>%
  filter(measurement == "Count") %>%
  rename("years_to_live" = "value",
         "area_code" = "feature_code") %>%
  select(-"measurement", -"units")  %>%
  filter(!str_detect(area_code, pattern = "S08")) %>% 
  replace_na(list(ca_name =  "Scotland"))

write_csv(life_expectancy_clean, 
          here("data/clean_data/life_expectancy_clean.csv"))


# Health board codes ------------------------------------------------------

council_codes <- area_names %>%
  select(area_code, hb_name)

health_board_codes <- area_names %>% 
  select(hb, hb_name) %>% 
  rename(area_code = hb)

health_board_names <- bind_rows(council_codes, health_board_codes) %>% 
  distinct()

write_csv(health_board_names, here("data/clean_data/health_board_names.csv"))


# Scottish Health Survey by Local Area ------------------------------------

scotland_health_survey_local <- read_csv(here("data/raw_data/scotland health survey local level.csv")) %>% 
  clean_names()

scotland_health_survey_local <- scotland_health_survey_local %>% 
  rename(area_code = feature_code)

scotland_health_survey_local <- area_names %>% 
  select(area_code, ca_name) %>% 
  distinct() %>% 
  right_join(scotland_health_survey_local, by = "area_code") 

# Select the same health indicators as for the scottish health survey scotland
scotland_health_survey_local_clean <-  scotland_health_survey_local %>% 
  filter(scottish_health_survey_indicator %in% c(
    "Any cardiovascular condition: Has a cardiovascular condition", 
    "Any cardiovascular condition: No cardiovascular condition", 
    "Life satisfaction: Above the mode (9 to 10-Extremely satisfied)", 
    "Life satisfaction: Below the mode (0-Extremely dissatisfied to 7)", 
    "Life satisfaction: Mode (8)", 
    "Obesity: Not obese", 
    "Obesity: Obese", 
    "Overweight: Not overweight or obese", 
    "Overweight: Overweight (including obese)", 
    "Overweight: Overweight (including obese)", 
    "Summary activity levels: Low activity", 
    "Summary activity levels: Meets recommendations", 
    "Summary activity levels: Some activity", 
    "Summary activity levels: Very low activity"),
         measurement == "Percent",
         date_code == "2016-2019") %>% 
  filter(!str_detect(area_code, pattern = "S08")) %>% 
  replace_na(list(ca_name =  "Scotland"))


scotland_health_survey_local_clean <- scotland_health_survey_local_clean %>% 
  rename(year = date_code, 
         percentage = value) %>% 
  select(area_code, ca_name, year, scottish_health_survey_indicator, 
         sex, percentage) %>% 
  arrange(area_code, year, scottish_health_survey_indicator, sex)

write_csv(scotland_health_survey_local_clean, 
          here("data/clean_data/scotland_health_survey_local_clean.csv"))

# Summary statistics ------------------------------------------------------

# Scottish Health Survey by Local Area
raw_scotland_health_survey_local <- read_csv(here(
  "data/raw_data/scotland health survey local level.csv")) %>% 
  clean_names()

summary_stat_scotland_health_2016_2019 <- raw_scotland_health_survey_local %>% 
  filter(
    date_code == "2016-2019",
    str_detect(feature_code, "^S92"),
    scottish_health_survey_indicator %in% c(
      "Any cardiovascular condition: Has a cardiovascular condition",
      "Any cardiovascular condition: No cardiovascular condition",
      "Life satisfaction: Above the mode (9 to 10-Extremely satisfied)",
      "Life satisfaction: Below the mode (0-Extremely dissatisfied to 7)",
      "Life satisfaction: Mode (8)", "Obesity: Not obese", "Obesity: Obese",
      "Overweight: Not overweight or obese", 
      "Overweight: Overweight (including obese)",
      "Overweight: Overweight (including obese)", 
      "Summary activity levels: Low activity",
      "Summary activity levels: Meets recommendations", 
      "Summary activity levels: Some activity",
      "Summary activity levels: Very low activity"),
    measurement == "Percent" | 
      measurement == "95% Lower Confidence Limit" | 
      measurement == "95% Upper Confidence Limit") %>% 
  pivot_wider(names_from = measurement, values_from = value)

summary_stat_scotland_health_2016_2019 <- summary_stat_scotland_health_2016_2019 %>% 
  rename(year = date_code) %>% 
  select(scottish_health_survey_indicator, sex, 
         "Percent", "95% Lower Confidence Limit", "95% Upper Confidence Limit")

write_csv(summary_stat_scotland_health_2016_2019, 
          here("data/clean_data/summary_stat_scotland_health_2016_2019.csv"))

# Statistics table for green space tab

filtered_scotland_health_survey_local_clean <- scotland_health_survey_local_clean %>% 
  filter(str_detect(area_code, "^S120"),
         sex == "All",
         scottish_health_survey_indicator %in% c(
           "Any cardiovascular condition: Has a cardiovascular condition", 
           "Life satisfaction: Below the mode (0-Extremely dissatisfied to 7)", 
           "Obesity: Obese", 
           "Overweight: Overweight (including obese)", 
           "Summary activity levels: Low activity", 
           "Summary activity levels: Very low activity"))

filtered_greenspace <- greenspace_council_names %>%
  filter(
    str_detect(area_code, "^S120"),
    date_code >= 2016,
    distance_to_nearest_green_or_blue_space == "A 5 minute walk or less",
    age == "All",
    gender == "All",
    urban_rural_classification == "All",
    simd_quintiles == "All",
    type_of_tenure == "All",
    household_type == "All",
    ethnicity == "All") %>% 
  group_by(
    area_code, ca_name, age
  ) %>% 
  summarise(mean_percent = mean(value_percent)) %>% 
  ungroup()

# Combine data together for green space summary table
local_greenspace <- filtered_scotland_health_survey_local_clean %>% 
  rename(indicator_percentage = percentage) %>% 
  left_join(filtered_greenspace, by = "area_code") %>% 
  select(-ca_name.x) %>% 
  rename(ca_name = ca_name.y) %>% 
  select(ca_name, scottish_health_survey_indicator, indicator_percentage, 
         mean_percent)

write_csv(local_greenspace, here("data/clean_data/local_greenspace.csv"))
