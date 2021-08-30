#load libraries
library(tidyverse)
library(janitor)
library(treemapify)
library(colorspace)
library(shiny)
library(shinydashboard)
library(dashboardthemes)

#load data
#use read.csv due to parsing errors with read_csv
lfb_animals <- read.csv("data/Animal Rescue incidents attended by LFB from Jan 2009.csv",
                            fileEncoding = "latin1") %>% 
  clean_names()

unknown_str <- c("Unknown - animal rescue from below ground - farm animal", 
                 "Unknown - animal rescue from water - farm animal",
                 "Unknown - heavy livestock animal",
                 "Unknown - domestic animal or pet")

#recode mistyped animals, regroup animal classifications
lfb_animals <- lfb_animals %>% 
mutate(animal = str_to_sentence(animal_group_parent),
       animal = str_replace(animal, "Budgie", "Bird"),
       animal = str_replace(animal, "^Unknown -*", "Unknown"))



# plots -------------------------------------------------------------------

#treemap for animal types
lfb_animals %>%
  group_by(animal) %>% 
  filter(cal_year == 2020) %>% 
  summarise(count_animal = sum(n())) %>% 
  ggplot() +
  aes(area = count_animal, label = animal, fill = animal) +
  geom_treemap() +
  scale_fill_discrete_qualitative(palette = "Dynamic") +
  geom_treemap_text(place = "middle", colour = "white", grow = TRUE)

