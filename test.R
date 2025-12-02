#test file 
library(tidyverse)
library(ggplot2)
library(janitor)

setwd("/Users/lizhen/Desktop/ICY1/Data Science /project")
continents <- read_csv("continents-according-to-our-world-in-data.csv") %>%
  clean_names() %>% rename(country = entity, iso3 = code, continent = continent) %>%
  filter(continent != "Antarctica")