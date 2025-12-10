# Load libraries
library(tidyverse)
library(ggplot2)

# Load data
neet <- read_csv("youth-not-in-education-employment-training.csv")
continents <- read_csv("continents-according-to-our-world-in-data.csv")


neet_tidy <- neet %>%
  select(
    country = Entity,
    year    = Year,
    neet_percent = `Share of youth not in education, employment or training, total (% of youth population)`
  )

continents_tidy <- continents %>%
  select(
    country   = Entity,
    continent = Continent
  )

# Add continent info to each NEET observation 
neet_cont <- neet_tidy %>%
  left_join(continents_tidy, by = "country") %>%
  filter(
    !is.na(continent),           # deleting rows w/ missing continent
    continent != "Antarctica",   # remove Antarctica
    !is.na(neet_percent)         # remove missing NEET values
  )

# Calculate average NEET% per continent per year 
continent_neet <- neet_cont %>%
  group_by(continent, year) %>%
  summarise(
    neet_percent = mean(neet_percent, na.rm = TRUE)
  )

# Plot NEET % over time by continent 
ggplot(continent_neet, aes(x = year, y = neet_percent, colour = continent)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("NEET (% of youth 15–24)") +
  ggtitle("Youth NEET rate over time by continent") 

names(neet)

