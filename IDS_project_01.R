library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(stringr)
library(scales)

#Load 3 required CSVs and Clean the data 
setwd("/Users/lizhen/Desktop/ICY1/Data Science/project")
continents <- read_csv("continents-according-to-our-world-in-data.csv") %>%
  clean_names() %>% rename(country = entity, iso3 = code, continent = continent) %>%
  filter(continent != "Antarctica")

gdp_pc <- read_csv("gdp-per-capita-worldbank.csv") %>%
  clean_names() %>%
  rename(country = entity, iso3 = code, year = year,
         gdp_pc_ppp_2017usd = gdp_per_capita_ppp_constant_2017_international) %>%
  mutate(year = as.integer(year))

neet <- read_csv("youth-not-in-education-employment-training.csv")  %>%
  clean_names() %>%
  rename(country = entity, iso3 = code, year = year,
         neet_pct = share_of_youth_not_in_education_employment_or_training_total_percent_of_youth_population) %>%
  mutate(year = as.integer(year))

#The additional dataset 
total_pop <- read_csv("WPP2024_TotalPopulationBySex.csv")
View(total_pop)
clean_pop <- total_pop %>%
  filter(Time >= 1970 & Time <= 2023) %>%
  select(Location,Time, PopTotal,ISO3_code) %>%
  rename(country = Location, population = PopTotal, year = Time, iso3 = ISO3_code) %>%
  filter(!is.na(iso3)) %>%
  mutate(year = as.integer(year))
View(clean_pop)

pop_by_age <- read_csv("WPP2024_Population1JanuaryByAge5GroupSex_Medium.csv")
clean_pop_by_age <- pop_by_age %>%
  filter(Time >= 1970 & Time <= 2023) %>%
  select(Location, AgeGrp, Time, PopTotal,ISO3_code) %>%
  rename(country = Location, population = PopTotal, year = Time, age = AgeGrp, iso3 = ISO3_code) %>%
  filter(!is.na(iso3)) %>%
  filter(age %in% c("15-19","20-24")) %>%
  group_by(country, year, iso3) %>%
  summarise(population_15_24 = sum(population, na.rm=TRUE),
            .groups = "drop"
  ) %>%
  mutate(year = as.integer(year))

#combining dataset 
base <- continents %>% select(country, iso3, continent)

gdp_pc2 <- gdp_pc %>% inner_join(base, by = c("iso3","country"))
neet2   <- neet   %>% inner_join(base, by = c("iso3","country"))

panel <- gdp_pc2 %>%
  full_join(neet2, by = c("country","iso3","continent","year")) %>%
  { if (!is.null(clean_pop)) left_join(., clean_pop %>% select(iso3, year, population), by = c("iso3","year")) else . } %>%
  { if (!is.null(clean_pop_by_age)) left_join(., clean_pop_by_age %>% select(iso3, year, population_15_24), by = c("iso3","year")) else . } %>%
  filter(year >= 1990, year <= 2024) %>%
  arrange(country, year)


# Country-level GDP per capita growth
panel <- panel %>%
  group_by(country) %>%
  arrange(year,.by_group = TRUE) %>%
  mutate(gdp_pc_growth_pct = (gdp_pc_ppp_2017usd - dplyr::lag(gdp_pc_ppp_2017usd)) / dplyr::lag(gdp_pc_ppp_2017usd) * 100) %>%
  ungroup()

# Weights (if population data is available)
panel <- panel %>%
  group_by(continent, year) %>%
  mutate(
    w_pop_total = if (!is.null(population)) if_else(is.na(population), 0, population / sum(population, na.rm = TRUE)) else NA_real_,
    w_pop_youth = if (!is.null(population_15_24)) if_else(is.na(population_15_24), 0, population_15_24 / sum(population_15_24, na.rm = TRUE)) else NA_real_
  ) %>%
  ungroup()

# Weighted continent averages
# Weighted GDP per capita growth (weights = total population)
gpc_cont_weighted <- panel %>%
  filter(!is.na(gdp_pc_growth_pct), !is.na(population)) %>%
  group_by(continent, year) %>%
  summarise(
    denom = sum(population, na.rm = TRUE),
    gdp_pc_growth_pct_weighted = ifelse(
      denom > 0,
      sum(population * gdp_pc_growth_pct, na.rm = TRUE) / denom,
      NA_real_
    ),.groups = "drop"
  ) %>%
  select(-denom)
View(gpc_cont_weighted)

# Weighted NEET (weights = youth population 15–24)
neet_cont_weighted <- panel %>%
  filter(!is.na(neet_pct), !is.na(population_15_24)) %>%
  group_by(continent, year) %>%
  summarise(
    denom = sum(population_15_24, na.rm = TRUE),
    neet_pct_weighted = ifelse(
      denom > 0,
      sum(population_15_24 * neet_pct, na.rm = TRUE) / denom,
      NA_real_
    ),.groups = "drop"
  ) %>%
  select(-denom)

# simple continent average 
gpc_cont_mean <- panel %>%
  filter(!is.na(gdp_pc_growth_pct)) %>%
  group_by(continent, year) %>%
  summarise(
    gdp_pc_growth_pct_mean = mean(gdp_pc_growth_pct, na.rm = TRUE),.groups = "drop"
  )

neet_cont_mean <- panel %>%
  filter(!is.na(neet_pct)) %>%
  group_by(continent, year) %>%
  summarise(
    neet_pct_mean = mean(neet_pct, na.rm = TRUE),.groups = "drop"
  )

# Plots - Weighted 
win_start <- 2015
win_end   <- 2023
gpc_w_15123 <- gpc_cont_weighted %>% filter(year >= win_start, year <= win_end)
gpc_m_15123 <- gpc_cont_mean     %>% filter(year >= win_start, year <= win_end)
neet_w_15123 <- neet_cont_weighted %>% filter(year >= win_start, year <= win_end)
neet_m_15123 <- neet_cont_mean     %>% filter(year >= win_start, year <= win_end)

p_w_gdp <- ggplot(gpc_w_15123, aes(x = year, y = gdp_pc_growth_pct_weighted, color = continent)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Weighted GDP per Capita Growth by Continent",
    x = "Year", y = "Growth (%)", color = "Continent"
  ) +
  theme_minimal(base_size = 13)
ggsave("output/figures/gdp_pc_growth_by_continent_weighted.png", p_w_gdp, width = 10, height = 6)

p_w_neet <- ggplot(neet_w_15123, aes(x = year, y = neet_pct_weighted, color = continent)) +
  geom_line(size = 1) +
  labs(
    title = "Weighted NEET (15–24) by Continent",
    x = "Year", y = "NEET (%)", color = "Continent"
  ) +
  theme_minimal(base_size = 13)
ggsave("output/figures/neet_by_continent_weighted.png", p_w_neet, width = 10, height = 6)

# Plots — Simple means 
p_m_gdp <- ggplot(gpc_m_15123, aes(x = year, y = gdp_pc_growth_pct_mean, color = continent)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "GDP per Capita Growth by Continent (Simple Average)",
    x = "Year", y = "Growth (%)", color = "Continent"
  ) +
  theme_minimal(base_size = 13)
ggsave("output/figures/gdp_pc_growth_by_continent_simple_mean.png", p_m_gdp, width = 10, height = 6)

p_m_neet <- ggplot(neet_m_15123, aes(x = year, y = neet_pct_mean, color = continent)) +
  geom_line(size = 1) +
  labs(
    title = "NEET (15–24) by Continent (Simple Average)",
    x = "Year", y = "NEET (%)", color = "Continent"
  ) +
  theme_minimal(base_size = 13)
ggsave("output/figures/neet_by_continent_simple_mean.png", p_m_neet, width = 10, height = 6)

# Check
message("Weighted GDP rows: ", nrow(gpc_cont_weighted))
message("Weighted NEET rows: ", nrow(neet_cont_weighted))
message("Simple-mean GDP rows: ", nrow(gpc_cont_mean))
message("Simple-mean NEET rows: ", nrow(neet_cont_mean))

#NEET change (baseline year:2010)
baseline_year <- 2010
# Weighted change
neet_2020_w <- neet_cont_weighted %>% filter(year == 2020) %>% select(continent, neet_2020_w = neet_pct_weighted)
neet_base_w <- neet_cont_weighted %>% filter(year == baseline_year) %>% select(continent, neet_base_w = neet_pct_weighted)
neet_change_w <- neet_2020_w %>%
  left_join(neet_base_w, by = "continent") %>%
  mutate(neet_change_pp_weighted = neet_2020_w - neet_base_w)
neet_change_w

# Simple (unweighted) change
neet_2020_m <- neet_cont_mean %>% filter(year == 2020) %>% select(continent, neet_2020_m = neet_pct_mean)
neet_base_m <- neet_cont_mean %>% filter(year == baseline_year) %>% select(continent, neet_base_m = neet_pct_mean)
neet_change_m <- neet_2020_m %>%
  left_join(neet_base_m, by = "continent") %>%
  mutate(neet_change_pp_mean = neet_2020_m - neet_base_m)

p_bar_w <- ggplot(neet_change_w, aes(x = continent, y = neet_change_pp_weighted, fill = continent)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = " pp")) +
  labs(
    title = paste0("Weighted ΔNEET (", baseline_year, " → 2020) by Continent"),
    subtitle = "Weights: youth population (15–24); negative values indicate improvement",
    x = "Continent", y = "Change (percentage points)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
p_bar_w
ggsave("output/figures/neet_change_weighted_bar.png", p_bar_w, width = 9, height = 6)

p_bar_m <- ggplot(neet_change_m, aes(x = continent, y = neet_change_pp_mean, fill = continent)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = " pp")) +
  labs(
    title = paste0("ΔNEET (", baseline_year, " → 2020) by Continent — Simple Average"),
    subtitle = "Unweighted mean across countries; negative values indicate improvement",
    x = "Continent", y = "Change (percentage points)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
ggsave("output/figures/neet_change_simple_mean_bar.png", p_bar_m, width = 9, height = 6)

