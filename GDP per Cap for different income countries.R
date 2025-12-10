library(tidyverse)

# Load dataset
gdp <- read.csv("gdp-per-capita-worldbank.csv", check.names = FALSE)

# Identify the GDP column automatically
gdp_col <- names(gdp)[grepl("^GDP", names(gdp))]

# Rename GDP column for convenience
gdp <- gdp %>%
  rename(GDP_per_capita = all_of(gdp_col))

# Income groups of interest
income_groups <- c(
  "High-income countries",
  "Low-income countries",
  "Lower-middle-income countries",
  "Middle-income countries",
  "Upper-middle-income countries"
)

# Filter to income groups
gdp_income <- gdp %>%
  filter(Entity %in% income_groups) %>%
  arrange(Entity, Year)

# ---- CALCULATE GROWTH RATES ----
gdp_growth <- gdp_income %>%
  group_by(Entity) %>%
  mutate(
    Growth_rate = (GDP_per_capita - lag(GDP_per_capita)) / lag(GDP_per_capita)
  ) %>%
  ungroup()

# ---- PLOT THE GROWTH RATES ----
ggplot(gdp_growth, aes(x = Year, y = Growth_rate, 
                       color = Entity, group = Entity)) +
  geom_line(linewidth = 1) +
  labs(
    title = "GDP per Capita Growth Rate of Income Groups Over Time",
    x = "Year",
    y = "Growth Rate (year-over-year)",
    color = "Income Group"
  ) +
  theme_minimal()
