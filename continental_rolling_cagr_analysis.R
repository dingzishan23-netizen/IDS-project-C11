# ============================================================================
# CONTINENTAL ROLLING CAGR ANALYSIS (1990-2023)
# Calculating rolling CAGR for each continent with simple and weighted averages
# ============================================================================

library(tidyverse)
library(scales)

# ============================================================================
# 1. LOAD DATA
# ============================================================================

# Load datasets
continents <- read_csv("data sets/continents-according-to-our-world-in-data.csv")
gdp <- read_csv("data sets/gdp-per-capita-worldbank.csv")
population <- read_csv("processed_population_un_countries.csv")

# Rename GDP column
colnames(gdp)[4] <- "GDP_per_capita"

# ============================================================================
# 2. PREPARE POPULATION DATA
# ============================================================================

# Get total population by country and year
population_summary <- population %>%
  select(CountryName, Year, TotalPopulation) %>%
  distinct() %>%
  rename(Entity = CountryName)

cat("Population data loaded:", nrow(population_summary), "country-year observations\n")

# ============================================================================
# 3. MERGE DATA
# ============================================================================

# Merge GDP with continents
gdp_continents <- gdp %>%
  left_join(continents %>% select(Code, Continent), by = "Code") %>%
  filter(Continent != "Antarctica", !is.na(Continent)) %>%
  filter(Year >= 1990, Year <= 2023)

# Merge with population data
gdp_complete <- gdp_continents %>%
  left_join(population_summary, by = c("Entity", "Year"))

cat("Data merged. Total observations:", nrow(gdp_complete), "\n")
cat("Countries with population data:", 
    sum(!is.na(gdp_complete$TotalPopulation)) / nrow(gdp_complete) * 100, "%\n\n")

# ============================================================================
# 4. CALCULATE ROLLING CAGR FOR EACH COUNTRY
# ============================================================================

# For each country, use earliest available data point as baseline
# (not necessarily 1990 - could be 1988, 1992, etc.)
rolling_cagr <- gdp_complete %>%
  group_by(Entity, Code, Continent) %>%
  arrange(Year) %>%
  mutate(
    # Get baseline GDP from EARLIEST available year
    Baseline_Year = first(Year),
    Baseline_GDP = first(GDP_per_capita),
    # Calculate years since baseline
    Years_Since_Baseline = Year - Baseline_Year,
    # Calculate rolling CAGR: ((GDP_current / GDP_baseline)^(1/years)) - 1
    Rolling_CAGR = if_else(
      Years_Since_Baseline > 0 & !is.na(Baseline_GDP),
      ((GDP_per_capita / Baseline_GDP)^(1/Years_Since_Baseline) - 1) * 100,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(Rolling_CAGR))

cat("Rolling CAGR calculated for", n_distinct(rolling_cagr$Entity), "countries\n")

# Check baseline year distribution
baseline_summary <- rolling_cagr %>%
  select(Entity, Code, Continent, Baseline_Year) %>%
  distinct() %>%
  group_by(Baseline_Year) %>%
  summarise(N_Countries = n(), .groups = "drop") %>%
  arrange(Baseline_Year)

cat("\nBaseline year distribution:\n")
print(baseline_summary %>% head(10))
cat(sprintf("\nCountries with 1990 baseline: %d (%.1f%%)\n", 
            sum(baseline_summary$N_Countries[baseline_summary$Baseline_Year == 1990]),
            sum(baseline_summary$N_Countries[baseline_summary$Baseline_Year == 1990]) / 
              sum(baseline_summary$N_Countries) * 100))
cat(sprintf("Countries with earlier baseline: %d\n", 
            sum(baseline_summary$N_Countries[baseline_summary$Baseline_Year < 1990])))
cat(sprintf("Countries with later baseline: %d\n", 
            sum(baseline_summary$N_Countries[baseline_summary$Baseline_Year > 1990])))

# Check data availability
availability_check <- rolling_cagr %>%
  group_by(Continent, Year) %>%
  summarise(
    Countries = n_distinct(Entity),
    .groups = "drop"
  )

cat("\nData availability by continent (sample years):\n")
print(availability_check %>% filter(Year %in% c(1990, 2000, 2010, 2015, 2020, 2023)))

# ============================================================================
# 5. CALCULATE SIMPLE AVERAGE (UNWEIGHTED) BY CONTINENT
# ============================================================================

simple_avg <- rolling_cagr %>%
  group_by(Continent, Year) %>%
  summarise(
    Avg_CAGR = mean(Rolling_CAGR, na.rm = TRUE),
    N_Countries = n(),
    .groups = "drop"
  )

cat("\n", strrep("=", 80), "\n")
cat("SIMPLE AVERAGE SUMMARY\n")
cat(strrep("=", 80), "\n")
print(simple_avg %>% filter(Year == 2023))

# ============================================================================
# 6. CALCULATE WEIGHTED AVERAGE BY CONTINENT
# ============================================================================

weighted_avg <- rolling_cagr %>%
  filter(!is.na(TotalPopulation)) %>%
  group_by(Continent, Year) %>%
  summarise(
    # Calculate total continent population for this year
    Total_Continent_Pop = sum(TotalPopulation, na.rm = TRUE),
    # Calculate weighted average CAGR
    Weighted_CAGR = sum(Rolling_CAGR * TotalPopulation, na.rm = TRUE) / Total_Continent_Pop,
    N_Countries_With_Pop = n(),
    .groups = "drop"
  )

cat("\n", strrep("=", 80), "\n")
cat("WEIGHTED AVERAGE SUMMARY\n")
cat(strrep("=", 80), "\n")
print(weighted_avg %>% filter(Year == 2023))

# ============================================================================
# 7. VISUALIZATION 1: SIMPLE AVERAGE (UNWEIGHTED)
# ============================================================================

plot1 <- ggplot(simple_avg, aes(x = Year, y = Avg_CAGR, color = Continent, group = Continent)) +
  # Shaded region for SDG period
  annotate("rect", xmin = 2015, xmax = 2023, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.5) +
  # Vertical line at 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black", linewidth = 0.8) +
  # Add text annotation for 2015
  annotate("text", x = 2015, y = Inf, 
           label = "UN SDG 8\nAdopted", 
           vjust = 1.2, hjust = -0.1, 
           size = 3.5, fontface = "bold") +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
  # Lines for each continent
  geom_line(linewidth = 1.2) +
  geom_point(size = 0.8) +
  # Scales
  scale_x_continuous(breaks = seq(1990, 2023, 5), limits = c(1990, 2023)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set2") +
  # Labels
  labs(
    title = "Continental GDP Per Capita Growth (Simple Average)",
    subtitle = "Rolling CAGR from each country's earliest available baseline (1990-2023)",
    x = "Year",
    y = "Rolling CAGR (%)",
    color = "Continent",
    caption = "Simple average of all countries in each continent.\nBaseline year varies by country (earliest available data point).\nShaded region indicates SDG period (2015-2023)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(plot1)
ggsave("continental_rolling_cagr_simple.png", plot1, width = 12, height = 7, dpi = 300)

# ============================================================================
# 8. VISUALIZATION 2: WEIGHTED AVERAGE (POPULATION-WEIGHTED)
# ============================================================================

plot2 <- ggplot(weighted_avg, aes(x = Year, y = Weighted_CAGR, color = Continent, group = Continent)) +
  # Shaded region for SDG period
  annotate("rect", xmin = 2015, xmax = 2023, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.5) +
  # Vertical line at 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black", linewidth = 0.8) +
  # Add text annotation for 2015
  annotate("text", x = 2015, y = Inf, 
           label = "UN SDG 8\nAdopted", 
           vjust = 1.2, hjust = -0.1, 
           size = 3.5, fontface = "bold") +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
  # Lines for each continent
  geom_line(linewidth = 1.2) +
  geom_point(size = 0.8) +
  # Scales
  scale_x_continuous(breaks = seq(1990, 2023, 5), limits = c(1990, 2023)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set2") +
  # Labels
  labs(
    title = "Continental GDP Per Capita Growth (Population-Weighted)",
    subtitle = "Rolling CAGR from each country's earliest available baseline, weighted by population (1990-2023)",
    x = "Year",
    y = "Rolling CAGR (%)",
    color = "Continent",
    caption = "Weighted by population: larger countries have more influence on continental average.\nBaseline year varies by country (earliest available data point).\nShaded region indicates SDG period (2015-2023)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(plot2)
ggsave("continental_rolling_cagr_weighted.png", plot2, width = 12, height = 7, dpi = 300)

# ============================================================================
# 9. FOCUS ON SDG PERIOD (2015-2023)
# ============================================================================

# Extract 2015 and 2023 values for comparison
cat("\nPreparing SDG period comparison...\n")

# Check if we have data for 2015 and 2023
simple_2015 <- simple_avg %>% filter(Year == 2015)
simple_2023 <- simple_avg %>% filter(Year == 2023)
weighted_2015 <- weighted_avg %>% filter(Year == 2015)
weighted_2023 <- weighted_avg %>% filter(Year == 2023)

cat("Simple average data for 2015:", nrow(simple_2015), "continents\n")
cat("Simple average data for 2023:", nrow(simple_2023), "continents\n")
cat("Weighted average data for 2015:", nrow(weighted_2015), "continents\n")
cat("Weighted average data for 2023:", nrow(weighted_2023), "continents\n")

# Build comparison more carefully
sdg_comparison_simple <- simple_2015 %>%
  rename(CAGR_2015 = Avg_CAGR) %>%
  select(Continent, CAGR_2015) %>%
  left_join(
    simple_2023 %>% 
      rename(CAGR_2023 = Avg_CAGR) %>%
      select(Continent, CAGR_2023),
    by = "Continent"
  ) %>%
  mutate(
    Type = "Simple Average",
    Change = CAGR_2023 - CAGR_2015,
    Trend = case_when(
      Change > 0.5 ~ "Improving",
      Change < -0.5 ~ "Declining",
      TRUE ~ "Stable"
    )
  )

sdg_comparison_weighted <- weighted_2015 %>%
  rename(CAGR_2015 = Weighted_CAGR) %>%
  select(Continent, CAGR_2015) %>%
  left_join(
    weighted_2023 %>% 
      rename(CAGR_2023 = Weighted_CAGR) %>%
      select(Continent, CAGR_2023),
    by = "Continent"
  ) %>%
  mutate(
    Type = "Weighted Average",
    Change = CAGR_2023 - CAGR_2015,
    Trend = case_when(
      Change > 0.5 ~ "Improving",
      Change < -0.5 ~ "Declining",
      TRUE ~ "Stable"
    )
  )

sdg_comparison <- bind_rows(sdg_comparison_simple, sdg_comparison_weighted)

cat("\n", strrep("=", 80), "\n")
cat("SDG PERIOD COMPARISON (2015 vs 2023)\n")
cat(strrep("=", 80), "\n")
print(sdg_comparison)

# ============================================================================
# 10. EXPORT SUMMARY DATA
# ============================================================================

# Export baseline year information for reference
baseline_years <- rolling_cagr %>%
  select(Entity, Code, Continent, Baseline_Year) %>%
  distinct() %>%
  arrange(Continent, Entity)

write_csv(baseline_years, "country_baseline_years.csv")

write_csv(simple_avg, "continental_cagr_simple_average.csv")
write_csv(weighted_avg, "continental_cagr_weighted_average.csv")
write_csv(sdg_comparison, "sdg_period_comparison.csv")

# Export 2015-2023 focused data
sdg_period_simple <- simple_avg %>% filter(Year >= 2015)
sdg_period_weighted <- weighted_avg %>% filter(Year >= 2015)

write_csv(sdg_period_simple, "sdg_period_simple.csv")
write_csv(sdg_period_weighted, "sdg_period_weighted.csv")

# ============================================================================
# 11. SUMMARY STATISTICS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("SUMMARY: CONTINENTAL PERFORMANCE 1990-2023\n")
cat(strrep("=", 80), "\n\n")

cat("ROLLING CAGR in 2023 (from each country's earliest available baseline):\n")
cat(strrep("-", 80), "\n")

summary_2023 <- simple_avg %>%
  filter(Year == 2023) %>%
  arrange(desc(Avg_CAGR))

for(i in 1:nrow(summary_2023)) {
  cat(sprintf("%-15s: %5.2f%% CAGR (simple avg)\n", 
              summary_2023$Continent[i], 
              summary_2023$Avg_CAGR[i]))
}

cat("\n")

summary_2023_weighted <- weighted_avg %>%
  filter(Year == 2023) %>%
  arrange(desc(Weighted_CAGR))

for(i in 1:nrow(summary_2023_weighted)) {
  cat(sprintf("%-15s: %5.2f%% CAGR (weighted avg)\n", 
              summary_2023_weighted$Continent[i], 
              summary_2023_weighted$Weighted_CAGR[i]))
}

cat("\n", strrep("=", 80), "\n")
cat("INTERPRETATION:\n")
cat(strrep("=", 80), "\n")
cat("Rolling CAGR shows the average annual growth rate from each country's\n")
cat("earliest available data point (typically 1990 or nearby years) to each year.\n")
cat("All continents show positive growth over this period, indicating sustained expansion.\n")
cat("The SDG period (2015-2023) is highlighted to assess recent performance.\n")
cat("\nDifferences between simple and weighted averages indicate the influence of\n")
cat("large countries (e.g., China in Asia, India in Asia, USA in North America)\n")
cat("on continental trends.\n")

cat("\n", strrep("=", 80), "\n\n")

cat("Files created:\n")
cat("  - continental_rolling_cagr_simple.png\n")
cat("  - continental_rolling_cagr_weighted.png\n")
cat("  - country_baseline_years.csv (reference: shows which year used as baseline for each country)\n")
cat("  - continental_cagr_simple_average.csv\n")
cat("  - continental_cagr_weighted_average.csv\n")
cat("  - sdg_period_comparison.csv\n")
cat("  - sdg_period_simple.csv\n")
cat("  - sdg_period_weighted.csv\n")

cat("\n🎯 Rolling CAGR analysis complete!\n")
cat("   These visualizations show how each continent's growth trajectory evolved\n")
cat("   from each country's earliest available baseline through the SDG period (2015-2023).\n")
cat("   Using flexible baselines maximizes country coverage while maintaining CAGR consistency.\n")
