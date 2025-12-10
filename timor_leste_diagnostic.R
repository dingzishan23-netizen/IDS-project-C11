# ============================================================================
# DIAGNOSTIC ANALYSIS: TIMOR-LESTE (EAST TIMOR) GDP DURING COVID
# Investigating why COVID inclusion changes target achievement
# ============================================================================

library(tidyverse)
library(scales)

# Load datasets
continents <- read_csv("continents-according-to-our-world-in-data.csv")
gdp <- read_csv("GDP-per-capita-worldbank.csv")
colnames(gdp)[4] <- "gdp_per_capita"

# ============================================================================
# 1. EXTRACT TIMOR-LESTE DATA
# ============================================================================

timor_data <- gdp %>%
  filter(Code == "TLS") %>%
  arrange(Year)

cat("TIMOR-LESTE (EAST TIMOR) GDP ANALYSIS\n")
cat(strrep("=", 80), "\n\n")

# ============================================================================
# 2. YEAR-BY-YEAR GDP AND GROWTH RATES
# ============================================================================

timor_analysis <- timor_data %>%
  arrange(Year) %>%
  mutate(
    Annual_Growth_Rate = (GDP_per_capita - lag(GDP_per_capita)) / lag(GDP_per_capita) * 100,
    Period = case_when(
      Year %in% c(2020, 2021, 2022) ~ "COVID Years",
      Year >= 2015 & Year <= 2019 ~ "Pre-COVID (2015-2019)",
      Year >= 2023 ~ "Post-COVID (2023+)",
      Year >= 2010 & Year <= 2014 ~ "Pre-SDG (2010-2014)",
      TRUE ~ "Other"
    )
  )

cat("YEAR-BY-YEAR GDP PER CAPITA AND GROWTH RATES:\n")
cat(strrep("-", 80), "\n")
timor_recent <- timor_analysis %>%
  filter(Year >= 2010) %>%
  select(Year, GDP_per_capita, Annual_Growth_Rate, Period)

print(timor_recent, n = Inf)
cat("\n")

# ============================================================================
# 3. CALCULATE CAGR WITH AND WITHOUT COVID
# ============================================================================

# WITH COVID (2015-2023)
timor_with_covid <- timor_data %>%
  filter(Year >= 2015, Year <= 2023) %>%
  arrange(Year) %>%
  summarise(
    Start_Year = first(Year),
    End_Year = last(Year),
    Start_GDP = first(GDP_per_capita),
    End_GDP = last(GDP_per_capita),
    Years_Span = End_Year - Start_Year,
    CAGR = ((End_GDP / Start_GDP)^(1/Years_Span) - 1) * 100
  )

# WITHOUT COVID (2010-2023, excluding 2020-2022)
timor_no_covid <- timor_data %>%
  filter(Year >= 2010, Year <= 2023, !Year %in% c(2020, 2021, 2022)) %>%
  arrange(Year) %>%
  summarise(
    Start_Year = first(Year),
    End_Year = last(Year),
    Start_GDP = first(GDP_per_capita),
    End_GDP = last(GDP_per_capita),
    Years_Span = End_Year - Start_Year,
    CAGR = ((End_GDP / Start_GDP)^(1/Years_Span) - 1) * 100
  )

cat("COMPOUND ANNUAL GROWTH RATE (CAGR) COMPARISON:\n")
cat(strrep("-", 80), "\n")
cat(sprintf("WITH COVID (2015-2023):\n"))
cat(sprintf("  Start: %d (GDP: $%.2f)\n", timor_with_covid$Start_Year, timor_with_covid$Start_GDP))
cat(sprintf("  End: %d (GDP: $%.2f)\n", timor_with_covid$End_Year, timor_with_covid$End_GDP))
cat(sprintf("  CAGR: %.2f%%\n", timor_with_covid$CAGR))
cat(sprintf("  TARGET ACHIEVED: %s\n\n", ifelse(timor_with_covid$CAGR >= 7, "YES ✓", "NO ✗")))

cat(sprintf("WITHOUT COVID (2010-2023, excluding 2020-2022):\n"))
cat(sprintf("  Start: %d (GDP: $%.2f)\n", timor_no_covid$Start_Year, timor_no_covid$Start_GDP))
cat(sprintf("  End: %d (GDP: $%.2f)\n", timor_no_covid$End_Year, timor_no_covid$End_GDP))
cat(sprintf("  CAGR: %.2f%%\n", timor_no_covid$CAGR))
cat(sprintf("  TARGET ACHIEVED: %s\n\n", ifelse(timor_no_covid$CAGR >= 7, "YES ✓", "NO ✗")))

cat(sprintf("IMPACT OF COVID INCLUSION:\n"))
cat(sprintf("  CAGR Difference: %.2f percentage points\n", timor_with_covid$CAGR - timor_no_covid$CAGR))
cat("\n")

# ============================================================================
# 4. WHAT HAPPENED IN COVID YEARS?
# ============================================================================

covid_years <- timor_analysis %>%
  filter(Year %in% c(2019, 2020, 2021, 2022, 2023))

cat("WHAT HAPPENED DURING COVID YEARS?\n")
cat(strrep("-", 80), "\n")
print(covid_years %>% select(Year, GDP_per_capita, Annual_Growth_Rate))
cat("\n")

if (nrow(covid_years %>% filter(Year == 2020)) > 0) {
  growth_2020 <- covid_years %>% filter(Year == 2020) %>% pull(Annual_Growth_Rate)
  cat(sprintf("2020 Growth Rate: %.2f%%\n", growth_2020))
  if (growth_2020 < 0) {
    cat("  → GDP DECLINED in 2020 (negative growth)\n")
  } else if (growth_2020 < 7) {
    cat("  → GDP grew but BELOW the 7% target\n")
  } else {
    cat("  → GDP grew ABOVE the 7% target\n")
  }
}

if (nrow(covid_years %>% filter(Year == 2021)) > 0) {
  growth_2021 <- covid_years %>% filter(Year == 2021) %>% pull(Annual_Growth_Rate)
  cat(sprintf("2021 Growth Rate: %.2f%%\n", growth_2021))
  if (growth_2021 < 0) {
    cat("  → GDP DECLINED in 2021 (negative growth)\n")
  } else if (growth_2021 < 7) {
    cat("  → GDP grew but BELOW the 7% target\n")
  } else {
    cat("  → GDP grew ABOVE the 7% target\n")
  }
}

if (nrow(covid_years %>% filter(Year == 2022)) > 0) {
  growth_2022 <- covid_years %>% filter(Year == 2022) %>% pull(Annual_Growth_Rate)
  cat(sprintf("2022 Growth Rate: %.2f%%\n", growth_2022))
  if (growth_2022 < 0) {
    cat("  → GDP DECLINED in 2022 (negative growth)\n")
  } else if (growth_2022 < 7) {
    cat("  → GDP grew but BELOW the 7% target\n")
  } else {
    cat("  → GDP grew ABOVE the 7% target\n")
  }
}
cat("\n")

# ============================================================================
# 5. VISUALIZATION: GDP TRAJECTORY
# ============================================================================

plot1 <- ggplot(timor_analysis %>% filter(Year >= 2010), 
                aes(x = Year, y = GDP_per_capita, color = Period, group = 1)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "red") +
  annotate("text", x = 2021, y = Inf, label = "COVID\nYears", 
           vjust = 1.5, color = "red", fontface = "bold", size = 3.5) +
  scale_color_manual(values = c(
    "Pre-SDG (2010-2014)" = "gray60",
    "Pre-COVID (2015-2019)" = "#3498db",
    "COVID Years" = "#e74c3c",
    "Post-COVID (2023+)" = "#2ecc71"
  )) +
  labs(
    title = "Timor-Leste GDP Per Capita Trajectory (2010-2023)",
    subtitle = "Showing impact of COVID years on economic growth",
    x = "Year",
    y = "GDP per capita (2017 USD PPP)",
    color = "Period",
    caption = "Red shaded area indicates COVID-affected years (2020-2022)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

print(plot1)
ggsave("timor_gdp_trajectory.png", plot1, width = 12, height = 7, dpi = 300)

# ============================================================================
# 6. VISUALIZATION: ANNUAL GROWTH RATES
# ============================================================================

plot2 <- ggplot(timor_analysis %>% filter(Year >= 2010, !is.na(Annual_Growth_Rate)), 
                aes(x = Year, y = Annual_Growth_Rate)) +
  geom_col(aes(fill = Annual_Growth_Rate >= 7), width = 0.7) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "red") +
  annotate("text", x = 2021, y = Inf, label = "COVID", 
           vjust = 1.5, color = "red", fontface = "bold") +
  annotate("text", x = 2015, y = 7, label = "7% Target", 
           vjust = -0.5, hjust = 0, color = "darkgreen", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                   labels = c("TRUE" = "Above 7%", "FALSE" = "Below 7%")) +
  labs(
    title = "Timor-Leste Annual GDP Growth Rates (2010-2023)",
    subtitle = "Year-by-year growth showing COVID impact",
    x = "Year",
    y = "Annual Growth Rate (%)",
    fill = "Target Achievement",
    caption = "Dashed line indicates 7% SDG target"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

print(plot2)
ggsave("timor_annual_growth.png", plot2, width = 12, height = 7, dpi = 300)

# ============================================================================
# 7. SUMMARY AND INTERPRETATION
# ============================================================================

cat(strrep("=", 80), "\n")
cat("SUMMARY: WHY COVID INCLUSION MATTERS FOR TIMOR-LESTE\n")
cat(strrep("=", 80), "\n\n")

avg_growth_pre_covid <- mean(timor_analysis %>% 
                              filter(Year >= 2015, Year <= 2019) %>% 
                              pull(Annual_Growth_Rate), na.rm = TRUE)

avg_growth_covid <- mean(timor_analysis %>% 
                         filter(Year %in% c(2020, 2021, 2022)) %>% 
                         pull(Annual_Growth_Rate), na.rm = TRUE)

avg_growth_post_covid <- mean(timor_analysis %>% 
                               filter(Year >= 2023) %>% 
                               pull(Annual_Growth_Rate), na.rm = TRUE)

cat(sprintf("Average growth 2015-2019 (pre-COVID): %.2f%%\n", avg_growth_pre_covid))
cat(sprintf("Average growth 2020-2022 (COVID): %.2f%%\n", avg_growth_covid))
cat(sprintf("Average growth 2023+ (post-COVID): %.2f%%\n\n", avg_growth_post_covid))

cat("INTERPRETATION:\n")
if (avg_growth_covid < avg_growth_pre_covid) {
  cat("→ COVID years had LOWER growth than pre-COVID period\n")
  cat("→ Including these years DRAGS DOWN the overall CAGR\n")
  cat("→ This caused Timor-Leste to MISS the 7% target when COVID is included\n")
} else {
  cat("→ COVID years had HIGHER or similar growth to pre-COVID\n")
  cat("→ Other factors explain the CAGR difference\n")
}

cat("\nMETHODOLOGICAL NOTE:\n")
cat("CAGR is calculated as: (End_GDP/Start_GDP)^(1/Years) - 1\n")
cat("Including COVID years affects:\n")
cat("  1. The end GDP value (if 2020-2022 had low/high growth)\n")
cat("  2. The time span over which growth is averaged\n")
cat("  3. The starting point (2010 vs 2015)\n")

cat("\nFOR YOUR REPORT:\n")
cat("This case study demonstrates why excluding COVID years provides a clearer\n")
cat("picture of structural growth trends vs pandemic-disrupted growth.\n")

cat(strrep("=", 80), "\n")

# Export data
write_csv(timor_analysis %>% filter(Year >= 2010), "timor_leste_detailed_analysis.csv")

cat("\nFiles created:\n")
cat("  - timor_gdp_trajectory.png\n")
cat("  - timor_annual_growth.png\n")
cat("  - timor_leste_detailed_analysis.csv\n")
