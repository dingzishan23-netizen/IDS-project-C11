# ============================================================================
# ALTERNATIVE ANALYSIS: LDC GDP GROWTH EXCLUDING COVID YEARS
# Period: 2010-2023 (excluding 2020-2021)
# Rationale: Assess LDC performance without COVID-19 disruption
# ============================================================================

library(tidyverse)
library(scales)

# ============================================================================
# 1. LOAD AND PREPARE DATA
# ============================================================================

# Load datasets
continents <- read_csv("continents-according-to-our-world-in-data.csv")
gdp <- read_csv("gdp-per-capita-worldbank.csv")

# Rename GDP column for easier handling
colnames(gdp)[4] <- "GDP_per_capita"

# Official UN LDC list (44 countries as of December 2024)
ldc_codes <- c(
  # Africa (32)
  "AGO", "BEN", "BFA", "BDI", "CAF", "TCD", "COM", "COD", "DJI", "ERI",
  "ETH", "GMB", "GIN", "GNB", "LSO", "LBR", "MDG", "MWI", "MLI", "MRT",
  "MOZ", "NER", "RWA", "SEN", "SLE", "SOM", "SSD", "SDN", "TGO", "UGA",
  "TZA", "ZMB",
  # Asia (8)
  "AFG", "BGD", "KHM", "LAO", "MMR", "NPL", "TLS", "YEM",
  # Caribbean (1)
  "HTI",
  # Pacific (3)
  "KIR", "SLB", "TUV"
)

# Merge and prepare data
gdp_analysis <- gdp %>%
  left_join(continents %>% select(Code, Continent), by = "Code") %>%
  filter(Continent != "Antarctica", !is.na(Continent)) %>%
  mutate(
    LDC_Status = ifelse(Code %in% ldc_codes, "LDC", "Non-LDC")
  )

# ============================================================================
# 2. FILTER DATA: 2010-2023 EXCLUDING 2020-2021
# ============================================================================

# Filter for extended period EXCLUDING COVID years
extended_data_no_covid <- gdp_analysis %>%
  filter(
    Year >= 2010, 
    Year <= 2023,
    !Year %in% c(2020, 2021, 2022)  # EXCLUDE COVID years
  )

cat(strrep("=", 80), "\n")
cat("DATA FILTERING SUMMARY\n")
cat(strrep("=", 80), "\n")
cat("Analysis period: 2010-2023\n")
cat("COVID years excluded: 2020, 2021, 2022\n")
cat("Effective years analyzed: 2010-2019, 2023 (10 years total)\n")
cat(strrep("=", 80), "\n\n")

# ============================================================================
# 3. CALCULATE CAGR FOR EXTENDED PERIOD (NO COVID)
# ============================================================================

# Calculate CAGR using only non-COVID years
gdp_cagr_extended <- extended_data_no_covid %>%
  filter(LDC_Status == "LDC") %>%
  group_by(Entity, Code, Continent) %>%
  arrange(Year) %>%
  filter(n() >= 5) %>%  # Need at least 5 years (higher threshold for longer period)
  summarise(
    Start_Year = first(Year),
    End_Year = last(Year),
    Start_GDP = first(GDP_per_capita),
    End_GDP = last(GDP_per_capita),
    Years_Span = End_Year - Start_Year,
    Data_Points = n(),
    CAGR = ((End_GDP / Start_GDP)^(1/Years_Span) - 1) * 100,
    Achieved_Target = CAGR >= 7,
    .groups = "drop"
  )

cat("LDCs analyzed in extended period (2010-2023, no COVID):", nrow(gdp_cagr_extended), "\n\n")

# ============================================================================
# 4. COMPARE WITH ORIGINAL ANALYSIS (2015-2023 WITH COVID)
# ============================================================================

# Calculate CAGR for original period WITH COVID for comparison
sdg_data_with_covid <- gdp_analysis %>%
  filter(Year >= 2015, Year <= 2023, LDC_Status == "LDC") %>%
  group_by(Entity, Code, Continent) %>%
  arrange(Year) %>%
  filter(n() >= 3) %>%
  summarise(
    Start_Year = first(Year),
    End_Year = last(Year),
    Start_GDP = first(GDP_per_capita),
    End_GDP = last(GDP_per_capita),
    Years_Span = End_Year - Start_Year,
    CAGR_with_COVID = ((End_GDP / Start_GDP)^(1/Years_Span) - 1) * 100,
    .groups = "drop"
  )

# Merge for comparison
comparison_data <- gdp_cagr_extended %>%
  select(Code, Entity, Continent, CAGR_no_COVID = CAGR, Achieved_Target) %>%
  inner_join(
    sdg_data_with_covid %>% select(Code, CAGR_with_COVID),
    by = "Code"
  ) %>%
  mutate(
    CAGR_Difference = CAGR_no_COVID - CAGR_with_COVID,
    Impact = case_when(
      CAGR_Difference > 0.5 ~ "Improved without COVID",
      CAGR_Difference < -0.5 ~ "Worse without COVID years",
      TRUE ~ "Minimal difference"
    )
  )

# ============================================================================
# 5. VISUALIZATION 1: EXTENDED PERIOD ACHIEVEMENT
# ============================================================================

achievement_summary_extended <- gdp_cagr_extended %>%
  summarise(
    Total_LDCs = n(),
    LDCs_Above_7 = sum(Achieved_Target),
    LDCs_Below_7 = sum(!Achieved_Target),
    Percent_Above_7 = (LDCs_Above_7 / Total_LDCs) * 100
  )

achievement_data_extended <- data.frame(
  Category = c("Achieved 7%+", "Below 7%"),
  Count = c(achievement_summary_extended$LDCs_Above_7, achievement_summary_extended$LDCs_Below_7),
  Percentage = c(achievement_summary_extended$Percent_Above_7, 100 - achievement_summary_extended$Percent_Above_7)
)

plot1_extended <- ggplot(achievement_data_extended, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Achieved 7%+" = "#2ecc71", "Below 7%" = "#e74c3c")) +
  labs(
    title = "LDC Achievement of 7% GDP Growth Target",
    subtitle = paste0("Extended Analysis: 2010-2023 (excluding COVID years 2020-2022)\n",
                      "Analysis of ", achievement_summary_extended$Total_LDCs, " LDCs with sufficient data"),
    x = NULL,
    y = "Number of Countries",
    caption = "Source: World Bank GDP data, UN LDC Classification\nCOVID years (2020-2022) excluded to isolate structural growth patterns"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12)
  ) +
  ylim(0, max(achievement_data_extended$Count) * 1.2)

print(plot1_extended)
ggsave("plot1_extended_no_covid.png", plot1_extended, width = 10, height = 6, dpi = 300)

# ============================================================================
# 6. VISUALIZATION 2: COMPARISON - WITH vs WITHOUT COVID
# ============================================================================

# Summary statistics for comparison
comparison_summary <- data.frame(
  Period = c("2015-2023\n(With COVID)", "2010-2023\n(No COVID)"),
  Mean_Growth = c(
    mean(sdg_data_with_covid$CAGR_with_COVID, na.rm = TRUE),
    mean(gdp_cagr_extended$CAGR, na.rm = TRUE)
  ),
  Percent_Above_7 = c(
    sum(sdg_data_with_covid$CAGR_with_COVID >= 7) / nrow(sdg_data_with_covid) * 100,
    achievement_summary_extended$Percent_Above_7
  )
)

plot2_comparison <- ggplot(comparison_summary, aes(x = Period, y = Percent_Above_7, fill = Period)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Percent_Above_7, 1), "%")),
            vjust = -0.5, size = 6, fontface = "bold") +
  geom_text(aes(label = paste0("Avg: ", round(Mean_Growth, 2), "%")),
            vjust = 1.5, size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("2015-2023\n(With COVID)" = "#e74c3c", 
                               "2010-2023\n(No COVID)" = "#3498db")) +
  labs(
    title = "Impact of Excluding COVID Years on LDC Performance Assessment",
    subtitle = "Comparison: % of LDCs achieving 7% growth target",
    x = "Analysis Period",
    y = "% of LDCs Achieving 7% Target",
    caption = "Extended period (2010-2023) excludes years 2020-2022"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  ) +
  ylim(0, 100)

print(plot2_comparison)
ggsave("plot2_covid_impact_comparison.png", plot2_comparison, width = 10, height = 6, dpi = 300)

# ============================================================================
# 7. VISUALIZATION 3: DISTRIBUTION COMPARISON
# ============================================================================

# Combine data for comparison
distribution_data <- bind_rows(
  gdp_cagr_extended %>% mutate(Analysis = "2010-2023 (No COVID)"),
  sdg_data_with_covid %>% 
    mutate(CAGR = CAGR_with_COVID, Achieved_Target = CAGR >= 7, Analysis = "2015-2023 (With COVID)") %>%
    select(Entity, Code, CAGR, Achieved_Target, Analysis)
)

plot3_distribution <- ggplot(distribution_data, aes(x = CAGR, fill = Analysis)) +
  geom_histogram(alpha = 0.6, bins = 25, position = "identity") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 7, y = Inf, label = "7% Target", 
           vjust = 1.5, hjust = 0.5, color = "red", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("2015-2023 (With COVID)" = "#e74c3c", 
                               "2010-2023 (No COVID)" = "#3498db")) +
  labs(
    title = "Growth Rate Distribution: Impact of COVID Year Exclusion",
    subtitle = "Comparing CAGR distributions with and without COVID-affected years",
    x = "Compound Annual Growth Rate (%)",
    y = "Number of LDCs",
    fill = "Analysis Period"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot3_distribution)
ggsave("plot3_distribution_comparison.png", plot3_distribution, width = 12, height = 7, dpi = 300)

# ============================================================================
# 8. VISUALIZATION 4: INDIVIDUAL COUNTRY IMPACT
# ============================================================================

# Top 5 most positively impacted and top 5 most negatively impacted
top_5_positive <- comparison_data %>%
  arrange(desc(CAGR_Difference)) %>%
  head(5)

top_5_negative <- comparison_data %>%
  arrange(CAGR_Difference) %>%
  head(5)

most_affected <- bind_rows(top_5_positive, top_5_negative) %>%
  mutate(Entity = factor(Entity, levels = Entity[order(CAGR_Difference)]))

plot4_impact <- ggplot(most_affected, aes(x = CAGR_Difference, y = Entity, fill = Impact)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c(
    "Improved without COVID" = "#2ecc71",
    "Worse without COVID years" = "#e74c3c",
    "Minimal difference" = "#95a5a6"
  )) +
  labs(
    title = "Country-Specific Impact of Excluding COVID Years",
    subtitle = "Top 5 most positively and top 5 most negatively impacted LDCs",
    x = "Change in CAGR (percentage points)",
    y = NULL,
    fill = "Impact Category",
    caption = "Positive = Higher growth without COVID disruption\nNegative = Paradoxically lower growth (possible measurement/recovery effects)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot4_impact)
ggsave("plot4_country_impact.png", plot4_impact, width = 12, height = 8, dpi = 300)

# ============================================================================
# 9. VISUALIZATION 5: TOP PERFORMERS - EXTENDED PERIOD
# ============================================================================

top_10_extended <- gdp_cagr_extended %>%
  arrange(desc(CAGR)) %>%
  head(10) %>%
  mutate(Entity = factor(Entity, levels = Entity[order(CAGR)]))

plot5_top10 <- ggplot(top_10_extended, aes(x = CAGR, y = Entity, fill = Achieved_Target)) +
  geom_col() +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red", linewidth = 0.7) +
  annotate("text", x = 5, y = 5.5, label = "7% Target", 
           vjust = 0, hjust = 0.5, color = "red", fontface = "bold", size = 3.5) +
  scale_fill_manual(
    values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
    labels = c("TRUE" = "Achieved", "FALSE" = "Not Achieved")
  ) +
  labs(
    title = "Top 10 LDC Performers (2010-2023, Excluding COVID Years)",
    subtitle = "Countries with highest CAGR over extended period (COVID years 2020-2022 excluded)",
    x = "Compound Annual Growth Rate (%)",
    y = NULL,
    fill = "Target Achievement"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot5_top10)
ggsave("plot5_top10_extended.png", plot5_top10, width = 10, height = 6, dpi = 300)

# ============================================================================
# 10. SUMMARY STATISTICS AND INSIGHTS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("COMPARATIVE ANALYSIS SUMMARY\n")
cat(strrep("=", 80), "\n\n")

cat("PERIOD 1: 2015-2023 (INCLUDING COVID YEARS 2020-2021)\n")
cat("  LDCs analyzed:", nrow(sdg_data_with_covid), "\n")
cat("  Mean CAGR:", round(mean(sdg_data_with_covid$CAGR_with_COVID, na.rm = TRUE), 2), "%\n")
cat("  Median CAGR:", round(median(sdg_data_with_covid$CAGR_with_COVID, na.rm = TRUE), 2), "%\n")
cat("  LDCs achieving 7%+:", sum(sdg_data_with_covid$CAGR_with_COVID >= 7), 
    sprintf("(%.1f%%)\n\n", sum(sdg_data_with_covid$CAGR_with_COVID >= 7) / nrow(sdg_data_with_covid) * 100))

cat("PERIOD 2: 2010-2023 (EXCLUDING COVID YEARS 2020-2022)\n")
cat("  LDCs analyzed:", nrow(gdp_cagr_extended), "\n")
cat("  Mean CAGR:", round(mean(gdp_cagr_extended$CAGR, na.rm = TRUE), 2), "%\n")
cat("  Median CAGR:", round(median(gdp_cagr_extended$CAGR, na.rm = TRUE), 2), "%\n")
cat("  LDCs achieving 7%+:", sum(gdp_cagr_extended$Achieved_Target), 
    sprintf("(%.1f%%)\n\n", achievement_summary_extended$Percent_Above_7))

cat("IMPACT OF EXCLUDING COVID YEARS:\n")
mean_diff <- mean(gdp_cagr_extended$CAGR, na.rm = TRUE) - mean(sdg_data_with_covid$CAGR_with_COVID, na.rm = TRUE)
pct_diff <- achievement_summary_extended$Percent_Above_7 - 
  (sum(sdg_data_with_covid$CAGR_with_COVID >= 7) / nrow(sdg_data_with_covid) * 100)

cat("  Change in mean CAGR:", round(mean_diff, 2), "percentage points\n")
cat("  Change in % achieving target:", round(pct_diff, 1), "percentage points\n\n")

if (mean_diff > 0) {
  cat("INTERPRETATION: Excluding COVID years INCREASES average LDC growth rates,\n")
  cat("suggesting the pandemic had a net negative impact on LDC economic growth.\n\n")
} else {
  cat("INTERPRETATION: Excluding COVID years DECREASES average LDC growth rates,\n")
  cat("suggesting either: (a) strong recovery effects in 2022-2023, or\n")
  cat("(b) the extended period (2010-2023) captures slower pre-SDG growth.\n\n")
}

cat("COUNTRY-LEVEL IMPACTS:\n")
cat("  Countries improved without COVID:", sum(comparison_data$Impact == "Improved without COVID"), "\n")
cat("  Countries worse without COVID:", sum(comparison_data$Impact == "Worse without COVID years"), "\n")
cat("  Minimal difference:", sum(comparison_data$Impact == "Minimal difference"), "\n\n")

cat("KEY INSIGHTS:\n")
cat("1. Extended period (2010-2023) provides longer-term growth perspective\n")
cat("2. Excluding COVID years (2020-2022) isolates structural growth from pandemic disruption\n")
cat("3. Comparison reveals which countries were most affected by COVID-19\n")
cat("4. Both analyses inform understanding of SDG 8.1 target feasibility\n")

cat(strrep("=", 80), "\n\n")

# ============================================================================
# 11. EXPORT DATA
# ============================================================================

write_csv(gdp_cagr_extended, "ldc_cagr_extended_no_covid.csv")
write_csv(comparison_data, "ldc_covid_impact_comparison.csv")
write_csv(comparison_summary, "summary_comparison.csv")

cat("Analysis complete!\n")
cat("\nFiles created:\n")
cat("  - plot1_extended_no_covid.png\n")
cat("  - plot2_covid_impact_comparison.png\n")
cat("  - plot3_distribution_comparison.png\n")
cat("  - plot4_country_impact.png\n")
cat("  - plot5_top10_extended.png\n")
cat("  - ldc_cagr_extended_no_covid.csv\n")
cat("  - ldc_covid_impact_comparison.csv\n")
cat("  - summary_comparison.csv\n")

cat("\n🎯 This alternative analysis shows how COVID-19 affected LDC growth assessments\n")
cat("   and provides a longer-term perspective (2010-2023, excluding 2020-2022) on LDC performance.\n")