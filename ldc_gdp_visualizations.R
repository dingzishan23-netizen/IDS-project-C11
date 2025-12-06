# ============================================================================
# VISUALIZING LDC ACHIEVEMENT OF 7% GDP GROWTH TARGET
# SDG 8.1: At least 7% GDP growth per annum in least developed countries
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
  "AFG", "AGO", "BGD", "BEN", "BFA", "BDI", "KHM", "CAF", "TCD", "COM",
  "COD", "DJI", "ERI", "ETH", "GMB", "GIN", "GNB", "HTI", "KIR", "LAO",
  "LSO", "LBR", "MDG", "MWI", "MLI", "MRT", "MOZ", "MMR", "NPL", "NER",
  "RWA", "STP", "SEN", "SLE", "SLB", "SOM", "SSD", "SDN", "TZA", "TLS",
  "TGO", "TUV", "UGA", "YEM", "ZMB"
)

# Merge and prepare data
gdp_analysis <- gdp %>%
  left_join(continents %>% select(Code, Continent), by = "Code") %>%
  filter(Continent != "Antarctica", !is.na(Continent)) %>%
  mutate(
    LDC_Status = ifelse(Code %in% ldc_codes, "LDC", "Non-LDC")
  )

# Calculate year-over-year growth rates
gdp_growth <- gdp_analysis %>%
  arrange(Entity, Year) %>%
  group_by(Entity, Code, Continent, LDC_Status) %>%
  mutate(
    GDP_growth_rate = (GDP_per_capita - lag(GDP_per_capita)) / lag(GDP_per_capita) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(GDP_growth_rate))

# Filter for SDG period (2015 onwards)
sdg_data <- gdp_growth %>%
  filter(Year >= 2015, Year <= 2023)

# Calculate CAGR for each country
gdp_cagr <- sdg_data %>%
  filter(LDC_Status == "LDC") %>%
  group_by(Entity, Code, Continent) %>%
  arrange(Year) %>%
  filter(n() >= 3) %>%  # Need at least 3 years
  summarise(
    Start_Year = first(Year),
    End_Year = last(Year),
    Start_GDP = first(GDP_per_capita),
    End_GDP = last(GDP_per_capita),
    Years_Span = End_Year - Start_Year,
    CAGR = ((End_GDP / Start_GDP)^(1/Years_Span) - 1) * 100,
    Achieved_Target = CAGR >= 7,
    .groups = "drop"
  )

# ============================================================================
# 2. VISUALIZATION 1: OVERALL ACHIEVEMENT - BAR CHART
# ============================================================================

# Summary statistics
achievement_summary <- gdp_cagr %>%
  summarise(
    Total_LDCs = n(),
    LDCs_Above_7 = sum(Achieved_Target),
    LDCs_Below_7 = sum(!Achieved_Target),
    Percent_Above_7 = (LDCs_Above_7 / Total_LDCs) * 100
  )

# Create data for visualization
achievement_data <- data.frame(
  Category = c("Achieved 7%+", "Below 7%"),
  Count = c(achievement_summary$LDCs_Above_7, achievement_summary$LDCs_Below_7),
  Percentage = c(achievement_summary$Percent_Above_7, 100 - achievement_summary$Percent_Above_7)
)

plot1 <- ggplot(achievement_data, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Achieved 7%+" = "#2ecc71", "Below 7%" = "#e74c3c")) +
  labs(
    title = "Did LDCs Achieve 7% GDP Growth Target?",
    subtitle = paste0("Analysis of ", achievement_summary$Total_LDCs, " LDCs with sufficient data (2015-2023)"),
    x = NULL,
    y = "Number of Countries",
    caption = "Source: World Bank GDP data, UN LDC Classification\nTarget: SDG 8.1 - At least 7% GDP growth per annum in LDCs"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  ylim(0, max(achievement_data$Count) * 1.2)

print(plot1)
ggsave("plot1_ldc_achievement_overview.png", plot1, width = 10, height = 6, dpi = 300)

# ============================================================================
# 3. VISUALIZATION 2: ACHIEVEMENT BY CONTINENT
# ============================================================================

achievement_by_continent <- gdp_cagr %>%
  group_by(Continent) %>%
  summarise(
    Total_LDCs = n(),
    Achieved = sum(Achieved_Target),
    Not_Achieved = sum(!Achieved_Target),
    Percent_Achieved = (Achieved / Total_LDCs) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Percent_Achieved))

# Reshape for stacked bar chart
continent_data_long <- achievement_by_continent %>%
  pivot_longer(
    cols = c(Achieved, Not_Achieved),
    names_to = "Status",
    values_to = "Count"
  ) %>%
  mutate(
    Status = factor(Status, levels = c("Achieved", "Not_Achieved"),
                   labels = c("Achieved 7%+", "Below 7%"))
  )

plot2 <- ggplot(continent_data_long, aes(x = reorder(Continent, -Percent_Achieved), 
                                         y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = achievement_by_continent,
            aes(x = Continent, y = Total_LDCs, label = paste0(round(Percent_Achieved, 0), "%")),
            vjust = -0.5, size = 4, fontface = "bold", inherit.aes = FALSE) +
  scale_fill_manual(values = c("Achieved 7%+" = "#2ecc71", "Below 7%" = "#e74c3c")) +
  labs(
    title = "LDC Achievement of 7% GDP Growth Target by Continent",
    subtitle = "Percentage labels show proportion achieving target (2015-2023)",
    x = "Continent",
    y = "Number of LDCs",
    fill = "Target Status",
    caption = "Source: World Bank, UN Classification"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 0)
  )

print(plot2)
ggsave("plot2_achievement_by_continent.png", plot2, width = 12, height = 7, dpi = 300)

# ============================================================================
# 4. VISUALIZATION 3: GROWTH RATE DISTRIBUTION
# ============================================================================

plot3 <- ggplot(gdp_cagr, aes(x = CAGR)) +
  geom_histogram(aes(fill = Achieved_Target), bins = 30, alpha = 0.8) +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red", linewidth = 1.2) +
  annotate("text", x = 7, y = Inf, label = "7% Target", 
           vjust = 1.5, hjust = -0.1, color = "red", fontface = "bold", size = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
    labels = c("TRUE" = "Achieved", "FALSE" = "Not Achieved")
  ) +
  labs(
    title = "Distribution of GDP Growth Rates Among LDCs",
    subtitle = "Compound Annual Growth Rate (CAGR) 2015-2023",
    x = "GDP Growth Rate (%)",
    y = "Number of LDCs",
    fill = "Target Achievement",
    caption = "Red dashed line indicates 7% target"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot3)
ggsave("plot3_growth_distribution.png", plot3, width = 10, height = 6, dpi = 300)

# ============================================================================
# 5. VISUALIZATION 4: INDIVIDUAL LDC PERFORMANCE
# ============================================================================

# Show top 10 and bottom 10 performers
top_bottom_ldcs <- bind_rows(
  gdp_cagr %>% arrange(desc(CAGR)) %>% head(10) %>% mutate(Group = "Top 10"),
  gdp_cagr %>% arrange(CAGR) %>% head(10) %>% mutate(Group = "Bottom 10")
) %>%
  mutate(
    Entity = factor(Entity, levels = Entity[order(CAGR)])
  )

plot4 <- ggplot(top_bottom_ldcs, aes(x = CAGR, y = Entity, fill = Achieved_Target)) +
  geom_col() +
  geom_vline(xintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
    labels = c("TRUE" = "Achieved", "FALSE" = "Not Achieved")
  ) +
  facet_wrap(~Group, scales = "free_y", ncol = 1) +
  labs(
    title = "Best and Worst Performing LDCs",
    subtitle = "GDP Growth Rate (CAGR) 2015-2023",
    x = "Compound Annual Growth Rate (%)",
    y = NULL,
    fill = "Target Achievement",
    caption = "Red line indicates 7% target"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12)
  )

print(plot4)
ggsave("plot4_top_bottom_performers.png", plot4, width = 12, height = 10, dpi = 300)

# ============================================================================
# 6. VISUALIZATION 5: YEAR-BY-YEAR ACHIEVEMENT TREND
# ============================================================================

yearly_achievement <- sdg_data %>%
  filter(LDC_Status == "LDC") %>%
  group_by(Year) %>%
  summarise(
    Total_LDCs = n_distinct(Code),
    LDCs_Above_7 = sum(GDP_growth_rate >= 7, na.rm = TRUE),
    Percent_Above_7 = (LDCs_Above_7 / Total_LDCs) * 100,
    .groups = "drop"
  )

plot5 <- ggplot(yearly_achievement, aes(x = Year, y = Percent_Above_7)) +
  geom_line(linewidth = 1.5, color = "#3498db") +
  geom_point(size = 3, color = "#3498db") +
  geom_hline(yintercept = 50, linetype = "dotted", color = "gray40") +
  geom_text(aes(label = paste0(round(Percent_Above_7, 0), "%")), 
            vjust = -1, size = 3.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "Percentage of LDCs Achieving 7%+ GDP Growth Over Time",
    subtitle = "Year-by-year trend analysis (2015-2023)",
    x = "Year",
    y = "% of LDCs Meeting 7% Target",
    caption = "Source: World Bank GDP data\nDotted line at 50% shows majority threshold"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(plot5)
ggsave("plot5_yearly_trend.png", plot5, width = 12, height = 7, dpi = 300)

# ============================================================================
# 7. VISUALIZATION 6: BOX PLOT COMPARISON - LDC vs NON-LDC
# ============================================================================

sdg_comparison <- sdg_data %>%
  filter(Year >= 2015, Year <= 2023)

plot6 <- ggplot(sdg_comparison, aes(x = LDC_Status, y = GDP_growth_rate, fill = LDC_Status)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 1.5, y = 7, label = "7% Target for LDCs", 
           vjust = -0.5, color = "red", fontface = "bold") +
  scale_fill_manual(values = c("LDC" = "#e74c3c", "Non-LDC" = "#95a5a6")) +
  labs(
    title = "GDP Growth Rate Distribution: LDCs vs Non-LDCs",
    subtitle = "Annual growth rates (2015-2023)",
    x = "Country Classification",
    y = "GDP Growth Rate (%)",
    caption = "Source: World Bank\nBox shows median and quartiles, points show outliers"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16)
  )

print(plot6)
ggsave("plot6_ldc_vs_nonldc_comparison.png", plot6, width = 10, height = 7, dpi = 300)

# ============================================================================
# 8. VISUALIZATION 7: HEATMAP OF LDC PERFORMANCE BY CONTINENT AND YEAR
# ============================================================================

heatmap_data <- sdg_data %>%
  filter(LDC_Status == "LDC") %>%
  group_by(Continent, Year) %>%
  summarise(
    Percent_Above_7 = sum(GDP_growth_rate >= 7, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  )

plot7 <- ggplot(heatmap_data, aes(x = Year, y = Continent, fill = Percent_Above_7)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(round(Percent_Above_7, 0), "%")), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient2(
    low = "#e74c3c", mid = "#f39c12", high = "#2ecc71",
    midpoint = 50, limits = c(0, 100),
    name = "% Achieving\n7% Target"
  ) +
  labs(
    title = "LDC Achievement of 7% GDP Growth: Continental Trends",
    subtitle = "Percentage of LDCs in each continent achieving target by year",
    x = "Year",
    y = "Continent",
    caption = "Source: World Bank GDP data, UN LDC Classification"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    axis.text.x = element_text(angle = 0)
  )

print(plot7)
ggsave("plot7_heatmap_continent_year.png", plot7, width = 12, height = 6, dpi = 300)

# ============================================================================
# 9. SUMMARY STATISTICS TABLE
# ============================================================================

summary_table <- gdp_cagr %>%
  summarise(
    Total_LDCs_Analyzed = n(),
    Achieved_7_Percent = sum(Achieved_Target),
    Below_7_Percent = sum(!Achieved_Target),
    Percent_Achieved = round((Achieved_7_Percent / Total_LDCs_Analyzed) * 100, 1),
    Mean_Growth_Rate = round(mean(CAGR, na.rm = TRUE), 2),
    Median_Growth_Rate = round(median(CAGR, na.rm = TRUE), 2),
    Min_Growth_Rate = round(min(CAGR, na.rm = TRUE), 2),
    Max_Growth_Rate = round(max(CAGR, na.rm = TRUE), 2)
  )

cat("\n" , "="*80, "\n")
cat("SUMMARY: LDC ACHIEVEMENT OF 7% GDP GROWTH TARGET (2015-2023)\n")
cat("="*80, "\n\n")
cat("Total LDCs with sufficient data:", summary_table$Total_LDCs_Analyzed, "\n")
cat("LDCs achieving 7%+ growth:", summary_table$Achieved_7_Percent, 
    sprintf("(%.1f%%)\n", summary_table$Percent_Achieved))
cat("LDCs below 7% growth:", summary_table$Below_7_Percent, 
    sprintf("(%.1f%%)\n", 100 - summary_table$Percent_Achieved))
cat("\nGrowth Rate Statistics:\n")
cat("  Mean CAGR:", summary_table$Mean_Growth_Rate, "%\n")
cat("  Median CAGR:", summary_table$Median_Growth_Rate, "%\n")
cat("  Range:", summary_table$Min_Growth_Rate, "% to", summary_table$Max_Growth_Rate, "%\n")
cat("\nCONCLUSION:\n")
if (summary_table$Percent_Achieved >= 50) {
  cat("✓ MAJORITY of LDCs achieved the 7% growth target\n")
} else {
  cat("✗ MAJORITY of LDCs did NOT achieve the 7% growth target\n")
}
if (summary_table$Mean_Growth_Rate >= 7) {
  cat("✓ Average growth rate EXCEEDS the 7% target\n")
} else {
  cat("✗ Average growth rate BELOW the 7% target (gap of", 
      round(7 - summary_table$Mean_Growth_Rate, 2), "percentage points)\n")
}
cat("="*80, "\n\n")

# ============================================================================
# 10. EXPORT SUMMARY DATA
# ============================================================================

# Save summary tables for your report
write_csv(gdp_cagr, "ldc_cagr_results.csv")
write_csv(achievement_by_continent, "ldc_achievement_by_continent.csv")
write_csv(yearly_achievement, "ldc_yearly_achievement.csv")
write_csv(summary_table, "ldc_summary_statistics.csv")

cat("All visualizations saved as PNG files!\n")
cat("Summary data exported to CSV files.\n")
cat("\nFiles created:\n")
cat("  - plot1_ldc_achievement_overview.png\n")
cat("  - plot2_achievement_by_continent.png\n")
cat("  - plot3_growth_distribution.png\n")
cat("  - plot4_top_bottom_performers.png\n")
cat("  - plot5_yearly_trend.png\n")
cat("  - plot6_ldc_vs_nonldc_comparison.png\n")
cat("  - plot7_heatmap_continent_year.png\n")
cat("  - ldc_cagr_results.csv\n")
cat("  - ldc_achievement_by_continent.csv\n")
cat("  - ldc_yearly_achievement.csv\n")
cat("  - ldc_summary_statistics.csv\n")

cat("\n🎯 Analysis complete! These visualizations clearly show whether LDCs achieved\n")
cat("   the 7% GDP growth target specified in SDG 8.1.\n")
