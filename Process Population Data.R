# Load required libraries
library(tidyverse)

# Read the CSV file
# Replace 'your_file.csv' with the actual path to your CSV file
data <- read.csv("WPP2024_PopulationByAge5GroupSex_Medium.csv", stringsAsFactors = FALSE)

# Remove columns C to I, L, R, S
# Based on the screenshot: C=Notes, D=ISO3_code, E=ISO2_code, F=SDMX_code, 
# G=LocTypeID, H=LocTypeName, I=ParentID, L=Variant, R=PopMale, S=PopFemale
columns_to_remove <- c("Notes", "ISO3_code", "ISO2_code", "SDMX_code", 
                       "LocTypeID", "LocTypeName", "ParentID", 
                       "Variant", "PopMale", "PopFemale")

# Remove specified columns (keeping only if they exist)
data_filtered <- data %>%
  select(-any_of(columns_to_remove))

# Filter for UN-recognized countries only
# List of exactly 195 UN-recognized countries (193 UN members + 2 observer states)
# This is based on ISO 3166-1 numeric codes

un_country_codes <- c(
  4,    # Afghanistan
  8,    # Albania
  12,   # Algeria
  20,   # Andorra
  24,   # Angola
  28,   # Antigua and Barbuda
  32,   # Argentina
  51,   # Armenia
  36,   # Australia
  40,   # Austria
  31,   # Azerbaijan
  44,   # Bahamas
  48,   # Bahrain
  50,   # Bangladesh
  52,   # Barbados
  112,  # Belarus
  56,   # Belgium
  84,   # Belize
  204,  # Benin
  64,   # Bhutan
  68,   # Bolivia
  70,   # Bosnia and Herzegovina
  72,   # Botswana
  76,   # Brazil
  96,   # Brunei Darussalam
  100,  # Bulgaria
  854,  # Burkina Faso
  108,  # Burundi
  132,  # Cabo Verde
  116,  # Cambodia
  120,  # Cameroon
  124,  # Canada
  140,  # Central African Republic
  148,  # Chad
  152,  # Chile
  156,  # China
  170,  # Colombia
  174,  # Comoros
  178,  # Congo
  180,  # Democratic Republic of the Congo
  188,  # Costa Rica
  384,  # Côte d'Ivoire
  191,  # Croatia
  192,  # Cuba
  196,  # Cyprus
  203,  # Czechia
  208,  # Denmark
  262,  # Djibouti
  212,  # Dominica
  214,  # Dominican Republic
  218,  # Ecuador
  818,  # Egypt
  222,  # El Salvador
  226,  # Equatorial Guinea
  232,  # Eritrea
  233,  # Estonia
  748,  # Eswatini
  231,  # Ethiopia
  242,  # Fiji
  246,  # Finland
  250,  # France
  266,  # Gabon
  270,  # Gambia
  268,  # Georgia
  276,  # Germany
  288,  # Ghana
  300,  # Greece
  308,  # Grenada
  320,  # Guatemala
  324,  # Guinea
  624,  # Guinea-Bissau
  328,  # Guyana
  332,  # Haiti
  336,  # Holy See (Vatican City)
  340,  # Honduras
  348,  # Hungary
  352,  # Iceland
  356,  # India
  360,  # Indonesia
  364,  # Iran
  368,  # Iraq
  372,  # Ireland
  376,  # Israel
  380,  # Italy
  388,  # Jamaica
  392,  # Japan
  400,  # Jordan
  398,  # Kazakhstan
  404,  # Kenya
  296,  # Kiribati
  408,  # North Korea
  410,  # South Korea
  414,  # Kuwait
  417,  # Kyrgyzstan
  418,  # Lao People's Democratic Republic
  428,  # Latvia
  422,  # Lebanon
  426,  # Lesotho
  430,  # Liberia
  434,  # Libya
  438,  # Liechtenstein
  440,  # Lithuania
  442,  # Luxembourg
  450,  # Madagascar
  454,  # Malawi
  458,  # Malaysia
  462,  # Maldives
  466,  # Mali
  470,  # Malta
  584,  # Marshall Islands
  478,  # Mauritania
  480,  # Mauritius
  484,  # Mexico
  583,  # Micronesia
  498,  # Moldova
  492,  # Monaco
  496,  # Mongolia
  499,  # Montenegro
  504,  # Morocco
  508,  # Mozambique
  104,  # Myanmar
  516,  # Namibia
  520,  # Nauru
  524,  # Nepal
  528,  # Netherlands
  554,  # New Zealand
  558,  # Nicaragua
  562,  # Niger
  566,  # Nigeria
  807,  # North Macedonia
  578,  # Norway
  512,  # Oman
  586,  # Pakistan
  585,  # Palau
  275,  # Palestine (Observer State)
  591,  # Panama
  598,  # Papua New Guinea
  600,  # Paraguay
  604,  # Peru
  608,  # Philippines
  616,  # Poland
  620,  # Portugal
  634,  # Qatar
  642,  # Romania
  643,  # Russian Federation
  646,  # Rwanda
  659,  # Saint Kitts and Nevis
  662,  # Saint Lucia
  670,  # Saint Vincent and the Grenadines
  882,  # Samoa
  674,  # San Marino
  678,  # Sao Tome and Principe
  682,  # Saudi Arabia
  686,  # Senegal
  688,  # Serbia
  690,  # Seychelles
  694,  # Sierra Leone
  702,  # Singapore
  703,  # Slovakia
  705,  # Slovenia
  90,   # Solomon Islands
  706,  # Somalia
  710,  # South Africa
  728,  # South Sudan
  724,  # Spain
  144,  # Sri Lanka
  729,  # Sudan
  740,  # Suriname
  752,  # Sweden
  756,  # Switzerland
  760,  # Syrian Arab Republic
  762,  # Tajikistan
  834,  # Tanzania
  764,  # Thailand
  626,  # Timor-Leste
  768,  # Togo
  776,  # Tonga
  780,  # Trinidad and Tobago
  788,  # Tunisia
  792,  # Turkey
  795,  # Turkmenistan
  798,  # Tuvalu
  800,  # Uganda
  804,  # Ukraine
  784,  # United Arab Emirates
  826,  # United Kingdom
  840,  # United States
  858,  # Uruguay
  860,  # Uzbekistan
  548,  # Vanuatu
  862,  # Venezuela
  704,  # Viet Nam
  887,  # Yemen
  894,  # Zambia
  716   # Zimbabwe
)

# Mapping of territories/dependencies to their parent countries
# These will be aggregated into their parent countries
territory_mapping <- data.frame(
  TerritoryCode = c(
    660,  # Anguilla -> UK
    533,  # Aruba -> Netherlands
    535,  # Bonaire, Sint Eustatius and Saba -> Netherlands
    92,   # British Virgin Islands -> UK
    136,  # Cayman Islands -> UK
    162,  # Christmas Island -> Australia
    166,  # Cocos Islands -> Australia
    184,  # Cook Islands -> New Zealand
    531,  # Curaçao -> Netherlands
    238,  # Falkland Islands -> UK
    234,  # Faroe Islands -> Denmark
    254,  # French Guiana -> France
    258,  # French Polynesia -> France
    304,  # Greenland -> Denmark
    312,  # Guadeloupe -> France
    316,  # Guam -> USA
    831,  # Guernsey -> UK
    344,  # Hong Kong -> China
    833,  # Isle of Man -> UK
    832,  # Jersey -> UK
    446,  # Macao -> China
    474,  # Martinique -> France
    175,  # Mayotte -> France
    500,  # Montserrat -> UK
    540,  # New Caledonia -> France
    570,  # Niue -> New Zealand
    574,  # Norfolk Island -> Australia
    580,  # Northern Mariana Islands -> USA
    630,  # Puerto Rico -> USA
    638,  # Réunion -> France
    652,  # Saint Barthélemy -> France
    654,  # Saint Helena -> UK
    663,  # Saint Martin (French part) -> France
    666,  # Saint Pierre and Miquelon -> France
    534,  # Sint Maarten (Dutch part) -> Netherlands
    239,  # South Georgia and the South Sandwich Islands -> UK
    744,  # Svalbard and Jan Mayen -> Norway
    158,  # Taiwan -> China
    772,  # Tokelau -> New Zealand
    796,  # Turks and Caicos Islands -> UK
    850,  # United States Virgin Islands -> USA
    876,  # Wallis and Futuna -> France
    732   # Western Sahara -> Morocco (disputed, but commonly grouped)
  ),
  ParentCode = c(
    826,  # UK
    528,  # Netherlands
    528,  # Netherlands
    826,  # UK
    826,  # UK
    36,   # Australia
    36,   # Australia
    554,  # New Zealand
    528,  # Netherlands
    826,  # UK
    208,  # Denmark
    250,  # France
    250,  # France
    208,  # Denmark
    250,  # France
    840,  # USA
    826,  # UK
    156,  # China
    826,  # UK
    826,  # UK
    156,  # China
    250,  # France
    250,  # France
    826,  # UK
    250,  # France
    554,  # New Zealand
    36,   # Australia
    840,  # USA
    840,  # USA
    250,  # France
    250,  # France
    826,  # UK
    250,  # France
    250,  # France
    528,  # Netherlands
    826,  # UK
    578,  # Norway
    156,  # China
    554,  # New Zealand
    826,  # UK
    840,  # USA
    250,  # France
    504   # Morocco
  ),
  stringsAsFactors = FALSE
)

# First, separate UN countries and territories
data_un_countries <- data_filtered %>%
  filter(LocID %in% un_country_codes)

data_territories <- data_filtered %>%
  filter(LocID %in% territory_mapping$TerritoryCode)

# Aggregate territories into their parent countries
if (nrow(data_territories) > 0) {
  data_territories_aggregated <- data_territories %>%
    left_join(territory_mapping, by = c("LocID" = "TerritoryCode")) %>%
    group_by(ParentCode, Time, MidPeriod, AgeGrp, AgeGrpStart, AgeGrpSpan) %>%
    summarise(PopTotal = sum(PopTotal, na.rm = TRUE), .groups = "drop") %>%
    rename(LocID = ParentCode) %>%
    # Get parent country names
    left_join(
      data_filtered %>% select(LocID, Location) %>% distinct(),
      by = "LocID"
    )
  
  # Combine UN countries with aggregated territories
  data_combined <- bind_rows(
    data_un_countries,
    data_territories_aggregated
  ) %>%
    # Sum up populations for countries that now include territories
    group_by(LocID, Location, Time, MidPeriod, AgeGrp, AgeGrpStart, AgeGrpSpan) %>%
    summarise(PopTotal = sum(PopTotal, na.rm = TRUE), .groups = "drop")
} else {
  data_combined <- data_un_countries
}

# Use the combined data for further processing
data_un_countries <- data_combined

# Filter for UN countries
data_un_countries <- data_un_countries %>%
  filter(LocID %in% un_country_codes)

# Filter for years 1976 to 2022 only
data_un_countries <- data_un_countries %>%
  filter(Time >= 1976 & Time <= 2022)

# Create the final dataset with required columns
# Column O in the original is "AgeGrp" based on the screenshot
final_data <- data_un_countries %>%
  select(
    CountryCode = LocID,           # ISO 3166-1 numeric code
    CountryName = Location,         # Country name
    Year = Time,                    # Year
    AgeGroup = AgeGrp,             # Age group
    Population = PopTotal          # Total population for that age group
  ) %>%
  group_by(CountryCode, CountryName, Year) %>%
  mutate(
    TotalPopulation = sum(Population, na.rm = TRUE),
    PercentageOfTotal = (Population / TotalPopulation) * 100
  ) %>%
  ungroup() %>%
  arrange(CountryCode, Year, AgeGroup)

# Display first few rows
head(final_data, 20)

# Get summary statistics
cat("\nTotal number of countries:", n_distinct(final_data$CountryCode), "\n")
cat("Year range:", min(final_data$Year), "to", max(final_data$Year), "\n")
cat("Total rows:", nrow(final_data), "\n")

# Save the processed data
write.csv(final_data, "processed_population_un_countries.csv", row.names = FALSE)

# Optional: Create a wide format with age groups as columns
data_wide <- final_data %>%
  pivot_wider(
    names_from = AgeGroup,
    values_from = Population,
    names_prefix = "Age_"
  )

# Save wide format
write.csv(data_wide, "processed_population_un_countries_wide.csv", row.names = FALSE)

cat("\nProcessed data saved to:\n")
cat("- processed_population_un_countries.csv (long format)\n")
cat("- processed_population_un_countries_wide.csv (wide format)\n")