# Load Packages -----------------------------------------------------------

library(tidyverse)
library(janitor)

# Measles -----------------------------------------------------------------

# CSV comes from https://publichealth.jhu.edu/ivac/resources/us-measles-tracker

# Import- measles cases dataset

us_states_and_territories <-
  state.name |>
  as_tibble() |>
  set_names("state") |>
  add_row(state = "District of Columbia") |>
  add_row(state = "Puerto Rico")

total_measles_cases <-
  read_csv("https://static.dwcdn.net/data/4zhkG.csv?v=1757352000000") |>
  clean_names() |>
  full_join(us_states_and_territories) |>
  select(state, total) |>
  arrange(state) |>
  mutate(total = replace_na(total, 0))

# Export data
total_measles_cases |>
  write_csv("data-clean/total_measles_cases.csv")

# MMR Coverage ------------------------------------------------------------
# CSV comes from CDC's SchoolVaxView (https://data.cdc.gov/Vaccinations/Vaccination-Coverage-and-Exemptions-among-Kinderga/ijqb-a7ye/about_data)

mmr_coverage <-
  read_csv("data-raw/mmr_coverage.csv") |>
  clean_names() |>
  filter(vaccine_exemption == "MMR") |>
  filter(geography_type == "States" | geography == "U.S. Median")

# Desired years
target_years <- c(
  "2020-21",
  "2021-22",
  "2022-23",
  "2023-24",
  "2024-25"
)

# Filter the data
mmr_filtered <-
  mmr_coverage |>
  filter(
    school_year %in% target_years
  )

# Additional target years for Montana
montana_years <- c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21")

montana_filtered <- mmr_coverage |>
  filter(
    school_year %in% montana_years,
    geography == "Montana"
  )

# Add West Virginia for 2019-20
wv_filtered <- mmr_coverage |>
  filter(
    school_year == "2019-20",
    geography == "West Virginia"
  )

# Sort the dataset by descending years
mmr_filtered_sorted <-
  bind_rows(mmr_filtered, wv_filtered, montana_filtered) |>
  arrange(geography, school_year)

# Export the dataset
mmr_filtered_sorted |>
  write_csv("data-clean/mmr_coverage_final.csv")


# Non-medical exemption rate-----------------------------------------------------------------
# Data comes from CDC's SchoolVaxView (same dataset as above)
# Filter for non-medical exemptions for 2023-2024 and 2024-2025

non_medical_exemptions_states_and_us <-
  read_csv("data-raw/mmr_coverage.csv") |>
  clean_names() |>
  filter(
    dose == "Non-Medical Exemption",
    (geography_type == "States" | geography == "U.S. Median")
  )

non_medical_exemptions_23_24 <-
  non_medical_exemptions_states_and_us |>
  filter(
    school_year %in% c("2023-24", "2024-25")
  )

# New York: 2017-18 and 2018-19
ny_filtered <-
  non_medical_exemptions_states_and_us |>
  filter(
    school_year %in% c("2017-18", "2018-19"),
    geography == "New York"
  )

# Montana: 2019-20 and 2020-21
mt_filtered <-
  non_medical_exemptions_states_and_us |>
  filter(
    school_year %in% c("2019-20", "2020-21"),
    geography == "Montana"
  )

# California: 2015-16 and 2016-17
ca_filtered <-
  non_medical_exemptions_states_and_us |>
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2015-16", "2016-17"),
    geography == "California"
  )

# Maine: 2022-23 and 2023-24
me_filtered <-
  non_medical_exemptions_states_and_us |>
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2022-23"),
    geography == "Maine"
  )

# West Virginia: NA
wv_filtered <-
  non_medical_exemptions_states_and_us |>
  filter(
    dose == "Non-Medical Exemption",
    school_year %in% c("2018-19", "2019-20"),
    geography == "West Virginia"
  )

non_medical_exemptions <-
  bind_rows(
    non_medical_exemptions_23_24,
    ny_filtered,
    mt_filtered,
    ca_filtered,
    me_filtered,
    wv_filtered
  )

# Export the dataset
non_medical_exemptions |>
  write_csv("data-clean/non_medical_exemption.csv")

# DTaP --------------------------------------------------------------------
# CSV comes from CDC's ChildVaxView (https://www.cdc.gov/childvaxview/about/interactive-reports.html)
# Import data set

dtap_filtered_states <-
  read_csv("data-raw/dtap_coverage.csv") |>
  clean_names() |>
  filter(
    vaccine == "DTaP",
    dose == "≥4 Doses",
    dimension == "24 Months",
    birth_year_birth_cohort %in% c("2017", "2018", "2019", "2020", "2021")
  ) |>
  mutate(year = as.numeric(birth_year_birth_cohort) + 2) |>
  arrange(geography, birth_year_birth_cohort)

# Export dataset
dtap_filtered_states |>
  write_csv("data-clean/dtap_coverage_final.csv")

# Vaccine Exemptions ------------------------------------------------------
# CSV from NCSL's brief (https://www.ncsl.org/health/state-non-medical-exemptions-from-school-immunization-requirements)

# library(rvest)
#
# vaccine_exemptions <-
#   read_html("https://www.ncsl.org/health/state-non-medical-exemptions-from-school-immunization-requirements") |>
#   html_table() |>
#   pluck(1)

vaccine_exemptions <-
  read_csv("data-raw/non_medical_exemption_policies.csv")

# Export dataset
vaccine_exemptions |>
  write_csv("data-clean/non_medical_exemption_policies_final.csv")

# Health Spending ---------------------------------------------------------
# CSV from from 'America's health rankings'

# Import dataset-Public Health Spending
health_spending <-
  read_csv("data-raw/health_spending.csv")

# Export dataset
health_spending |>
  write_csv("data-clean/health_spending_final.csv")

# Universal vaccine purchase program------------------------------------------------------
# CSV from AIM resources page
universal_purchase <-
  read_csv("data-raw/universal_purchase.csv")

# Export dataset
universal_purchase |>
  write_csv("data-clean/universal_purchase_final.csv")

# State policies------------------------------------------------------
state_policies <-
  read_csv("data-raw/state_policies.csv") |>
  clean_names() |>
  filter(x1_include_in_brief == 1)

# Export dataset
state_policies |>
  write_csv("data-clean/state_policies_final.csv")

# Census Data ------------------------------------------------------------

# library(tidycensus)

# population_by_state <-
#   get_decennial(
#     geography = "state",
#     variables = "P1_001N", # Total population variable
#     year = 2020,
#     survey = "pl" # PL 94-171 Redistricting Data
#   ) |>
#   select(NAME, value) |>
#   rename(
#     state = NAME,
#     total_population = value
#   ) |>
#   arrange(desc(total_population))

# population_by_state |>
#   write_rds("data-clean/population_by_state.rds")


# Geospatial Data ---------------------------------------------------------

# library(tigris)
# 
# us_states <-
#   states() |>
#   clean_names() |>
#   select(name) |>
#   filter(
#     !name %in%
#       c(
#         "Commonwealth of the Northern Mariana Islands",
#         "American Samoa",
#         "United States Virgin Islands"
#       )
#   )
# 
# us_states |> 
#   write_rds("data-clean/us_states.rds")
