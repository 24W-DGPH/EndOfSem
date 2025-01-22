# Load Packages -------------------------
pacman::p_load(
  shiny,
  janitor,
  tidyverse, # data management and visualization
  rio
)


# Import Data ---------------------------
# TODO: check wrong file on import

who_data <- import("data/world_health_data.csv", setclass = "tibble")

# Examine structure of the data
str(who_data)

# data exploration
unique(who_data$country)

unique(who_data$country) %>% sort()

# invalid entries in country variable

# cleaning: country iso codes and names with standard reference
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv

iso_reference <- import("data/iso_codes.csv", setclass = "tibble")

# clean reference data
iso_reference <- janitor::clean_names(iso_reference)

# extract standard country iso codes
country_reference <- iso_reference$alpha_3

# clean who_data with standard iso reference  (for country names)
who_data_clean <- who_data %>% filter(country_code %in% country_reference)

# Renaming columns
who_data_clean <- who_data_clean %>%
  rename(
  hiv_prevalence = prev_hiv,
  tb_incidence = inci_tuberc,
  malnutrition_prevalence = prev_undernourishment
)

# Remove unused data
rm(iso_reference)
#rm(who_data)


