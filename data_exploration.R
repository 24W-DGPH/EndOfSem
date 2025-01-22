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

# Convert year to a date time variable
# who_data$year <- year(who_data$year)

# data exploration
unique(who_data$country)
unique(who_data$country) %>% sort()

# cleaning country iso codes and names with standard reference
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

# View column names
colnames(who_data_clean)


# Summary statistics ------------------------------------------------------

d <- who_data_clean %>% 
  group_by(country) %>% 
  summarise(mean_health_exp = mean(health_exp, na.rm = TRUE),
            mean_infant_mortality = mean(infant_mortality, na.rm = T),
            mean_neonatal_mortality = mean(neonatal_mortality, na.rm = T),
            mean_maternal_mortility = mean(maternal_mortality, na.rm = T),
            mean_under_5_mort = mean(under_5_mortality, na.rm = T),
  ) %>% 
  arrange(desc(mean_health_exp)) 

us <- who_data_clean %>% filter(country == "Germany")
ggplot(us, aes(x = year)) +
  geom_line(aes(y = life_expect))

# Correlation between key indicators
cor.test(who_data_clean$hiv_prevalence, who_data_clean$malnutrition_prevalence)
cor.test(who_data_clean$health_exp, who_data_clean$life_expect)

table <- tibble(
  cor_hiv_mal = cor.test(who_data_clean$hiv_prevalence, who_data_clean$malnutrition_prevalence)$estimate,
  cor_heal_life = cor.test(who_data_clean$health_exp, who_data_clean$life_expect)$estimate,
  
)

who_data_clean %>%  group_by(year) %>% 
  summarise(max = max(who_data_clean$health_exp, na.rm = T))


# country with the highest prevalence of (variable) 
#life expectancy variations across the years for each country
#correlation of health expenditure to life expectancy for each year
# Graphs: distribution of (variable) across time
