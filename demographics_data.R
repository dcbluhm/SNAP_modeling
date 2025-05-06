#Data Science Group Project
##Census Data

library(tidyverse)
library(tidycensus)
library(dplyr)
library(purrr)
library(readxl)

#Census Data
#######
census_vars <- tidycensus::load_variables(year = 2016,
                                          dataset = c("acs1"))

V#B06011_001E is median income
#B19013_001 is median household income in past 12 months (colinearity w/median income?)
#B01001_001E is total population
#B01001_002E is number of males
#B17001_002E is number below poverty line
#B01002_001 is median age (total)
#B03002_003 is total number of non-hispanic white (_001 is overall)
#B15003_017 to _025 is education attainment levels
#B11001_002 is number of family households (_001 is overall)
#B18101_001 is total number disabled status
      #_004,_007, etc are number that are disabled for age/sex groups
#B25003_003 is total number of renters (_001 is overall)
#B22001_002 is total number of households that received food stamps in prior 12 months (_001 is overall)
#B05001_006 is total number of non-citizens (_001 is overall)
#B01001_020-25 and _044-049 is total over 65 (_001 is overall)


get_snap <- function(year) {
  
  available_vars <- load_variables(year, "acs1", cache = TRUE)$name
  
  snap_vars <- c("B06011_001E", "B01001_001E", "B01001_002E",
                      "B17001_002E", "B17001_001E", "B19013_001",
                      "B01002_001", "B03002_003", "B03002_001",
                      "B15003_017", "B15003_018", "B15003_019",
                      "B15003_020", "B15003_021", "B15003_022",
                      "B15003_023", "B15003_024", "B15003_025",
                      "B11001_002", "B11001_001", "B25003_003",
                      "B25003_001", "B18101_001", "B22001_002", "B22001_001",
                 "B18101_004", "B18101_007", "B18101_010", "B18101_013",
                 "B18101_016", "B18101_019", "B18101_022", "B18101_025",
                 "B18101_028", "B18101_031", "B05001_006", "B05001_001",
                 "B01001_020", "B01001_021", "B01001_022", "B01001_023",
                 "B01001_024", "B01001_025", "B01001_044", "B01001_045",
                 "B01001_046", "B01001_047", "B01001_048", "B01001_049")
  
  snap_vars_nosuffix <- gsub("E$", "", snap_vars)
  
  usable_vars <- intersect(snap_vars_nosuffix, available_vars)
  
  if (length(usable_vars) == 0) {
    warning(paste0("No matching variables available for year ", year))
    return(NULL)
  }
  
  census_demo <- get_acs(geography = "county",
                         variables = usable_vars,
                         year = year,
                         survey = "acs1",
                         output = "tidy",
                         progress_bar = FALSE) |>
    mutate(year = year) |>
    mutate(variable = case_when(
      variable == "B06011_001" ~ "median_income_individual",
      variable == "B01001_001" ~ "total_population",
      variable == "B01001_002" ~ "male_population",
      variable == "B17001_002" ~ "below_poverty",
      variable == "B17001_001" ~ "poverty_total",
      variable == "B19013_001"  ~ "median_household_income",
      variable == "B01002_001"  ~ "median_age",
      variable == "B03002_003"  ~ "white_alone",
      variable == "B03002_001"  ~ "total_race_population",
      variable == "B15003_017"  ~ "hs_grad",
      variable == "B15003_018"  ~ "some_college_less_1yr",
      variable == "B15003_019"  ~ "some_college_more_1yr",
      variable == "B15003_020"  ~ "assoc_degree",
      variable == "B15003_021"  ~ "bachelors_degree",
      variable == "B15003_022"  ~ "masters_degree",
      variable == "B15003_023"  ~ "professional_degree",
      variable == "B15003_024"  ~ "doctorate_degree",
      variable == "B15003_025"  ~ "education_total_higher",
      variable == "B11001_002"  ~ "family_households",
      variable == "B11001_001"  ~ "total_households",
      variable == "B25003_003"  ~ "renter_occupied",
      variable == "B25003_001"  ~ "total_occupied_housing",
      variable == "B18101_001"  ~ "total_disability_universe",
      variable == "B22001_002"  ~ "snap_recipient_hh_prior12",
      variable == "B22001_001"  ~ "total_snap_universe",
      variable == "B05001_006" ~ "non_citizens",
      variable == "B05001_001" ~ "non_citizens_universe",
      TRUE ~ variable  # keep original name if no match
    ))
  
  census_wide <- census_demo |>
    select(GEOID, NAME, year, variable, estimate) |>
    pivot_wider(names_from = "variable",
                values_from = "estimate")
  
  census_wide <- census_wide |>
    mutate(male_pct = (male_population/total_population),
           white_pct = (white_alone/total_race_population),
           family_pct = (family_households/total_households),
           bachelor_or_higher_pct = ((bachelors_degree + masters_degree + 
                                        professional_degree + doctorate_degree)/
                                       education_total_higher),
           poverty_pct = (below_poverty/poverty_total),
           disabled_pct = ((B18101_004 + B18101_007 + B18101_010 + B18101_013 +
                             B18101_016 + B18101_019 + B18101_022 + B18101_025 +
                             B18101_028 + B18101_031)/total_disability_universe),
           snap_pct = (snap_recipient_hh_prior12/total_snap_universe),
           renter_pct = (renter_occupied/total_occupied_housing),
           non_citizen_pct = (non_citizens/non_citizens_universe),
           over_65_pct = ((B01001_020 + B01001_021 + B01001_022 + B01001_023 + 
                             B01001_024 + B01001_025 + B01001_044 + B01001_045 +
                             B01001_046 + B01001_047 + B01001_048 + B01001_049)/
                            total_population)) |>
    select(GEOID, NAME, year, male_pct, median_age, white_pct, family_pct, bachelor_or_higher_pct,
           poverty_pct, disabled_pct, snap_pct, renter_pct, median_age,
           median_income_individual, median_household_income, 
           snap_recipient_hh_prior12, non_citizen_pct, over_65_pct)
           
  
  assign(paste0("snap_", year), census_wide, envir = .GlobalEnv)
}

years <- c(2015, 2016, 2017, 2018, 2019, 2023)

map(.x = years, .f = get_snap)


combined_census <- bind_rows(snap_2015, snap_2016, snap_2017, snap_2018, 
                             snap_2019, snap_2023) |>
  arrange(GEOID)
#######

#USDA Rural-Urban Data
#######
ruralurbancodes2013 <- read_excel("data/ruralurbancodes2013.xls")
ruralurbancodes2023 <- read_excel("data/Ruralurbancontinuumcodes2023.xlsx")

ruralurbancodes2013 <- ruralurbancodes2013 |>
  rename(GEOID = FIPS)

ruralurbancodes2023 <- ruralurbancodes2023 |>
  rename(GEOID = FIPS)

census_urban_rural <- combined_census |>
  left_join(ruralurbancodes2013 |>
              select(GEOID, RUCC_2013), by = "GEOID") |>
  left_join(ruralurbancodes2023 |>
              select(GEOID, RUCC_2023), by = "GEOID") |>
  mutate(rural_urban_score = case_when(
    year == 2023 ~ RUCC_2023,
    TRUE ~ RUCC_2013)) |>
  select(-RUCC_2013, -RUCC_2023)
######

#Univ Kentucky Welfare Data
######
ky_welfare <- read_excel("data/UKCPR_National_Welfare_Data_1980_2023 (1).xlsx", 
                         sheet = "Data", 
                         col_types = c("text", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric")) |>
  janitor::clean_names()

ky_welfare <- ky_welfare |>
  filter(year %in% c(2015,2016,2017,2018,2019,2023)) |>
  mutate(state_fips = str_pad(state_fips, width = 2, side = "left", pad = "0")) |>
  select(state_fips, year, unemployment_rate, marginally_food_insecure, food_insecure,
         very_low_food_secure, gross_state_product, governor_is_democrat_1_yes,
         fraction_of_state_house_that_is_democrat, fraction_of_state_senate_that_is_democrat,
         state_minimum_wage)

census_urban_rural <- census_urban_rural |>
  mutate(state_fips = substr(GEOID, 1, 2))

demographic_combined <-  census_urban_rural |>
  left_join(ky_welfare, by = c("state_fips", "year")) 

#Last cleaning details and Exploration
#####
View(demographic_combined |>
  filter(is.na(governor_is_democrat_1_yes))) #All DC and PR

demographic_combined <- demographic_combined |>
  filter(state_fips != 72) #Filtering out PR

#Manually imputing 1 to reflect DC's Democratic mayor each year
demographic_combined <- demographic_combined |>
  mutate(governor_is_democrat_1_yes = ifelse(GEOID == "11001", 1, governor_is_democrat_1_yes))

demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_house_that_is_democrat = ifelse(
    GEOID == "11001", 1, fraction_of_state_house_that_is_democrat))

#Manually imputing Nebraska's unicameral makeup to both state house and senate pct
#Though technically nonpartisan, data on party endorsements of candidates is used
demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_house_that_is_democrat = ifelse(
    state_fips == "31" & year %in% c(2015, 2016), 12/49, 
    fraction_of_state_house_that_is_democrat))

demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_senate_that_is_democrat = ifelse(
    state_fips == "31" & year %in% c(2015, 2016), 12/49, 
    fraction_of_state_senate_that_is_democrat))

demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_house_that_is_democrat = ifelse(
    state_fips == "31" & year %in% c(2017, 2018, 2019), 15/49, 
    fraction_of_state_house_that_is_democrat))

demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_senate_that_is_democrat = ifelse(
    state_fips == "31" & year %in% c(2017, 2018, 2019), 15/49, 
    fraction_of_state_senate_that_is_democrat))

demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_house_that_is_democrat = ifelse(
    state_fips == "31" & year %in% c(2023), 17/49, 
    fraction_of_state_house_that_is_democrat))

demographic_combined <- demographic_combined |>
  mutate(fraction_of_state_senate_that_is_democrat = ifelse(
    state_fips == "31" & year %in% c(2023), 17/49, 
    fraction_of_state_senate_that_is_democrat))

