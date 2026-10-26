library(tidyverse)

# Read in maternal mortality data
matmor <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)
infmor <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)
neomor <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
un5mor <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)
covariates <- read.csv("./data/raw/covariates.csv", header = TRUE) 

# Change wide to long format
matmor_long <- matmor |>
  pivot_longer(cols = starts_with("X"), #selecting columns starting w X
                 names_to = "year", #new column called year
                 names_prefix = "X", # removes X from year column
                 values_to = "maternal_mortality") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, maternal_mortality)

#make a function that applies this code to each of the WB dataframes
long_WB <- function(df, varname) {
  df %>%
    pivot_longer(cols = starts_with("X"),
  names_to = "year",
  names_prefix = "X",
  values_to = varname) %>%
    mutate(year = as.numeric(year)) %>%
    select(iso, year, varname)
}

#executing the function#
matmor_long <-  long_WB(df = matmor, varname = "matmor")
neonatal_long <- long_WB(df = neomor, varname = "neomor")
infmor_long <- long_WB(df = infmor, varname = "infmor")
un5mor_long <- long_WB(df = un5mor, varname = "un5mor")


# DISASTER DATA #
disaster_df <- read.csv("./data/raw/disaster.csv", header = TRUE) %>% janitor::clean_names()

disaster_df_filter <- disaster_df %>% 
  filter(between(year, 2000L, 2019L), disaster_type %in% c("Earthquake", "Drought"), !is.na(iso)) %>%
transmute(
    iso = as.character(iso), year = as.integer(year),
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) |>
  group_by(iso, year) |>
  summarise(earthquake = max(earthquake), drought = max(drought), .groups = "drop") %>% 
  select(year, iso, earthquake, drought)


#PREPARE CONFLICT DATA#
#Sum deaths across all conflicts in a country-year, then lag within country.
# "At least 25" deaths is implemented as total annual deaths >= 25.
conflict_annual <- read.csv("./data/raw/conflict.csv", header = TRUE) |>
  transmute(iso = as.character(iso), year = as.integer(year), best = as.numeric(best)) |>
  filter(!is.na(iso), !is.na(year)) |>
  group_by(iso, year) |>
  summarise(total_battle_deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  mutate(armed_conflict_current = as.integer(total_battle_deaths >= 25))

# Include the prior year so the first analytical year's lag is defined correctly.
analysis_years <- seq.int(min(covariates$year) - 1L, max(covariates$year))
conflict_data <- tidyr::crossing(iso = sort(unique(covariates$iso)), year = analysis_years) |>
  left_join(conflict_annual |> select(iso, year, armed_conflict_current), by = c("iso", "year")) |>
  mutate(armed_conflict_current = coalesce(armed_conflict_current, 0L)) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(armed_conflict = lag(armed_conflict_current)) |>
  ungroup() |>
  filter(year %in% covariates$year) |>
  select(iso, year, armed_conflict)


#MERGE#
final_list <- list(disaster_df_filter, conflict_data, matmor_long, infmor_long, neonatal_long, un5mor_long, covariates) 

#merge all data frames in list
final_list <- final_list %>% reduce(full_join, by = c("iso", "year")) %>% mutate(earthquake = coalesce(earthquake, 0L), drought = coalesce(drought, 0L)) %>%
  arrange(iso, year)

view(final_list)
write.csv(final_list, "data/processed/final_list.csv", row.names = FALSE)

#after running the Codex script, I have decided to stay with my own script. 
#the final csv files seemed to have the same values. 

