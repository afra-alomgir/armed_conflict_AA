library(tidyverse)

# Read in maternal mortality data
matmor <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)
infmor <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)
neomor <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
un5mor <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)

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
View(disaster_df)

disaster_df_filter <- disaster_df %>% 
  filter(disaster_type %in% c("Earthquake", "Drought")) %>%
  filter(between(year, 2000, 2019)) %>% 
  select(year, iso, disaster_type)
view(disaster_df_filter)

# create indicators using dummy variables 
disaster_df_dummy <- disaster_df_filter %>%
  mutate(
    drought = if_else(disaster_df_filter$disaster_type == "Drought", 1, 0),
    earthquake = if_else(disaster_df_filter$disaster_type == "Earthquake", 1, 0)
  ) %>%
  group_by(year, iso) %>%
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  ) %>% 
  select(year, iso, earthquake, drought)
view(disaster_df_dummy)

#country year combos 
country_years <- expand_grid(
  iso = unique(disaster_df_filter$iso),
  year = 2000:2019
)

#adding indicators and replace missing values with 0 
disaster_df_dummy <- country_years %>%
  left_join(disaster_df_dummy, by = c("iso", "year")) %>%
  mutate(
    drought = replace_na(drought, 0),
    earthquake = replace_na(earthquake, 0)
  ) %>%
  select(year, iso, earthquake, drought)

view(disaster_df_dummy)

#PREPARE CONFLICT DATA#
conflict_df <- read.csv("./data/raw/conflict.csv", header = TRUE) 
view(conflict_df)

#Derive the binary armed conflict variable that was used as the primary exposure in the paper
# we want the final data to only include the indicator of whether the country had a conflict that year
# based on the definition used in the paper (at least 25 deaths in a year)
# iso, year, sum of deaths, yes/no 

#diff conflict IDs. only wants IDs where best > 25
head(conflict_df)

armed_conflict_year <- conflict_df %>%
  group_by(conflict_id, iso, year) %>%
  summarise(
    sum_deaths = sum(best, na.rm=TRUE), .groups = "drop") %>%
  group_by(iso, year) %>%
  summarise(
    armed_conflict = if_else(any(sum_deaths >= 25), 1, 0), .groups = "drop") 

#lag
armed_conflict_year <- armed_conflict_year %>%
  group_by(iso) %>%
  complete(year = 2000:2019, fill = list(armed_conflict = 0)) %>%
  ungroup()

armed_conflict_year <- armed_conflict_year %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(
    armed_conflict_lag1 = lag(armed_conflict, n = 1)
  ) %>%
  ungroup()

view(armed_conflict_year)


#MERGE#
final_list <- list(disaster_df_dummy, armed_conflict_year, matmor_long, infmor_long, neonatal_long, un5mor_long)

#merge all data frames in list
final_list <- final_list |> reduce(full_join, by = c("iso", "year")) 
view(final_list)
write.csv(final_list, "processed/final_list.csv", row.names = FALSE)

#after running the Codex script, I have decided to stay with my own script. 
#the final csv files seemed to have the same values. 