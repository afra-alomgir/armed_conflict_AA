# Independently reproduce the analytical country-year data set.
# Run from the repository root: Rscript scripts/codex_script.R

suppressPackageStartupMessages({ library(tidyverse); library(janitor) })

# Prefer the assignment's stated location, with this repository's raw directory
# as a compatible fallback.
input_dir <- if (dir.exists("data/original")) "data/original" else "data/raw"
output_file <- "data/processed/final_data_codex.csv"
read_input <- function(file_name) readr::read_csv(file.path(input_dir, file_name), show_col_types = FALSE)

assert_unique_key <- function(data, data_name, key = c("iso", "year")) {
  duplicate_count <- data |> count(across(all_of(key))) |> filter(n > 1) |> nrow()
  if (duplicate_count > 0) stop(data_name, " has ", duplicate_count, " duplicate country-year key(s).")
  invisible(data)
}

# Reusable wide-to-long World Bank mortality transformation.
prepare_mortality <- function(file_name, outcome_name) {
  mortality_raw <- read_input(file_name)
  year_columns <- names(mortality_raw)[str_detect(names(mortality_raw), "^X?[0-9]{4}$")]
  id_column <- intersect(c("iso", "ISO", "country_code", "Country Code"), names(mortality_raw))[1]
  if (length(year_columns) == 0 || is.na(id_column)) {
    stop(file_name, " does not contain recognizable year and country identifier columns.")
  }
  output <- mortality_raw |>
    pivot_longer(
      cols = all_of(year_columns), names_to = "year",
      names_transform = list(year = readr::parse_number), values_to = outcome_name
    ) |>
    transmute(
      iso = as.character(.data[[id_column]]), year = as.integer(year),
      !!outcome_name := .data[[outcome_name]]
    ) |>
    arrange(iso, year)
  assert_unique_key(output, outcome_name)
  output
}

# 1. Mortality outcomes
maternal_mortality <- prepare_mortality("maternal_mortality.csv", "maternal_mortality")
infant_mortality <- prepare_mortality("infant_mortality.csv", "infant_mortality")
neonatal_mortality <- prepare_mortality("neonatal_mortality.csv", "neonatal_mortality")
under5_mortality <- prepare_mortality("under5_mortality.csv", "under5_mortality")
mortality_data <- list(maternal_mortality, infant_mortality, neonatal_mortality, under5_mortality) |>
  reduce(full_join, by = c("iso", "year"))
assert_unique_key(mortality_data, "Combined mortality data")

# 2. Disaster indicators, collapsed to a binary country-year record.
disaster_data <- read_input("disaster.csv") |>
  clean_names() |>
  filter(between(year, 2000L, 2019L), disaster_type %in% c("Earthquake", "Drought"), !is.na(iso)) |>
  transmute(
    iso = as.character(iso), year = as.integer(year),
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) |>
  group_by(iso, year) |>
  summarise(earthquake = max(earthquake), drought = max(drought), .groups = "drop")
assert_unique_key(disaster_data, "Disaster data")

# The covariates provide the country-year backbone of the analytical data set.
covariates <- read_input("covariates.csv") |>
  clean_names() |>
  mutate(iso = as.character(iso), year = as.integer(year))
assert_unique_key(covariates, "Covariate data")

# 3. Sum deaths across all conflicts in a country-year, then lag within country.
# "At least 25" deaths is implemented as total annual deaths >= 25.
conflict_annual <- read_input("conflict.csv") |>
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
assert_unique_key(conflict_data, "Lagged conflict data")

# 4. Merge all prepared data. Missing disaster records indicate no recorded event.
final_data <- covariates |>
  left_join(mortality_data, by = c("iso", "year")) |>
  left_join(conflict_data, by = c("iso", "year")) |>
  left_join(disaster_data, by = c("iso", "year")) |>
  mutate(earthquake = coalesce(earthquake, 0L), drought = coalesce(drought, 0L)) |>
  arrange(iso, year)
assert_unique_key(final_data, "Final analytical data")

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write_csv(final_data, output_file, na = "")

# 5. Quality-control checks
cat("\n--- Coding-agent final dataset quality checks ---\n")
cat("Dimensions:", nrow(final_data), "rows x", ncol(final_data), "columns\n")
cat("Column names:\n", paste(names(final_data), collapse = ", "), "\n", sep = "")
cat("First six rows:\n"); print(head(final_data))
cat("Duplicate country-year combinations:", final_data |> count(iso, year) |> filter(n > 1) |> nrow(), "\n")
cat("Year range:", paste(range(final_data$year, na.rm = TRUE), collapse = " to "), "\n")
cat("Number of countries:", n_distinct(final_data$iso), "\n")
cat("Frequency: armed_conflict\n"); print(table(final_data$armed_conflict, useNA = "ifany"))
cat("Frequency: earthquake\n"); print(table(final_data$earthquake, useNA = "ifany"))
cat("Frequency: drought\n"); print(table(final_data$drought, useNA = "ifany"))
cat("Missing values in exposure variables:\n")
print(colSums(is.na(final_data[c("armed_conflict", "earthquake", "drought")])))
cat("One observation per country-year:", if_else(nrow(final_data) == n_distinct(paste(final_data$iso, final_data$year)), "PASS", "FAIL"), "\n")
cat("Coding-agent final dataset saved to data/processed/final_data_codex.csv\n")
