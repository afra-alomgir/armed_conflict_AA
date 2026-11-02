suppressPackageStartupMessages({ library(tidyverse); library(ggplot2); library(dplyr); library(table1); library(labelled) })
#Assignment 2

#loading in dataset
final_data <- read.csv("./data/processed/final_data_codex.csv", header = TRUE)

#isolating countries that experienced an increase in maternal mortality 
final_data_figure <- final_data %>%
  select(iso, year, maternal_mortality) %>%
  filter(year < 2018) %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L]) %>%
  filter(
    first(year) == 2000,
    last(year) == 2017,
    last(diff_matmor) > 0
  ) %>%
  ungroup()
#I am including countries that had a net positive in matmor, even if there was fluctuation between the years

#line plot that shows increase between 2000-2017 including the fluctutations between the years
ggplot(data = final_data_figure, aes(x = year, y = maternal_mortality, colour = iso, group = iso)) + 
  geom_line() + geom_point(size = 1) + theme_minimal() +
  labs(
    title = "Figure 1. Increasing Maternal Mortality Trends between 2000-2017",
    x = "Year",
    y = "Maternal Mortality (log[10] scale)",
    color = "Country") + 
  scale_y_log10()
