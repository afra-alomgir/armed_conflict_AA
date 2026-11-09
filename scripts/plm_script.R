install.packages("plm")
install.packages("stargazer")
suppressPackageStartupMessages({ library(tidyverse); library(ggplot2); library(dplyr); library(table1); library(plm); library(stargazer) })

#loading in dataset
final_data <- read.csv("./data/processed/final_data_codex.csv", header = TRUE)

#new log transformed variable 
summary(final_data$gdp_1000) #check for 0 and negative values
final_data$log_gdp <- log(final_data$gdp_1000)

#panel regressions
preds <- as.formula("~ armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")
maternal_mod <- plm(
  update.formula(preds, maternal_mortality ~ .), #same predictors for all 
  index = c("iso", "year"),
  effect = "twoways", #includes country and year fixed effects
  model = "within", #fixed effects estimator
  data = final_data
)

under5_mod <- plm(
  update.formula(preds, under5_mortality ~ .), 
  index = c("iso", "year"),
  effect = "twoways", 
  model = "within", 
  data = final_data
)

infant_mod <- plm(
  update.formula(preds, infant_mortality ~ .), 
  index = c("iso", "year"),
  effect = "twoways", 
  model = "within", 
  data = final_data
)

neonatal_mod <- plm(
  update.formula(preds, neonatal_mortality ~ .), 
  index = c("iso", "year"),
  effect = "twoways", 
  model = "within", 
  data = final_data
)


#table with only binary armed conflict 
#this means there will be 4 columns for each mortality outcome

#regression table
stargazer(
  maternal_mod,
  under5_mod,
  infant_mod,
  neonatal_mod,
  type = "html",
  title = "Table 2. Association Between Binary Armed Conflict and Maternal and Child Mortalities (Adjusted Beta Coefficients, Standard Error)",
  column.labels = c("Maternal (per 100,000 live births)", "Under-5 (per 1,000 live births)", "Infant (per 1,000 live births)", "Neonatal (per 1,000 live births)"),
  dep.var.labels = " ",
  dep.var.caption = "Mortality",
  model.numbers = FALSE,
  notes = "GDP: gross domestic product; OECD: Organisation for Economic Co-operation and Development.",
  covariate.labels = c(
    "Armed conflict (yes vs no)",
    "Log GDP (USD)",
    "OECD (yes vs no)",
    "Population density",
    "Urban population",
    "Age dependency",
    "Male education",
    "Mean annual temperature",
    "Mean annual rainfall",
    "Earthquake (yes vs no)",
    "Drought (yes vs no)"
  ),
  digits = 3,
  out = "table_02_regression_table_loggdp.html"
)






