install.packages("mice")
install.packages("modelsummary")
suppressPackageStartupMessages({ library(tidyverse); library(ggplot2); library(dplyr); library(mice); library(modelsummary) })

#loading in dataset from plm_script.R for log_gdp
source("scripts/plm_script.R")

#iso and year r fixed effects so dont need to show. just show outcome and relevant covariates ig

#convert iso to numeric 
micedata <- final_data %>%
  mutate(iso_num = as.numeric(as.factor(final_data$iso))) %>%
  select(-iso)

#dry run
mice0 <- mice(micedata, seed = 100, m=5, maxit = 0, print = F)

#meth and pred
meth <- mice0$method
meth[c("log_gdp", "pop_dens", "urban" , "age_dep" , "male_edu" , "temp" , "rainfall_1000" , "earthquake" , "drought", "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("log_gdp", "pop_dens", "urban" , "male_edu" , "temp" , "rainfall_1000" , "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality"), "iso_num"] <- -2

#MI with 10 imputations and 20 max imputations 
mice.multi.out <- mice(micedata, seed = 100, m=10, maxit = 20, method = meth, predictorMatrix = pred)

#plotting to check convergence
plot(mice.multi.out)
save(mice.multi.out, file = "outputs//mice_out.RData")

#fitting four regression models

#maternal mortality
fit_mi_matmor <- with(mice.multi.out, 
  lm(maternal_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
    as.factor(iso_num) + as.factor(year)))

out_mi_matmor <- pool(fit_mi_matmor)

#neonatal mortality
fit_mi_neonatmor <- with(mice.multi.out, 
  lm(neonatal_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
    as.factor(iso_num) + as.factor(year)))

out_mi_neonatmor <- pool(fit_mi_neonatmor)

#under 5 mortality
fit_mi_u5mor <- with(mice.multi.out, 
  lm(under5_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
    as.factor(iso_num) + as.factor(year)))

out_mi_u5mor <- pool(fit_mi_u5mor)

#infant mortality 
fit_mi_infantmor <- with(mice.multi.out, 
  lm(infant_mortality ~ -1 + armed_conflict + log_gdp + oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
    as.factor(iso_num) + as.factor(year)))

out_mi_infantmor <- pool(fit_mi_infantmor)

#making results for table
neonatmor_mi_results <- summary(out_mi_neonatmor, conf.int = TRUE)
u5mor_mi_results <-summary(out_mi_u5mor, conf.int = TRUE)
infantmor_mi_results <-summary(out_mi_infantmor, conf.int = TRUE)
matmor_mi_results <- summary(out_mi_matmor, conf.int = TRUE)


#creating table using model summary 

models_mi <- list(
  "Maternal [MIA]" = out_mi_matmor,
  "Under-5 [MIA]" = out_mi_u5mor,
  "Infant [MIA]" = out_mi_infantmor,
  "Neonatal [MIA]" = out_mi_neonatmor,
  "Maternal [CCA]" = maternal_mod,
  "Under-5 [CCA]" = under5_mod,
  "Infant [CCA]" = infant_mod,
  "Neonatal [CCA]" = neonatal_mod
)

modelsummary(
  models_mi,
  coef_omit = "iso_num|year",
  estimate = "{estimate}{stars}",
  gof_omit = ".*",
  statistic = "({conf.low}, {conf.high})", 
  fmt = 3,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  coef_map = c(
    "armed_conflict" = "Armed conflict (yes vs no)",
    "log_gdp" = "Log GDP (USD)",
    "oecd" = "OECD (yes vs no)",
    "pop_dens" = "Population density",
    "urban" = "Urban population",
    "age_dep" = "Age dependency",
    "male_edu" = "Male education",
    "temp" = "Mean annual temperature",
    "rainfall_1000" = "Mean annual rainfall",
    "earthquake" = "Earthquake (yes vs no)",
    "drought" = "Drought (yes vs no)"
  ),
  title = "Table 3. Complete Case Analysis (CCA) and Multiple Imputation Analysis (MIA) Regression Coefficients (95% CI) Comparison for Armed Conflict Data",
  notes = "Estimates are presented as regression coefficients with 95% confidence intervals (CIs). All models were adjusted for armed conflict, gross domestic product (GDP), Organisation for Economic Co-operation and Development (OECD) membership, population density, urban population, age dependency, male education, mean annual temperature, mean annual rainfall, earthquakes, and droughts. 
  Country and year fixed effects were included in all models but are not displayed. 
  Multiple Imputation Analysis (MIA) estimates were obtained by pooling results across 10 imputed datasets using Rubin's rules. Complete Case Analysis (CCA) estimates were obtained using observations with complete data on the variables included in each model. 
  GDP: gross domestic product; OECD: Organisation for Economic Co-operation and Development.",
  output = "outputs/table_03_MIA_CCA_compare.html"
)


