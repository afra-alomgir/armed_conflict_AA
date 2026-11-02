suppressPackageStartupMessages({ library(tidyverse); library(ggplot2); library(dplyr); library(table1); library(kableExtra)})
install.packages("webshot2")
library(webshot2)
#Assignment 1

#loading in datasets
final_data <- read.csv("./data/processed/final_list.csv", header = TRUE)
final_data_codex <- read.csv("./data/processed/final_data_codex.csv", header = TRUE)

#quick look at what the raw table looks like
table1(~ maternal_mortality + factor(earthquake) + factor(drought) + gdp_1000 + rainfall_1000 + temp + urban + pop_dens + factor(oecd) + age_dep + male_edu | 
  armed_conflict, data=final_data_codex)

#creating new dataset for labelling 
final_data_labeled <- final_data_codex

final_data_labeled$earthquake <- 
  factor(final_data_labeled$earthquake, levels=c(1,0),
         labels=c("Yes", 
                  "No"))

final_data_labeled$drought <- 
  factor(final_data_labeled$drought, levels=c(1,0),
         labels=c("Yes", 
                  "No"))

final_data_labeled$oecd <- 
  factor(final_data_labeled$oecd, levels=c(1,0),
         labels=c("Yes", 
                  "No"))

final_data_labeled$armed_conflict <- 
  factor(final_data_labeled$armed_conflict, levels=c(1,0),
         labels=c("Armed Conflict Present", 
                  "Armed Conflict Absent"))

label(final_data_labeled$maternal_mortality) <- "Maternal mortality"
label(final_data_labeled$neonatal_mortality) <- "Neonatal mortality"
label(final_data_labeled$infant_mortality) <- "Infant mortality"
label(final_data_labeled$under5_mortality) <- "Under-5 mortality"
label(final_data_labeled$earthquake) <- "Earthquake"
label(final_data_labeled$drought) <- "Drought"
label(final_data_labeled$gdp_1000) <- "GDP per capita* (USD)"
label(final_data_labeled$rainfall_1000) <- "Mean annual rainfall (mm/year)"
label(final_data_labeled$temp) <- "Mean annual temperature (Celsius)"
label(final_data_labeled$urban) <- "Urban residence"
label(final_data_labeled$pop_dens) <- "Population density"
label(final_data_labeled$oecd) <- "OECD membership"
label(final_data_labeled$age_dep) <- "Age dependency ratio"
label(final_data_labeled$male_edu) <- "Male education"
label(final_data_labeled$armed_conflict) <- "Armed conflict"

caption <- "Table 1. Characteristics of Country-Year Observations by Armed Conflict Status"
footnote <- "*GDP per capita scaled up by 1,000, OECD = Organisation for Economic Co-operation and Development "

#final optimized table
table_01_armed_conflict <- table1(~ maternal_mortality + neonatal_mortality + infant_mortality + under5_mortality + earthquake + drought + gdp_1000 + rainfall_1000 + temp + urban + pop_dens + oecd + age_dep + male_edu | 
  armed_conflict, data=final_data_labeled, footnote = footnote, caption = caption)

t1_latex <- t1kable(table_01_armed_conflict)

t1_latex %>% save_kable("table_01_armed_conflict.pdf")
