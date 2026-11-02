suppressPackageStartupMessages({ library(tidyverse); library(ggplot2); library(dplyr); library(table1); library(labelled) })
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
label(final_data_labeled$earthquake) <- "Earthquake"

final_data_labeled$drought <- 
  factor(final_data_labeled$drought, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
label(final_data_labeled$drought) <- "Drought"

final_data_labeled$oecd <- 
  factor(final_data_labeled$oecd, levels=c(1,0),
         labels=c("Yes", 
                  "No"))
label(final_data_labeled$oecd) <- "OECD membership"

label(final_data_labeled$maternal_mortality) <- "Maternal mortality"
label(final_data_labeled$gdp_1000) <- "GDP per capita*"
label(final_data_labeled$rainfall_1000) <- "Rainfall (mm/year)"
label(final_data_labeled$temp) <- "Temperature"
label(final_data_labeled$pop_dens) <- "Population density"
label(final_data_labeled$age_dep) <- "Age dependency ratio"
label(final_data_labeled$male_edu) <- "Male education"
label(final_data_labeled$urban) <- "Urban residence"

final_data_labeled$armed_conflict <- 
  factor(final_data_labeled$armed_conflict, levels=c(1,0),
         labels=c("Armed Conflict Present", 
                  "Armed Conflict Not Present"))
label(final_data_labeled$armed_conflict) <- "Armed conflict"

caption <- "Table 1. Description of country characteristics by presence of an amred conflict"
footnote <- "*GDP per capita scaled up by 1,000, and is in USD, OECD = Organisation for Economic Co-operation and Development "

#final optimized table
table1(~ maternal_mortality + earthquake + drought + gdp_1000 + rainfall_1000 + temp + urban + pop_dens + oecd + age_dep + male_edu | 
  armed_conflict, data=final_data_labeled, footnote = footnote, caption = caption)




