### read in all data sets
install.packages("here")
install.packages("countrycode")
library(here)
library(tidyverse)
library(countrycode)

matmor0 <- read.csv(here("data", "raw", "maternal_mortality.csv"), header = TRUE)
infmor0 <- read.csv(here("data", "raw", "infant_mortality.csv"), header = TRUE)
neomor0 <- read.csv(here("data", "raw", "neonatal_mortality.csv"), header = TRUE)
un5mor0 <- read.csv(here("data", "raw", "under5_mortality.csv"), header = TRUE)


matmor0$iso <- countrycode(matmor0$Country.Name, 
                          origin = "country.name", 
                          destination = "iso3c") 
#renaming , but it was already name this
matmor <- matmor0 |>
  dplyr::mutate(country = Country.Name, indicator = Indicator.Name) |>
  dplyr::select(iso, country, indicator, X2000:X2019)

un5mor0$iso <- countrycode(un5mor0$Country.Name, 
                           origin = "country.name", 
                           destination = "iso3c") 
un5mor <- un5mor0 |>
  dplyr::mutate(country = Country.Name, indicator = Indicator.Name) |>
  dplyr::select(iso, country, indicator, X2000:X2019)

write.csv(matmor, here("data", "raw", "maternal_mortality.csv"), row.names = F)


#trying it out. my code ####
#calling upon iso since that is what we have already 
matmor0$Country.Name <- countrycode(matmor0$iso, 
                          origin = "iso3c", 
                          destination = "country.name") 

un5mor0$Country.Name <- countrycode(un5mor0$iso, 
                           origin = "iso3c", 
                           destination = "country.name") 

write.csv(matmor, here("data", "raw", "maternal_mortality.csv"), row.names = F)

get_country_name <- function(x){
  x$Country.Name <- countryname(x$iso,
  origin = "iso3c",
  destination = "country.name")
  x <- x %>%
    dplyr::select(Country.Name, X2000:X2019) 
}




### write a function that does the above manipulation to each data
wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(Country.Name, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(Country.Name, year)
}

matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

#put all data frames into list
wblist <- list(matmor, infmor, neomor, un5mor)

#merge all data frames in list
wblist |> reduce(full_join, by = c('Country.Name', 'year')) -> wbdata


# add ISO-3 to data
wbdata$ISO <- countrycode(wbdata$Country.Name, 
                          origin = "country.name", 
                          destination = "iso3c")
wbdata <- wbdata |>
  dplyr::select(-Country.Name)

