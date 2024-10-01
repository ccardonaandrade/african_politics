## ---------------------------
## Script name: African Coups and Polity IV
## Author: Carlos Cardona
## Date Created: `r paste(Sys.Date())`
## ---------------------------
## set working directory
setwd("C:/Users/ccard/OneDrive/Documentos/GitHub/african_politics")     # Tim's working directory (PC)
## ---------------------------
## load up the packages we will need:  (uncomment as required)

library(tidyverse)
# This package comes from here. Looks cool
# https://github.com/xmarquez/democracyData
library(democracyData)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)
library(readxl)
library(janitor)
library(fixest)
library(broom)



###########
# POLITY V
###########

# Uploading the PolityV dataset using `democracyData`package
polity5 <- download_polity_annual()

# Let's stay just with African countries
polity5$continent <- countrycode(polity5$cown, origin = 'cown', destination = 'continent')
polity5 <- polity5 %>% filter(continent=="Africa")


# I am just interested in a few variables
polity5 <- polity5 %>% select(polity_annual_country, year, democ,autoc,polity,polity2,cown,continent)



##############
# Coup Dataset
##############


# Uploading the coup dataset
# https://militarycoups.org/
coupdata <- read_csv("data/cam_wide_3.0.csv")


# Let's again keep African countries
coupdata$continent <- countrycode(coupdata$cowcode, origin = 'cown', destination = 'continent')
coupdata <- coupdata %>% filter(continent=="Africa")


# I just want to know if there was a coup
coupdata <- coupdata %>% select(cowcode, year, coup1:coup4)
coupdata <- coupdata %>% rename(cown=cowcode)


# Merging both datasets
african_politics <- left_join(polity5,coupdata)


# Keeping year>1965

african_politics <- african_politics %>% 
  filter(year>1964)

# Dropping Sao Tome
# Check this again later!
african_politics <- african_politics %>% 
  filter(!is.na(continent))

# Redefining coup
african_politics <- african_politics %>%
  mutate(across(c(coup1, coup2, coup3, coup4), ~ if_else(is.na(.), 0, .)))


# Running a simple regression to check the relationship between coups and the polity score
regression <- feols(polity ~ coup1 | year + cown, data = african_politics)
# Check results
tidy(regression)


# Checking for the distribution of the polity score
# It seems that polity 2 is fine.
ggplot(african_politics, aes(x=polity2)) + geom_histogram()

african_politics <- african_politics %>% rename(country_name=polity_annual_country)

save(african_politics, file="clean/african_politics.R")
