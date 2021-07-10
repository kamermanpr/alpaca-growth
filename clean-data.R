############################################################
#                                                          #
#                    Clean alpaca data                     #
#                                                          #
############################################################
#-- Load packages --#
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(readr)

#-- Create folders, if required --#
if(!dir.exists('data-cleaned')){
    dir.create('data-cleaned')
}

#-- Import data --#
# Main data
data <- read_xlsx('data-original/data.xlsx')

# Weather data
weather <- read_xlsx('data-original/weather.xlsx')

# Bagel's data
bagel <- read_xlsx('data-original/bagel.xlsx')

#######################
#   Clean main data   #
#######################

# Inspect main data
dim(data)
names(data)
head(data)
tail(data)
glimpse(data)

# Remove unwanted leptin columns and R2 column
data_2 <- data %>%
    select(-starts_with('Leptin_'), -R2)

# Rename column to give units of measure
data_3 <- data_2 %>%
    rename(name = Name,
           status = Status,
           date = Date,
           mesor_oC = Mesor,
           amplitude_oC = Ampl,
           min_oC = Min,
           max_oC = Max,
           acrophase_hms = Acrophase,
           mass_kg = Mass,
           BG_mmol.l = BG,
           insulin_uU.ml = Insulin,
           leptin_ng.ml = Leptin)

# Convert acrophase
data_4 <- data_3 %>%
    mutate(acrophase_hms = hms(paste(hour(acrophase_hms), ':',
                                     minute(acrophase_hms), ':00')),
           acrophase_decimal_hours = period_to_seconds(acrophase_hms) / 3600) %>%
    relocate(acrophase_decimal_hours, .after = acrophase_hms) %>%
    select(-acrophase_hms)

# Add a month counter
data_5 <- data_4 %>%
    group_by(name) %>%
    mutate(month_counter = -2:7) %>%
    ungroup() %>%
    relocate(month_counter, .after = date)

# Add mass_gain_kg column (gain from previous month)
data_6 <- data_5 %>%
    group_by(name) %>%
    mutate(mass_monthly_gain_kg = mass_kg - lag(mass_kg, n = 1)) %>%
    relocate(mass_monthly_gain_kg, .after = mass_kg) %>%
    ungroup()

# Remove "Jackson". Issues with calibration
data_7 <- data_6 %>%
    filter(name != 'Jackson')

# Check data
glimpse(data_7)

# Save cleaned data
write_csv(data_7, 'data-cleaned/data.csv')

##########################
#   Clean weather data   #
##########################

# Inspect main data
dim(weather)
names(weather)
head(weather)
tail(weather)
glimpse(weather)

# Remove unwanted THI column
weather_2 <- weather %>%
    select(-7)

# Rename column to give units of measure
weather_3 <- weather_2 %>%
    rename(date_time = DT,
           air_temperature_oC = `AIRT DegC`,
           relative_humidity_percent = `RH  %`,
           solar_radiation_kj.m2 = `SOLAR kjm*m`,
           rainfall_mm= `RAIN  mm`,
           windspeed_m.s = `WIND m/s`)

# Create separate date and time columns
weather_4 <- weather_3 %>%
    mutate(extra = date_time) %>%
    separate(col = extra,
             into = c('date', 'time'),
             sep = ' ') %>%
relocate(date, time, .after = date_time)

# Check data
glimpse(weather_4)

# Save cleaned data
write_csv(weather_4, 'data-cleaned/weather.csv')

#######################
#  Clean Bagel data   #
#######################

# Inspect main data
dim(bagel)
names(bagel)
head(bagel)
tail(bagel)
glimpse(bagel)

# Rename column to give units of measure
bagel_2 <- bagel %>%
    rename(date_time = DT,
           body_temperature_oC = `Bagel core`)

# Save cleaned data
write_csv(bagel_2, 'data-cleaned/bagel.csv')

