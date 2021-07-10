############################################################
#                                                          #
#     Monthly descriptive statistics for all measures      #
#                                                          #
############################################################

#-- Load packages --#
library(dplyr)
library(readr)
library(lubridate)
library(skimr)

#-- My skimr --#
my_skim <- skim_with(numeric = sfl(hist = NULL),
                     base = sfl(n_missing = n_missing,
                                n_complete = n_complete))

#-- Load data --#
data <- read_csv('data-cleaned/data.csv')

weather <- read_csv('data-cleaned/weather.csv')

#-- skim core data --#
# Average monthly data
data %>%
    # Extract 1 March to 31 October 2010 data
    filter(date >= as.Date('2010-03-01') & date < as.Date('2010-11-01')) %>%
    # Remove date and animal name
    select(-name, -date) %>%
    # Convert castration status to a factor
    mutate(status = factor(status)) %>%
    # Group by month (-2 = January)
    group_by(month_counter) %>%
    # Summarise
    my_skim()

# Average weight gain
data %>%
    # Extract 1 March to 31 October 2010 data
    filter(date >= as.Date('2010-03-01') & date < as.Date('2010-11-01')) %>%
    # Remove date and animal name
    select(name, date, mass_kg) %>%
    # Group by name
    group_by(name) %>%
    # Calculate change in weight from March
    mutate(cumulative_mass_gain_kg = mass_kg - mass_kg[[1L]]) %>%
    # Add month_counter
    mutate(month_counter = 0:7) %>%
    ungroup() %>%
    select(-name, -mass_kg, -date) %>%
    # Group by month (-2 = January)
    group_by(month_counter) %>%
    # Summarise
    my_skim()

#-- Skim weather data --#
## Average monthly data
weather %>%
    # Extract 1 March to 31 October 2010 data
    filter(date >= as.Date('2010-03-01') & date < as.Date('2010-11-01')) %>%
    # Group by date and then summarise
    group_by(date) %>%
    summarise(across(.cols = 3:7, ~mean(.x))) %>%
    # Extract month
    mutate(month = month(date)) %>%
    mutate(month_counter = month - 3) %>%
    select(-date, -month) %>%
    # Group by month counter
    group_by(month_counter) %>%
    my_skim()

## Full March to October period
weather %>%
    # Extract 1 March to 31 October 2010 data
    filter(date >= as.Date('2010-03-01') & date < as.Date('2010-11-01')) %>%
    # Get daily average
    group_by(date) %>%
    summarise(across(.cols = 3:7, ~mean(.x))) %>%
    # Remove unwanted columns
    select(-date) %>%
    # Summarise
    my_skim()

