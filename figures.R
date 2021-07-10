############################################################
#                                                          #
#                    Publication plots                     #
#                                                          #
############################################################

# Load packages
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(patchwork)

# ggplot theme
theme_set(new = theme_minimal(base_size = 24) +
              theme(axis.line = element_line(size = 0.8),
                    axis.ticks = element_line(size = 0.8),
                    axis.text = element_text(size = 24,
                                             colour = '#000000'),
                    plot.title = element_text(size = 24),
                    panel.grid = element_blank()))

# Set directory for figure outputs
if(!dir.exists('figures')) {
    dir.create('figures')
}

################
#   Figure 1   #
################
data_fig1 <- read_csv('data-cleaned/bagel.csv') %>%
    # Filter date range
    filter(date_time > ymd_hms('2009-12-01 23:30:00') &
               date_time < ymd_hms('2011-02-01 00:00:00'))

plot_fig1 <- ggplot(data = data_fig1) +
    aes(x = date_time,
        y = body_temperature_oC) +
    geom_line() +
    geom_rect(aes(ymin = 35, ymax = 35.3,
                  xmin = ymd_hms('2010-03-01 00:00:00'),
                  xmax = ymd_hms('2010-10-31 23:30:00')),
              fill = '#000000') +
    geom_segment(aes(x = ymd_hms('2009-11-01 00:00:00'),
                     xend = ymd_hms('2009-11-01 00:00:00'),
                     y = 39.5,
                     yend = 40),
                 size = 2,
                 lineend = 'butt',
                 linejoin = 'mitre',
                 arrow = arrow(ends = 'first',
                               type = 'closed',
                               length = unit(0.4, units = 'cm'))) +
    geom_segment(aes(x = ymd_hms('2010-11-30 00:00:00'),
                     xend = ymd_hms('2010-11-30 00:00:00'),
                     y = 39.5,
                     yend = 40),
                 size = 2,
                 lineend = 'butt',
                 linejoin = 'mitre',
                 arrow = arrow(ends = 'first',
                               type = 'closed',
                               length = unit(0.4, units = 'cm'))) +
    annotate(geom = 'text',
             label = 'Shearing',
             y = 40.1,
             x = ymd_hms('2009-11-01 00:00:00'),
             size = 6,
             hjust = 0.3) +
    annotate(geom = 'text',
             label = 'Shearing',
             y = 40.1,
             x = ymd_hms('2010-11-30 00:00:00'),
             size = 6,
             hjust = 0.3) +
    labs(x = 'Month',
         y = expression('Body temperature ('*degree*'C)')) +
    scale_x_datetime(date_breaks = '2 months',
                     date_labels = '%b') +
    scale_y_continuous(limits = c(35, 40.5),
                       expand = c(0, 0)); plot_fig1

ggsave(filename = 'figures/figure-1.png',
       plot = plot_fig1,
       width = 11,
       height = 8)

################
#   Figure 2   #
################
data_fig2 <- read_csv('data-cleaned/data.csv') %>%
    filter(month_counter >= 0)

data_fig2_weather <- read_csv('data-cleaned/weather.csv')

#-- Ambient temperature --#
data_weather <- data_fig2_weather %>%
    # Extract 1 January to 31 December 2010 data
    filter(date >= as.Date('2010-01-01') & date < as.Date('2010-11-01')) %>%
    # Group by date and then summarise
    group_by(date) %>%
    summarise(mean_day = mean(air_temperature_oC)) %>%
    # Extract month
    mutate(month = month(date)) %>%
    mutate(month_counter = month - 3) %>%
    select(-date, -month) %>%
    # Group by month and the summarise again
    group_by(month_counter) %>%
    summarise(mean = mean(mean_day, na.rm = TRUE),
              SD_minus = mean - sd(mean_day, na.rm = TRUE),
              SD_plus = mean + sd(mean_day, na.rm = TRUE)) %>%
    ungroup() %>%
    # Remove January and February data (month_counter = -2 and -1)
    filter(month_counter >= 0)

plot_weather <- ggplot(data = data_weather) +
    aes(x = month_counter,
        y = mean,
        ymin = SD_minus,
        ymax = SD_plus) +
    geom_line(size = 1) +
    geom_errorbar(size = 1, width = 0.2) +
    geom_point(size = 8) +
    labs(x = NULL,
         y = expression('Ambient temperature ('*degree*'C)')) +
    scale_y_continuous(limits = c(5, 30),
                       breaks = seq(5, 30, by = 5),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1, 7, by = 2),
                       labels = NULL) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(t = 5.5, b = 30, l = 5.5, r = 5.5))

#-- Body mass --#
data_mass <- data_fig2 %>%
    group_by(month_counter) %>%
    summarise(mean = mean(mass_kg, na.rm = TRUE),
              SD_minus = mean - sd(mass_kg, na.rm = TRUE),
              SD_plus = mean + sd(mass_kg, na.rm = TRUE)) %>%
    ungroup()

plot_mass <- ggplot(data = data_mass) +
    aes(x = month_counter,
        y = mean,
        ymin = SD_minus,
        ymax = SD_plus) +
    geom_line(size = 1) +
    geom_errorbar(size = 1, width = 0.2) +
    geom_point(size = 8) +
    labs(x = NULL,
         y = 'Body mass (kg)') +
    scale_y_continuous(limits = c(40, 75),
                       breaks = seq(40, 75, by = 5),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1, 7, by = 2),
                       labels = NULL) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(t = 5.5, b = 30, l = 5.5, r = 5.5))

#-- Mass gain --#
data_mass_gain <- data_fig2 %>%
    group_by(month_counter) %>%
    summarise(mean = mean(mass_monthly_gain_kg, na.rm = TRUE),
              SD_minus = mean - sd(mass_monthly_gain_kg, na.rm = TRUE),
              SD_plus = mean + sd(mass_monthly_gain_kg, na.rm = TRUE)) %>%
    ungroup()

plot_mass_gain <- ggplot(data = data_mass_gain) +
    aes(x = month_counter,
        y = mean,
        ymin = SD_minus,
        ymax = SD_plus) +
    geom_line(size = 1) +
    geom_errorbar(size = 1, width = 0.2) +
    geom_point(size = 8) +
    labs(x = NULL,
         y = 'Monthly body mass gain (kg)') +
    scale_y_continuous(limits = c(-0.5, 3),
                       breaks = seq(-0.5, 3, by = 0.5),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1, 7, by = 2),
                       labels = NULL) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(t = 5.5, b = 30, l = 5.5, r = 5.5))

#-- Body temperature --#
data_Tb <- data_fig2 %>%
    group_by(month_counter) %>%
    summarise(mesor_mean = mean(mesor_oC, na.rm = TRUE),
              mesor_SD_minus = mesor_mean - sd(mesor_oC, na.rm = TRUE),
              mesor_SD_plus = mesor_mean + sd(mesor_oC, na.rm = TRUE),
              min_mean = mean(min_oC, na.rm = TRUE),
              max_mean = mean(max_oC, na.rm = TRUE)) %>%
    ungroup()

plot_Tb <- ggplot(data = data_Tb) +
    aes(x = month_counter,
        y = mesor_mean,
        ymin = mesor_SD_minus,
        ymax = mesor_SD_plus) +
    geom_line(size = 1) +
    geom_errorbar(size = 1, width = 0.2) +
    geom_point(size = 8) +
    geom_line(aes(y = min_mean),
              linetype = 2,
              size = 1) +
    geom_line(aes(y = max_mean),
              linetype = 2,
              size = 1) +
    labs(x = NULL,
         y = expression('Body temperature ('*degree*'C)')) +
    scale_y_continuous(limits = c(37.5, 38.5),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1, 7, by = 2),
                       labels = NULL) +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(t = 5.5, b = 30, l = 5.5, r = 5.5))

#-- Amplitude of body temperature --#
data_Tamp <- data_fig2 %>%
    group_by(month_counter) %>%
    summarise(mean = mean(amplitude_oC, na.rm = TRUE),
              SD_minus = mean - sd(amplitude_oC, na.rm = TRUE),
              SD_plus = mean + sd(amplitude_oC, na.rm = TRUE)) %>%
    ungroup()

plot_tamp <- ggplot(data = data_Tamp) +
    aes(x = month_counter,
        y = mean,
        ymin = SD_minus,
        ymax = SD_plus) +
    geom_line(size = 1) +
    geom_errorbar(size = 1, width = 0.2) +
    geom_point(size = 8) +
    labs(x = 'Month',
         y = expression('Body temperature amplitude ('*degree*'C)')) +
    scale_y_continuous(limits = c(0.1, 0.4),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1, 7, by = 2),
                       labels = c('April', 'June', 'August', 'October'))

#-- Patch together --#
fig_2 <- plot_weather + plot_mass + plot_Tb + plot_tamp + plot_layout(ncol = 1)

ggsave(filename = 'figures/figure-2.png',
       plot = fig_2,
       height = 25,
       width = 12)
