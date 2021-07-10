############################################################
#                                                          #
#                     Primary analysis                     #
#                                                          #
############################################################

#-- Load packages --#
library(dplyr)
library(readr)
library(nlme)
library(lme4)
library(performance)
library(sjPlot)
library(ggplot2)
library(patchwork)

#-- Plot theme --#
theme_set(new = theme_bw(base_size = 18) +
              theme(panel.grid = element_blank(),
                    legend.position = c(0.15, 0.85),
                    legend.title = element_blank(),
                    plot.title = element_text(size = 18),
                    plot.subtitle = element_text(size = 14),
                    plot.caption = element_text(size = 12),
                    axis.text = element_text(colour = '#000000')))

#-- Import and process data --#
data <- read_csv('data-cleaned/data.csv') %>%
    # Extract March to October data
    filter(month_counter >= 0) %>%
    # Calculate change from baseline for body mass
    group_by(name) %>%
    mutate(mass_change_kg = mass_kg - mass_kg[1L]) %>%
    ungroup() %>%
    # Arrange columns
    relocate(mass_change_kg, .after = mass_monthly_gain_kg)

#-- Question: Body mass vs Tb_minimum, Tb_amplitude, and time of year --#
# Build model
mod <- lme(mass_change_kg ~ min_oC +
               amplitude_oC +
               month_counter,
           random = ~1|name,
           data = data)

# Get model summary
summary(mod)

# Get 95% CI for the estimates
intervals(mod)

#-- Check model diagnostics --#
# (Needs a lme4 object)
mod_lme4 <- lmer(mass_change_kg ~ min_oC +
                     amplitude_oC +
                     month_counter +
                     (1|name),
                 data = data)

check_model(mod_lme4)

#-- Plots --#
# Generate interaction plots
plots <- plot_model(mod_lme4, type = 'pred')

# Tailor plots
## Minimum body temperature plot
plot_min <- plots[[1]] +
    labs(title = 'Predicted values',
         subtitle = 'Change in body mass vs minimum body temperature (95% CI)*',
         caption = '*Controlling for: body temperature amplitude and month',
         x = expression('Minimum body temperature ('*degree*'C)'),
         y = 'Change in body mass (kg)')

## Amplitude in body temperature
plot_amp <- plots[[2]] +
    labs(title = NULL,
         subtitle = 'Change in body mass vs body temperature amplitude (95% CI)**',
         caption = '**Controlling for: minimum body temperature and month',
         x = expression('Body temperature amplitude ('*degree*'C)'),
         y = 'Change in body mass (kg)') +
    theme(legend.position = 'none')

## Month
plot_month <- plots[[3]] +
    labs(title = NULL,
         subtitle = 'Change in body mass vs months (95% CI)***',
         caption = '***Controlling for: body temperature amplitude and minimum body temperature',
         x = 'Months since March',
         y = 'Change in body mass (kg)') +
    theme(legend.position = 'none')

# Piece plots together
plots_combined <- plot_min + plot_amp + plot_month + plot_layout(ncol = 1)

# Save plot
ggsave(filename = 'figures/supplementary-figure-2.png',
       plot = plots_combined,
       width = 7,
       height = 15)
