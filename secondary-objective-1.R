############################################################
#                                                          #
#          Secondary analysis: Castration status           #
#                                                          #
############################################################

#-- Load packages --#
library(dplyr)
library(readr)
library(nlme)
library(lme4)
library(sjPlot)
library(ggplot2)
library(patchwork)
library(performance)

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

#-- Question: Interaction between predictors and castration status --#
# Base model
# (no interaction with castration status)
mod_base <- lme(mass_change_kg ~ min_oC +
                  amplitude_oC +
                  status +
                  month_counter,
                random = ~1|name,
                data = data)

## Get model summary
summary(mod_base)

## Get 95% CI for the estimates
intervals(mod_base)

## Check model assumption (use lme4 model)
check_model(lmer(mass_change_kg ~ min_oC +
                   amplitude_oC +
                   status +
                   month_counter +
                   (1|name),
                 data = data),
            check = c('qq', 'normality', 'ncv',
                      'homogeneity', 'outliers', 'reqq'))

# Interaction model
# (added interaction between base temperature variables and castration status)
mod_interaction <- lme(mass_change_kg ~ (min_oC + amplitude_oC) * status +
                         month_counter,
                       random = ~1|name,
                       data = data)

## Get model summary
summary(mod_interaction)

## Get 95% CI for the estimates
intervals(mod_interaction)

## Check model assumption (use lme4 model)
check_model(lmer(mass_change_kg ~ (min_oC + amplitude_oC) * status +
                   month_counter +
                   (1|name),
                 data = data),
            check = c('qq', 'normality', 'ncv',
                      'homogeneity', 'outliers', 'reqq'))

# Compare the two models using the likelihood ratio test
# (Need to update the models to use the ML method and not REML method)
anova(update(mod_interaction, . ~ ., method = 'ML'),
      update(mod_base, .~ ., method = 'ML'))

#-- Plots --#
# Generate interaction plots (use lme4 model)
plots <- plot_model(lmer(mass_change_kg ~ (min_oC + amplitude_oC) * status +
                           month_counter +
                           (1|name),
                         data = data), type = 'int')

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

# Piece plots together
plots_combined <- plot_min + plot_amp + plot_layout(ncol = 1)

# Save plot
ggsave(filename = 'figures/supplementary-figure-3.png',
       plot = plots_combined,
       width = 7,
       height = 10)
