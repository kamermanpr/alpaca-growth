############################################################
#                                                          #
#         Secondary analysis: metabolic markers            #
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

#-- Question: relationship between mass change and metabolic markers --#
# Base model
mod_base <- lme(mass_change_kg ~ min_oC +
                  amplitude_oC +
                  month_counter +
                  BG_mmol.l +
                  insulin_uU.ml +
                  leptin_ng.ml,
                random = ~1|name,
                data = data)

## Get model summary
summary(mod_base)

## Get 95% CI for the estimates
intervals(mod_base)

## Check model assumption (use lme4 model)
check_model(lmer(mass_change_kg ~ min_oC +
                   amplitude_oC +
                   month_counter +
                   BG_mmol.l +
                   insulin_uU.ml +
                   leptin_ng.ml +
                   (1|name),
                 data = data),
            check = c('vif', 'qq', 'normality', 'ncv',
                      'homogeneity', 'outliers', 'reqq'))

# Plots
## Generate interaction plots (use lme4 model)
plots <- plot_model(lmer(mass_change_kg ~ min_oC +
                           amplitude_oC +
                           month_counter +
                           BG_mmol.l +
                           insulin_uU.ml +
                           leptin_ng.ml +
                           (1|name),
                         data = data),
                    type = 'pred')

## Tailor plots
### Blood glucose
plot_BG <- plots[[4]] +
    labs(title = 'Predicted values',
         subtitle = 'Change in body mass vs blood glucose concentration (95% CI)*',
         caption = '*Controlling for: minimum and amplitude in body temperature,\nmonth, insulin concentration, and leptin concentration',
         x = expression('Blood glucose (mmol.l'^-1*')'),
         y = 'Change in body mass (kg)')

## Insulin
plot_insulin <- plots[[5]] +
    labs(title = NULL,
         subtitle = 'Change in body mass vs insulin concentration (95% CI)**',
         caption = '**Controlling for: minimum and amplitude in body temperature,\nmonth, blood glucose concentration, and leptin concentration',
         x = expression('Insulin concentration ('*mu*'U.ml'^-1*')'),
         y = 'Change in body mass (kg)')

## Leptin
plot_leptin <- plots[[6]] +
  labs(title = NULL,
       subtitle = 'Change in body mass vs leptin concentration (95% CI)***',
       caption = '***Controlling for: minimum and amplitude in body temperature,\nmonth, blood glucose concentration, and insulin concentration',
       x = expression('Leptin concentration (ng.ml'^-1*')'),
       y = 'Change in body mass (kg)')

plots <- plot_BG + plot_insulin + plot_leptin + plot_layout(ncol = 1)

ggsave(filename = 'figures/supplementary-figure-4.png',
       plot = plots,
       width = 7,
       height = 15)
