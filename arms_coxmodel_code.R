library(tidyverse)
library(texreg)
library(survival)
library(survminer)
library(readxl)
library(ggalt)
library(writexl)

load("~/Africa_arms_estimation/africa_arms.RData")

# Adapt a dataset to create a Gant plot
gantt_plot_df <- africa_arms %>%
  # the variables we are interested in
  dplyr::select(country, conflict_year, conflict_end) %>% group_by(country) %>%
  filter(conflict_year == min(conflict_year) | conflict_year == max(conflict_year)) %>%
  # we need to remove the observations for countries that "are born" with #direct democracy:
  filter(!(conflict_year == min(conflict_year) & conflict_end == 1)) %>% summarize(year_enters = min(conflict_year),                                                                               year_exits = max(conflict_year),
                                                                                   exits_bc_cd = max(conflict_end)) %>% ungroup()
gantt_plot_df %>% filter(exits_bc_cd == 1)

gantt_plot <- ggplot(data = gantt_plot_df,
                     mapping = aes(x = year_enters, xend = year_exits,
                                   y = fct_rev(country),
                                   color = factor(exits_bc_cd))) +
  geom_dumbbell(size_x = 2, size_xend = 2) +
  geom_text(aes(label = year_enters), vjust = -0.4) +
  geom_text(aes(x = year_exits, label = year_exits), vjust = -0.4) +
  labs(x = "year", y = "",
       color = "Does conflict end?") +
  theme(axis.text.x = element_blank()) +
  theme_bw(base_family="Times New Roman")

gantt_plot

# Create a graph of survival curves. We will estimate a non-parametric survival curve, using the Kaplan-Meier method.
africa_arms_b <- africa_arms %>%
  group_by(country) %>%
  # we will eliminate the first year for each country;
  # as it is not at risk of dying yet!
  filter(conflict_year != min(conflict_year)) %>%
  mutate(risk_time_at_end = c(1:n()),
         risk_time_at_start = c(0:(n() - 1))) %>%
  ungroup() %>%
  dplyr::select(country, conflict_year, risk_time_at_start, risk_time_at_end, everything())

km <- survfit(Surv(time = risk_time_at_start, time2 = risk_time_at_end,
                   event=conflict_end) ~ mcw_dummy,
              type = "kaplan-meier",
              conf.type = "log",
              data = africa_arms_b)

# Plot the curve
ggsurvplot(km, data = africa_arms_b,
           conf.int = TRUE, risk.table = FALSE,
           break.x.by = 5,
           legend.labs = c("Поставка ОВ не состоялась = 0",
                           "Поставка ОВ состоялась = 1"), legend.title = "",
           legend = c(0.2, 0.2),
           ggtheme = theme_classic(base_size=25, base_family = "Times New Roman"),
           font.family = "Times New Roman",
           palette = c("#229954", "#F4D03F"),
           xlab = "Продолжительность (в годах)", ylab = c("Вероятность выживания"))

# Build a Cox model
## We assume that the hazards are proportional over time, i.e. the effect of a risk factor is constant over time.
###  If one (or more) of the tested predictors by time interactions reaches statistical significance (e.g., p<0.05), then the assumption of proportionality is violated. Hence, Cox is not appropriate.

## Model 1 NO CONTROLS, INDEPENDENT VARIABLES ONLY
cox_m1 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + major_minor + mcw_tiv:major_minor,
  data = africa_arms_b,
  method = "breslow"
)

test_cox_m1 <- cox.zph(cox_m1) # Its p-values are much greater than the 0.05 cutoff. Cox fits.
test_cox_m1
ggcoxzph(test_cox_m1, point.col = "black")

# Model 2 WITH ECONOMIC CONTROLS (no polity V - sample too small)
cox_m2 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + major_minor + mcw_tiv:major_minor + log10(pop) + log10(gdppc) + doll_perday,
  data = africa_arms_b,
  method = "breslow"
)

test_cox_m2 <- cox.zph(cox_m2) # Global p-value (0.049) is less but very close to the cutoff 0.05.
test_cox_m2
## There is a significant variable in its Chi-squared: gdppc, with a p of 0.024 which causes global test to be violated.
ggcoxzph(test_cox_m2, point.col = "black")

# Model 3 WITH GEO CONTROLS
cox_m3 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + major_minor + mcw_tiv:major_minor + rugged + politgeog_design,
  data = africa_arms_b,
  method = "breslow"
)

test_cox_m3 <- cox.zph(cox_m3) # Global p-value is much greater than the cutoff 0.05, i.e. without problems.
test_cox_m3
ggcoxzph(test_cox_m3, point.col = "black")

# Model 4 WITH ALL CONTROLS
cox_m4 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + major_minor + mcw_tiv:major_minor + log10(pop) + log10(gdppc) + doll_perday + rugged + politgeog_design,
  data = africa_arms_b,
  method = "breslow"
)

test_cox_m4 <- cox.zph(cox_m4)
test_cox_m4
ggcoxzph(test_cox_m4, point.col = "black")

# Global p-value (0.041) of a complete model is little less than 0.05 again because of gdppc.
## We can fix it by adjusting the model.
### One way of solving it is by interacting the problematic variable (gdppc) with the natural logarithm of the temporal variable we created before.
cox_m4_adj <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + major_minor + mcw_tiv:major_minor + log10(pop) + log10(gdppc) + doll_perday + rugged + politgeog_design + log10(gdppc):log(risk_time_at_end),
  data = africa_arms_b,
  method = "breslow" )

cox.zph(cox_m4_adj) # The test no longer shows problems with the proportional hazard assumption.So Cox remains.

# Look at all the models together with "texreg". Sadly, Russian transcript is unreadable :(( Apparently, this version of R doesn't support it.

all_models <- list(cox_m1, cox_m2, cox_m3, cox_m4, cox_m4_adj)
coef_names <- c(
  "MCW Import",
  "Threat",
  "MCW Import х Threat",
  "Population (ln)",
  "GDP per capita (ln)",
  "Poverty (dollar-a-day)",
  "Terrain",
  "Geopolitical design",
  "GDP per capita (ln) x Time at risk(ln)", "", ""
  )

htmlreg(
  all_models,
  custom.model.names = c("Model 1", "Model 2", "Model 3",
                         "Model 4", "Model 4b"),
  custom.coef.names = coef_names,
  custom.header = list("DV: Conflict Duration" = 1:5),
  override.coef = map(all_models, ~ exp(coef(.x))),
  override.se = map(all_models, ~ odds_se(.x)),
  override.pvalues = map(all_models, ~ odds_pvalues(.x)),
  caption.above = "Cox Regression: Proportional Hazards",
  include.aic=FALSE,
  include.zph=TRUE,
  include.missings=FALSE,
  include.rsquared=FALSE,
  include.maxrs=FALSE,
  include.events=FALSE,
  include.phtest=FALSE,
  file = "Arms_Model.html"
  )

all_models <- list(cox_m1, cox_m2, cox_m3, cox_m4)
coef_names <- c(
  "MCW Import",
  "Threat scale",
  "MCW Import x Threat scale",
  "Population (ln)",
  "GDP per capita (ln)",
  "Poverty (dollar-a-day)",
  "Terrain",
  "Geopolitical design", "", ""
)

htmlreg(
  all_models,
  custom.model.names = c("Model 1", "Model 2", "Model 3",
                         "Model 4"),
  custom.coef.names = coef_names,
  custom.header = list("DV: Conflict Duration" = 1:4),
  override.coef = map(all_models, ~ exp(coef(.x))),
  override.se = map(all_models, ~ odds_se(.x)),
  override.pvalues = map(all_models, ~ odds_pvalues(.x)),
  caption = "Cox Regression: Proportional Hazards",
  caption.above=TRUE,
  include.aic=FALSE,
  include.zph=TRUE,
  include.missings=FALSE,
  include.rsquared=FALSE,
  include.maxrs=FALSE,
  include.events=FALSE,
  include.phtest=FALSE,
  file = "Arms_Model.html"
)
