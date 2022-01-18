library(tidyverse)
library(texreg)
library(survival)
library(stargazer)
library(survminer)
library(ggalt)
library(readxl)

load("~/Downloads/Arms_and_conflict-main/arms_trade_africa.RData")

# Note, if you decide to use an xlsx file instead of RData, convert factor variables into numeric. Otherwise the model won't work.
## What we are doing is filtering the data so that when the event of interest occurs (in this case is the conflict_end variable) the following periods of time are replaced with NAs.
arms_trade_africa_b <- arms_trade_africa %>% group_by(country) %>%
  # that the cumulative sum of the dummy is maximum 1
  filter(cumsum(conflict_end) <= 1) %>%
  ungroup()

# Adapt a dataset to create a Gant plot
gantt_plot_df <- arms_trade_africa_b %>%
  # the variables we are interested in
  dplyr::select(country, conflict_year, conflict_end) %>% group_by(country) %>%
  filter(conflict_year == min(conflict_year) | conflict_year == max(conflict_year)) %>%
  # we need to remove the observations for countries that "are born" with conflict:
  filter(!(conflict_year == min(conflict_year) & conflict_end == 1)) %>% summarize(year_enters = min(conflict_year), year_exits = max(conflict_year),
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
arms_trade_africa_c <- arms_trade_africa_b %>%
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
              data = arms_trade_africa_c)

# Plot the curve
ggsurvplot(km, data = arms_trade_africa_c,
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
    mcw_tiv + magarea + magfight + mcw_tiv:magarea:magfight,
  data = arms_trade_africa_c,
  method = "breslow"
)

test_cox_m1 <- cox.zph(cox_m1) # Its p-values are much greater than the 0.05 cutoff. Cox fits.
test_cox_m1
ggcoxzph(test_cox_m1, point.col = "black")

# Model 2 WITH ECONOMIC CONTROLS
cox_m2 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + magarea + magfight + mcw_tiv:magarea:magfight +
    log10(pop) + log10(gdppc),
  data = arms_trade_africa_c,
  method = "breslow"
)

test_cox_m2 <- cox.zph(cox_m2) # Global p-value is much greater than the cutoff 0.05.
test_cox_m2
ggcoxzph(test_cox_m2, point.col = "black")

# Model 3 WITH ECONOMIC AND GEO CONTROLS
cox_m3 <- coxph(
  Surv(risk_time_at_start, risk_time_at_end, conflict_end) ~
    mcw_tiv + magarea + magfight + mcw_tiv:magarea:magfight +
    log10(pop) + log10(gdppc) +
    politgeog_design,
  data = arms_trade_africa_c,
  method = "breslow"
)

test_cox_m3 <-cox.zph(cox_m3)
test_cox_m3
ggcoxzph(test_cox_m2, point.col = "black")

# Global p-value is greater than the cutoff, therefore there's no violation of the proportional hazards assumption. So, Cox fits.
## Combine all models together with "texreg".
### Sadly, Russian transcript is unreadable :(( Apparently, this version of R doesn't support it.

all_models <- list(cox_m1, cox_m2, cox_m3)
coef_names <- c("MCW Import (TIV)",
                "Area Affected",
                "Rebel Force",
                "MCW Import x Area x Rebel Force",
                "Population (ln)",
                "GDP per capita (ln)",
                "Political Geography"
                )

# To save a model as an HTML file use "htmlreg" instead of "screenreg" and write file="arms_to_africa_cox.html"
screenreg(
  all_models,
  custom.model.names = c("Model 1", "Model 2", "Model 3"),
  custom.coef.names = coef_names,
  custom.header = list("DV: Conflict Duration" = 1:3),
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
)
