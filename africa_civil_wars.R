##############################
# COX PROPRTIONAL HAZARDS MODEL
##############################

library(tidyverse)
library(survival)
library(survminer)
library(stargazer)

# LOADING AFRICA FILE

load("~/Downloads/Arms_and_conflict-main/africa_civil_wars.RData")

africa_civil_wars <- africa_civil_wars %>% mutate(coef_desert=as.factor(coef_desert),
                                                  coef_tropical=as.factor(coef_tropical),
                                                  coef_milgdpshare=as.integer(coef_milgdpshare),
                                                  intensity=as.factor(intensity),
                                                  mcw_tiv_log10=log10(mcw_tiv))

# MODEL 1. MILITARY CAPABILITIES
cox_m1 <- coxph(
  Surv(duration_d, status) ~
    mcw_tiv_log10 + milper_log10 + mil_gdpshare,
  data = africa_civil_wars,
  method = "breslow"
)

# MODEL 2. THREAT SCALE
cox_m2 <- coxph(
  Surv(duration_d, status) ~
    distance_log10 + troopratio_log10 + coef_area,
  data = africa_civil_wars,
  method = "breslow"
)

# MODEL 3 MILITARY CAPABILITIES AND THREAT SCALE
cox_m3 <- coxph(
  Surv(duration_d, status) ~
    mcw_tiv_log10 + milper_log10 + mil_gdpshare +
    distance_log10 + troopratio_log10 + coef_area,
  data = africa_civil_wars,
  method = "breslow"
)

# MODEL 4. MILITARY CAPABILITIES, THEAT SCALE AND GEORAPHY
cox_m4 <- coxph(
  Surv(duration_d, status) ~
    mcw_tiv_log10 + milper_log10 + mil_gdpshare +
    distance_log10 + troopratio_log10 + coef_area +
    coef_tropical + coef_desert,
  data = africa_civil_wars,
  method = "breslow"
)

# MODEL 5. TOTAL
cox_m5 <- coxph(
  Surv(duration_d, status) ~
    mcw_tiv_log10 + milper_log10 + mil_gdpshare +
    distance_log10 + troopratio_log10 + coef_area +
    coef_tropical + coef_desert +
    gdppc_log10 + pop_log10,
  data = africa_civil_wars,
  method = "breslow"
)

# STARGAZER TABLE
all_models <- list(cox_m1, cox_m2, cox_m3, cox_m4, cox_m5)

stargazer(all_models, type="text")

var_names_eng <- c(
  mcw_tiv_log10 = "MCW Imports (ln)",
  milper_log10 = "Number of military personnel (ln)",
  mil_gdpshare = "Military expenditures as % of GDP",
  distance_log10 = "Distance from a capital (ln)",
  troopratio_log10 = "Ratio of forces (ln)",
  coef_area = "% of area in conflict",
  coef_tropical = "% of tropical forests",
  coef_desert = "% of deserts",
  gdppc_log10 = "GDP per capita (ln)",
  pop_log10= "Population (ln)"
)

civil_conflicts_eng <- stargazer(all_models, # Models come first
                             title="Cox Proportional Hazards Model", # Gives table a proper title
                             dep.var.caption="DV: Duration (in days)", # Adds a DV caption
                             dep.var.labels.include=FALSE,
                             model.numbers=FALSE, # Removes (1), (2) etc. to insert own labels
                             column.labels=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), # Adds column names
                             covariate.labels=var_names_eng, # Changes the IV labels
                             notes.label="Standard errors are clustered in paretheses.", # Labels the note at the bottom of the table
                             omit.stat=c("LL","rsq","wald", "logrank", "max.rsq", "lr"), no.space=TRUE, # Adapts notes at the bottom
                             single.row=FALSE, # Compresses the table output
                             type="text"
)
