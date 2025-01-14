## code to prepare `nhefs_weights` dataset goes here
library(tidyverse)
library(broom)
library(causaldata)
library(propensity)
options(propensity.quiet = TRUE)
propensity_model <- glm(
  qsmk ~ sex +
    race + age + I(age^2) + education +
    smokeintensity + I(smokeintensity^2) +
    smokeyrs + I(smokeyrs^2) + exercise + active +
    wt71 + I(wt71^2),
  family = binomial(),
  data = nhefs_complete
)

nhefs_weights <- propensity_model %>%
  augment(type.predict = "response", data = nhefs_complete) %>%
  mutate(
    wts = 1 / ifelse(qsmk == 0, 1 - .fitted, .fitted),
    w_ate = wt_ate(.fitted, qsmk),
    w_att = wt_att(.fitted, qsmk),
    w_atc = wt_atu(.fitted, qsmk),
    w_atm = wt_atm(.fitted, qsmk),
    w_ato = wt_ato(.fitted, qsmk)
  ) %>%
  select(
    qsmk,
    race,
    age,
    education,
    smokeintensity,
    smokeyrs,
    exercise,
    active,
    wt71,
    starts_with("w_")
  )

usethis::use_data(nhefs_weights, overwrite = TRUE)
