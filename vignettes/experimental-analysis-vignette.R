## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----simulate_data------------------------------------------------------------
library(PaluckTools)
library(dplyr)

set.seed(123)  # For reproducibility

# Simulate experimental data
n_schools <- 20
n_per_school <- 100
n_total <- n_schools * n_per_school

experiment_data <- data.frame(
  student_id = 1:n_total,
  school_id = rep(1:n_schools, each = n_per_school),
  # Treatment assignment (randomized within schools)
  treatment = rep(c(rep(0, n_per_school/2), rep(1, n_per_school/2)), n_schools),
  # Baseline covariates
  baseline_score = rnorm(n_total, mean = 70, sd = 15),
  gender = sample(c("female", "male"), n_total, replace = TRUE),
  # Outcomes (treatment effect = +5 points on average)
  test_score = 70 + 5*rep(c(rep(0, n_per_school/2), rep(1, n_per_school/2)), n_schools) +
               rnorm(n_total, sd = 12),
  attendance = 0.85 + 0.03*rep(c(rep(0, n_per_school/2), rep(1, n_per_school/2)), n_schools) +
               rnorm(n_total, sd = 0.1),
  attitudes = 3.5 + 0.3*rep(c(rep(0, n_per_school/2), rep(1, n_per_school/2)), n_schools) +
              rnorm(n_total, sd = 0.8)
) |>
  mutate(
    treatment = factor(treatment, levels = c(0, 1), labels = c("Control", "Treatment")),
    gender = factor(gender)
  )

# Preview the data structure
head(experiment_data)

