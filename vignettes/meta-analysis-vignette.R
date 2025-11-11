## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----single_pooled_eff_size---------------------------------------------------
library(PaluckMetaSOP)
sv_data |> map_robust()

## ----robust_rma---------------------------------------------------------------
library(metafor)
robust(x = metafor::rma(yi = sv_data$d, vi = sv_data$var_d), 
       cluster = sv_data$unique_study_id)

## ----contact_data_demonstration-----------------------------------------------
library(dplyr)
contact_data |> select(-unique_study_id) |> # remove unique study id to reconstruct it
  group_by(name_short) |> mutate(unique_study_id = cur_group_id()) |>
  map_robust()
#' note: this Delta is a little different than the one we report in TCHR
#' we used Stata at the time to calculate the pooled average effect size
#' (and probably a few other small differences)

# now robust analysis
robust(x = metafor::rma(yi = contact_data$d, vi = contact_data$var_d), 
       cluster = contact_data$unique_study_id)

# same as if we drop the clustering information
rma(yi = d, vi = var_d, data = contact_data)

## ----split_by_behavioral_ideas_outcomes---------------------------------------
library(purrr)
sv_data |> split(~behavior_type) |> map(map_robust) |> bind_rows() 

## ----split_plus_bind_cols-----------------------------------------------------

sv_data |> split(~behavior_type) |> map(map_robust) |> 
  bind_rows(.id = "behavior_type")

## ----split_plus_kable---------------------------------------------------------
library(knitr)
sv_data |> split(~behavior_type) |> map(map_robust) |> 
  bind_rows() |> kable('markdown')

## ----significance_stars-------------------------------------------------------
get_significance_stars <- function(pval) {
  sapply(pval, function(x) {
    if (is.na(x)) {
      ""
    } else if (x < 0.001) {
      "***"
    } else if (x < 0.01) {
      "**"
    } else if (x < 0.05) {
      "*"
    } else {
      ""
    }
  })
}

## ----closer_to_pub_ready_table------------------------------------------------
sv_data |> split(~behavior_type) |> map(map_robust) |> 
  bind_rows(.id = "behavior_type") |> 
  mutate(delta_se = sprintf("%.3f%s (%.3f)", 
                            Delta, 
                            get_significance_stars(pval), se)) |> 
  select(behavior_type, N_studies, delta_se)

## ----pub_ready_table_with_gt--------------------------------------------------
library(gt) 
sv_data |> split(~behavior_type) |> map(map_robust) |> 
  bind_rows(.id = "behavior_type") |> 
  mutate(delta_se = sprintf("%.3f%s (%.3f)", 
                            Delta, 
                            get_significance_stars(pval), se)) |>
  select(behavior_type, N_studies, delta_se) |> 
gt() |>
  tab_header(
    title = "∆ by category of dependent variable") |>
  cols_label(
    behavior_type = "Behavior type",
    N_studies = "N (Studies)",
    delta_se = "∆ (se)"
  ) |>
  tab_source_note(
    source_note = "* < 0.05, ** < 0.01, *** < 0.001.")
 

