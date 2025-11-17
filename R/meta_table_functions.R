# Global variable bindings to avoid R CMD check notes
utils::globalVariables(c("unique_study_id", "var_d", "Delta", "Moderator",
                         "p_val", "p_val_ref", "N_Studies", "N_Estimates", "CI"))

#' Run Subset Meta-Analysis with Flexible Filtering
#'
#' This function performs meta-analysis on subsets of data with
#' flexible filtering options, using metafor with cluster-robust standard errors.
#' It's designed to handle edge cases like single studies or failed models
#' gracefully, making it ideal for creating moderator analysis tables.
#'
#' @param data A dataset containing effect sizes with columns for effect size (`d`),
#'   variance (`var_d`), and study identifier (`unique_study_id`).
#' @param group_var Optional grouping variable name (as string) to filter by level.
#' @param level Optional level of group_var to filter to.
#' @param filter_string Optional string to filter by (used with filter_column).
#' @param filter_column Optional column name to apply filter_string to.
#' @param str_detect_flag Logical indicating whether to use string detection (TRUE)
#'   or exact matching (FALSE) when filtering (default: TRUE).
#' @param approach_name Optional custom name for the approach/moderator level.
#' @param col_name Name for the first column in output (default: "Moderator").
#' @param include_tau Logical indicating whether to include tau (residual
#'   heterogeneity) in the output (default: FALSE).
#'
#' @return A tibble with meta-analysis results including columns for the
#'   moderator level, number of studies, number of estimates, effect size
#'   estimate (Delta), confidence interval, and p-value. If include_tau is TRUE,
#'   also includes tau.
#'
#' @family meta-analysis functions
#' @seealso \code{\link{run_meta_regression}} for meta-regression analysis,
#'   \code{\link{process_group}} for complete moderator table creation,
#'   \code{\link{extract_model_results}} for simpler result extraction
#'
#' @importFrom metafor rma robust
#' @importFrom tibble tibble
#' @importFrom dplyr filter n_distinct mutate
#' @importFrom rlang sym !! :=
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Overall analysis
#' BLPlabtools::sv_data |> run_subset_meta_analysis()
#'
#' # Analysis for a specific group
#' BLPlabtools::sv_data |>
#'   run_subset_meta_analysis(group_var = "study_design",
#'                           level = "RCT",
#'                           include_tau = TRUE)
#'
#' # Analysis with string filtering
#' BLPlabtools::contact_data |>
#'   run_subset_meta_analysis(filter_column = "intervention_type",
#'                           filter_string = "direct",
#'                           str_detect_flag = TRUE)
#' }
#'
run_subset_meta_analysis <- function(data, group_var = NULL, level = NULL,
                                     filter_string = NULL, filter_column = NULL,
                                     str_detect_flag = TRUE, approach_name = NULL,
                                     col_name = "Moderator", include_tau = FALSE) {
  # Handle filtering
  if (!is.null(filter_column) && !is.null(filter_string)) {
    if (str_detect_flag) {
      data_subset <- data |> filter(stringr::str_detect(!!sym(filter_column), filter_string))
    } else {
      data_subset <- data |> filter(!!sym(filter_column) == filter_string)
    }
    Moderator <- ifelse(is.null(approach_name), filter_string, approach_name)
  } else if (!is.null(group_var) && !is.null(level)) {
    data_subset <- data |> filter(!!sym(group_var) == level)
    Moderator <- level
  } else {
    data_subset <- data
    Moderator <- ifelse(is.null(approach_name), "Overall", approach_name)
  }

  # Get number of studies and estimates
  num_studies <- n_distinct(data_subset$unique_study_id)
  num_estimates <- nrow(data_subset)

  if (num_studies < 1) {
    # No studies available
    result <- tibble::tibble(
      !!col_name := Moderator,
      N_Studies = num_studies,
      N_Estimates = num_estimates,
      Delta = NA_real_,
      CI = NA_character_,
      p_val = NA_character_
    )
    return(result)
  }

  # Run meta-analysis (even with 1 study)
  model <- tryCatch({
    base_model <- metafor::rma(yi = data_subset$d, vi = data_subset$var_d)
    metafor::robust(base_model, cluster = data_subset$unique_study_id)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(model)) {
    result <- tibble::tibble(
      !!col_name := Moderator,
      N_Studies = num_studies,
      N_Estimates = num_estimates,
      Delta = NA_real_,
      CI = NA_character_,
      p_val = NA_character_
    )
    return(result)
  }

  # Extract results
  estimate <- as.numeric(model$beta)
  ci_lower <- model$ci.lb
  ci_upper <- model$ci.ub
  p_val <- format(round(model$pval, 3), scientific = FALSE)
  p_val <- sub("^0\\.", ".", p_val)  # Remove leading zero

  # Create result tibble
  result <- tibble::tibble(
    !!col_name := Moderator,
    N_Studies = num_studies,
    N_Estimates = num_estimates,
    Delta = round(estimate, 2)
  )

  # Add tau if requested
  if (include_tau) {
    tau <- round(sqrt(model$tau2), 2)
    result <- result |> mutate(tau = tau)
  }

  # Add remaining columns
  result <- result |> mutate(
    CI = paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]"),
    p_val = p_val
  )

  return(result)
}

#' Run Meta-Regression and Extract P-values
#'
#' This function runs a meta-regression with a categorical moderator using
#' metafor with cluster-robust standard errors and extracts p-values for
#' comparisons with a reference level. It's designed to work with
#' \code{\link{process_group}} to create complete moderator analysis tables.
#'
#' @param data A dataset containing effect sizes with columns for effect size (`d`),
#'   variance (`var_d`), and study identifier (`unique_study_id`).
#' @param group_var Name of the grouping variable (as string).
#' @param ref_level The reference level for the meta-regression.
#'
#' @return A named vector of formatted p-values, with the reference level
#'   marked as "ref" and other levels showing their p-values from the
#'   meta-regression.
#'
#' @family meta-analysis functions
#' @seealso \code{\link{run_subset_meta_analysis}} for subset analysis,
#'   \code{\link{process_group}} for complete moderator analysis
#'
#' @importFrom metafor rma robust
#' @importFrom dplyr group_by summarise filter pull n_distinct mutate
#' @importFrom rlang sym !!
#' @export
#'
#' @examples
#' \dontrun{
#' # Get p-values comparing each study design to RCT
#' BLPlabtools::sv_data |>
#'   run_meta_regression(group_var = "study_design",
#'                      ref_level = "RCT")
#' }
#'
run_meta_regression <- function(data, group_var, ref_level) {
  # Include all levels with at least one study
  sufficient_levels <- data |>
    group_by(!!sym(group_var)) |>
    summarise(N_Studies = n_distinct(unique_study_id)) |>
    filter(N_Studies >= 1) |>
    pull(!!sym(group_var))

  # Subset data to include all sufficient levels
  data <- data |> filter(!!sym(group_var) %in% sufficient_levels)

  # Ensure the grouping variable is a factor with the reference level first
  data <- data |>
    mutate(
      group_var_factor = factor(!!sym(group_var),
                               levels = c(ref_level, setdiff(sufficient_levels, ref_level)))
    )

  # Check if there are at least two levels to run meta-regression
  if (length(unique(data$group_var_factor)) < 2) {
    # Not enough groups to run meta-regression
    p_values_named <- setNames(rep(NA_character_, length(sufficient_levels)), sufficient_levels)
    p_values_named[ref_level] <- "ref"
    return(p_values_named)
  }

  # Run meta-regression with cluster-robust SEs
  model <- tryCatch({
    base_model <- metafor::rma(yi = d ~ group_var_factor, vi = var_d, data = data)
    metafor::robust(base_model, cluster = data$unique_study_id)
  }, error = function(e) {
    # Handle error in meta-regression
    p_values_named <- setNames(rep(NA_character_, length(sufficient_levels)), sufficient_levels)
    p_values_named[ref_level] <- "ref"
    return(p_values_named)
  })

  # Extract p-values for each level (excluding intercept)
  level_names <- levels(data$group_var_factor)[-1]
  p_values <- model$pval[-1]  # Exclude intercept

  # Format p-values
  p_values_formatted <- formatC(p_values, format = "f", digits = 3)
  p_values_formatted <- sub("^0\\.", ".", p_values_formatted)

  # Create a named vector of p-values
  p_values_named <- setNames(rep(NA_character_, length(sufficient_levels)), sufficient_levels)
  p_values_named[ref_level] <- "ref"
  p_values_named[level_names] <- p_values_formatted

  return(p_values_named)
}

#' Process Grouped Meta-Analysis with Moderator Comparisons
#'
#' This is a high-level function that orchestrates subset meta-analyses
#' and meta-regression to create complete moderator analysis tables.
#' It runs separate meta-analyses for each level of a grouping variable
#' and adds p-values from meta-regression comparing each level to a
#' reference level. Uses metafor with cluster-robust standard errors.
#'
#' @param data A dataset containing effect sizes with columns for effect size (`d`),
#'   variance (`var_d`), and study identifier (`unique_study_id`).
#' @param group_var Name of the grouping variable (as string).
#' @param ref_level The reference level for meta-regression comparisons.
#' @param order_levels Optional vector specifying the order of levels in the output.
#' @param include_tau Logical indicating whether to include tau (default: FALSE).
#'
#' @return A tibble with one row per moderator level containing:
#'   - Moderator: The level name
#'   - N_Studies: Number of studies in this level
#'   - N_Estimates: Number of effect size estimates
#'   - Delta: Effect size estimate
#'   - CI: Confidence interval
#'   - p_val: P-value for the effect within this level
#'   - p_val_ref: P-value comparing this level to the reference level
#'
#' @family meta-analysis functions
#' @seealso \code{\link{run_subset_meta_analysis}} for individual subset analysis,
#'   \code{\link{run_meta_regression}} for meta-regression p-values
#'
#' @importFrom dplyr bind_rows filter pull mutate select across
#' @importFrom rlang sym !!
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' # Create moderator table for study design
#' BLPlabtools::sv_data |>
#'   process_group(group_var = "study_design",
#'                ref_level = "RCT")
#'
#' # With custom ordering and tau
#' BLPlabtools::sv_data |>
#'   process_group(group_var = "study_design",
#'                ref_level = "RCT",
#'                order_levels = c("RCT", "Quasi-experimental", "Observational"),
#'                include_tau = TRUE)
#' }
#'
process_group <- function(data, group_var, ref_level, order_levels = NULL, include_tau = FALSE) {
  # Automatically determine group levels present in data
  group_levels <- data |>
    filter(!is.na(!!sym(group_var))) |>
    pull(!!sym(group_var)) |>
    unique()

  # Use the custom order if provided, otherwise set reference level first
  if (!is.null(order_levels)) {
    group_levels <- order_levels[order_levels %in% group_levels]
  } else {
    group_levels <- c(ref_level, setdiff(group_levels, ref_level))
  }

  # Run subset meta-analyses for each level
  group_results <- lapply(group_levels, function(level) {
    run_subset_meta_analysis(data, group_var, level, include_tau = include_tau)
  }) |> bind_rows()

  # Identify levels with sufficient data
  sufficient_levels <- group_results |> filter(!is.na(Delta)) |> pull(Moderator)

  # Run meta-regression to get second p-values
  group_p_values <- run_meta_regression(data, group_var, ref_level)

  # Add the second p-values to the results
  group_results <- group_results |>
    mutate(
      p_val_ref = group_p_values[Moderator],
      # Replace both p-values with N/A if both are 0
      across(c(p_val, p_val_ref), ~ ifelse(. %in% c("0", ".000"), "N/A", .))
    )

  # Reorder columns
  group_results <- group_results |>
    select(Moderator, N_Studies, N_Estimates, Delta, CI, p_val, p_val_ref)

  return(group_results)
}
