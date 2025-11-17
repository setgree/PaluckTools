# Global variable bindings to avoid R CMD check notes
utils::globalVariables(c("unique_study_id", "var_d"))

#' Extract Model Results from Meta-Analysis
#'
#' This function runs a meta-analysis using `metafor::rma()` with cluster-robust
#' standard errors and extracts key results into a clean, publication-ready
#' tibble format. It's particularly useful for creating summary tables of
#' meta-analytic results across different subsets or approaches.
#'
#' @param data A dataset containing effect sizes with columns for effect size (`d`),
#'   variance (`var_d`), and study identifier (`unique_study_id`).
#' @param approach_name A character string describing the analysis approach
#'   or subset (default: "Overall"). This will appear in the output table.
#'
#' @author John-Henry Pezzuto
#'
#' @return A tibble with one row containing:
#' \describe{
#'   \item{Approach}{Name of the analysis approach}
#'   \item{N_studies}{Number of unique studies}
#'   \item{N_estimates}{Number of effect size estimates}
#'   \item{Delta}{Point estimate (rounded to 2 decimals)}
#'   \item{SE}{Standard error (rounded to 2 decimals)}
#'   \item{CI}{Confidence interval formatted as bracketed range}
#'   \item{p_val}{P-value (formatted as "< 0.001" or without leading zero)}
#'   \item{tau}{Residual heterogeneity (rounded to 3 decimals)}
#' }
#'
#' @family meta-analysis functions
#' @seealso \code{\link{run_subset_meta_analysis}} for more flexible subset analysis,
#'   \code{\link{map_robust}} for the existing robust meta-analysis function
#'
#' @importFrom metafor rma robust
#' @importFrom tibble tibble
#' @importFrom dplyr n_distinct
#' @export
#'
#' @examples
#' # Extract overall results from contact intervention data
#' BLPlabtools::contact_data |> extract_model_results()
#'
#' # Extract results for a specific subset
#' \dontrun{
#' library(dplyr)
#' BLPlabtools::sv_data |>
#'   filter(study_design == "RCT") |>
#'   extract_model_results(approach_name = "RCTs only")
#' }
#'
extract_model_results <- function(data, approach_name = "Overall") {

  # Run meta-analysis with cluster-robust standard errors
  base_model <- metafor::rma(yi = data$d, vi = data$var_d)
  model <- metafor::robust(base_model, cluster = data$unique_study_id)

  # Format p-value based on the specified rule
  formatted_p_val <- ifelse(
    model$pval < 0.001,
    "< 0.001",
    sub("^0\\.", ".", format(round(model$pval, 3), scientific = FALSE))
  )

  # Construct the results tibble
  tibble::tibble(
    Approach = approach_name,
    N_studies = model$n,  # number of clusters (studies)
    N_estimates = model$k,  # number of estimates
    Delta = round(as.numeric(model$beta), 2),
    SE = round(model$se, 2),
    CI = paste0("[", round(model$ci.lb, 2), ", ", round(model$ci.ub, 2), "]"),
    p_val = formatted_p_val,
    tau = round(sqrt(model$tau2), 3)  # residual heterogeneity
  )
}
