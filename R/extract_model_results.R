# Global variable bindings to avoid R CMD check notes
utils::globalVariables(c("unique_study_id", "var_d"))

#' Extract Model Results from Robust Meta-Analysis
#'
#' This function runs a robust meta-analysis using `robumeta::robu` and
#' extracts key results into a clean, publication-ready tibble format.
#' It's particularly useful for creating summary tables of meta-analytic
#' results across different subsets or approaches.
#'
#' @param data A dataset containing effect sizes with columns for effect size (`d`),
#'   variance (`var_d`), and study identifier (`unique_study_id`).
#' @param approach_name A character string describing the analysis approach
#'   or subset (default: "Overall"). This will appear in the output table.
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
#'   \item{tau}{Between-study heterogeneity (rounded to 3 decimals)}
#' }
#'
#' @family meta-analysis functions
#' @seealso \code{\link{run_subset_meta_analysis}} for more flexible subset analysis,
#'   \code{\link{map_robust}} for simpler robust meta-analysis
#'
#' @importFrom robumeta robu
#' @importFrom tibble tibble
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

  # Run the robumeta::robu model on the provided data
  model <- robumeta::robu(
    formula = d ~ 1,
    data = data,
    studynum = unique_study_id,
    var.eff.size = var_d,
    modelweights = 'CORR',
    small = TRUE
  )

  # Format p-value based on the specified rule
  formatted_p_val <- ifelse(
    model$reg_table$prob < 0.001,
    "< 0.001",
    sub("^0\\.", ".", format(round(model$reg_table$prob, 3), scientific = FALSE))
  )

  # Construct the results tibble directly
  tibble::tibble(
    Approach = approach_name,
    N_studies = length(unique(model$X.full$study)),
    N_estimates = nrow(model$data.full),
    Delta = round(model$reg_table$b.r, 2),
    SE = round(model$reg_table$SE, 2),
    CI = paste0("[", round(model$reg_table$CI.L, 2), ", ", round(model$reg_table$CI.U, 2), "]"),
    p_val = formatted_p_val,
    tau = round(sqrt(model$mod_info$tau.sq), 3)
  )
}
