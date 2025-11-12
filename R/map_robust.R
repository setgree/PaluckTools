#' Perform robust meta-analysis (including in a pipe or split paradigm)
#'
#' Performs robust meta-analysis on either data subsets or the whole dataset.
#' Its intended usage is in in context of the  split/map & pipes paradigm:
#' `dat |> split(~some_var) |> map(map_robust)`.
#'
#' `dat |> map_robust()` will return the meta-analytic estimate for the whole dataset.
#'
#' @note If there is only one study in a cluster, this function will
#' just reproduce that study's meta-analytic estimate verbatim. This is good for
#' some circumstances but not others. Adapt to your needs.
#'
#' @importFrom metafor robust
#' @importFrom metafor rma
#' @importFrom tibble as_tibble
#'
#' @param x The dataset or subset to perform meta-analysis on.
#' @return A tibble with meta-analysis results.
#' @seealso \code{\link{d_calc}} and \code{\link{var_d_calc}} for preparing data before meta-analysis
#' @examples
#' # example 1: meta-analyze entire dataset
#' BLPlabtools::sv_data |> map_robust()
#' # example 2: meta-analyze many subsets and create overall table
#' \dontrun{
#' library(dplyr); library(purrr)
#' sv_data |> split(~behavior_type) |> map(map_robust) |>
#'  bind_rows(.id = "behavior_type")
#'  }
#' @export
map_robust <- function(x) {
  # Function to format p-values without leading zeroes or unnecessary detail
  # if they're very small
  format_pval <- function(p) {
    if (is.na(p)) {
      "NA     "  # Using space padding to align with other p-values
    } else if (p < 0.0001) {
      "<.0001"  # Special case without space for very small p-values
    } else {
      formatted_p <- formatC(p, format = "f", digits = 4)
      if (substr(formatted_p, 1, 1) == "0") {
        paste0(" ", substr(formatted_p, 2, nchar(formatted_p)))  # Add a leading space for non-zero leading p-values
      } else {
        paste0(" ", formatted_p)  # Ensure leading space for all other cases
      }
    }
  }
  # Check if there is only one study in the cluster
  if (nrow(x) == 1) {
    # Directly take Delta and SE from the dataset
    Delta <- round(x$d, 3)
    se <- round(x$se_d, 3)
    pval <- NA

    # Return these values in a tibble
    return(data.frame(N_unique = nrow(x),
                      Delta = Delta,
                      se = se,
                      pval = pval))
  } else {
    # Perform robust meta-analysis using metafor::robust function
    result <- metafor::robust(x = metafor::rma(yi = x$d, vi = x$var_d), cluster = x$unique_study_id)
    num_studies <- length(x$unique_study_id)

    # Extract relevant results and format them into a tibble/data frame
    output <- data.frame(
      N_observations = result$k,
      N_studies = result$n,
      Delta = round(result$beta, 4),
      se = round(result$se, 4),
      pval = noquote(format_pval(result$pval))) |>
      tibble::as_tibble()

    return(output)
  }
}
