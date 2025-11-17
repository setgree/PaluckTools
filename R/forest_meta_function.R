#' Prepare Meta-Analysis Results for Forest Plots
#'
#' This function performs a basic meta-analysis using `metafor::rma.uni`
#' and returns results in a format suitable for creating forest plots.
#' It's designed to work with grouped data where you want to meta-analyze
#' within each group and then display the results visually.
#'
#' @param x A dataset containing effect sizes with columns for effect size (`d`),
#'   variance (`var_d`), `author`, and `year`.
#'
#' @return A tibble with one row containing:
#' \describe{
#'   \item{author}{First author name from the group}
#'   \item{year}{Publication year from the group}
#'   \item{theory}{Theory category from the group (if present)}
#'   \item{estimate}{Meta-analytic estimate}
#'   \item{se}{Standard error of the estimate}
#'   \item{var}{Variance of the estimate}
#'   \item{ci.lb}{Lower bound of confidence interval}
#'   \item{ci.ub}{Upper bound of confidence interval}
#' }
#'
#' @family meta-analysis functions
#' @seealso \code{\link{extract_model_results}} for publication tables,
#'   \code{\link{map_robust}} for robust meta-analysis
#'
#' @importFrom metafor rma.uni
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(purrr)
#' # Group by study and prepare forest plot data
#' BLPlabtools::contact_data |>
#'   group_by(unique_study_id) |>
#'   group_split() |>
#'   map_df(forest_meta_function)
#' }
#'
forest_meta_function <- function(x) {
  result <- metafor::rma.uni(yi = x$d, vi = x$var_d, method = 'EE',
                             slab = paste0(x$author, x$year))
  tibble::tibble(
    author = x$author[1],
    year = x$year[1],
    theory = if("theory" %in% names(x)) x$theory[1] else NA_character_,
    estimate = as.numeric(result$b),
    se = result$se,
    var = result$vb,
    ci.lb = result$ci.lb,
    ci.ub = result$ci.ub
  )
}
