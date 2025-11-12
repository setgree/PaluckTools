#' Calculate Variance of Cohen's D or Glass's Delta
#'
#' This function computes the variance of Cohen's D or Glass's Delta based on the provided effect size
#' and sample sizes, following the equation from Cooper, Hedges, and Valentine (2009).
#' You can calculate variance on an individual basis by manually putting in a d value
#' from d_calc(), or you can do so in a batch way based on an entire dataset.
#'
#' @param d Standardized effect size calculated using `d_calc()`.
#' @param n_t Treatment sample size.
#' @param n_c Control group sample size.
#' @return Variance of Cohen's D or Glass's Delta
#' @family effect size functions
#' @seealso \code{\link{d_calc}} for calculating effect sizes
#' @export
#' @examples
#' #  example 1: calculating d, var_d, and se_d for a single study
#' ditullio_results <- d_calc(stat_type = "d_i_m", stat = 5.708 - 3.0798, sample_sd = 1.0381)
#' ditullio_variance <- var_d_calc(d = ditullio_results, n_t = 38, n_c = 38)
#' ditullio_se <- sqrt(ditullio_variance)
#'
#' # example 2: calculating d, var_d, and se_d for an entire dataset
#' \dontrun{
#' BLPlabtools::contact_data |>
#' select(-var_d)
#' mutate(var_d = mapply(
#' FUN = var_d_calc,
#' d = d,
#' n_t = n_t,
#' n_c = n_c)) |>
#' mutate(se_d = sqrt(var_d))
#'}

var_d_calc <- function(d, n_t, n_c) {
  # Calculate unstandardized variance of d
  ust_var_d <- ((n_t + n_c) / (n_t * n_c)) + ((d^2) / (2 * (n_t + n_c)))

  # Calculate Hedge's g correction factor
  hedge_g <- 1 - (3 / ((4 * (n_t + n_c - 2)) - 1))

  # Calculate variance of Cohen's D or Glass's Delta
  result <- round((hedge_g^2) * ust_var_d, digits = 3)
  return(result)
}
