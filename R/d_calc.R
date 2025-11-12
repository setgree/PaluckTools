#' Calculate Cohen's D or Glass's Delta
#'
#' This function converts raw effect sizes into Cohen's D or Glass's Delta
#' based on the measure of sample variance for standardization.
#'
#' All calculations derived from Cooper, Hedges, and Valentine (2009), except
#' for difference in proportions, which, to the best of our knowledge, Don Green
#' came up with while we were working on _Prejudice Reduction: Progress and Challenges_.
#' We elaborate more on this estimator in the paper's appendix.
#'
#' @param stat_type Category of statistical result reported or derived from the paper.
#' Possible values are "d", "d_i_d", "d_i_m", "d_i_p", "eta_squared", "f_test",
#' "log_odds_ratio", "odds_ratio", "reg_coef", "s_m_d", "t_test", "unspecified null", "z".
#' @param stat Unstandardized effect size.
#' @param sample_sd Standard deviation of the relevant sample (preferably of the control group).
#' @param n_t Treatment group sample size.
#' @param n_c Control group sample size.
#' @return Cohen's D or Glass's Delta value.
#' @family effect size functions
#' @seealso \code{\link{var_d_calc}} for calculating the variance of effect sizes
#'
#' @export
#'
#' @examples
#' # Example: Calculate d for a study that provides difference in means results
#' difference_in_means_result <- d_calc(stat_type = "d_i_m", stat = 1, sample_sd = 0.3)
#' # Example: Calculate d for a study that provides an F-test
#' f_test_result <- d_calc(stat_type = "f_test", stat = 1, n_t = 50, n_c = 40)
#' # Example: Use mapply to calculate d from rows in dataset
#' \dontrun{
#' library(dplyr)
#' BLPlabtools::contact_data |>
#' select(-d) |> # remove d in order to recalculate it
#' mutate(d = mapply(
#' FUN = d_calc,
#' stat_type = statistic,
#' stat =  unstand,
#' sample_sd = sd_c,
#' n_t = n_t,
#' n_c = n_c)) |> select(name_short, d)
#' }

d_calc <- function(stat_type, stat, sample_sd, n_t, n_c) {
  # Calculate Cohen's D or Glass's $\Delta$ based on effect size and sample SD
  if (stat_type == "d" ||
      stat_type == "s_m_d") {
    # Directly use the reported change of SDs
    d <- stat
  }
  else if (stat_type == "d_i_m" ||
           stat_type == "d_i_d" ||
           stat_type == "reg_coef" ||
           stat_type == "regression" ||
           stat_type == "beta") {
    d <- round(stat / sample_sd, digits = 3)
  } else if (stat_type == "t_test" ||
             stat_type == "T-test") {
    # Calculate Cohen's D for t test
    d <- round(stat * sqrt((n_t + n_c) / (n_t * n_c)), digits = 3)
  } else if (stat_type == "f_test" ||
             stat_type == "F-test" ||
             stat_type == "F") {
    # Calculate Cohen's D for f test
    d <- round(sqrt((stat * (n_t + n_c)) / (n_t * n_c)), digits = 3)
  } else if (stat_type == "eta_squared") {
    # Calculate Cohen's D for eta squared
    d <- sqrt(stat) / sqrt(1 - stat)
  } else if (stat_type == "z") {
    # Calculate Cohen's D for z test
    d <- stat / sqrt(n_t + n_c)
  } else if (stat_type == "odds_ratio") {
    # Calculate Cohen's D for odds ratio
    d <- log(stat) * sqrt(3) / pi
  } else if (stat_type == "log_odds_ratio") {
    # Calculate Cohen's D for log odds ratio
    d <- stat * sqrt(3) / pi
  } else if (stat_type == 'd_i_p') {
    # Calculate Glass's Delta for difference in proportions
    # "SD" as an input is a misnomer here; input the _proportion_ of the incident
    # in the control group as the variance estimate, and then this calculator
    # treats that as draws from a Bernoulli distribution. Variance of Bernoulli
    # is $p(1-p)$; so the estimator in total is
    #  $$\Delta = \frac{p_{1} - p_{2}}{\sqrt{p_{2} * (1 - p_{2)}}}$$
    d = stat / (sqrt(sample_sd * (1 - sample_sd)))
    }
  else if (stat_type == "unspecified_null" ||
                 stat_type == "unspecified null") {
    # Set an 'unspecified null' result to a default small value
    d <- 0.01
  }
  else {
    # Default value if stat_type is unrecognized
    d <- NA
  }

  return(d)
}
