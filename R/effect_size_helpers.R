# Effect Size Conversion Helpers
# These are utility functions for data preparation and effect size conversions

#' @importFrom stats qt
NULL

#' Convert Confidence Interval to Standard Deviation
#'
#' Converts confidence intervals to standard deviation using Cochrane methodology.
#'
#' @param upper_ci Upper confidence interval bound
#' @param lower_ci Lower confidence interval bound
#' @param n Sample size
#' @param interval Confidence interval level (90 or 95). Default is 95.
#'
#' @return Standard deviation
#' @export
#' @family effect size functions
#'
#' @references
#' Cochrane Handbook 5.1, Section 7.7.3.2
#' \url{https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm}
#'
#' @examples
#' # Convert 95% CI to SD
#' ci_2_sd(upper_ci = 5.2, lower_ci = 2.8, n = 50)
#'
#' # Convert 90% CI to SD
#' ci_2_sd(upper_ci = 5.2, lower_ci = 2.8, n = 50, interval = 90)
ci_2_sd <- function(upper_ci, lower_ci, n, interval = 95) {
  if (interval == 95) {
    sd <- (sqrt(n) * (upper_ci - lower_ci)) / 3.92
  } else if (interval == 90) {
    sd <- (sqrt(n) * (upper_ci - lower_ci)) / 3.29
  } else {
    stop("Interval must be either 90 or 95")
  }
  return(sd)
}


#' Convert Standard Error to Standard Deviation
#'
#' Simple conversion from standard error to standard deviation.
#'
#' @param se Standard error
#' @param n Sample size
#'
#' @return Standard deviation
#' @export
#' @family effect size functions
#'
#' @examples
#' se_2_sd(se = 0.5, n = 100)
se_2_sd <- function(se, n) {
  sd <- se * sqrt(n)
  return(sd)
}


#' Calculate Pooled Standard Deviation
#'
#' Pools standard deviations across multiple groups using Hedges (1981) method.
#'
#' @param sd Vector of standard deviations
#' @param n Vector of sample sizes (same length as sd)
#'
#' @return Pooled standard deviation
#' @export
#' @family effect size functions
#'
#' @references Hedges, L. V. (1981). Distribution Theory for Glass's Estimator of
#' Effect Size and Related Estimators. Journal of Educational Statistics, 6(2), 107-128.
#'
#' @examples
#' sd_pooled(sd = c(2.1, 2.5, 1.9), n = c(30, 25, 35))
sd_pooled <- function(sd, n) {
  if (length(sd) != length(n)) {
    stop("Lengths of 'sd' and 'n' must be equal")
  }

  k <- length(sd)
  sd2 <- sd^2
  df <- n - 1

  num <- sum(sd2 * df)
  dem <- sum(n) - k

  sd_pooled <- sqrt(num / dem)
  return(sd_pooled)
}


#' Calculate Difference-in-Differences Effect Size
#'
#' Computes a standardized difference-in-differences effect size from raw means.
#'
#' @param mean_treatment_post Post-treatment mean for treatment group
#' @param mean_treatment_pre Pre-treatment mean for treatment group
#' @param mean_control_post Post-treatment mean for control group
#' @param mean_control_pre Pre-treatment mean for control group
#' @param sd Standard deviation (preferably from control group)
#'
#' @return Standardized difference-in-differences effect size
#' @export
#' @family effect size functions
#'
#' @examples
#' did_calculator(mean_treatment_post = 5.2, mean_treatment_pre = 4.8,
#'                mean_control_post = 4.5, mean_control_pre = 4.6, sd = 1.2)
did_calculator <- function(mean_treatment_post, mean_treatment_pre,
                          mean_control_post, mean_control_pre, sd) {
  did <- ((mean_treatment_post - mean_treatment_pre) -
          (mean_control_post - mean_control_pre)) / sd
  return(did)
}


# Internal conversion utilities (not exported) ----------------------------

# These functions are useful for specific conversions but users should
# generally use d_calc() as their main interface

#' @keywords internal
beta_2_d <- function(beta, standard_error, n) {
  t <- beta / standard_error
  d <- (t * 2) / sqrt(n - 2)
  return(d)
}

#' @keywords internal
t_2_d <- function(t, n) {
  d <- (t * 2) / sqrt(n - 2)
  return(d)
}

#' @keywords internal
z_2_d <- function(z, n) {
  d <- 2 * z / sqrt(n)
  return(d)
}

#' @keywords internal
chisq_2_d <- function(chisq, df, n) {
  if (df == 1) {
    d <- 2 * sqrt(chisq / (n - chisq))
  } else {
    d <- 2 * sqrt(chisq / n)
  }
  return(d)
}

#' @keywords internal
r_2_d <- function(r) {
  d <- (4 * r^2) / (1 - r^2)
  return(d)
}

#' @keywords internal
d_2_r <- function(d) {
  r <- sqrt(d^2 / (4 + d^2))
  return(r)
}

#' @keywords internal
beta_2_r <- function(beta, standard_error, n) {
  t <- beta / standard_error
  d <- (t * 2) / sqrt(n - 2)
  r <- sqrt(d^2 / (4 + d^2))
  return(r)
}

#' @keywords internal
t_2_r <- function(t, n) {
  d <- (t * 2) / sqrt(n - 2)
  r <- sqrt(d^2 / (4 + d^2))
  return(r)
}

#' @keywords internal
z_2_r <- function(z, n) {
  r <- sqrt(z^2 / (z^2 + n))
  return(r)
}

#' @keywords internal
chisq_2_r <- function(chisq, df, n) {
  if (df == 1) {
    r <- sqrt(chisq / n)
  } else {
    r <- sqrt(chisq / (chisq + n))
  }
  return(r)
}

#' @keywords internal
t_inverse <- function(p, n) {
  # Calculate t value from p-value and sample size
  qt(1 - p / 2, n - 2)
}

#' @keywords internal
weighted_r <- function(r, n) {
  Wr <- (n - 1) / ((1 - r^2)^2)
  return(Wr)
}

#' @keywords internal
merged_r <- function(r, weighted_r) {
  if (length(r) != length(weighted_r)) {
    stop("Lengths of 'r' and 'weighted_r' must be equal")
  }
  total_weighted_r <- sum(weighted_r)
  new_weighted_r <- weighted_r / total_weighted_r
  total_product <- new_weighted_r * r
  sum_total_product <- sum(total_product)
  return(sum_total_product)
}
