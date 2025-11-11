#' Data from 'The Contact Hypothesis Re-evaluated'
#'
#' A dataset containing contact hypothesis RCTs with at least a single day
#' of delay between the commencement of treatment and outcome measurement.
#' Studies span 1969 to 2016.
#' @format A data frame with 27 rows and 20 columns
#' @source This dataset was constructed by one author (SA Green) of the package
#'  for "The contact hypothesis re-evaluated" by EL Paluck, SA Green, and DP Green,
#'  published in 2018 in Behavioural Public Policy, Issue 3 (2), pages 129–155.
#' \describe{
#'   \item{Study}{Name of the study.}
#'   \item{name_short}{Shortened or abbreviated name of the study.}
#'   \item{pub_date}{Publication year of the study.}
#'   \item{target}{Numerical code representing the target group of the study (e.g., disability, foreigners, gender).}
#'   \item{target_spelled_out}{Detailed description of the target group (e.g., disability, foreigners, gender, LGBT, race, religion).}
#'   \item{pop}{Numerical code representing the population segment studied (e.g., adults, high school students).}
#'   \item{pop_spelled_out}{Detailed description of the population segment (e.g., elementary and middle school students, adults, college aged young adults).}
#'   \item{unstand}{Unstandardized effect size of the study's finding.}
#'   \item{statistic}{Type of statistical result or method used in the study (e.g., d_i_d, regression, beta, T-test, F-test).}
#'   \item{n_t}{Sample size of the treatment group.}
#'   \item{n_c}{Sample size of the control group.}
#'   \item{sd_c}{Standard deviation of the control group, if available.}
#'   \item{what}{Reference to the location in the publication where the data or statistic is reported (e.g., page number, table number).}
#'   \item{d}{Standardized effect size measure (Cohen’s d or Glass’s Delta, depending on the context).}
#'   \item{var_d}{Variance of the standardized effect size.}
#'   \item{se_d}{Standard error of the standardized effect size.}
#'   \item{p_a_p}{Indicates presence (1) or absence (0) of a pre-analysis plan.}
#'   \item{publish}{Numerical code indicating the publication status or venue (0 for published, 1 for dissertation, 2 for preprint).}
#'   \item{days_delay}{Number of days between study assignment and treatment.}
#'   \item{unique_study_id}{A unique numeric identifier for each study.}
#' }
#' @keywords dataset
"contact_data"
