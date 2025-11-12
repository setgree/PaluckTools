#' Count the Number of Unique Observations in a Dataset or Subset
#'
#' This simple function counts how many studies are in a dataset
#' or in different subsets of your data. It assumes your dataset
#' has a variable called `unique_study_id`.
#' @param dataset The dataset or subset.
#' @param counting_var The variable to count unique observations (default: "unique_study_id").
#' @return A tibble with the count of distinct observations.
#' @family summary functions
#' @seealso \code{\link{summarise_table}} for frequency tables, \code{\link{summarise_lm}} for regression summaries
#' @importFrom dplyr summarise
#' @importFrom dplyr n_distinct
#' @importFrom rlang sym
#' @export
#' @examples
#' # simple example: entire datasets
#' BLPlabtools::sv_data |> study_count()
#' # example with split, apply to many datasets, and create summary table
#' \dontrun{
#' library(purrr)
#' BLPlabtools::sv_data |> split(~study_design) |>
#' map(study_count) |> bind_rows(.id = "study_design")
#'}
#'
study_count <- function(dataset, counting_var = "unique_study_id") {
  # Convert the string into a symbol
  counting_var_sym <- sym(counting_var)

  # Use the symbol with non-standard evaluation
  result <- dataset |>
    summarise(N_unique = n_distinct(!!counting_var_sym))

  return(result)
}
