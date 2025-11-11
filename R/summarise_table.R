#' Create a Frequency Table for a Variable in a Data Frame
#'
#' `summarise_table` takes a built in function (table) and turns it into something
#' that can be integrated into a sequence of pipes. As with `summarise_lm`,
#' this is useful for when you want to see many tables at once,
#' or put a few tables into a bigger table.
#' Writing this was surprisingly complicated -- it uses enquo() to capture
#'  the unquoted variable name and !! to unquote it within the function.
#' @param data A data frame or tibble.
#' @param var_name The name of the variable/column to generate the frequency table.
#' @return A table showing the frequency of each unique value in the specified variable.
#' @family summary functions
#' @seealso \code{\link{summarise_lm}} for regression summaries, \code{\link{study_count}} for counting studies
#'
#' @importFrom dplyr pull
#' @importFrom rlang !! enquo
#' @export
#' @examples
#' # simple example: entire dataset
#' PaluckTools::sv_data |> summarise_table(behavior_type)
#' \dontrun{
#' # example with split and apply to many subsets
#' library(purrr)
#' PaluckTools::sv_data |> split(~study_design) |>
#' map(~ summarise_table(., behavior_type)) |> bind_rows(.id = "study_design")
#'}

summarise_table <- function(data, var_name) {
  var_name <- enquo(var_name)
  freq_table <- table(data |> pull(!!var_name))
  return(freq_table)
}
