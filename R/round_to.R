#' Round Numbers to Arbitrary Accuracy with Directional Control
#'
#' This utility function rounds numbers to a specified accuracy with
#' control over the rounding direction (up, down, or standard rounding).
#' It's particularly useful when you need to round to non-standard values
#' like the nearest 5, 10, 100, etc.
#'
#' @param x A numeric value or vector to round.
#' @param accuracy The accuracy to round to (e.g., 5, 10, 0.1, etc.).
#' @param direction The direction to round: "default" for standard rounding,
#'   "up" for ceiling, "down" for floor.
#'
#' @author John-Henry Pezzuto
#'
#' @return A numeric value or vector rounded to the specified accuracy.
#'
#' @family utility functions
#'
#' @export
#'
#' @examples
#' # Round to nearest 5
#' round_to(23, 5)  # Returns 25
#'
#' # Round down to nearest 100
#' round_to(847, 100, direction = "down")  # Returns 800
#'
#' # Round up to nearest 10
#' round_to(23, 10, direction = "up")  # Returns 30
#'
#' # Works with decimals too
#' round_to(3.14159, 0.01)  # Returns 3.14
#'
round_to <- function(x, accuracy, direction = "default") {
  f <- switch(direction,
              "up" = ceiling,
              "down" = floor,
              "default" = round)
  f(x / accuracy) * accuracy
}
