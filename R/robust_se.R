#' Robust Standard Error
#'
#' Calculate cluster-robust standard errors for linear models
#'
#' @param model A fitted linear model object (from lm())
#' @param cluster A vector indicating cluster membership for computing robust SEs
#'
#' @return A list with two elements: the robust covariance matrix and coefficient test results
#'
#' @author John-Henry Pezzuto
#' @export
robust_se <- function(model, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse_cov <- dfc * sandwich::sandwich(model, meat = crossprod(uj)/N)
  rcse_se <- lmtest::coeftest(model, rcse_cov)
  return(list(rcse_cov, rcse_se))
}
