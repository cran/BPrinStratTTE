#' Inverse logit function
#'
#' @param x Numeric value  (usually a logarithm of odds).
#'
#' @return Numeric value on the interval \eqn{[0,1]}, result of `log(pi/(1-pi))`.
#' @return Numeric value, result of `exp(x)/(1+exp(x))`.
#' @export
#' 
#' @details
#' The inverse logit function is also known as logistic function.
#'
#' @seealso [logit()]
#' 
#' @examples
#' # probabilities 
#' prob_ICE_base <- 0.3
#' prob_ICE_risk <- 0.6
#' # model coefficients
#' (beta1 <- logit(prob_ICE_base))
#' (beta2 <- logit(prob_ICE_risk) - logit(prob_ICE_base))
#' # linear predictor 
#' logit(prob_ICE_base); (lin_pred1 <- beta1 + beta2*0)
#' logit(prob_ICE_risk); (lin_pred2 <- beta1 + beta2*1)
#' # inverse logit of linear predictor 
#' (inv_logit(lin_pred1)) # prob for X1 = 0
#' (inv_logit(lin_pred2)) # prob for X1 = 1
inv_logit <- function(x) {
  exp(x)/(1+exp(x))
  }
