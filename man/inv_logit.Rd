% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inv_logit.R
\name{inv_logit}
\alias{inv_logit}
\title{Inverse logit function}
\usage{
inv_logit(x)
}
\arguments{
\item{x}{Numeric value  (usually a logarithm of odds).}
}
\value{
Numeric value on the interval \eqn{[0,1]}, result of \code{log(pi/(1-pi))}.

Numeric value, result of \code{exp(x)/(1+exp(x))}.
}
\description{
Inverse logit function
}
\details{
The inverse logit function is also known as logistic function.
}
\examples{
# probabilities 
prob_ICE_base <- 0.3
prob_ICE_risk <- 0.6
# model coefficients
(beta1 <- logit(prob_ICE_base))
(beta2 <- logit(prob_ICE_risk) - logit(prob_ICE_base))
# linear predictor 
logit(prob_ICE_base); (lin_pred1 <- beta1 + beta2*0)
logit(prob_ICE_risk); (lin_pred2 <- beta1 + beta2*1)
# inverse logit of linear predictor 
(inv_logit(lin_pred1)) # prob for X1 = 0
(inv_logit(lin_pred2)) # prob for X1 = 1
}
\seealso{
\code{\link[=logit]{logit()}}
}
