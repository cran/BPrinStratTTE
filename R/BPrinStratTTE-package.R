#' The 'BPrinStratTTE' package.
#'
#' @description Bayesian models to estimate causal effects of biological 
#'     treatments on time-to-event endpoints in clinical trials with principal
#'     strata defined by the occurrence of antidrug antibodies. 
#'     The methodology is based on Frangakis and Rubin (2002) 
#'     <doi:10.1111/j.0006-341x.2002.00021.x> and Imbens and Rubin (1997)
#'     <doi:10.1214/aos/1034276631>, and intended to be applied to a 
#'     specific time-to-event setting.#'
#' @name BPrinStratTTE-package
#' @aliases BPrinStratTTE
#' @useDynLib BPrinStratTTE, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#'
#' @references
#' Stan Development Team (2022). RStan: the R interface to Stan. R package version 2.21.5. https://mc-stan.org
#'
NULL

## usethis namespace: start
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom furrr future_map
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats pexp
#' @importFrom stats rexp
#' @importFrom stats runif
#' @importFrom stringr str_pad
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
## usethis namespace: end
NULL
