#' Simulate data from multiple two-arm trials with an exponentially distributed time-to-event endpoint and no predictor of the intercurrent event
#'
#' @param n_iter Positive integer value, number of trials to be simulated.
#' @param params List of data parameters as used in `sim_dat_one_trial_exp_nocovar`.
#'
#' @return A list of length `n_iter`, containing objects of class `tibble()`, each containing one simulated trial dataset.
#' @export
#' 
#' @seealso [sim_dat_mult_trials_exp_covar()]
#'
#' @examples
#' d_params_nocovar <- list(
#'   n = 500L,
#'   nt = 250L,
#'   prob_ice = 0.5,
#'   fu_max = 336L,
#'   T0T_rate = 0.2,
#'   T0N_rate = 0.2,
#'   T1T_rate = 0.15,
#'   T1N_rate = 0.1
#' )
#' dat_mult_trials <- sim_dat_mult_trials_exp_nocovar(
#'   n_iter = 3,
#'   params = d_params_nocovar 
#' )
#' lapply(dat_mult_trials, dim)
#' head(dat_mult_trials[[1]]) 
#' 
sim_dat_mult_trials_exp_nocovar <- function(n_iter, params) {
  replicate(
    n_iter,
    sim_dat_one_trial_exp_nocovar(
      n = params[["n"]],
      nt = params[["nt"]],
      prob_ice = params[["prob_ice"]],
      fu_max = params[["fu_max"]],
      T0T_rate = params[["T0T_rate"]],
      T0N_rate = params[["T0N_rate"]],
      T1T_rate = params[["T1T_rate"]],
      T1N_rate = params[["T1N_rate"]]
    ),
    simplify = F
  ) %>%
    lapply(FUN = function(x) x[!(names(x) %in% c("PAT_ID","T0N","T0T","T1N","T1T"))])
}