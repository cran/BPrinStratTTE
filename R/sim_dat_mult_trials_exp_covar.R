#' Simulate data from multiple two-arm trials with an exponentially distributed time-to-event endpoint and one predictor of the intercurrent event
#'
#' @param n_iter Positive integer value, number of trials to be simulated.
#' @param params List of data parameters as used in `sim_dat_one_trial_exp_covar`.
#'
#' @return A list of length `n_iter`, containing objects of class `tibble()`, each containing one simulated trial dataset.
#' @export
#'
#' @seealso [sim_dat_mult_trials_exp_nocovar()]
#'
#' @examples
#' d_params_covar <- list(
#'   n = 1000,        
#'   nt = 500,       
#'   prob_X1 = 0.4, 
#'   prob_ice_X1 = 0.5, 
#'   prob_ice_X0 = 0.2,
#'   fu_max = 336L,   
#'   prop_cens = 0.15,    
#'   T0T_rate = 0.2,     
#'   T0N_rate = 0.2,     
#'   T1T_rate = 0.15,     
#'   T1N_rate = 0.1
#'  )
#' dat_mult_trials <- sim_dat_mult_trials_exp_covar(
#'   n_iter = 3L,
#'   params = d_params_covar 
#' )
#' lapply(dat_mult_trials, dim)
#' head(dat_mult_trials[[1]]) 
#' 
sim_dat_mult_trials_exp_covar <- function(n_iter, params) {
  replicate(
    n_iter,
    sim_dat_one_trial_exp_covar(
      n           = params[["n"]],        
      nt          = params[["nt"]],
      prob_X1     = params[["prob_X1"]], 
      prob_ice_X1 = params[["prob_ice_X1"]], 
      prob_ice_X0 = params[["prob_ice_X0"]],
      fu_max      = params[["fu_max"]],
      prop_cens   = params[["prop_cens"]],
      T0T_rate    = params[["T0T_rate"]],     
      T0N_rate    = params[["T0N_rate"]],     
      T1T_rate    = params[["T1T_rate"]],     
      T1N_rate    = params[["T1N_rate"]] 
    ),
    simplify = FALSE
  ) %>%
    lapply(FUN = function(x) x[!(names(x) %in% c("PAT_ID","T0N","T0T","T1N","T1T"))])
}
