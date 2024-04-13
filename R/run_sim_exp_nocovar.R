#' Run simulation of two-arm trials with an exponentially distributed time-to-event endpoint and no predictor of the intercurrent event
#'
#' @param n_iter Positive integer value, number of trials to be simulated.
#' @param d_params List of data parameters as used in `sim_dat_one_trial_exp_nocovar`.
#' @param m_params List of model parameters as used in `fit_single_exp_nocovar`.
#' @param seed Numeric value, seed for reproducibility.
#'
#' @return A list of length 3, containing objects call `ocs`, `d_params`, `m_params`, where `ocs` is a `tibble` containing averaged parameter estimates and operating characteristics, and `d_params` and `m_params` are the objects supplied to the function.
#' @export
#'
#' @seealso [run_sim_exp_covar()]
#'
#' @examples
#' d_params_nocovar <- list(
#'   n = 500L,
#'   nt = 250L,
#'   prob_ice = 0.5,
#'   fu_max = 336L,
#'   prop_cens = 0.15,
#'   T0T_rate = 0.2,
#'   T0N_rate = 0.2,
#'   T1T_rate = 0.15,
#'   T1N_rate = 0.1
#' )
#' m_params_nocovar <- list(
#'   tg = 48L,
#'   prior_piT = c(0.5, 0.5),
#'   prior_0N = c(1.5, 5),
#'   prior_1N = c(1.5, 5),
#'   prior_0T = c(1.5, 5),
#'   prior_1T = c(1.5, 5),
#'   t_grid =  seq(7, 7 * 48, 7) / 30,
#'   chains = 2L,
#'   n_iter = 3000L,
#'   warmup = 1500L,
#'   cores = 2L,
#'   open_progress = FALSE,
#'   show_messages = TRUE  
#' )
#' \donttest{ 
#' dat_ocs <- run_sim_exp_nocovar(
#'   n_iter = 3, 
#'   d_params = d_params_nocovar, 
#'   m_params = m_params_nocovar, 
#'   seed = 12
#'   )
#' print(dat_ocs)
#' }
run_sim_exp_nocovar <- function(
  n_iter, d_params, m_params, seed
) {
  # Simulate data
  d_mult <- sim_dat_mult_trials_exp_nocovar(
    n_iter = n_iter, 
    params = d_params
    )
  # Run models
  multiple_fits <- fit_mult_exp_nocovar(
    dat_mult_trials = d_mult,
    params = m_params,
    seed = seed
  )
  # Obtain operating characteristics
  sim <- ocs_exp_nocovar(
    multiple_fits = multiple_fits,
    d_params = d_params,
    m_params = m_params
    )
  return(sim)
}