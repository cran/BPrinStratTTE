#' Determine operating characteristics of fits from two-arm trials with an exponentially distributed time-to-event endpoint and one predictor of the intercurrent event
#'
#' @param multiple_fits List of model fits from `fit_mult_exp_covar`.
#' @param d_params List of data parameters as used in `sim_dat_one_trial_exp_covar`.
#' @param m_params List of model parameters as used in `fit_single_exp_covar`.
#'
#' @return A list of length 3, containing objects call `ocs`, `d_params`, `m_params`, where `ocs` is a `tibble` containing averaged parameter estimates and operating characteristics, and `d_params` and `m_params` are the objects supplied to the function.
#' @export
#'
#' @details
#' This function is used in `run_sim_exp_covar()`, the output of the two functions is the same.
#' 
#' @seealso [ocs_exp_nocovar()] and [run_sim_exp_covar()].
#'
#' @examples
#' d_params_covar <- list(
#'   n = 1000,        
#'   nt = 500,       
#'   prob_X1 = 0.4, 
#'   prob_ice_X1 = 0.5, 
#'   prob_ice_X0 = 0.2,
#'   fu_max = 48*7,
#'   prop_cens = 0.15,       
#'   T0T_rate = 0.2,     
#'   T0N_rate = 0.2,     
#'   T1T_rate = 0.15,     
#'   T1N_rate = 0.1
#'  )
#' dat_mult_trials <- sim_dat_mult_trials_exp_covar(
#'   n_iter = 2,
#'   params = d_params_covar 
#' )
#' m_params_covar <- list(
#'   tg = 48,
#'   p = 2, 
#'   prior_delta = matrix(
#'     c(0, 5, 0, 5),
#'     nrow = 2, byrow = TRUE),
#'   prior_0N = c(1.5, 5),
#'   prior_1N = c(1.5, 5),
#'   prior_0T = c(1.5, 5),
#'   prior_1T = c(1.5, 5),
#'   t_grid =  seq(7, 7 * 48, 7) / 30,
#'   chains = 2,
#'   n_iter = 3000,
#'   warmup = 1500,
#'   cores = 2,
#'   open_progress = FALSE,
#'   show_messages = TRUE
#' )
#' \donttest{
#' fit_multiple <- fit_mult_exp_covar(
#'   dat_mult_trials = dat_mult_trials,
#'   params = m_params_covar,
#'   seed = 12
#' )
#' list_ocs <- ocs_exp_covar(
#'   multiple_fits = fit_multiple, 
#'   d_params = d_params_covar, 
#'   m_params = m_params_covar
#' )
#' print(list_ocs)
#' }
#' 
ocs_exp_covar <- function(multiple_fits, d_params, m_params) {
  # obtain names of parameters to evaluate
  var <- multiple_fits[[1]] %>% select(var)
  # aggregate (numeric variables)
  ocs <- purrr::map(
    .x = multiple_fits,
    .f = ~ true_vals_exp_covar(
      x = .x,
      d_params = d_params,
      m_params = m_params
    )
  ) %>%
    purrr::map(~ dplyr::select(.x, -var)) %>%
    purrr::map(as.matrix) %>%
    simplify2array() %>%
    apply(c(1, 2), mean)
  # add names of parameters
  ocs <- tibble::as_tibble(cbind(var = var, ocs))
  # return ocs and data/model parameters
  return(list(
    "ocs" = ocs, 
    "d_params" = unlist(d_params),
    "m_params" = unlist(m_params)
  ))
}