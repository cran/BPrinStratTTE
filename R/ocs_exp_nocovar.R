#' Determine operating characteristics of fits from two-arm trials with an exponentially distributed time-to-event endpoint and no predictor of the intercurrent event
#'
#' @param multiple_fits List of model fits from `fit_mult_exp_nocovar`.
#' @param d_params List of data parameters as used in `sim_dat_one_trial_exp_nocovar`.
#' @param m_params List of model parameters as used in `fit_single_exp_nocovar`.
#'
#' @return A list of length 3, containing objects call `ocs`, `d_params`, `m_params`, where `ocs` is a `tibble` containing averaged parameter estimates and operating characteristics, and `d_params` and `m_params` are the objects supplied to the function.
#' @export
#' 
#' @details
#' This function is used in `run_sim_exp_nocovar()`, the output of the two functions is the same.
#' 
#' @seealso [ocs_exp_covar()] and [run_sim_exp_nocovar()].
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
#'   n_iter = 2,
#'   params = d_params_nocovar 
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
#' fit_multiple <- fit_mult_exp_nocovar(
#'   dat_mult_trials = dat_mult_trials,
#'   params = m_params_nocovar,
#'   seed = 12
#' )
#' list_ocs <- ocs_exp_nocovar(
#'   multiple_fits = fit_multiple, 
#'   d_params = d_params_nocovar, 
#'   m_params = m_params_nocovar
#' )
#' print(list_ocs)
#' }
ocs_exp_nocovar <- function(multiple_fits, d_params, m_params) {
  # obtain names of parameters to evaluate
  var <- multiple_fits[[1]] %>% select(var)
  # aggregate (numeric variables)
  ocs <- purrr::map(
    .x = multiple_fits,
    .f = ~ true_vals_exp_nocovar(
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