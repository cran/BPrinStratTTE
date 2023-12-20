#' Adding true values to estimates for models with an exponential endpoint and consideration of predictors of the intercurrent event
#' 
#' @param x Model object as returned by `fit_single_exp_covar()`.
#' @param d_params List of data parameters as used in `fit_single_exp_covar()`.
#' @param m_params List of model parameters as used in `fit_single_exp_covar()`.
#'
#' @return A summary table with parameter estimates, true values and differences.
#' @export
#' 
#' @seealso [true_vals_exp_nocovar()]
#' 
#' @examples
#' d_params_covar <- list(
#'   n = 1000,        
#'   nt = 500,       
#'   prob_X1 = 0.4, 
#'   prob_ice_X1 = 0.5, 
#'   prob_ice_X0 = 0.2,
#'   fu_max = 48*7,       
#'   T0T_rate = 0.2,     
#'   T0N_rate = 0.2,     
#'   T1T_rate = 0.15,     
#'   T1N_rate = 0.1
#'  )
#' dat_single_trial <- sim_dat_one_trial_exp_covar(
#'   n = d_params_covar[["n"]], 
#'   nt = d_params_covar[["nt"]],
#'   prob_X1 = d_params_covar[["prob_X1"]],
#'   prob_ice_X1 = d_params_covar[["prob_ice_X1"]],
#'   prob_ice_X0 = d_params_covar[["prob_ice_X0"]],
#'   fu_max = d_params_covar[["fu_max"]],  
#'   T0T_rate = d_params_covar[["T0T_rate"]],
#'   T0N_rate = d_params_covar[["T0N_rate"]],
#'   T1T_rate = d_params_covar[["T1T_rate"]],
#'   T1N_rate = d_params_covar[["T1N_rate"]] 
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
#' fit_single <- fit_single_exp_covar(
#'   data = dat_single_trial,
#'   params = m_params_covar,
#'   summarize_fit = TRUE
#' )
#' print(fit_single)
#' tab_obs_truth <- true_vals_exp_covar(
#'   x = fit_single,
#'   d_params = d_params_covar,
#'   m_params = m_params_covar
#' )
#' print(tab_obs_truth)
#' } 
true_vals_exp_covar <- function(x, d_params, m_params) {
  # use model summary
  patterns <- c("S_", "lp", "n_eff")
  y <- tibble::as_tibble(x) %>% 
    dplyr::filter(!grepl(paste(patterns, collapse="|"), var))
  # add new variables
  y <- y %>% dplyr::mutate(
    true_val = NA, diff_mean = NA, diff_median = NA, 
    coverage = NA, int_excl1 = NA, int_excl0 = NA,
    rhat_check = NA) 
  # add true proportion of patients with ICE
  y <- y %>% mutate(
    true_val = ifelse(var=="delta[1]", logit(d_params[["prob_ice_X0"]]), true_val),
    true_val = ifelse(var=="delta[2]",
                      logit(d_params[["prob_ice_X1"]])-logit(d_params[["prob_ice_X0"]]),
                      true_val)
  )
  # add true hazard rate and hazard ratios
  y <- y %>% dplyr::mutate(
    true_val = ifelse(var=="lambda_0N", d_params[["T0N_rate"]], true_val),
    true_val = ifelse(var=="lambda_1N", d_params[["T1N_rate"]], true_val),
    true_val = ifelse(var=="lambda_0T", d_params[["T0T_rate"]], true_val),
    true_val = ifelse(var=="lambda_1T", d_params[["T1T_rate"]], true_val),
    true_val = ifelse(var=="hr_N", d_params[["T1N_rate"]]/d_params[["T0N_rate"]], 
                      true_val),
    true_val = ifelse(var=="hr_T", d_params[["T1T_rate"]]/d_params[["T0T_rate"]], 
                      true_val)
  )
  # add RMSTs and differences in RMSTs
  t_grid <- m_params[["t_grid"]]
  S_0n <- 1-pexp(t_grid, d_params[["T0N_rate"]])
  S_1n <- 1-pexp(t_grid, d_params[["T1N_rate"]])
  S_0c <- 1-pexp(t_grid, d_params[["T0T_rate"]])
  S_1c <- 1-pexp(t_grid, d_params[["T1T_rate"]])
  rmst_0n <- rmst_1n <- rmst_0c <- rmst_1c <- 0
  for (j in 2:length(t_grid)) {
    rmst_0n <- rmst_0n + ((S_0n[j-1]+S_0n[j])/2*(t_grid[j]-t_grid[j-1]))
    rmst_1n <- rmst_1n + ((S_1n[j-1]+S_1n[j])/2*(t_grid[j]-t_grid[j-1]))
    rmst_0c <- rmst_0c + ((S_0c[j-1]+S_0c[j])/2*(t_grid[j]-t_grid[j-1]))
    rmst_1c <- rmst_1c + ((S_1c[j-1]+S_1c[j])/2*(t_grid[j]-t_grid[j-1]))
  }
  d_rmst_n <- rmst_1n - rmst_0n
  d_rmst_c <- rmst_1c - rmst_0c
  y <- y %>% dplyr::mutate(
    true_val = ifelse(var=="rmst_N", d_rmst_n, true_val),
    true_val = ifelse(var=="rmst_T", d_rmst_c, true_val),
    true_val = ifelse(var=="rmst_0N", rmst_0n, true_val),
    true_val = ifelse(var=="rmst_1N", rmst_1n, true_val),
    true_val = ifelse(var=="rmst_0T", rmst_0c, true_val),
    true_val = ifelse(var=="rmst_1T", rmst_1c, true_val)
  )
  # add comparisons to true values and checks
  y <- y %>% dplyr::mutate(
    diff_mean = mean - true_val,
    diff_median = `50%` - true_val,
    coverage = ifelse(true_val >=`2.5%` & true_val <= `97.5%`, 1, 0),
    int_excl1= ifelse(1 <=`2.5%` | 1 >= `97.5%`, 1, 0),
    int_excl0= ifelse(0 <=`2.5%` | 0 >= `97.5%`, 1, 0),
    rhat_check = ifelse(Rhat >=0.98 & Rhat <= 1.02, 1, 0),
  )
  # return summary table
  return(y)
}