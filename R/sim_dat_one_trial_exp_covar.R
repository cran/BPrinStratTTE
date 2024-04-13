#' Simulate data from a single two-arm trial with an exponentially distributed time-to-event endpoint and one predictor of the intercurrent event
#'
#' @param n Positive integer value, number of subjects in the trial.
#' @param nt Positive integer value, number of treated subjects.
#' @param prob_X1 Numeric value on the interval \eqn{(0,1)}, probability of being at high risk of experiencing the intercurrent event of interest when treated (i.e. the event that determines the principal stratum membership).
#' @param prob_ice_X1 Numeric value on the interval \eqn{(0,1)}, probability of the intercurrent event of interest if treated and at high risk of the intercurrent event.
#' @param prob_ice_X0 Numeric value on the interval \eqn{(0,1)}, probability of the intercurrent event of interest if treated and not at high risk of the intercurrent event. 
#' @param fu_max Positive integer value, maximum follow-up time in days (administrative censoring assumed afterwards).
#' @param prop_cens Numeric value on the interval \eqn{[0,1)}, proportion of uniformly censored patients (default is 0). 
#' @param T0T_rate Positive numeric value, monthly event rate in control subjects that would develop the intercurrent event if treated.
#' @param T0N_rate Positive numeric value, monthly event rate in control subjects that never develop the intercurrent event.
#' @param T1T_rate Positive numeric value, monthly event rate in treated subjects that develop the intercurrent event.
#' @param T1N_rate Positive numeric value, monthly event rate in treated subjects that never develop the intercurrent event.
#'
#' @return ...
#' @export
#' 
#' @seealso [sim_dat_one_trial_exp_nocovar()]
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
#' dat_single_trial <- sim_dat_one_trial_exp_covar(
#'   n = d_params_covar[["n"]], 
#'   nt = d_params_covar[["nt"]],
#'   prob_X1 = d_params_covar[["prob_X1"]],
#'   prob_ice_X1 = d_params_covar[["prob_ice_X1"]],
#'   prob_ice_X0 = d_params_covar[["prob_ice_X0"]],
#'   fu_max = d_params_covar[["fu_max"]],
#'   prop_cens = d_params_covar[["prop_cens"]],  
#'   T0T_rate = d_params_covar[["T0T_rate"]],
#'   T0N_rate = d_params_covar[["T0N_rate"]],
#'   T1T_rate = d_params_covar[["T1T_rate"]],
#'   T1N_rate = d_params_covar[["T1N_rate"]] 
#' )
#' dim(dat_single_trial)
#' head(dat_single_trial)
#'                   
sim_dat_one_trial_exp_covar <- function(
    n,             # number of patients
    nt,            # number of treated patients
    prob_X1,       # probability of X=1
    prob_ice_X1,   # probability of intercurrent event for X=1
    prob_ice_X0,   # probability of intercurrent event for X=0
    fu_max,        # maximum follow-up (days)
    prop_cens = 0, # proportion of censored patients
    T0T_rate,      # monthly event rate in controls TD
    T0N_rate,      # monthly event rate in controls ND
    T1T_rate,      # monthly event rate in treated TD
    T1N_rate       # monthly event rate in treated ND
) {
  # baseline data
  Z  <- sample(c(rep(0L, n - nt), rep(1L, nt)))
  # binary covariate
  X <- sample(c(0, 1), size = n, 
              prob = c(1 - prob_X1, prob_X1), replace = T)
  n_X1 <- sum(X==1)
  # intercurrent event data
  G <- rep(NA, n)
  G[X==1] <- sample(c(0,1), size = n_X1,
                    prob = c(1 - prob_ice_X1, prob_ice_X1), replace = T)
  G[X==0] <- sample(c(0,1), size = n - n_X1, 
                    prob = c(1 - prob_ice_X0, prob_ice_X0), replace = T)
  S <- G
  S[Z==0L] <- 0L
  # censoring
  n_cens <- n * prop_cens
  if (n_cens > 0) {
    pat_cens <- sample(x = 1:n, size = n_cens) |> sort()
    unif_cens <- runif(n = length(pat_cens), min = 1, max = fu_max)
    cens <- rep(fu_max, n)
    cens[pat_cens] <- unif_cens
  } 
  if (n_cens == 0) {
    cens <- rep(fu_max, n)
  }
  # time to event endpoint data by principal stratum
  T0T <- rexp(n = n, rate = T0T_rate) * 30
  T0N <- rexp(n = n, rate = T0N_rate) * 30
  T1T <- rexp(n = n, rate = T1T_rate) * 30
  T1N <- rexp(n = n, rate = T1N_rate) * 30
  E0T <- as.integer(T0T <= cens)
  E0N <- as.integer(T0N <= cens)
  E1T <- as.integer(T1T <= cens)
  E1N <- as.integer(T1N <= cens)
  EVENT <- NULL
  EVENT[Z==0 & G==0] <- E0N[Z==0 & G==0]
  EVENT[Z==0 & G==1] <- E0T[Z==0 & G==1]
  EVENT[Z==1 & G==0] <- E1N[Z==1 & G==0]
  EVENT[Z==1 & G==1] <- E1T[Z==1 & G==1]
  TIME <- NULL
  TIME[EVENT==0] <- round(cens[EVENT==0])
  TIME[EVENT==1 & Z==0 & G==0] <- round(T0N[EVENT==1 & Z==0 & G==0])
  TIME[EVENT==1 & Z==0 & G==1] <- round(T0T[EVENT==1 & Z==0 & G==1])
  TIME[EVENT==1 & Z==1 & G==0] <- round(T1N[EVENT==1 & Z==1 & G==0])
  TIME[EVENT==1 & Z==1 & G==1] <- round(T1T[EVENT==1 & Z==1 & G==1])
  TIME <- as.integer(TIME)
  return(
    tibble(
      PAT_ID = stringr::str_pad(1:n, nchar(n), pad = "0"),
      Z = Z,
      X = X,
      G = G,
      S = S,
      TIME = TIME,
      EVENT = EVENT,
      T0N = T0N,
      T0T = T0T,
      T1N = T1N,
      T1T = T1T, 
      CENS_TIME = cens
    )
  ) 
}
