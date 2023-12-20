#' Simulate data from a single two-arm trial with an exponentially distributed time-to-event endpoint and no predictor of the intercurrent event
#'
#' @param n Positive integer value, number of subjects in the trial.
#' @param nt Positive integer value, number of treated subjects.
#' @param prob_ice Numeric value on the interval \eqn{(0,1)}, probability of the intercurrent event of interest (i.e. the event that determines the principal stratum membership).
#' @param fu_max Positive integer value, maximum follow-up time in days (administrative censoring assumed afterwards).
#' @param T0T_rate Positive numeric value, monthly event rate in control subjects that would develop the intercurrent event if treated.
#' @param T0N_rate Positive numeric value, monthly event rate in control subjects that never develop the intercurrent event.
#' @param T1T_rate Positive numeric value, monthly event rate in treated subjects that develop the intercurrent event.
#' @param T1N_rate Positive numeric value, monthly event rate in treated subjects that never develop the intercurrent event.
#'
#' @return A `tibble()`containing the trial data for analysis.
#' @export
#'
#' @seealso [sim_dat_one_trial_exp_covar()]
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
#' dat_single_trial <- sim_dat_one_trial_exp_nocovar(
#'   n = d_params_nocovar[["n"]], 
#'   nt = d_params_nocovar[["nt"]],
#'   prob_ice = d_params_nocovar[["prob_ice"]],
#'   fu_max = d_params_nocovar[["fu_max"]],  
#'   T0T_rate = d_params_nocovar[["T0T_rate"]],
#'   T0N_rate = d_params_nocovar[["T0N_rate"]],
#'   T1T_rate = d_params_nocovar[["T1T_rate"]],
#'   T1N_rate = d_params_nocovar[["T1N_rate"]] 
#' )
#' dim(dat_single_trial)
#' head(dat_single_trial)
#' 
sim_dat_one_trial_exp_nocovar <- function(
  n,             # number of patients
  nt,            # number of treated patients
  prob_ice,      # prob ICE 
  fu_max,        # maximum follow-up (days)
  T0T_rate,      # monthly event rate in controls TD
  T0N_rate,      # monthly event rate in controls ND
  T1T_rate,      # monthly event rate in treated TD
  T1N_rate       # monthly event rate in treated ND
) {
  # baseline data
  Z  <- sample(c(rep(0L, n - nt), rep(1L, nt)))
  # intercurrent event data
  G <- sample(c(0L, 1L), size = n, prob = c(1 - prob_ice, prob_ice), replace = T)
  S <- G
  S[Z==0L] <- 0L
  # time to event endpoint data by principal stratum
  cens <- runif(n = n, min = 1, max = fu_max)
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
    tibble::tibble(
      PAT_ID = stringr::str_pad(1:n, nchar(n), pad = "0"),
      Z = Z,
      G = G,
      S = S,
      TIME = TIME,
      EVENT = EVENT,
      T0N = T0N,
      T0T = T0T,
      T1N = T1N,
      T1T = T1T
    )
  ) 
}
