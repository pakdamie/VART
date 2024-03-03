#' Creation of parameters to simulate the phenology with
#'
#' @param shape The shape parameter for the Erlang distribution
#' @param initial_val The initial starting value of the first stage individuals (i.e Juveniles)
#' @param dev_val The development rate (i.e The mean development time is 1/dev)
#' @param mu_val The natural moratlity rate of the second stage individuals (adults)
#' @param m1_val The Hill function of the pesticide parameter
#' @param m2_val The Hill function of the pesticide parameter
#' @param mk_val The Hill function of the pesticide parameter
#' @param delay1 The decay rate of the pesticide
#' @param delay1 DO you already have the action threshold, then include a
#' a datframe (default is NA)
#' @return A list with two elements: the initial value or the parameters
#' @export
#'
#' @examples params_creator(100)
params_creator <- function(shape_val, initial_val = 1000,
                           dev_val = 1 / 30,
                           mu_val = 0.33,
                           m1_val = 8,
                           m2_val = 0.5,
                           mk_val = 5,
                           delay1 = 1 / 5,
                           action_threshold = NA) {


  if (is.null(nrow(action_threshold))== FALSE) {
          initial <- c(S1 = c(initial_val, rep(0, shape_val - 1)), S2 = 0, P = 0)
  }else {
          initial <- c(S1 = c(initial_val, rep(0, shape_val - 1)), S2 = 0)
  }



  params <- c(
    n = shape_val, # shape parameter,
    dev = dev_val, # development rate,
    mu = mu_val, # mortality rate
    m1 = m1_val,
    m2 = m2_val,
    mk = mk_val,
    a = delay1
  )


  if (is.null(nrow(action_threshold))== TRUE) {
    return(list(initial, params, NA))
  } else {

    AT <- subset(action_threshold, action_threshold$shape == params[['n']])

    return(list(initial, params, AT$time))
  }
}
