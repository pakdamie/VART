#' Plotting the dynamics of a single species and calculating the first and
#' second derivative
#'
#' @param list_params_init Output from the params_creator (list with two elments)
#' @param plot Plot the output as well as the first and second derivatives
#'
#' @return A data.frame of all the results as well as the plots
#' @export
#'
#' @examples SingleSpecies_Dynamics(x)
SingleSpecies_Dynamics <- function(list_params_init) {
  x_int_interest <- list_params_init[[1]] # the initial values
  param_int_interest <- list_params_init[[2]] # the parameters

  output <- data.frame(lsodar(
    y = x_int_interest,
    times = seq(0, 100, 1 / 10),
    func = ErlangInsectSimple,
    parms = param_int_interest,
    rtol = 1e-4,
    atol = 1e-6
  ))

  ### this is the output but we're only interested in the adult
  output_S2 <- output[, c("time", "S2")]
  output_S2$Shape <- param_int_interest["n"]


  ### To calculate the first and second derivative
  S2_Function <- splinefun(output_S2$time, output_S2$S2)

  output_dynamics <- cbind.data.frame(
    output_S2, # dynamics
    D1 = S2_Function(output_S2$time, deriv = 1), # first derivative
    D2 = S2_Function(output_S2$time, deriv = 2)
  ) # second derivative

  return(output_dynamics)
}
