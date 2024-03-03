#' Calculate the total area under curve without intervention
#'
#' Due to the attrition in the second stage, the total damage (the area under the curve)
#' may be different depending on the shape parameter. This function calculates the initial
#' starting value so that the total insect damage is the same for all shapes IF there is no
#' interventions
#' @useDynLib VART
#' @importFrom Rcpp sourceCpp

#' @param initial_individuals The initial starting value of the first stage individuals (i.e Juveniles)
#' @param dev The development rate (i.e The mean development time is 1/dev)
#' @param mu The natural moratlity rate of the second stage individuals (adults)
#'
#'
#' @return A data.frame with the starting initial value in the first stage depending on the shape
#' @export
#'
#' @examples onesp_insect_damage(1000, 1 / 30, 4)
SingleSpecies_AUC <- function(initial, dev, mu) {
  ### If the max n shape is 100, that means this would ultimately
  ### have the highest peak abundance!

  userinput_shape_params <- c(n = 3000, dev = dev, mu = mu) # Puts the parameter in to a variable

  userinput_initial <- c(
    S1 = c(
      initial,
      rep(0, userinput_shape_params["n"] - 1)
    ),
    S2 = 0
  )

  output_n_discrete <- data.frame(
    ode(
      y = userinput_initial,
      times = seq(1, 100, 1 / 10),
      func = ErlangInsectSimple,
      parms = userinput_shape_params
    )
  )

  ### We create an approximation function to calculate the area under the curve

  AUC_output_prime <- trapz(output_n_discrete$time, output_n_discrete$S2)

  ### This is the optimizing function as we are looking at how
  ### the initial values should change so that the
  ### total area under the curve should be the same regardless
  ### of the shape parameter.

  optimizer_AUC <- function(initial_val) {
    ### For initial conditions with the shape_loop
    ### representing n
    initial_conditions <- c(
      S1 = c(
        initial_val,
        rep(0, shape_loop - 1)
      ),
      S2 = 0
    )

    ### This creates the data.frame
    output_shape <-
      data.frame(ode(
        y = initial_conditions,
        time = seq(1, 100, 1 / 10),
        func = ErlangInsectSimple,
        parms = dparams
      ))

    AUC_output_shape <- trapz(output_shape$time, output_shape$S2)


    return((AUC_output_prime - AUC_output_shape)^2)
  }

  shapes_initial_List <- NULL

  CV <- seq(0.1, 0.5, 0.05)
  Shape <- c(1, round(1 / (CV^2)))

  for (s in seq(1, length(Shape))) {
    dparams <- userinput_shape_params
    shape_loop <- Shape[s]
    dparams["n"] <- Shape[s]

    optim_initial <- optim(1000,
      fn = optimizer_AUC,
      method = "Brent",
      lower = 1,
      upper = 5000
    )
    ### returns the data.frame of
    ### what shape and what the initial should be
    shapes_initial_List[[s]] <- c(shape_loop, optim_initial$par)
  }
  return(do.call(rbind, shapes_initial_List))
}


