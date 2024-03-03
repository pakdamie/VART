#' Inflection finder, findds the infection of the emergence curve
#'
#' @param list_dynamics Output from the SingleSpecies_Dynamics
#'
#' @return dataframe containing the shape parameter as well as the time of inflection
#' @export
#'
#' @examples Inflection_Finder(x)
Inflection_Finder <- function(list_dynamics) {
  approximate_function <- approxfun(list_dynamics$time, list_dynamics$D2)
  root_D2 <- uniroot(approximate_function,
    c(5, 30),
    extendInt = "yes"
  )$root

  return(cbind.data.frame(time = root_D2, shape = unique(list_dynamics$Shape)))
}
