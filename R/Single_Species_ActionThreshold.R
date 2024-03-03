#' Calculate the action threshold for the single species due to the inflection
#' point
#'
#' @param output_dataframe The dynamics output
#' @param inflection_df The data.frame containing the timing of the inflection
#' for each of the shape parameter
#'
#' @return A list of the shape and action thresholds
#' @export
#'
#' @examples Single_Species_ActionThreshold(DF, Inflection_DF)
#'
Single_Species_ActionThreshold <- function(output_dataframe, inflection_df) {
  AT_threshold <- NULL
  for (shape in seq(1, nrow(inflection_df))) {
    approximate_function <- approxfun(
      Single_Species_Dynamics[[shape]]$time,
      Single_Species_Dynamics[[shape]]$S2
    )

    inflection_time <- inflection_df[shape, ]$time

    AT_Abundance <- approximate_function(inflection_time)
    AT_threshold[[shape]] <- cbind.data.frame(
      AT = AT_Abundance,
      Shape = Shape[[shape]]
    )
  }
  return(AT_threshold)
}
