#' Perform a Donut FE analysis
#'
#' Performs multiple FE-models with a given data set.
#' The independent variable and the population vary.
#'
#' @param dist A vector with two distances: the inner and the outer.
#'
#' @importFrom plm plm
#' @import dplyr
#' @import dtplyr
#' @importFrom stats reformulate
#' @import assertthat
#' @importFrom utils globalVariables
#'
#' @return A a list of plm models.
#'
#' @export
#'

donut_analysis <- function(){
  data(Cigar)
  Cigar <- Cigar |>  mutate(dist_km = rnorm(nrow(Cigar), 20, 10)) |>  filter(dist_km >= 0)
  model <-
  basic_donut_fe(dist = c(5, 20),
                 ds = Cigar,
                 dep_var = "price", indep_vars = "pop", fe = "state")
  return(model)
}
