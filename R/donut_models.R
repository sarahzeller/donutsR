#' Perform a Donut FE analysis
#'
#' Performs a single FE-models with a given data set.
#' The independent variable and the population can be adjusted.
#'
#' @param inner A vector with the inner radii.
#' @param outer A vector with the outer radii
#' @param ds The dataset
#' @param indep_vars The independent variables to pass on to `donut_analysis`
#' @param ... Additional arguments
#'
#' @importFrom data.table CJ
#' @importFrom data.table transpose
#'
#' @export
#'
#' @return A donut_models list: `donut_list`
#'

# convenience wrappers for donut_analysis
donut_models <- function(inner = 3:9, outer = (1:5)*10,
                         ds,
                         indep_vars,
                         ...) {
  models <- CJ(inner, outer) |>
    transpose() |>
    lapply(donut_analysis,
           ds = ds,
           indep_vars = indep_vars,
           ...)
  class(models) <- c("donut_list", class(models))
  return(models)
}
