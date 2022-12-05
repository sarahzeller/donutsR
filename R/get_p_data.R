#' Get data on p-values
#'
#' Helper function, extracts data on p-values for `plot_significance`.
#'
#' @param donut_model Object of class `donut_model`
#' @param filter_80 Boolean: should the result be ignored if the treated pop. is >80%?
#'
#' @keywords internal
#' @export

get_p_data <- function(donut_model, filter_80, ...){
  UseMethod("get_p_data")
}
