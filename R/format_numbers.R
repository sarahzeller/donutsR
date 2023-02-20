#' Helper function for modelsummary
#'
#' Formats numbers for nobs and R2
#'
#'
#' @keywords internal
#' @export
format_numbers <- function() {
  f1 <- function(x) format(round(x, 3), big.mark=",")
  gof_tidy <- list(
    list("raw" = "nobs", "clean" = "Num. Observations", "fmt" = f1),
    list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3))
}
