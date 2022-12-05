#' Get data on p-values
#'
#' Helper function, extracts data on p-values for `plot_significance`.
#'
#' @param donut_model Object of class `donut_model`
#' @param filter_80 Boolean: should the result be ignored if the treated pop. is >80%?
#'
#' @importFrom tibble tibble
#' @import dplyr
#'
#' @keywords internal
#' @export
#'

get_p_data.plm <- function (donut_model, filter_80, ...) {
  if ((filter_80 == TRUE &
       donut_model$n_treated / (donut_model$model |> nrow()) <= 0.8) |
      filter_80 == FALSE) {
    s <- summary(donut_model)
    tibble(
      name = s[["coefficients"]] |> rownames(),
      inner = donut_model[["radius"]][["inner"]],
      outer = donut_model[["radius"]][["outer"]],
      coefficient = s[["coefficients"]][, 1],
      positive = coefficient >= 0,
      pval = s[["coefficients"]][, 4],
      p01 = pval < .01,
      p05 = pval < .05,
      p10 = pval < .1,
      stars = factor(x = ifelse(
        p01 == TRUE, 3,
        ifelse(p05 == TRUE, 2,
               ifelse(p10 == TRUE, 1, 0))
      ),
      levels = 3:0)
    )
  }
}
