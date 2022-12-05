#' Get data on p-values
#'
#' Helper function, extracts data on p-values for `plot_significance`.
#'
#' @param donut_model Object of class `donut_model`
#' @param filter_80 Boolean: should the result be ignored if the treated pop. is >80%?
#' @param name The name of the variable to be plotted
#'
#' @importFrom tibble tibble
#' @importFrom fixest coeftable
#' @import dplyr
#'
#' @keywords internal
#' @export

get_p_data.fixest <- function(donut_model, filter_80, name, ...) {
  if ((filter_80 == TRUE &
       donut_model$n_treated / donut_model$nobs <= 0.8) |
      filter_80 == FALSE) {
    tibble(
      name = rownames(coeftable(donut_model)),
      inner = donut_model[["radius"]][["inner"]],
      outer = donut_model[["radius"]][["outer"]],
      coefficient = donut_model[["coefficients"]],
      positive = coefficient >= 0,
      pval = coeftable(donut_model)[, 4]
      # p01 = pval < .01,
      # p05 = pval < .05,
      # p10 = pval < .1,
    ) |>
      # bootstrapped p-values
      mutate(pval_bs = ifelse(
        name == "dist" &
          "bootstrap_dist" %in% names(donut_model),
        donut_model[["bootstrap_dist"]][["p_val"]],
        pval
      )) |>
      mutate(p01 = pval_bs < .01) |>
      mutate(p05 = pval_bs < .05) |>
      mutate(p10 = pval_bs < .1) |>
      mutate(stars = factor(
        x = ifelse(p01 == TRUE, 3,
                   ifelse(
                     p05 == TRUE, 2,
                     ifelse(p10 == TRUE, 1, 0)
                   )),
        levels = 3:0
      ))

  }
}
