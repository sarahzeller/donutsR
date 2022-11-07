#' Extract distances from donut_analysis model
#'
#' Convenience function to extract distances from donut_analysis model.
#' Input needs to be a (list of) `donut_analysis` models.
#' Consider adding `theme(legend.text.align = 1)` to right-align the p-value legend.
#'
#' @param donut_models List of `donut_analysis` models
#' @param var The dependent variable in all `donut_analysis` models which should be plotted.
#'
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @import dplyr
#' @importFrom tibble tibble
#'
#' @export
#' @return A ggplot2 plot
#'
#' @examples
#' library(plm)
#' library(dplyr)
#' data(Cigar)
#' Cigar <- Cigar |>  mutate(dist_km = rnorm(nrow(Cigar), 20, 10)) |>  filter(dist_km >= 0)
#' cigar_models <-
#' data.table::CJ(inner = 2:6, outer = (1:5)*10) |>
#'  data.table::transpose() |>
#'  lapply(donut_analysis,
#'         ds = Cigar,
#'         dep_var = "price",
#'         indep_vars = "pop",
#'         fe = "state")
#' plot_significance(cigar_models, var = "pop")


plot_significance <- function(donut_models,
                              var) {
  assert_that(donut_models[[1]][11] |> names() == "radius",
                          msg = "Insert a list of donut_models.")
  assert_that(missing(var) == FALSE,
                          msg = "Please add a variable of interest.")
  summaries <- lapply(1:length(donut_models),
                      function(x) summary(donut_models[[x]]))
  dep_var = summaries[[1]]$call$formula[[2]]
  assert_that(summaries[[1]]$call$formula == summaries[[2]]$call$formula,
              msg = "Ensure that the formula is the same in each model.")

  p_value <- lapply(1:length(summaries),
                    function (x) {
                      tibble(name = summaries[[x]][["coefficients"]] |> rownames(),
                             inner = summaries[[x]][["radius"]][["inner"]],
                             outer = summaries[[x]][["radius"]][["outer"]],
                             coefficient = summaries[[x]][["coefficients"]][, 1],
                             positive = coefficient >= 0,
                             pval = summaries[[x]][["coefficients"]][, 4],
                             p01 = pval < .01,
                             p05 = pval < .05,
                             p10 = pval < .1,
                             stars = factor(x = ifelse(p01 == TRUE, 3,
                                                       ifelse(p05 == TRUE, 2,
                                                              ifelse(p10 == TRUE, 1, 0))),
                                            levels = 3:0)
                      )
                    }) |>
    do.call(rbind, args = _)

  lim <-  p_value[p_value$name == var,]$coefficient |> max() |> abs()

  plot <-
    p_value |>
    filter(name == var) |>
    ggplot(aes(x = inner, y = outer, col = coefficient, size = stars)) +
    geom_point() +
    scale_color_viridis_c(direction = -1,
                          limits = c(-lim, lim)) +
    labs(x = "Inner radius",
         y = "Outer radius",
         caption = "Inner radius: Treated population lives within ... km of landfill. Outer radius: Population lives within ... km of landfill.",
         title =  "Significance varies with inner and outer radius.",
         subtitle = paste("Variable of interest:", var,
                          ", dependent variable:"),
         col = "Coefficient size",
         size = "Significance level") +
    scale_size_manual(breaks = 3:0,
                      labels = c( "*** p < 0.01", "** p < 0.05", "*  p < 0.1", "p >= 0.1"),
                      values = (4:1)*2,
                      drop = FALSE)

  return(plot)
}
