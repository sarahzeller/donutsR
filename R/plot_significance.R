#' Extract distances from donut_analysis model
#'
#' Convenience function to extract distances from donut_analysis model.
#' Input needs to be a (list of) `donut_analysis` models.
#'
#' @param donut_models List of `donut_analysis` models
#' @param var The dependent variable in all `donut_analysis` models which should be plotted.
#'
#' @import ggplot2
#' @import assertthat
#' @import dplyr
#' @import tibble
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
                             stars = ifelse(p01 == TRUE, "*** p < 0.01",
                                            ifelse(p05 == TRUE, "**  p < 0.05",
                                                   ifelse(p10 == TRUE,
                                                          "*   p < 0.1",
                                                          "p >= 0.1"))) |>
                               as.factor()
                      )
                    }) |>
    do.call(rbind, args = _)

  plot <-
    p_value |>
    filter(name == var) |>
    ggplot(aes(x = inner, y = outer, col = coefficient, size = stars)) +
    geom_point() +
    scale_color_viridis_c(direction = -1) +
    labs(x = "Inner radius",
         y = "Outer radius",
         caption = "Inner radius: Treated population lives within ... km of landfill. Outer radius: Population lives within ... km of landfill.",
         title =  "Significance varies with inner and outer radius.",
         subtitle = paste("Dependent variable:", var),
         col = "Coefficient size",
         size = "Significance level")
    # custom_theme()

  return(plot)
}
