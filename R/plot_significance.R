#' Extract distances from donut_analysis model
#'
#' Convenience function to extract distances from donut_analysis model.
#' Input needs to be a (list of) `donut_analysis` models.
#' Consider adding `theme(legend.text.align = 1)` to right-align the p-value legend.
#'
#' @param donut_models List of `donut_analysis` models
#' @param var The dependent variable in all `donut_analysis` models which should be plotted.
#' @param filter_80 Boolean: Should only those regressions be displayed for which
#' the treatment group is >= 80% of the whole population? Defaults to `TRUE`.
#'
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom scales muted
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
#' donut_models(inner = 2:4, outer = c(10, 20), ds = Cigar,
#' dep_var = "dist_km", indep_vars = "pop", fe = "state")
#' plot_significance(cigar_models, var = "pop")


plot_significance <- function(donut_models,
                              var,
                              filter_80 = TRUE) {
  assert_that(inherits(donut_models, "donut_list"),
              msg = "Insert a list of donut_models.")
  assert_that(missing(var) == FALSE,
              msg = "Please add a variable of interest.")

  summaries <-
    lapply(1:length(donut_models), function(x)
      summary(donut_models[[x]]))
  assert_that(ifelse(is.null(summaries[[1]][["formula"]]),
                     (summaries[[1]]$call$fml == summaries[[2]]$call$fml), # fixest
                     (summaries[[1]]$formula == summaries[[2]]$formula)), #plm
              msg = "Ensure that the formula is the same in each model.")

  dep_var <- ifelse(is.null(summaries[[1]][["formula"]]),
                    all.vars(summaries[[1]][["call"]])[[1]],
                    as.character(summaries[[1]][["formula"]][[2]]))

  p_value <- lapply(donut_models,
                    get_p_data,
                    filter_80) |>
    do.call(rbind, args = _)

  lim <-
    p_value[p_value$name == var,]$coefficient |> max() |> abs()
  plot <-
    p_value |>
    filter(name == var) |>
    ggplot(aes(
      x = inner,
      y = outer,
      fill = coefficient,
      size = stars
    )) +
    geom_point(pch = 21,
               col = "grey50") +
    scale_fill_gradient2(
      low = muted("red"),
      mid = "white",
      high = muted("blue"),
      midpoint = 0,
      limits = c(-lim, lim)
    ) +
    labs(
      x = "Inner radius",
      y = "Outer radius",
      caption = paste(
        "Inner radius: Treated population lives within ... km of landfill. \nOuter radius: Population lives within ... km of landfill.",
        ifelse(
          filter_80 == TRUE,
          "\n Note: Only regressions with \u226480% treatment shown.",
          ""
        ),
        ifelse(
          "bootstrap_dist" %in% names(donut_models[[1]]),
          "Standard errors are bootstrapped.",
          ""
        )
      ),
      title =  "Significance varies with inner and outer radius.",
      subtitle = paste0("Variable of interest: ", var,
                        ", dependent variable: ", dep_var),
      col = "Coefficient size",
      size = "Significance level"
    ) +
    scale_size_manual(
      breaks = 3:0,
      labels = c("*** p < 0.01", "** p < 0.05", "*  p < 0.1", "p >= 0.1"),
      values = (4:1) * 2,
      drop = FALSE
    )
  return(plot)

}
