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
#' @param p_value Alternative to `donut_models`: already ready p-value data set.
#' @param dep_var If `p_value` param is chosen: dependent variable. Character.
#' @param se If `p_value` is chosen: How standard errors are corrected. Character.
#' @param ... additional parameters to be passed to `ggplot`
#'
#' @import ggplot2
#' @importFrom assertthat assert_that
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom scales muted
#' @import sarahsFunctions
#' @import extrafont
#' @import Rttf2pt1
#'
#' @export
#' @return A ggplot2 plot
#'
#' @examples
#' data(donut_data)
#' models <-
#' donut_models(inner = 2:4, outer = c(10, 15, 20), ds = donut_data,
#' dep_var = "wealth_index", indep_vars = "age", fe = "id")
#' plot <- plot_significance(models, var = "dist")
#' plot
#' plot$data |> plot_significance(p_value = _, var = "dist", dep_var = "wealth_index",
#' se = "Clustered standard errors.")


plot_significance <- function(donut_models,
                              var,
                              filter_80 = TRUE,
                              p_value,
                              dep_var,
                              se = NULL,
                              ...) {
  assert_that(ifelse(missing(donut_models),
                     TRUE,
                     inherits(donut_models, "donut_list")),
              msg = "Insert a list of donut_models.")
  assert_that(ifelse(missing(p_value),
                     TRUE,
                     is.data.frame(p_value)),
              msg = "Please insert a data.frame with the p-values.")
  assert_that(missing(var) == FALSE,
              msg = "Please add a variable of interest.")

  if (missing(p_value)) {


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

  se <- ifelse(sum(p_value$pval != p_value$pval_bs) > 0,
              "Standard errors are cluster-bootstrapped. ",
              ifelse(donut_models[[1]][["standard_error"]] == "conley",
                     "Standard errors are Conley-corrected. ",
              ifelse(donut_models[[1]][["standard_error"]] == "cluster",
                      "Standard errors are clustered by landfill. ",
                     "")))
  }

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
    ),
    ...) +
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
      caption = paste0(
        "Inner radius: Treated population lives within ... km of landfill. \nOuter radius: Population lives within ... km of landfill. ",
        ifelse(
          filter_80 == TRUE,
          "\n Note: Only regressions with \u226480% treatment shown. ",
          ""
        ),
        se
      ),
      # title =  "Significance varies with inner and outer radius.",
      subtitle = paste0("Variable of interest: ", var,
                        ", dependent variable: ", dep_var),
      fill = "Coefficient size",
      size = "Significance level"
    ) +
    scale_size_manual(
      breaks = 3:0,
      labels = c("*** p < 0.01", "** p < 0.05", "*  p < 0.1", "p \u2265 0.1"),
      values = (4:1) * 2,
      drop = FALSE
    ) +
    clean_theme()
  return(plot)

}
