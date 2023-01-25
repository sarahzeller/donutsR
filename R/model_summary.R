#' Model summary table for donut_list
#'
#' Convenience function to make a modelsummary table.
#' Input needs to be a list of `donut_analysis` models, i.e. a `donut_list`.
#'
#' @param donut_list The donut_list
#' @param r_inner The inner radius; optional. A (vector of) integers which are
#' the inner radius of a `donut_model`. Must be in the `donut_list`.
#' @param filter_80 Should only regressions where treated are <=80% be shown?
#' Defaults to true.
#' @param title Optional: a title for the summary table.
#' Defaults to "Regression results (r_inner km radius)".
#' @param hist If `donut_list` has clustered standard errors: Should a histogram of the
#' clusters be shown?
#' @param ... Optional arguments for histogram and modelsummary
#'
#' @import modelsummary
#' @importFrom assertthat assert_that
#'
#' @export
#' @return A modelsummary object, usually a `kableExtra` table.
#'
#' @examples
#' data(donut_data)
#' models <-
#' donut_models(inner = 2:4, outer = c(10, 20), ds = donut_data,
#' dep_var = "wealth_index", indep_vars = "age", fe = "id")
#' table <-
#' model_summary(donut_list = models, r_inner = 3, height = 200)
#' library(modelsummary)
#' model_not_donut <- feols(age ~ male | id, data = donut_data, vcov = "cluster")
#' modelsummary(model_not_donut)
#' model_w_bootstrap <- donut_models(inner = 2:4, outer = c(10, 20), ds = donut_data,
#' dep_var = "wealth_index", indep_vars = "age", fe = "id", bootstrap = TRUE)
#' model_summary(model_w_bootstrap, r_inner = 4)
#' models_list <- list(model_w_bootstrap, models) |> unlist(recursive = FALSE)
#' model_summary(models_list, r_inner = 3)

model_summary <- function(donut_list,
                          r_inner = NULL,
                          filter_80 = TRUE,
                          title = NULL,
                          hist = TRUE,
                          ...) {
  assert_that(inherits(donut_list[[1]], "donut_model"),
              msg = "Please choose a donut_list object.")
  assert_that(missing(r_inner) | (is.numeric(r_inner) & r_inner > 0 & r_inner <= 20),
              msg = "Please ensure that your inner radius is part of the
              donut_list regressions.")

  if (filter_80 == TRUE) {
    over_80 <- extract_info(donut_list)
    donut_list <- donut_list[which(over_80$perc_treated <= .8)]
  }

  names <- extract_info(donut_list)

  if (is.null(r_inner) == TRUE) {
    # automatically set r_inner to largest possible radius
    r_inner <- names$inner |> max()
  }
  donut_list <- donut_list[which(names$inner == r_inner)]

  # set title
  if (is.null(title)) {
    title <- "Regression results"
  }

  title <- paste(title,
                 paste0("(",
                        r_inner,
                        "km inner radius)"))

  names(donut_list) <- paste0("(", (1:length(donut_list)), ")")

  modelsummary(
    donut_list,
    stars = TRUE,
    coef_rename = TRUE,
    gof_omit = "IC|RMSE|Adj|Within",
    add_rows = info_rows(donut_list,
                         hist,
                         ...),
    notes = ifelse(donut_list[[1]][["standard_error"]] == "cluster_bs",
                   "The dist p-values are bootstrapped, so std. errors are not applicable.",
                   ""),
    title = title,
    ...
  )
}
