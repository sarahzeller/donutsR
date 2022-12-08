#' Model summary table for donut_list
#'
#' Convenience function to make a modelsummary table.
#' Input needs to be a list of `donut_analysis` models, i.e. a `donut_list`.
#'
#' @param donut_list The donut_list
#' @param r_inner The inner radius; optional. A (vector of) integers which are
#' the inner radius of a `donut_model`. Must be in the `donut_list`.
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
#' library(fixest)
#' library(dplyr)
#' data(Cigar)
#' Cigar <- Cigar |>
#' mutate(dist_km = stats::rnorm(nrow(Cigar), 20, 10)) |>
#' filter(dist_km >= 0)
#'
#' cigar_models <-
#' donut_models(inner = 2:4, outer = c(10, 20), ds = Cigar,
#' dep_var = "dist_km", indep_vars = "pop", fe = "state")
#' table <-
#' model_summary(donut_list = cigar_models, r_inner = 3, height = 200)
#'

model_summary <- function(donut_list,
                          r_inner = NULL,
                          title = NULL,
                          hist = TRUE,
                          ...) {
  assert_that(inherits(donut_list[[1]], "donut_model"),
              msg = "Please choose a donut_list object.")
  assert_that(is.numeric(r_inner) & r_inner > 0 & r_inner <= 20,
              msg = "Please ensure that your inner radius is part of the
              donut_list regressions.")

  if (is.null(title)) {
    title <- "Regression results"
  }

  if (is.null(r_inner) == FALSE) {
    names <- extract_info(donut_list) |>
      filter(inner == r_inner) |>
      pull(name)
    donut_list <- donut_list[names]

    title <- paste(title,
                   paste0("(",
                          r_inner,
                          "km inner radius)"))
  }

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
