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
#' @param sort_variables Should the variables be sorted so that "dist" is at the top? Logical.
#' Currently, this is supported for all `se` options but "basic".
#' @param output The output of modelsummary, e.g. `kableExtra` and `modelsummary_list`.
#' Note that `modelsummary_list` gets additional list elements so the input information
#' does not get lost.
#' @param format_numbers Logical. Should numbers be formatted so that the thousands are separated by a comma?
#' @param ... Optional arguments for histogram and modelsummary
#'
#' @import modelsummary
#' @importFrom assertthat assert_that
#' @importFrom labelled var_label
#' @importFrom kableExtra footnote_marker_number
#' @importFrom kableExtra footnote
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
#' donut_summary(donut_list = models, r_inner = 3, height = 200)
#' library(modelsummary)
#' model_not_donut <- feols(age ~ male | id, data = donut_data, vcov = "cluster")
#' modelsummary(model_not_donut)
#' model_w_bootstrap <- donut_models(inner = 2:4, outer = c(10, 20), ds = donut_data,
#' dep_var = "wealth_index", indep_vars = "age", fe = "id", bootstrap = TRUE)
#' donut_summary(model_w_bootstrap, r_inner = 4)
#' models_list <- list(model_w_bootstrap, models) |> unlist(recursive = FALSE)
#' donut_summary(models_list, r_inner = 3)
#' models_basic <- donut_models(inner = 2:4, outer = c(10, 20), ds = donut_data,
#' dep_var = "wealth_index", indep_vars = "age", fe = "id", se = "basic")
#' donut_summary(models_basic, sort_variables = TRUE)

donut_summary <- function(donut_list,
                          filter_80 = TRUE,
                          title = NULL,
                          hist = TRUE,
                          r_inner = NULL,
                          sort_variables = TRUE,
                          output,
                          format_numbers = TRUE,
                          ...) {
  # assertions
  assert_that(inherits(donut_list[[1]], "donut_model"),
              msg = "Please choose a donut_list object.")
  assert_that(ifelse(missing(r_inner),
                     TRUE,
                     (is.numeric(r_inner) & r_inner > 0 & r_inner <= 20)),
              msg = "Please ensure that your inner radius is part of the
              donut_list regressions.")


  # filter data
  if (filter_80 == TRUE) {
    over_80 <- extract_info(donut_list)
    donut_list <- donut_list[which(over_80$perc_treated <= .8)]
  }

  names <- extract_info(donut_list)

  if (is.null(r_inner) == TRUE) {
    r_inner <- names$inner |> max()
  }
  donut_list <- donut_list[which(names$inner == r_inner)]

  # title
  if (is.null(title)) {title <- "Regression results"}
  title <- paste(title,
                 paste0("(", r_inner, "km inner radius)"))

  names(donut_list) <- paste0("(", (1:length(donut_list)), ")")

  # sort vars
  if (sort_variables == TRUE & donut_list[[1]][["standard_error"]] != "basic") {
    vars <- sorted_vars(donut_list)
  } else {
    vars <- NULL
  }

  if (format_numbers == TRUE) {
    gof_tidy <- format_numbers()
  } else {gof_tidy <- NULL}

  # add info on clusters and treated
  rows <- info_rows(donut_list, hist = FALSE)
  if ("Clusters" %in% rows$term) {
    footnote_numbers <-
      c(
        "Numbers in brackets refer to the treated percentage",
        "Numbers in brackets refer to the number of treated clusters"
      )
  } else {
    footnote_numbers <-
      c("Numbers in brackets refer to the treated percentage")
  }
  if (donut_list[[1]][["standard_error"]] == "cluster_bs") {
    footnote_bs <-
      "P-values for the treatment status are bootstrapped, so standard errors are not applicable."
  } else {
    footnote_bs <- NULL
  }

  # create table
  if (ifelse(missing(output),
             TRUE,
             output != "modelsummary_list")) {

    modelsummary(
      donut_list,
      stars = TRUE,
      gof_omit = "IC|RMSE|Adj|Within|FE|Std",
      gof_map = gof_tidy,
      add_rows = info_rows(donut_list, hist, footnote = TRUE, ...),
      title = title,
      coef_map = vars,
      ...
    ) |>
      footnote(general = paste("Numbers in parentheses refer to standard errors.",
                           footnote_bs),
               number = footnote_numbers)
  } else {
    make_modelsummary_list(donut_list = donut_list,
                           output = output,
                           hist = hist,
                           title = title,
                           vars = vars,
                           footnote_numbers = footnote_numbers,
                           footnote_bs = footnote_bs,
                           ...)
  }
}
