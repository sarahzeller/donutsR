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
#' @param output The output of modelsummary, e.g. `kableExtra` and `modelsummary_list`.
#' Note that `modelsummary_list` gets additional list elements so the input information
#' does not get lost.
#' @param ... Optional arguments for histogram and modelsummary
#'
#' @import modelsummary
#' @importFrom assertthat assert_that
#' @importFrom labelled var_label
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
                          filter_80 = TRUE,
                          title = NULL,
                          hist = TRUE,
                          r_inner = NULL,
                          sort_variables = TRUE,
                          output,
                          ...) {
  assert_that(inherits(donut_list[[1]], "donut_model"),
              msg = "Please choose a donut_list object.")
  assert_that(ifelse(missing(r_inner),
                     TRUE,
                     (is.numeric(r_inner) & r_inner > 0 & r_inner <= 20)),
              msg = "Please ensure that your inner radius is part of the
              donut_list regressions.")

  if (is.null(title)) {
    title <- "Regression results"
  }

  if (filter_80 == TRUE) {
    over_80 <- extract_info(donut_list)
    donut_list <- donut_list[which(over_80$perc_treated <= .8)]
  }

  names <- extract_info(donut_list)

  if (is.null(r_inner) == TRUE) {
    r_inner <- names$inner |> max()
  }
  donut_list <- donut_list[which(names$inner == r_inner)]

  title <- paste(title,
                 paste0("(",
                        r_inner,
                        "km inner radius)"))

  names(donut_list) <- paste0("(", (1:length(donut_list)), ")")

  if (sort_variables == TRUE) {
    vars <- var_label(donut_list[[1]]$call$data, unlist = TRUE)
    # replace those without labels
    vars[vars == ""] <- names(donut_list[[1]]$call$data)[vars == ""] |>
      gsub("_", " ", x = _)
    # kick those out that aren't in the first formula (-dependent variable)
    vars <- vars[names(vars) %in% all.vars(donut_list[[1]]$fml[-1])] |>
      # turn them to sentence case
      sentence_case()
    # sort them so "dist" is up front
    if ("dist" %in% names(vars)) {
    vars <- c("dist" = vars[["dist"]],
              vars[names(vars) != "dist"])
    }
  } else {
    vars <- NULL
  }

  if (ifelse(missing(output),
             TRUE,
             output != "modelsummary_list")) {
    modelsummary(
      donut_list,
      stars = TRUE,
      gof_omit = "IC|RMSE|Adj|Within|FE|Std",
      add_rows = info_rows(donut_list,
                           hist,
                           ...
                           ),
      notes = ifelse(
        "standard_error" %in% names(donut_list[[1]]),
        ifelse(
          donut_list[[1]][["standard_error"]] == "cluster_bs",
          "P-values are bootstrapped, so std. errors are not applicable.",
          ""
        ),
        ""
      ),
      title = title,
      coef_map = vars,
      ...
    )
  } else {
    out <- modelsummary(
      donut_list,
      ...
    )
    out$gof_omit <- "IC|RMSE|Adj|Within|FE|Std"
    out$rows <- info_rows(donut_list,
                          hist,
                          ...)
    out$title <- title
    out$notes <- ifelse("standard_error" %in% names(donut_list[[1]]),
                        ifelse(
                          donut_list[[1]][["standard_error"]] == "cluster_bs",
                          "The dist p-values are bootstrapped, so std. errors are not applicable.",
                          ""
                        ),
                        ""
    )
    out$vars <- vars
    return(out)
  }
}
