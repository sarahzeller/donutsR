#' Helper function for donut_summary
#'
#' Prettifies variables and their labels, and sorts them so "dist" is up front
#'
#' @keywords internal
#' @export

sorted_vars <- function(donut_list) {
  # get all vars with labels
  vars <- var_label(donut_list[[1]]$call$data,
                    unlist = TRUE)
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
  return(vars)
}
