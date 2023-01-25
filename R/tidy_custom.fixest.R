#' Helper function for modelsummary
#'
#' @importFrom fixest coeftable
#' @importFrom labelled var_label
#'
#' @keywords internal
#' @export

tidy_custom.fixest <- function (x, ...)
{
  if (("donut_list" %in% class(x) | "donut_model" %in% class(x)) &
      is.null(x[["bootstrap_dist"]]) == FALSE) {
    pval <- coeftable(x)[, 4] |> round(digits = 4)
    pval[["dist"]] <- ifelse(is.null(x[["bootstrap_dist"]]),
                             pval[["dist"]], x[["bootstrap_dist"]][["p_val"]])
    s <- coeftable(x)[, 2]
    s[["dist"]] <- ifelse(is.null(x[["bootstrap_dist"]]),
                          s[["dist"]], NA_integer_)
    labels <- var_label(x$call$data, unlist = TRUE)
    out <- data.frame(term = labels[names(pval)],
                      p.value = pval,
                      std.error = s)
    return(out)
  }
}

