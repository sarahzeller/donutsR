#' Helper function for modelsummary
#'
#' Extracts infos from `donut_list`
#'
#'
#' @keywords internal
#' @export

info_rows <-
  function(donut_model,
           hist = TRUE,
           ...) {

    model_names <- names(donut_model)

    if (donut_model[[1]][["standard_error"]] %in% c("cluster", "cluster_bs")) {
      rows <- extract_info_clust(donut_model, hist,  ...)
    } else if (donut_model[[1]][["standard_error"]] %in% c("basic", "conley")) {
      rows <- extract_info_conley_basic(donut_model)
    }
    names(rows) <- c("term", model_names)
    return(rows)
  }
