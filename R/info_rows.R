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
           footnote = FALSE,
           ...) {

    model_names <- names(donut_model)

    if (donut_model[[1]][["standard_error"]] %in% c("cluster", "cluster_bs")) {
      rows <- extract_info_clust(donut_model, hist,  ...)
    } else if (donut_model[[1]][["standard_error"]] %in% c("basic", "conley")) {
      rows <- extract_info_conley_basic(donut_model)
    }
    names(rows) <- c("term", model_names)

    if (footnote == TRUE) {
      # add footnote marker
      rows$term[rows$term == "Treated"] <- paste0("Treated",
                                                  footnote_marker_number(1))
      rows$term[rows$term == "Clusters"] <- paste0("Clusters",
                                                   footnote_marker_number(2))
    }

    return(rows)
  }
