#' Helper function for modelsummary
#'
#' Extracts infos from `donut_list` which is not clustered
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom data.table transpose
#'
#' @keywords internal
#' @export
#'

extract_info_conley_basic <- function(donut_model, ...) {
  rows <- extract_info(donut_model) |>
    mutate(perc_treated = n_treated / n_obs * 100) |>
    mutate(n_treated = paste0(n_treated, " (", round(perc_treated, 2), "%)")) |>
    select(outer, n_treated) |>
    transpose()
  rows <- cbind(names = c("Outer radius (km)",
                          "Num. treated (%)"),
                rows)
  return(rows)
}
