#' Helper function for modelsummary
#'
#' Extracts infos from `donut_list` which is not clustered
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom data.table transpose
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#' @export
#'

extract_info_conley_basic <- function(donut_model, ...) {
  rows <- extract_info(donut_model) |>
    mutate(perc_treated = n_treated / n_obs * 100) |>
    mutate_at(vars(starts_with("n_")), ~format(., big.mark = ",")) |>
    mutate(perc_treated = round(perc_treated, 2)) |>
    mutate(perc_treated = paste0("[", perc_treated, "]")) |>
    select(outer, n_treated, perc_treated) |>
    transpose()
  rows <- cbind(names = c("Outer radius in km",
                          "Treated",
                          " "),
                rows)
  return(rows)
}
