#' Extract distances from donut_analysis model
#'
#' Convenience function to extract distances from donut_analysis model.
#' Input needs to be a (list of) `donut_analysis` models.
#'
#' @param models List of `donut_analysis` models, or single `donut_analysis` model
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#'
#' @export
#' @return A tibble
#'
#' @examples
#' library(plm)
#' library(dplyr)
#' data(Cigar)
#' Cigar <- Cigar |>  mutate(dist_km = rnorm(nrow(Cigar), 20, 10)) |>  filter(dist_km >= 0)
#' cigar_model <-
#' donut_analysis(dist = c(5, 20), ds = Cigar, dep_var = "price", indep_vars = "pop", fe = "state")
#' extract_dist(cigar_model)


extract_dist <- function(models){
  if (is.null(models[["radius"]])) {

    vapply(1:length(models),
           function (x) models[[x]][["radius"]],
           FUN.VALUE = numeric(2)) |>
      as_tibble(rownames = NA) |>
      rownames_to_column() |>
      pivot_longer(!rowname) |>
      pivot_wider(names_from = "rowname")
  }
  else if (is.null(models[["radius"]]) == FALSE){
    models[["radius"]] |>
      as_tibble(rownames = NA) |>
      rownames_to_column() |>
      pivot_longer(!rowname) |>
      pivot_wider(names_from = "rowname")
  }
}
