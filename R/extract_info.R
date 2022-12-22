#' Extract additional info from donut_analysis model
#'
#' Convenience function to extract distances as well as number of treated,
#' observations, original observations, percentage of treated, clusters, and
#' treated clusters from donut_analysis model.
#' Input needs to be a (list of) `donut_analysis` models.
#'
#' @param models List of `donut_analysis` models, or single `donut_analysis` model
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom tibble rownames_to_column
#'
#' @export
#' @return A tibble
#'
#' @examples
#' data(donut_data)
#' model <-
#' donut_analysis(dist = c(5, 20), ds = donut_data, dep_var = "wealth_index",
#' indep_vars = "age", fe = "id", clust = TRUE, bootstrap = FALSE)
#' extract_info(model)


extract_info <- function(models){
  # for list of donut_models
  if (is.null(models[["radius"]])) {
    info <-
      vapply(1:length(models),
             function (x) models[[x]][["radius"]],
             FUN.VALUE = numeric(2)) |>
      as_tibble(rownames = NA) |>
      rownames_to_column() |>
      pivot_longer(!rowname) |>
      pivot_wider(names_from = "rowname") |>
      mutate(n_treated =
               sapply(1:length(models),
                      function(x) models[[x]][["n_treated"]])) |>
      mutate(n_obs = sapply(1:length(models),
                            function(x) models[[x]][["nobs"]])) |>
      mutate(n_obs_origin =
               sapply(1:length(models),
                      function(x) models[[x]][["nobs_origin"]])) |>
      mutate(perc_treated = n_treated/n_obs) |>
      mutate(n_clust = sapply(1:length(models),
                              function (x) models[[x]][["summary_clust"]][["n"]])) |>
      mutate(n_clust_treated =
               sapply(1:length(models),
                      function (x) models[[x]][["summary_clust"]][["n_treated"]]))
  }
  # for just one donut_model
  else if (is.null(models[["radius"]]) == FALSE){
    info <-
      models[["radius"]] |>
      as_tibble(rownames = NA) |>
      rownames_to_column() |>
      pivot_longer(!rowname) |>
      pivot_wider(names_from = "rowname") |>
      mutate(n_treated = models[["n_treated"]]) |>
      mutate(n_obs = models[["nobs"]]) |>
      mutate(n_obs = models[["nobs_origin"]]) |>
      mutate(perc_treated = n_treated/n_obs) |>
      mutate(n_clust = models[["summary_clust"]][["n"]]) |>
      mutate(n_clust_treated = models[["summary_clust"]][["n_treated"]])
  }
  return(info)
}
