#' Perform a Donut FE analysis
#'
#' Performs a single FE-models with a given data set.
#' The independent variable and the population can be adjusted.
#'
#' @param dist A vector with two distances: the inner and the outer.
#' The inner distance refers to the maximum distance for treatment.
#' The outer distance refers to the overall population: it is the cut-off value.
#' @param ds The dataset.
#' @param dep_var The dependent variable in the regression
#' @param indep_vars a character vector of independent variables. Must be part of `ds`.
#' @param fe a character vector of Fixed Effects variables. Must be part of `ds`.
#' @param dist_var A character referring to the distance parameter.
#' Defaults to `dist_km`.
#' @param se is a character, defaulting to `cluster`.
#' It shows how to adjust standard errors: By clustering them by the `fe` param
#' (`cluster`, fixest::feols),
#' no adjustment, (`plm`, plm::plm), or with `conley` (`conley` param in fixest::feols).
#' @param bootstrap a boolean: should the standard errors for `dist` be bootstrapped?
#' @param B the number of bootstraps
#' @param lat Latitude; needed for `conley`-standard errors
#' @param lon Longitude; needed for `conley`-standard errors
#' @param ... Additional arguments
#'
#' @importFrom plm plm
#' @import dplyr
#' @import dtplyr
#' @importFrom stats reformulate
#' @importFrom stats rnorm
#' @importFrom assertthat assert_that
#' @importFrom utils globalVariables
#' @import fixest
#' @importFrom tidyr drop_na
#' @import fwildclusterboot
#' @importFrom labelled labelled
#' @importFrom haven zap_labels
#'
#' @export
#'
#' @return A single `donut_model` object with class `plm` (clust = FALSE) or
#' class `fixest`. The `donut_model` has additional list element which contain
#' the inner and outer radii and additional info.
#' It can be plotted with the `plot_significance` function.
#'
#' @examples
#' library(fixest)
#' library(dplyr)
#' data(Cigar)
#' set.seed(123)
#' Cigar1 <- Cigar |>  mutate(dist_km = stats::rnorm(nrow(Cigar), 20, 10)) |>  filter(dist_km >= 0)
#' cigar_model1 <-
#' donut_analysis(dist = c(5, 20), ds = Cigar1, dep_var = "price", indep_vars = "pop", fe = "state")
#' Cigar2 <- Cigar |>  mutate(dist_km1 = rnorm(nrow(Cigar), 20, 10)) |>  filter(dist_km1 >= 0)
#' cigar_model2 <-
#'   donut_analysis(dist = c(5, 20),
#'                  ds = Cigar2,
#'                  dep_var = "price",
#'                  indep_vars = "pop",
#'                  fe = "state",
#'                  dist_var = "dist_km1")


donut_analysis <- function(dist,
                           ds,
                           dep_var,
                           indep_vars,
                           fe = "id",
                           dist_var = "dist_km",
                           se = "cluster",
                           bootstrap = FALSE,
                           B = 9999,
                           lat = "lat",
                           lon = "lon",
                           ...) {
  assert_that(length(dist) == 2 & is.numeric(dist),
              msg = "Please enter two numeric values for the distances.")
  assert_that(is.data.frame(ds),
              msg = "Please enter a data.frame for ds.")
  assert_that(sum(c(dep_var, indep_vars, fe) %in% names(ds) == FALSE) == 0,
              msg = "Please ensure dep_var, indep_vars, and fe are columns in ds.")
  assert_that(length(dep_var) == 1 & is.character(dep_var),
              msg = "dep_var needs to be a single character string.")
  assert_that(is.character(dist_var) & dist_var %in% names(ds),
              msg = "Please ensure the dist_var is in the ds.")
  assert_that((se %in% c("basic", "conley") & bootstrap == FALSE) |
                se == "cluster",
              msg = "Bootstrapping not possible without clustering. Please set se to 'cluster'.")
  assert_that(se %in% c("basic", "conley", "cluster"),
              msg = "se must be one of 'basic', 'conley', 'cluster'")
  assert_that(se != "conley" | (se == "conley" & lat %in% names(ds) & lon %in% names(ds)),
              msg = "Please ensure you have a lat and lon variable in your dataset.
              This is needed for Conley-correcting the standard errors.")

  if ("geometry" %in% names(ds)) {ds <- ds |> select(-geometry)}

  inner <- dist[1]
  outer <- dist[2]

  if (se != "conley") {
    lat <- NULL
    lon <- NULL
  }

  formula <- reformulate(c(indep_vars, "dist"), dep_var)

  if (dist_var == "dist_km"){
    data <- ds |>
      filter(dist_km <= outer) |>
      mutate(dist = labelled(((dist_km <= inner) |> as.integer()),
                             label = "lives close to landfill")) |>
      select(c(all.vars(formula),
               all_of(fe),
               all_of(lat),
               all_of(lon))) |>
      collect() |>
      drop_na() |>
      zap_labels()

  } else {
    data <- ds[ds[[dist_var]] <= outer,]
    data <- data |>
      mutate(dist = labelled(((dist_var <= inner) |> as.integer()),
                             label = "lives close to landfill")) |>
      select(c(all.vars(formula),
               all_of(fe),
               all_of(lon),
               all_of(lon))) |>
      collect() |>
      drop_na() |>
      zap_labels()
  }

  if (se == "basic") {
    model_fe <- do.call('plm',
                        list(formula,
                             data = quote(data),
                             index = fe,
                             model = "within"))
    clust <- NULL
    summary_clust <- NULL

  } else if (se == "cluster") {
    model_fe <- do.call("feols", list(formula(paste(c(formula, fe), collapse = "|")),
                                      data = data,
                                      "cluster"))
    clust <- data |>
      group_by(get(fe)) |>
      summarize(clust_size = n(), treated_clust = !sum(dist) == 0)
    names(clust)[1] <- fe
    summary_clust <- c(n = nrow(clust),
                       n_treated = sum(clust$treated_clust))
  } else if (se == "conley") {
    model_fe <- do.call("feols", list(formula(paste(c(formula, fe), collapse = "|")),
                                      data = data,
                                      vcov_conley(lat = lat,
                                                  lon = lon,
                                                  cutoff = outer,
                                                  distance = "spherical")))
    clust <- NULL
    summary_clust <- NULL
  }

  if (bootstrap == TRUE & se == "cluster") {

    bootstrap_se <- boottest(
      model_fe,
      param = "dist",
      B = B,
      clustid = fe,
      # # use WCU
      # impose_null = FALSE,
      seed = 123,
      fe = fe,
      ...
    )
    model_fe[["bootstrap_dist"]] <- bootstrap_se
  }

  #add additional info
  model_fe[["radius"]] <- c(inner = inner, outer = outer)
  model_fe[["n_treated"]] <- data |> filter(dist == TRUE) |> nrow()
  model_fe[["clust"]] <- clust
  model_fe[["summary_clust"]] <- summary_clust
  model_fe[["standard_error"]] <- paste0(se, ifelse(bootstrap, "_bs", ""))
  class(model_fe) <- c(class(model_fe), "donut_model")
  return(model_fe)
}
