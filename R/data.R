#' Randomly generated data
#'
#' A data set with which the `donut_analysis` functions can be used.
#'
#' #' @format ## `donut_data`
#' A data frame with 1000 rows and 7 columns:
#' \describe{
#'   \item{dist_km}{Distance to nearest point of interest}
#'   \item{wealth_index}{wealth index for observation}
#'   \item{male, age}{gender and age of household head}
#'   \item{id}{ID number for point of interest}
#'   \item{lat, lon}{position of observation}
#' }
"donut_data"

# library(stats)
# set.seed(123)
#
# donut_data <- data.frame(dist_km = rnorm(1000, mean = 25, sd = 10),
#                          wealth_index = sample(1:5, size = 100, replace = TRUE),
#                          male = sample(0:1, size = 1000, replace = TRUE),
#                          age = sample(15:99, size = 1000, replace = TRUE),
#                          id = sample(1:20, size = 1000, replace = TRUE),
#                          lat = rnorm(1000, mean = 4, sd = .5),
#                          lon = rnorm(1000, mean = 6, sd = .5))
#
# saveRDS(donut_data, file = "data/donut_data.rds")
