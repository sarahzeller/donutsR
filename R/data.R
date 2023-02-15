#' Randomly generated data
#'
#' A data set with which the `donut_analysis` functions can be used.
#'
#' #' @format ## `donut_data`
#' A data frame with 100,00 rows and 7 columns:
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
# n <- 100000
# donut_data <- data.frame(dist_km = rnorm(n, mean = 25, sd = 10),
#                          wealth_index = sample(1:5, size = n, replace = TRUE),
#                          male = sample(0:1, size = n, replace = TRUE),
#                          age = sample(15:99, size = n, replace = TRUE),
#                          id = sample(1:20, size = n, replace = TRUE),
#                          lat = rnorm(n, mean = 4, sd = .5),
#                          lon = rnorm(n, mean = 6, sd = .5))
#
# saveRDS(donut_data, file = "data/donut_data.rds")
