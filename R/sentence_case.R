#' Helper function for donut_summary
#'
#' Turns character vectors to sentence case, and retains the names.
#'
#' @keywords internal
#' @export

sentence_case <- function(x) {
  x_labels <- names(x)
  new_x <- paste0(substr(x, 1, 1) |> toupper(),
                  substr(x, 2, nchar(x)) |> tolower())

  # title case for Global Human Footprint
  new_x <- ifelse(tolower(new_x) == "global human footprint",
                  "Global Human Footprint",
                  new_x)
  names(new_x) <- ifelse(tolower(x_labels) == "global human footprint",
                         "Global Human Footprint",
                         x_labels)
  return(new_x)
}
