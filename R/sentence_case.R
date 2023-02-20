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
  names(new_x) <- x_labels
  return(new_x)
}
