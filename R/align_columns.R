#' Alignment to qmd table syntax
#'
#' Takes an alignment and outputs the qmd table syntax.
#' Is input into the \code{convert_df_to_qmd_table} function.
#'
#' @param align A string vector with elements "c" (center), "r" (right), or "l" (left).
#'
#' @importFrom stringr str_replace
#' @importFrom assertthat assert_that
#'
#' @return A character vector

align_columns <- function(align) {
  assert_that(is.character(align))
  assert_that(sum(!align %in% c("c", "l", "r")) == 0,
              msg = "Please enter 'c', 'l' or 'r'.")
  aligned_columns <- align |>
    str_replace("l", ":-----") |>
    str_replace("r", "-----:") |>
    str_replace("c", ":-----:")
  return(aligned_columns)
}
