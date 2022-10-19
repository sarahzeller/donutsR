#' Data frame to qmd table
#'
#' Converts a data frame or tibble to a qmd table.
#' This is handy for \code{qmd} Output.
#' Note: if you want to use this, you need to specify the following in the R chunk:
#' \code{#| output: asis}
#'
#' @param df The data frame that should be converted
#' @param column_names The column names that should be printed.
#' Defaults to \code{names(df)}.
#' @param align An optional character vector to define how the columns should be aligned.
#' Each element can be either "l" (left), "r" (right), or "c" (center).
#' The position of each element corresponds with the position of each column in \code{column_names}.
#'
#' @importFrom dplyr mutate
#' @import assertthat
#'
#' @export
#' @return A qmd table.
#'
#' @examples
#' convert_to_qmd_table(iris[1:10,])


convert_to_qmd_table <-
  function(df,
           column_names = NA,
           align = NA){

    if (missing(column_names)) {column_names <- names(df)}

    assert_that(is.character(align) | missing(align))
    if(!missing(align)) {assert_that(length(column_names) == length(align),
                                     msg = "Please adjust the length of align to the number of columns.")}

    df <- df |>
      mutate(paragraph = "\n")

    text <- "\n"
    for (row in 1:nrow(df)) {
      text <- paste0(c(text, df[row, ]))
    }

    if (all(align %in% c("c", "r", "l"))) {
      paste0(c("", column_names, "\n",
               align_columns(align),
               text),
             collapse = "|") |>
        cat()

    } else {
      paste0(c("", column_names, "\n",
               rep("-----", length(column_names)),
               text),
             collapse = "|") |>
        cat()
    }

  }
