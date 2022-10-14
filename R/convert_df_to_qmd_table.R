#' Data frame to qmd table
#'
#' Converts a data frame or tibble to a qmd table.
#' This is handy for \code{qmd} Output.
#' Note: if you want to use this, you need to specify the following:
#' \code{#| eval: asis} in the R chunk.
#'
#' @param df The data frame that should be converted
#' @param column_names The column names that should be printed.
#' Defaults to \code{names(df)}.
#'
#' @importFrom dplyr mutate
#'
#' @export
#' @return A qmd table.
#'
#' @examples
#' convert_to_qmd_table(iris[1:10,])

convert_to_qmd_table <-
  function(df,
           column_names = names(df)[-ncol(df)]){

    df <- df |>
      mutate(paragraph = "\n")

    text <- "\n"
    for (row in 1:nrow(df)) {
      text <- paste0(c(text, df[row, ]))
    }


    paste0(c("", column_names, "\n",
             rep("-----", length(column_names)),
             text),
           collapse = "|") |>
      cat()
  }
