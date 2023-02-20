#' Helper function for modelsummary
#'
#' Makes modelsummary_list with donut_list elements
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom data.table transpose
#' @importFrom kableExtra spec_hist
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#' @export

make_modelsummary_list <-
  function(donut_list,
           output,
           hist,
           title,
           vars,
           footnote_numbers,
           footnote_bs,
           ...) {
    out <- modelsummary(donut_list,
                        output = output,
                        ...)
    out$gof_omit <- "IC|RMSE|Adj|Within|FE|Std"
    out$rows <- info_rows(donut_list,
                          hist,
                          footnote = FALSE,
                          ...)
    out$title <- title
    out$vars <- vars
    out$footnote_numbers <- footnote_numbers
    out$footnote_bs <- footnote_bs

    return(out)
  }
