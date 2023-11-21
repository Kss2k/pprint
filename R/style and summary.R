
#' pretty print for dataframes
#'
#' @param x a dataframe to be printed in pprint style
#' @param primary primary colour this is automatically chosen, but can be overwritten
#' @param secondary secondary colour this is automatically chosen, but can be overwritten
#' @param plaintext colour for plaintext this is automatically chosen, but can be overwritten
#' @param style chose style to print in, currently only "default" is supported
#' @param rows maximum number of rows to print
#'
#'
#' @return
#' @export
#'
#' @examples ## regression preg(
#' lm-object,
#' primary = theme_colors(primary = T),
#' secondary = theme_colors(secondary = T),
#' plaintext = theme_colors(fg = T),
#' std_beta = FALSE,
#' ...)
#'
pprint <- function(x, ...) {
  cPrint(x, ...)
}



#' Summary for dataframes USE psummary() instead
#'
#' @param x dataframe, matrix or tibble object
#' @param primary primary colour this is automatically chosen, but can be overwritten
#' @param secondary secondary colour this is automatically chosen, but can be overwritten
#' @param plaintext colour for plaintext this is automatically chosen, but can be overwritten
#' @param style chose style to print in, currently only "default" is supported
#' @param rows maximum number of rows to print
#' @param ... arguments passed to functions within the function
#'
#' @return
#' @export
#'
#' @examples
psummary.data.frame <- function(x,
                       haven_labelled = FALSE,
                       rows = nrow(x),
                       ...){

  if (haven_labelled == TRUE) {
    x <- haven::zap_labels(x)
  }


  table_x <- data.frame(Variables = as.plainTxt(colnames(x)),
                        Type = purrr::map_chr(x, class),
                        Obs = apply(x, 2, get_N),
                        Missing = purrr::map_dbl(x, count_na),
                        Unique = purrr::map_dbl(x, count_unique),
                        Mean = round(purrr::map_dbl(x, getmean), 3),
                        SD = round(purrr::map_dbl(x, get_sd), 3),
                        Min = round(purrr::map_dbl(x, getmin), 3),
                        Max = round(purrr::map_dbl(x, getmax), 3)) |>
    suppressWarnings()

  cPrint(table_x, maxRow = rows)
}

# Overall Summary

#' Summary for different types of objects
#'
#' @param object a dataframe/tibble/matrix, lm-object, psych fa/principal object, or GLM-object family = binomial(link = "logit")
#' @param ... arguments passed on to the summary functions for each object, respectively: pdataframe, preg, pfactor and plogit
#'
#' @return
#' @export
#'
#' @examples
psummary <- function(object, ...) {
  UseMethod("psummary")
}
