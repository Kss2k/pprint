### Style and summary ####
#### pretty_style ####

pstyle <- function(x,
                   primary = theme_colors(primary = T),
                   secondary = theme_colors(secondary = T),
                   plaintext = theme_colors(fg = T),
                   style = "default",
                   ...){

  if (style == "default") {
    x <- as.data.frame(x)
    colorDF::as.colorDF(x, theme = "wb") -> x

    colorDF::df_style(x) <- list(
      col.names  = list(fg=plaintext, decoration= "underline", align="center"),
      row.names = list(fg = plaintext, decoration = F, align = "right"),
      digits = 3,
      fg_na = secondary,
      interleave = NULL,
      colorDF::col_type(x, cols = "Summary") <-("summary"),
      colorDF::col_type(x, cols = "Class") <- ("class"),
      type.styles = list(
        integer    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
        character  = list(align="right"),
        numeric    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
        logical    = list(fg_true="blue", fg_false=secondary, align="right"),
        factor     = list(fg="blue", is.numeric=FALSE, align="right"),
        identifier = list(decoration="bold", align="right"),
        match      = list(fg="purple", fg_match=secondary),
        pval       = list(fg_sign=secondary, fg=primary, align = "right", sign.thr=0.05, is.pval=F),
        default    = list(align="left"),
        summary  = list(align = "center", fg = primary),
        class = list(align = "right", fg = secondary))

    )
    return(x)
  }
}


#### prettyDF_summary ####

pdataframe_old <- function(x, primary = theme_colors(primary = T), secondary = theme_colors(secondary = T), plaintext = theme_colors(fg = T), style = "default", rows = nrow(x)){
  x <- as.data.frame(x)
  means <- purrr::map_vec(x, getmean, na.ignore = T)
  min_x <- purrr::map_vec(x, getmin)
  max_x <- purrr::map_vec(x, getmax)


  sd_x = purrr::map_vec(x, get_sd, na.ignore = T)
  x |>
    colorDF::summary_colorDF() |>
    dplyr::select(-Summary) |>
    dplyr::mutate(Mean = means
    ) -> table_x
  table_x <- dplyr::mutate(table_x, SD = as.numeric(sd_x))
  table_x <- dplyr::mutate(table_x, Min = as.numeric(min_x))
  table_x <- dplyr::mutate(table_x, Max = as.numeric(max_x))
  table_x[5:8] <- round(table_x[5:8], digits = 2)


  suppressWarnings(pprint(table_x, primary = primary, secondary = secondary, plaintext = plaintext, style = style, rows = rows))

}



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
pprint <- function(x,
                   primary = theme_colors(primary = T),
                   secondary = theme_colors(secondary = T),
                   plaintext = theme_colors(fg = T),
                   style = "default",
                   rows = 20){

  if (style == "default") {
    x <- as.data.frame(x)
    colorDF::as.colorDF(x, theme = "wb") -> x

    colorDF::df_style(x) <- list(
      col.names  = list(fg=plaintext, decoration= "underline", align="center"),
      row.names = list(fg = plaintext, decoration = F, align = "right"),
      digits = 3,
      fg_na = secondary,
      interleave = NULL,
      colorDF::col_type(x, cols = "Summary") <- "summary",
      colorDF::col_type(x, cols = "Class") <- "class",
      colorDF::col_type(x, cols = "Col") <- "col",
      colorDF::col_type(x, cols = c("Variable",
                                    "Parameter")) <- "var",
      type.styles = list(
        integer    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
        character  = list(align="right", fg = secondary),
        numeric    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
        logical    = list(fg_true="blue", fg_false=secondary, align="right"),
        factor     = list(fg="blue", is.numeric=FALSE, align="right"),
        identifier = list(decoration="bold", align="right"),
        match      = list(fg="purple", fg_match=secondary),
        pval       = list(fg_sign=secondary, fg=primary, align = "right", sign.thr=0.05, is.pval=F),
        default    = list(align="left"),
        summary  = list(align = "center", fg = primary),
        class = list(align = "right", fg = secondary),
        col = list(align = "left", fg = plaintext),
        var = list(align = "left", fg = plaintext)

        )

    )
    invisible(colorDF::print_colorDF(x, n = rows))
  }
}

get_N <- function(x) {
  complete.cases(x) |> sum()
}
count_na <- function(x) {
  is.na(x) |> sum()
}

count_unique <- function(x) {
  unique(x) |> length()
}



#' Summary for dataframes USE psummary instead
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
pdataframe <- function(x,
                       primary = theme_colors(primary = T),
                       secondary = theme_colors(secondary = T),
                       plaintext = theme_colors(fg = T),
                       style = "default", rows = nrow(x),
                       ...){




  table_x <- data.frame(Variables = colnames(x),
                        Type = purrr::map_chr(x, class),
                        Obs = apply(x, 2, get_N),
                        Missing = purrr::map_dbl(x, count_na),
                        Unique = purrr::map_dbl(x, count_unique),
                        Mean = purrr::map_dbl(x, mean, na.rm = T),
                        SD = apply(x, 2, sd, na.rm = T),
                        Min = apply(x, 2, min, na.rm = T),
                        Max = apply(x, 2, max, na.rm = T)) |>
    suppressWarnings()

  colorDF::as.colorDF(table_x, theme = "wb") -> table_x

  colorDF::df_style(table_x) <- list(
    col.names  = list(fg=plaintext, decoration= "underline", align="center"),
    row.names = list(fg = plaintext, decoration = F, align = "right"),
    digits = 3,
    fg_na = secondary,
    interleave = NULL,
    colorDF::col_type(table_x, cols = "Summary") <- "summary",
    colorDF::col_type(table_x, cols = "Class") <- "class",
    colorDF::col_type(table_x, cols = "Col") <- "col",
    colorDF::col_type(table_x, cols = "Variables") <- "var",
    type.styles = list(
      integer    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      character  = list(align="right", fg = secondary),
      numeric    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      logical    = list(fg_true="blue", fg_false=secondary, align="right"),
      factor     = list(fg="blue", is.numeric=FALSE, align="right"),
      identifier = list(decoration="bold", align="right"),
      match      = list(fg="purple", fg_match=secondary),
      pval       = list(fg_sign=secondary, fg=primary, align = "right", sign.thr=0.05, is.pval=F),
      default    = list(align="left"),
      summary  = list(align = "center", fg = primary),
      class = list(align = "right", fg = secondary),
      col = list(align = "left", fg = plaintext),
      var = list(align = "left", fg = plaintext)

    )

  )
  invisible(colorDF::print_colorDF(table_x, n = rows, row.names = FALSE))
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
  class_object <- class(object)


  if (class_object[1] == "lm") {
    preg(object, ...)
  }

  else if (class_object[1] == "psych") {
    if (class_object[2] == "fa" |
        class_object[2] == "principal") {
      pfactor(object, ...)
    }
  }

  else if (is.data.frame(object) |
           tibble::is_tibble(object) |
           is.matrix(object)) {
    pdataframe(object, ...)
  }
  else if (class_object[1] == "glm") {
    glm_type <- family(object)
    if (glm_type[[1]] == "binomial" |
        glm_type[[2]] == "logit")
      plogit(object, ...)

    else cat("This family of GLM is not supported yet")
  }


}
#function adding colors to parameters function from parameters package
pparameters <- function(x){
  x |> parameters::parameters() |> pprint()
}
