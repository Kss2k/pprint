#' Summary for linear regressions
#'
#' @param x lm-object
#' @param primary primary colour this is automatically chosen, but can be overwritten
#' @param secondary secondary colour this is automatically chosen, but can be overwritten
#' @param plaintext colour for plaintext this is automatically chosen, but can be overwritten
#' @param std_beta if TRUE std.beta for continuous variables are included, or cohens'd for factor variables
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
preg <- function(x, primary = theme_colors(primary = T),
                 secondary = theme_colors(secondary = T),
                 plaintext = theme_colors(fg = T),
                 std_beta = FALSE,
                 ...) {

  # Independent variable
  x[["model"]][1] |> names() -> indep

  # Coefficients
  model_coefficients <- as.data.frame(summary(x)["coefficients"])

  # adding std_beta
  if (std_beta == TRUE){
    # Coefficients
    model_coefficients <- as.data.frame(summary(x)["coefficients"])

    # Std.coefficients
    # model_coefficients[5] <- lm(data.frame(scale(x$model)))[["coefficients"]]
    model_coefficients[5] <- lm(data.frame(scale_df(x$model)))[["coefficients"]]

    # reordering
    model_coefficients <- model_coefficients[, c(1, 5, 2, 3, 4)]

    # adding CI
    model_coefficients[6:7] <- confint(x)[1:nrow(model_coefficients), 1:2]

    # adding colnames
    colnames(model_coefficients) <- c(" Coefficients",
                                      "std.beta",
                                      " std.error",
                                      " t-value",
                                      " p-value",
                                      " CI[2.5%]",
                                      " CI[97.5%]")

    # Rounding (all except p-values)
    model_coefficients[,-5] <- round(model_coefficients[,-5], digits = 3)
  }

  # without std_beta
  else {
    # adding confint (different location than previously)
    model_coefficients[5:6] <- confint(x)[1:nrow(model_coefficients), 1:2]
    # adding colnames
    colnames(model_coefficients) <- c(" Coefficients",
                                      " std.error",
                                      " t-value",
                                      " p-value",
                                      " CI[2.5%]",
                                      " CI[97.5%]")
    # Rounding (all except p-values)
    model_coefficients[,-4] <- round(model_coefficients[,-4], digits = 3)
  }




  # adding color
  colorDF::as.colorDF(model_coefficients, theme = "wb") -> model_coefficients
  colorDF::df_style(model_coefficients) <- list(
    col.names  = list(fg = plaintext, decoration= "underline", align="center"),
    row.names = list(fg = plaintext, decoration = F, align = "right"),
    digits = 3,
    interleave = NULL,
    colorDF::col_type(model_coefficients, cols = " p-value") <-("pval"),
    type.styles = list(
      integer    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      character  = list(align="left"),
      numeric    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign=secondary, fg=primary, align = "right", sign.thr=0.05, is.pval= T),
      default    = list(align="left"))

  )

  ###Sum of squares, df and R^2
  y <- x$model[[1]]
  residuals <- x$residuals
  TSS <- (y - mean(y))^2 |> sum()
  RSS <- residuals^2 |> sum()
  MSS <- TSS - RSS

  df_model <- summary(x)[["fstatistic"]]
  df_1 <- df_model[["numdf"]]
  df_2 <- df_model[["dendf"]]
  F_model <- round(df_model[["value"]], digits = 2)
  N <- round(length(y), digits = 0)
  F_test <- signif(pf(F_model, df_1, df_2, lower.tail = FALSE), digits = 0)


  sum_of_squares_table <- data.frame(SS = as.numeric(c(MSS, RSS, TSS)),
                                     df = c(df_1, df_2, sum(df_1, df_2)),
                                     M. = c(paste0("F(", as.character(df_1), ", ", as.character(df_2), ")"),
                                            "prob > F",
                                            "N"),
                                     M = c(as.character(F_model),
                                           as.character(F_test),
                                           as.character(N)),


                                     row.names = c("Model",
                                                   "Residual",
                                                   "Total"),
                                     R = c("R-squared",
                                           "adj R-squared",
                                           "MSE"),
                                     R. = c(summary(x)[["r.squared"]],
                                            summary(x)[["adj.r.squared"]],
                                            summary(x)[["sigma"]] ))

  #sum_of_squares_table <- round(sum_of_squares_table, digits = 3)


  colorDF::as.colorDF(sum_of_squares_table, theme = "wb") -> sum_of_squares_table

  colorDF::df_style(sum_of_squares_table) <- list(
    col.names  = list(fg=plaintext, align= "right", decoration= "underline", decoration = T),
    row.names = list(fg = plaintext, decoration = F, align = "right"),
    colorDF::col_type(sum_of_squares_table, cols = "M") <-("div"),
    digits = 3,
    autoformat = T,
    interleave = NULL,
    type.styles = list(
      integer    = list(fg=primary, fg_neg=secondary, is.numeric= TRUE, align="center"),
      character  = list(align="right"),
      numeric    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      identifier = list(decoration="bold", align="right", fg = plaintext),
      pval       = list(fg_sign=secondary, fg=primary, align = "right", sign.thr=0.05, is.pval=TRUE),
      default    = list(align="left"),
      div = list(fg = primary, align = "right"))


  )

  cat(" \n \n \n", crayon::bold("Model fit:"), crayon::bold(indep), "\n")
  invisible(print(sum_of_squares_table))
  cat("\n \n",  crayon::bold("Coefficients:"), crayon::bold(indep), "\n")
  invisible(print(model_coefficients))

}
