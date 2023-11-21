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
psummary.lm <- function(x, std_beta = FALSE, standardizeAll = FALSE,
                 ...) {
  if (standardizeAll == TRUE) {
    formula <- rlang::call_args(x$call)$formula
    model <- lm(formula, scale_df(x$model))
  } else if (standardizeAll == FALSE) {
    model <- x
  }
  # Independent variable
  model[["model"]][1] |> names() -> indep

  # Coefficients
  modelCoefficients <- as.data.frame(summary(model)["coefficients"])

  # adding confint (different location than previously)
  modelCoefficients[5:6] <- confint(model)[1:nrow(modelCoefficients), 1:2]
  # adding colnames
  colnames(modelCoefficients) <- c("Coefficients",
                                    "std.error",
                                    "t-value",
                                    "p-value",
                                    "CI[2.5%]",
                                    "CI[97.5%]")

  # adding std_beta
  if (std_beta == TRUE){
    # Std.coefficients
    formula <- rlang::call_args(x$call)$formula
    modelCoefficients <-
      dplyr::mutate(modelCoefficients,
             std.beta = lm(formula, scale_df(x$model))[["coefficients"]],
             .after = "Coefficients")
  }
  # Rounding (all except p-values)
  modelCoefficients[!grepl("p-value", colnames(modelCoefficients))] <-
    round(modelCoefficients[!grepl("p-value", colnames(modelCoefficients))], digits = 3)
  modelCoefficients[["p-value"]] <- as.pval(modelCoefficients[["p-value"]])

  ###Sum of squares, df and R^2
  y <- model$model[[1]]
  residuals <- model$residuals
  TSS <- (y - mean(y))^2 |>
    sum() |>
    round(2)
  RSS <- residuals^2 |>
    sum() |>
    round(2)
  MSS <- TSS - RSS |>
    sum() |>
    round(2)

  df_model <- summary(model)[["fstatistic"]]
  df_1 <- df_model[["numdf"]]
  df_2 <- df_model[["dendf"]]
  F_model <- round(df_model[["value"]], digits = 2)
  N <- round(length(y), digits = 0)
  F_test <- signif(pf(F_model, df_1, df_2, lower.tail = FALSE), digits = 0)


  sum_of_squares_table <- data.frame(SS = as.numeric(c(MSS, RSS, TSS)),
                                     df = c(df_1, df_2, sum(df_1, df_2)),
                                     M. = as.plainTxt(c(paste0(
                                       "F(", as.character(df_1),
                                       ", ", as.character(df_2), ")"),
                                            "prob > F",
                                            "N")),
                                     M = c(F_model,
                                           F_test,
                                           N),
                                     row.names = c("Model",
                                                   "Residual",
                                                   "Total"),
                                     R = as.plainTxt(c("R-squared",
                                           "adj R-squared",
                                           "MSE")),
                                     R. = round(c(summary(model)[["r.squared"]],
                                            summary(model)[["adj.r.squared"]],
                                            summary(model)[["sigma"]] ), 2))

  cat(" \n \n \n", crayon::bold("Model fit:"), crayon::bold(indep), "\n")
  cPrint(sum_of_squares_table, rowNames = TRUE, ...)
  cat("\n \n",  crayon::bold("Coefficients:"), crayon::bold(indep), "\n")
  cPrint(modelCoefficients, rowNames = TRUE, ...)

}


