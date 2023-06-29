#' Summary for logistic regression model using GLM
#'
#' @param logistic_reg_model a GLM type object family = binomial(link = "logit)
#' @param primary primary colour this is automatically chosen, but can be overwritten
#' @param secondary secondary colour this is automatically chosen, but can be overwritten
#' @param plaintext colour for plaintext this is automatically chosen, but can be overwritten
#' @param accuracy If = TRUE TRUE/FALSE positives/negatives are included, and specificity, sensitivity and positiv/negative predictive values
#'
#' @return
#' @export
#'
#' @examples
#'
plogit <- function(logistic_reg_model,
                   odds_ratio = FALSE,
                   primary = theme_colors(primary = T),
                   secondary = theme_colors(secondary = T),
                   plaintext = theme_colors(fg = T),
                   accuracy = FALSE) {

  # Independent variable
  logistic_reg_model[["model"]][1] |> names() -> indep

  # Coefficients
  model_coefficients <- as.data.frame(summary(logistic_reg_model)["coefficients"])




  suppressMessages(model_coefficients[5:6] <- confint(logistic_reg_model)[1:nrow(model_coefficients), 1:2])
  # adding colnames
  colnames(model_coefficients) <- c("Coefficients",
                                    "std.error",
                                    "t-value",
                                    "p-value",
                                    "CI[2.5%]",
                                    "CI[97.5%]")

  # Rounding (all except p-values)
  model_coefficients[,-4] <- round(model_coefficients[-4], digits = 3)

  # Adding Odds Ratio Coefficients
  if (odds_ratio == TRUE) {

    model_coefficients <- dplyr::mutate(model_coefficients,
                                 OddsRatio =
                                   exp(Coefficients) |>
                                   as.vector() |>
                                   round(3),
                                 .after = Coefficients
                                 )
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

  # General information
  N = nrow(logistic_reg_model[["model"]])
  log_lik <- lmtest::lrtest(logistic_reg_model)$LogLik[1] |>
    round(3)
  chi_sq <- lmtest::lrtest(logistic_reg_model)$Chisq[2]
  Pr_chi_sq <- lmtest::lrtest(logistic_reg_model)[["Pr(>Chisq)"]][2] |>
    signif(3)
  PseudoRsq <- DescTools::PseudoR2(logistic_reg_model, which = "McFadden")

  summary_table <- data.frame(Values = c(N,
                                         log_lik,
                                         chi_sq,
                                         Pr_chi_sq,
                                         PseudoRsq),
                              row.names = c("Number of Obs   = ",
                                            "Log Likelyhood  = ",
                                            "Chi Square      = ",
                                            "Prob > ChiSq    = ",
                                            "Pseudo R2       = ")

  )
  summary_table$Values <- round(summary_table$Values, 3)
  summary_table$Values <- format(summary_table$Values, scientific = FALSE, digits = 3)


  colorDF::as.colorDF(summary_table, theme = "wb") -> summary_table
  colorDF::df_style(summary_table) <- list(
    col.names  = list(fg = plaintext, decoration= "underline", align="center"),
    row.names = list(fg = plaintext, decoration = F, align = "right"),
    digits = 3,
    interleave = NULL,
    colorDF::col_type(summary_table, cols = "Values") <-("pval"),
    type.styles = list(
      integer    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      character  = list(align="left"),
      numeric    = list(fg=primary, fg_neg=secondary, is.numeric=TRUE, align="right"),
      identifier = list(decoration="bold", align="right"),
      pval       = list(fg_sign=secondary, fg=primary, align = "right", sign.thr=0.05, is.pval= T),
      default    = list(align="left"))

  )
  # cat(" \n \n \n", crayon::bold("Model fit:"), crayon::bold(indep), "\n",
  #     "Number of Obs = ", crayon::N, "\n",
  #     "Chi Square = ", chi_sq, "\n",
  #     "Prob > ChiSq = ", Pr_chi_sq, "\n",
  #     "Log Likelyhood = ", log_lik, "\n",
  #     "Pseudo R2 = ", PseudoRsq)
  cat(" \n \n \n", crayon::bold("Model fit:"), crayon::bold(indep), "\n")
  print(summary_table)
  cat("\n \n",  crayon::bold("Coefficients:"), crayon::bold(indep), "\n")
  print(model_coefficients)

  if(accuracy == TRUE) {
    logit_class(logistic_reg_model)
  }
}



logit_class <- function(logistic_reg_model) {


  # Gathering the dataset used in our model (this way we do not need to remove missing values our selves)
  model_data_with_predictions <- logistic_reg_model[["model"]] |>
    dplyr::mutate(predicted_logit = predict(logistic_reg_model))







  # Optionally we can just use the logit it selv, since 0.5 corresponds to 0 logit
  model_data_with_predictions <- model_data_with_predictions |> dplyr::mutate(
    predicted = dplyr::case_when(predicted_logit <= 0 ~ 0,
                           predicted_logit >= 0 ~ 1)
  )

  # Using logit to create a table with classifications
  classifications <- table(model_data_with_predictions[,1],
        model_data_with_predictions$predicted) |>
    as.data.frame()


  classification_matrix <- classifications$Freq |> matrix(nrow = 2,
                                                          byrow = T) |>
    as.data.frame()

  classification_matrix[3,1:2] <- apply(classification_matrix, 2, sum)
  classification_matrix[1:3,3] <- apply(classification_matrix, 1, sum)

  colnames(classification_matrix) <- c("Predicted Negative",
                                       "Predicted Positive",
                                       "Total")
  rownames(classification_matrix) <- c("Observed Negative",
                                       "Observed Positive",
                                       "Total")


  # classification_matrix <- classifications$Freq |> matrix(nrow = 2,
  #                                byrow = T,
  #                                dimnames = list(c("Observed Negative",
  #                                                  "Observed Positive"),
  #                                                c("Predicted Negative",
  #                                                  "Predicted Positive")
  #                                                )
  #                                )
  #


  # true_negative <- data.frame(Frequency = classifications$Freq,
  #                             row.names = c("True Negatives",
  #                                           "False Positives",
  #                                           "False Negatives",
  #                                           "True Positives")
  # )
  specificity = classification_matrix[1,1]/ (classification_matrix[2,1] + classification_matrix[1,1])
  sensitivity = classification_matrix[2,2]/ (classification_matrix[1,2] + classification_matrix[2,2])
  positive_predictive_val <- classification_matrix[2,2]/ (classification_matrix[2,1] + classification_matrix[2,2])
  negative_predictive_val <- classification_matrix[1,1]/ (classification_matrix[1,2] + classification_matrix[1,1])



  sensitivity_specificity <- data.frame(Percentage = c(sensitivity,
                                                       specificity,
                                                       positive_predictive_val,
                                                       negative_predictive_val),
                                        row.names = c("Sensitivity",
                                                      "Specificity",
                                                      "Positve Predictive Value",
                                                      "Negative Predictive Value"))

  cat("\n",
      "\n",
      crayon::bold("Predicted and Observed Positives and Negatives"),
      "\n")
  pprint(classification_matrix)
  cat("\n",
      "\n", crayon::bold("Sensitivity and Specificity"),
      "\n")
  pprint(sensitivity_specificity)

}


