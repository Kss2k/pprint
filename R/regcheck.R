#Dependencies

#car
#lmtest

#### regcheck ####


regcheck <- function(reg_model, sig_level = 0.050, accurate = F) {
  cat(crayon::bold("           Summary Regression Assumptions\n"))
  #Heteroscedastity

  p_heteroscedastity <- lmtest::bptest(reg_model)[["p.value"]]
  if (p_heteroscedastity <= sig_level) {
    cat("         __________________________________________________ \n ")
    cat("          There are hetereoscedactity issues ",
        crayon::red(
      paste0("p <  ", sig_level)),
      "\n                                              ",
      crayon::red("p = "),
      crayon::red(signif(p_heteroscedastity, 0)))
  }
  if (p_heteroscedastity >= sig_level){
    cat("         __________________________________________________ \n ")
    cat("          Hetereoscedactity is not an issue  ",
        crayon::green(
          paste0("p >  ", sig_level)),
        "\n                                            ",
        crayon::green("p = "),
        crayon::green(signif(p_heteroscedastity, 0)))
  }
  cat("\n         __________________________________________________ ")
  #Multicolinearity
  VIF_results <- VIF_regclass(reg_model, indent = "")

  output <- VIF_results[["output"]]
  multicolinearity <- VIF_results[["multicolinearity"]]

  if (multicolinearity == T){
    cat("  \n ")
    cat("          There are multicolinearity issues ",
        crayon::red("VIF >  5.000"),
        "\n", output, sep = "")
  }
  if (multicolinearity == F){
    cat("\n ")
    cat("          Multicolinearity is not an issue ",
        crayon::green(" VIF <  5.000"),
         "\n", output, sep = "")
  }
  cat("         __________________________________________________ \n ")
  #Normally distributed residuals

  wilks_test <- reg_model[["residuals"]] |> shapiro.test()
  wilks_test <- signif(wilks_test[["p.value"]], digits =0)
  if (wilks_test <= .01) {
    cat("           Residuals not normaly distributed ",crayon::red("p < 0.01 \n"),
        "             Shapiro Wilk normality test:",
        crayon::red("    p ="),
        crayon::red(wilks_test))
  }
  if (wilks_test > .01) {
    cat("           Residuals are normally distributed", crayon::green(" p >  0.01 \n"),
        "             Shapiro Wilk normality test:",
        crayon::green("  p ="),
        crayon::green(wilks_test))
  }


}





VIF_regclass <- function(reg_model, indent = " "){
  VIFs <- regclass::VIF(reg_model) |> round(3)
  variables <- names(VIFs)
  values <- list(NULL)
  formatted_values <- list(NULL)
  multicolinearity <- FALSE

  for (i in 1:length(VIFs)){
    if (i == 1) {
      sep <- ""
    }
    else {
      sep <- indent
    }
    if (VIFs[[i]] >= 5 & VIFs[[i]] < 10) {
      values[i] <- paste0(variables[i],
                          ": VIF =  ",
                          crayon::red(VIFs[[i]]),
                          sep, "\n")
      formatted_values[i] <- sprintf("%+68s", as.character(values[i]))
      multicolinearity <- TRUE
    }
    if (VIFs[[i]] < 5) {
      values[i] <- paste0(variables[i],
                          ": VIF =  ",
                          crayon::green(VIFs[[i]]),
                          sep, "\n")
      formatted_values[i] <- sprintf("%+68s", as.character(values[i]))
      }
    if (VIFs[[i]] >= 10) {
      values[i] <- paste0(variables[i],
                          ": VIF = ",
                          crayon::green(VIFs[[i]]),
                          sep, "\n")
      formatted_values[i] <- sprintf("%+68s", as.character(values[i]))
      }
    }

  output <- list(output = as.character(formatted_values),
                 multicolinearity = multicolinearity )
  return(output)
}
