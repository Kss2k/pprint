% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pprint_logistic_regression.R
\name{plogit}
\alias{plogit}
\title{Summary for logistic regression model using GLM}
\usage{
plogit(
  logistic_reg_model,
  odds_ratio = FALSE,
  primary = theme_colors(primary = T),
  secondary = theme_colors(secondary = T),
  plaintext = theme_colors(fg = T),
  accuracy = FALSE
)
}
\arguments{
\item{logistic_reg_model}{a GLM type object family = binomial(link = "logit)}

\item{odds_ratio}{if TRUE odds ratio coefficients are displayed}

\item{primary}{primary colour this is automatically chosen, but can be overwritten}

\item{secondary}{secondary colour this is automatically chosen, but can be overwritten}

\item{plaintext}{colour for plaintext this is automatically chosen, but can be overwritten}

\item{accuracy}{If = TRUE TRUE/FALSE positives/negatives are included, and specificity, sensitivity and positiv/negative predictive values}
}
\description{
Summary for logistic regression model using GLM
}
