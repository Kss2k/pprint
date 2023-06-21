#### Div functions ####

format_percent <- function(x) {
  paste0(round(x*100, digits = 0), "%")
}

#### Functions inside pdataframe ####

get_N <- function(x) {
  complete.cases(x) |> sum()
}

count_na <- function(x) {
  is.na(x) |> sum()
}

count_unique <- function(x) {
  unique(x) |> length()
}


getmax <- function(x) {
  if (is.numeric(x) | is.integer(x)) {
    max(x, na.rm = T)
  }
  else {
    NA
  }

}

getmin <- function(x) {
  if (is.numeric(x) | is.integer(x)) {
    uniqv <- min(x, na.rm = T)
  }
  else {
    NA
  }
}

getmean <- function(x) {
  if (is.numeric(x) | is.integer(x)) {
    mean(x, na.rm = T)
  }
  else {
    NA
  }
}

get_sd <- function(x){
  if (is.numeric(x) | is.integer(x)) {
    sd(x, na.rm = T)
  }
  else {
    NA
  }
}


# Div

abs_exp_dev <- function(model_input) {
  (model_input$model[[1]] - mean(model_input$model[[1]])) |>
    abs() |> sum() -> total_sum_of_absolute_deviation
  model_input$residuals |> abs() |> sum() -> residual_sum_of_absolute_deviation
  model_sum_of_absolute_deviation <- total_sum_of_absolute_deviation - residual_sum_of_absolute_deviation
  explained_absolute_deviation <- model_sum_of_absolute_deviation/total_sum_of_absolute_deviation
  return(explained_absolute_deviation)
}

# Two functions which together only scale numeric and integer variables. (i.e, allowing me to get cohens'd for factor-variables)
scale_if_numeric <- function(variable) {
  if (is.integer(variable) | is.numeric(variable)) {
    return(scale(variable))
  }
  else {
    return(variable)
  }
}

scale_df <- function(dataframe) {
  df_out <- dataframe
  for (i in 1:ncol(dataframe)){
    df_out[[i]] <- scale_if_numeric(dataframe[[i]])
  }
  return(df_out)
}

