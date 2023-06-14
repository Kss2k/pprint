#### Div functions ####

format_percent <- function(x) {
  paste0(round(x*100, digits = 0), "%")
}

getmax <- function(x) {

  if (is.numeric(x) | is.integer(x)) {
    uniqv <- max(x, na.rm = T)
  }
  else {
    uniqv <- NA
  }
  return(uniqv)
}

getmin <- function(x) {

  if (is.numeric(x) | is.integer(x)) {
    uniqv <- min(x, na.rm = T)

  }
  else {
    uniqv <- NA
  }

  return(uniqv)
}

getmean <- function(x, na.ignore = T) {
  if (is.numeric(x) | is.integer(x)) {
    arithmetic_mean <- mean(x, na.rm = na.ignore)
  }
  else {
    arithmetic_mean <- NA
  }
  return(arithmetic_mean)
}

get_sd <- function(x, na.ignore = T){
  if (is.numeric(x) | is.integer(x)) {
    std.dev <- sd(x, na.rm = na.ignore)
  }
  else {
    std.dev <- NA
  }
  return(std.dev)
}



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

