#### Factoranalysis ####


#' Summary for fa/principal object from psych package
#'
#' @param factor_model pscyh fa/principal object
#' @param primary primary colour this is automatically chosen, but can be overwritten
#' @param secondary secondary colour this is automatically chosen, but can be overwritten
#' @param plaintext colour for plaintext this is automatically chosen, but can be overwritten
#' @param threshold factorloadings below threshold are blanked default = 0.4
#' @param eigenvalues if TRUE (default) eigenvalues are printed
#' @param highlight_correlations factor correlations above the value are highlighted
#' @param Pattern_Matrix if TRUE (default) pattern matrix is printed
#' @param correlations if TRUE (default) factor correlations are printed
#' @param rotation_map if TRUE (default) rotation map is printed
#' @param scree_plot if TRUE screeplot is printed NB: not implemented yet
#' @param descriptives if TRUE descriptives are included NB: not implemented yet
#'
#' @return
#' @export
#'
#' @examples
psummary.psych <- function(factor_model,
                    threshold = .4,
                    eigenvalues = T,
                    highlight_correlations = .2,
                    pattern_matrix = T,
                    correlations = T,
                    rotation_map = F,
                    scree_plot = F,
                    descriptives = F) {


  #Eigenvaluetable¨
  if (eigenvalues == TRUE) {
    eigenvalue_table(factor_model)
  }
  #Factorloadings
  if (pattern_matrix == TRUE) {
    pattern_matrix(factor_model,
                   blank = threshold)
  }

  #Factor correlations
  if (correlations == TRUE) {
    factor_correlations(factor_model ,
                        primary = primary,
                        highlight = highlight_correlations)
  }

  if (rotation_map == TRUE){
    rotation_matrix(factor_model ,
                        primary = primary,
                        highlight = highlight_correlations)
  }

}

#### Eigenvalue_table ####

eigenvalue_table <- function(factor_model) {
  #number of factors
  number_of_factors <- factor_model[["Call"]][["nfactors"]]
  #itemnames
  item_names <- names(factor_model[["communality"]])
  number_of_items <- length(item_names)
  #Factornames
  factor_names <- list(length = number_of_items)

  for (i in 1:number_of_items) {
    factor_names[[i]] <- paste0(" Factor ", i)
  }


  #Eigenvalues

  eigenvalues <- factor_model[["values"]]
  percent_decimal <- eigenvalues/sum(eigenvalues)
  percent <- format_percent(percent_decimal)
  cumulative_decimal <- percent_decimal

  for (i in 1:length(eigenvalues)){
    cumulative_decimal[i] <- sum(percent_decimal[1:i])
  }

  cumulative <- format_percent(cumulative_decimal)

  #Rotated eigenvalues

  pattern_matrix <- factor_model$loadings |>
    as.list() |> unlist() |>
    matrix(nrow = number_of_items, ncol = number_of_factors) |>
    as.data.frame(row.names = item_names, make.names = T) |>
    apply(2, as.numeric)


  rotated_eigenvalues <- pattern_matrix^2 |> apply(2, sum) |> as.data.frame()
  rotated_eigenvalues[nrow(rotated_eigenvalues) +
                        number_of_items - number_of_factors,] <- NA

  rotated_eigenvalues[is.na(rotated_eigenvalues)] <- 0

  percent_decimal_rotated <- rotated_eigenvalues/sum(eigenvalues)
  percent_rotated <- apply(percent_decimal_rotated, 1, format_percent) |> as.vector()
  cumulative_decimal_rotated <- percent_decimal_rotated

  for (i in 1:length(eigenvalues)){
    cumulative_decimal_rotated[i,1] <- sum(percent_decimal_rotated[1:i,1])
  }

  cumulative_rotated <- apply(cumulative_decimal_rotated, 1, format_percent) |> as.vector()



  eigenvalue_table <- data.frame(Eigenvalues = round(eigenvalues, 3),
                                 Percent = percent,
                                 Cumulative = cumulative,
                                 Extraction_Eigenvalues = round(rotated_eigenvalues, digits = 3),
                                 Extraction_Percent = percent_rotated,
                                 Extraction_Cumulative = cumulative_rotated,
                                 row.names = factor_names)

  # Creating blank values for rotated solution
  for (i in (number_of_factors + 1):number_of_items) {
    eigenvalue_table[i, 4:6] <- ""
  }

  colnames(eigenvalue_table) <- c("Eigenvalues", "Percent",  "Cumulative", "Eigenvalues.", "Percent.", "Cumulative.")

  cat("\n \n \n \n Summary: Eigenvalues and Explained Variance \n")
  cPrint(eigenvalue_table, maxRow = nrow(eigenvalue_table),
         rowNames = TRUE)
}


#### P matrix ####
pattern_matrix <- function(factor_model,
                           blank = .4#,
                           #highlight = 0.6
                           ) {
  #number of factors
  number_of_factors <- factor_model[["Call"]][["nfactors"]]
  #itemnames
  item_names <- names(factor_model[["communality"]])
  #Factornames
  factor_names <- list(length = length(item_names))
  for (i in 1:number_of_factors) {
    factor_names[[i]] <- paste0("Factor", i)
    factor_names[[i+1]] <- "Complexity"
    factor_names[[i+2]] <- "Communality"
  }

  pattern_matrix <- factor_model$loadings |>
    as.list() |> unlist() |>
    matrix(nrow = length(item_names), ncol = number_of_factors) |>
    as.data.frame(row.names = item_names, make.names = T)

  #adding complexity
  pattern_matrix[number_of_factors + 1] <- as.vector(factor_model[["complexity"]])
  #adding communality
  pattern_matrix[number_of_factors + 2] <- as.vector(factor_model[["communality"]])
  #naming the columns
  colnames(pattern_matrix) <- factor_names

  #Making it a colorfull DF
  pattern_matrix <- colorDF::as.colorDF(pattern_matrix, theme = "wb")
  pattern_matrix <- round(pattern_matrix, digits = 3)
  # Old pattern_matrix[1:number_of_factors][pattern_matrix[1:number_of_factors] < blank] <- ""

  # New
  pattern_matrix[1:number_of_factors][abs(pattern_matrix[1:number_of_factors]) < blank] <- ""


  # New 15/06/23
  pattern_matrix[1:number_of_factors][pattern_matrix[1:number_of_factors] < blank &
                                        pattern_matrix[1:number_of_factors] > -blank] <- ""


  cat("\n \n \n Summary: Factorloadings and Communalities \n")
  cPrint(pattern_matrix, maxRow = nrow(pattern_matrix),
         rowNames = TRUE)

}


#### Factor correlations ####

factor_correlations <- function(factor_model,
                                primary = theme_colors(primary = T),
                                secondary = theme_colors(secondary = T),
                                plaintext = theme_colors(fg = T),
                                highlight = 0.2){
  #Correlation matrix
  correlation_matrix <- as.data.frame(factor_model[["r.scores"]])

  #number of factors
  number_of_factors <- factor_model[["Call"]][["nfactors"]]

  #factor names
  factor_names <- list(length = number_of_factors)
  for (i in 1:number_of_factors) {
    factor_names[[i]] <- paste0(" Factor", i)

  }
  correlation_matrix <- colorDF::as.colorDF(correlation_matrix, theme = "wb")

  colnames(correlation_matrix) <- factor_names
  rownames(correlation_matrix) <- factor_names

  correlation_matrix <- round(correlation_matrix, digits = 3)

  cat("\n \n \n Summary: Factor correlations \n")
  cPrint(correlation_matrix, maxRow = nrow(correlation_matrix),
         rowNames = TRUE)
}

rotation_matrix <- function(factor_model,
                            primary = theme_colors(primary = T),
                            secondary = theme_colors(secondary = T),
                            plaintext = theme_colors(fg = T),
                            highlight = 0.2) {

  rotation_matrix <- as.data.frame(factor_model[["rot.mat"]])

  #number of factors
  number_of_factors <- factor_model[["Call"]][["nfactors"]]

  #factor names
  factor_names <- list(length = number_of_factors)
  for (i in 1:number_of_factors) {
    factor_names[[i]] <- paste0(" Factor", i)

  }
  rotation_matrix <- colorDF::as.colorDF(rotation_matrix, theme = "wb")

  colnames(rotation_matrix) <- factor_names
  rownames(rotation_matrix) <- factor_names


  rotation_matrix <- round(rotation_matrix, digits = 3)

  cat("\n \n \n Summary: Rotation Matrix \n")
  cPrint(rotation_matrix, maxRow = nrow(rotation_matrix),
         rowNames = TRUE)

}

