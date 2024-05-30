cPrint <- function(df, rowNames = FALSE, maxRow = 300, ...) {
  # Get types:
  setColorFuncs()
  dataTypes <- vapply(df,
                      FUN = class,
                      FUN.VALUE = vector("character", 1L))
  clrData <- lapplyDf(df, FUN = colorVector, ...)
  chrData <- rbind(colnames(clrData), lapplyDf(df, FUN = as.character))
  colnamesData <- colnames(clrData)
  maxWidthCols <- vapply(chrData,
                         FUN = maxWidth,
                         FUN.VALUE = vector("integer", 1L))

  if (rowNames == TRUE) {
    maxWidthCols <- c(maxWidth(rownames(df)), maxWidthCols)
    clrData <- cbind.data.frame(as.character(rownames(df)), clrData)
    colnamesData <- c("", colnamesData)
  }

  colnames <- formatDfRow(colnamesData, widths = maxWidthCols, underline = TRUE)

  rows <- c(colnames, apply(clrData,
                MARGIN = 1,
                FUN = formatDfRow,
                widths = maxWidthCols))
  cat("\n")
  cat(stringr::str_c(rows, collapse = "\n"), "\n")
}


maxWidth <- function(x) {
  if (grepl("p-val", x[[1]])) {
    return(8L)
  }
  max(nchar(as.character(x)), na.rm = TRUE)
}


lapplyDf <- function(df, FUN, ...) {
  structure(lapply(df, FUN, ...),
            names = names(df),
            row.names = 1:nrow(df),
            class = "data.frame")
}


setColorFuncs <- function() {
  primaryColor <<- cli::make_ansi_style(themeColors("primary"))
  secondaryColor <<- cli::make_ansi_style(themeColors("secondary"))
}


colorVector <- function(x, ...) {
  switch(class(x),
         "numeric" = numericColor(x),
         "double" = numericColor(x),
         "integer" = numericColor(x),
         "character" = secondaryColor(x),
         "factor" = secondaryColor(x),
         "pval" = pvalColor(x, ...),
         "plainTxt" = as.character(x),
         secondaryColor(x))
}


numericColor <- function(x) {
  ifelse(is.na(x), secondaryColor("NA"),
         ifelse(x < 0, secondaryColor(x), primaryColor(x)))
}


pvalColor <- function(x, scientific = FALSE) {
  class(x) <- as.numeric(x)
  if (scientific == FALSE) {
    x <- round(x, 3)
  }
  out <- ifelse(x < 0.001,
         secondaryColor(format(x, scientific = scientific, digits = 3,
                               nsmall = 3)),
         primaryColor(format(x, scientific = scientific, digits = 3,
                             nsmall = 3)))
  out
}


formatDfRow <- function(row, widths, sep = "│ ", underline = FALSE) {
  if (!is.character(row)) {
    as.character(row)
  } else if (!is.numeric(widths)) {
    stop("widths are not numeric")
  }
  out <- purrr::map2_chr(.x = row,
                         .y = widths,
                         .f = function(elem, width)
                    cli::ansi_align(elem, width = width, align = "right")) |>
    stringr::str_c(collapse = sep)

  if (underline == TRUE) {
    out <- cli::style_underline(out)
  }

  out
}


as.plainTxt <- function(x) {
  class(x) <- "plainTxt"
  x
}


as.pval <- function(x) {
  class(x) <- "pval"
  x
}
