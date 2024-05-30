default_theme_options <- settings::options_manager(
  primary_color = "#00CC66",
  secondary_color = "red",
  tertiary_color = "blue",
  gather_colors_from_theme = TRUE

)


# get themes
theme_support <- function(theme_name) {
  theme_name <- stringr::str_replace_all(theme_name, " ", "_")

  themes <- list(
    Ambiance = list(primary = "#fa8d6a",
                   secondary = "#8f9d6a",
                   foreground = "snow"),
    Dreamweaver = list(primary = rgb(0, 0, 205, maxColorValue = 255),
                      secondary = "red",
                      foreground = "black"),
    Tomorrow_Night_Blue = list(primary = "#D1F1A9",
                              secondary = "#99FFFF",
                              foreground = "snow"),
    Pastel_On_Dark = list(primary = rgb(0, 0, 205, maxColorValue = 255),
                         secondary = "red",
                         foreground = rgb(234, 234, 234, maxColorValue = 255)),
    Monokai = list(primary = "#f92672",
                  secondary = "#AE81FF",
                  foreground = "#F8F8F2"),
    Dracula = list(primary = "mediumpurple2",
                   secondary = "magenta",
                   foreground = "#F8F8F2")


  )

  if (theme_name %in% names(themes)) {
   return(themes[[theme_name]])
  } else {
   return(FALSE)
  }
}


themeColors <- function(type = "primary") {
  current_theme_name <- 
    tryCatch(rstudioapi::getThemeInfo()$editor, 
             error = function(e) "Monokai")


  if (default_theme_options("gather_colors_from_theme") == TRUE){
    theme_settings <- theme_support(current_theme_name)
  } else {
    theme_settings <- FALSE
  }

  if (is.list(theme_settings)){
    primary_col <- theme_settings$primary
    secondary_col <- theme_settings$secondary
  } else {
    primary_col <- default_theme_options("primary_color")
    secondary_col <- default_theme_options("secondary_color")
  }

  if (type == "primary"){
    return(primary_col)
  } else if (type == "secondary"){
    return(secondary_col)
  } 
  stop("Invalid type argument. Must be 'primary' or 'secondary'")
}
