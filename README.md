# pprintAlpha
 
pprint is a package for R which intends to improve the summary output for commonly used R objects, like dataframes linear regressions, factor models etc. 

the most important function in pprint is the psummary function which summarises different objects. 

pprint automatically detects your theme on Rstudio (not all are supported yet) and adapts the color of your output accordingly. The use of colors is meant to make the output easier to read. You can change the default color settings using default_theme_options() in your .Rprofile

## Example


```
default_theme_options(primary_color = "red", # this changes the primary colour used when the Rstudio theme is not used
                      secondary_color = "green", # this changes the secondary colour used when the Rstudio theme is not used
                      tertiary_color = "blue" # this changes the tertiary colour used when the Rstudio theme is not used
                      gather_colors_from_theme = TRUE) # if FALSE pprint never uses Rstudio theme, instead using the default colors
```
