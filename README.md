# pprintAlpha
 
pprint is a package for R which intends to improve the summary output for commonly used R objects, like dataframes linear regressions, factor models etc. 

the most important function in pprint is the psummary function which summarises different objects. 

## Example: dataframe summary
```
psummary(iris)
```

## Example: Linear Regression
```
lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris) |> psummary(std_beta = T)
```
## Example: Logistic Regression 
NB: You first have to install the ISLR package, which includes the ISLR dataset
```
glm(default ~ balance + income, data = ISLR::Default, family = binomial(link = logit)) |> psummary(accuracy = TRUE)
```
## Example: factor analysis 
```
psych::fa(iris[1:4], nfactors = 2, fm = "pa") |> suppressWarnings() |> psummary() 
```
