## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----setup--------------------------------------------------------------------
data("iris")

## -----------------------------------------------------------------------------

myModel <- 
  linearRegression::linreg(formula = Sepal.Length ~ . ,data =  iris)

# alternatively

library(linearRegression)

myModel <- linreg(formula = Sepal.Length ~ . ,data = iris)

## -----------------------------------------------------------------------------

myFormula <- Sepal.Length ~ Sepal.Width

linreg(formula = myFormula, data = iris)

## -----------------------------------------------------------------------------

class(iris$Species)
levels(iris$Species)

myFormula <- Sepal.Length ~ Sepal.Width + Species


linreg(formula = myFormula, data = iris)


## -----------------------------------------------------------------------------


myFormula <- Sepal.Length ~ Sepal.Width + I(Sepal.Width^2) + I(log(Petal.Width)) + Species


linreg(formula = myFormula, data = iris)


## -----------------------------------------------------------------------------


myFormula <- Sepal.Length ~ Sepal.Width + I(Sepal.Width^2) + I(log(Petal.Width)) + Petal.Width + Species + Petal.Width:Species


myModel <- linreg(formula = myFormula, data = iris)

myModel

## -----------------------------------------------------------------------------
myModel$summary()


## -----------------------------------------------------------------------------
myModel$plot()


## -----------------------------------------------------------------------------
## linreg$new(formula, data) == linreg(formula, data)

myModel <- linreg$new(formula = myFormula, data = iris)

class(linreg)
class(myModel)

## -----------------------------------------------------------------------------

#beta coefficients
myModel$beta_hats

#10 first residuals
head(myModel$resid(), 10)




## -----------------------------------------------------------------------------
                        formula <- myFormula
                        data <- iris
                        formula_call <- deparse(substitute(formula))
                        data_call <- deparse(substitute(data))
                        
                        X <- model.matrix(formula, data) # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/model.matrix
                        y <- as.matrix(data[all.vars(formula)[1]])
                        
                        beta_hats <- solve(t(X) %*% X) %*% t(X) %*% y
                        y_hat <- X %*% beta_hats
                        res <- y - y_hat
                        df <- length(y) - length(beta_hats)
                        res_var <- ((t(res) %*% res) / df)[1] # [1] to access the value
                        var_beta_hats <- diag(res_var * solve(t(X) %*% X))
                        t_beta_hats <- beta_hats / sqrt(var_beta_hats)
                        p_beta_hats <- pt(abs(t_beta_hats), df, lower.tail=FALSE)*2 # x2 for two-tailed p-values

## -----------------------------------------------------------------------------
# Plot the residuals with a theme matching the graphic profile of Linköpings University!

myModel$plot(theme = "liu")

