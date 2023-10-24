context("linearRegression")

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)


test_that("lenreg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda=0))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda=0))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

test_that("print() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  
  expect_output(ridgereg_mod$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  expect_output(ridgereg_mod$print(),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("predict() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  
  expect_equal(round(unname(ridgereg_mod$predict()[c(1,5,7)]), 2), c(1.85, 1.53, 1.09))    
})

test_that("coef() method works", {
  ridgereg_mod1 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0)
  ridgereg_mod2 <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0, scale=1)
  
  expect_true(all(round(unname(ridgereg_mod1$coef()),2) %in% c(-2.52, -1.34, 1.78)))
  expect_true(all(round(unname(ridgereg_mod2$coef()),2) %in% c(3.76, -0.58, 1.47)))
})
