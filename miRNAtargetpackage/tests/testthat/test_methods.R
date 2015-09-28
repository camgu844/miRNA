library(linregpackage)
context("Tests the methods coefficients, resid and pred")

data(faithful)
formula <- eruptions ~ waiting
data <- faithful

m1 = linreg(formula, data)
m2 = lm(formula, data)


test_that("Coefficients", {
  expect_less_than(sum(as.numeric(abs(coef(m1))-abs(coef(m2)))),0.01)
})

test_that("Residuals", {
  expect_less_than(sum(as.numeric(abs(residuals(m1))-abs(residuals(m2)))),0.01)
})

test_that("Predictions", {
  expect_less_than(sum(as.numeric(abs(predict(m1))-abs(predict(m2)))),0.01)
})
