library(linregpackage)
context("tests the function linreg")

data(faithful)

formula <- eruptions ~ waiting
data <- faithful

test_that("Class returned by linreg", {
  expect_that(class(linreg(formula, data)), equals("linreg"))
})

