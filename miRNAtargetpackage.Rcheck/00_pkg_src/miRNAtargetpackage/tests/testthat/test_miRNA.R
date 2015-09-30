library(miRNAtargetpackage)
library(httr)

context("tests the function miRNA_target_interactions")

r = miRNA_target_interactions("gene", "evidence count", 672,  3)

test_that("Class returned by targetHub", {
  expect_that(class(r), equals("targetHub"))
})


test_that("r is a list after applying as.list", {
  expect_that(class(as.list(r)), equals("list"))
})


r = miRNA_target_interactions("geNe", "EvidencE Count", 672,  3)

test_that("Test case sensitive", {
  expect_that(class(r), equals("targetHub"))
})

r = miRNA_target_interactions("gene", "specific method", 672,  "MirtarBase")

test_that("Test specific method", {
  expect_that(class(as.list(r)), equals("list"))
})
