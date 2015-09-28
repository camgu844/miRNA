library(miRNAtargetpackage)
library(httr)

context("tests the function miRNA_target_interactions")

tmp = miRNA_target_interactions("gene", "evidence count", 672,  3)

test_that("Class returned by targetHub", {
  expect_that(class(tmp), equals("targetHub"))
})

test_that("Test extract method", {
  expect_that(class(tmp$extract()), equals("list"))
})

test_that("Test atleast method", {
  expect_that(class(tmp$atleast()), equals("list"))
})


tmp = miRNA_target_interactions("geNe", "EvidencE Count", 672,  3)

test_that("Test case sensitive", {
  expect_that(class(tmp), equals("targetHub"))
})

tmp = miRNA_target_interactions("gene", "specific method", 672,  "MirtarBase")

test_that("Test specific method", {
  expect_that(class(tmp$extract()), equals("list"))
})
