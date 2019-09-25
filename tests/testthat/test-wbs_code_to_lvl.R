context("wbs_code_to_lvl() tests")
library(dstCSDR)

test_that("WBS Element Codes as chars return correct level", {
  expect_equal(wbs_code_to_lvl("1.0"), 0L)
  expect_equal(wbs_code_to_lvl("1.1"), 1L)
  expect_equal(wbs_code_to_lvl("1.1.1"), 2L)
  expect_equal(wbs_code_to_lvl("1.1.1.1.1.1.1.1.1.1.1"), 10L)
})

test_that("WBS Element Codes as factors return correct level", {
  expect_equal(wbs_code_to_lvl(factor("1.0")), 0L)
  expect_equal(wbs_code_to_lvl(factor("1.1")), 1L)
  expect_equal(wbs_code_to_lvl(factor("1.1.1")), 2L)
  expect_equal(wbs_code_to_lvl(factor("1.1.1.1.1.1.1.1.1.1.1")), 10L)
})

test_that("WBS Element Codes with padding return correct level", {
  expect_equal(wbs_code_to_lvl(factor("1.0 ")), 0L)
  expect_equal(wbs_code_to_lvl(factor("1.1     ")), 1L)
  expect_equal(wbs_code_to_lvl("1.0 "), 0L)
  expect_equal(wbs_code_to_lvl(("1.1     ")), 1L)
})

test_that("Character vectors that are not WBS Element Codes produce an error", {
  expect_error(wbs_code_to_lvl("abc"))
  expect_error(wbs_code_to_lvl("123"))
  expect_error(wbs_code_to_lvl("1.x.1"))
})

test_that("Non-character, non-factor objects produce an error", {
  expect_error(wbs_code_to_lvl(iris))
  expect_error(wbs_code_to_lvl(TRUE))
  expect_error(wbs_code_to_lvl(1+4i))
  expect_error(wbs_code_to_lvl(as.matrix(c(1:3, 4:6), ncol = 2)))
})
