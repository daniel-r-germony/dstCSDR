library(testthat)
library(dstCSDR)

test_check("dstCSDR")

# wbs_code_to_lvl() texts

test_that("WBS Element Codes other than '1.0' return correct level", {
  expect_equal(wbs_code_to_lvl("1.1"), 1L)
  expect_equal(wbs_code_to_lvl("1.1.1"), 2L)
  expect_equal(wbs_code_to_lvl("1.1.1.1.1.1.1.1.1.1.1"), 10L)
})

test_that("WBS Element Code '1.0' returns level '0'", {
  expect_equal(wbs_code_to_lvl("1.0"), 0L)
})

test_that("Character vectors that are not WBS Element Codes produce an error", {
  expect_error(wbs_code_to_lvl("abc"))
  expect_error(wbs_code_to_lvl("123"))
  expect_error(wbs_code_to_lvl("1.x.1"))
})

test_that("Non-character, non-factor objects produce an error", {
  expect_error(wbs_code_to_lvl(iris))
  expect_error(wbs_code_to_lvl(lm(1 ~ 2 + 3)))
})
