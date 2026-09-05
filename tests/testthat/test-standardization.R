library(testthat)
library(csis360)
library(dplyr)

# =============================================================================
# na_non_positive_log
# =============================================================================

test_that("na_non_positive_log: positive values return their natural log", {
  x      <- c(1, exp(1), exp(2))
  result <- na_non_positive_log(x)
  expect_equal(result, c(0, 1, 2))
})

test_that("na_non_positive_log: zero is converted to NA before logging", {
  # log(0) is -Inf; the function must replace 0 with NA first.
  expect_true(is.na(na_non_positive_log(0)))
})

test_that("na_non_positive_log: negative values are converted to NA", {
  result <- na_non_positive_log(c(-5, -1, -0.001))
  expect_true(all(is.na(result)))
})

test_that("na_non_positive_log: mixed vector handles each element correctly", {
  x      <- c(-1, 0, 1, exp(1))
  result <- na_non_positive_log(x)
  expect_true(is.na(result[1]))    # -1 → NA
  expect_true(is.na(result[2]))    # 0  → NA
  expect_equal(result[3], 0)       # log(1) = 0
  expect_equal(result[4], 1)       # log(e) = 1
})

test_that("na_non_positive_log: existing NAs in input remain NA", {
  result <- na_non_positive_log(c(NA, 1))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 0)
})

# =============================================================================
# replace_nas_with_unlabeled
# =============================================================================

test_that("replace_nas_with_unlabeled: replaces NA in a character column", {
  df     <- data.frame(cat = c("Alpha", NA, "Beta", NA), stringsAsFactors = FALSE)
  result <- replace_nas_with_unlabeled(df, var = "cat")
  expect_false(any(is.na(result$cat)))
  expect_true(all(result$cat[c(2, 4)] == "Unlabeled"))
})

test_that("replace_nas_with_unlabeled: non-NA values are not changed", {
  df     <- data.frame(cat = c("Alpha", NA, "Beta"), stringsAsFactors = FALSE)
  result <- replace_nas_with_unlabeled(df, var = "cat")
  expect_equal(result$cat[1], "Alpha")
  expect_equal(result$cat[3], "Beta")
})

test_that("replace_nas_with_unlabeled: custom replacement string is used", {
  df     <- data.frame(cat = c("A", NA), stringsAsFactors = FALSE)
  result <- replace_nas_with_unlabeled(df, var = "cat", replacement = "Unknown")
  expect_equal(result$cat[2], "Unknown")
})

test_that("replace_nas_with_unlabeled: factor column gains replacement as a new level", {
  # 'Unlabeled' must be a valid factor level so downstream factor operations
  # don't silently re-introduce NAs.
  df     <- data.frame(cat = factor(c("Alpha", NA, "Beta")))
  result <- replace_nas_with_unlabeled(df, var = "cat")
  expect_true("Unlabeled" %in% levels(result$cat))
  expect_false(any(is.na(result$cat)))
})

test_that("replace_nas_with_unlabeled: column with no NAs is returned unchanged", {
  df     <- data.frame(cat = c("A", "B", "C"), stringsAsFactors = FALSE)
  result <- replace_nas_with_unlabeled(df, var = "cat")
  expect_equal(result$cat, df$cat)
})

test_that("replace_nas_with_unlabeled: other columns in the data frame are preserved", {
  df     <- data.frame(cat = c("A", NA), value = c(10, 20), stringsAsFactors = FALSE)
  result <- replace_nas_with_unlabeled(df, var = "cat")
  # Replacing NAs in 'cat' must not disturb the 'value' column.
  expect_equal(result$value, df$value)
})
