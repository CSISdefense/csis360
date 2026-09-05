library(testthat)
library(csis360)
library(ggplot2)
library(dplyr)

# =============================================================================
# jitter_binary
# =============================================================================

test_that("jitter_binary: zeros stay within [0, jitt] with default jitt", {
  set.seed(42)
  a <- rep(0L, 100)
  result <- jitter_binary(a)
  # All outputs must be non-negative and no greater than the default jitter band.
  expect_true(all(result >= 0))
  expect_true(all(result <= 0.05))
})

test_that("jitter_binary: ones stay within [1 - jitt, 1] with default jitt", {
  set.seed(42)
  a <- rep(1L, 100)
  result <- jitter_binary(a)
  expect_true(all(result >= 0.95))
  expect_true(all(result <= 1))
})

test_that("jitter_binary: custom jitt value is respected", {
  set.seed(7)
  jitt  <- 0.2
  zeros <- jitter_binary(rep(0L, 50), jitt = jitt)
  ones  <- jitter_binary(rep(1L, 50), jitt = jitt)
  expect_true(all(zeros >= 0        & zeros <= jitt))
  expect_true(all(ones  >= 1 - jitt & ones  <= 1))
})

test_that("jitter_binary: mixed 0/1 vector preserves banding for each value", {
  set.seed(99)
  a      <- c(0, 1, 0, 1, 0, 1)
  result <- jitter_binary(a)
  expect_true(all(result[a == 0] >= 0    & result[a == 0] <= 0.05))
  expect_true(all(result[a == 1] >= 0.95 & result[a == 1] <= 1))
})

test_that("jitter_binary: output length matches input length", {
  set.seed(1)
  a <- c(0, 1, 0, 0, 1)
  expect_length(jitter_binary(a), length(a))
})

# =============================================================================
# label_units
# =============================================================================

test_that("label_units: NA input returns the string 'NA'", {
  # NA must be handled before any arithmetic is attempted.
  expect_equal(label_units(NA), "NA")
})

test_that("label_units: single-digit values format to two decimal places", {
  # Values < 10 should round to 2 dp.
  expect_equal(label_units(5),    "5")
  expect_equal(label_units(1.23), "1.23")
})

test_that("label_units: two-digit values format to one decimal place", {
  # Values >= 10 and < 100 should round to 1 dp.
  expect_equal(label_units(50),   "50")
  expect_equal(label_units(12.3), as.character(round(12.3, 1)))
})

# NOTE: There is a pre-existing bug in label_units for values in [100, 999].
# The branch `if(abs(y) < 1000)` assigns to `ylab` (no underscore) instead of
# `y_lab`, so the update is silently discarded. Tests for this range are
# therefore omitted until the bug is fixed; adding an assertion here would
# encode the wrong behaviour as correct.

test_that("label_units: thousands format with 'k' suffix", {
  expect_equal(label_units(5000),  "5k")
  expect_equal(label_units(50000), paste0(round(50000 / 1000, 1), "k"))
})

test_that("label_units: millions format with 'M' suffix", {
  expect_equal(label_units(5e6), paste0(round(5e6 / 1e6, 1), "M"))
  expect_equal(label_units(5e7), paste0(round(5e7 / 1e6, 0), "M"))
})

test_that("label_units: billions format with 'B' suffix", {
  expect_equal(label_units(2e9),  paste0(round(2e9  / 1e9, 2), "B"))
  expect_equal(label_units(2e10), paste0(round(2e10 / 1e9, 1), "B"))
  expect_equal(label_units(2e11), paste0(round(2e11 / 1e9, 0), "B"))
})

test_that("label_units: vectorised over a mixed set of values", {
  input  <- c(NA, 5, 5000, 5e6, 2e9)
  result <- label_units(input)
  expect_length(result, length(input))
  expect_equal(result[1], "NA")
})

# =============================================================================
# get_plot_theme
# =============================================================================

test_that("get_plot_theme: returns an object of class 'theme'", {
  t <- get_plot_theme()
  expect_s3_class(t, "theme")
})

test_that("get_plot_theme: legend is placed at the bottom by default", {
  t <- get_plot_theme()
  expect_equal(t$legend.position, "bottom")
})

test_that("get_plot_theme: erase_legend_title=TRUE blanks the legend title", {
  t <- get_plot_theme(erase_legend_title = TRUE)
  expect_s3_class(t$legend.title, "element_blank")
})

test_that("get_plot_theme: erase_legend_title=FALSE keeps a non-blank legend title", {
  t <- get_plot_theme(erase_legend_title = FALSE)
  expect_false(inherits(t$legend.title, "element_blank"))
})

test_that("get_plot_theme: blank_x_lines=TRUE removes x grid lines", {
  t <- get_plot_theme(blank_x_lines = TRUE)
  expect_s3_class(t$panel.grid.major.x, "element_blank")
  expect_s3_class(t$panel.grid.minor.x, "element_blank")
})

test_that("get_plot_theme: blank_x_lines=FALSE adds visible x grid lines", {
  t <- get_plot_theme(blank_x_lines = FALSE)
  expect_s3_class(t$panel.grid.major.x, "element_line")
  expect_s3_class(t$panel.grid.minor.x, "element_line")
})
