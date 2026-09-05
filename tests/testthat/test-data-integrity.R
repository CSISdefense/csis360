library(testthat)
library(csis360)
library(dplyr)

# Helper: a small tidy data frame, created fresh inside each test_that block
# to prevent shared mutable state between tests.
make_df <- function() {
  data.frame(
    id    = c(1, 2, 3, 4),
    group = c("A", "A", "B", "B"),
    label = c("x", "x", "y", "y"),
    value = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# check_key
# =============================================================================

test_that("check_key: returns TRUE when key uniquely identifies every row", {
  expect_true(check_key(make_df(), "id"))
})

test_that("check_key: returns FALSE and issues a warning when duplicates exist", {
  # 'group' has only two distinct values for four rows — not a unique key.
  expect_warning(
    result <- check_key(make_df(), "group"),
    regexp = "duplicated"
  )
  expect_false(result)
})

test_that("check_key: composite key that is unique returns TRUE", {
  expect_true(check_key(make_df(), c("id", "group")))
})

test_that("check_key: stops with a clear message when a key column is missing", {
  expect_error(check_key(make_df(), "nonexistent_col"), "missing from data frame")
})

# =============================================================================
# all_duplicate
# =============================================================================

test_that("all_duplicate: returns zero rows when there are no duplicate keys", {
  result <- all_duplicate(make_df(), "id")
  expect_equal(nrow(result), 0)
})

test_that("all_duplicate: returns BOTH occurrences of a duplicated key", {
  df     <- rbind(make_df(), make_df()[1, ])   # row with id=1 appears twice
  result <- all_duplicate(df, "id")
  expect_equal(nrow(result), 2)
  expect_true(all(result$id == 1))
})

test_that("all_duplicate: with key=NULL falls back to comparing all columns", {
  df <- make_df()
  expect_equal(nrow(all_duplicate(df)), 0)     # no fully identical rows

  df2 <- rbind(df, df[2, ])                    # one fully identical row added
  expect_equal(nrow(all_duplicate(df2)), 2)     # both copies returned
})

# =============================================================================
# check_derived
# =============================================================================

test_that("check_derived: returns TRUE when derived_col is consistent with key", {
  # Every 'id' maps to exactly one 'value'.
  expect_true(check_derived(make_df(), key = "id", derived_col = "value"))
})

test_that("check_derived: returns FALSE when derived_col is inconsistent with key", {
  df       <- make_df()
  df$value <- c(10, 99, 30, 40)   # group A maps to both 10 and 99
  expect_warning(
    result <- check_derived(df, key = "group", derived_col = "value")
  )
  expect_false(result)
})

test_that("check_derived: stops when key column is absent", {
  expect_error(check_derived(make_df(), key = "missing", derived_col = "value"),
               "missing from data frame")
})

test_that("check_derived: stops when derived_col is absent", {
  expect_error(check_derived(make_df(), key = "id", derived_col = "no_such_col"),
               "missing from data frame")
})

test_that("check_derived: stops when derived_col is entirely NA", {
  df       <- make_df()
  df$value <- NA
  expect_error(check_derived(df, key = "id", derived_col = "value"), "all na")
})

test_that("check_derived: stops when derived_col is part of the key", {
  # A column cannot be derived from a key that includes it.
  expect_error(check_derived(make_df(), key = c("id", "value"), derived_col = "value"),
               "should not be part of key")
})

test_that("check_derived: na.rm=TRUE tolerates partial NAs in derived_col", {
  df          <- make_df()
  df$value[3] <- NA   # one missing entry; na.rm should allow this
  expect_true(check_derived(df, key = "id", derived_col = "value", na.rm = TRUE))
})

# =============================================================================
# fill_derived
# =============================================================================

test_that("fill_derived: fills NA entries from consistent non-NA values", {
  df          <- make_df()
  df$value[2] <- NA   # id=2 is unique, so the fill is unambiguous
  result      <- fill_derived(df, key = "id", derived_col = "value")
  expect_false(any(is.na(result$value)))
})

test_that("fill_derived: stops when derived_col values are inconsistent", {
  df       <- make_df()
  df$value <- c(10, 99, 30, 40)   # group A maps to both 10 and 99
  expect_error(fill_derived(df, key = "group", derived_col = "value"), "Inconsistent")
})

test_that("fill_derived: returns a data frame with the same number of rows", {
  df          <- make_df()
  df$value[1] <- NA
  result      <- fill_derived(df, key = "id", derived_col = "value")
  expect_equal(nrow(result), nrow(df))
})

# =============================================================================
# group_by_list
# =============================================================================

test_that("group_by_list: single-column key groups correctly", {
  result <- group_by_list(make_df(), "group")
  expect_equal(dplyr::group_vars(result), "group")
})

test_that("group_by_list: multi-column key groups on all specified columns", {
  result <- group_by_list(make_df(), c("group", "label"))
  expect_setequal(dplyr::group_vars(result), c("group", "label"))
})

test_that("group_by_list: empty key returns an ungrouped data frame", {
  result <- group_by_list(make_df(), character(0))
  expect_equal(length(dplyr::group_vars(result)), 0)
})

test_that("group_by_list: grouped result works in a summarise pipeline", {
  result <- make_df() %>%
    group_by_list("group") %>%
    dplyr::summarise(total = sum(value), .groups = "drop")
  expect_equal(nrow(result), 2)          # two groups: A and B
  expect_true("total" %in% colnames(result))
})
