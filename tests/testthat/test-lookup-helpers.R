library(testthat)
library(csis360)
library(dplyr)
library(lubridate)

# =============================================================================
# get_delim
# =============================================================================

test_that("get_delim: csv extension returns comma", {
  expect_equal(get_delim("mydata.csv"), ",")
})

test_that("get_delim: txt extension returns tab", {
  expect_equal(get_delim("mydata.txt"), "\t")
})

test_that("get_delim: full path with csv still works (only last 3 chars matter)", {
  expect_equal(get_delim("path/to/some/file.csv"), ",")
})

test_that("get_delim: unknown extension throws an error", {
  # Any extension other than csv/txt is explicitly unsupported.
  expect_error(get_delim("mydata.xlsx"), "Unknown file type")
  expect_error(get_delim("mydata.tsv"),  "Unknown file type")
})

# =============================================================================
# text_to_number
# =============================================================================

test_that("text_to_number: strips dollar signs", {
  expect_equal(text_to_number("$1234.56"), 1234.56)
})

test_that("text_to_number: strips commas", {
  expect_equal(text_to_number("1,234,567"), 1234567)
})

test_that("text_to_number: converts accounting-style negatives (X) to -X", {
  # The pattern ($1,234) is common in Excel exports of financial data.
  expect_equal(text_to_number("($1,234)"), -1234)
})

test_that("text_to_number: handles combined dollar, commas, and parens", {
  expect_equal(text_to_number("($2,500.00)"), -2500)
})

test_that("text_to_number: passes numeric values through unchanged", {
  # Numeric input should not be coerced at all.
  x <- 42.5
  expect_equal(text_to_number(x), x)
})

test_that("text_to_number: works on a character vector", {
  result <- text_to_number(c("$100", "1,000", "($50)"))
  expect_equal(result, c(100, 1000, -50))
})

test_that("text_to_number: works on a factor", {
  f      <- factor(c("$200", "($300)"))
  result <- text_to_number(f)
  expect_equal(result, c(200, -300))
})

# =============================================================================
# text_to_bit
# =============================================================================

test_that("text_to_bit: recognises common 'yes' strings as 1L", {
  yes_inputs <- c("Y", "YES", "Y: YES", "Y:", "1", "TRUE", "TRUE:", "T", "T:")
  result     <- text_to_bit(yes_inputs)
  expect_true(all(result == 1L))
  expect_type(result, "integer")
})

test_that("text_to_bit: recognises common 'no' strings as 0L", {
  no_inputs <- c("N", "NO", "N: NO", ": NO", "N:", "0", "FALSE", "FALSE:", "F", "F:")
  result    <- text_to_bit(no_inputs)
  expect_true(all(result == 0L))
})

test_that("text_to_bit: empty string and bare colon return NA", {
  result <- text_to_bit(c("", ":"))
  expect_true(all(is.na(result)))
})

test_that("text_to_bit: matching is case-insensitive", {
  # Input is uppercased internally, so lower-case equivalents must also work.
  expect_equal(text_to_bit("yes"),   1L)
  expect_equal(text_to_bit("no"),    0L)
  expect_equal(text_to_bit("true"),  1L)
  expect_equal(text_to_bit("false"), 0L)
})

test_that("text_to_bit: unrecognised values throw an error", {
  # Any value not in the defined lists must be rejected to prevent silent misclassification.
  expect_error(text_to_bit("maybe"), "text_to_bit does not know how to handle")
  expect_error(text_to_bit("2"),     "text_to_bit does not know how to handle")
})

test_that("text_to_bit: mixed valid vector handled correctly", {
  result <- text_to_bit(c("Y", "N", ""))
  expect_equal(result, c(1L, 0L, NA_integer_))
})

# =============================================================================
# get_fiscal_year
# =============================================================================

test_that("get_fiscal_year: January stays in the same calendar year (FY starts Oct)", {
  # The US federal fiscal year starts 1 October; months 1-9 map to the same year.
  expect_equal(get_fiscal_year(as.Date("2020-01-15")), 2020)
})

test_that("get_fiscal_year: September stays in the same calendar year", {
  expect_equal(get_fiscal_year(as.Date("2020-09-30")), 2020)
})

test_that("get_fiscal_year: October rolls forward to the next calendar year", {
  expect_equal(get_fiscal_year(as.Date("2020-10-01")), 2021)
})

test_that("get_fiscal_year: December rolls forward to the next calendar year", {
  expect_equal(get_fiscal_year(as.Date("2020-12-31")), 2021)
})

test_that("get_fiscal_year: works on a vector of dates", {
  dates  <- as.Date(c("2019-09-30", "2019-10-01"))
  result <- get_fiscal_year(dates)
  expect_equal(result, c(2019L, 2020L))
})

# =============================================================================
# remove_bom
# =============================================================================

test_that("remove_bom: removes UTF-8 BOM prefix from first column name", {
  # Files saved from certain editors/Excel exports prepend a 3-byte BOM
  # (0xEF 0xBB 0xBF) that appears as 'ï..' when read as Latin-1.
  bom_df <- data.frame(matrix(ncol = 2, nrow = 1))
  colnames(bom_df) <- c("\xef\xbb\xbfFiscal.Year", "Value")
  result <- remove_bom(bom_df)
  expect_equal(colnames(result)[1], "Fiscal.Year")
})

test_that("remove_bom: data frame without BOM passes through unchanged", {
  clean_df <- data.frame(Fiscal.Year = 2020, Value = 1)
  result   <- remove_bom(clean_df)
  expect_equal(colnames(result), colnames(clean_df))
})
