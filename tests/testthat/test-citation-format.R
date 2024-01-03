library(testthat)
library(dplyr)

# Load the testing set
test_citations <- read.csv("labelled_test_set.csv")
test_citations <- as.data.frame(test_citations)

# Test case: No record_id column
test_that("No record_id column", {
  test_citations_no_col_for_id <- test_citations %>%
    select(-record_id)

  w <- capture_warnings(dedup_citations(test_citations_no_col_for_id, merge_citations = TRUE, user_input = 1))
  expect_match(w, "Search contains missing values for the record_id column. A record_id will be created using row numbers", all = FALSE)
})

# Test case: Blank record_id column
test_that("Blank record_id column", {
  test_citations_no_record_id <- test_citations
  test_citations_no_record_id$record_id <- ""

  expect_warning(
    dedup_citations(test_citations_no_record_id, merge_citations = TRUE),
    "Search contains missing values for the record_id column. A record_id will be created using row numbers"
  )
})

# Test case: NA record_id column
test_that("NA record_id column", {
  test_citations_na_record_id <- test_citations
  test_citations_na_record_id$record_id <- NA

  expect_warning(
    dedup_citations(test_citations_na_record_id, merge_citations = TRUE, user_input = 1),
    "Search contains missing values for the record_id column. A record_id will be created using row numbers"
  )
})

# Test case: Non-unique record_id column
test_that("Non-unique record_id column", {
  test_citations_non_unique <- test_citations
  test_citations_non_unique$record_id <- rep(c(1:10), times = round(nrow(test_citations)), length.out = nrow(test_citations))

  expect_warning(
    dedup_citations(test_citations_non_unique, merge_citations = TRUE),
    "The record_id column is not unique. A record_id will be created using row numbers"
  )
})

# Test case: Some missing record_id values
test_that("Some missing record_id values", {
  test_citations_missing <- test_citations
  test_citations_missing$record_id <- rep(c(1, 2, 3, 4, NA), times = round(nrow(test_citations)), length.out = nrow(test_citations))

  expect_warning(
    dedup_citations(test_citations_missing, merge_citations = TRUE),
    "Search contains missing values for the record_id column. A record_id will be created using row numbers"
  )
})

# Test case: Formatting citations doesn't drop rows
test_that("Formatting citations doesn't drop rows", {
  expect_equal(nrow(ASySD:::format_citations(test_citations)), nrow(test_citations))
})

