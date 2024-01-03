library(testthat)
library(dplyr)

# Load the testing set
test_citations <- read.csv("labelled_test_set.csv")
test_citations <- as.data.frame(test_citations)

# Test case: No duplicates detected
test_that("No duplicates detected", {
  test_citations_nodups <- test_citations[c(6, 21, 65),]

  expect_warning({
    a <- dedup_citations(test_citations_nodups, merge_citations = TRUE)
  }, "No duplicates detected!")

  expect_warning({
    a <- dedup_citations(test_citations_nodups, merge_citations = FALSE)
  }, "No duplicates detected!")
})

# Test case: Duplicate pairs detected but no TRUE pairs
test_that("Duplicate pairs detected but no TRUE pairs", {
  test_citations_notruedups <- test_citations[c(5, 11, 13),]
  test_citations_notruedups$doi <- test_citations_notruedups$doi[2]

  expect_warning({
    a <- dedup_citations(test_citations_notruedups, merge_citations = TRUE)
  }, "No duplicates detected!")

  expect_warning({
    a <- dedup_citations(test_citations_notruedups, merge_citations = FALSE)
  }, "No duplicates detected!")
})

# Test case: Non-essential column missing
test_that("Non-essential column missing", {
  test_citations_nolabel <- test_citations %>% select(-label)

  expect_no_warning({
    a <- dedup_citations(test_citations_nolabel, merge_citations = FALSE)
  })
})

# Test case: Essential column missing
test_that("Essential column missing", {
  test_citations_nopages <- test_citations %>% select(-pages)

  res <- dedup_citations(test_citations_nopages, merge_citations = FALSE, user_input = 1)
  expect_equal(length(res$unique$duplicate_id), 584)

  expect_match({
    dedup_citations(test_citations_nopages, merge_citations = FALSE, user_input = 2)
  }, regexp = "Halting dedup")
})

# Test case: Deduplication performing as normal
test_that("Deduplication performing as normal", {
  res <- dedup_citations(test_citations, merge_citations = FALSE, manual_dedup = FALSE)
  expect_equal(length(res$duplicate_id), 586)
})

# Test case: Deduplication with merge performing as normal
test_that("Deduplication with merge performing as normal", {
  res <- dedup_citations(test_citations, merge_citations = TRUE)
  expect_equal(length(res$unique$duplicate_id), 586)
})

# Test case: Deduplication with non-numeric record IDs
test_that("Deduplication with non-numeric record IDs", {
  expect_no_warning(
  tibble(
    title = c(rep(LETTERS[1:3], 2), LETTERS[7:9]), author = c(rep(LETTERS[1:3], 2), LETTERS[7:9]),
    journal = c(rep(LETTERS[1:3], 2), LETTERS[7:9]),
    year = c(rep(2010:2012, 2), 2017:2019), source = c(rep("aa", 5), rep("bb", 4)),
    record_id = letters[1:9], label = ""
  ) %>% ASySD::dedup_citations(user_input=1)
)

})
