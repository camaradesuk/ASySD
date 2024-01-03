test_that("dedup_check", {

  # get testing set
  test_citations <- read.csv("labelled_test_set.csv")
  test_citations <- as.data.frame(test_citations)

  # when no dups, check warning works
  test_citations_nodups <- test_citations[c(6,21,65),]

  expect_warning(
    a <- dedup_citations(test_citations_nodups, merge_citations = TRUE), "No duplicates detected!")

  expect_warning(
    a <- dedup_citations(test_citations_nodups, merge_citations = FALSE), "No duplicates detected!")


  # duplicate pairs detected but no TRUE pairs
  test_citations_notruedups <- test_citations[c(5,11,13),]
  test_citations_notruedups$doi <- test_citations_notruedups$doi[2]

  expect_warning(
    a <- dedup_citations(test_citations_notruedups, merge_citations = TRUE), "No duplicates detected!")

  expect_warning(
    a <- dedup_citations(test_citations_notruedups, merge_citations = FALSE), "No duplicates detected!")

  # when non-essential column missing
  test_citations_nolabel <- test_citations %>% select(-label)

  expect_no_warning(
    a <- dedup_citations(test_citations_nolabel, merge_citations = FALSE))

  # when essential column missing
  test_citations_nopages <- test_citations %>% select(-pages)

  expect_warning(regexp = "The following columns are missing: pages",
                 dedup_citations(test_citations_nopages, merge_citations = FALSE, user_input = 1)
  )

  res <- dedup_citations(test_citations_nopages, merge_citations = FALSE, user_input = 1)
  expect_equal(
    length(res$unique$duplicate_id), 584)

  expect_match(regexp = "Halting dedup",
    dedup_citations(test_citations_nopages, merge_citations = FALSE, user_input = 2)
  )

  # check deduplication performing as normal
  res <- dedup_citations(test_citations, merge_citations = FALSE, manual_dedup = FALSE)

  expect_equal(
    length(res$duplicate_id), 586)

  # check deduplication with merge performing as normal
  res <- dedup_citations(test_citations, merge_citations=TRUE)

  expect_equal(
  length(res$unique$duplicate_id), 586)
})
