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


  # check deduplication performing as normal
  res <- dedup_citations(test_citations, merge_citations = FALSE)

  expect_equal(
    length(res$unique$duplicate_id), 586)

  # check deduplication with merge performing as normal
  res <- dedup_citations(test_citations, merge_citations=TRUE)

  expect_equal(
  length(res$unique$duplicate_id), 586)
})
