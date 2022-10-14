test_that("dedup_check", {

  # get testing set
  test_citations <- read.csv("labelled_test_set.csv")
  test_citations <- as.data.frame(test_citations)

  # when no dups, check warning works
  test_citations_nodups <- test_citations[c(6,21,65),]

  expect_warning(
    a <- dedup_citations(test_citations_nodups, merge_citations = TRUE), "No duplicates detected!")

})
