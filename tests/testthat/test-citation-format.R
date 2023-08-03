test_that("citations_check", {

  # get testing set
  test_citations <- read.csv("labelled_test_set.csv")
  test_citations <- as.data.frame(test_citations)

  # create set with no record_id col
  test_citations_no_col_for_id <- test_citations %>%
    select(-record_id)

  # create set with all blank record_id col
  test_citations_no_record_id <- test_citations
  test_citations_no_record_id$record_id <- ""

  # create set with all NA record_id col
  test_citations_na_record_id <- test_citations
  test_citations_na_record_id$record_id <- NA

  # create set with non unique record_id col
  test_citations_non_unique <- test_citations
  test_citations_non_unique$record_id <- rep(c(1:10), times=round(nrow(test_citations)), length.out = nrow(test_citations))

  # create set with some missing record_id
  test_citations_missing <- test_citations
  test_citations_missing$record_id <- rep(c(1, 2, 3, 4, NA), times=round(nrow(test_citations)), length.out = nrow(test_citations))

  expect_warning(
     dedup_citations(test_citations_no_col_for_id, merge_citations = TRUE), "Search does not contain a record_id column. A record_id will be created using row numbers")

  expect_warning(
      dedup_citations(test_citations_no_record_id, merge_citations = TRUE),"Search contains missing values for the record_id column. A record_id will be created using row numbers")

  expect_warning(
      dedup_citations(test_citations_na_record_id, merge_citations = TRUE), "Search contains missing values for the record_id column. A record_id will be created using row numbers")

  expect_warning(
      dedup_citations(test_citations_non_unique, merge_citations = TRUE), "The record_id column is not unique. A record_id will be created using row numbers")

  expect_warning(
      dedup_citations(test_citations_missing, merge_citations = TRUE), "Search contains missing values for the record_id column. A record_id will be created using row numbers")

  # check formatting citations doesn't drop rows
  expect_equal(nrow(ASySD:::format_citations(test_citations)), nrow(test_citations))
})


