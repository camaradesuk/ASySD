# labelled_test_set <- read_csv("labelled_test_set.csv")
#
# unique_merged <- dedup_citations(labelled_test_set, merge_citations = TRUE)
# unique_merged <- unique_merged$unique
#
# unique_pick_one <- dedup_citations(labelled_test_set, merge_citations = FALSE)
# unique_pick_one <- unique_pick_one$unique
#
# unique_pick_one_labelled <- dedup_citations(labelled_test_set,
#                                             merge_citations = FALSE,
#                                             preferred_source = "unique")
# unique_pick_one_labelled <- unique_pick_one_labelled$unique
#
# labelled_test_set$record_id <- NA
# unique_merged <- dedup_citations(labelled_test_set, merge_citations = TRUE)
