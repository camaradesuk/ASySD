####------ Deduplicate citations WITH manual dups added function ------ ####
#'
#' This function performs additional deduplication with the additional of manually flagged duplicates
#' @export
#' @import dplyr
#' @param unique_citations A dataframe containing citations after automated deduplication
#' @param merge_citations Logical value. Do you want to merge matching citations?
#' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
#' @param keep_label Selected citation label to preferentially retain in the dataset as the unique record
#' @param additional_pairs dataframe of citations with manual pairs, a subset of the manual pairs export
#' @param extra_merge_fields Add additional fields to merge, output will be similar to the label, source, and record_id columns with commas between each merged value
#' @param show_unknown_tags When a label, source, or other merged field is missing, do you want this to show as "unknown"?
#' @return Unique citations post manual deduplication
#' @examples
#'
#' # Perform deduplication
#' result <- dedup_citations(citations_df, keep_source="Embase")
#'
#' # View unique citations
#' res_unique <- result$unique
#' head(result$manual_dedup)
#'
#'true_dups <- result$manual_dedup[1:5,]
#'
#'final_result <- dedup_citations_add_manual(res_unique, additional_pairs = true_dups)
#'
dedup_citations_add_manual <- function(unique_citations, merge_citations=TRUE, keep_source=NULL, keep_label=NULL,
                                       additional_pairs, extra_merge_fields = NULL, show_unknown_tags=TRUE){

  # if extra merge fields not null
  if(!is.null(extra_merge_fields)){

    extra_cols <- paste(extra_merge_fields, 1:2, sep = "")

    duplicates <- additional_pairs %>%
      dplyr::select(duplicate_id.x, duplicate_id.y, label1, label2, source1, source2, !!!extra_cols)
  } else{

    duplicates <- additional_pairs %>%
      dplyr::select(duplicate_id.x, duplicate_id.y, label1, label2, source1, source2)
  }

    unique_citations <- unique_citations %>%
      rename(record_id = duplicate_id)

    res <- generate_dup_id(additional_pairs, unique_citations, keep_source, keep_label)
    unique_citations_with_metadata <- merge_metadata(res, extra_merge_fields)

    return(unique_citations_with_metadata)
  }
