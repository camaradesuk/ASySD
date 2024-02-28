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

dedup_citations_add_manual <- function(unique_citations, merge_citations=TRUE, keep_source=NULL, keep_label=NULL,
                                       additional_pairs, extra_merge_fields = NULL, show_unknown_tags=TRUE){

  # if extra merge fields not null
  if(!is.null(extra_merge_fields)){

    extra_cols <- paste(extra_merge_fields, 1:2, sep = "")

    duplicates <- additional_pairs %>%
      dplyr::select(duplicate_id.x, duplicate_id.y, label1, label2, source1, source2, !!!extra_cols)


    if (!('record_ids' %in% colnames(unique_citations))) {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_id = ifelse(is.na(.data$duplicate_id.y), .data$record_id, paste0(.data$record_id, ", ", .data$duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(.data$duplicate_id.y), .data$label, paste0(.data$label, ", ", .data$label2))) %>%
        mutate(source = ifelse(is.na(.data$duplicate_id.y), .data$source, paste0(.data$source, ", ", .data$source2))) %>%
        mutate(across(all_of({{extra_merge_fields}}), ~ifelse(is.na(duplicate_id.y), .x, paste0(.x, ", ", get(paste0(cur_column(), 2))))))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, {{extra_merge_fields}}, sep=", ") %>%
        select(-duplicate_id.y, -!!extra_cols) %>%
        unique() %>%
        ungroup() %>%
        group_by(.data$record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations <- unique_citations %>%
        select(-duplicate_id)

      unique_citations_with_metadata <- keep_one_unique_citation(ids)

    } else {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_ids = ifelse(is.na(duplicate_id.y), record_ids, paste0(record_ids, ", ", duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(duplicate_id.y), label, paste0(label, ", ", label2))) %>%
        mutate(source = ifelse(is.na(duplicate_id.y), source, paste0(source, ", ", source2))) %>%
        mutate(across(all_of({{extra_merge_fields}}), ~ifelse(is.na(duplicate_id.y), .x, paste0(.x, ", ", get(paste0(cur_column(), 2))))))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, {{extra_merge_fields}}, sep=", ") %>%
        rename(record_id = record_ids) %>%
        select(-duplicate_id.y, -!!extra_cols) %>%
        unique() %>%
        ungroup() %>%
        group_by(record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations_with_metadata <- merge_metadata(ids, extra_merge_fields)

    }

    unique_citations_with_metadata <-unique_citations_with_metadata %>%
      select(-c(source1, label1, source2, label2))

    return(unique_citations_with_metadata)

  }  else {

    duplicates <- additional_pairs %>%
      dplyr::select(duplicate_id.x, duplicate_id.y, label1, label2, source1, source2)

    if (!('record_ids' %in% colnames(unique_citations))) {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_id = ifelse(is.na(.data$duplicate_id.y), .data$record_id, paste0(.data$record_id, ", ", .data$duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(.data$duplicate_id.y), .data$label, paste0(.data$label, ", ", .data$label2))) %>%
        mutate(source = ifelse(is.na(.data$duplicate_id.y), .data$source, paste0(.data$source, ", ", .data$source2)))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, sep=", ") %>%
        select(-duplicate_id.y) %>%
        unique() %>%
        ungroup() %>%
        group_by(.data$record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations <- unique_citations %>%
        select(-duplicate_id)

      unique_citations_with_metadata <- keep_one_unique_citation(ids)

    } else {

      unique_citations <- unique_citations %>%
        left_join(duplicates, by=c("duplicate_id" = "duplicate_id.x")) %>%
        mutate(record_ids = ifelse(is.na(duplicate_id.y), record_ids, paste0(record_ids, ", ", duplicate_id.y))) %>%
        mutate(label = ifelse(is.na(duplicate_id.y), label, paste0(label, ", ", label2))) %>%
        mutate(source = ifelse(is.na(duplicate_id.y), source, paste0(source, ", ", source2)))

      unique_citations <- unique_citations %>%
        group_by(.data$duplicate_id) %>%
        tidyr::separate_rows(record_ids, source, label, sep=", ") %>%
        rename(record_id = record_ids) %>%
        select(-duplicate_id.y) %>%
        unique() %>%
        ungroup() %>%
        group_by(record_id) %>%
        slice_head() %>%
        ungroup()

      ids <- unique_citations

      unique_citations_with_metadata <- merge_metadata(ids, extra_merge_fields)

    }

    unique_citations_with_metadata <-unique_citations_with_metadata %>%
      select(-c(source1, label1, source2, label2))

    return(unique_citations_with_metadata)
  }

}

