calculate_performance_1 <- function(data){

  asysd_result <- dedup_citations(data, keep_label = "Unique", merge_citations = FALSE)
  unique <- asysd_result$unique

  unique_to_match <- unique %>%
    rename(asysd_label = label) %>%
    mutate(asysd_label = "Unique") %>%
    select(asysd_label, record_id)

  check <- left_join(data, unique_to_match, by="record_id")
  check <- check %>%
    mutate(asysd_label = ifelse(is.na(asysd_label), "Duplicate", paste(asysd_label)))

  summary <- check %>%
    ungroup() %>%
    count(label, asysd_label)

  true_neg <- as.numeric(summary$n[which(summary$label == "Unique" & summary$asysd_label == "Unique")])
  false_neg <- as.numeric(summary$n[which(summary$label == "Duplicate" & summary$asysd_label == "Unique")])
  true_pos <- as.numeric(summary$n[which(summary$label == "Duplicate" & summary$asysd_label == "Duplicate")])
  false_pos <- as.numeric(summary$n[which(summary$label == "Unique" & summary$asysd_label == "Duplicate")])

  if(length(false_neg) == 0){
    false_neg <- 0
  }else{
    false_neg <- false_neg
  }

  if(length(false_pos) == 0){
    false_pos <- 0
    }else{
      false_pos <- false_pos
  }

  sensitivity <- true_pos / (true_pos + false_neg) * 100
  specificity <- true_neg / (true_neg + false_pos) * 100

  data.frame(Measure= c("True_neg", "True_pos", "False_neg", "False_pos", "Sensitivity", "Specificity"),
   Result = c(true_neg, true_pos, false_neg, false_pos, sensitivity, specificity))

  }

calculate_performance_2 <- function(data){

  asysd_result <- dedup_citations(data, keep_label = "Unique", merge_citations = TRUE)
  unique <- asysd_result$unique

  unique_to_match <- unique %>%
    rename(asysd_label = label) %>%
    mutate(asysd_label = "Unique") %>%
    select(asysd_label, duplicate_id)

  check <- left_join(data, unique_to_match, by=c("record_id"="duplicate_id"))

  check <- check %>%
    mutate(asysd_label = ifelse(is.na(asysd_label), "Duplicate", paste(asysd_label)))

  summary <- check %>%
    ungroup() %>%
    count(label, asysd_label)

  true_neg <- as.numeric(summary$n[which(summary$label == "Unique" & summary$asysd_label == "Unique")])
  false_neg <- as.numeric(summary$n[which(summary$label == "Duplicate" & summary$asysd_label == "Unique")])
  true_pos <- as.numeric(summary$n[which(summary$label == "Duplicate" & summary$asysd_label == "Duplicate")])
  false_pos <- as.numeric(summary$n[which(summary$label == "Unique" & summary$asysd_label == "Duplicate")])

  if(length(false_neg) == 0){
    false_neg <- 0

  } else{
    false_neg <- false_neg
  }

  if(length(false_pos) == 0){
    false_pos <- 0
  }else{
    false_pos <- false_pos
  }

  sensitivity <- true_pos / (true_pos + false_neg) * 100
  specificity <- true_neg / (true_neg + false_pos) * 100

  data.frame(Measure= c("True_neg", "True_pos", "False_neg", "False_pos", "Sensitivity", "Specificity"),
             Result = c(true_neg, true_pos, false_neg, false_pos, sensitivity, specificity))

}
