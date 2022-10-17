####------ Assign id ------ ####

#' This function adds an id to citation data if missing
#' @param raw_citations Citation dataframe with relevant columns
#' @return Dataframe of citations with id
#' @import dplyr
add_id_citations <- function(raw_citations){

  # add record id from row number if missing
  raw_citations <- raw_citations %>%
    mutate(record_id =  as.character(row_number()+1000))

}

#' This function orders citation data for deduplication
#' @param raw_citations Citation dataframe with relevant columns and id column
#' @return Dataframe of ordered citations with id
#' @import dplyr
#' @import utf8
order_citations <- function(raw_citations){
# arrange by Year and presence of an Abstract - we want to keep newer records and records with an abstract preferentially

ordered_citations <- raw_citations %>%
  arrange(desc(year), abstract) %>%
  dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8

# select relevant columns
ordered_citations <- ordered_citations  %>%
  select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label, source)

return(ordered_citations)
}

####------ Format citation data ------ ####

#' This function formats citation data for deduplication
#' @param raw_citations Citation dataframe with relevant columns and id column
#' @return Dataframe of formatted citations with id
#' @import dplyr
format_citations <- function(raw_citations){

  # make sure author is a character
  raw_citations$author <- as.character(raw_citations$author)

  # Fix author formatting so similar
  raw_citations <- raw_citations %>%
    mutate(author = ifelse(author=="", "Unknown", author)) %>%
    mutate(author = ifelse(is.na(author), "Unknown", author)) %>%
    mutate(author = ifelse(author=="Anonymous", "Unknown", author)) %>%
    dplyr::mutate_if(is.character, utf8::utf8_encode) # make sure utf8


  # Make all upper case
  formatted_citations <- as.data.frame(sapply(raw_citations, toupper))

  # get rid of punctuation and differnces in doi formatting
  formatted_citations["doi"] <- sapply(formatted_citations["doi"], function(x) gsub("%28", "(", x))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("%29", ")", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTP://DX.DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTPS://DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTPS://DX.DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("HTTP://DOI.ORG/", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI: ", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI:", "", x)))
  formatted_citations["doi"] <- as.data.frame(sapply(formatted_citations["doi"], function(x) gsub("DOI", "", x)))

  formatted_citations["title"] <- as.data.frame(sapply(formatted_citations["title"], function(x) gsub("[[:punct:]]", "", x)))
  formatted_citations["year"] <- as.data.frame(sapply(formatted_citations["year"], function(x) gsub("[[:punct:]]", "", x)))
  formatted_citations["abstract"] <- as.data.frame(sapply(formatted_citations["abstract"], function(x) gsub("[[:punct:]]", "", x)))

  formatted_citations["isbn"] <- as.data.frame(sapply(formatted_citations["isbn"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  formatted_citations["isbn"] <- as.data.frame(sapply(formatted_citations["isbn"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  formatted_citations<-formatted_citations %>%
    filter(!is.na(record_id))

  # sort out NA / missing data formatting for optimal matching
  formatted_citations <- formatted_citations %>%
    mutate(author = ifelse(author=="NA", NA, paste(author))) %>%
    mutate(year = ifelse(year=="NA", NA, paste(year))) %>%
    mutate(title = ifelse(title=="NA", NA, paste(title))) %>%
    mutate(number = ifelse(number=="NA", NA, paste(number))) %>%
    mutate(volume = ifelse(volume=="NA", NA, paste(volume))) %>%
    mutate(pages = ifelse(pages=="NA", NA, paste(pages))) %>%
    mutate(abstract = ifelse(abstract=="NA", NA, paste(abstract))) %>%
    mutate(doi = ifelse(doi=="NA", NA, paste(doi))) %>%
    mutate(journal = ifelse(journal=="NA", NA, paste(journal))) %>%
    mutate(isbn = ifelse(isbn=="", NA, paste(isbn)))

  formatted_citations<- formatted_citations %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, source, label)

  return(formatted_citations)

}

####------ Identify all possible matching pairs of citations ------ ####

#' This function identifies matching pairs of citations
#' @param formatted_citations Formatted citation dataframe with relevant columns and id column
#' @return Dataframe of citation pairs
#' @import RecordLinkage
#' @import parallel
match_citations <- function(formatted_citations){

  # ROUND 1: run compare.dedup function and block by title&pages OR title&author OR title&abstract OR doi
  try(newpairs <- compare.dedup(formatted_citations, blockfld = list(c(2,8), c(1,2), c(2,5), 6), exclude=c("record_id", "source", "label")), silent=TRUE)

  # Create df of pairs
  try(linkedpairs <- as.data.frame(newpairs$pairs), silent=TRUE)

  # ROUND 2: run compare.dedup function and block by author&year&pages OR journal&volume&pages or isbn&volume&pages OR title&isbn
  try(newpairs2 <- compare.dedup(formatted_citations, blockfld = list(c(1,3,8), c(4,9,8), c(10,9,8), c(2,10)), exclude= c("record_id", "source", "label")), silent=TRUE)

  #Create df of pairs
  try(linkedpairs2 <- as.data.frame(newpairs2$pairs), silent=TRUE)

  # ROUND 3: run compare.dedup function and block by year&pages&volume OR year&number&volume or year&pages&number
  try(newpairs3 <- compare.dedup(formatted_citations, blockfld = list(c(3,8,9), c(3,7,9), c(3,8,7)), exclude=c("record_id", "source", "label")), silent = TRUE)

  #Create df of pairs
  try(linkedpairs3 <- as.data.frame(newpairs3$pairs), silent=TRUE)

  # ROUND 4: run compare.dedup function and block by author&year OR year&title OR title&volume OR title&journal
  try(newpairs4 <- compare.dedup(formatted_citations, blockfld = list(c(1,3), c(3,2),c(2,9), c(2,4)), exclude=c("record_id", "source", "label")), silent = TRUE)

  # Create df of pairs
  try(linkedpairs4 <- as.data.frame(newpairs4$pairs), silent=TRUE)

  # Combine all possible pairs
  pairs <- rbind(get0("linkedpairs"),
                 get0("linkedpairs2"),
                 get0("linkedpairs3"),
                 get0("linkedpairs4"))

  pairs <- unique(pairs)

  if(is.null(pairs)){

    return()
  }

  # Obtain metadata for matching pairs
  pairs <- pairs  %>%
    mutate(author1 =formatted_citations$author[id1]) %>%
    mutate(author2 =formatted_citations$author[id2]) %>%
    mutate(title1 =formatted_citations$title[id1]) %>%
    mutate(title2 =formatted_citations$title[id2]) %>%
    mutate(abstract1 =formatted_citations$abstract[id1]) %>%
    mutate(abstract2 =formatted_citations$abstract[id2]) %>%
    mutate(doi1= formatted_citations$doi[id1]) %>%
    mutate(doi2 =formatted_citations$doi[id2]) %>%
    mutate(year1=formatted_citations$year[id1]) %>%
    mutate(year2=formatted_citations$year[id2]) %>%
    mutate(number1 =formatted_citations$number[id1]) %>%
    mutate(number2 =formatted_citations$number[id2]) %>%
    mutate(pages1 =formatted_citations$pages[id1]) %>%
    mutate(pages2 =formatted_citations$pages[id2]) %>%
    mutate(volume1 =formatted_citations$volume[id1]) %>%
    mutate(volume2 =formatted_citations$volume[id2]) %>%
    mutate(journal1 =formatted_citations$journal[id1]) %>%
    mutate(journal2 =formatted_citations$journal[id2]) %>%
    mutate(isbn1 =formatted_citations$isbn[id1]) %>%
    mutate(isbn2 =formatted_citations$isbn[id2]) %>%
    mutate(record_id1=formatted_citations$record_id[id1]) %>%
    mutate(record_id2 =formatted_citations$record_id[id2]) %>%
    mutate(label1 =formatted_citations$label[id1]) %>%
    mutate(label2 =formatted_citations$label[id2]) %>%
    mutate(source1 =formatted_citations$source[id1]) %>%
    mutate(source2 =formatted_citations$source[id2])

  pairs <- pairs %>%
    select(id1, id2, author1, author2, author, title1, title2, title, abstract1, abstract2, abstract, year1, year2, year, number1, number2, number, pages1, pages2, pages, volume1, volume2, volume, journal1, journal2, journal, isbn, isbn1, isbn2, doi1, doi2, doi, record_id1, record_id2, label1, label2, source1, source2)

  numCores <- parallel::detectCores()
  numCores

  try(pairs$author <- mapply(jarowinkler, pairs$author1, pairs$author2), silent = TRUE)
  try(pairs$title <- mapply(jarowinkler, pairs$title1, pairs$title2), silent = TRUE)
  try(pairs$abstract <- parallel::mcmapply(jarowinkler, pairs$abstract1, pairs$abstract2, mc.cores = numCores), silent = TRUE)
  try(pairs$year <- mapply(jarowinkler, pairs$year1, pairs$year2), silent = TRUE)
  try(pairs$number <- mapply(jarowinkler, pairs$number1, pairs$number2), silent = TRUE)
  try(pairs$volume <- mapply(jarowinkler, pairs$volume1, pairs$volume2), silent = TRUE)
  try(pairs$journal <- mapply(jarowinkler, pairs$journal1, pairs$journal2), silent = TRUE)
  try(pairs$isbn <- mapply(jarowinkler, pairs$isbn1, pairs$isbn2), silent = TRUE)
  try(pairs$doi <- mapply(jarowinkler, pairs$doi1, pairs$doi2), silent = TRUE)

  pairs <- pairs %>%
    mutate(abstract = ifelse(is.na(abstract1) & is.na(abstract2), 0, abstract)) %>%
    mutate(pages = ifelse(is.na(pages1) & is.na(pages2), 1, pages)) %>%
    mutate(volume = ifelse(is.na(volume1) & is.na(volume2), 1, volume)) %>%
    mutate(number = ifelse(is.na(number1) & is.na(number2), 1, number)) %>%
    mutate(doi = ifelse(is.na(doi1) & is.na(doi2), 0, doi)) %>%
    mutate(isbn = ifelse(is.na(isbn1) & is.na(isbn2), 0, isbn))

}


#' This function identifies true pairs from matching pairs of citations and pairs which may be duplicates - for manual deduplication
#' @param pairs citation matches which may be duplicates
#' @return Dataframe of true citation pairs
identify_true_matches <- function(pairs){

  ####------ Filter matching pairs - retain correct matches ------ ####
  true_pairs <- pairs %>%
    filter(
      (pages>0.8 & volume>0.8 & title>0.90 & abstract>0.90 & author>0.50 & isbn>0.99) |
        (pages>0.8 & volume>0.8 & title>0.90 & abstract>0.90 & author>0.50 & journal>0.6) |
        (pages>0.8 & number>0.8 & title>0.90 & abstract>0.90 & author>0.50 & journal>0.6) |
        (volume >0.8 & number>0.8 & title>0.90 & abstract>0.90 & author>0.50  & journal>0.6) |

        (volume >0.8 & number>0.8 & title>0.90 & abstract>0.90 & author>0.8) |
        (volume>0.8 & pages>0.8 & title>0.90 & abstract>0.9 & author>0.8) |
        (pages>0.8 & number>0.8 & title>0.90 & abstract>0.9 & author>0.8) |

        (doi>0.95 & author>0.75 & title>0.9) |

        (title>0.80 & abstract>0.90 & volume>0.85 & journal>0.65 & author>0.9) |
        (title>0.90 & abstract>0.80 & volume>0.85 & journal>0.65 & author>0.9)|

        (pages>0.8 & volume>0.8 & title>0.90 & abstract>0.8 & author>0.9 & journal>0.75) |
        (pages>0.8 & number>0.8 & title>0.90 & abstract>0.80 & author>0.9 & journal>0.75) |
        (volume>0.8 & number>0.8 & title>0.90 & abstract>0.8 & author>0.9  & journal>0.75) |

        (title>0.9 & author>0.9 & abstract>0.9 & journal >0.7)|
        (title>0.9 & author>0.9 & abstract>0.9 & isbn >0.99)|

        (pages>0.9 & number>0.9 & title>0.90 & author>0.80 & journal>0.6) |
        (number>0.9 & volume>0.9 & title>0.90 & author>0.90 & journal>0.6) |
        (pages>0.9 & volume>0.9 & title>0.90 & author>0.80 & journal>0.6) |
        (pages>0.9 & number>0.9 & title>0.90 & author>0.80 & isbn>0.99) |
        (pages>0.9 & number>0.9 & title>0.90 & author>0.80 & isbn>0.99) |
        (pages>0.9 & number>0.9 & title>0.90 & author>0.80 & isbn>0.99) |

        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & journal>0.9) |
        (number>0.8 & volume>0.8 & title>0.95 & author>0.80 & journal>0.9)|
        (number>0.8 & pages>0.8 & title>0.95 & author>0.80 & journal>0.9) |
        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & isbn>0.99) |
        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & isbn>0.99) |
        (pages>0.8 & volume>0.8 & title>0.95 & author>0.80 & isbn>0.99))

  # Find papers with low matching dois - often indicates FALSE positive matches
  true_pairs_mismatch_doi <- true_pairs %>%
    filter(!(is.na(doi)| doi ==0 | doi > 0.99)) %>%
    filter(!(title > 0.9 & abstract > 0.9 & (journal|isbn > 0.9)))

  # Remove papers with low matching dois from filtered matched
  true_pairs <- true_pairs %>%
    filter(is.na(doi)| doi > 0.99 | doi == 0 | (title > 0.9 & abstract>0.9 & (journal|isbn > 0.9)))

  true_pairs <- unique(true_pairs)

  # Make year numeric, then find matches where year differs
  true_pairs$year1 <- as.numeric(as.character(true_pairs$year1))
  true_pairs$year2 <- as.numeric(as.character(true_pairs$year2))
  year_mismatch <- true_pairs[which(true_pairs$year1 != true_pairs$year2),]
  year_mismatch_minor1 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2+1 ),]
  year_mismatch_minor2 <- year_mismatch[which(year_mismatch$year1 == year_mismatch$year2-1 ),]

  year_mismatch_minor <- rbind(year_mismatch_minor1,year_mismatch_minor2)
  year_mismatch_minor <- unique(year_mismatch_minor)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  year_mismatch_major <- year_mismatch[which(!rownames(year_mismatch) %in% rownames(year_mismatch_minor)),]

  true_pairs <- true_pairs[which(!rownames(true_pairs) %in% rownames(year_mismatch_major)),]

  true_pairs <- unique(true_pairs)

  true_pairs$record_id1 <- as.character(true_pairs$record_id1)
  true_pairs$record_id2 <- as.character(true_pairs$record_id2)

  # Get potential duplicates for manual deduplication
  maybe_pairs <- pairs %>%
    filter(doi > 0.99 & title > 0.8 |
             title>0.85 & author>0.75 |
             title>0.80 & abstract>0.80 |
             title>0.80 & isbn>0.99 |
             title>0.80 & journal>0.80) %>%
    filter(doi > 0.95 | doi == 0 | is.na(doi)) %>%
    filter(!(as.numeric(year1) - as.numeric(year2) >1)) %>%
    filter(!(as.numeric(year2) - as.numeric(year1) >1))

  # get pairs required for manual dedup which are not in true pairs
  maybe_pairs <- anti_join(maybe_pairs, true_pairs, by = c("record_id1", "record_id2"))

  # Add in problem doi matching pairs and different year data in ManualDedup
  important_mismatch <- rbind(true_pairs_mismatch_doi, year_mismatch_major)
  maybe_pairs <- rbind(maybe_pairs, important_mismatch)
  maybe_pairs <- unique(maybe_pairs)

  true_pairs <- true_pairs %>%
    select(author1, author2, title1, title2, year1, year2, journal1, journal2, doi1, doi2,
           record_id1, record_id2)

  return(list("true_pairs" = true_pairs,
              "maybe_pairs" = maybe_pairs))

}

#' This function generates a duplicate ID for sets of matching citations
#' @param true_pairs citation matches which are true duplicates'
#' @param formatted_citations formatted citation data
#' @return Dataframe of formatted citation data with duplicate id
#' @import dplyr
generate_dup_id <- function(true_pairs, formatted_citations){

  # generate duplicate IDs
  dup_ids <- true_pairs %>%
    group_by(record_id1) %>%
    mutate(duplicate_id = first(record_id1)) %>%
    ungroup() %>%
    group_by(record_id2) %>%
    mutate(duplicate_id = first(duplicate_id)) %>%
    ungroup() %>%
    select(record_id1, record_id2, duplicate_id) %>%
    unique()

  citations_with_dup_id1 <- inner_join(formatted_citations, dup_ids, by=c("record_id"="record_id1")) %>%
    select(-record_id2) %>%
    unique()

  citations_with_dup_id2 <- inner_join(formatted_citations, dup_ids, by=c("record_id"="record_id2")) %>%
    select(-record_id1) %>%
    unique()

  citations_with_dup_id <- rbind(citations_with_dup_id1, citations_with_dup_id2) %>% unique()

  unique_citations <- formatted_citations %>%
    filter(!record_id %in% citations_with_dup_id$record_id) %>%
    mutate(duplicate_id = record_id)

  citations_with_dup_id <- rbind(unique_citations, citations_with_dup_id)
  matched_pairs_with_ids <- unique(citations_with_dup_id)

  # extra merging is required
  # record A = record B, record B = record C, BUT if no link to indicate A=C, need to ensure that A,B,C are all part of the same group (gets complicated quickly)
  matched_pairs_with_ids <- matched_pairs_with_ids %>%
    group_by(record_id) %>%
    arrange(duplicate_id) %>%
    add_count() # get count of duplicate ids assigned to a single record ID (happens when A = B, A = C, A = D for example, duplicate ID for A could be both D and B

  matched_pairs_with_ids <- matched_pairs_with_ids %>%
    mutate(duplicate_id = ifelse(n>1, first(duplicate_id), paste(duplicate_id))) %>% #when more than 1 duplicate id for one record id, make duplicate ID the FIRST one.
    mutate(duplicate_id = ifelse(n==1 & duplicate_id %in% matched_pairs_with_ids$record_id,
                                 paste(matched_pairs_with_ids$duplicate_id[which(matched_pairs_with_ids$record_id == duplicate_id)]),
                                 paste0(duplicate_id))) %>%
    ungroup()

  return(matched_pairs_with_ids)

}

# Remove duplicate papers ----------------------------------------------
#' This function retains one citation in a set of matching records
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @param raw_citations original citation data with ids
#' @return Dataframe of citation data with duplicate citation rows removed
#' @import dplyr
keep_one_unique_citation <- function(raw_citations, matched_pairs_with_ids, keep_source, keep_label){

  duplicate_id <- matched_pairs_with_ids %>%
    select(duplicate_id, record_id) %>%
    unique()

  raw_citations <- raw_citations %>%
    mutate(record_id = as.character(record_id))

  all_metadata_with_duplicate_id <- left_join(duplicate_id, raw_citations)

  if(!is.null(keep_source)){

  citations_with_dup_id_pick <- all_metadata_with_duplicate_id %>%
    mutate_all(~replace(., .=='NA', NA)) %>%
    group_by(duplicate_id) %>%
    arrange(doi, abstract) %>%
    mutate(Order = ifelse(source == keep_source, 1, 2)) %>%
    arrange(Order) %>%
    select(-Order) %>%
    slice_head()
  }

  else if(!is.null(keep_label)){

    citations_with_dup_id_pick <- all_metadata_with_duplicate_id %>%
      mutate_all(~replace(., .=='NA', NA)) %>%
      group_by(duplicate_id) %>%
      arrange(doi, abstract) %>%
      mutate(Order = ifelse(label == keep_label, 1, 2)) %>%
      arrange(Order) %>%
      select(-Order) %>%
      slice_head()
  }

  else{
    citations_with_dup_id_pick <- all_metadata_with_duplicate_id %>%
      mutate_all(~replace(., .=='NA', NA)) %>%
      group_by(duplicate_id) %>%
      arrange(doi, abstract) %>%
      slice_head()

  }
}

#' This function generates a duplicate ID for sets of matching citations
#' @param matched_pairs_with_ids citation data with duplicate ids
#' @param raw_citations  original citation data with unique ids
#' @return Dataframe of formatted citation data with duplicate id
#' @import dplyr
  merge_metadata <- function(raw_citations, matched_pairs_with_ids){

    # get df of duplicate ids and record ids
    duplicate_id <- matched_pairs_with_ids %>%
      select(duplicate_id, record_id) %>%
      unique()

    raw_citations <- raw_citations %>%
      mutate(record_id = as.character(record_id))

    # make character
    duplicate_id$record_id <- as.character(duplicate_id$record_id)

    # join duplicate id to raw citation metadata (e.g. title, author, journal)
    all_metadata_with_duplicate_id <- left_join(duplicate_id, raw_citations, by="record_id")

    all_metadata_with_duplicate_id <- all_metadata_with_duplicate_id %>%
      mutate_if(is.character, utf8::utf8_encode) %>% # ensure all utf8
      mutate_all(~replace(., .=='NA', NA)) %>% #replace NA
      group_by(duplicate_id) %>% # group by duplicate id
      summarise(across(everything(), ~trimws(paste(na.omit(.), collapse = ';;;')))) %>% #merge all rows with same dup id, dont merge NA values
      mutate(across(c(everything(), -label, -source, -record_id), gsub, pattern = ";;;.*", replacement = "")) %>% #remove extra values in each col, keep first one only
      mutate(across(label, gsub, pattern = ";;;", replacement = ", ")) %>%
      mutate(across(source, gsub, pattern = ";;;", replacement = ", ")) %>%
      mutate(across(record_id, gsub, pattern = ";;;", replacement = ", ")) %>% #replace separator to comma
      ungroup() %>%
      mutate(record_ids = record_id) %>%
      select(-record_id) %>%
      ungroup()


  }

  #' Deduplicate citations
  #'
  #' This function deduplicates citation data
  #' @export
  #' @import dplyr
  #' @param raw_citations A dataframe containing duplicate ciations
  #' @param manual_dedup Logical value. Do you want to retrieve dataframe for manual deduplication?
  #' @param merge_citations Logical value. Do you want to merge matching citations?
  #' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
  #' @param keep_source Selected citation label to preferentially retain in the dataset as the unique record
  #' @return A list of 2 dataframes - unique citations and citations to be manually deduplicated if option selected
  #' @examples
  #'
  #' result <- dedup_citations(data, keep_source = "pubmed")
  #' unique_citations <- result$unique

  dedup_citations <- function(raw_citations, manual_dedup = TRUE,
                              merge_citations=FALSE, keep_source=NULL, keep_label=NULL) {

    print("formatting data...")

    # add warning for no record id
    if(!"record_id" %in% names(raw_citations)){
      warning("Search does not contain a record_id column. A record_id will be created using row names")

       # add record id using row number
       raw_citations <- add_id_citations(raw_citations)
    }

    # add warning for any missing record id
    else if(any(is.na(raw_citations$record_id) | raw_citations$record_id=="")){
    warning("Search contains missing values for the record_id column. A record_id will be created using row names")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)
    }

    # add warning for non unique ids
    else if(length(unique(raw_citations$record_id)) != nrow(raw_citations)){
    warning("The record_id column is not unique. A record_id will be created using row names")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)
    }

    ordered_citations <- order_citations(raw_citations)
    formatted_citations <- format_citations(ordered_citations)

    print("identifying potential duplicates...")

    # find matching pairs
    pairs <- match_citations(formatted_citations)

    # warning if no duplicates
    if(is.null(pairs)) {
      warning("No duplicates detected!")
      return(raw_citations)
    }

    pair_types <- identify_true_matches(pairs)
    true_pairs <- pair_types$true_pairs

    # warning if no duplicates
    if(is.null(true_pairs)) {
      warning("No duplicates detected!")
      return(raw_citations)
    }

    print("identified duplicates!")

    matched_pairs_with_ids <- generate_dup_id(true_pairs, formatted_citations)

      if(manual_dedup == TRUE){

        print("flagging potential pairs for manual dedup...")

        maybe_pairs <- pair_types$maybe_pairs

        maybe_pairs <- maybe_pairs  %>%
          mutate(author1 =ordered_citations$author[id1]) %>%
          mutate(author2 =ordered_citations$author[id2]) %>%
          mutate(title1 =ordered_citations$title[id1]) %>%
          mutate(title2 =ordered_citations$title[id2]) %>%
          mutate(abstract1 =ordered_citations$abstract[id1]) %>%
          mutate(abstract2 =ordered_citations$abstract[id2]) %>%
          mutate(doi1= ordered_citations$doi[id1]) %>%
          mutate(doi2 =ordered_citations$doi[id2]) %>%
          mutate(year1=ordered_citations$year[id1]) %>%
          mutate(year2=ordered_citations$year[id2]) %>%
          mutate(number1 =ordered_citations$number[id1]) %>%
          mutate(number2 =ordered_citations$number[id2]) %>%
          mutate(pages1 =ordered_citations$pages[id1]) %>%
          mutate(pages2 =ordered_citations$pages[id2]) %>%
          mutate(volume1 =ordered_citations$volume[id1]) %>%
          mutate(volume2 =ordered_citations$volume[id2]) %>%
          mutate(journal1 =ordered_citations$journal[id1]) %>%
          mutate(journal2 =ordered_citations$journal[id2]) %>%
          mutate(isbn1 =ordered_citations$isbn[id1]) %>%
          mutate(isbn2 =ordered_citations$isbn[id2]) %>%
          mutate(record_id1=ordered_citations$record_id[id1]) %>%
          mutate(record_id2 =ordered_citations$record_id[id2]) %>%
          mutate(label1 =ordered_citations$label[id1]) %>%
          mutate(label2 =ordered_citations$label[id2]) %>%
          mutate(source1 =ordered_citations$source[id1]) %>%
          mutate(source2 =ordered_citations$source[id2]) %>%
          select(author1, author2, author, title1,
                 title2, title, abstract1, abstract2, abstract, year1,
                 year2, year, number1, number2, number, pages1, pages2,
                 pages, volume1, volume2, volume, journal1, journal2,
                 journal, isbn, isbn1, isbn2, doi1, doi2, doi,
                 record_id1, record_id2, label1,
                 label2, source1, source2)

        manual_dedup <- maybe_pairs
      }

      print("merging citations...")


    if(merge_citations == TRUE){

      unique_citations_with_metadata <- merge_metadata(raw_citations, matched_pairs_with_ids)
    } else{
      unique_citations_with_metadata <- keep_one_unique_citation(raw_citations, matched_pairs_with_ids, keep_source, keep_label)

    }

    return(list("unique" = unique_citations_with_metadata,
                "manual_dedup" = manual_dedup))
  }

  #' Deduplicate citations
  #'
  #' This function deduplicates citation data
  #' @export
  #' @import dplyr
  #' @param raw_citations A dataframe containing duplicate ciations
  #' @param manual_dedup Logical value. Do you want to retrieve dataframe for manual deduplication?
  #' @param merge_citations Logical value. Do you want to merge matching citations?
  #' @param keep_source Character vector. Selected citation source to preferentially retain in the dataset as the unique record
  #' @param keep_source Selected citation label to preferentially retain in the dataset as the unique record
  #' @return A list of 2 dataframes - unique citations and citations to be manually deduplicated if option selected
  #' @examples
  #'
  #' result <- dedup_citations(data, keep_source = "pubmed")
  #' unique_citations <- result$unique
  dedup_citations_add_manual <- function(raw_citations, merge_citations=FALSE, preferred_source=NULL, additional_pairs){

    print("formatting data...")
    # add warning for no record id
    if(!"record_id" %in% names(raw_citations)){
      warning("Search does not contain a record_id column. A record_id will be created using row names")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)
    }

    # add warning for any missing record id
    else if(any(is.na(raw_citations$record_id) | raw_citations$record_id=="")){
      warning("Search contains missing values for the record_id column. A record_id will be created using row names")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)
    }

    # add warning for non unique ids
    else if(length(unique(raw_citations$record_id)) != nrow(raw_citations)){
      warning("The record_id column is not unique. A record_id will be created using row names")

      # add record id using row number
      raw_citations <- add_id_citations(raw_citations)
    }

    ordered_citations <- order_citations(raw_citations)
    formatted_citations <- format_citations(ordered_citations)

    print("identifying potential duplicates...")
    pairs <- match_citations(formatted_citations)
    pair_types <- identify_true_matches(pairs)
    true_pairs <- plyr::rbind.fill(pair_types$true_pairs, additional_pairs)

    print("identified duplicates!")
    matched_pairs_with_ids <- generate_dup_id(true_pairs, formatted_citations)

    if(merge_citations == TRUE){

      unique_citations_with_metadata <- merge_metadata(raw_citations, matched_pairs_with_ids)
    }  else{
      unique_citations_with_metadata <- keep_one_unique_citation(raw_citations, matched_pairs_with_ids, keep_source, keep_label)

    }
  }
