#' @export
get_dedup_results <-function(x,
                      author = "author",
                      title = "title",
                      year = "year",
                      journal = "journal",
                      isbn = "isbn",
                      abstract = "abstract",
                      doi = "doi",
                      number = "number",
                      pages = "pages",
                      volume = "volume",
                      record_id = "record_id",
                      label = "label")
{

  # Rename columns if necessary
  x <- x %>%
    rename("author" = author,
           "title" = title,
           "year" = year,
           "journal" = journal,
           "isbn" = isbn,
           "abstract" = abstract,
           "doi" = doi,
           "number" = number,
           "pages" = pages,
           "volume" = volume,
           "record_id"=record_id,
           "label" = label)

  # Select relevant columns
  newdatformatted <-  x  %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label)


  # Arrange by year and presence of an abstract - we want to keep newer records and records with an abstract preferentially
  newdatformatted <- newdatformatted %>%
    arrange(desc(year), abstract)

  # Rename this ordered data as y
  y <- newdatformatted


  # Make sure author is a character
  newdatformatted$author <- as.character(newdatformatted$author)

  # Fix author formatting so similar
  newdatformatted <- newdatformatted %>%
    mutate(author = ifelse(author=="", "Unknown", author)) %>%
    mutate(author = ifelse(is.na(author), "Unknown", author)) %>%
    mutate(author = ifelse(author=="Anonymous", "Unknown", author))

  # Make all upper case
  newdatformatted <- as.data.frame(sapply(newdatformatted, toupper))


  ##Get rid of punctuation and differnces in doi formatting
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("%28", "(", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("%29", ")", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("HTTP://DX.doi.ORG/", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("HTTPS://doi.ORG/", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("doi: ", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("doi:", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("doi", "", x)))

  newdatformatted["title"] <- as.data.frame(sapply(newdatformatted["title"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["year"] <- as.data.frame(sapply(newdatformatted["year"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["abstract"] <- as.data.frame(sapply(newdatformatted["abstract"], function(x) gsub("[[:punct:]]", "", x)))

  newdatformatted["isbn"] <- as.data.frame(sapply(newdatformatted["isbn"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  newdatformatted["isbn"] <- as.data.frame(sapply(newdatformatted["isbn"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  newdatformatted<-newdatformatted %>%
    filter(!is.na(record_id))


  # Sort out NA / missing fata formatting for optimal matching
  newdatformatted <- newdatformatted %>%
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


  newdatformatted<- newdatformatted %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label)


  # Deduplication steps ---------------------------------------

  # ROUND 1: run compare.dedup function and block by title&pages OR title&author OR title&abstract OR doi
  try(newpairs <- compare.dedup(newdatformatted, blockfld = list(c(2,8), c(1,2), c(2,5), 6), strcmp = TRUE, exclude=c("record_id", "label")), silent=TRUE)

  # Create df of pairs
  linkedpairs <- as.data.frame(if(exists("newpairs")) newpairs$pairs)

  # ROUND 2: run compare.dedup function and block by author&year&pages OR journal&volume&pages or isbn&volume&pages OR title&isbn
  try(newpairs2 <- compare.dedup(newdatformatted, blockfld = list(c(1,3,8), c(4,9,8), c(10,9,8), c(2,10)), strcmp = TRUE, exclude= c("record_id", "label")), silent=TRUE)

  #Create df of pairs
  linkedpairs2 <- as.data.frame(if(exists("newpairs2")) newpairs2$pairs)

  # ROUND 3: run compare.dedup function and block by year&pages&volume OR year&Issue&volume or year&pages&Issue
  try(newpairs3 <- compare.dedup(newdatformatted, blockfld = list(c(3,8,9), c(3,7,9), c(3,8,7)), strcmp = TRUE, exclude=c("record_id", "label")), silent = TRUE)

  #Create df of pairs
  linkedpairs3 <- as.data.frame(if(exists("newpairs3")) newpairs3$pairs)

  # ROUND 4: run compare.dedup function and block by author&year OR year&title OR title&volume OR title&journal
  try(newpairs4 <- compare.dedup(newdatformatted, blockfld = list(c(1,3), c(3,2), c(2,9), c(2,4)), strcmp = TRUE, exclude=c("record_id", "label")), silent = TRUE)

  # Create df of pairs
  linkedpairs4 <- as.data.frame(if(exists("newpairs4")) newpairs4$pairs)


  # Combine all possible pairs
  SeePairs <- rbind(if(exists("linkedpairs")) linkedpairs,
                    if(exists("linkedpairs2")) linkedpairs2,
                    if(exists("linkedpairs3")) linkedpairs3,
                    if(exists("linkedpairs4")) linkedpairs4)



  SeePairs <- unique(SeePairs)

  # Obtain metadata for matching pairs
  SeePairs <- SeePairs  %>%
    mutate(author1 =y$author[id1]) %>%
    mutate(author2 =y$author[id2])

  SeePairs <- SeePairs %>%
    mutate(title1 =y$title[id1]) %>%
    mutate(title2 =y$title[id2]) %>%
    mutate(abstract1 =y$abstract[id1]) %>%
    mutate(abstract2 =y$abstract[id2]) %>%
    mutate(doi1= y$doi[id1]) %>%
    mutate(doi2 =y$doi[id2])

  SeePairs <- SeePairs  %>%
    mutate(year1=y$year[id1]) %>%
    mutate(year2=y$year[id2]) %>%
    mutate(number1 =y$number[id1]) %>%
    mutate(number2 =y$number[id2]) %>%
    mutate(pages1 =y$pages[id1]) %>%
    mutate(pages2 =y$pages[id2]) %>%
    mutate(volume1 =y$volume[id1]) %>%
    mutate(volume2 =y$volume[id2])

  SeePairs <- SeePairs  %>%
    mutate(journal1 =y$journal[id1]) %>%
    mutate(journal2 =y$journal[id2]) %>%
    mutate(isbn1 =y$isbn[id1]) %>%
    mutate(isbn2 =y$isbn[id2]) %>%
    mutate(record_id1=y$record_id[id1]) %>%
    mutate(record_id2 =y$record_id[id2]) %>%
    mutate(label1 =y$label[id1]) %>%
    mutate(label2 =y$label[id2])

  SeePairs <- SeePairs %>%
    select(id1, id2, author1, author2, author, title1, title2, title, abstract1, abstract2, abstract, year1, year2, year, number1, number2, number, pages1, pages2, pages, volume1, volume2, volume, journal1, journal2, journal, isbn, isbn1, isbn2, doi1, doi2, doi, record_id1, record_id2, label1, label2)

  SeePairs <- SeePairs %>%
    mutate(abstract = ifelse(is.na(abstract1) & is.na(abstract2), 0, abstract)) %>%
    mutate(pages = ifelse(is.na(pages1) & is.na(pages2), 1, pages)) %>%
    mutate(volume = ifelse(is.na(volume1) & is.na(volume2), 1, volume)) %>%
    mutate(number = ifelse(is.na(number1) & is.na(number2), 1, number)) %>%
    mutate(doi = ifelse(is.na(doi1) & is.na(doi2), 0, doi)) %>%
    mutate(isbn = ifelse(is.na(isbn1) & is.na(isbn2), 0, isbn))



  # Filter matching pairs to less likely unlikely pairs - make sure we only remove true duplicate matches

  SeePairsFiltered <- SeePairs %>%
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
  SeePairsFiltereddoiBAD <- SeePairsFiltered %>%
    filter(!(is.na(doi)| doi ==0 | doi > 0.99)) %>%
    filter(!(title > 0.9 & abstract > 0.9 & (journal|isbn > 0.9)))

  # Remove papers with low matching dois from filtered matched
  SeePairsFiltered <- SeePairsFiltered %>%
    filter(is.na(doi)| doi > 0.99 | doi == 0 | (title > 0.9 & abstract>0.9 & (journal|isbn > 0.9)))

  SeePairsFiltered <- unique(SeePairsFiltered)

  # Make year numeric, then find matches where year differs
  SeePairsFiltered$year1 <- as.numeric(as.character(SeePairsFiltered$year1))
  SeePairsFiltered$year2 <- as.numeric(as.character(SeePairsFiltered$year2))
  yearsDiff <- SeePairsFiltered[which(SeePairsFiltered$year1 != SeePairsFiltered$year2),]
  yearsNotVeryDiff1 <- yearsDiff[which(yearsDiff$year1 == yearsDiff$year2+1 ),]
  yearsNotVeryDiff2 <- yearsDiff[which(yearsDiff$year1 == yearsDiff$year2-1 ),]

  yearsNotVeryDiff <- rbind(yearsNotVeryDiff1, yearsNotVeryDiff2)
  yearsNotVeryDiff <- unique(yearsNotVeryDiff)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  yearsVeryDiff <- yearsDiff[which(!rownames(yearsDiff) %in% rownames(yearsNotVeryDiff)),]

  ManualDedup <- yearsVeryDiff
  SeePairsFiltered <- SeePairsFiltered[which(!rownames(SeePairsFiltered) %in% rownames(yearsVeryDiff)),]

  SeePairsFiltered <- unique(SeePairsFiltered)

  SeePairsFiltered$record_id1 <- as.character(SeePairsFiltered$record_id1)
  SeePairsFiltered$record_id2 <- as.character(SeePairsFiltered$record_id2)

  # Remove duplicate papers ----------------------------------------------

  # Get original data ready for removing duplicates
  dedupdat <- newdatformatted
  dedupdat$record_id <- as.character(dedupdat$record_id)

  SeePairsToDedup <- SeePairsFiltered

  # Keep record 1 and remove record 2
  linkedpairskeep1 <- SeePairsToDedup

  # Select all Record2 IDs and remove from dataset
  removerefs4 <- unique(linkedpairskeep1$record_id2)
  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeep1)),]
  dedupdat <- dedupdat[which(!dedupdat$record_id %in% removerefs4),]

  dedupdat <- unique(dedupdat)

  # Get list of removed IDs
  checkremovedalreadyID <- c(removerefs4)
  checkremovedalreadyID <-unique(checkremovedalreadyID)


  # Get original data ready for removing duplicates
  dedupdat <- newdatformatted
  dedupdat$record_id <- as.character(dedupdat$record_id)

  # Keep record1 and remove record2
  linkedpairslabelledkeep1 <- SeePairsFiltered
  removerefslabelled <- unique(linkedpairslabelledkeep1$record_id2)
  dedupdat <- dedupdat[which(!dedupdat$record_id %in% removerefslabelled),]

  # Get list of references removed
  checkremovedalreadyID <- c(removerefslabelled)
  checkremovedalreadyID <-unique(checkremovedalreadyID)

  # Get potential duplicates for manual deduplication
  MaybePairs <- SeePairs %>%
    filter(record_id1 %in% dedupdat$record_id &
             record_id2 %in% dedupdat$record_id) %>%

    filter(doi > 0.99 |
             title>0.85 & author>0.75 |
             title>0.80 & abstract>0.80 |
             title>0.80 & isbn>0.99 |
             title>0.80 & journal>0.80)

  # Add in problem doi matching pairs and different year data in ManualDedup
  MaybePairs <- rbind(MaybePairs, ManualDedup, SeePairsFiltereddoiBAD)
  MaybePairs <- unique(MaybePairs)

  y$record_id <- as.character(y$record_id)
  dedupdat$record_id <- as.character(dedupdat$record_id)

  uniquedat <- x %>%
    filter(record_id %in% dedupdat$record_id)

  #Remove one record_id1 when 2 match the same record_id2
  # additional <- SeePairsFiltered %>%
  #   filter(record_id1 %in% uniquedat$record_id) %>%
  #   group_by(record_id2) %>%
  #   mutate(N = length(unique(record_id1))) %>%
  #   filter(N>1) %>%
  #   select(-N) %>%
  #   mutate(record_id1 = first(record_id1)) %>%
  #   mutate(label1 = "additionaldup") %>%
  #   ungroup()

  MaybePairs <- MaybePairs %>%
    filter(record_id1 %in% uniquedat$record_id &
             record_id2 %in% uniquedat$record_id)

  removedat <- x
  removedat <-removedat %>%
    filter(!record_id %in% uniquedat$record_id)


  return(list("ManualDedup" = MaybePairs,
              "Unique" = uniquedat,
              "TruePairs" = SeePairsFiltered,
              "DuplicateRefsRemoved" = removedat))

}

dedup_labelled_step <-function(x,
                               author = "author",
                               title = "title",
                               year = "year",
                               journal = "journal",
                               isbn = "isbn",
                               abstract = "abstract",
                               doi = "doi",
                               number = "number",
                               pages = "pages",
                               volume = "volume",
                               record_id = "record_id",
                               label = "label",
                               labelKeep = "De-duplicate as normal"){

  # Rename columns if necessary
  x <- x %>%
    rename("author" = author,
           "title" = title,
           "year" = year,
           "journal" = journal,
           "isbn" = isbn,
           "abstract" = abstract,
           "doi" = doi,
           "number" = number,
           "pages" = pages,
           "volume" = volume,
           "record_id"=record_id,
           "label" = label)

  #Ensure columns are in this order
  newdatformatted <- x  %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label)

  newdatformatted <- newdatformatted %>%
    mutate(Order = ifelse(label == labelKeep, 1, 2)) %>%
    arrange(Order) %>%
    select(-Order) %>%
    unique()

  # Rename this ordered data as y
  y <- newdatformatted

  # Make sure author is a character
  newdatformatted$author <- as.character(newdatformatted$author)

  # Fix author formatting so similar
  newdatformatted <- newdatformatted %>%
    mutate(author = ifelse(author=="", "Unknown", author)) %>%
    mutate(author = ifelse(is.na(author), "Unknown", author)) %>%
    mutate(author = ifelse(author=="Anonymous", "Unknown", author))

  # Make all upper case
  newdatformatted <- as.data.frame(sapply(newdatformatted, toupper))

  ##Get rid of punctuation and differnces in doi formatting
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("%28", "(", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("%29", ")", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("HTTP://DX.doi.ORG/", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("HTTPS://doi.ORG/", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("doi: ", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("doi:", "", x)))
  newdatformatted["doi"] <- as.data.frame(sapply(newdatformatted["doi"], function(x) gsub("doi", "", x)))

  newdatformatted["title"] <- as.data.frame(sapply(newdatformatted["title"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["year"] <- as.data.frame(sapply(newdatformatted["year"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["abstract"] <- as.data.frame(sapply(newdatformatted["abstract"], function(x) gsub("[[:punct:]]", "", x)))

  newdatformatted["isbn"] <- as.data.frame(sapply(newdatformatted["isbn"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  newdatformatted["isbn"] <- as.data.frame(sapply(newdatformatted["isbn"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  newdatformatted<-newdatformatted %>%
    filter(!is.na(record_id))

  # Sort out NA / missing fata formatting for optimal matching
  newdatformatted <- newdatformatted %>%
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

  newdatformatted<- newdatformatted %>%
    select(author, title, year, journal, abstract, doi, number, pages, volume, isbn, record_id, label)

  # Deduplication steps ---------------------------------------

  # ROUND 1: run compare.dedup function and block by title&pages OR title&author OR title&abstract OR doi
  newpairs = compare.dedup(newdatformatted, blockfld = list(c(2,8), c(1,2), c(2,5), 6), strcmp = TRUE, exclude=c("record_id", "label"))

  # Create df of pairs
  dfpairs <- as.data.frame(newpairs$pairs)
  linkedpairs <- dfpairs

  # ROUND 2: run compare.dedup function and block by author&year&pages OR journal&volume&pages or title&isbn
  newpairs2 = compare.dedup(newdatformatted, blockfld = list(c(1,3,8), c(4,9,8), c(10,9,8), c(2,10)), strcmp = TRUE, exclude= c("record_id", "label"))


  #Create df of pairs
  dfpairs2 <- as.data.frame(newpairs2$pairs)
  linkedpairs2 <- dfpairs2

  # ROUND 3: run compare.dedup function and block by year&pages&volume OR year&Issue&volume or year&pages&Issue
  newpairs3 = compare.dedup(newdatformatted, blockfld = list(c(3,8,9), c(3,7,9), c(3,8,7)), strcmp = TRUE, exclude=c("record_id", "label"))

  #Create df of pairs
  dfpairs3 <- as.data.frame(newpairs3$pairs)
  linkedpairs3 <- dfpairs3

  # ROUND 4: run compare.dedup function and block by author&year OR year&title OR title&volume OR title&journal
  newpairs4 = compare.dedup(newdatformatted, blockfld = list(c(1,3), c(3,2), c(2,9), c(2,4)), strcmp = TRUE, exclude=c("record_id", "label"))

  # Create df of pairs
  dfpairs4 <- as.data.frame(newpairs4$pairs)
  linkedpairs4 <- dfpairs4

  # Combine all possible pairs
  SeePairs <- rbind(linkedpairs, linkedpairs2, linkedpairs3, linkedpairs4)
  SeePairs <- unique(SeePairs)

  # Obtain metadata for matching pairs
  SeePairs <- SeePairs  %>%
    mutate(author1 =y$author[id1]) %>%
    mutate(author2 =y$author[id2])

  SeePairs <- SeePairs %>%
    mutate(title1 =y$title[id1]) %>%
    mutate(title2 =y$title[id2]) %>%
    mutate(abstract1 =y$abstract[id1]) %>%
    mutate(abstract2 =y$abstract[id2]) %>%
    mutate(doi1= y$doi[id1]) %>%
    mutate(doi2 =y$doi[id2])

  SeePairs <- SeePairs  %>%
    mutate(year1=y$year[id1]) %>%
    mutate(year2=y$year[id2]) %>%
    mutate(number1 =y$number[id1]) %>%
    mutate(number2 =y$number[id2]) %>%
    mutate(pages1 =y$pages[id1]) %>%
    mutate(pages2 =y$pages[id2]) %>%
    mutate(volume1 =y$volume[id1]) %>%
    mutate(volume2 =y$volume[id2])

  SeePairs <- SeePairs  %>%
    mutate(journal1 =y$journal[id1]) %>%
    mutate(journal2 =y$journal[id2]) %>%
    mutate(isbn1 =y$isbn[id1]) %>%
    mutate(isbn2 =y$isbn[id2]) %>%
    mutate(record_id1=y$record_id[id1]) %>%
    mutate(record_id2 =y$record_id[id2]) %>%
    mutate(label1 =y$label[id1]) %>%
    mutate(label2 =y$label[id2])

  SeePairs <- SeePairs %>%
    select(id1, id2, author1, author2, author, title1, title2, title, abstract1, abstract2, abstract, year1, year2, year, number1, number2, number, pages1, pages2, pages, volume1, volume2, volume, journal1, journal2, journal, isbn, isbn1, isbn2, doi1, doi2, doi, record_id1, record_id2, label1, label2)

  SeePairs <- SeePairs %>%
    mutate(abstract = ifelse(is.na(abstract1) & is.na(abstract2), 0, abstract)) %>%
    mutate(pages = ifelse(is.na(pages1) & is.na(pages2), 1, pages)) %>%
    mutate(volume = ifelse(is.na(volume1) & is.na(volume2), 1, volume)) %>%
    mutate(number = ifelse(is.na(number1) & is.na(number2), 1, number)) %>%
    mutate(doi = ifelse(is.na(doi1) & is.na(doi2), 0, doi)) %>%
    mutate(isbn = ifelse(is.na(isbn1) & is.na(isbn2), 0, isbn))


  # Filter matching pairs to less likely unlikely pairs - make sure we only remove true duplicate matches
  SeePairsFiltered <- SeePairs %>%
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
  SeePairsFiltereddoiBAD <- SeePairsFiltered %>%
    filter(!(is.na(doi)| doi ==0 | doi > 0.99)) %>%
    filter(!(title > 0.9 & abstract > 0.9 & (journal|isbn > 0.9)))

  # Remove papers with low matching dois from filtered matched
  SeePairsFiltered <- SeePairsFiltered %>%
    filter(is.na(doi)| doi > 0.99 | doi == 0 | (title > 0.9 & abstract>0.9 & (journal|isbn > 0.9)))

  SeePairsFiltered <- unique(SeePairsFiltered)

  # Make year numeric, then find matches where year differs
  SeePairsFiltered$year1 <- as.numeric(as.character(SeePairsFiltered$year1))
  SeePairsFiltered$year2 <- as.numeric(as.character(SeePairsFiltered$year2))
  yearsDiff <- SeePairsFiltered[which(SeePairsFiltered$year1 != SeePairsFiltered$year2),]
  yearsNotVeryDiff1 <- yearsDiff[which(yearsDiff$year1 == yearsDiff$year2+1 ),]
  yearsNotVeryDiff2 <- yearsDiff[which(yearsDiff$year1 == yearsDiff$year2-1 ),]

  yearsNotVeryDiff <- rbind(yearsNotVeryDiff1, yearsNotVeryDiff2)
  yearsNotVeryDiff <- unique(yearsNotVeryDiff)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  yearsVeryDiff <- yearsDiff[which(!rownames(yearsDiff) %in% rownames(yearsNotVeryDiff)),]

  ManualDedup <- yearsVeryDiff
  SeePairsFiltered <- SeePairsFiltered[which(!rownames(SeePairsFiltered) %in% rownames(yearsVeryDiff)),]

  SeePairsFiltered <- unique(SeePairsFiltered)

  SeePairsFiltered$record_id1 <- as.character(SeePairsFiltered$record_id1)
  SeePairsFiltered$record_id2 <- as.character(SeePairsFiltered$record_id2)

  # Remove duplicate papers ----------------------------------------------

  # Get original data ready for removing duplicates
  dedupdat <- newdatformatted
  dedupdat$record_id <- as.character(dedupdat$record_id)

  # Keep record1 and remove record2
  linkedpairslabelledkeep1 <- SeePairsFiltered
  removerefslabelled <- unique(linkedpairslabelledkeep1$record_id2)
  dedupdat <- dedupdat[which(!dedupdat$record_id %in% removerefslabelled),]

  # Get list of references removed
  checkremovedalreadyID <- c(removerefslabelled)
  checkremovedalreadyID <-unique(checkremovedalreadyID)

  # Get potential duplicates for manual deduplication
  MaybePairs <- SeePairs %>%
    filter(record_id1 %in% dedupdat$record_id &
             record_id2 %in% dedupdat$record_id) %>%
    filter(doi > 0.99 |
             title>0.85 & author>0.75 |
             title>0.80 & abstract>0.80 |
             title>0.80 & isbn>0.99 |
             title>0.80 & journal>0.80)

  # Add in problem doi matching pairs and different year data in ManualDedup
  MaybePairs <- rbind(MaybePairs, ManualDedup, SeePairsFiltereddoiBAD)
  MaybePairs <- unique(MaybePairs)

  y$record_id <- as.character(y$record_id)
  dedupdat$record_id <- as.character(dedupdat$record_id)

  uniquedat <- x %>%
    filter(record_id %in% dedupdat$record_id)

  # #Remove one record_id1 when 2 match the same record_id2
  # additional <- SeePairsFiltered %>%
  #   filter(!label1 == labelKeep) %>%
  #   filter(record_id1 %in% uniquedat$record_id) %>%
  #   group_by(record_id2) %>%
  #   mutate(N = length(unique(record_id1))) %>%
  #   filter(N>1) %>%
  #   select(-N) %>%
  #   mutate(record_id1 = first(record_id1)) %>%
  #   mutate(label1 = "additionaldup") %>%
  #   ungroup()
  #
  # uniquedat <- uniquedat %>%
  #   filter(!record_id %in% unique(additional$record_id1))

  MaybePairs <- MaybePairs %>%
    filter(record_id1 %in% uniquedat$record_id &
             record_id2 %in% uniquedat$record_id)

  # SeePairsFiltered <- rbind(SeePairsFiltered, additional)
  SeePairsFiltered <- as.data.frame(SeePairsFiltered)

  removedat <- x
  removedat <-removedat %>%
    filter(!record_id %in% uniquedat$record_id)


  return(list("ManualDedup" = MaybePairs,
              "Unique" = uniquedat,
              "TruePairs" = SeePairsFiltered,
              "DuplicateRefsRemoved" = removedat))

}

#' @export
get_labelled_results <- function (x,
                                 labelKeep = ""){



  result1 <- dedup_labelled_step(x,
                                 labelKeep = labelKeep)
  manual1 <- result1$ManualDedup
  unique1 <- result1$Unique
  pairs1 <- result1$TruePairs
  allmatches1 <- result1$PotentialPairs
  removed1 <- result1$DuplicateRefsRemoved

  unique1<-unique1 %>%
    select(record_id, author, year, title, journal, abstract, volume, number, pages, doi, label, isbn)

  manual_otherway <- manual1 %>%
    mutate(id1 = record_id2) %>%
    mutate(id2= record_id1) %>%
    rename(title1 = title2,
           title2 = title1,
           author1 = author2,
           author2 = author1,
           year1 = year2,
           year2 = year1,
           journal1 = journal2,
           journal2 = journal1,
           abstract1 = abstract2,
           abstract2 = abstract1,
           volume1 = volume2,
           volume2 = volume1,
           number1 = number2,
           number2 = number1,
           pages1 = pages2,
           pages2 = pages1,
           doi1 = doi2,
           doi2 = doi1,
           label1 = label2,
           label2 = label1,
           isbn1 = isbn2,
           isbn2 = isbn1,
           record_id1 = record_id2,
           record_id2 = record_id1
    )


  result2 <- get_dedup_results(unique1)

  manual2id <- result2$ManualDedup
  manual2 <-result2$ManualDedup  %>%
    mutate(id1 = record_id1,
           id2 = record_id2)

  manual2 <- anti_join(manual2, manual_otherway)

  unique2 <- result2$Unique
  pairs2 <- result2$TruePairs
  allmatches2 <- result2$PotentialPairs
  removed2 <- result2$DuplicateRefsRemoved

  unique2<-unique2 %>%
    select(record_id, author, year, title, journal, abstract, volume, number, pages, doi, label, isbn)

  manual <- rbind(manual1, manual2)
  manual <- manual %>%
    group_by(record_id1, record_id2) %>%
    mutate(id1 = first(id1)) %>%
    mutate(id2 = first(id2)) %>%
    ungroup()

  manual <- unique(manual)

  unique <- unique(unique2)

  pairs <- rbind(pairs1, pairs2)
  pairs <- unique(pairs)

  allmatches <- rbind(allmatches1, allmatches2)
  allmatches<-unique(allmatches)

  removed <- rbind(removed1, removed2)

  print(length(unique1$record_id))
  print(length(unique2$record_id))

  return(list("ManualDedup" = manual,
              "Unique" = unique,
              "TruePairs" = pairs,
              "DuplicateRefsRemoved" = removed))
}


#' @export
get_unique <-function(x,
                      author = "author",
                      title = "title",
                      year = "year",
                      journal = "journal",
                      isbn = "isbn",
                      abstract = "abstract",
                      doi = "doi",
                      number = "number",
                      pages = "pages",
                      volume = "volume",
                      record_id = "record_id",
                      label = "label"){

dedup_results <- get_dedup_results(x)

return(dedup_results$Unique)

}

#' @export
get_dups_removed <-function(x,
                      author = "author",
                      title = "title",
                      year = "year",
                      journal = "journal",
                      isbn = "isbn",
                      abstract = "abstract",
                      doi = "doi",
                      number = "number",
                      pages = "pages",
                      volume = "volume",
                      record_id = "record_id",
                      label = "label"){

dedup_results <- get_dedup_results(x)

return(dedup_results$DuplicateRefsRemoved)
}

#' @export
get_pairs <-function(x,
                            author = "author",
                            title = "title",
                            year = "year",
                            journal = "journal",
                            isbn = "isbn",
                            abstract = "abstract",
                            doi = "doi",
                            number = "number",
                            pages = "pages",
                            volume = "volume",
                            record_id = "record_id",
                            label = "label"){

  dedup_results <- get_dedup_results(x)

  return(dedup_results$TruePairs)
}

#' @export
get_potential_pairs <-function(x,
                         author = "author",
                         title = "title",
                         year = "year",
                         journal = "journal",
                         isbn = "isbn",
                         abstract = "abstract",
                         doi = "doi",
                         number = "number",
                         pages = "pages",
                         volume = "volume",
                         record_id = "record_id",
                         label = "label"){

  dedup_results <- get_dedup_results(x)

  return(dedup_results$ManualDedup)
}
