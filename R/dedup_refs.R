#' @export
get_dedup_results <-function(x,
                      Author = "Author",
                      Title = "Title",
                      Year = "Year",
                      Journal = "Journal",
                      ISBN = "ISBN",
                      Abstract = "Abstract",
                      DOI = "DOI",
                      Number = "Number",
                      Pages = "Pages",
                      Volume = "Volume",
                      RecordID = "RecordID",
                      Label = "Label")
{

  # Rename columns if necessary
  x <- x %>%
    rename("Author" = Author,
           "Title" = Title,
           "Year" = Year,
           "Journal" = Journal,
           "ISBN" = ISBN,
           "Abstract" = Abstract,
           "DOI" = DOI,
           "Number" = Number,
           "Pages" = Pages,
           "Volume" = Volume,
           "RecordID"=RecordID,
           "Label" = Label)

  # Select relevant columns
  newdatformatted <-  x  %>%
    select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, ISBN, RecordID, Label)


  # Arrange by Year and presence of an Abstract - we want to keep newer records and records with an abstract preferentially
  newdatformatted <- newdatformatted %>%
    arrange(desc(Year), Abstract)

  # Rename this ordered data as y
  y <- newdatformatted


  # Make sure author is a character
  newdatformatted$Author <- as.character(newdatformatted$Author)

  # Fix author formatting so similar
  newdatformatted <- newdatformatted %>%
    mutate(Author = ifelse(Author=="", "Unknown", Author)) %>%
    mutate(Author = ifelse(is.na(Author), "Unknown", Author)) %>%
    mutate(Author = ifelse(Author=="Anonymous", "Unknown", Author))

  # Make all upper case
  newdatformatted <- as.data.frame(sapply(newdatformatted, toupper))


  ##Get rid of punctuation and differnces in DOI formatting
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("%28", "(", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("%29", ")", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("HTTP://DX.DOI.ORG/", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("HTTPS://DOI.ORG/", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("DOI: ", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("DOI:", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("DOI", "", x)))

  newdatformatted["Title"] <- as.data.frame(sapply(newdatformatted["Title"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Year"] <- as.data.frame(sapply(newdatformatted["Year"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Abstract"] <- as.data.frame(sapply(newdatformatted["Abstract"], function(x) gsub("[[:punct:]]", "", x)))

  newdatformatted["ISBN"] <- as.data.frame(sapply(newdatformatted["ISBN"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  newdatformatted["ISBN"] <- as.data.frame(sapply(newdatformatted["ISBN"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  newdatformatted<-newdatformatted %>%
    filter(!is.na(RecordID))


  # Sort out NA / missing fata formatting for optimal matching
  newdatformatted <- newdatformatted %>%
    mutate(Author = ifelse(Author=="NA", NA, paste(Author))) %>%
    mutate(Year = ifelse(Year=="NA", NA, paste(Year))) %>%
    mutate(Title = ifelse(Title=="NA", NA, paste(Title))) %>%
    mutate(Number = ifelse(Number=="NA", NA, paste(Number))) %>%
    mutate(Volume = ifelse(Volume=="NA", NA, paste(Volume))) %>%
    mutate(Pages = ifelse(Pages=="NA", NA, paste(Pages))) %>%
    mutate(Abstract = ifelse(Abstract=="NA", NA, paste(Abstract))) %>%
    mutate(DOI = ifelse(DOI=="NA", NA, paste(DOI))) %>%
    mutate(Journal = ifelse(Journal=="NA", NA, paste(Journal))) %>%
    mutate(ISBN = ifelse(ISBN=="", NA, paste(ISBN)))


  newdatformatted<- newdatformatted %>%
    select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, ISBN, RecordID, Label)


  # Deduplication steps ---------------------------------------

  # ROUND 1: run compare.dedup function and block by Title&Pages OR Title&Author OR Title&Abstract OR DOI
  try(newpairs <- compare.dedup(newdatformatted, blockfld = list(c(2,8), c(1,2), c(2,5), 6), strcmp = TRUE, exclude=c("RecordID", "Label")), silent=TRUE)

  # Create df of pairs
  linkedpairs <- as.data.frame(if(exists("newpairs")) newpairs$pairs)

  # ROUND 2: run compare.dedup function and block by Author&Year&Pages OR Journal&Volume&Pages or ISBN&Volume&Pages OR Title&ISBN
  try(newpairs2 <- compare.dedup(newdatformatted, blockfld = list(c(1,3,8), c(4,9,8), c(10,9,8), c(2,10)), strcmp = TRUE, exclude= c("RecordID", "Label")), silent=TRUE)

  #Create df of pairs
  linkedpairs2 <- as.data.frame(if(exists("newpairs2")) newpairs2$pairs)

  # ROUND 3: run compare.dedup function and block by Year&Pages&Volume OR Year&Issue&Volume or Year&Pages&Issue
  try(newpairs3 <- compare.dedup(newdatformatted, blockfld = list(c(3,8,9), c(3,7,9), c(3,8,7)), strcmp = TRUE, exclude=c("RecordID", "Label")), silent = TRUE)

  #Create df of pairs
  linkedpairs3 <- as.data.frame(if(exists("newpairs3")) newpairs3$pairs)

  # ROUND 4: run compare.dedup function and block by Author&Year OR Year&Title OR Title&Volume OR Title&Journal
  try(newpairs4 <- compare.dedup(newdatformatted, blockfld = list(c(1,3), c(3,2), c(2,9), c(2,4)), strcmp = TRUE, exclude=c("RecordID", "Label")), silent = TRUE)

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
    mutate(Author1 =y$Author[id1]) %>%
    mutate(Author2 =y$Author[id2])

  SeePairs <- SeePairs %>%
    mutate(Title1 =y$Title[id1]) %>%
    mutate(Title2 =y$Title[id2]) %>%
    mutate(Abstract1 =y$Abstract[id1]) %>%
    mutate(Abstract2 =y$Abstract[id2]) %>%
    mutate(DOI1= y$DOI[id1]) %>%
    mutate(DOI2 =y$DOI[id2])

  SeePairs <- SeePairs  %>%
    mutate(Year1=y$Year[id1]) %>%
    mutate(Year2=y$Year[id2]) %>%
    mutate(Number1 =y$Number[id1]) %>%
    mutate(Number2 =y$Number[id2]) %>%
    mutate(Pages1 =y$Pages[id1]) %>%
    mutate(Pages2 =y$Pages[id2]) %>%
    mutate(Volume1 =y$Volume[id1]) %>%
    mutate(Volume2 =y$Volume[id2])

  SeePairs <- SeePairs  %>%
    mutate(Journal1 =y$Journal[id1]) %>%
    mutate(Journal2 =y$Journal[id2]) %>%
    mutate(ISBN1 =y$ISBN[id1]) %>%
    mutate(ISBN2 =y$ISBN[id2]) %>%
    mutate(RecordID1=y$RecordID[id1]) %>%
    mutate(RecordID2 =y$RecordID[id2]) %>%
    mutate(Label1 =y$Label[id1]) %>%
    mutate(Label2 =y$Label[id2])

  SeePairs <- SeePairs %>%
    select(id1, id2, Author1, Author2, Author, Title1, Title2, Title, Abstract1, Abstract2, Abstract, Year1, Year2, Year, Number1, Number2, Number, Pages1, Pages2, Pages, Volume1, Volume2, Volume, Journal1, Journal2, Journal, ISBN, ISBN1, ISBN2, DOI1, DOI2, DOI, RecordID1, RecordID2, Label1, Label2)

  SeePairs <- SeePairs %>%
    mutate(Abstract = ifelse(is.na(Abstract1) & is.na(Abstract2), 0, Abstract)) %>%
    mutate(Pages = ifelse(is.na(Pages1) & is.na(Pages2), 1, Pages)) %>%
    mutate(Volume = ifelse(is.na(Volume1) & is.na(Volume2), 1, Volume)) %>%
    mutate(Number = ifelse(is.na(Number1) & is.na(Number2), 1, Number)) %>%
    mutate(DOI = ifelse(is.na(DOI1) & is.na(DOI2), 0, DOI)) %>%
    mutate(ISBN = ifelse(is.na(ISBN1) & is.na(ISBN2), 0, ISBN))



  # Filter matching pairs to less likely unlikely pairs - make sure we only remove true duplicate matches

  SeePairsFiltered <- SeePairs %>%
    filter(
      (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & ISBN>0.99) |
        (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & Journal>0.6) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & Journal>0.6) |
        (Volume >0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50  & Journal>0.6) |

        (Volume >0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.8) |
        (Volume>0.8 & Pages>0.8 & Title>0.90 & Abstract>0.9 & Author>0.8) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.9 & Author>0.8) |

        (DOI>0.95 & Author>0.75 & Title>0.9) |

        (Title>0.80 & Abstract>0.90 & Volume>0.85 & Journal>0.65 & Author>0.9) |
        (Title>0.90 & Abstract>0.80 & Volume>0.85 & Journal>0.65 & Author>0.9)|

        (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.8 & Author>0.9 & Journal>0.75) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.80 & Author>0.9 & Journal>0.75) |
        (Volume>0.8 & Number>0.8 & Title>0.90 & Abstract>0.8 & Author>0.9  & Journal>0.75) |

        (Title>0.9 & Author>0.9 & Abstract>0.9 & Journal >0.7)|
        (Title>0.9 & Author>0.9 & Abstract>0.9 & ISBN >0.99)|

        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & Journal>0.6) |
        (Number>0.9 & Volume>0.9 & Title>0.90 & Author>0.90 & Journal>0.6) |
        (Pages>0.9 & Volume>0.9 & Title>0.90 & Author>0.80 & Journal>0.6) |
        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & ISBN>0.99) |
        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & ISBN>0.99) |
        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & ISBN>0.99) |

        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & Journal>0.9) |
        (Number>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & Journal>0.9)|
        (Number>0.8 & Pages>0.8 & Title>0.95 & Author>0.80 & Journal>0.9) |
        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & ISBN>0.99) |
        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & ISBN>0.99) |
        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & ISBN>0.99))

  # Find papers with low matching DOIs - often indicates FALSE positive matches
  SeePairsFilteredDOIBAD <- SeePairsFiltered %>%
    filter(!(is.na(DOI)| DOI ==0 | DOI > 0.99)) %>%
    filter(!(Title > 0.9 & Abstract > 0.9 & (Journal|ISBN > 0.9)))

  # Remove papers with low matching DOIs from filtered matched
  SeePairsFiltered <- SeePairsFiltered %>%
    filter(is.na(DOI)| DOI > 0.99 | DOI == 0 | (Title > 0.9 & Abstract>0.9 & (Journal|ISBN > 0.9)))

  SeePairsFiltered <- unique(SeePairsFiltered)

  # Make year numeric, then find matches where year differs
  SeePairsFiltered$Year1 <- as.numeric(as.character(SeePairsFiltered$Year1))
  SeePairsFiltered$Year2 <- as.numeric(as.character(SeePairsFiltered$Year2))
  YearsDiff <- SeePairsFiltered[which(SeePairsFiltered$Year1 != SeePairsFiltered$Year2),]
  YearsNotVeryDiff1 <- YearsDiff[which(YearsDiff$Year1 == YearsDiff$Year2+1 ),]
  YearsNotVeryDiff2 <- YearsDiff[which(YearsDiff$Year1 == YearsDiff$Year2-1 ),]

  YearsNotVeryDiff <- rbind(YearsNotVeryDiff1, YearsNotVeryDiff2)
  YearsNotVeryDiff <- unique(YearsNotVeryDiff)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  YearsVeryDiff <- YearsDiff[which(!rownames(YearsDiff) %in% rownames(YearsNotVeryDiff)),]

  ManualDedup <- YearsVeryDiff
  SeePairsFiltered <- SeePairsFiltered[which(!rownames(SeePairsFiltered) %in% rownames(YearsVeryDiff)),]

  SeePairsFiltered <- unique(SeePairsFiltered)

  SeePairsFiltered$RecordID1 <- as.character(SeePairsFiltered$RecordID1)
  SeePairsFiltered$RecordID2 <- as.character(SeePairsFiltered$RecordID2)

  # Remove duplicate papers ----------------------------------------------

  # Get original data ready for removing duplicates
  dedupdat <- newdatformatted
  dedupdat$RecordID <- as.character(dedupdat$RecordID)

  SeePairsToDedup <- SeePairsFiltered

  # Keep record 1 and remove record 2
  linkedpairskeep1 <- SeePairsToDedup

  # Select all Record2 IDs and remove from dataset
  removerefs4 <- unique(linkedpairskeep1$RecordID2)
  SeePairsToDedup <- SeePairsToDedup[which(!rownames(SeePairsToDedup) %in% rownames(linkedpairskeep1)),]
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefs4),]

  dedupdat <- unique(dedupdat)

  # Get list of removed IDs
  checkremovedalreadyID <- c(removerefs4)
  checkremovedalreadyID <-unique(checkremovedalreadyID)


  # Get original data ready for removing duplicates
  dedupdat <- newdatformatted
  dedupdat$RecordID <- as.character(dedupdat$RecordID)

  # Keep record1 and remove record2
  linkedpairslabelledkeep1 <- SeePairsFiltered
  removerefslabelled <- unique(linkedpairslabelledkeep1$RecordID2)
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefslabelled),]

  # Get list of references removed
  checkremovedalreadyID <- c(removerefslabelled)
  checkremovedalreadyID <-unique(checkremovedalreadyID)

  # Get potential duplicates for manual deduplication
  MaybePairs <- SeePairs %>%
    filter(RecordID1 %in% dedupdat$RecordID &
             RecordID2 %in% dedupdat$RecordID) %>%

    filter(DOI > 0.99 |
             Title>0.85 & Author>0.75 |
             Title>0.80 & Abstract>0.80 |
             Title>0.80 & ISBN>0.99 |
             Title>0.80 & Journal>0.80)

  # Add in problem DOI matching pairs and different year data in ManualDedup
  MaybePairs <- rbind(MaybePairs, ManualDedup, SeePairsFilteredDOIBAD)
  MaybePairs <- unique(MaybePairs)

  y$RecordID <- as.character(y$RecordID)
  dedupdat$RecordID <- as.character(dedupdat$RecordID)

  uniquedat <- x %>%
    filter(RecordID %in% dedupdat$RecordID)

  #Remove one recordID1 when 2 match the same recordID2
  # additional <- SeePairsFiltered %>%
  #   filter(RecordID1 %in% uniquedat$RecordID) %>%
  #   group_by(RecordID2) %>%
  #   mutate(N = length(unique(RecordID1))) %>%
  #   filter(N>1) %>%
  #   select(-N) %>%
  #   mutate(RecordID1 = first(RecordID1)) %>%
  #   mutate(Label1 = "additionaldup") %>%
  #   ungroup()

  MaybePairs <- MaybePairs %>%
    filter(RecordID1 %in% uniquedat$RecordID &
             RecordID2 %in% uniquedat$RecordID)

  removedat <- x
  removedat <-removedat %>%
    filter(!RecordID %in% uniquedat$RecordID)


  return(list("ManualDedup" = MaybePairs,
              "Unique" = uniquedat,
              "TruePairs" = SeePairsFiltered,
              "DuplicateRefsRemoved" = removedat))

}

dedup_labelled_step <-function(x,
                               Author = "Author",
                               Title = "Title",
                               Year = "Year",
                               Journal = "Journal",
                               ISBN = "ISBN",
                               Abstract = "Abstract",
                               DOI = "DOI",
                               Number = "Number",
                               Pages = "Pages",
                               Volume = "Volume",
                               RecordID = "RecordID",
                               Label = "Label",
                               LabelKeep = "De-duplicate as normal"){

  # Rename columns if necessary
  x <- x %>%
    rename("Author" = Author,
           "Title" = Title,
           "Year" = Year,
           "Journal" = Journal,
           "ISBN" = ISBN,
           "Abstract" = Abstract,
           "DOI" = DOI,
           "Number" = Number,
           "Pages" = Pages,
           "Volume" = Volume,
           "RecordID"=RecordID,
           "Label" = Label)

  #Ensure columns are in this order
  newdatformatted <- x  %>%
    select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, ISBN, RecordID, Label)

  newdatformatted <- newdatformatted %>%
    mutate(Order = ifelse(Label == LabelKeep, 1, 2)) %>%
    arrange(Order) %>%
    select(-Order) %>%
    unique()

  # Rename this ordered data as y
  y <- newdatformatted

  # Make sure author is a character
  newdatformatted$Author <- as.character(newdatformatted$Author)

  # Fix author formatting so similar
  newdatformatted <- newdatformatted %>%
    mutate(Author = ifelse(Author=="", "Unknown", Author)) %>%
    mutate(Author = ifelse(is.na(Author), "Unknown", Author)) %>%
    mutate(Author = ifelse(Author=="Anonymous", "Unknown", Author))

  # Make all upper case
  newdatformatted <- as.data.frame(sapply(newdatformatted, toupper))

  ##Get rid of punctuation and differnces in DOI formatting
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("%28", "(", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("%29", ")", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("HTTP://DX.DOI.ORG/", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("HTTPS://DOI.ORG/", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("DOI: ", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("DOI:", "", x)))
  newdatformatted["DOI"] <- as.data.frame(sapply(newdatformatted["DOI"], function(x) gsub("DOI", "", x)))

  newdatformatted["Title"] <- as.data.frame(sapply(newdatformatted["Title"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Year"] <- as.data.frame(sapply(newdatformatted["Year"], function(x) gsub("[[:punct:]]", "", x)))
  newdatformatted["Abstract"] <- as.data.frame(sapply(newdatformatted["Abstract"], function(x) gsub("[[:punct:]]", "", x)))

  newdatformatted["ISBN"] <- as.data.frame(sapply(newdatformatted["ISBN"], function(x) gsub("[[:space:]]\\(PRINT\\).*", "", x)))
  newdatformatted["ISBN"] <- as.data.frame(sapply(newdatformatted["ISBN"], function(x) gsub("[[:space:]]\\(ELECTRONIC\\).*", "", x)))

  newdatformatted<-newdatformatted %>%
    filter(!is.na(RecordID))

  # Sort out NA / missing fata formatting for optimal matching
  newdatformatted <- newdatformatted %>%
    mutate(Author = ifelse(Author=="NA", NA, paste(Author))) %>%
    mutate(Year = ifelse(Year=="NA", NA, paste(Year))) %>%
    mutate(Title = ifelse(Title=="NA", NA, paste(Title))) %>%
    mutate(Number = ifelse(Number=="NA", NA, paste(Number))) %>%
    mutate(Volume = ifelse(Volume=="NA", NA, paste(Volume))) %>%
    mutate(Pages = ifelse(Pages=="NA", NA, paste(Pages))) %>%
    mutate(Abstract = ifelse(Abstract=="NA", NA, paste(Abstract))) %>%
    mutate(DOI = ifelse(DOI=="NA", NA, paste(DOI))) %>%
    mutate(Journal = ifelse(Journal=="NA", NA, paste(Journal))) %>%
    mutate(ISBN = ifelse(ISBN=="", NA, paste(ISBN)))

  newdatformatted<- newdatformatted %>%
    select(Author, Title, Year, Journal, Abstract, DOI, Number, Pages, Volume, ISBN, RecordID, Label)

  # Deduplication steps ---------------------------------------

  # ROUND 1: run compare.dedup function and block by Title&Pages OR Title&Author OR Title&Abstract OR DOI
  newpairs = compare.dedup(newdatformatted, blockfld = list(c(2,8), c(1,2), c(2,5), 6), strcmp = TRUE, exclude=c("RecordID", "Label"))

  # Create df of pairs
  dfpairs <- as.data.frame(newpairs$pairs)
  linkedpairs <- dfpairs

  # ROUND 2: run compare.dedup function and block by Author&Year&Pages OR Journal&Volume&Pages or Title&ISBN
  newpairs2 = compare.dedup(newdatformatted, blockfld = list(c(1,3,8), c(4,9,8), c(10,9,8), c(2,10)), strcmp = TRUE, exclude= c("RecordID", "Label"))


  #Create df of pairs
  dfpairs2 <- as.data.frame(newpairs2$pairs)
  linkedpairs2 <- dfpairs2

  # ROUND 3: run compare.dedup function and block by Year&Pages&Volume OR Year&Issue&Volume or Year&Pages&Issue
  newpairs3 = compare.dedup(newdatformatted, blockfld = list(c(3,8,9), c(3,7,9), c(3,8,7)), strcmp = TRUE, exclude=c("RecordID", "Label"))

  #Create df of pairs
  dfpairs3 <- as.data.frame(newpairs3$pairs)
  linkedpairs3 <- dfpairs3

  # ROUND 4: run compare.dedup function and block by Author&Year OR Year&Title OR Title&Volume OR Title&Journal
  newpairs4 = compare.dedup(newdatformatted, blockfld = list(c(1,3), c(3,2), c(2,9), c(2,4)), strcmp = TRUE, exclude=c("RecordID", "Label"))

  # Create df of pairs
  dfpairs4 <- as.data.frame(newpairs4$pairs)
  linkedpairs4 <- dfpairs4

  # Combine all possible pairs
  SeePairs <- rbind(linkedpairs, linkedpairs2, linkedpairs3, linkedpairs4)
  SeePairs <- unique(SeePairs)

  # Obtain metadata for matching pairs
  SeePairs <- SeePairs  %>%
    mutate(Author1 =y$Author[id1]) %>%
    mutate(Author2 =y$Author[id2])

  SeePairs <- SeePairs %>%
    mutate(Title1 =y$Title[id1]) %>%
    mutate(Title2 =y$Title[id2]) %>%
    mutate(Abstract1 =y$Abstract[id1]) %>%
    mutate(Abstract2 =y$Abstract[id2]) %>%
    mutate(DOI1= y$DOI[id1]) %>%
    mutate(DOI2 =y$DOI[id2])

  SeePairs <- SeePairs  %>%
    mutate(Year1=y$Year[id1]) %>%
    mutate(Year2=y$Year[id2]) %>%
    mutate(Number1 =y$Number[id1]) %>%
    mutate(Number2 =y$Number[id2]) %>%
    mutate(Pages1 =y$Pages[id1]) %>%
    mutate(Pages2 =y$Pages[id2]) %>%
    mutate(Volume1 =y$Volume[id1]) %>%
    mutate(Volume2 =y$Volume[id2])

  SeePairs <- SeePairs  %>%
    mutate(Journal1 =y$Journal[id1]) %>%
    mutate(Journal2 =y$Journal[id2]) %>%
    mutate(ISBN1 =y$ISBN[id1]) %>%
    mutate(ISBN2 =y$ISBN[id2]) %>%
    mutate(RecordID1=y$RecordID[id1]) %>%
    mutate(RecordID2 =y$RecordID[id2]) %>%
    mutate(Label1 =y$Label[id1]) %>%
    mutate(Label2 =y$Label[id2])

  SeePairs <- SeePairs %>%
    select(id1, id2, Author1, Author2, Author, Title1, Title2, Title, Abstract1, Abstract2, Abstract, Year1, Year2, Year, Number1, Number2, Number, Pages1, Pages2, Pages, Volume1, Volume2, Volume, Journal1, Journal2, Journal, ISBN, ISBN1, ISBN2, DOI1, DOI2, DOI, RecordID1, RecordID2, Label1, Label2)

  SeePairs <- SeePairs %>%
    mutate(Abstract = ifelse(is.na(Abstract1) & is.na(Abstract2), 0, Abstract)) %>%
    mutate(Pages = ifelse(is.na(Pages1) & is.na(Pages2), 1, Pages)) %>%
    mutate(Volume = ifelse(is.na(Volume1) & is.na(Volume2), 1, Volume)) %>%
    mutate(Number = ifelse(is.na(Number1) & is.na(Number2), 1, Number)) %>%
    mutate(DOI = ifelse(is.na(DOI1) & is.na(DOI2), 0, DOI)) %>%
    mutate(ISBN = ifelse(is.na(ISBN1) & is.na(ISBN2), 0, ISBN))


  # Filter matching pairs to less likely unlikely pairs - make sure we only remove true duplicate matches
  SeePairsFiltered <- SeePairs %>%
    filter(
      (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & ISBN>0.99) |
        (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & Journal>0.6) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50 & Journal>0.6) |
        (Volume >0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.50  & Journal>0.6) |

        (Volume >0.8 & Number>0.8 & Title>0.90 & Abstract>0.90 & Author>0.8) |
        (Volume>0.8 & Pages>0.8 & Title>0.90 & Abstract>0.9 & Author>0.8) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.9 & Author>0.8) |

        (DOI>0.95 & Author>0.75 & Title>0.9) |

        (Title>0.80 & Abstract>0.90 & Volume>0.85 & Journal>0.65 & Author>0.9) |
        (Title>0.90 & Abstract>0.80 & Volume>0.85 & Journal>0.65 & Author>0.9)|

        (Pages>0.8 & Volume>0.8 & Title>0.90 & Abstract>0.8 & Author>0.9 & Journal>0.75) |
        (Pages>0.8 & Number>0.8 & Title>0.90 & Abstract>0.80 & Author>0.9 & Journal>0.75) |
        (Volume>0.8 & Number>0.8 & Title>0.90 & Abstract>0.8 & Author>0.9  & Journal>0.75) |

        (Title>0.9 & Author>0.9 & Abstract>0.9 & Journal >0.7)|
        (Title>0.9 & Author>0.9 & Abstract>0.9 & ISBN >0.99)|

        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & Journal>0.6) |
        (Number>0.9 & Volume>0.9 & Title>0.90 & Author>0.90 & Journal>0.6) |
        (Pages>0.9 & Volume>0.9 & Title>0.90 & Author>0.80 & Journal>0.6) |
        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & ISBN>0.99) |
        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & ISBN>0.99) |
        (Pages>0.9 & Number>0.9 & Title>0.90 & Author>0.80 & ISBN>0.99) |

        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & Journal>0.9) |
        (Number>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & Journal>0.9)|
        (Number>0.8 & Pages>0.8 & Title>0.95 & Author>0.80 & Journal>0.9) |
        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & ISBN>0.99) |
        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & ISBN>0.99) |
        (Pages>0.8 & Volume>0.8 & Title>0.95 & Author>0.80 & ISBN>0.99))


  # Find papers with low matching DOIs - often indicates FALSE positive matches
  SeePairsFilteredDOIBAD <- SeePairsFiltered %>%
    filter(!(is.na(DOI)| DOI ==0 | DOI > 0.99)) %>%
    filter(!(Title > 0.9 & Abstract > 0.9 & (Journal|ISBN > 0.9)))

  # Remove papers with low matching DOIs from filtered matched
  SeePairsFiltered <- SeePairsFiltered %>%
    filter(is.na(DOI)| DOI > 0.99 | DOI == 0 | (Title > 0.9 & Abstract>0.9 & (Journal|ISBN > 0.9)))

  SeePairsFiltered <- unique(SeePairsFiltered)

  # Make year numeric, then find matches where year differs
  SeePairsFiltered$Year1 <- as.numeric(as.character(SeePairsFiltered$Year1))
  SeePairsFiltered$Year2 <- as.numeric(as.character(SeePairsFiltered$Year2))
  YearsDiff <- SeePairsFiltered[which(SeePairsFiltered$Year1 != SeePairsFiltered$Year2),]
  YearsNotVeryDiff1 <- YearsDiff[which(YearsDiff$Year1 == YearsDiff$Year2+1 ),]
  YearsNotVeryDiff2 <- YearsDiff[which(YearsDiff$Year1 == YearsDiff$Year2-1 ),]

  YearsNotVeryDiff <- rbind(YearsNotVeryDiff1, YearsNotVeryDiff2)
  YearsNotVeryDiff <- unique(YearsNotVeryDiff)

  # Identify where year differs >1 and remove from filtered dataset - need to manually deduplicate
  YearsVeryDiff <- YearsDiff[which(!rownames(YearsDiff) %in% rownames(YearsNotVeryDiff)),]

  ManualDedup <- YearsVeryDiff
  SeePairsFiltered <- SeePairsFiltered[which(!rownames(SeePairsFiltered) %in% rownames(YearsVeryDiff)),]

  SeePairsFiltered <- unique(SeePairsFiltered)

  SeePairsFiltered$RecordID1 <- as.character(SeePairsFiltered$RecordID1)
  SeePairsFiltered$RecordID2 <- as.character(SeePairsFiltered$RecordID2)

  # Remove duplicate papers ----------------------------------------------

  # Get original data ready for removing duplicates
  dedupdat <- newdatformatted
  dedupdat$RecordID <- as.character(dedupdat$RecordID)

  # Keep record1 and remove record2
  linkedpairslabelledkeep1 <- SeePairsFiltered
  removerefslabelled <- unique(linkedpairslabelledkeep1$RecordID2)
  dedupdat <- dedupdat[which(!dedupdat$RecordID %in% removerefslabelled),]

  # Get list of references removed
  checkremovedalreadyID <- c(removerefslabelled)
  checkremovedalreadyID <-unique(checkremovedalreadyID)

  # Get potential duplicates for manual deduplication
  MaybePairs <- SeePairs %>%
    filter(RecordID1 %in% dedupdat$RecordID &
             RecordID2 %in% dedupdat$RecordID) %>%
    filter(DOI > 0.99 |
             Title>0.85 & Author>0.75 |
             Title>0.80 & Abstract>0.80 |
             Title>0.80 & ISBN>0.99 |
             Title>0.80 & Journal>0.80)

  # Add in problem DOI matching pairs and different year data in ManualDedup
  MaybePairs <- rbind(MaybePairs, ManualDedup, SeePairsFilteredDOIBAD)
  MaybePairs <- unique(MaybePairs)

  y$RecordID <- as.character(y$RecordID)
  dedupdat$RecordID <- as.character(dedupdat$RecordID)

  uniquedat <- x %>%
    filter(RecordID %in% dedupdat$RecordID)

  # #Remove one recordID1 when 2 match the same recordID2
  # additional <- SeePairsFiltered %>%
  #   filter(!Label1 == LabelKeep) %>%
  #   filter(RecordID1 %in% uniquedat$RecordID) %>%
  #   group_by(RecordID2) %>%
  #   mutate(N = length(unique(RecordID1))) %>%
  #   filter(N>1) %>%
  #   select(-N) %>%
  #   mutate(RecordID1 = first(RecordID1)) %>%
  #   mutate(Label1 = "additionaldup") %>%
  #   ungroup()
  #
  # uniquedat <- uniquedat %>%
  #   filter(!RecordID %in% unique(additional$RecordID1))

  MaybePairs <- MaybePairs %>%
    filter(RecordID1 %in% uniquedat$RecordID &
             RecordID2 %in% uniquedat$RecordID)

  # SeePairsFiltered <- rbind(SeePairsFiltered, additional)
  SeePairsFiltered <- as.data.frame(SeePairsFiltered)

  removedat <- x
  removedat <-removedat %>%
    filter(!RecordID %in% uniquedat$RecordID)


  return(list("ManualDedup" = MaybePairs,
              "Unique" = uniquedat,
              "TruePairs" = SeePairsFiltered,
              "DuplicateRefsRemoved" = removedat))

}

#' @export
get_labelled_results <- function (x,
                                 LabelKeep = ""){



  result1 <- dedup_labelled_step(x,
                                 LabelKeep = LabelKeep)
  manual1 <- result1$ManualDedup
  unique1 <- result1$Unique
  pairs1 <- result1$TruePairs
  allmatches1 <- result1$PotentialPairs
  removed1 <- result1$DuplicateRefsRemoved

  unique1<-unique1 %>%
    select(RecordID, Author, Year, Title, Journal, Abstract, Volume, Number, Pages, DOI, Label, ISBN)

  manual_otherway <- manual1 %>%
    mutate(id1 = RecordID2) %>%
    mutate(id2= RecordID1) %>%
    rename(Title1 = Title2,
           Title2 = Title1,
           Author1 = Author2,
           Author2 = Author1,
           Year1 = Year2,
           Year2 = Year1,
           Journal1 = Journal2,
           Journal2 = Journal1,
           Abstract1 = Abstract2,
           Abstract2 = Abstract1,
           Volume1 = Volume2,
           Volume2 = Volume1,
           Number1 = Number2,
           Number2 = Number1,
           Pages1 = Pages2,
           Pages2 = Pages1,
           DOI1 = DOI2,
           DOI2 = DOI1,
           Label1 = Label2,
           Label2 = Label1,
           ISBN1 = ISBN2,
           ISBN2 = ISBN1,
           RecordID1 = RecordID2,
           RecordID2 = RecordID1
    )


  result2 <- get_dedup_results(unique1)

  manual2id <- result2$ManualDedup
  manual2 <-result2$ManualDedup  %>%
    mutate(id1 = RecordID1,
           id2 = RecordID2)

  manual2 <- anti_join(manual2, manual_otherway)

  unique2 <- result2$Unique
  pairs2 <- result2$TruePairs
  allmatches2 <- result2$PotentialPairs
  removed2 <- result2$DuplicateRefsRemoved

  unique2<-unique2 %>%
    select(RecordID, Author, Year, Title, Journal, Abstract, Volume, Number, Pages, DOI, Label, ISBN)

  manual <- rbind(manual1, manual2)
  manual <- manual %>%
    group_by(RecordID1, RecordID2) %>%
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

  print(length(unique1$RecordID))
  print(length(unique2$RecordID))

  return(list("ManualDedup" = manual,
              "Unique" = unique,
              "TruePairs" = pairs,
              "DuplicateRefsRemoved" = removed))
}


#' @export
get_unique <-function(x,
                      Author = "Author",
                      Title = "Title",
                      Year = "Year",
                      Journal = "Journal",
                      ISBN = "ISBN",
                      Abstract = "Abstract",
                      DOI = "DOI",
                      Number = "Number",
                      Pages = "Pages",
                      Volume = "Volume",
                      RecordID = "RecordID",
                      Label = "Label"){

dedup_results <- get_dedup_results(x)

return(dedup_results$Unique)

}

#' @export
get_dups_removed <-function(x,
                      Author = "Author",
                      Title = "Title",
                      Year = "Year",
                      Journal = "Journal",
                      ISBN = "ISBN",
                      Abstract = "Abstract",
                      DOI = "DOI",
                      Number = "Number",
                      Pages = "Pages",
                      Volume = "Volume",
                      RecordID = "RecordID",
                      Label = "Label"){

dedup_results <- get_dedup_results(x)

return(dedup_results$DuplicateRefsRemoved)
}

#' @export
get_pairs <-function(x,
                            Author = "Author",
                            Title = "Title",
                            Year = "Year",
                            Journal = "Journal",
                            ISBN = "ISBN",
                            Abstract = "Abstract",
                            DOI = "DOI",
                            Number = "Number",
                            Pages = "Pages",
                            Volume = "Volume",
                            RecordID = "RecordID",
                            Label = "Label"){

  dedup_results <- get_dedup_results(x)

  return(dedup_results$TruePairs)
}

#' @export
get_potential_pairs <-function(x,
                         Author = "Author",
                         Title = "Title",
                         Year = "Year",
                         Journal = "Journal",
                         ISBN = "ISBN",
                         Abstract = "Abstract",
                         DOI = "DOI",
                         Number = "Number",
                         Pages = "Pages",
                         Volume = "Volume",
                         RecordID = "RecordID",
                         Label = "Label"){

  dedup_results <- get_dedup_results(x)

  return(dedup_results$ManualDedup)
}
