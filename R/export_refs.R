# utils::globalVariables(c("record_ids", "."))

#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @import synthesisr
#' @importFrom utils read.csv read.table
#' @importFrom rlang .data
#' @param citations A data frame containing citations - usually post-deduplication
#' @param type export type
#' @param filename output file name
#' @return file export
write_citations <- function(citations, type=c("ris", "txt", "csv", "bib"), filename){

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source')
  citations[cols_to_modify] <- lapply(citations[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))

  if(type == "txt"){

    refs <- citations %>%
      mutate("Reference Type" = "Journal Article") %>%
      mutate("ISBN/ISSN" =  isbn) %>%
      rename(`Custom 1` = duplicate_id,
             Author = author,
             Title = title,
             URL=url,
             Volume = volume,
             Number = number,
             Label = label,
             Year = year,
             Abstract = abstract,
             Pages = pages,
             DOI = doi,
             `Name of Database` = source,
             `Secondary Title` = journal) %>%
      select(`Reference Type`, Author, Year,
             `Secondary Title`, DOI, Title,
              Pages, Volume, Number, Abstract,
             `Custom 1`, `ISBN/ISSN`, Label, `Name of Database`, URL)

    write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "csv"){

    refs <- citations %>%
      rename(Authors = author,
             Title = title,
             Url = url,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      mutate(Url = "",
             AuthorAddress = "",
             AlternateName = "",
             ReferenceType = "",
             CustomId = .data$duplicate_id,
             Keywords = "",
             PdfRelativePath = paste0(.data$duplicate_id, ".pdf")) %>%
      mutate(URL = ifelse("url" %in% names(.), url, NA)) %>%
      select(Title,
             Authors,
             PublicationName,
             AlternateName,
             Abstract,
             Url,
             AuthorAddress,
             Year,
             DOI,
             ReferenceType,
             Keywords,
             PdfRelativePath,
             CustomId,
             URL)

    write.csv(refs, filename,
              col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "ris"){

    citations$source_type <- "JOUR" #for RIS import to work
    citations <- citations %>% select(source_type , everything()) %>%
      rename(issue = number) %>%
      select(-isbn)
    citations <- tidyr::separate(citations, pages, into = c("start_page", "end_page"), sep = "-", convert = TRUE)
    synthesisr::write_refs(citations,
                           format = "ris",
                           file = filename
    )
  } else if(type == "csv"){

    write.csv(citations, filename, row.names = F, quote=TRUE, na="")

  } else if(type == "bib"){

    citations <- as.data.frame(citations)
    refs <- synthesisr::write_refs(citations, format = "bib",
                                   file = filename)
  }
}

#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom utils read.csv read.table
#' @import synthesisr
#' @param citations A dataframe containing citations - usually post-deduplication
#' @param type export type
#' @param filename output file name
#' @return file export
write_citations_app <- function(citations, type=c("ris", "txt", "csv", "bib"), filename){

  cols_to_modify <-  c('title', 'year', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label', 'source')
  citations[cols_to_modify] <- lapply(citations[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))


  citations <- citations %>%
    select(-file_name)

  if(type == "txt"){


    citations$`Custom 2` <- ""

    if("flag" %in% colnames(citations))
    {
      citations$`Custom 2` <- citations$flag
    }


    refs <- citations %>%
      mutate(`Reference Type` = "Journal Article") %>%
      mutate(`ISBN/ISSN` = isbn) %>%
      rename(`Custom 1` = duplicate_id,
             URL=url,
             Author = author,
             Title = title,
             Volume = volume,
             Number = number,
             Label = label,
             Year = year,
             Abstract = abstract,
             Pages = pages,
             DOI = doi,
             `Name of Database` = source,
             `Secondary Title` = journal) %>%
      select("Reference Type", Author, Year,
             "Secondary Title", DOI, Title,
             Pages, Volume, Number, Abstract,
             "Custom 1", "Custom 2", "ISBN/ISSN", Label, "Name of Database", URL)

    utils::write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")

    utils::write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "syrf_csv"){

    refs <- citations %>%
      rename(Authors = author,
             Title = title,
             Url=url,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      mutate(AuthorAddress = "",
             AlternateName = "",
             ReferenceType = "",
             CustomId = .data$duplicate_id,
             Keywords = "",
             PdfRelativePath = paste0(duplicate_id, ".pdf")) %>%
      select(Title,
             Authors,
             PublicationName,
             AlternateName,
             Abstract,
             Url,
             AuthorAddress,
             Year,
             DOI,
             ReferenceType,
             Keywords,
             PdfRelativePath,
             CustomId)

    utils::write.csv(refs, filename, row.names = F, quote=TRUE, na="")

  }
  else if(type == "csv"){

    utils::write.csv(citations, filename, row.names = F, quote=TRUE, na="")

  }

  else if(type == "ris"){

    citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    if("flag" %in% colnames(citations))
    {
      citations$notes <- citations$flag
    }

    citations$source_type <- "JOUR" #for RIS import to work
    citations <- citations %>% select(source_type , everything()) %>%
      select(-isbn)
    citations <- tidyr::separate(citations, .data$pages, into = c("start_page", "end_page"), sep = "-", convert = TRUE)
    citations$accession_number <- citations$accession

    synthesisr::write_refs(citations,
                           format = "ris",
                           file = filename
    )
  } else if(type == "bib"){

    citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    if("flag" %in% colnames(citations))
    {
      citations$notes <- citations$flag
    }

    refs <- synthesisr::write_refs(citations, format = "bib",
                                   file = filename)
  }
}


