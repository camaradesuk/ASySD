#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @import synthesisr
#' @param citations A dataframe containing citations - usually post-deduplication
#' @param type export type
#' @param filename output file name
#' @return file export
write_citations <- function(citations, type=c("ris", "txt", "csv", "bib"), filename){

  if(type == "txt"){

    refs <- citations %>%
      mutate("Reference Type" = "Journal Article") %>%
      mutate("ISBN/ISSN" = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
      rename("Custom 1" = duplicate_id,
             "Author" = author,
             "Title" = title,
             "Volume" = volume,
             "Number" = number,
             "Label" = label,
             "Year" = year,
             "Abstract" = abstract,
             "Pages" = pages,
             "DOI" = doi,
             "Name of Database" = source,
             "Secondary Title" = journal) %>%
      select("Reference Type", "Author", "Year",
             "Secondary Title", "DOI", "Title",
             "Pages", "Volume", "Number", "Abstract",
             "Custom 1", "ISBN/ISSN", "Label", "Name of Database") %>%
      mutate(Abstract = gsub("\\r\\n|\\r|\\n", "", Abstract))

    refs[] <- lapply(refs, function(x) gsub("\\r\\n|\\r|\\n", "", x))

    write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "csv"){

    refs <- citations %>%
      rename(Authors = author,
             Title = title,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      mutate(Url = "",
             AuthorAddress = "",
             AlternateName = "",
             ReferenceType = "",
             CustomId = duplicate_id,
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

    write.csv(refs, filename,
              col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "ris"){

    citations <- as.data.frame(citations)
    synthesisr::write_refs(citations,
                           format = "ris",
                           file = filename
    )
  } else if(type == "bib"){

    citations <- as.data.frame(citations)
    refs <- synthesisr::write_refs(citations, format = "bib",
                                   file = filename)
  }
}

#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @import synthesisr
#' @param citations A dataframe containing citations - usually post-deduplication
#' @param type export type
#' @param filename output file name
#' @return file export
write_citations_app <- function(citations, type=c("ris", "txt", "csv", "bib"), filename){

  if(type == "txt"){


    citations$`Custom 2` <- ""

    if("flag" %in% colnames(citations))
    {
      citations$`Custom 2` <- citations$flag
    }


    refs <- citations %>%
      mutate("Reference Type" = "Journal Article") %>%
      mutate("ISBN/ISSN" = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
      rename("Custom 1" = duplicate_id,
             "Author" = author,
             "Title" = title,
             "Volume" = volume,
             "Number" = number,
             "Label" = label,
             "Year" = year,
             "Abstract" = abstract,
             "Pages" = pages,
             "DOI" = doi,
             "Name of Database" = source,
             "Secondary Title" = journal) %>%
      select("Reference Type", "Author", "Year",
             "Secondary Title", "DOI", "Title",
             "Pages", "Volume", "Number", "Abstract",
             "Custom 1", "ISBN/ISSN", "Label", "Name of Database") %>%
      mutate(Abstract = gsub("\\r\\n|\\r|\\n", "", Abstract))

    write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")


    refs[] <- lapply(refs, function(x) gsub("\\r\\n|\\r|\\n", "", x))

    write.table(refs, filename, sep="\t",
                col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "csv"){

    citations[] <- lapply(citations, function(x) gsub("\\r\\n|\\r|\\n", "", x))

    refs <- citations %>%
      rename(Authors = author,
             Title = title,
             Abstract = abstract,
             Year = year,
             DOI= doi,
             PublicationName = journal)  %>%
      mutate(Url = "",
             AuthorAddress = "",
             AlternateName = "",
             ReferenceType = "",
             CustomId = duplicate_id,
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

    write.csv(refs, filename,
              col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "ris"){

    citations[] <- lapply(citations, function(x) gsub("\\r\\n|\\r|\\n", "", x))

    citations <- as.data.frame(citations)
    citations$database <- citations$duplicate_id
    citations$notes <- ""

    if("flag" %in% colnames(citations))
    {
      citations$notes <- citations$flag
    }

    synthesisr::write_refs(citations,
                           format = "ris",
                           file = filename
    )
  } else if(type == "bib"){

    citations[] <- lapply(citations, function(x) gsub("\\r\\n|\\r|\\n", "", x))

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


