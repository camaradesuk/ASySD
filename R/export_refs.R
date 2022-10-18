#' This function writes citation data to disk in different formats
#' @export
#' @import dplyr
#' @import synthesisr
#' @param citations A dataframe containing citations - usually post-deduplication
#' @param type export type
#' @return file export
write_citations <- function(citations, type=c("ris", "endnote-tab", "syrf-csv", "csv", "bibtex"), filename){

  if(type == "endnote-tab"){

  refs <- citations %>%
    mutate("Reference Type" = "Journal Article") %>%
    mutate(ISBN = gsub("\\r\\n|\\r|\\n", "", isbn)) %>%
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
           "Secondary Title" = journal) %>%
    select("Reference Type", "Author", "Year",
           "Secondary Title", "DOI", "Title",
           "Pages", "Volume", "Number", "Abstract",
           "Custom 1", "ISBN", "Label") %>%
    mutate(Abstract = gsub("\\r\\n|\\r|\\n", "", Abstract))

  write.table(refs, filename, sep="\t",
              col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "syrf-csv"){

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
             PdfRelativePath)

    write.csv(refs, filename,
              col.names=TRUE, row.names = F, quote=FALSE, na="")

  } else if(type == "ris"){

    citations <- as.data.frame(citations)
    synthesisr::write_refs(citations,
               format = "ris",
               file = TRUE
    )
  } else if(type == "bibtex"){

    citations <- as.data.frame(citations)
    refs <- synthesisr::write_refs(citations, format = "bib",
                                   file = TRUE)
  }
}
