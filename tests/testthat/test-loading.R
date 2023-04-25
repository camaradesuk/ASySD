test_that("BibTeX file is read in correctly", {
  path <- "zotero_export.bib"
  name <- "zotero_export.bib"
  df1 <- load_multi_search(path, name, "bib")
  df2 <- load_search(path, "bib")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

})

# Test for reading in a CSV file exported from Zotero
test_that("Zotero CSV file is read in correctly", {
  path <- "zotero_export.csv"
  name <- "zotero_export.csv"
  df1 <- load_multi_search(path, name, "zotero_csv")
  df2 <- load_search(path, "zotero_csv")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

})
# Test for reading in a RIS file
test_that("RIS file is read in correctly", {
  path <- "zotero_export.ris"
  name <- "zotero_export.ris"
  df1 <- load_multi_search(path, name, "ris")
  df2 <- load_search(path, "ris")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

})

# Test for reading in an XML file exported from EndNote
test_that("EndNote XML file from Zotero is read in correctly", {
  path <- "zotero_export.xml"
  name <- "zotero_export.xml"
  df1 <- load_multi_search(path, name, "endnote")
  df2 <- load_search(path, "endnote")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

})
# Test for reading in an XML file exported from EndNote
test_that("EndNote XML file from Endnote is read in correctly", {
  path <- "endnote_export.xml"
  name <- "endnote_export.xml"
  df1 <- load_multi_search(path, name, "endnote")
  df2 <- load_search(path, "endnote")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

})

# Test for reading in an XML file exported from EndNote
test_that("EndNote XML file from Mendeley is read in correctly", {
  path <- "mendeley_export.xml"
  name <- "mendeley_export.xml"
  df1 <- load_multi_search(path, name, "endnote")
  df2 <- load_search(path, "endnote")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

})
