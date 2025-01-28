test_that("BibTeX file is read in correctly", {
  path <- "zotero_export.bib"
  name <- "zotero_export.bib"
  df1 <- load_multi_search(path, name, "bib")
  df2 <- load_search(path, "bib")

  # Check columns
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 1000)
  expect_equal(nrow(df2), 1000)
})

# Test for reading in a CSV file exported from Zotero
test_that("Zotero CSV file is read in correctly", {
  path <- "zotero_export.csv"
  name <- "zotero_export.csv"
  df1 <- load_multi_search(path, name, "zotero_csv")
  df2 <- load_search(path, "zotero_csv")

  # Check columns
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 1233)
  expect_equal(nrow(df2), 1233)
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

  # Check rows
  expect_equal(nrow(df1), 1000)
  expect_equal(nrow(df2), 1000)
})

# Test for reading in an XML file exported from Zotero
test_that("EndNote XML file from Zotero is read in correctly", {
  path <- "zotero_export.xml"
  name <- "zotero_export.xml"
  df1 <- load_multi_search(path, name, "endnote")
  df2 <- load_search(path, "endnote")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 1233)
  expect_equal(nrow(df2), 1233)
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

  # Check rows
  expect_equal(nrow(df1), 1233)
  expect_equal(nrow(df2), 1233)
})

# Test for reading in an XML file exported from Mendeley
test_that("EndNote XML file from Mendeley is read in correctly", {
  path <- "mendeley_export.xml"
  name <- "mendeley_export.xml"
  df1 <- load_multi_search(path, name, "endnote")
  df2 <- load_search(path, "endnote")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 1233)
  expect_equal(nrow(df2), 1233)
})

# Test for reading in an XML file exported from Mendeley
test_that("RIS file from Endnote is read in correctly", {
  path <- "ris_export.txt"
  name <- "ris_export.txt"
  df1 <- load_multi_search(path, name, "ris")
  df2 <- load_search(path, "ris")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 69)
  expect_equal(nrow(df2), 69)
})

# Test for reading in an NBIB file from PubMed
test_that("NBIB file from PubMed is read in correctly", {
  path <- "pubmed_test.nbib"
  name <- "pubmed_test.nbib"
  df1 <- load_multi_search(path, name, "bib")
  df2 <- load_search(path, "bib")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 3286)
  expect_equal(nrow(df2), 3286)
})

# Test for reading in an RIS file from PubMed
test_that("RIS file from PubMed is read in correctly", {
  path <- "pubmed_test.txt"
  name <- "pubmed_test.txt"
  df1 <- load_multi_search(path, name, "ris")
  df2 <- load_search(path, "ris")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 3286)
  expect_equal(nrow(df2), 3286)
})

# Test for reading in an RIS file from WoS
test_that("RIS file from WoS is read in correctly", {
  path <- "wos_test.ris"
  name <- "wos_test.ris"
  df1 <- load_multi_search(path, name, "ris")
  df2 <- load_search(path, "ris")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 50)
  expect_equal(nrow(df2), 50)
})

# Test for reading in an RIS file from Scopus
test_that("RIS file from Scopus is read in correctly", {
  path <- "scopus.ris"
  name <- "scopus.ris"
  df1 <- load_multi_search(path, name, "ris")
  df2 <- load_search(path, "ris")

  # Add your expectations here
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df1)))
  expect_true(all(c("author", "year", "journal", "doi", "title", "pages", "volume", "number", "abstract", "record_id", "isbn", "label", "source") %in% colnames(df2)))

  # Check rows
  expect_equal(nrow(df1), 10)
  expect_equal(nrow(df2), 10)
})

