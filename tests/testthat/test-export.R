test_that("write_citations exports data frame correctly in txt format", {
  citations <- data.frame(
    duplicate_id = 1:3,
    author = c("Author 1", "Author 2", "Author 3"),
    title = c("Title 1", "Title 2", "Title 3"),
    year = c(2020, 2021, 2022),
    journal = c("Journal 1", "Journal 2", "Journal 3"),
    abstract = c("Abstract 1", "Abstract 2", "Abstract 3"),
    doi = c("DOI 1", "DOI 2", "DOI 3"),
    number = c(1, 2, 3),
    url = c("", "", ""),
    pages = c("1-10", "20-30", "40-50"),
    volume = c(10, 20, 30),
    isbn = c("ISBN 1", "ISBN 2", "ISBN 3"),
    source = c("Source 1", "Source 2", "Source 3"),
    label = c("Label1", "Label2", "Label3")
  )

  filename <- "test_export.txt"
  write_citations(citations, "txt", filename)
  # Check if the file is created
  expect_true(file.exists(filename))
  # Check if the file content matches the expected content
  expected_content <- readLines(filename)
  expect_equal(length(expected_content), 4) # Check header + 3 rows

  exported <- utils::read.delim(filename, sep = "\t", check.names = FALSE)
  expect_true(all(c("Keywords", "Accession Number", "URL") %in% names(exported)))
  expect_equal(exported$Keywords, c("", "", ""))
  expect_equal(exported$`Accession Number`, c("", "", ""))
  expect_equal(exported$URL, c("", "", ""))
})

test_that("write_citations_app txt export tolerates missing optional columns", {
  citations <- data.frame(
    duplicate_id = 1:2,
    author = c("Author 1", "Author 2"),
    title = c("Title 1", "Title 2"),
    year = c(2020, 2021),
    journal = c("Journal 1", "Journal 2"),
    abstract = c("Abstract 1", "Abstract 2"),
    doi = c("DOI 1", "DOI 2"),
    number = c(1, 2),
    pages = c("1-10", "20-30"),
    volume = c(10, 20),
    isbn = c("ISBN 1", "ISBN 2"),
    source = c("Source 1", "Source 2"),
    label = c("Label1", "Label2"),
    file_name = c("", "")
  )

  filename <- "test_export_app.txt"
  write_citations_app(citations, "txt", filename)
  expect_true(file.exists(filename))

  exported <- utils::read.delim(filename, sep = "\t", check.names = FALSE)
  expect_true(all(c("Keywords", "Accession Number", "URL") %in% names(exported)))
  expect_equal(exported$Keywords, c("", ""))
  expect_equal(exported$`Accession Number`, c("", ""))
  expect_equal(exported$URL, c("", ""))
})

test_that("write_citations txt export retains keywords when provided", {
  citations <- data.frame(
    duplicate_id = 1,
    author = "Author 1",
    title = "Title 1",
    year = 2020,
    journal = "Journal 1",
    abstract = "Abstract 1",
    doi = "DOI 1",
    number = 1,
    pages = "1-10",
    volume = 10,
    isbn = "ISBN 1",
    source = "Source 1",
    label = "Label1",
    keywords = "alpha; beta"
  )

  filename <- "test_export_keywords.txt"
  write_citations(citations, "txt", filename)

  exported <- utils::read.delim(filename, sep = "\t", check.names = FALSE)
  expect_true("Keywords" %in% names(exported))
  expect_equal(exported$Keywords, "alpha; beta")
})

test_that("write_citations exports data frame correctly in csv format", {
  citations <- data.frame(
    duplicate_id = 1:3,
    author = c("Author 1", "Author 2", "Author 3"),
    title = c("Title 1", "Title 2", "Title 3"),
    year = c(2020, 2021, 2022),
    journal = c("Journal 1", "Journal 2", "Journal 3"),
    abstract = c("Abstract 1", "Abstract 2", "Abstract 3"),
    doi = c("DOI 1", "DOI 2", "DOI 3"),
    number = c(1, 2, 3),
    url = c("", "", ""),
    pages = c("1-10", "20-30", "40-50"),
    volume = c(10, 20, 30),
    isbn = c("ISBN 1", "ISBN 2", "ISBN 3"),
    source = c("Source 1", "Source 2", "Source 3"),
    label = c("Label1", "Label2", "Label3")
  )

  filename <- "test_export.csv"
  write_citations(citations, "csv", filename)
  # Check if the file is created
  expect_true(file.exists(filename))
  # Check if the file content matches the expected content
  expected_content <- readLines(filename)
  expect_equal(length(expected_content), 4) # Check header + 3 rows
  # Add more specific checks here if needed
})

test_that("write_citations exports data frame correctly in syrf csv format", {
  citations <- data.frame(
    duplicate_id = 1:3,
    author = c("Author 1", "Author 2", "Author 3"),
    title = c("Title 1", "Title 2", "Title 3"),
    year = c(2020, 2021, 2022),
    journal = c("Journal 1", "Journal 2", "Journal 3"),
    abstract = c("Abstract 1", "Abstract 2", "Abstract 3"),
    doi = c("DOI 1", "DOI 2", "DOI 3"),
    number = c(1, 2, 3),
    url = c("", "", ""),
    pages = c("1-10", "20-30", "40-50"),
    volume = c(10, 20, 30),
    isbn = c("ISBN 1", "ISBN 2", "ISBN 3"),
    source = c("Source 1", "Source 2", "Source 3"),
    label = c("Label1", "Label2", "Label3")
  )

  filename <- "test_export.csv"
  write_citations(citations, "syrf_csv", filename)
  # Check if the file is created
  expect_true(file.exists(filename))
  # Check if the file content matches the expected content
  expected_content <- readLines(filename)
  expect_equal(length(expected_content), 4) # Check header + 3 rows
  # Add more specific checks here if needed
})

test_that("write_citations exports data frame correctly in ris format", {
  citations <- data.frame(
    duplicate_id = 1:3,
    author = c("Author 1", "Author 2", "Author 3"),
    title = c("Title 1", "Title 2", "Title 3"),
    year = c(2020, 2021, 2022),
    journal = c("Journal 1", "Journal 2", "Journal 3"),
    abstract = c("Abstract 1", "Abstract 2", "Abstract 3"),
    doi = c("DOI 1", "DOI 2", "DOI 3"),
    number = c(1, 2, 3),
    url = c("", "", ""),
    pages = c("1-10", "20-30", "40-50"),
    volume = c(10, 20, 30),
    isbn = c("ISBN 1", "ISBN 2", "ISBN 3"),
    source = c("Source 1", "Source 2", "Source 3"),
    label = c("Label1", "Label2", "Label3")
  )

  filename <- "test_export.ris"
  write_citations(citations, "ris", filename)
  # Check if the file is created
  expect_true(file.exists(filename))
  # Check if the file content matches the expected content
  expected_content <- readLines(filename)
})

test_that("write_citations exports data frame correctly in bib format", {
   citations <- data.frame(
    duplicate_id = 1:3,
    author = c("Author 1", "Author 2", "Author 3"),
    title = c("Title 1", "Title 2", "Title 3"),
    year = c(2020, 2021, 2022),
    journal = c("Journal 1", "Journal 2", "Journal 3"),
    abstract = c("Abstract 1", "Abstract 2", "Abstract 3"),
    doi = c("DOI 1", "DOI 2", "DOI 3"),
    number = c(1, 2, 3),
    url = c("", "", ""),
    pages = c("1-10", "20-30", "40-50"),
    volume = c(10, 20, 30),
    isbn = c("ISBN 1", "ISBN 2", "ISBN 3"),
    source = c("Source 1", "Source 2", "Source 3"),
    label = c("Label1", "Label2", "Label3")
  )

  filename <- "test_export.bib"
  write_citations(citations, "bib", filename)
  # Check if the file is created
  expect_true(file.exists(filename))
  # Check if the file content matches the expected content
  expected_content <- readLines(filename)
})
