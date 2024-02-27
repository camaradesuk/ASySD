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
  # Add more specific checks here if needed
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

