# Specify path to search file
path <- "test.xml"

# Load citations from Endnote, CSV, or Text file
searchfile <- load_search(path, "endnote")
# searchfile <- load_refs(path, "csv")
# searchfile <- load_refs(path, "txt")

# Get unique citations after deduplication
unique_citations <- get_unique(searchfile)

# Get list of extra potential matching citation pairs to assess manually
potential_pairs <- get_potential_pairs(searchfile)

# Get dataframe of duplicate citations removed from search
removed_citations <- get_dups_removed(searchfile)

# Get dataframe of matching duplicate pairs
