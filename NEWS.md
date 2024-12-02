# ASySD 0.4.1

* Improvements to load_search() to allow for multiple files and without the need to explicity add the method argument

# ASySD 0.4.0

* Addition of manual_dedup_shiny() function to aid manual deduplication process in the package

# ASySD 0.3.5

* Simplification of merge functions in deduplication process
* Bug fix for extra_merge_fields functionality which prevented correct merging of record_ids
* New unit test for manual deduplication

# ASySD 0.3.4

* Simplification of deduplication functions - broken down into smaller functions and called
* Added functionality for batch deduplication + accompanying vignette
* Automatically detecting whether shiny app in use or not (for deduplication functions) and removing redundant arguments
* Improvements to documentation and examples

# ASySD 0.3.3

* Improvements to export metadata - now retains URLs in app export
* Improved error handling when 0 duplicates present
* Additional unit tests for package stability

# ASySD 0.3.2

* Bug fix in RIS export 
* Improved error handling for manual deduplication (in app)

# ASySD 0.3.1

* Bug fix in multi-search upload for RIS

# ASySD 0.3.0

* Streamlining merging / keep_one_unique functions to adapt to new duplicate id generation logic (igraph)
* Ensuring extra_merge_fields supported throughout merging and manual dedup
* Improvements to bib imports via databases (bibliometrix data import required)
* Removing documentation for internal functions
* Simplifying and removing redundant functions

# ASySD 0.2.1

* Minor bug fix in app logic
* Change of wording when generating unique ids (based on row number not row name) 
* Adding shiny progress updates back in 

# ASySD 0.2.0
* Improvements to documentation files (CRAN submission preparation)
* Changed default deduplication type to merged
* Improved bib imports via bibliometrix and field code conversions
* Improvements to manual deduplication process - no need to go through entire deduplication procedure again 
* Improvements to merging process / duplicate_id selection by building a network of matching records using graph theory (via igraph package)
* Shiny app improvements
  - Added helper pop-ups
  - Added flag symbols to indicate when a pair of citations have been flagged for later review in manual_dedup
  - Bug fix in selection logic when selecting a label / source to keep 
  - Aesthetic changes (colour highlight in tables changed to align with colour scheme)

# ASySD 0.1.2
* Added new dedup_citations argument for show_unknown_tags (logical) to specify whether merged fields should have
"unknown" elements for sources / labels [TRUE] or not [FALSE]
* Put a shiny alert with redirect notice on old RDedup web link to Shiny app

# ASySD 0.1.1
* Added shiny_progress argument to allow loading bar on shiny app
* Bug fixes for multiple uploads to shiny app
* Bug fixes in RIS import

# ASySD 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Bug fix for parallel processing errors (on windows OS)
* CRAN preparation - small fixes to ensure CMD check pass
* Added multi-search loading functions for shiny app
* Added additional import options to load_search() functions, including RIS
* Added documentation (user guide and about page) to shiny app
* Enhanced error handling and user messages
* Improved unit tests for package stability
