# ASySD 0.2.1

* Minor bug fix in app logic
* Chnage of wording when generating unique ids (based on row number not row name) 
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
