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
