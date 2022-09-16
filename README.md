# hydrographr <img src="man/figures/hydrographr.svg" align="right" />

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)

`hydrographr` provides a collection of `R` function wrappers for GDAL and GRASS-GIS functions to efficiently work with Hydrography90m spatial data. The package is currently under development. More and more useful functions will be made available over time here. 


### Next steps in package development

- Test first test function `get_subcID()`
  * Check link between the .sh script in '*/inst/sh' and the `processx::run()` call
  * Is making scripts executable always necessary? (`system(paste0('chmod u=x', x))`)
- Generate light weight test data set for implementation in '*/inst/extdata'
- Implementation and documentation (header, etc.) of further functions
- Generalization of functions, now functions cover very specific cases
- Update of gh-page
- Establish common guideline for package development
  * naming of branches, variables, functions
  * workflow to avoid conflicts
  * version of package
