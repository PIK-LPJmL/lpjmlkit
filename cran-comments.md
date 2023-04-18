## CHANGES

* file URIs removed from inst/README.md
* currently there is no publication of lpjmlkit that could be added to
DESCRIPTION, if that changes it will be added
* unexecutable code removed from example sections of functions
`detect_io_type()`, `read_io()`, `set_header_item()`, `transform()` and
`run_lpjml()`
* missing Rd-tag return added to `dim.LPJmLData()`, `dimnames.LPJmLData()`,
`length.LPJmLData()`, `plot.LPJmLData()` and `summary.LPJmLData()` functions
* dontrun{} removed from functions where possible: `set_header_item()`
* `with_par()` added to `plot.LPJmLData()` to handle setting change of par
options
* json parsing tests are skipped on Mac M1 systems
* links to raster and terra packages fixed