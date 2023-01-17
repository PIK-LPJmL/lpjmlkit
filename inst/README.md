## Overview

### **[LPJmL Runner :runner:](./vignettes/lpjml-runner.md)**  to perform LPJmL simulations <sub><sup>[**PDF**](./vignettes/lpjml-runner.pdf)</sup></sub>
- :writing_hand: [`write_config()`](./vignettes/lpjml-runner.md#1-clipboard-define-a-table-of-modified-configuration-parameters) write config.json files using a tibble with parameters to be changed and a base lpjml.js file
- :mag: [`check_config()`](./vignettes/lpjml-runner.md#2-writing_hand-create-corresponding-configuration-files) check if generated config.json files are valid for LPJmL simulations
- :arrow_forward: [`run_lpjml()`](./vignettes/lpjml-runner.md#4-arrow_forward-run-or-rocket-submit-lpjml) run LPJmL directly (e.g. single cell simulations) or :rocket: [`submit_lpjml()`](./vignettes/lpjml-runner.md#4-arrow_forward-run-or-rocket-submit-lpjml) to SLURM (e.g. global simulations)

### **[LPJmL Data :floppy_disk: ](./vignettes/lpjml-data.md)** for reading and processing LPJmL data <sub><sup>[**PDF**](./vignettes/lpjml-data.pdf)</sup></sub>
- [`read_io()`](./vignettes/lpjml-data.md#1-book-data-reading-function-read_io) read LPJmL input and output as an [`LPJmLData`](/vignettes/lpjml-data.md#2-file_folder-data-class-lpjmldata) object, containing the data array and LPJmLMetaData
    - :chart_with_upwards_trend: [`plot()`](./vignettes/lpjml-data.md#3-chart_with_upwards_trend-base-stats-of-lpjmldata-objects) the data or get insights via [`summary()`](./vignettes/lpjml-data.md#3-chart_with_upwards_trend-base-stats-of-lpjmldata-objects) and other base stats
    - :repeat: [`transform()`](./vignettes/lpjml-data.md#4-pencil2-modify-lpjmldata-objects) it to other time and space formats
    - :scissors: [`subset()`](./vignettes/lpjml-data.md#4-pencil2-modify-lpjmldata-objects) the underlying data
    - :package: [`as_array()`](./vignettes/lpjml-data.md#5-package-export-lpjmldata-objects), [`as_tibble()`](./vignettes/lpjml-data.md#5-package-export-lpjmldata-objects) and [`as_raster()` / `as_terra()`](./vignettes/lpjml-data.md#5-package-export-lpjmldata-objects) to export into common R data formats

- [`read_meta()`](./vignettes/lpjml-data.md#miscellaneous) read meta or header files as [`LPJmLMetaData`](./vignettes/lpjml-data.md#miscellaneous) object

### **miscellaneous**
- functions to handle LPJmL file headers, `read_header()` read the header of LPJmL files, `get_headersize()` get the size of a file header or `create_header()` to create a header object for writing input files
- `get_datatype()` get information on the data type used in different LPJmL files
- `asub()` functionality of the subset method to be used on a base array, also to replace data
- ... *more functions via `library(help = "lpjmlkit")`*