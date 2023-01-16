## Overview

### **[LPJmL Runner :runner:](./vignettes/lpjml-runner.md)**  to perform LPJmL simulations <sub><sup>[**PDF**](./vignettes/lpjml-runner.pdf)</sup></sub>
- :writing_hand: `write_config()` write config.json files using a tibble with parameters to be changed and a base lpjml.js file
- :mag:  `check_config()` check if generated config.json files are valid for LPJmL simulations
- :arrow_forward:  `run_lpjml()` run LPJmL directly (e.g. single cell simulations)
- :rocket: `submit_lpjml()` submit LPJmL to SLURM (e.g. global simulations)

### **[LPJmL Data :floppy_disk: ](./vignettes/lpjml-data.md)** for reading and processing LPJmL data <sub><sup>[**PDF**](./vignettes/lpjml-data.pdf)</sup></sub>
- `read_io()` read LPJmL input and output as an `LPJmLData` object, containing the data array and LPJmLMetaData
    - :chart_with_upwards_trend: `plot()` the data or get insights via `summary()` and other base stats
    - :repeat: `transform()` it to other time and space formats
    - :scissors:  `subset()` the underlying data
    - :package: `as_array()`, `as_tibble()`, `as_raster()` and `as_terra()` to export into established data formats\

- `read_meta()` read meta or header files as `LPJmLMetaData` object

### **miscellaneous**
- functions to handle LPJmL file headers, `read_header()` read the header of LPJmL files, `get_headersize()` get the size of a file header or `create_header()` to create a header object for writing input files
- `get_datatype()` get information on the data type used in different LPJmL files
- `asub()` functionality of the subset method to be used on a base array, also to replace data
- ... *more functions via `library(help = "lpjmlkit")`*
