## Overview

### **LPJmL Runner &#127939;**  to perform LPJmL simulations
   *LPJmL Runner only supports appropriately configured Unix-based operating systems.*
- &#9997; `write_config()` write config.json files using a data frame with parameters to be changed and a base configuration file
- &#128269; `check_config()` check if generated config.json files are valid for LPJmL simulations
- &#9654; `run_lpjml()` run LPJmL directly (e.g. single cell simulations) or &#128640; `submit_lpjml()` to SLURM (e.g. global simulations)


### **LPJmL Data &#128190;** for reading and processing LPJmL data
- `read_io()` read LPJmL input and output as a `LPJmLData` object, containing the data array and LPJmLMetaData
    - &#128200; `plot()` the data or get insights via `summary()` and other base stats
    - &#128257; `transform()` it to other time and space formats
    - &#9986; `subset()` the underlying data
    - &#128230; `as_array()`, `as_tibble()` and `as_raster()` / `as_terra()` to export into common R data formats

- `read_meta()` read meta or header files as `LPJmLMetaData` object

### **miscellaneous**
- `calc_cellarea()` to calculate the area of LPJmLData objects underlying grid
or for other objects latitudes
- functions to handle LPJmL file headers, `read_header()` read the header of LPJmL files, `get_headersize()` get the size of a file header or `create_header()` to create a header object for writing input files
- `get_datatype()` get information on the data type used in different LPJmL files
- `asub()` functionality of the subset method to be used on a base array, also to replace data
- ... *more functions via `library(help = "lpjmlkit")`*
