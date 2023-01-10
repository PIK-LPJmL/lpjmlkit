## Usage

```R
library(lpjmlkit)

# To get an overview of all functions included you can use:
library(help = "lpjmlkit")
```

`lpjmlkit` contains:
- the [LPJmL Data](TODO:vignette) module for reading and processing
  - `read_io` *read LPJmL input and output as `LPJmLData` object*
  - `LPJmLData` class, *containing the data array and the `LPJmLMetaData` while ensuring integrity and providing methods for processing*
    - **modify** methods `add_grid`, `subset` and `transform` *to add grid data,
    subset the underlying data or transform it to other time and space formats*
    - **base stats** methods `summary`, `dim`, `dimnames` *to get an overview on the data*
    - **export** methods `as_array`, `as_tibble`, `as_raster` and `as_terra` *to export into established formats*
  - `read_meta` *read meta files as `LPJmLMetaData` object*
  - `LPJmLMetaData` class, *including meta data from meta files, file headers or those provided manually as possible connection to `LPJmLData`*
    - **export** methods `as_list` & `as_header` *to export as base list or LPJmL header format*
- the [LPJmL Runner](./vignettes/lpjml-runner.pdf) to run LPJmL simulations
  - `write_config` *write* `"*_config.json"` *file(s) based on a parameters tibble and a (precompiled) lpjml.js. `read_config` later or `view_config`*
  - `make_lpjml` *compile LPJmL* and `check_lpjml`
  - `run_lpjml`, `submit_lpjml` *run or submit LPJmL (to Slurm) with \*_config.json* (Note: you need to `module load lpjml`)
- and functions to handle LPJmL file headers (mostly used in input files)
  - `read_header` *read the header of LPJmL files*
  - `get_headersize` *to get the size of a file header*
  - `get_datatype` *get information on the data type used in different LPJmL files*
