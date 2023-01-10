# lpjmlkit  <a href=''><img src='./inst/img/logo.png' align="right" height="139" /></a>

_A kit of R-functions to work with inputs, outputs, configurations & simulations of the LPJmL model_

## Overview

lpjmlkit is a collection of base functions to facilitate the work with the DGVM LPJmL hosted at the Potsdam Institute for Climate Impact Research.
It provides functions for running LPJmL, as well as reading, processing and writing model-related data such as inputs and outputs or configuration files.

## Installation

First clone the project to your location of choice:

```bash
git clone https://gitlab.pik-potsdam.de/lpjml/lpjmlkit.git <path_to_lpjmlkit>
```

or for a package update:

```bash
cd <path_to_lpjmlkit>
git pull origin master
```

Open an R session:
```R
devtools::install("<path_to_lpjmlkit>/lpjmlkit")
```
*For more information please read [INSTALL.md](./INSTALL.md)*

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


## Getting help

If you encounter a clear bug, please file an issue with a minimal reproducible example. If you have any questions please contact [Jannes Breier](mailto:jannesbr@pik-potsdam.de), [Stephen Wirth](mailto:wirth@pik-potsdam.de), [Sebastian Ostberg](mailto:ostberg@pik-potsdam.de) or [Fabian Stenzel](mailto:stenzel@pik-potsdam.de).  
If you want to **contibute to lpjmlkit** have a look at [CONTRIBUTING.md](./CONTRIBUTING.md)


