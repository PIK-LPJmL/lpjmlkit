# Function kit for basic LPJmL handling

R package **lpjmlkit**, version **0.5.1**

[![CRAN status](https://www.r-pkg.org/badges/version/lpjmlkit)](https://cran.r-project.org/package=lpjmlkit)  [![R build status](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/workflows/check/badge.svg)](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/actions) [![codecov](https://codecov.io/gh/lpjml/lpjmlkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lpjml/lpjmlkit) 

## Purpose and Functionality

A collection of base functions to facilitate the work with the DGVM LPJmL hosted at the Potsdam Institute for Climate Impact Research.
    It provides functions for running LPJmL, as well as reading, processing and writing model-related data such as inputs and outputs or configuration files.
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

```

## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("lpjmlkit")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("lpjml-runner") # LPJmL Runner
```

## Questions / Problems

In case of questions / problems please contact Jannes Breier <jannesbr@pik-potsdam.de>.

## Citation

To cite package **lpjmlkit** in publications use:

Mueller C, Stenzel F, Minoli S, Breier J, Ostberg S, Wirth S (2023). _lpjmlkit: Function kit for basic LPJmL handling_. R package version 0.5.1.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lpjmlkit: Function kit for basic LPJmL handling},
  author = {Christoph Mueller and Fabian Stenzel and Sara Minoli and Jannes Breier and Sebastian Ostberg and Stephen Wirth},
  year = {2023},
  note = {R package version 0.5.1},
}
```
