# Toolkit for basic LPJmL handling <a href=''><img src='inst/img/logo.png' align='right' height='139' /></a>

R package **lpjmlkit**, version **0.5.2**

[![CRAN status](https://www.r-pkg.org/badges/version/lpjmlkit)](https://cran.r-project.org/package=lpjmlkit)  [![R build status](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/workflows/check/badge.svg)](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/actions) [![codecov](https://codecov.io/gh/lpjml/lpjmlkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lpjml/lpjmlkit) [![r-universe](https://pik-piam.r-universe.dev/badges/lpjmlkit)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

A collection of base functions to facilitate the work with the DGVM LPJmL hosted at the Potsdam Institute for Climate Impact Research.
    It provides functions for performing LPJmL simulations, as well as reading, processing and writing model-related data such as inputs and outputs or configuration files.
## Overview

### **[LPJmL Runner](./vignettes/lpjml-runner.pdf)** to perform LPJmL simulations
  - `write_config()` write config.json files using a tibble with parameters to be changed and a base lpjml.js file
  - `check_config()` check if generated config.json files are valid for LPJmL simulations
  - `run_lpjml()` run LPJmL directly (e.g. single cell simulations)
  - `submit_lpjml()` submit LPJmL to SLURM (e.g. global simulations)

### **[LPJmL Data](TODO:vignette)** for reading and processing LPJmL data
- `read_io` read LPJmL input and output as an `LPJmLData` object, containing the data array and LPJmLMetaData
  - `subset()` subset the underlying data
  - `transform()` transform it to other time and space formats
  -  `as_array()`, `as_tibble()`, `as_raster()` and `as_terra()` to export into established data formats
- `read_meta()` read meta or header files as `LPJmLMetaData` object

### **miscellaneous**
- functions to handle LPJmL file headers, `read_header()` read the header of LPJmL files, `get_headersize()` get the size of a file header or `create_header()` to create a header object for writing input files
- `get_datatype()` get information on the data type used in different LPJmL files
- `asub()` functionality of the subset method to be used on a base array, also to replace data

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

Breier J, Ostberg S, Wirth S, Minoli S, Stenzel F, Mueller C (2023). _lpjmlkit: Toolkit for basic LPJmL handling_. R package version 0.5.2.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lpjmlkit: Toolkit for basic LPJmL handling},
  author = {Jannes Breier and Sebastian Ostberg and Stephen Wirth and Sara Minoli and Fabian Stenzel and Christoph Mueller},
  year = {2023},
  note = {R package version 0.5.2},
}
```
