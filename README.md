# Toolkit for basic LPJmL handling <a href=''><img src='inst/img/logo.png' align='right' height='139' /></a>

R package **lpjmlkit**, version **0.5.17**

[![CRAN status](https://www.r-pkg.org/badges/version/lpjmlkit)](https://cran.r-project.org/package=lpjmlkit)  [![R build status](https://github.com/PIK-LPJmL/lpjmlkit/workflows/check/badge.svg)](https://github.com/PIK-LPJmL/lpjmlkit/actions) [![codecov](https://codecov.io/gh/PIK-LPJmL/lpjmlkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PIK-LPJmL/lpjmlkit) [![r-universe](https://pik-piam.r-universe.dev/badges/lpjmlkit)](https://pik-piam.r-universe.dev/ui#builds)

## Purpose and Functionality

A collection of basic functions to facilitate the work with the
    Dynamic Global Vegetation Model (DGVM) Lund-Potsdam-Jena managed Land
    (LPJmL) hosted at the Potsdam Institute for Climate Impact Research (PIK).
    It provides functions for performing LPJmL simulations, as well as reading,
    processing and writing model-related data such as inputs and outputs or
    configuration files.
## Overview

### **[LPJmL Runner &#127939;](./vignettes/lpjml-runner.md)**  to perform LPJmL simulations
- &#9997; [`write_config()`](./vignettes/lpjml-runner.md#1-clipboard-define-a-table-of-modified-configuration-parameters) write config.json files using a tibble with parameters to be changed and a base lpjml.js file
- &#128269; [`check_config()`](./vignettes/lpjml-runner.md#2-writing_hand-create-corresponding-configuration-files) check if generated config.json files are valid for LPJmL simulations
- &#9654; [`run_lpjml()`](./vignettes/lpjml-runner.md#4-arrow_forward-run-or-rocket-submit-lpjml) run LPJmL directly (e.g. single cell simulations) or &#128640; [`submit_lpjml()`](./vignettes/lpjml-runner.md#4-arrow_forward-run-or-rocket-submit-lpjml) to SLURM (e.g. global simulations)

### **[LPJmL Data &#128190;](./vignettes/lpjml-data.md)** for reading and processing LPJmL data
- [`read_io()`](./vignettes/lpjml-data.md#1-book-data-reading-function-read_io) read LPJmL input and output as a [`LPJmLData`](/vignettes/lpjml-data.md#2-file_folder-data-class-lpjmldata) object, containing the data array and LPJmLMetaData
    - &#128200; [`plot()`](./vignettes/lpjml-data.md#3-chart_with_upwards_trend-base-stats-of-lpjmldata-objects) the data or get insights via [`summary()`](./vignettes/lpjml-data.md#3-chart_with_upwards_trend-base-stats-of-lpjmldata-objects) and other base stats
    - &#128257; [`transform()`](./vignettes/lpjml-data.md#4-pencil2-modify-lpjmldata-objects) it to other time and space formats
    - &#9986; [`subset()`](./vignettes/lpjml-data.md#4-pencil2-modify-lpjmldata-objects) the underlying data
    - &#128230; [`as_array()`](./vignettes/lpjml-data.md#5-package-export-lpjmldata-objects), [`as_tibble()`](./vignettes/lpjml-data.md#5-package-export-lpjmldata-objects) and [`as_raster()` / `as_terra()`](./vignettes/lpjml-data.md#5-package-export-lpjmldata-objects) to export into common R data formats

- [`read_meta()`](./vignettes/lpjml-data.md#miscellaneous) read meta or header files as [`LPJmLMetaData`](./vignettes/lpjml-data.md#miscellaneous) object

### **miscellaneous**
- `calc_cellarea()` to calculate the area of LPJmLData objects underlying grid
or for other objects latitudes
- functions to handle LPJmL file headers, `read_header()` read the header of LPJmL files, `get_headersize()` get the size of a file header or `create_header()` to create a header object for writing input files
- `get_datatype()` get information on the data type used in different LPJmL files
- `asub()` functionality of the subset method to be used on a base array, also to replace data
- ... *more functions via `library(help = "lpjmlkit")`*

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

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("lpjml-data")   # LPJmL Data
vignette("lpjml-runner") # LPJmL Runner
```

## Questions / Problems

In case of questions / problems please contact Jannes Breier <jannesbr@pik-potsdam.de>.

## Citation

To cite package **lpjmlkit** in publications use:

Breier J, Ostberg S, Wirth S, Minoli S, Stenzel F, Mueller C (2023). _lpjmlkit: Toolkit for basic LPJmL handling_. R package version 0.5.17, <URL: https://github.com/PIK-LPJmL/lpjmlkit>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {lpjmlkit: Toolkit for basic LPJmL handling},
  author = {Jannes Breier and Sebastian Ostberg and Stephen Wirth and Sara Minoli and Fabian Stenzel and Christoph Mueller},
  year = {2023},
  note = {R package version 0.5.17},
  url = {https://github.com/PIK-LPJmL/lpjmlkit},
}
```
