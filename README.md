# lpjmlKit
*Automatization, Data Analysis, Visualization and more*

## Overview
This project contains reading, writing, plotting or processing functions that are related to the DGVM LPJmL hosted at the Potsdam Institute for Climate Impact Research. 
It further provides functions to run the LPJmL as well as process model related data such as In- and Outputs. It comes in the [R package format](https://r-pkgs.org/intro.html). Main functions are:

#### Run LPJmL
* `write_config()` *to write config\*.json based on precompiled lpjml.js and params data frame*
* `make_lpjml()` and `run_lpjml()` *to compile and run LPJmL with config\*.json* 
#### Work with LPJmL files
* `read_header()` *to read header information from LPJmL files (usually input files, but output files can also contain headers)*
* `create_header()` and `write_header()` *to create LPJmL files with headers yourself*
* `get_datatype()` *to use header information about the data type of an LPJmL file in `readBin()` or `writeBin()` functions*


## Usage/Installation

First of all git clone the project to your location of choice

```bash
git clone https://gitlab.pik-potsdam.de/lpjml/lpjmlkit.git
```

Use the [`devtools`](https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf) library to install lpjmlkit.
```R
devtools::install("<path_to_lpjmlkit>/lpjmlkit")

library(lpjmlKit)
```


## Contact

If you have any questions please contact [Fabian Stenzel](mailto:stenzel@pik-potsdam.de), [Sara Minoli](mailto:minoli@pik-potsdam.de), [Stephen Wirth](mailto:wirth@pik-potsdam.de) or [Jannes Breier](mailto:jannesbr@pik-potsdam.de).
