# lpjmlKit  <a href=''><img src='./inst/img/logo.png' align="right" height="139" /></a>

_A kit of R-functions to work with inputs, outputs and configuration files of the LPJmL model_

## Overview

This project contains reading, writing, plotting or processing functions that are related to the DGVM LPJmL hosted at the Potsdam Institute for Climate Impact Research. 
It further provides functions to run the LPJmL model, as well as to process model-related data such as In- and Outputs. It comes in the [R package format](https://r-pkgs.org/intro.html).

[[_TOC_]]

## Installation

**Clone & Pull**

First of all, clone the project to your location of choice.

```bash
git clone https://gitlab.pik-potsdam.de/lpjml/lpjmlkit.git <path_to_lpjmlkit>
```

Then, pull to update the code to the most recent changes

```bash
cd <path_to_lpjmlkit>
git pull origin master
```

**Windows users only**
- If you do not have `Rtools` yet, you will need to install it: download Rtools from here https://cran.rstudio.com/bin/windows/Rtools/ (e.g. `rtools40v2-x86_64.exe`)

either  
**modules on the cluster**  
- for installation:  
    `module purge` (to be sure that lpjml is not loaded)  
    `module load piam` (allows you to load many standard packages)  
- for functions requiring LPJmL functionality (write_config/check_config/submit_lpjml/...):  
    `module load piam` (allows you to load many standard packages)  
    `module load lpjml` (loads all the dependencies required for compiling/running lpjml)  

or  
**local equivalant to module loading**  
make sure you have these R-packages installed:  
- devtools (for installing the package from source)
- roxygen2 (for updating vignettes)  
- testthat (for testing the functions and catching errors)  
- lintr (for complying with the coding style standards)  

usually not required:  
**Create package files for independent installation**  
- Open .RProj file with RStudio  
- Build -> More -> Build source package  
- This creates a compressed file (e.g. `.tar.gz`) in the package folder.  

installation option 1:
**Install from source in R (any OS)**  
- Use the [`devtools`](https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf) library to install lpjmlkit:
```R
devtools::install("<path_to_lpjmlkit>")
```

or option 2:
**Install from source in RStudio (any OS)**  
- Open .RProj file with RStudio  
- Build -> Install (the package and restart R)

or option 3:
**Install from package file in R (or RStudio)**  
- `install.packages(<path_to_package_file>, repos = NULL, type="source")`

or option 4:
**Build & Install from the Command Line (Linux)**  

```bash
cd <path_to_lpjmlkit>
R CMD build lpjmlkit
R CMD INSTALL lpjmlkit_<version_nr>.tar.gz
```
[Go to Top](#)

## Usage

After the installation, you can use `lpjmlKit` as any other R package.

```R
# Load lpjmlKit
library(lpjmlKit)
```
To get an overview of all functions included you can use:
```R
library(help = "lpjmlKit")
```

- In `lpjmlKit` there are functions to handle LPJmL file headers (mostly used in input files), e.g.:
  - `read_header` *read the header of LPJmL files*
  - `get_headersize` *to get the size of a file header*
  - `get_datatype` *get information on the data type used in different LPJmL files*
- If you want to use the [LPJmL Runner](./vignettes/lpjml-runner.pdf):
  - `write_config` *write* `"\*_config.json"` *file(s) based on a parameters tibble and a (precompiled) lpjml.js. `read_config` later or `view_config`*
  - `make_lpjml` *compile LPJmL* and `check_lpjml`
  - `run_lpjml`, `submit_lpjml` *run or submit LPJmL (to Slurm) with \*_config.json*


[Go to Top](#)

## Contribute

If you want to contribute to further develop `lpjmlKit`, by modifying or adding new functions, please follow the guidelines here below.

**Required libraries**

```R
# For generating automated documentation
library(roxygen2)
library(devtools)
# For testing the functions and catching errors
library(testthat)
# For complying with the coding style standards
library(lintr)
```

**Note on the structure of the lpjmlKit repository**

The `lpjmlKit` repository include the following branches:

- `master`: this is the clean version of the package. Note: **this branch is protected**, direct push is not allowed, changes to `master` are possible only via 'Merge Requests'.
- `your_temporary_branch`:  The branch you create whenever you want to make a new development. **Please, delete it when you are done.**
- `development`: this is a lose collection of un-cleaned functions. **This branch should NOT be merged with the master!** Here you can find / add useful code that can serve as an inspiration to develop new  functions, or that can be useful to make accessible to other colleagues.

**How to add a new function**

Note: To be done for every new development that should go to master.
 
1. Create `your_temporary_branch` branch from `master` (`git checkout -b your_temporary_branch` and `git push origin your_temporary_branch`)
1. Manually copy the functions you want to clean from the `development` to `your_temporary_branch` branch, or write a new function from scratch
1. Clean the function and coding style (`lintr::lint("R/my_function.R")`)
1. Add documentation and examples. In Rstudio: `Code -> Insert roxygen skeleton` or by `Ctrl + Alt + Shift + R`
1. Add tests in the `tests/testthat` folder. Small sample data can be added in `tests/testdata`, if needed
1. Test if the new functions compile well with the rest of the package
1. If you are ready to merge your changes to the `master`, see below how to make a merge request

**How to Create a Merge Request to the `master`**

1. Increase version number in `DESCRIPTION` file (follow `MajorVersionNumber.MinorVersionNumber.PatchNumber`)
1. Run roxygen2 and update the descriptions `roxygen2::roxygenise()` or `CTRL + Shift + D`
1. Commit and push your changes
1. Make a merge request
1. Apply the 4-eyes principles
1. Delete `your_temporary_branch` branch
1. Ask for feedback and help if you have any questions!

[Go to Top](#)

## Contacts

If you have any questions please contact [Fabian Stenzel](mailto:stenzel@pik-potsdam.de), [Sara Minoli](mailto:minoli@pik-potsdam.de), [Stephen Wirth](mailto:wirth@pik-potsdam.de), [Sebastian Ostberg](mailto:ostberg@pik-potsdam.de) or [Jannes Breier](mailto:jannesbr@pik-potsdam.de).


