## Requirements

**Windows users only**
- If you do not have `Rtools` yet, you will need to install it: download Rtools from here https://cran.rstudio.com/bin/windows/Rtools/ (e.g. `rtools40v2-x86_64.exe`)

**PIK cluster**  
- for installation:  
    `module purge` (to be sure that lpjml is not loaded)  
    `module load piam` (allows you to load many standard packages)  
- for functions requiring LPJmL functionality (write_config/check_config/submit_lpjml/...):  
    `module load piam` (allows you to load many standard packages)  
    `module load lpjml` (loads all the dependencies required for compiling/running lpjml)  

**local**  
make sure you have these R-packages installed:  
- devtools (for installing the package from source)
- roxygen2 (for updating vignettes)  
- testthat (for testing the functions and catching errors)  
- lintr (for complying with the coding style standards)  


## Clone & Pull

First of all, clone the project to your location of choice.

```bash
git clone https://gitlab.pik-potsdam.de/lpjml/lpjmlkit.git <path_to_lpjmlkit>
```

Then, pull to update the code to the most recent changes

```bash
cd <path_to_lpjmlkit>
git pull origin master
```

## Installation

**(Option 1) Install from source in R (any OS)**  
- Use the [`devtools`](https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf) library to install lpjmlkit:
```R
devtools::install("<path_to_lpjmlkit>")
```

**(Option 2) Install from source in RStudio (any OS)**  
- Open .RProj file with RStudio  
- Build -> Install (the package and restart R)

**(Option 3) Install from package file in R (or RStudio)**  
- `install.packages(<path_to_package_file>, repos = NULL, type="source")`

**(Option 4) Build & Install from the Command Line (Linux)**  

- Open .RProj file with RStudio  
- Build -> More -> Build source package  
- This creates a compressed file (e.g. `.tar.gz`) in the package folder. 

```bash
cd <path_to_lpjmlkit>
R CMD build lpjmlkit
R CMD INSTALL lpjmlkit_<version_nr>.tar.gz
```
