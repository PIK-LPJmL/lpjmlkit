# Function kit for basic LPJmL handling

R package **lpjmlkit**, version **0.5.1**

[![CRAN status](https://www.r-pkg.org/badges/version/lpjmlkit)](https://cran.r-project.org/package=lpjmlkit)  [![R build status](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/workflows/check/badge.svg)](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/actions) [![codecov](https://codecov.io/gh/lpjml/lpjmlkit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/lpjml/lpjmlkit) 

## Purpose and Functionality

Function kit for basic LPJmL handling, including simulations,
    corresponding configurations as well as input & output data.


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
