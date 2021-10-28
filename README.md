# lpjmlKit
*Automatization, Data Analysis, Visualization and more*

## Overview
This project contains reading, writing, plotting or processing functions that are related to the DGVM LPJmL hosted at the Potsdam Institute for Climate Impact Research. 
It further provides functions to run the LPJmL as well as process model related data such as In- and Outputs. It comes in the [R package format](https://r-pkgs.org/intro.html). Main functions are:

* `writeConfig()` *write config\*.json based on precompiled lpjml.js and params data frame*
* `makeLPJmL()` and `runLPJmL()` *to compile and run LPJmL with config\*.json* 


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
