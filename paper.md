---
title: 'lpjmlkit: A toolkit for operating LPJmL and model-specific data processing'
tags:
  - R
  - LPJmL
  - DGVM
  - biogeochemical modeling
  - model simulations
  - data analysis
  - hpc

authors:
  - name: Jannes Breier
    orcid: 0000-0002-9055-6904
    equal-contrib: true
    affiliation: 1
    corresponding: true
  - name: Sebastian Ostberg
    orcid: 0000-0002-2368-7015
    equal-contrib: true
    affiliation: 1
    corresponding: true
  - name: Stephen Wirth
    orcid: 0000-0002-2261-8771
    corresponding: false
    affiliation: 1, 3
  - name: Sara Minoli
    orcid: 0000-0002-2261-8771
    corresponding: false
    affiliation: 1, 2
  - name: Fabian Stenzel
    orcid: 0000-0002-5109-0048
    corresponding: false
    affiliation: 1
  - name: Christoph Müller
    orcid: 0000-0002-9491-3550
    corresponding: false
    affiliation: 1
affiliations:
 - name: Potsdam Institute for Climate Impact Research (PIK), Member of the Leibniz Association, P.O. Box 60 12 03, 14412 Potsdam, Germany
   index: 1
 - name: Climate Focus, Germany
   index: 2
 - name: Institute of Crop Science and Plant Breeding, Grass and Forage Science/Organic Agriculture, Kiel University, Hermann-Rodewald-Str. 9, 24118, Kiel, Germany
   index: 3

date: 30 January 2023
bibliography: paper.bib

---

# Summary

<img src="inst/figures/logo.png" alt="drawing" style="width:24%;"/>

[//]: # (![]\(inst/figures/logo.png\){width=24% align=left} -> use for final publication)

The *lpjmlkit* R package [@lpjmlkit_manual] is an open source software that is
developed for handling the open source dynamic global vegetation model (DGVM)
[LPJmL](https://github.com/PIK-LPJmL/LPJmL).
It contains two main modules.
One, *LPJmL Runner*, provides the functionality to create multiple model
configurations and start the corresponding simulations either on a personal
computer or on an HPC (High Perfomance Computing) cluster with SLURM
(Simple Linux Utility for Resource Management) support, in both cases requiring
a working LPJmL installation.\
The other, *LPJmL Data*, offers a generic function that supports reading both
simulation output and model input data in multiple file formats used by LPJmL.
The associated data class *LPJmLData* contains both the data and the
corresponding metadata to ensure data integrity within a single instance.
LPJmLData objects act as data containers that provide modification functions
such as subsetting or transformations of the data.
LPJmLData objects can be exported into various other common R data formats.
In addition to these modules, other functions are included to facilitate common
use cases of LPJmL.
This article introduces *lpjmlkit*, an R package that serves as an interface to
LPJmL to simplify direct work with the model and to enable new generic software
developments based on LPJmL simulations or data.


# Statement of need

A simple interface makes software easier to use and improves accessibility,
user experience, and overall adoption. Often, scientists with expertise in
their respective field that can meaningfully design and interpret simulation
experiments lack substantial training in software engineering.
Developing tools for standard tasks, such as input preparation or basic output
processing introduces inefficiencies and is error-prone as such on-demand
solutions typically lack proper testing and documentation.
Traditional process-based numerical models in low-level languages can be
challenging for less experienced users to operate, `lpjmlkit` provides a more
user-friendly interface to the LPJmL model, making it easier for a wider range
of users to conduct complex simulations.
LPJmL is a well-established and widely-used dynamic global vegetation,
hydrological and crop model, widely used in the scientific community.\
LPJmL has been used for more than a decade and was employed by researchers
to conduct numerous studies in various research areas related to the
terrestrial biosphere.
To this end, the original DGVM LPJ [@Sitch2003] was extended by adding an
improved representation of the hydrological cycle [@gerten_terrestrial_2004],
by implementing managed land components, forming "LPJmL"
(LPJ with *m*anaged *L*and)) [@bondeau_modelling_2007; @rolinski_modeling_2018;@lutz2019simulating; @Porwollik2022cover], and by including the nitrogen cycle
[@von_bloh_implementing_2018].\
This facilitated broader, interdisciplinary studies such as the work of
@gerten_feeding_2020, which answered the question of whether it is possible to
feed ten billion people within four planetary boundaries, or studies that
implemented features of sustainable agriculture in LPJmL
[@Porwollik2022cover; @Herzfeld2021soc] that had not been simulated before
within a DGVM.
There are many other examples of different scientific studies based on LPJmL
simulations, all using individual scripts to create inputs for the model or
analyze outputs with the risks already described.
Unlike its sister model LPJ-GUESS [@bagnara_r_2019] or other models, such as
MAgPIE [@dietrich_magpie_2019] LPJmL was never equipped with standardised
interfaces for higher level programming languages to either run simulations or
read and process input or output data.\
"The lack of a standardized interface means that both beginners and experienced
users need to constantly develop their own custom scripts and tools for what
should be routine (data) processing steps.
Tools shared informally between individual users often have limited scope and as
such limited re-usability.
They lack documentation and are often not well tested or maintained, nor
released to a wide user base in a transparent manner.\
`lpjmlkit` was developed to eliminate these problems and at the same time create
a standard in the handling of LPJmL to improve the design of simulation
experiments and the transparency of studies with LPJmL.
By using the LPJmL Runner functionality model configurations are stored in a
single and unique configuration file that references the exact model version of
LPJmL used for the simulations to achieve reproducible results.
LPJmL Data subsequently ensures a generic standard for version-independent
processing of LPJmL (output) data.
This way `lpjmlkit` serves as a user and programming interface to LPJmL and
provides an easy-to-use basis for interaction with LPJmL in a simple R script
as well as for further software development based on LPJmL, for example model
calibration, benchmarking or indicator development.

# Package features

`lpjmlkit` is an R package that provides functions for conducting LPJmL
simulations and processing their data. With the LPJmL Runner module, users can
create model configurations, check their validity, compile LPJmL, and conduct
corresponding simulations.
The LPJmL Data module provides functionality for reading and processing the
output or input data from LPJmL simulations. 
Together, these modules cover the main applications of the LPJmL model and its
data.

The LPJmL Runner module is designed to support Unix-based operating systems and
includes four key functions.
`write_config()` creates configuration files in the JSON format using a data
frame with parameters to be changed and a base pre-configuration file.
`check_config()` checks if generated config.json files are valid for LPJmL
simulations. `run_lpjml()` runs LPJmL directly, such as for single-cell
simulations, while `submit_lpjml()` submits LPJmL to SLURM, such as for global
simulations.

```R
library(lpjmlkit)


lpjml_path <- "./LPJmL_internal"
sim_path <- "./simulations"

# Define data frame with configuration parameters to be changed
config_params <- data.frame(
  sim_name = c("spinup", "lu", "pnv"),
  landuse = c("no", "yes", "no"),
  reservoir = c(FALSE, TRUE, FALSE),
  river_routing = c(FALSE, FALSE, FALSE),
  wateruse = c("no", "yes", "no"),
  const_deposition = c(FALSE, FALSE, TRUE),
  dependency = c(NA, "spinup", "spinup")
)

# Write corresponding configuration files using a base configuration file
config_details <- write_config(config_params,
                               model_path = lpjml_path,
                               output_path = sim_path)

# Check validity of each written configuration file using LPJmL
check_config(config_details,
             model_path = model_path,
             output_path = output_path)

# Submit LPJmL simulations to be run by SLURM
submit_lpjml(config_details,
             model_path = model_path,
             output_path = output_path)
```

The LPJmL Data module provides various functions for reading and processing
LPJmL data.
`read_io()` reads LPJmL input and output as an LPJmLData object, which contains
the data `array` and `LPJmLMetaData`.
This object can be used for further analysis and visualization, with `plot()`,
`summary()`, and `transform()` functions available.
Users can also subset the data using `subset()`, or export it to common R data
formats using as_array(), as_tibble(), and as_raster() / as_terra().

```R
# Read runoff output from corresponding 
runoff <- read_io(filename = "./simulations/output/lu/runoff.bin.json"),
                  subset = list(year = as.character(2010:2019)))

# Transform into required format to be subsetted by months and latitudes and
# plot aggregated years and months
runoff %>%
  transform(to = c("year_month_day", "lon_lat")) %>%
  subset(month = 6:9,
         lat = as.character(seq(0.25, 83.75, by = 0.5))) %>%
  plot(aggregate = list(year = mean, month = sum),
       raster_extent = raster::extent(-180, 180, 0, 60),
       main = "Northern hemisphere summer runoff [mm]")
abline(h = 0, lty = 2)
```
![example_plot_lpjml_data](inst/figures/example_plot.png)


The `lpjmlkit` package also includes various other functions to support
different applications when using LPJmL.
For example, `calc_cellarea()` calculates the area of LPJmLData objects'
underlying grid or for other objects' latitudes.
Functions to handle LPJmL file headers, such as `read_header()`,
`get_headersize()`, and create_header(), are also included.
`get_datatype()` gets information on the data type used in different LPJmL
files, while `asub()` provides the functionality of the subset method to be
used on a base array.

# Documentation & License

`lpjmlkit` is an open-source software package for operating LPJmL and processing
its data that provides comprehensive documentation and vignettes.
The documentation is available online at
https://pik-piam.r-universe.dev/lpjmlkit/ and includes instructions for
installation, usage, and examples.
The package is licensed under the GNU Affero General Public License (AGPL-3.0),
which grants users the freedom to access, use, and modify the code, and ensures
that any modifications or derivative works are also available under the same
license, allowing for continued collaboration and development within the
community.
The source code is available on GitHub at https://github.com/PIK-LPJmL/lpjmlkit,
where users can also report issues and suggest improvements.

# Acknowledgements

Special thanks go to Werner von Bloh for implementing new
features into LPJmL to enable the development of lpjmlkit, such as writing meta
files.

# References