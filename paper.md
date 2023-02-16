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

<img src="inst/img/logo.png" alt="drawing" style="width:24%;"/>

[//]: # (![]\(inst/img/logo.png\){width=24% align=left} -> use for final publication)

The *lpjmlkit* R package [@lpjmlkit_manual] is an open source software,
which is mainly developed and maintained by the Potsdam Institute for 
Climate Impact Research (PIK) and is used for handling the open source 
Dynamic Global Vegetation model (DGVM) [LPJmL](https://github.com/PIK-LPJmL/LPJmL).
It contains two main modules. One, *LPJmL Runner*, provides the functionality to
create multiple model configurations and start the corresponding simulations
either on a personal computer or on an HPC (High Perfomance Computing) cluster
with SLURM (Simple Linux Utility for Resource Management) support, in both cases
requiring a working LPJmL installation.\
The other, *LPJmL Data*, offers a generic function that supports reading both
simulation output and model input data in multiple file formats used by LPJmL.
The associated data class *LPJmLData* contains both the data and the
corresponding metadata to ensure data integrity within a single instance.
LPJmLData objects act as data containers that provide modification functions
such as subsetting or transformations of the data. LPJmLData objects can be
exported into various other common R data formats.
In addition to these modules, other functions are included to facilitate common
use cases of LPJmL. This article introduces *lpjmlkit*, an R package
that serves as an interface to LPJmL to simplify direct work with the model and
to enable new generic software developments based on LPJmL simulations or data.


# Statement of need

A simple interface makes software easier to use and improves accessibility,
user experience, and overall adoption. Often, scientists with expertise in
their respective field that can meaningfully design and interpret simulation
experiments lack substantial training in software engineering.
Developing tools for standard tasks, such as input preparation or basic output
processing introduces inefficiencies and is error-prone as such on-demand
solutions typically lack proper testing and documentation.
In the natural-sciences, the progressive development of complex process-based
numerical models in low-level languages like C comes at the expense of
usability, making them accessible only to experienced users [@wilson_best_2014].
*lpjmlkit* has come to break with this trend and serve as a user interface
for the LPJmL model, a well-established and widely-used dynamic global
vegetation, hydrological and crop model, widely used in the scientific 
community.\
LPJmL has been used for more than a decade and was employed by researchers
to conduct numerous studies in various research areas related the
terrestrial biosphere. To this end, the original dynamic global vegetation model
(DGVM) LPJ [@Sitch2003] was extended by adding an improved representation of the
hydrological cycle [@gerten_terrestrial_2004], by implementing managed land
components, forming "LPJmL" (LPJ with *m*anaged *L*and))
[@bondeau_modelling_2007; @rolinski_modeling_2018; @lutz2019simulating;
@Porwollik2022cover], and by including the nitrogen cycle
[@von_bloh_implementing_2018].\
This facilitated broader, interdisciplinary studies such as the work of
@gerten_feeding_2020, which answered the question of whether it is possible to
feed ten billion people within four planetary boundaries, or studies that
implemented features of sustainable agriculture in LPJmL
[@Porwollik2022cover; @Herzfeld2021soc] that had not been simulated before
within a DGVM. There are many other examples of different scientific studies
based on LPJmL simulations, all using individual software solutions with the
risks already described.
Unlike its sister model LPJ-GUESS [@bagnara_r_2019] or other models, such as
MAgPIE [@dietrich_magpie_2019] LPJmL was never equipped with standardised
interfaces for higher level programming languages to either run simulations or
read and process input or output data.\
"The lack of a standardized interface means that both beginners and experienced
users need to constantly develop their own custom scripts and tools for what
should be routine (data) processing steps. Tools shared informally between
individual users often have limited scope and as such limited re-usability.
They lack documentation and are often not well tested or maintained, nor
released to a wide user base in a transparent manner.\
`lpjmlkit` was developed to eliminate these problems and at the same time create
a standard in the handling of LPJmL to improve the design of simulation
experiments and the transparency of studies with LPJmL. By using the LPJmL
Runner functionality model configurations are stored in a single and unique
configuration file that references the exact model version of LPJmL
used for the simulations to achieve reproducible results. LPJmL Data
subsequently ensures a generic standard for version-independent processing of
LPJmL (output) data.
This way `lpjmlkit` serves as a user and programming interface to LPJmL and
provides an easy-to-use basis for interaction with LPJmL in a simple R script
as well as for further software development based on LPJmL, for example model
calibration, benchmarking or indicator development.

# Package features

## LPJML Runner to perform LPJmL simulations
- LPJmL Runner only supports appropriately configured Unix-based operating systems.
- `write_config()` write config.json files using a tibble with parameters to be changed and a base lpjml.js file
- `check_config()` check if generated config.json files are valid for LPJmL simulations
- `run_lpjml()` run LPJmL directly (e.g. single cell simulations) or `submit_lpjml()`to SLURM (e.g. global simulations)


## LPJmL Data for reading and processing LPJmL data
- `read_io()` read LPJmL input and output as a `LPJmLData` object, containing the data array and LPJmLMetaData
    - `plot()` the data or get insights via `summary()` and other base stats
    - `transform()` it to other time and space formats
    - `subset()` the underlying data
    - `as_array()`, `as_tibble()` and `as_raster()` / `as_terra()` to export into common R data formats

- `read_meta()` read meta or header files as `LPJmLMetaData` object

## miscellaneous
- `calc_cellarea()` to calculate the area of LPJmLData objects underlying grid
or for other objects latitudes
- functions to handle LPJmL file headers, `read_header()` read the header of LPJmL files, `get_headersize()` get the size of a file header or `create_header()` to create a header object for writing input files
- `get_datatype()` get information on the data type used in different LPJmL files
- `asub()` functionality of the subset method to be used on a base array, also to replace data
- ... *more functions via `library(help = "lpjmlkit")`*


# Acknowledgements

We would like to thank Susanne Rolinski and Marie Hemmen for their
contributions. Special thanks also go to Werner von Bloh for implementing new
features into LPJmL to enable the development of lpjmlkit, such as writing meta
files.

# References
