---
title: 'lpjmlkit: A toolkit for basic LPJmL handling'
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

The *lpjmlkit* R package [@lpjmlkit_manual] is an open source software for
handling the open source Dynamic Global Vegetation model (DGVM)
[LPJmL](https://github.com/PIK-LPJmL/LPJmL) mainly developed and maintained
at the Potsdam Institute for Climate Impact Research (PIK).
It contains two main modules. One, *LPJmL Runner*, provides the functionality to
create multiple model configurations and start the corresponding simulations
either on a personal computer or on an HPC cluster with SLURM support, in both
cases requiring a working LPJmL installation.\
The other, *LPJmL Data*, offers a generic read function that supports
simulation output and model input data in multiple file formats.
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
user experience, and overall adoption. Especially in science, where it is rather
the exception for someone to have substantial training in both software
engineering and expertise in their respective field needed to meaningfully
design and interpret simulation experiments.
In the natural-sciences, the progressive development of complex process-based
numerical models in low-level languages like C comes at the expense of
usability, making them accessible only to experienced users.
*lpjmlkit* has come to break with this trend and serve as an application
programming interface (API) for the LPJmL model, a well-established and
widely-used dynamic global vegetation, hydrological and crop model, widely used
in the scientific community.\
LPJmL has been used for more than a decade and was employed by researchers
to conduct numerous studies in various research areas around the
terrestrial biosphere. To this end, the dynamic global vegetation model (DGVM)
LPJ was extended by scientists at PIK by adding the hydrological cycle
[@gerten_terrestrial_2004], by implementing managed land components, forming
"LPJmL" (LPJ with *m*anaged *L*and)) [@bondeau_modelling_2007;
@rolinski_modeling_2018;@lutz2019simulating; @Porwollik2022cover], and by
including the nitrogen cycle [@von_bloh_implementing_2018].\
This facilitated broader, interdisciplinary studies such as the work of
@gerten_feeding_2020, which answered the question of whether it is possible to
feed ten billion people within four planetary boundaries, or studies that
implemented features of sustainable agriculture in LPJmL
[@Porwollik2022cover; @Herzfeld2021soc] that had not been simulated before
within a DGVM. LPJmL is also used extensively in the context of simulating
natural vegetation dynamics with disturbances such as fire. ...
**TODO** ...\
However, unlike its sister model LPJ-GUESS [@bagnara_r_2019] or other
models, such as MAgPIE [@dietrich_magpie_2019] LPJmL was never equipped with
standardised interfaces for higher level programming languages to either run
simulations or read and process input or output data.\
This makes it difficult for beginners to work with LPJmL, but even scientists
who have been using LPJmL for a while had to develop their own scripts and tools
for routine data processing instead of using a generic, standardized interface.
Numerous processing scripts have been shared, adapted and reapplied among
scientists. A lot of time was spent constantly developing solutions for recurring
use cases, resulting in redundant work. In addition, the code of the processing
scripts is rarely reviewed, leaving potential bugs undetected.
`lpjmlkit` was developed to eliminate these problems and at the same time create
a standard in the handling of LPJmL to improve the design of simulation experiments
and the transparency of studies with LPJmL. By using the LPJmL Runner
functionality model configurations are stored in a single and unique
configuration file that references the exact model version of LPJmL
used for the simulations to achieve reproducible results.
This way `lpjmlkit` serves as an API to
LPJmL and provides an easy-to-use basis for interaction with LPJmL in a simple
R script as well as for further software development based on LPJmL,
for example model calibration, benchmarking or indicator development.

# Package features


## LPJmL Runner


## LPJmL Data


## miscellaneous


# Acknowledgements

We would like to thank Susanne Rolinski and Marie Hemmen for their
contributions. Special thanks also go to Werner von Bloh for incremental changes
to the LPJmL model, such as writing meta files.

# References
