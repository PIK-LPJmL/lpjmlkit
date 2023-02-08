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
    affiliation: 1
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
 - name: Potsdam Institute for Climate Impact Research (PIK), Germany
   index: 1
 - name: Climate Focus, Germany
   index: 2

date: 30 January 2023
bibliography: paper.bib

---

# Summary

<img src="inst/img/logo.png" alt="drawing" style="width:24%;"/>

[//]: # (![]\(inst/img/logo.png\){width=24% align=left} -> use for final publication)

The *lpjmlkit* R package [@lpjmlkit_manual] is an open source software for
handling the open source Dynamic Global Vegetation model (DGVM)
[LPJmL](https://github.com/PIK-LPJmL/LPJmL) mainly developed and maintained
by the Potsdam Institute for Climate Impact Research (PIK).
It contains two main modules. One, *LPJmL Runner*, provides the functionality to
create model configurations for multiple simulations simultaneously and run them
either on a configured local computer or on an HPC cluster with SLURM support.\
The other, *LPJmL Data* offers a generic read function that supports
simulation output and model input data for multiple file formats.
The associated data class *LPJmLData* contains both the data and the
corresponding metadata to ensure data integrity within an instance. LPJmLData
objects act as data containers that provide special modification functions
such as subsetting or transformations of the data. They can be exported into
various other common R data formats.
In addition to these modules, other functions are included to facilitate common
use cases of LPJmL. This article introduces *lpjmlkit*, an R package
that serves as an interface to LPJmL to simplify direct work with the model and
to enable new generic software developments based on LPJmL simulations or data.



# Statement of need

A simple interface makes software easier to use and improves accessibility,
user experience, and overall adoption. Especially in science, where it is rather
the exception for someone to have substantial training in both computer science and the
natural-sciences needed to meaningfully design and interpret simulation experiments. 
Often, the progressive development of complex process-based
numerical models in low-level languages like C comes at the expense of
usability, making them accessible only to experienced users.
*lpjmlkit* has come to break with this trend and serve as an API for the  
model called LPJmL, a well-established and widely-used dynamic global vegetation, hydrology and crop model, 
widely used in the scientific community.\
LPJmL has been in use for more than a decade and was employed by researchers
to conduct numerous studies in various research areas around the
terrestrial biosphere. To this end, the dynamic global vegetation model (DGVM) LPJ was extended by
scientists at PIK by adding the hydroligcal cycle [@gerten_terrestrial_2004],
by implementing managed land components, forming "LPJmL" (LPJ with *m*anaged *L*and))
[@bondeau_modelling_2007; @rolinski_modeling_2018; @lutz2019simulating; @Porwollik2022cover],
and by including the nitrogen cycle [@von_bloh_implementing_2018].\
This allowed for broader, interdisciplinary studies such as the work of
@gerten_feeding_2020, which answered the question of whether it is possible to
feed ten billion people within four planetary boundaries, or studies that
implemented features of sustainable agriculture in LPJmL [@Porwollik2022cover; @Herzfeld2021soc] 
that had not been simulated before within a DGVM. LPJmL is also used extensively in the context of
simulating natural vegetation dynamics with disturbances such as fire. ...
**TODO** ...\
However, unlike its sister model LPJ-GUESS [@bagnara_r_2019] or other
models at PIK [@dietrich_magpie_2019] LPJmL was never equipped with standardised
interfaces for higher level programming languages to either run simulations or
read and process input or output data.\
This makes it difficult for beginners to work with LPJmL, but even scientists
who have been using LPJmL for a while had to develop their own scripts and tools
for routine data processing instead of using a generic, standardized interface.
Numerous processing scripts have been shared, adapted and reapplied among
scientists. A lot of time was spent on finding solutions for different and
sometimes similar use cases. Nevertheless, the scope of such solutions was
always limited, a never-ending cycle of work that is avoidable.
`lpjmlkit` was developed to eliminate these problems and at the same time create
a standard in the handling of LPJmL to improve the design of simulation experiments
and the transparency of studies with LPJmL. By using the LPJmL Runner
functionality model configurations are stored in a single and unique
configuration file that references the exact model version of LPJmL
used for the simulations to achieve reproducible results.
This way `lpjmlkit` serves as an application programming interface (API) to
LPJmL and provides an easy-to-use basis for interaction with LPJmL in a simple
R script as well as for further software development based on LPJmL. It is now
possible to write functions with LPJmL data for all kinds of applications.
The same is true for performing complex LPJmL simulations.

# Package features


## LPJmL Runner


## LPJmL Data


## miscellaneous


# Acknowledgements

We would like to thank Susanne Rolinski and Marie Hemmen for their
contributions. Special thanks also go to Werner von Bloh for incremental changes
to the LPJmL model, such as writing metafiles.

# References
