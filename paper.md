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
  - name: Sebastian Ostberg
    orcid: 0000-0002-2368-7015
    equal-contrib: true
    affiliation: 1
  - name: Stephen Wirth
    orcid: 0000-0002-2261-8771
    corresponding: true
    affiliation: 1
  - name: Sara Minoli
    orcid: 0000-0002-2261-8771
    corresponding: true
    affiliation: 1
  - name: Fabian Stenzel
    orcid: 0000-0002-5109-0048
    corresponding: true
    affiliation: 1
  - name: Christoph Müller
    orcid: 0000-0002-9491-3550
    corresponding: true
    affiliation: 1
affiliations:
 - name: Potsdam Institute for Climate Impact Research (PIK), Germany
   index: 1

date: 30 January 2023
bibliography: paper.bib

---

# Summary

<img src="inst/img/logo.png" alt="drawing" style="width:24%;"/>

[//]: # (![]\(inst/img/logo.png\){width=24% align=left} -> use for final publication)

The *lpjmlkit* R package [@lpjmlkit_manual] is an open source software for
handling the open source Dynamic Global Vegetation model (DGVM)
[LPJmL](https://github.com/PIK-LPJmL/LPJmL) mainly developed and maintained
by the Potsdam Institute of Climate Impact Research (PIK).
It contains two main modules, one of which (LPJmL Runner) provides the
functionality to create model configurations for model configurations for
multiple simulations at once and run them either on a configured local machine
or on an HPC cluster with SLURM support.\
The other (LPJmL Data) offers a generic read function that supports both
simulations and model input data for multiple file types. The associated data
class LPJmLData manages both the data and the corresponding metadata to ensure
the integrity of both within an instantiated object. LPJmLData objects can be
modified to a limited extent and exported to other common R data formats.
In addition to these modules, further functions that facilitate the
common use cases of LPJmL are included. This article introduces *lpjmlkit*,
an R package that serves as an interface to LPJmL and thus both simplifies
direct work with the model and enables new software developments.


# Statement of need

A simple interface makes software easier to use and improves accessibility,
user experience, and overall adoption. Especially in science, where it is rather
the exception for someone to have a degree in computer science or something
comparable. Often, the progressive development of complex process-based
numerical models in low-level languages like C comes at the expense of
usability, making them accessible only to experienced users.
*lpjmlkit* has come to break with this trend and serve as an API for such
a model called LPJmL, a well-established and widely-used DGVM in the scientific
community.\
LPJmL has been in use for more than a decade and has helped researchers in
conducting numerous studies in various research areas that affect the
terrestrial biosphere. To this end, scientists at PIK added the hydroligcal
cycle to LPJ [@gerten_terrestrial_2004], the previous model name, it was 
complemented by managed land to "LPJmL"
[@bondeau_modelling_2007; @rolinski_modeling_2018] and later the nitrogen cycle
was introduced [@von_bloh_implementing_2018].\
This allowed for broader, interdisciplinary studies such as the work of
@gerten_feeding_2020, which answered the question of whether it is possible to
feed ten billion people within four planetary boundaries, or studies that
implemented features of sustainable agriculture in LPJmL that had never been
simulated before with a DGVM. LPJmL is also used extensively in the context of
simulating natural vegetation dynamics with disturbances such as fire. ...
**TODO** ...\
However, unlike its sister model LPJ-GUESS [@bagnara_r_2019] or other
models at PIK [@dietrich_magpie_2019] LPJmL was never equipped with standardised
interfaces for higher level programming languages to either run simulations or
read and process its data.\
This makes it difficult for beginners to work with LPJmL, but even scientists
who have been using LPJmL for a while have never developed simple, generic
solutions that cover all use cases. Numerous scripts have been shared, adapted
and reapplied among scientists. Bugs were introduced and a lot of time was spent
debugging, updating or adapting them to new model developments.\
`lpjmlkit` was developed to eliminate these problems and at the same time create
a standard in the handling of LPJmL to improve the conduct and transparency of
studies with LPJmL.\
This way `lpjmlkit` serves as an application programming interface (API) to 
LPJmL and provides an easy to use basis for interaction with LPJmL in a simple
R script as well as for further software development based on LPJmL.

# Package features


## LPJmL Runner


## LPJmL Data


## miscellaneous


# Acknowledgements

We would like to thank Susanne Rolinski and Marie Hemmen for their
contributions. Special thanks also go to Werner von Bloh for incremental changes
to the LPJmL model, such as writing metafiles.

# References