<p align="center">
  <img src="https://github.com/OxfordDemSci/ex2020/blob/master/out/figs-png/fig-1-ann.png?raw=true" width="100%"/>
</p>


## Quantifying impacts of the COVID-19 pandemic through life expectancy losses:
### A population-level study of 29 countries

[![DOI](https://zenodo.org/badge/334238621.svg)](https://zenodo.org/badge/latestdoi/334238621)
[![Generic badge](https://img.shields.io/badge/R-4.0.4-orange.svg)](https://shields.io/)
[![Generic badge](https://img.shields.io/badge/License-GNU-<green>.svg)](https://shields.io/)
[![Generic badge](https://img.shields.io/badge/Maintained-Yes-red.svg)](https://shields.io/)
[![Generic badge](https://img.shields.io/badge/BuildPassing-Yes-purple.svg)](https://shields.io/)

### Introduction
------------

This is a repository to accompany 'Quantifying impacts of the COVID-19 pandemic through life expectancy losses: a population-level study of 29 countries'. A link to the open-access version of the paper can be found by clicking [here](https://www.medrxiv.org/content/10.1101/2021.03.02.21252772v3). The replication files for this paper include customised functionality written in the [**R**](https://www.r-project.org/) statistical programming language.

An interactive data visualisation relating to this work can be found here:

<p align="center">
https://covid19.demographicscience.ox.ac.uk/lifeexpectancy
</p>

### Prerequisites
------------

As a pre-requisite to running this locally, you will need a working installation of [**R**](https://www.r-project.org/) with all of the necessary dependencies installed. 

### Running the Code
------------

To run this code, do something like:

```console
$ git clone https://github.com/OxfordDemSci/ex2020.git
```

and then execute each of the scripts (0 through 10) which will undertake sequential tasks like defining skeletons, to undertaking the PCLM, cleaning outputs for analysis, and data visualisation


### Structure
----------------

* _cfg_ relates to: configuration files
* _dat_ relates to: input source data
* _out_ relates to: output data, figures, and sensitivity analysis
* _src_ relates to: code to replicate the wrangling, analysis and visualisation
* _tmp_ relates to: a subdir to store temporary files
* _ass_ relates to: a place to store repo assets

### Versioning
------------

This version of the code is pre-publication (v.0.1.0). If you have any suggestions, please don't hesitate to raise an issue here on this repository, or to e-mail one of the corresponding authors of the paper!

### License
------------

This work is free. You can redistribute it and/or modify it under the terms of the GNU Public license and subject to all prior terms and licenses imposed by the free, public data sources provided by the HMD-STMF, CoverAge-DB, UK-ONS, and US-CDC (i.e. the 'data originators'). The code comes without any warranty, to the extent permitted by applicable law.

### Acknowledgements

We are grateful to the extensive comments provided by Jim Oeppen, Alyson van Raalte, John Ermisch and Christiaan Monden. Funding was generously provided by a British Academy Newton International Fellowship, the Rockwool Foundation’s Excess Deaths grant, a Leverhulme Trust Large Centre Grant, a John Fell Fund grant, a European Research Council grant and the Interdisciplinary Centre on Population Dynamics (CPop).
