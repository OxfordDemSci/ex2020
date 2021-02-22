<p align="center">
  <img src="https://github.com/OxfordDemSci/ex2020/blob/master/assets/fig-1-wo2020-ann.png" width="1000"/>
</p>


# Code to Replicate 'Recent Gains in Life Expectancy Reversed by the COVID-19 Pandemic'

[![Generic badge](https://img.shields.io/badge/R-4.0.4-orange.svg)](https://shields.io/)  [![Generic badge](https://img.shields.io/badge/License-GNU-<green>.svg)](https://shields.io/)  [![Generic badge](https://img.shields.io/badge/Maintained-Yes-red.svg)](https://shields.io/)

### Introduction
------------

This is a repository to accompany 'Recent Gains in Life Expectancy Reversed by the COVID-19 Pandemic'. A link to the open-access version of the paper can be found [**here**](https://vimeo.com/462292180). The replication files for this paper include customised functionality written in the [**R**](https://www.r-project.org/) statistical programming language.


### Prerequisites
------------

As a pre-requisite to running this locally, you will need a working installation of R with all of the necessary dependencies installed. 

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
* _tmp_ relates to: a subdir to store temproary files

### Versioning
------------

This version of the code is pre-publication (v.0.1.0). Please note: although the library logs data updates, it could be that additional dictionary based classifications are required with regards to the ```/data/support/dict_replacer_broad.tsv``` file. Please raise an issue in this repo to alert us of any necessary entries, or any suggestions which you may have in general, although we will monitor this over time.

### License
------------

This work is free. You can redistribute it and/or modify it under the terms of the GNU Public license and subject to all prior terms and licenses imposed by the free, public data sources provided by the HMD-STMF, CoverAge-DB, UK-ONS, and US-CDC (i.e. the 'data originators'). The code comes without any warranty, to the extent permitted by applicable law.

### Acknowledgements

We are grateful to the extensive comments provided by Jim Oeppen, Alyson van Raalte and Christiaan Monden. Funding was generously provided through by aBritish Academy’s Newton International Fellowship,the Rockwool Foundation’s Excess Deaths grant, a Leverhulme Trust Large Centre Grant, a John Fell Fund grant and a European Research Council grant.
