
# eCerto

<!-- badges: start -->
[![R-CMD-check](https://github.com/janlisec/eCerto/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/eCerto/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/janlisec/eCerto/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/eCerto/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/janlisec/eCerto/branch/main/graph/badge.svg)](https://app.codecov.io/gh/janlisec/eCerto?branch=main)
<!-- badges: end -->

<img src="inst/app/www/hex-eCerto.png" width="170" align="right"/>

**eCerto** is an R-package providing functions to perform statistical tests
required during the production of a certified reference material.

The production of certified reference materials (CRMs) is a core task of 
the Bundesanstalt für Materialforschung und -prüfung (BAM). Various statistical 
tests are required and applied depending on the task and recorded data to ensure 
that reported values of CRMs are appropriate. Many of them are calculated 
according to the procedures described in `ISO GUIDE 35:2017`. 

The **eCerto** package contains a `Shiny`-App which provides functionality to 
load, process, report and backup data for the statistical evaluation of analyses 
conducted during CRM production.

## Installation

To run **eCerto** locally you can install the development version from 
[GitHub](https://github.com/janlisec/eCerto) and start the app with:

``` r
# install.packages("devtools")
devtools::install_github("janlisec/eCerto")
eCerto::run_app()
```

Other than that, **eCerto** currently has only few exported functions. Before 
installation you can test the app version hosted at [www.bam.de/eCerto](https://apps.bam.de/shn00/eCerto/)
using your own or the provided example data.

## Detailed documentation

A description of **eCerto** showing a re-evaluation of previously published CRM data 
can be found in this [scientific article](https://doi.org/10.1007/s00216-023-05099-3).
Most of the functionality is also described in the online help.
