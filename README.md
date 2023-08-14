
# eCerto

<!-- badges: start -->
[![R-CMD-check](https://github.com/janlisec/eCerto/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/eCerto/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/janlisec/eCerto/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/eCerto/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/janlisec/eCerto/branch/main/graph/badge.svg?token=NSY6DITZVH)](https://codecov.io/gh/janlisec/eCerto)

<!-- badges: end -->

<img src="inst/app/www/hex-eCerto.png" width="170" align="right"/>

**eCerto** is an R-package providing functions to perform statistical tests
required during the production of a certified reference material.

The production of certified reference materials (CRMs) is a core task of 
the Bundesanstalt für Materialforschung und -prüfung (BAM). Various statistical 
tests are required and applied depending on the task and recorded data to ensure 
that reported values of CRMs are appropriate. Many of them are calculated 
according to the procedures described in `ISO GUIDE 35:2017`. 

The eCerto package contains a `Shiny`-App which provides functionality to load, 
process and backup data for the statistical evaluation of analyses conducted 
during CRM production.

## Installation

You can install the development version of **eCerto** from 
[GitHub](https://github.com/janlisec/eCerto) with:

``` r
# install.packages("devtools")
devtools::install_github("janlisec/eCerto")
```

## Quick Example

**eCerto** currently has only few exported functions as its use is intended
in form of a `Shiny`-App. You can start this app using:

``` r
eCerto::run_app()
```

## Detailed documentation

A publication is currently in preparation and will be basis of a Vignette.
Most of the functionality is described in the online help which can be found
starting the App from [www.bam.de/eCerto](https://www.bam.de/eCerto).
