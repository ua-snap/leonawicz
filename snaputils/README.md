
<!-- README.md is generated from README.Rmd. Please edit that file -->
snaputils
=========

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/snaputils.svg?branch=master)](https://travis-ci.org/leonawicz/snaputils) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/snaputils?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/snaputils) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/snaputils/master.svg)](https://codecov.io/github/leonawicz/snaputils?branch=master)

`snaputils` contains utilities for Shiny app development. The package inherits primarily from `apputils`, which contains common utility functions, settings and references for use across multiple Shiny apps. It also imports `maputils`. As the name suggests, `snaputils` is specific to a SNAP context. So is `maputils` though separate. Only `apputils` has a more general non-SNAP context. All three packages are satellite members of the [SNAPverse](https://leonawicz.github.io/snapverse/) collection of R packages.

<p style="text-align:center;">
<img src="man/figures/sv_satellites_utils_snap.png" width=350>
</p>
<br>

Installation
------------

You can install maputils from github with:

``` r
# install.packages('devtools')
devtools::install_github("leonawicz/snaputils")
```

Reference
---------

The complete set of satellite packages is shown below.

<p style="text-align:center;">
<img src="man/figures/sv_satellites_all.png" width=350>
</p>
<br>

[Complete package reference and function documentation](https://leonawicz.github.io/snaputils/)
