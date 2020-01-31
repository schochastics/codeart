
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codeart

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This package is inspired by the generative art of [Eric
Davidson](https://github.com/erdavids/Simulated-Code)

## Installation

You can install the released version of codeart from GitHub with:

``` r
remotes::install_github("schochastics/codeart")
```

## Examples

The code below shows how to transform an existing R script into
“abstract art”.

``` r
library(codeart)
library(showtext)

font_add_google("Niconne", "niconne")
showtext_auto()

dat <- codeart_script("R/codeart_script.R")
plot_codeart_script(dat,font = "niconne",title = "codeart_script.R",caption = "@schochastics")
```

<img src="man/figures/example_script.png">

You can also generate random art with the package.

``` r
library(codeart)
dat <- codeart_random()
plot_codeart_rand(dat)
```

<img src="man/figures/README-example_random-1.png" width="100%" />
