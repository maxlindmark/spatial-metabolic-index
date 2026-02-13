# Density-dependence drive range shifts in a rapidly deteriorating range-edge ecosystem
This repo contains data and R code for fitting species distribution models to biomass density data from the BITS trawl survey, to estimate changes in the niche and ranges of cod, flounder and place, in relation to environmental change and population abundance.

### Reproducing Results

To reproduce our results you can either:

1. Fork the repository, clone it, open a new RStudio project with version control, and paste the repo url

2. Download a zip and work locally on your computer

We use [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package versions. Once you've downloaded the project, run `renv::restore()` in your current working directory. This will install the package versions we used when this repository was archived. Note that packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else! `renv` does *not* help with different versions of R. We used R version 4.3.2, and ran the analysis on a 24 GB Apple M2 laptop.

### Repository structure

`R`: code to prepare data, fit models and produce figures

`data`: fish length at temperature data

`figures`: figures including figures for supporting information 