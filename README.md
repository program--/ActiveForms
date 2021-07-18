
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ActiveForms

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**ActiveForms** is a DoD PDF forms parser built with R, Shiny, and
Electron intended for Systems Administrators to easily apply
configuration to networked devices where manually reading forms would
take *too much* time. For example, SAAR (DD 2875) forms for
authorization of user accounts in Active Directory.

## Installation

You can install the developmental version of ActiveForms using
`remotes`:

``` r
# install.packages("remotes")
remotes::install_github("program--/ActiveForms")
```

## Example

ActiveForms can be run by either the `Electron`-built desktop
application or as a web service. To run the web service, simply install
ActiveForms and then call:

``` r
ActiveForms::run_app()
```
