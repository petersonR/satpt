
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satpt

<!-- badges: start -->

[![R-CMD-check](https://github.com/deboonstra/satpt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/deboonstra/satpt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This *R* package identifies the saturation point of multinomial
responses from a survey using standard errors of sample proportions.

## Installation

You can install the current stable released version of `satpt` from
[CRAN](https://cran.r-project.org) with

``` r
install.packages("satpt")
```

**when available on CRAN**.

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of `satpt` from
[GitHub](https://github.com/deboonstra/satpt).

``` r
# install.packages("remotes")
remotes::install_github("deboonstra/satpt")
```

## Usage

For basic usage of `satpt` simply specify the responses of the survey in
`y` and the data collection period information in `by`, as seen below.

``` r
library(satpt) # load package
data(diagnoses) # load example diagnoses data
res <- satpt::satpt(y = diagnoses$q2, by = diagnoses$wave)
print(res)
```

    #> Saturation achieved for q2.
    #> With 640 responses, the largest 95% CI half-width is ±3.7 percentage points (within the ±4.9 pp threshold).
    #> 
    #> Analysis based on: q2 
    #> Saturation achieved?  Yes 
    #> 
    #> Overall Sample Proportions and Standard Errors
    #> ==============================================
    #>             y: q2
    #> Statistics   Not at all  Often   Once Rarely Sometimes
    #>   Proportion     0.2531 0.0750 0.0375 0.3688    0.2656
    #>   SE             0.0172 0.0104 0.0075 0.0191    0.0175

Saturation of each individual response category may be examined
graphically while comparing the standard errors to the saturation
threshold.

``` r
graphics::par(oma = c(0, 0, 0, 8))
plot(res)
# adding legend
satpt::legend_right(
  legend = "Saturation\nthreshold",
  col = "firebrick", lty = 3, lwd = 2,
  cex = 0.75
)
```

<img src="man/figures/README-basic-plot.svg" width="100%" />

## Methodology

Presented below is a simplified version of the algorithm that is
employed in `satpt` to determine whether saturation of the responses has
been achieved and whether pooled standard errors should be calculated to
account for response bias.

<img src="man/figures/README-satpt-algorithm.png" width="100%" />

## Learn more

To get started, first read the [*Getting started with
satpt*](https://deboonstra.github.io/satpt/articles/getting-started.html)
vignette. Then, read more about how `satpt` may handle *Select All
Apply* questions in the [*Impelementing with select all apply
questions*](https://deboonstra.github.io/satpt/articles/select-all-apply.html)
vignette.
