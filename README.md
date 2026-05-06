
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satpt

<!-- badges: start -->

[![R-CMD-check](https://github.com/deboonstra/satpt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/deboonstra/satpt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`satpt` helps you decide when your survey has collected enough responses
to stop. Given the responses to one or more questions, it computes how
precisely you have measured each response category and tells you whether
further data collection is likely to change the picture, and if not,
about how many more responses would.

## When does this help?

Use `satpt` when you have a convenience-sample survey (members of a
professional society, patients of a particular health system, etc.)
where chasing higher response rates is expensive or distorting. Instead
of asking *“have we hit our response-rate target?”*, `satpt` asks *“are
our proportions precise enough to act on?”*

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

## Quick start

The most common task is “of all the questions in my survey, which have
stabilized and which haven’t?” `satpt_survey()` answers this in one
call. The `split` argument handles select-all-that-apply questions whose
responses are stored as a single delimited string per row (here, `q1`’s
five tokens are separated by `|`):

``` r
library(satpt)
data(diagnoses) # example: a 643-respondent survey collected in waves

satpt_survey(diagnoses, by = "wave", split = list(q1 = "|"))
```

    #> Saturation analysis: 2 of 2 questions saturated.
    #> ========================================================
    #>  question   n     max_se saturation n_to_saturation
    #>        q1 640 0.01953115       TRUE               0
    #>        q2 640 0.01907114       TRUE               0

For a single question, `satpt()` shows category proportions, standard
errors, and a plain-language headline:

``` r
res <- satpt::satpt(y = diagnoses$q2, by = diagnoses$wave)
print(res)
```

    #> Saturation achieved for q2.
    #> With 640 responses, the largest 95% CI half-width is ±3.7 percentage points (within the ±4.9 pp threshold).
    #> 
    #> Overall Sample Proportions and Standard Errors
    #> ==============================================
    #>             y: q2
    #> Statistics   Not at all  Often   Once Rarely Sometimes
    #>   Proportion     0.2531 0.0750 0.0375 0.3688    0.2656
    #>   SE             0.0172 0.0104 0.0075 0.0191    0.0175

When `satpt()` reports a question is *not* yet saturated, the result
also exposes `res$n_to_saturation`. This is an estimate of how many
additional responses are needed to bring the largest standard error
below `threshold`, assuming current proportions hold.

The default `threshold = 0.025` corresponds to 95% confidence intervals
about ±5 percentage points wide. Tighten it (e.g. `threshold = 0.01`)
when you need finer precision; loosen it when wider intervals are
acceptable.

## Plotting

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

After installing, you can pull up the package’s vignettes locally:

``` r
vignette("getting-started", package = "satpt")
vignette("select-all-apply", package = "satpt")
```

The same vignettes are available online: [*Getting started with
satpt*](https://deboonstra.github.io/satpt/articles/getting-started.html)
and [*Implementing with select-all-apply
questions*](https://deboonstra.github.io/satpt/articles/select-all-apply.html).
