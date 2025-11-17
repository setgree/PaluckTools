# BLPlabtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

BLPlabtools documents the [BLP
lab's](http://www.betsylevypaluck.com/) approach to conducting
meta-analyses and analyzing experimental data.

This package integrates functions from three sources:

- [PaluckMetaSOP](https://github.com/setgree/PaluckMetaSOP) -
  meta-analysis functions featured in “[The Contact Hypothesis
  Re-evaluated](https://doi.org/10.1017/bpp.2018.25),” “[Preventing
  Sexual Violence —A Behavioral Problem Without a Behaviorally-Informed
  Solution](https://doi.org/10.1177/15291006231221978),” 
  and "[Meaningfully reducing consumption
  of meat and animal products is an unsolved problem: 
  A meta-analysis](https://doi.org/10.1016/j.appet.2025.108233)" 
  (a meta-analysis that I (Seth Green) co-authored using methods I
  learned at the Paluck Lab.)
- [blpl](https://github.com/johnhenrypezzuto/blpl) - John-Henry
  Pezzuto’s experimental analysis and table formatting tools
- [prejudice-reduction-code-and-data](https://github.com/setgree/prejudice-reduction-code-and-data) -
  additional helper functions for effect size conversion and data
  preparation used to generate the results of "[Prejudice Reduction:
  Progress and Challenges](https://doi.org/10.1146/annurev-psych-071620-030619)" 

## Installation

``` r
remotes::install_github('setgree/BLPlabtools', build_vignettes = TRUE)
```

## What you’ll find

This package contains:

- **Functions** for meta-analysis and experimental analysis
- **Vignettes** that walk through different workflows, focusing on meta-analysis
- **Datasets**:
  - `sv_data` - Sexual violence prevention meta-analysis
  - `contact_data` - Contact hypothesis meta-analysis
  - `map_reduction_data` - Meat/animal product reduction meta-analysis
    (Green et al., 2025)

## Getting started

After installation, run:

``` r
browseVignettes(package = "BLPlabtools")
```

We recommend starting with the overview vignette, which will guide you
to the right workflow for your needs.

## Quick Start

### Calculate effect sizes

``` r
library(BLPlabtools)

# Example: Convert difference in means to Cohen's d
d <- d_calc(stat_type = "d_i_m", stat = 0.7, sample_sd = 0.75)
# Result: d = 0.933

# Calculate variance of the effect size
variance <- var_d_calc(d = d, n_t = 21, n_c = 21)
# Result: variance = 0.092
```

### Run a meta-analysis

``` r
library(BLPlabtools)
library(dplyr)

# Use built-in contact hypothesis data
contact_data |>
  map_robust() |>
  print()

# Shows: pooled effect = 0.29, SE = 0.09, CI, heterogeneity stats
```

### Calculate cluster-robust standard errors

``` r
library(BLPlabtools)
library(broom)
data(contact_data)

# Run a regression
model <- lm(d ~ days_delay + publish, data = contact_data)

# Add cluster-robust SEs by unique_study_id
robust_results <- robust_se(model, cluster = contact_data$unique_study_id)

# View as a formatted table
robust_results[[2]]  # Print coefficient table with cluster-robust SEs

# Or convert to a tidy data frame
broom::tidy(robust_results[[2]])
```

## The functions

### Meta-analysis workflow

#### Effect size calculation:

- `d_calc`: Calculate standardized mean differences (Cohen’s D/Glass’s
  ∆)
- `var_d_calc`: Calculate variance of effect sizes
- `ci_2_sd`, `se_2_sd`, `sd_pooled`, `did_calculator`: Helper functions
  for data preparation

#### Running meta-analyses:

- `map_robust`: Tidyverse-friendly wrapper around `metafor::robust()`
  for robust variance estimation

#### Writing and visualization:

- `study_count`: Count unique studies in grouped data
- `summarise_lm`: Pipe-friendly wrapper for `summary(lm())`
- `summarise_table`: Pipe-friendly version of `table()`
- `make_bib`: Generate bibliography from DOIs
- `write_dockerfile`: Create reproducible research environments

### Experimental analysis workflow

#### Regression analysis:

- `tidy_lm`: Comprehensive function for running multiple regression
  specifications
- `robust_se`: Calculate cluster-robust standard errors

#### Table formatting:

- `star_ready`: Prepare tidy_lm output for stargazer tables
- `table_prep` functions: 14 utilities for LaTeX table formatting
  (add_parentheses, add_endnote, stargazer_pvalues, print_table, etc.)

For more documentation, see the vignettes or add a `?` before any
function (e.g., `?BLPlabtools::tidy_lm`).

## For BLP Lab members

To contribute:

1.  Clone this package:

    ``` bash
    git clone https://github.com/setgree/BLPlabtools.git
    ```

2.  Add or amend functions and vignettes

3.  Open a [pull
    request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)

The first few chapters of [*R packages*](https://r-pkgs.org/) provide
excellent guidance on package development.
