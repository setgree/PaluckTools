# BLPlabtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

BLPlabtools documents the [BLP
lab's](http://www.betsylevypaluck.com/) approach to conducting
meta-analyses and analyzing experimental data.

## Installation & getting started

``` r
remotes::install_github('setgree/BLPlabtools', build_vignettes = TRUE)
browseVignettes(package = "BLPlabtools")
```
The overview vignette previews the remainder. 

## This package contains

- **Functions** for meta-analysis and experimental analysis
- **Vignettes** that walk through different workflows, focusing on meta-analysis
- **Datasets**:
  - `sv_data` - data from [sexual violence prevention meta-analysis](https://doi.org/10.1177/15291006231221978)
  - `contact_data` - data from [contact hypothesis meta-analysis](https://doi.org/10.1017/bpp.2018.25)
  - `map_reduction_data` - data from [meat animal product reduction meta-analysis](https://doi.org/10.1016/j.appet.2025.108233)

It draws from four projects:
- [PaluckMetaSOP](https://github.com/setgree/PaluckMetaSOP) -
  meta-analysis functions featured in "[The Contact Hypothesis
  Re-evaluated](https://doi.org/10.1017/bpp.2018.25)" and "[Preventing
  Sexual Violence —A Behavioral Problem Without a Behaviorally-Informed
  Solution](https://doi.org/10.1177/15291006231221978)"
- [blpl](https://github.com/johnhenrypezzuto/blpl) - John-Henry
  Pezzuto’s experimental analysis and table formatting tools
-  [vegan-meta](https://github.com/hsflabstanford/vegan-meta) - code and data for "[Meaningfully reducing consumption
  of meat and animal products is an unsolved problem:
  A meta-analysis](https://doi.org/10.1016/j.appet.2025.108233)"
  (this meta-analysis was not conducted at the BLP Lab but was highly influenced by BLP Lab methods)
- [prejudice-reduction-code-and-data](https://github.com/setgree/prejudice-reduction-code-and-data) -
  additional helper functions for effect size conversion and data
  preparation used to generate the results of "[Prejudice Reduction:
  Progress and Challenges](https://doi.org/10.1146/annurev-psych-071620-030619)"

## A few quick examples:

**Calculate effect sizes**

``` r
library(BLPlabtools)

# Example: Convert difference in means to Cohen's d
d <- d_calc(stat_type = "d_i_m", stat = 0.7, sample_sd = 0.75)
# Result: d = 0.933

# Calculate variance of the effect size
variance <- var_d_calc(d = d, n_t = 21, n_c = 21)
# Result: variance = 0.092
```

**&Run a meta-analysis**

``` r
library(BLPlabtools)
library(dplyr, warn.conflicts = FALSE)

# Use built-in sexual violence prevention data
sv_data |> map_robust() 
```
This prints:
```
# A tibble: 1 × 5
  N_observations N_studies Delta     se pval     
           <int>     <int> <dbl>  <dbl> <noquote>
1            489       295 0.283 0.0251 <.0001   
```

**Calculate cluster-robust standard errors**

``` r
library(BLPlabtools)
data(contact_data)

# basic model:
model <- lm(d ~ days_delay + publish, data = contact_data)

# Add cluster-robust SEs by unique_study_id and
robust_se(model, cluster = contact_data$unique_study_id)[[2]] 
```
This prints
```
t test of coefficients:

               Estimate  Std. Error t value Pr(>|t|)   
(Intercept)  0.49606110  0.14650011  3.3861  0.00244 **
days_delay  -0.00020262  0.00036177 -0.5601  0.58061   
publish     -0.04319455  0.10299523 -0.4194  0.67867   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

## For BLP Lab members

To contribute:

1.  Clone this package (`git clone https://github.com/setgree/BLPlabtools.git`);
2.  Add or amend functions and vignettes
3.  Open a [pull
    request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)

See [*R packages*](https://r-pkgs.org/) for guidance. Or just use Claude Code 😃
