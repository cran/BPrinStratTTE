
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BPrinStratTTE: Causal Effects in Principal Strata Defined by Antidrug Antibodies

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/BPrinStratTTE)](https://CRAN.R-project.org/package=BPrinStratTTE)
<!-- badges: end -->

Bayesian models to estimate causal effects of biological treatments on
time-to-event endpoints in clinical trials with principal strata defined
by the occurrence of antidrug antibodies.

## Scope

- The package contains functions to fit Bayesian principal
  stratification models and to perform clinical trial simulations to
  determine operating characteristics for given scenarios.
- Two-arm clinical trials of biological therapies are considered
  - with an intercurrent event (determining the principal stratum of
    interest) that can only occur in the treated arm (such as the
    development of antidrug antibodies), and
  - with a time-to-event endpoint that is assumed to follow an
    exponential distribution.
- Effect estimators are hazard ratios and restricted mean survival
  times.
- Potential predictors of the intercurrent event can be taken into
  account.
- The models are fitted by Monte Carlo Markov Chain (MCMC) sampling,
  they are coded in [Stan](https://mc-stan.org/) and precompiled.
- More flexible time-to-event distributions (piecewise-exponential and
  Weibull) will be considered in future versions of the package.

## Principal stratification methodology

- Principal stratification is an approach to estimate causal effects in
  partitions of subjects determined by post-treatment events. It was
  introduced in the biostatistical literature by Frangakis and Rubin
  (2002).
- The ICH E9 (R1) addendum on estimands and sensitivity analysis in
  clinical trials proposed principal stratification as one approach to
  deal with intercurrent events in clinical trials (International
  Council for Harmonisation (ICH) (2020)).
- Principal stratum membership is typically not known with certainty. A
  Bayesian approach may be particularly suited to deal with this type of
  uncertainty. Following a proposal by Imbens and Rubin (1997),
  principal stratum membership can be treated as a latent mixture
  variable.
- Motivated by scientific questions arising in clinical trials of
  biological therapies, in this package the approach by Imbens and
  Rubin (1997) is adapted to a specific clinical trial setting with a
  time-to-event endpoint and the intercurrent event only occurring in
  the treated group.
- For recent reviews of applications to clinical trials see Lipkovich et
  al. (2022) and Bornkamp et al. (2021).

<!-- <font size="3"> -->
References: <br>

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bornkamp2021" class="csl-entry">

Bornkamp, B., Rufibach, K., Lin, J., Liu, Y., Mehrotra, D. V.,
Roychoudhury, S., Schmidli, H., Shentu, Y., and Wolbers, M. (2021),
“<span class="nocase">Principal stratum strategy: Potential role in drug
development</span>,” *Pharm Stat*, 20, 737–751.
<https://doi.org/10.1002/pst.2104>.

</div>

<div id="ref-Frangakis2002" class="csl-entry">

Frangakis, C. E., and Rubin, D. B. (2002),
“<span class="nocase">Principal stratification in causal
inference</span>,” *Biometrics*, 58, 21–29.
<https://doi.org/10.1111/j.0006-341x.2002.00021.x>.

</div>

<div id="ref-Imbens1997" class="csl-entry">

Imbens, G. W., and Rubin, D. B. (1997), “Bayesian Inference for Causal
Effects in Randomized Experiments with Noncompliance,” *Ann Stat*, 25,
305–327. <https://doi.org/10.1214/aos/1034276631>.

</div>

<div id="ref-ICHE9R1Guideline" class="csl-entry">

International Council for Harmonisation (ICH) (2020), “ICH E9 (R1)
addendum on estimands and sensitivity analysis in clinical trials to the
guideline on statistical principles for clinical trials.”
<https://database.ich.org/sites/default/files/E9-R1_Step4_Guideline_2019_1203.pdf>.

</div>

<div id="ref-Lipkovich2022" class="csl-entry">

Lipkovich, I., Ratitch, B., Qu, Y., Zhang, X., Shan, M., and
Mallinckrodt, C. (2022), “Using principal stratification in analysis of
clinical trials,” *Stat Med*, 41, 3837–387.
<https://doi.org/10.1002/sim.9439>.

</div>

</div>

<!-- </font> -->

## Installation

The current stable version of the package can be installed from CRAN
with:

``` r
install.packages("BPrinStratTTE")
```

The development version of the package can be installed from GitHub
with:

``` r
if (!require("remotes")) {install.packages("remotes")}
remotes::install_github("Boehringer-Ingelheim/BPrinStratTTE")
```
