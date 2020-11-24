# dstat
Stata module to compute summary statistics and distribution functions including 
standard errors and optional covariate balancing

`dstat` unites a variety of methods to describe (univariate)
statistical distributions. Covered are density estimation, histograms,
cumulative distribution functions, probability distributions, quantile
functions, lorenz curves, percentile shares, and a large collection
of summary statistics such as classical and robust measures of location, scale,
skewness, and kurtosis, as well as inequality and poverty measures. Particular
features of the command are that it provides consistent standard errors
supporting complex sample designs for all covered statistics and that the
simultaneous analysis of multiple variables across multiple subpopulations is
possible. Furthermore, the command supports covariate balancing based on
reweighting techniques (inverse probability weighting and entropy balancing),
including appropriate correction of standard errors. Standard error estimation
is implemented in terms of influence functions, which can be stored 
for further analysis, for example, using RIF regression.

To install `dstat`, type

    . net install dstat, replace from(https://raw.githubusercontent.com/benjann/dstat/main/)

in Stata. Stata version 14 or newer is required. Furthermore, the `moremata` 
package is required. To install `moremata`, type

    . net install moremata, replace from(https://raw.githubusercontent.com/benjann/moremata/master/)

---

Main changes:

    24nov2020
    - issues encountered with regexr() in Stata 14; no longer using regexr()

    24nov2020 (version 1.0.0):
    - reldist released on GitHub

