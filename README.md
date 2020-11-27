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

To install `dstat` from the SSC Archive, type

    . ssc install dstat, replace

in Stata. Stata version 14 or newer is required. Furthermore, `moremata` and
`coefplot` are required. To install these packages from the SSC Archive, type

    . ssc install moremata, replace
    . ssc install coefplot, replace

---

Installation from GitHub:

    . net install dstat, replace from(https://raw.githubusercontent.com/benjann/dstat/main/)
    . net install moremata, replace from(https://raw.githubusercontent.com/benjann/moremata/master/)
    . net install coefplot, replace from(https://raw.githubusercontent.com/benjann/coefplot/master/)

---

Main changes:

    27nov2020 (version 1.0.4)
    - "version, user" issue now finally fixed (hopefully); the issue was related
      to -set dp comma-

    27nov2020 (version 1.0.3)
    - yet another try to fix the "version, user" issue

    27nov2020 (version 1.0.2)
    - graph option -merge- added
    - added code to circumvent the "version, user" error that appears to occur
      in some variants of Stata installations

    24nov2020 (version 1.0.1)
    - issues encountered with regexr() in Stata 14; no longer using regexr()
    - fixed another awkward Stata 14 issue

    24nov2020 (version 1.0.0):
    - dstat released on GitHub

