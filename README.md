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

    16jun2025 (version 1.4.8)
    - when computing IFs of concentration curves, -dstat- now uses mm_loclin()
      rather than -lpoly- to obtain the required local linear fit; this implies
      that moremata version 2.0.5 is required

    04jun2025 (version 1.4.7)
    - -dstat graph- failed after -dstat histogram- and -dstat share- in case of
      over() with suboption -contrast-; this is fixed
    - -dstat graph- after -dstat histogram-, -dstat proportion-, or -dstat share-
      now leaves the margin of the plotregion unchanged in case of over() with
      suboption -contrast-; furthermore, base() is now set to 1 (rather than 0)
      in case of over() with suboption -ratio-

    25apr2025 (version 1.4.6)
    - statistic -smse- in -dstat summarize- has been renamed to -rmse- (root mean
      squared error)
    - -dstat summarize- could be unnecessarily slow on small datasets due to an
      unfortunate use of Mata's findexternal(); this is fixed
    - inequality statistic [gw_|w_|b_]ge(alpha) in -dstat summarize- did not
      correctly diagnose out-of-support observations if alpha was 0 or 1; this is
      fixed

    04apr2025 (version 1.4.5)
    - return e(sinfo) added (undocumented)
    - -dstat summarize- now has undocumented option -noclean- to retain duplicate
      statistics

    24mar2023 (version 1.4.4)
    - generate() stored the influence functions of the raw statistics rather than the
      influence functions of the transformed statistics if suboption -lnratio- was
      specified in over(); this also implied that vce(svy) reported the standard errors
      of the raw statistics rather than standard errors of the transformed statistics
      if suboption -lnratio- was specified in over(); this is fixed

    28dec2022 (version 1.4.3)
    - command -dstat (somersd) Y, by(X)- computed D(X|Y) rather than D(Y|X); I now
      changed this so that D(Y|X) is computed, which is more intuitive (and more in
      line with how other asymmetric statistics are computed by dstat); thanks to
      Maurizio Pisati for pointing out this inconsistency

    15dec2022 (version 1.4.2)
    - modified dstat_svyr such that replication-based svy estimators no longer
      apply checks for omitted coefficients; this prevents the estimators from
      failing on results that have zero variance (e.g. a zero-frequency histogram
      bar)

    14dec2022 (version 1.4.1)
    - [no]cov is no longer a suboption within vce(); it is now a regular option
    - dstat predict now has option scaling() to determine the scaling of the
      generated influence functions
    - option nobwfixed added; code to obtain grid and bandwidth in case of
      replication estimators revised
    - revised implementation of vce(svy)
    - revised implementation of predict

    12dec2022 (version 1.4.0)
    - dstat pw did not work with vce() set to bootstrap, jackknife, or svy; this is
      fixed
    - the returned information on sample and population size included observations
      that were excluded from estimation due to missing values if vce(svy) with
      replication-based variance estimation was specified; this is fixed
    - the secondary variable (-by-) can now be string for inequality decomposition
      measures as well as for cohend, mindex, uc[l|r], cramersv, and dissim

    05dec2022 (version 1.3.9)
    - statistic -sdlog- added
    - new methods in citype() for proportions: agresti, exact, jeffreys, wilson
    - citype(normal) can now be abbreviated as citype(norm)
    - reorganized code for computation of CIs
    - dstat graph: overlay can now be specified as a synonym for merge
    - r() from -dstat- is now preserved if option -graph- is specified; this ensures
      that r(table) will be available after running -dstat- with both the -graph-
      option and the -table- option; furthermore, r() from dstat is now also
      preserved if option -generate()- or -rif()- is applied
    - the display routine is now executed even if -quietly- is applied to -dstat-,
      so that r(table) will created even if -quietly- is applied
    - the display routine will now clear preexisting r() even if -notable- is applied
    - -dstat predict- no longer modified r()
    - an informative error message is now displayed if a string variable is
      specified in by(), pline(), or as an argument to a statistic

    21nov2022 (version 1.3.8)
    - dstat density: option [l|r]tight added; requires newest update of moremata

    20oct2022 (version 1.3.7)
    - dstat returned error if option -nose- was applied with statistics that set
      standard errors to zero (e.g. min and max); this is fixed
    
    22sep2022 (version 1.3.6)
    - dstat returned error if histogram method -scott- was specified; this is fixed
    - now using errprintf() to display errors in Mata

    11aug2022 (version 1.3.5)
    - statistic -cohend- added
    - statistic -freq- without argument can now be used to obtain
      overall frequence/sum of weights; can also type -count- 

    17feb2022 (version 1.3.4)
    - dstat pw added (wrapper for dstat summarize to compute pairwise correlations
      and similar)
    - informative error message is now displayed if factor variables are used in
      -dstat proportion- without option -nocategorical-

    14feb2022 (version 1.3.3)
    - additional statistics in dstat sum: -slope- or -b- (regression coefficient;
      may also be used to compute mean difference or risk difference),
      -or- (odds ratio in 2x2 table), -rr- (risk ratio in 2x2 table)
    - version of moremata library is now checked

    17jan2022 (version 1.3.2)
    - option hdtrim() added (trimmed Harrell-Davis quantiles)
    - grid size in _ds_mq_d_init() now 1024+1 because first point will be removed

    11jan2022 (version 1.3.1)
    - now using a properly derived expression for the influence function of 
      Harrell-Davis quantiles (rather than obtaining the IF by analogy to the
      jackknife approach proposed by Harrell and Davis 1982); the new formulas
      lead to slightly different results

    07jan2022 (version 1.3.0)
    - dstat sum: huber, biweight, mad[n], mae[n], mscale now take account of qdef()
    - dstat sum: computation of IFs for winsor, qskew, qw, lqw, rqw revised so that
      qdef() is taken into account (only relevant if qdef=10 or qdef=11)
  
    30dec2021 (version 1.2.9)
    - system for managing selection of observations and temporary results rewritten
      (more systematic, cleaner code, less error prone, more efficient)
    - dstat sum: harmonic mean (hmean) is now set to zero if at least one outcome
      value is equal to zero

    22dec2021 (version 1.2.8)
    - dstat sum: computation of taua was wrong in case of fweights; this is fixed
    - dstat sum: renamed cdfm to mcdf, cdff to fcdf, ccdfm to mccdf, ccdff to fccdf
    - system for parsing syntax of -dstat sum- rewritten (more general, cleaner
      code, easier to manage/expand, better error messages)

    22dec2021 (version 1.2.7)
    - support for qdef(11) added (mid-quantile); option -mquantile- is a synonym
      for qdef(11)
    - dstat sum: mquantile, gw_vlog, w_vlog, b_vlog, ekurtosis, rsquared added
    - dstat sum: now using quad precision when taking cross products in variance,
      sd, cv, md, gini, vlog, sen, sst, takayama, lvar, mse, spearman, skewness,
      kurtosis, gci, corr, cov 
    - default for napprox() increased from 512 to 1024
    - dstat histogram: in case of pweights or iweights, the effective sample size 
      (sum(w)^2/sum(w^2)) is now used instead of the physical number number of
      observations in the rules for selecting the number of bins
    - default bandwidth selector for density estimation is now -dpi(2)-; -sjpi-
      can be erratic on data that contains heaping
    - improved error messages and some code cleaning
    
    05dec2021 (version 1.2.6)
    - IF of b_gini assumes that the order of group means is stable; this is an
      assumption that is typically not very critical; comparison to the jackknife
      illustrates that the IF is quite accurate even in small samples; removed
      the corresponding disclaimer in the help file

    05dec2021 (version 1.2.6)
    - dstat_sum: b_gini added (IF not fully correct yet; may only serve as a rough
      approximation)
    - dstat sum: gw_gini, gw_mld, gw_theil, gw_ge added
    - datat sum: mldwithin renamed to w_mld; mldbetween renamed to b_mld
    - datat sum: theilwithin renamed to w_theil; theilbetween renamed to b_theil
    - datat sum: gewithin renamed to w_ge; gebetween renamed to b_ge

    04dec2021 (version 1.2.5)
    - dstat sum: gewithin and gebetween added
    - dstat sum: IF of dissim made more efficient

    03dec2021 (version 1.2.4)
    - dstat sum: mldwithin, mldbetween, teilwithin, teilbetween, dissim added
    - dstat sum: now using more efficient approach to compute IFs of categorical
      measures (hhi, entropy, mindex, etc)
    - option zvar() is now called by(); zvar() still supported but no longer documented

    27nov2021 (version 1.2.3)
    - -nocasewise- had a bug that could crash -dstat- in some cases; this is fixed

    26nov2021 (version 1.2.2)
    - new system to manage temporary results to improve efficiency of -dstat sum-
    - due to a type the values for gamma and tau_b could be somewhat off if weight
      were specified; this is fixed

    25nov2021 (version 1.2.1)
    - added association statistics: taua, taub, somersd, gamma; using a fast
      algorithm by R. Newson (2006. Efficient Calculation of Jackknife Confidence 
      Intervals for Rank Statistics. Journal of Statistical Software 15/1) to
      compute the difference in the sum of concordant and discordant pairs
    - dstat automatically (and silently) recentered (all) influence functions if
      any IF had a relative error (i.e. deviation from zero relative to the value
      of the statistic) larger than 1e-14; a corresponding warning message was only
      displayed if any IF had a relative error larger than 1e-6; the former type
      of recentering is now discarded; that is, recentering is now only applied
      if at least one relative error is larger than 1e-6 (all IFs will be
      affected) and a warning message is always displayed if recentering is applied
    - option -relax- could cause error in some situations; this is fixed
    - dstat no longer enforces user version 14.2 when writing coefficient names to
      e(b) (enforcing user version 14.2 caused issues with bootstrap and similar
      commands); a consequence of this is that in Stata 15 (and in Stata 16 prior
      to the 30mar2021 update) the results table from -dstat summarize- might look
      slightly awkward if statistics with parameters in parentheses are specified;
      type -version 14: dstat summarize ...- for better output in these cases
    - over-legend is no longer displayed if the coefficients table is suppressed
    - subcmd is now always set to -summarize-, if no known subcmd is specified; for
      example, -datat x1-x5- now works

    20nov2021 (version 1.2.0)
    - a bug in -nocasewise- led to erroneous selection of observations or crashed
      dstat in some situations; this is fixed
    - added statistics for categorical variables: hhi, hhin, gimp, entropy, hill,
      renyi, mindex, uc, cramer

    03aug2021 (version 1.1.9)
    - fixed header layout in Stata 17, employing _coef_table_header options
      introduced in the 13jul2021 update of Stata 17
  
    14jul2021 (version 1.1.8)
    - option -discrete- now allowed in -dstat histogram-; -dstat histogram, discrete-
      is an alias for -dstat proportion, nocategorical-
    - graphs after -dstat proportion- now use a continuous axis instead of a categorical
      axis if option -nocategorical- has been specified
    - -dstat frequency- can now be used as alias for -dstat proportion, frequency-
    - statistic hdquantile() now fully supports weights; computation of influence
      functions has been improved
    - option -qdef(10)- can now be specified to use Harrell-Davis quantiles; option
      -hdquantile- is a synonym for -qdef(10)-

    01jul2021 (version 1.1.7)
    - statistic hdquantile() added
    - SEs of quantile(0) and quantile(1) now set to 0
    - -dstat pdf- now allowed as alias for -dstat pdf-
    - better error message if an invalid subcommand is specified

    30jun2021 (version 1.1.6)
    - additional poverty measures: tip (TIP ordinate) and atip (absolute TIP ordinate)
    - -datat tip- failed if a variables was specified in -pline()- instead of a
      fixed value; this is fixed
    - -dstat tip- no longer returns HCR and PGI in e()

    29jun2021 (version 1.1.5)
    - -dstat tip- (Tip curve) added
    - option range() added to subcommands density, cdf, ccdf, quantile, lorenz, tip
    - association measures added: corr (correlation), cov (covariance), spearman
      (Spearman's rank correlation)
    - additional poverty measures: apgap (absolute poverty gap), apgi (absolute
      poverty gap index)
    - contrast(lag) and contrast(lead) now allowed in over()
    - can now specify custom p1 and p2 with -iqrn-
    - observations with missing on variables specified in zvar() or pline() (or
      corresponding variables specified as arguments to individual statistics) are
      no longer excluded from the overall estimation sample if -nocasewise- is 
      specified
    - number of obs and sum of weights now returned for each parameter in e(nobs)
      and e(sumw)

    23jun2021 (version 1.1.4)
    - additional inequality statistic: hoover index (robin hood index)
    - additional poverty statistics: hcr (head count ratio), pgap (poverty gap),
      pgi (poverty gap index), sen (Sen poverty index), sst (Sen-Shorrocks-Thon),
      takayama (Takayama poverty index), chu (Clark-Hemming-Ulph)
    - new option -pstrong- to employ the "strong" poverty definition; -fgt- now uses
      the "weak" definition by default
    - option -relax- of -dstat summarize- was not included in e() and was not passed 
      through to -predict-; this is fixed
    - the routine computing -md- could break in some contexts; this is fixed

    10jun2021 (version 1.1.2)
    - -predict- could fail after -dstat proportion-; this is fixed
    - contrast options -ratio- and -lnratio- now again supported for statistics
      that are not normalized by the sample size (frequencies, totals)
    - fixed bug that could occur if nocasewise and unconditional were both specified

    07jun2021 (version 1.1.1)
    - option -nocasewise- added
    - option -relax- added
    - dstat now always uses scores for totals/frequencies instead of influence
      functions; (sub)option svy in -predict-, -vce(analytic)- and -vce(cluster)-
      is discontinued; option -unconditional(fixed)- is discontinued; treatment of
      totals/freqs now consistent with survey estimation by default (i.e. supopulation
      sizes are assumed random; number PSUs is assumed fixed); this is different
      from how official command -total- handles subpops if used without -svy-
      prefix
    - contrast options -ratio- and -lnratio- are no longer supported for statistics
      that are not normalized by the sample size (frequencies, totals); -ratio- and
      -lnratio- now imply -contrast-
    - option -compact- of -predict/generate()/rif()- no longer allowed with
      -over(, contrast/accumulate)- or with statistics that are not normalized by
      the sample size
    - dstat summarize applied sorting even if not necessary; this is fixed
    - omitted estimates are no longer flagged in the coefficient names; vector
      e(omit) is now returned
    - density estimation settings are now returned in e() only if density estimation
      has, in fact, been employed; e(bwidth) now has better column names
    - in some situations, dstat histogram computed wrong results for the first bin
      if option balance() was specified; this is fixed
    - _makesymmetric() is now applied to e(V) to remove asymmetry due to possible
       roundoff-error

    22dec2020 (version 1.1.0)
    - results for statistics mad(0,0), madn(0,0), mae(0), and maen(0) were wrong
      in case of weights; this is fixed

    16dec2020 (version 1.0.9)
    - new subopions -contrast()-, -ratio-, -lnratio-, and -accumulate- in -over()-
    - new -common- option in -dstat density-, -dstat histogram-, and -datat [c]cdf-
    - new display options -cref- and -pvalue-
    - citype() now sets CI to missing if value of coef is outside domain of
      transformation function
    - option select() in -dstat graph- can now contain -reverse- instead of a
      numlist

    11dec2020 (version 1.0.8)
    - cluster variable in vce(cluster) can now be string
    - over(..., rescale) now implemented as subcommand-specific option
      -unconditional-; -unconditional(fixed)- added to treat subpopulation
      sizes as fixed
    - dstat cdf/ccdf: specifying -ipolate- together with -floor- returned error; this
      is fixed

    10dec2020 (version 1.0.7)
    - vce(analytic/cluster, svy)
      o svy was not taken into account if no clusters and no weights, iweights, or
        fweights were specified; this is fixes
      o revised code to preserve memory and avoid double work
    - for reasons of consistency, in case of iweights, the sum of weights is now
      reported in e(N)  instead of the physical number of observations

    09dec2020 (version 1.0.6)
    - new option select() in -dstat graph- to select and order subgraphs and plots
    - new suboption select() in over(): select and order subpopulations to be included
      in results; total will still include obs from all groups
    - new suboption -rescale- in over(): rescale results by the relative size of the
      subpopulation
    - suboption -svy- in vce(analytic) and vce(cluster) to compute SEs for
      frequencies and totals like svy does 
    - new statistics: min, max, range, midrange (IFs/SEs will be set to zero for 
      these statistics)
    - vce(svy), vce(bootstrap), and vce(jackknife) now feature suboption [no]cov to
      decide whether to store full e(V) or only e(se); default is -cov- for 
      -dstat summarize- and -nocov- else; with vce(svy) option -nocov- also removes
      auxiliary covariance matrices such as e(V_srs)
    - dstat density: standard errors were correct only in the first subpopulation 
      if -over()- was specified together with -exact-; this is fixed 

    05dec2020 (version 1.0.5)
    - new -dstat ccdf- command for complementary CDF (tail distribution, survival
      function)
    - -dstat cdf- has new options -frequency-, -percent-, -floor-, and -ipolate-
    - additional statistics: total(), cdff(), ccdf(), ccdfm(), ccdff()
    - statistics trim(p1,p2) and winsor(p1,p2) now documented; furthermore, qdef()
      is now taken into account by trim() and winsor()
    - option -sum- in -dstat lorenz- and -dstat share- now documented
    - statistics tlorenz(), tshare(), tccurve(), tcshare() now documented
    - option generate() has a new -svy- suboption to generated scores for survey 
      estimation instead of influence functions; this is only makes a difference for
      unnormalized statistics (frequencies, totals)
    - VCE for unnormalized statistics (frequencies, totals) did not take account of
      the extra uncertainty induced by the variability of the sum of weights in the
      context of survey estimation; this is fixed
    - confidence limits had wrong scale if -percent- was specified, citype() was not
      normal, and width of confidence interval was zero; this is fixed
    - predict after survey estimation with subpop() returned missing in observations
      outside subpop(); the IFs for these observations are now set to 0
    - revised code of some IFs to avoid double work; affected functions are
      dstat_density_IF(), dstat_cdf_IF(), dstat_sum_hist(), dstat_sum_cdf(),
      dstat_sum_cdfm(), dstat_sum_freq()
    - now using pstyle(p#line) instead of pstyle(p#) in graphs if appropriate
    - no longer using mm_repeat(); using J() instead

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

