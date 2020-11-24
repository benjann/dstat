{smcl}
{* 23nov2020}{...}
{viewerjumpto "Syntax" "dstat##syntax"}{...}
{viewerjumpto "Description" "dstat##description"}{...}
{viewerjumpto "Summary statistics" "dstat##stats"}{...}
{viewerjumpto "Options" "dstat##options"}{...}
{viewerjumpto "Examples" "dstat##examples"}{...}
{viewerjumpto "Methods and formulas" "dstat##methods"}{...}
{viewerjumpto "Saved results" "dstat##saved_results"}{...}
{viewerjumpto "References" "dstat##references"}{...}
{hline}
help for {hi:dstat}{...}
{right:{browse "http://github.com/benjann/dstat/"}}
{hline}

{title:Title}

{pstd}{hi:dstat} {hline 2} Summary statistics and distribution functions


{marker syntax}{...}
{title:Syntax}

{pstd}
    Estimation

{pmore}
    Summary statistics

{p 12 17 2}
{cmd:dstat} [{cmdab:su:mmarize}] [{cmd:(}{it:{help dstat##statistics:stats}}{cmd:)}] {varlist}
    [ {cmd:(}{it:{help dstat##statistics:stats}}{cmd:)} {varlist} {it:...} ]
    {ifin} {weight} [{cmd:,}  {help dstat##opts:{it:options}} ]

{pmore}
    Distribution functions

{p 12 17 2}
{cmd:dstat} {it:subcmd} {varlist} {ifin} {weight} [{cmd:,}  {help dstat##opts:{it:options}} ]

{pmore2}
    where {it:subcmd} is

{p2colset 15 28 30 2}{...}
{p2col:{opt d:ensity}}density function{p_end}
{p2col:{opt h:istogram}}histogram{p_end}
{p2col:{opt p:roportion}}frequency distribution{p_end}
{p2col:{opt c:df}}cumulative distribution{p_end}
{p2col:{opt q:uantile}}quantile function{p_end}
{p2col:{opt l:orenz}}lorenz curve{p_end}
{p2col:{opt sh:are}}percentile shares{p_end}

{pmore}
    {it:varlist} may contain factor variables; see {help fvvarlist}.
    {p_end}
{pmore}
    {cmd:fweight}s, {cmd:pweight}s, and {cmd:iweight}s are allowed; see {help weight}.

{pstd}
    Postestimation

{pmore}
    Replay results

{p 12 17 2}
{cmd:dstat} [{cmd:,} {help dstat##repopts:{it:reporting_options}} ]

{pmore}
    Draw graph

{p 12 17 2}
{cmd:dstat} {cmdab:gr:aph}
    [{cmd:,} {help dstat##graph_opts:{it:graph_options}} ]

{pmore}
    Obtain (recentered) influence functions

{p 12 17 2}
    {cmd:predict} {c -(}{help newvarlist##stub*:{it:stub}}{cmd:*} |
        {it:{help newvar:newvar1}} {it:{help newvar:newvar2}} {cmd:...}{c )-} {ifin}
        [{cmd:,} {it:{help dstat##predict_opts:predict_options}} ]


{synoptset 26 tabbed}{...}
{marker opts}{col 5}{help dstat##options:{it:options}}{col 33}Description
{synoptline}
{syntab:{help dstat##mainopts:Main}}
{synopt:{cmdab:o:ver(}{help varname:{it:overvar}}{cmd:)}}compute results for subpopulations defined by {it:overvar}
    {p_end}
{synopt:{opt tot:al}}include results for total population
    {p_end}
{synopt:{cmdab:bal:ance(}{help dstat##balance:{it:spec}}{cmd:)}}balance
    covariates using reweighting; requires {cmd:over()}
    {p_end}
{synopt:{help dstat##repopts:{it:reporting_options}}}reporting options
    {p_end}
{synopt:{opt qdef(#)}}quantile definition
    {p_end}
{synopt:{it:{help dstat##densopts:density_options}}}details of density estimation
    {p_end}
{synopt:{opt noval:ues}}do not use values as coefficient names
    {p_end}
{synopt:{opth vf:ormat(fmt)}}format for coefficient name values
    {p_end}

{syntab:{help dstat##vce:SE/VCE}}
{synopt:{cmd:vce(}{it:vcetype}{cmd:)}}variance estimation method;
    {it:vcetype} may be {cmd:none} (skip variance estimation),
    {cmdab:a:nalytic}, {cmdab:cl:uster} {it:clustvar}, {cmdab:svy}, {cmdab:boot:strap},
    or {cmdab:jack:knife}
    {p_end}
{synopt:{cmd:nose}}alias for {cmd:vce(none)}
    {p_end}
{synopt:{cmd:rif(}{it:names}[{cmd:,} {it:opts}]{cmd:)}}store recentered influence functions
    {p_end}
{synopt:{opt r:eplace}}allow replacing existing variables
    {p_end}

{syntab:{help dstat##sum:Subcommand {bf:summarize}}}
{synopt:{opth z:var(varname)}}default sort variable for concentration measures
    {p_end}
{synopt:{opt pl:ine(#|varname)}}default poverty line
    {p_end}

{syntab:{help dstat##density:Subcommand {bf:density}}}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt:{opth at(numlist)}}custom grid of evaluation points
    {p_end}

{syntab:{help dstat##hist:Subcommand {bf:histogram}}}
{synopt:{opt prop:ortion}}estimate proportions instead of densities
    {p_end}
{synopt:{opt per:cent}}estimate percent instead of densities
    {p_end}
{synopt:{opt freq:uency}}estimate frequencies instead of densities
    {p_end}
{synopt:{cmd:n(}{cmd:#}|{it:{help dstat##hist:method}}{cmd:)}}number of
    histogram bins; default is {cmd:n(sqrt)}
    {p_end}
{synopt:{cmd:ep}}use equal probability bins instead of equal width bins
    {p_end}
{synopt:{opth at(numlist)}}custom bin definitions
    {p_end}

{syntab:{help dstat##prop:Subcommand {bf:proportion}}}
{synopt:{opt per:cent}}estimate percent instead of probabilities
    {p_end}
{synopt:{opt freq:uency}}estimate frequencies instead of probabilities
    {p_end}
{synopt:{opth at(numlist)}}custom list of levels for which to estimate proportions
    {p_end}
{synopt:{opt nocat:egorical}}allow variables that do not comply to Stata's rules
    for factor variables
    {p_end}

{syntab:{help dstat##cdf:Subcommand {bf:cdf}}}
{synopt:{opt mid}}apply mid-distribution adjustment
    {p_end}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt:{opth at(numlist)}}custom grid of evaluation points
    {p_end}
{synopt:{opt disc:rete}}treat data as discrete
    {p_end}

{syntab:{help dstat##quantile:Subcommand {bf:quantile}}}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt:{opth at(numlist)}}custom grid of evaluation points
    {p_end}

{syntab:{help dstat##lorenz:Subcommand {bf:lorenz}}}
{synopt:{opt per:cent}}report percent instead of proportions
    {p_end}
{synopt:{opt general:ized}}estimate generalized Lorenz curve
    {p_end}
{synopt:{opt gap}}estimate equality gap curve
    {p_end}
{synopt:{opt abs:olute}}estimate absolute Lorenz curve
    {p_end}
{synopt:{opth z:var(varname)}}estimate concentration curve with respect to specified variable
    {p_end}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(101)}
    {p_end}
{synopt:{opth at(numlist)}}custom grid of evaluation points
    {p_end}

{syntab:{help dstat##share:Subcommand {bf:share}}}
{synopt:{opt prop:ortion}}estimate proportions instead of densities
    {p_end}
{synopt:{opt per:cent}}estimate percent instead of densities
    {p_end}
{synopt:{opt general:ized}}estimate generalized shares instead of densities
    {p_end}
{synopt:{opt ave:rage}}estimate averages instead of densities
    {p_end}
{synopt:{opth z:var(varname)}}estimate concentration shares with respect to specified variable
    {p_end}
{synopt:{opt n(#)}}number of bins; default is {cmd:n(20)}
    {p_end}
{synopt:{opth at(numlist)}}custom bin definitions
    {p_end}
{synoptline}
{pstd}

{marker graph_opts}{col 5}{help dstat##graph_options:{it:graph_options}}{col 33}Description
{synoptline}
{synopt:{cmd:flip}}change how results are allocated to plots and subgraphs
    {p_end}
{synopt:{cmdab:bys:tats}[{cmd:(}{it:arg}{cmd:)}]}group results by statistics; only relevant for {cmd:dstat summarize}
    {p_end}
{synopt:[{cmd:no}]{cmd:step}}do/do not use step function; only relevant for {cmd:dstat cdf}
    {p_end}
{synopt:{cmdab:noref:line}}suppress equality line; only relevant for {cmd:dstat lorenz}
    {p_end}
{synopt:{opth ref:line(line_options)}}affect rendition of equality line; only relevant for {cmd:dstat lorenz}
    {p_end}
{synopt:{help coefplot:{it:coefplot_options}}}options to be passed through to {helpb coefplot}
    {p_end}
{synoptline}

{marker predict_opts}{col 5}{help dstat##predict_options:{it:predict_options}}{col 33}Description
{synoptline}
{synopt:{opt rif}}store recentered influence functions
    {p_end}
{synopt:{opt com:pact}}store influence functions in compact form; not allowed with {cmd:balance()}
    {p_end}
{synopt:{opt qui:etly}}do not display list of generated variables
    {p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
    {cmd:dstat} provides a unified framework for the analysis of (univariate)
    distributions. It supports the estimation of various distribution
    functions (such as the PDF and CDF, quantiles, probabilities and
    frequencies, histograms, Lorenz and concentration curves) and a large
    collection of summary statistics (classical and robust measures of
    location, scale, skewness, and kurtosis, measures of inequality,
    concentration, and poverty).

{pstd}
    {cmd:dstat} is an estimation command. Its results are stored
    in {cmd:e()} and standard errors are provided for all
    estimates. Variance-covariance estimation is based on influence functions
    (Hampel 1974, Deville 1999) and fully supports complex survey data; see the {helpb dstat##vce:vce()}
    option. Influence functions or recentered influence functions (RIFs) can be
    generated for all statistics supported by {cmd:dstat}, either using the
    {helpb dstat##generate:generate()} or {cmd:rif()} option, or
    by applying {cmd:predict} after estimation.

{pstd}
    {cmd:dstat} supports simultaneous estimation for multiple variables and
    multiple subpopulations, and allows for covariate balancing or
    standardization between subpopulations based on inverse probability
    weighting (IPW) or entropy balancing. See the {helpb dstat##over:over()}
    and {helpb dstat##balance:balance()} options. Standard errors will take
    account of the uncertainty induced by the balancing.

{pstd}
    Basic functionality for graphing results is provided through the
    {cmd:graph()} option or by applying command {cmd:dstat graph}
    after estimation. {cmd:dstat} employs {helpb coefplot} for
    graphing, which needs to be installed on the system; see
    {net "describe coefplot, from(http://fmwww.bc.edu/repec/bocode/c/)":{bf:ssc describe coefplot}}. Furthermore,
    {cmd:dstat} requires the {helpb moremata} package; see
    {net "describe moremata, from(http://fmwww.bc.edu/repec/bocode/m/)":{bf:ssc describe moremata}}.


{marker statistics}{...}
{title:Summary statistics}

{pstd}
    The syntax for specifying summary statistics and variables with
    {cmd:dstat summarize} is

        [ {cmd:(}{it:{help dstat##stats:stats}}{cmd:)} ] {varlist} [ {cmd:(}{it:{help dstat##stats:stats}}{cmd:)} {varlist} {it:...} ]

{pstd}
    where {it:stats} is a space-separated list of statistics as documented below
    and {it:varlist} is a list of numeric variables, possibly including factor
    variables (see {help fvvarlist}). The default statistic is {cmd:mean}. Statistics
    and variables may be repeated. {cmd:dstat} will rearrange the statistics by variables
    and remove duplicate combinations.

{pstd}
    The names of the statistics can be abbreviated and typed in lower or
    uppercase letters (the names will be used as typed in the output; repeated
    statistics with differently typed names will be treated as different
    statistics). If abbreviation is ambiguous, the first matching statistic in
    the sorted list of supported statistics will be used (with the following
    exceptions: {cmd:q} or {cmd:p} can be used for {cmd:quantile}, {cmd:d}
    for {cmd:density}, and {cmd:f} for {cmd:freq}). For example, to obtain the
    geometric mean, you could type {cmd:gmean}, {cmd:gm}, {cmd:GM}, {cmd:gMean},
    {cmd:gme}, or any other variant including at least the first two letters.

{pstd}
    Many of the statistics allow or require one or several arguments in
    parentheses. Parentheses can be omitted if there is only a single numeric
    argument and no space is included between the name and the argument. For
    example, to obtain the 5% trimmed mean you could type {cmd:trim(5)} or
    simply {cmd:trim5} (omitting parentheses also works with numbers that
    contain decimal places, that is, you could type {cmd:trim5.5} to obtain the
    5.5% trimmed mean; in this case, however, parentheses will be added in the
    output).

{synoptset 25 tabbed}{...}
{marker stats}{col 5}{it:stats}{col 33}Description
{synoptline}
{syntab:Points in the distribution}
{synopt:{opt quantile}{cmd:(}{it:p}{cmd:)}}{it:p}/100-quantile; {it:p} in [0,100]
    {p_end}
{synopt:{opt p}{cmd:(}{it:p}{cmd:)}}alias for {cmd:quantile()}
    {p_end}
{synopt:{opt density}{cmd:(}{it:x}{cmd:)}}kernel density at value {it:x}
    {p_end}
{synopt:{opt hist}{cmd:(}{it:x1}{cmd:,}{it:x2}{cmd:)}}histogram density of data within ({it:x1},{it:x2}]
    {p_end}
{synopt:{opt cdf}{cmd:(}{it:x}{cmd:)}}cumulative distribution (CDF) at value {it:x}
    {p_end}
{synopt:{opt cdfm}{cmd:(}{it:x}{cmd:)}}mid-adjusted CDF at value {it:x}
    {p_end}
{synopt:{opt prop}{cmd:(}{it:x}{cmd:)}}proportion of data equal to value {it:x}
    {p_end}
{synopt:{opt prop}{cmd:(}{it:x1}{cmd:,}{it:x2}{cmd:)}}proportion of data within [{it:x1},{it:x2}]
    {p_end}
{synopt:{opt pct}{cmd:(}{it:x}{cmd:)}}percent of data equal to value {it:x}
    {p_end}
{synopt:{opt pct}{cmd:(}{it:x1}{cmd:,}{it:x2}{cmd:)}}percent of data within [{it:x1},{it:x2}]
    {p_end}
{synopt:{opt freq}{cmd:(}{it:x}{cmd:)}}frequency of data equal to value {it:x}
    {p_end}
{synopt:{opt freq}{cmd:(}{it:x1}{cmd:,}{it:x2}{cmd:)}}frequency of data within [{it:x1},{it:x2}]
    {p_end}

{syntab:Location measures}
{synopt:{opt mean}}arithmetic mean
    {p_end}
{synopt:{opt gmean}}geometric mean (data must be positive)
    {p_end}
{synopt:{opt hmean}}harmonic mean (data must be positive)
    {p_end}
{synopt:{opt trim}[{cmd:(}{it:alpha}{cmd:)}]}{it:alpha} trimmed mean; {it:alpha}
    in [0,50]; default is {it:alpha}=25
    {p_end}
{synopt:{opt winsor}[{cmd:(}{it:alpha}{cmd:)}]}{it:alpha} winsorized mean; {it:alpha}
    in [0,50]; default is {it:alpha}=25
    {p_end}
{synopt:{opt median}}median; equal to {cmd:q50}
    {p_end}
{synopt:{opt huber}[{cmd:(}{it:p}{cmd:)}]}Huber M estimate with gaussian efficiency
    {it:p} in [63.7,99.9]; default is {it:p}=95
    {p_end}
{synopt:{opt biweight}[{cmd:(}{it:p}{cmd:)}]}biweight M estimate with gaussian
    efficiency {it:p} in [.01,99.9]; default is {it:p}=95
    {p_end}
{synopt:{opt hl}}Hodges-Lehmann location measure (Hodges and Lehmann 1963)
    {p_end}

{syntab:Scale measures}
{synopt:{opt sd}[{cmd:(}{it:df}{cmd:)}]}standard deviation; {it:df} applies
    small-sample adjustment; default is {it:df}=1
    {p_end}
{synopt:{opt variance}[{cmd:(}{it:df}{cmd:)}]}variance; default is {it:df}=1
    {p_end}
{synopt:{opt mse}[{cmd:(}{it:t}[{cmd:,}{it:df}]{cmd:)}]}mean squared error from target
    {it:t}; default is {it:t}=0 and {it:df}=0
    {p_end}
{synopt:{opt smse}[{cmd:(}{it:t}[{cmd:,}{it:df}]{cmd:)}]}square-root of mean
    squared error; default is {it:t}=0 and {it:df}=0
    {p_end}
{synopt:{opt iqr}[{cmd:(}{it:p1}{cmd:,}{it:p2}{cmd:)}]}interquantile range; default
    is {cmd:iqr(25,75)} (interquartile range)
    {p_end}
{synopt:{opt iqrn}}normalized interquartile range; equal to
    1 / (invnormal(0.75) - invnormal(0.25)) * {cmd:iqr}
    {p_end}
{synopt:{opt mad}[{cmd:(}{it:l}[{cmd:,}{it:t}]{cmd:)}]}median (or mean if {it:l}!=0)
    absolute deviation from the median (or mean if {it:t}!=0)
    {p_end}
{synopt:{opt madn}[{cmd:(}{it:l}[{cmd:,}{it:t}]{cmd:)}]}normalized MAD; equal to
    1/invnormal(0.75) * {cmd:mad} (or sqrt(pi/2) * {cmd:mad} if {it:l}!=0)
    {p_end}
{synopt:{opt mae}[{cmd:(}{it:l}[{cmd:,}{it:t}]{cmd:)}]}median (or mean if {it:l}!=0)
    absolute error from target {it:t}; default is {it:t}=0
    {p_end}
{synopt:{opt maen}[{cmd:(}{it:l}[{cmd:,}{it:t}]{cmd:)}]}normalized MAE; equal to
    1/invnormal(0.75) * {cmd:mae} or (sqrt(pi/2) * {cmd:mae} if {it:l}!=0)
    {p_end}
{synopt:{opt md}}mean absolute pairwise difference; equal to 2 * {cmd:mean} * {cmd:gini}
    {p_end}
{synopt:{opt mdn}}normalized mean absolute pairwise difference; equal to sqrt(pi)/2 * {cmd:md}
    {p_end}
{synopt:{opt mscale}[{cmd:(}{it:bp}{cmd:)}]}M estimate of scale with breakdown
    point {it:bp} in [1,50]; default is {it:bp}=50
    {p_end}
{synopt:{opt qn}}Qn scale coefficient (Rousseeuw and Croux 1993)
    {p_end}

{syntab:Skewness measures}
{synopt:{opt skewness}}skewness
    {p_end}
{synopt:{opt qskew}[{cmd:(}{it:alpha}{cmd:)}]}quantile skewness (Hinkley 1975);
    {it:alpha} in [0,50]; default is {it:alpha}=25
    {p_end}
{synopt:{opt mc}}medcouple (Brys et al. 2004)
    {p_end}

{syntab:Kurtosis measures}
{synopt:{opt kurtosis}}kurtosis
    {p_end}
{synopt:{opt qw}[{cmd:(}{it:alpha}{cmd:)}]}quantile tail weight; {it:alpha}
    in [0,50]; default is {it:alpha}=25
    {p_end}
{synopt:{opt lqw}[{cmd:(}{it:alpha}{cmd:)}]}left quantile tail weight; {it:alpha}
    in [0,50]; default is {it:alpha}=25
    {p_end}
{synopt:{opt rqw}[{cmd:(}{it:alpha}{cmd:)}]}right quantile tail weight;
    {it:alpha} in [0,50]; default is {it:alpha}=25
    {p_end}
{synopt:{opt lmc}}left medcouple tail weight measure (Brys et al. 2006)
    {p_end}
{synopt:{opt rmc}}right medcouple tail weight measure (Brys et al. 2006)
    {p_end}

{syntab:Inequality measures}
{synopt:{opt gini}[{cmd:(}{it:df}{cmd:)}]}Gini coefficient; {it:df} applies
    small-sample adjustment; default is {it:df}=0
    {p_end}
{synopt:{opt agini}[{cmd:(}{it:df}{cmd:)}]}absolute Gini coefficient
    {p_end}
{synopt:{opt mld}}mean log deviation; equal to {cmd:ge(0)}
    {p_end}
{synopt:{opt theil}}Theil index; equal to {cmd:ge(1)}
    {p_end}
{synopt:{opt cv}[{cmd:(}{it:df}{cmd:)}]}coefficient of variation; default is {it:df}=1;
    {cmd:cv(0)}=sqrt(2*{cmd:ge(1)})
    {p_end}
{synopt:{opt ge}[{cmd:(}{it:alpha}{cmd:)}]}generalized entropy (Shorrocks 1980)
    with parameter {it:alpha}
    {p_end}
{synopt:{opt atkinson}[{cmd:(}{it:epsilon}{cmd:)}]}Atkinson index with parameter
    {it:epsilon}>=0; default is {it:epsilon}=1
    {p_end}
{synopt:{opt lvar}[{cmd:(}{it:df}{cmd:)}]}logarithmic variance; default is {it:df}=1
    {p_end}
{synopt:{opt vlog}[{cmd:(}{it:df}{cmd:)}]}variance of logarithm; default is {it:df}=1
    {p_end}
{synopt:{opt top}[{cmd:(}{it:p}{cmd:)}]}outcome share of top {it:p} percent; default
    is {it:p}=10
    {p_end}
{synopt:{opt bottom}[{cmd:(}{it:p}{cmd:)}]}outcome share of bottom {it:p} percent;
    default is {it:p}=40
    {p_end}
{synopt:{opt mid}[{cmd:(}{it:p1}{cmd:,}{it:p2}{cmd:)}]}outcome share of mid
    {it:p1} to {it:p2} percent; default is {it:p1}=40 and {it:p2}=90
    {p_end}
{synopt:{opt palma}}palma ratio; equal to {cmd:top}/{cmd:bottom} or {cmd:sratio(40,90)}
    {p_end}
{synopt:{opt qratio}[{cmd:(}{it:p1}{cmd:,}{it:p2}{cmd:)}]}quantile ratio
    {cmd:q}({it:p2})/{cmd:q}({it:p1}); default is {it:p1}=10 and {it:p2}=90
    {p_end}
{synopt:{opt sratio}[{cmd:(}{it:u1}{cmd:,}{it:l2}{cmd:)}]}percentile share ratio;
    default is {it:u1}=10 and {it:l2}=90
    {p_end}
{synopt:{opt sratio}[{cmd:(}{it:l1}{cmd:,}{it:u1}{cmd:,}{it:l2}{cmd:,}{it:u2}{cmd:)}]}percentile
    share ratio; default is {it:l1}=0, {it:u1}=10, {it:l2}=90, {it:u2}=100
    {p_end}
{synopt:*{cmd:lorenz}{cmd:(}{it:p}{cmd:)}}Lorenz ordinate, {it:p} in [0,100];
    prefix {it:*} is empty for default, {cmd:g} for generalized,
    {cmd:a} for absolute, {cmd:e} for equality gap
    {p_end}
{synopt:*{cmd:share}{cmd:(}{it:p1}{cmd:,}{it:p2}{cmd:)}}percentile
    share, {it:p1} and {it:p2} in [0,100]; prefix {it:*} is empty for default,
    {cmd:d} for density, {cmd:g} for generalized, {cmd:a} for average
    {p_end}

{syntab:Concentration measures}
{synopt:{opt gci}{cmd:(}{it:{help varname:zvar}}[{cmd:,}{it:df}]{cmd:)}}Gini concentration index;
    {it:zvar} specifies the sort variable; {it:df} applies small-sample adjustment; default is {it:df}=0
    {p_end}
{synopt:{opt gci}[{cmd:(}{it:df}{cmd:)}]}{cmd:gci} using sort variable from option {cmd:zvar()}
    {p_end}
{synopt:{opt aci}{cmd:(}{it:{help varname:zvar}}[{cmd:,}{it:df}]{cmd:)}}absolute Gini concentration index;
    {it:zvar} and {it:df} are as for {cmd:gci}
    {p_end}
{synopt:{opt aci}[{cmd:(}{it:df}{cmd:)}]}{cmd:aci} using sort variable from option {cmd:zvar()}
    {p_end}
{synopt:*{cmd:ccurve}{cmd:(}{it:p}[{cmd:,}{it:{help varname:zvar}}]{cmd:)}}concentration curve ordinate,
    {it:p} in [0,100]; prefix {it:*} is empty for default, {cmd:g} for generalized, {cmd:t} for total,
    {cmd:a} for absolute, {cmd:e} for equality gap
    {p_end}
{synopt:*{cmd:cshare}{cmd:(}{it:p1}{cmd:,}{it:p2}[{cmd:,}{it:{help varname:zvar}}]{cmd:)}}concentration share,
    {it:p1} and {it:p2} in [0,100]; prefix {it:*} is empty for default, {cmd:d} for density,
    {cmd:g} for generalized, {cmd:t} for total, {cmd:a} for average
    {p_end}

{syntab:Poverty measures}
{synopt:{opt watts}[{cmd:(}{it:pline}{cmd:)}]}Watts index (see, e.g., Saisana 2014); {it:pline} specifies the poverty line(s) > 0; {it:pline} can
    be {varname} or {it:#}; the default is as set by option {cmd:pline()}
    {p_end}
{synopt:{opt fgt}[{cmd:(}{it:a}[{cmd:,}{it:pline}]{cmd:)}]}Foster–Greer–Thorbecke index with {it:a}>=0
    (Foster et al. 1984, 2010); default is {it:a}=0 (headcount ratio);
    {it:pline} specifies the poverty line(s) > 0; {it:pline} can be {varname} or {it:#}; the default is as set by option {cmd:pline()}
    {p_end}
{synoptline}


{marker options}{...}
{title:Options}

{marker mainopts}{...}
{dlgtab:Main}

{marker over}{...}
{phang}
    {cmd:over(}{help varname:{it:overvar}}{cmd:)} computes results for each subpopulation defined
    by the values of {it:overvar}.

{phang}
    {cmd:total} reports additional results across all subpopulations. {cmd:total}
    only has an effect if {cmd:over()} is specified.

{marker balance}{...}
{phang}
    {cmd:balance(}{it:spec}{cmd:)} balances covariate distributions between
    subpopulations using reweighting. {opt balance()} requires {cmd:over()}. The
    syntax of {it:spec} is

            [{it:method}{cmd::}] {varlist} [{cmd:,} {it:options}]

{pmore}
    where {it:method} is either {cmd:ipw} for inverse probability weighting based
    on logistic regression (the default) or {cmd:eb} for entropy balancing using
    {helpb mf_mm_ebal:mm_ebal()} from {helpb moremata}, and
    {it:varlist} specifies the list of covariates to be balanced (factor-variable
    notation is allowed). For information on inverse probability weighting
    see, e.g., DiNardo et al. (1996) and {helpb teffects ipw}. For entropy balancing see
    Hainmueller (2012) and Section 3.8 in
    {browse "http://ideas.repec.org/p/bss/wpaper/35.html":Jann (2020)}. {it:options} are as follows:

{phang2}
    {opt ref:erence(#)} identifies the reference distribution. The default is
    use the total across all subpopulations as the reference distribution. Specify
    {cmd:reference(}{it:#}{cmd:)} to obtain the reference distributions from
    observations for which {it:overvar}={it:#}.

{phang2}
    {it:logit_options} are options to be passed through to {helpb logit}. {it:logit_options}
    are only allowed if {it:method} is {cmd:ipw}.

{phang2}
    {opt btol:erance(#)}, {it:#}>=0, specifies the tolerance for the entropy
    balancing algorithm. The default is {cmd:btolerance(1e-5)}. A warning
    message is displayed if a balancing solution is not within the specified
    tolerance. {cmd:btolerance()} is only allowed if {it:method} is {cmd:eb}.

{phang2}
    {opt noi:sily} displays the output of the balancing procedure.

{phang2}
    {opt gen:erate(newvar)} stores the balancing weights in variable
    {it:newvar}. This is useful if you want to check whether covariates have been
    balanced successfully.

{marker repopts}{...}
{phang}
    {it:reporting_options} are options affecting how results are reported. The options
    are as follows:

{phang2}
    {opt l:evel(#)} specifies the confidence level, as a percentage, for
    confidence intervals. The default is {cmd:level(95)} or as set by
    {helpb set level}.

{phang2}
    {opt citype(type)} specifies the method for the computation of the
    confidence interval limits. {it:type} can be
    {cmd:normal} (no transformation), {cmd:logit} or {cmd:probit}
    (logit or probit transformation; useful for statistics in [0,1]),
    {cmd:atanh} (inverse hyperbolic tangent transformation;
    useful for statistics in [-1,1]), or {cmd:log} (logarithmic transformation;
    useful for statistics >=0). The default depends on subcommand
    and options. Use {cmd:citype()} to override the default.

{pmore2}
    {cmd:dstat} will store the confidence limits given {cmd:level()} and
    {cmd:citype()} in {cmd:e(ci)}. Replaying the results with different settings
    will update {cmd:e(ci)}. (In Stata 14, normal confidence intervals will be
    displayed in the output table irrespective of the contents of {cmd:e(ci)}.)

{phang2}
    {opt nohead:er} suppress the output header.

{phang2}
    {opt notab:le} suppresses the output table containing the estimated
    coefficients. {opt tab:le} enforces displaying the table.

{marker display_opts}{...}
{phang2}
    {it:display_options} are standard reporting options such as {cmd:cformat()} or
    {cmd:coeflegend}; see the Reporting options
    in {helpb estimation options:[R] Estimation options}.
    {synoptset 26 tabbed}{...}

{phang2}
    {opt gr:aph}[{cmd:(}{help dstat##graph_options:{it:graph_options}}{cmd:)}]
    displays the results in a graph using {helpb coefplot}. The coefficients
    table will be suppressed in this case (unless option {cmd:table} is
    specified). Alternatively, use command {cmd:dstat graph} to display the
    graph after estimation.

{phang}
    {opt qdef(#)} sets the quantile definition to be used when computing
    quantiles, with {it:#} in {c -(}0,...,9{c )-}. The default is
    {cmd:qdef(2)}. Definitions 1-9 are as described in Hyndman and Fan
    (1996), definition 0 is the "hight" quantile; see
    {helpb mf_mm_quantile:mm_quantile()} for more information.

{marker densopts}{...}
{phang}
    {it:density_options} set the details of density
    estimation. These settings are relevant for command
    {cmd:dstat density} and statistic {cmd:density()} as well as for
    the computation of influence functions that involve density
    estimation (e.g., the influence function of a quantile). For more information
    on density estimation see {helpb mf_mm_density:mm_density()},
    {browse "http://boris.unibe.ch/69421/2/kdens.pdf":Jann (2007)}, and
    Wand and Jones (1995). The options are as follows:

{phang2}
    {cmdab:bw:idth(}{it:method}[{cmd:,} {opt adj:ust(#)} {cmd:rd}]{cmd:)}
    specifies how the bandwidth of the kernel is determined. Possible choices
    for {it:method} are:

{p2colset 17 32 34 2}{...}
{p2col:{cmdab:s:ilverman}}optimal of Silverman
    {p_end}
{p2col:{cmdab:n:ormalscale}}normal scale rule
    {p_end}
{p2col:{cmdab:o:versmoothed}}oversmoothed rule
    {p_end}
{p2col:{opt sj:pi}}Sheather-Jones solve-the-equation plug-in
    {p_end}
{p2col:{cmdab:d:pi}[{cmd:(}{it:#}{cmd:)}]}Sheather-Jones direct plug-in,
    where {it:#} specifies the number of stages of functional estimation;
    default is {cmd:2}
    {p_end}
{p2col:{opt isj}}diffusion estimator bandwidth (Botev et al. 2010)
    {p_end}

{pmore2}
    The default is {cmd:bwidth(sjpi)}. Suboption {opt adjust(#)}, with #>0, can be
    used to adjust the automatic bandwidth by factor {it:#}. Suboption {cmd:rd}
    applies relative-data correction to the automatic bandwidth (Cwik and Mielniczuk 1993).

{pmore2}
    Instead of using an automatic bandwidth selector, specify {opth bwidth(numlist)}
    to set the bandwidth to a specific value. If {it:numlist} contains multiple values,
    the values are used one after the other across the variables and
    subpopulations (recycling values if needed). The specified values must be larger
    than zero.

{phang2}
    {opt k:ernel(kernel)} specifies the kernel function. {it:kernel} may
    be {opt e:panechnikov}, {opt epan2} (alternative Epanechnikov kernel
    function), {opt b:iweight}, {opt triw:eight}, {opt c:osine},
    {opt g:aussian}, {opt p:arzen}, {opt r:ectangle} or {opt t:riangle}. The default
    is {cmd:kernel(gaussian)}.

{phang2}
    {opt adapt:ive(#)} specifies the number of iterations used by the adaptive
    kernel density estimator. The default is {cmd:adaptive(0)} (non-adaptive
    density estimator).

{phang2}
    {cmd:exact} causes the exact kernel density estimator to be used instead
    of the binned approximation estimator. The exact estimator can be slow in large
    datasets if the density is evaluated at many points.

{phang2}
    {opt na:pprox(#)} specifies the grid size used by the binned approximation
    density estimator (and by the data-driven bandwidth selectors). The default
    is {cmd:napprox(512)}.

{phang2}
    {opt pad(#)} specifies the padding proportion of the approximation grid. Default is
    {cmd:pad(0.1)}.

{phang2}
    {opt ll(#)} specifies the lower boundary of the support of data and causes
    boundary-correction to be applied to the density estimate. Error will be
    returned if the data contains values smaller than {it:#}.

{phang2}
    {opt ul(#)} specifies the upper boundary of the support of data and causes
    boundary-correction to be applied to the density estimate. Error will be
    returned if the data contains values larger than {it:#}.

{phang2}
    {opt bo:undary(method)} sets the type of boundary correction. Choices are
    {opt ren:orm} (renormalization method; the default), {opt refl:ect} (reflection method), or
    {opt lc} (linear combination technique). This is only relevant if {cmd:ll()} or {cmd:ul()}
    has been specified.

{phang}
    {opt novalues} prevents using the values of the evaluation points as
    coefficient names. This is not relevant for {cmd:dstat summarize}. If {cmd:novalues}
    is specified, the coefficients will be named as {it:stub#}, where
    {it:#} is consecutive number and {it:stub} is
    {cmd:d} in case of {cmd:dstat density},
    {cmd:h} in case of {cmd:dstat histogram},
    {cmd:p} in case of {cmd:dstat proportion},
    {cmd:c} in case of {cmd:dstat cdf},
    {cmd:q} in case of {cmd:dstat quantile},
    {cmd:l} in case of {cmd:dstat lorenz},
    {cmd:s} in case of {cmd:dstat share} (and for {cmd:dstat histogram} and
    {cmd:dstat share} the last coefficient, i.e. the upper limit of last bin, will be named
    {cmd:_ul}).

{phang}
    {opth vformat(fmt)} sets the display format used to create coefficient names
    from evaluation points. This is not relevant for {cmd:dstat summarize}. See
    help {helpb format} for available formats.

{marker vce}{...}
{dlgtab:SE/VCE}

{phang}
    {opt vce(vcetype)} determines how standard errors are computed. {it:vcetype} may be:

            {opt none}
            [{opt a:nalytic}] [{cmd:,} [{cmd:no}]{cmd:cov} ]
            {opt cl:uster} {it:clustvar} [{cmd:,} [{cmd:no}]{cmd:cov} ]
            {opt svy} [{help svy##svy_vcetype:{it:svy_vcetype}}] [{cmd:,} {help svy##svy_options:{it:svy_options}} ]
            {opt boot:strap} [{cmd:,} {help bootstrap:{it:bootstrap_options}} ]
            {opt jack:knife} [{cmd:,} {help jackknife:{it:jackknife_options}} ]

{pmore}
    {cmd:vce(none)} omits the computation of standard errors. This saves computer
    time.

{pmore}
    {cmd:vce(analytic)}, the default, computes standard errors based
    on influence functions. Likewise, {bind:{cmd:vce(cluster} {it:clustvar}{cmd:)}} computes
    influence-function based standard errors allowing for intragroup correlation,
    where {it:clustvar} specifies to which group each observation
    belongs. In both cases, option {cmd:cov} requests that the full variance-covariance matrix
    is estimated and stored in {cmd:e(V)}, whereas option {cmd:nocov} requests that only the
    standard errors are estimated and stored in vector {cmd:e(se)}. The default is
    {cmd:cov} for subcommand {cmd:summarize} and {cmd:nocov} for all other
    subcommands. {cmd:nocov} reduces computer time and saves memory
    if the number of evaluation points is large (for example, if you estimate the
    density using 400 points across two subpopulations, the covariance matrix has
    800 x 800 = 640'000 elements; the vector of standard errors has only 800 elements).

{pmore}
    {cmd:vce(svy)} computes standard errors taking the survey design as set by
    {helpb svyset} into account. The syntax is equivalent to the syntax of the {helpb svy}
    prefix command; that is, {cmd:vce(svy)} is {cmd:dstat}'s way to support
    the {helpb svy} prefix.

{pmore}
    {cmd:vce(bootstrap)} and {cmd:vce(jackknife)} compute standard errors using
    {helpb bootstrap} or {helpb jackknife}, respectively; see help {it:{help vce_option}}.

{pmore}
    If a replication technique is used for standard error estimation,
    i.e. {cmd:vce(bootstrap)}, {cmd:vce(jackknife)}, {cmd:vce(svy)} with
    {help svy##svy_vcetype:{it:svy_vcetype}} other than {cmd:linearized},
    the bandwidth used for density estimation will be held fixed across
    replications (that is, if relevant, the bandwidth will be determined upfront
    and then held constant). If you want to repeat bandwidth
    search in each replication, use {helpb bootstrap}, {helpb jackknife}, or {helpb svy}
    as a prefix command.

{phang}
    {cmd:nose} is an alias for {cmd:vce(none)}. {cmd:nose} overrides {cmd:vce(analytic)} and
    {cmd:vce(cluster)}, but has no effect if specified together with
    {cmd:vce(svy)}, {cmd:vce(bootstrap)}, or {cmd:vce(jackknife)}.

{marker generate}{...}
{phang}
    {cmd:generate(}{it:names}[{cmd:,} {it:options}]{cmd:)} stores the influence functions that were used
    to compute the standard errors, where {it:names} is either a list of (new) variable names
    or {help newvarlist##stub*:{it:stub}}{cmd:*} to create names {it:stub}{cmd:1},
    {it:stub}{cmd:2}, etc. {it:options} are {cmd:rif} to store RIFs, {cmdab:com:pact}
    to merge the influence functions across subpopulations, and {cmdab:qui:etly}
    to suppress output; see {it:{help dstat##predict_options:predict_options}} below.

{phang}
    {cmd:rif(}{it:names}[{cmd:,} {it:options}]{cmd:)} is an alias for
    {cmd:generate(}{it:names}{cmd:,} {cmd:rif} [{it:options}]{cmd:)}.

{phang}
    {opt replace} allows replacing existing variables.

{marker sum}{...}
{dlgtab:Subcommand summarize}

{phang}
    {opth zvar(varname)} specifies a default sort variable for concentration
    measures.

{phang}
    {opt pline(#|varname)} specifies a default poverty line for poverty
    measures, either as a single value or as a variable containing observation-specific
    values.

{marker density}{...}
{dlgtab:Subcommand density}

{phang}
    {opt n(#)} sets the number of points for which the density is to
    be estimated. A regular grid of {it:#} points spanning the
    data range (within subpopulation; plus some padding) will be used. The
    default is {cmd:n(99)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} specifies a custom grid of evaluation points. Only
    one of {cmd:n()} and {cmd:at()} is allowed.

{marker hist}{...}
{dlgtab:Subcommand histogram}

{phang}
    {opt proportion} estimates proportions instead of densities.

{phang}
    {opt percent} estimates percent instead of densities.

{phang}
    {opt frequency} estimates frequencies instead of densities.

{phang}
    {cmd:n(}{cmd:#}|{it:method}{cmd:)} selects
    (the method to determine) the number of histogram bins. Specify {opt n(#)}
    to use {it:#} bins. Alternatively, specify {opt n(method)} to determine the
    number of bins automatically, where {it:method} may be one of the following:

{p2colset 13 23 25 2}{...}
{p2col:{opt sq:rt}}modified square-root choice as used by {helpb histogram}
    {p_end}
{p2col:{opt st:urges}}Sturges' formula
    {p_end}
{p2col:{opt ri:ce}}Rice rule
    {p_end}
{p2col:{opt do:ane}}Doane's formula
    {p_end}
{p2col:{opt sc:ott}}Scott's normal reference rule
    {p_end}
{p2col:{opt fd}}Freedman–Diaconis' choice
    {p_end}
{p2col:{opt ep}}power-maximizing number of equiprobable bins
    {p_end}

{pmore}
    The default is {cmd:n(sqrt)}; see help {helpb histogram} for details on this
    rule. For the other rules see {browse "http://en.wikipedia.org/wiki/Histogram"}. The generated
    bins will span the range of the observed data (within subpopulation).

{phang}
    {cmd:ep} uses equal probability bins (approximately) instead of equal
    width bins.

{phang}
    {opth at(numlist)} specifies custom cutpoints for the bins (in ascending
    order). If {it:numlist} contains {it:n} numbers, {it:n}-1 bins will be
    created. Note that the constructed bins will cover all data only if the first
    cutpoint is smaller than or equal to the minimum of the data and the last
    cutpoint is larger than or equal to the maximum ({cmd:dstat} does {it:not} check
    this condition and does not display a warning if the condition is violated).

{marker prop}{...}
{dlgtab:Subcommand proportion}

{phang}
    {opt percent} estimates percent instead of proportions.

{phang}
    {opt frequency} estimates frequencies instead of proportions.

{phang}
    {opth at(numlist)} provides a custom list of levels for which to estimate
    proportions. The default is to use all levels observed in the data (across
    subpopulations).

{phang}
    {opt nocat:egorical} allows outcome variables that do not comply to
    Stata's rules for factor variables (e.g. variables that contain negative
    or noninteger values). This also affects how the coefficients are
    labeled in the output.

{marker cdf}{...}
{dlgtab:Subcommand cdf}

{phang}
    {opt mid} applies mid-distribution adjustment to the estimated CDF. The
    empirical distribution function is a step function with a step at each
    observed level in the data and step sizes proportional to the
    observed frequencies of the levels. If {cmd:mid} is specified, the empirical
    distribution function is shifted downwards at each level by half the
    relevant step size.

{phang}
    {opt n(#)} sets the number of points at which the CDF is to be
    evaluated. A regular grid of {it:#} points spanning the
    observed data range (within subpopulation) will be used. The default is
    {cmd:n(99)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of points at which to evaluate
    the CDF. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {cmd:discrete} treats the data as discrete. In this case, the CDF will
    be estimated at each level observed in the data
    (across all subpopulations). Option {cmd:n()} is not allowed if
    {cmd:discrete} is specified.

{marker quantile}{...}
{dlgtab:Subcommand quantile}

{phang}
    {opt n(#)} sets the number of quantiles to be computed. A regular grid
    of {it:#} points between 1/(#+1) and #/(#+1) will be used. The default is
    {cmd:n(99)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of probabilities at which to
    compute quantiles. The specified values must be within [0,1]. Only one of
    {cmd:n()} and {cmd:at()} is allowed.

{marker lorenz}{...}
{dlgtab:Subcommand lorenz}

{phang}
    {opt percent} expresses results in percent instead of
    proportions. {cmd:percent} is not allowed with
    {cmd:generalized} or {cmd:absolute}.

{phang}
    {opt generalized} estimates the generalized Lorenz curve.

{phang}
    {opt gap} estimates the equality gap curve.

{phang}
    {opt absolute} estimates the absolute Lorenz curve.

{phang}
    {opth zvar(varname)} estimates the concentration curve with respect to
    {it:varname} instead of the Lorenz curve.

{phang}
    {opt n(#)} sets the number of ordinates to be estimated. A regular grid
    of {it:#} values between 0 and 1 will be used. The default is
    {cmd:n(101)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of points at which to
    estimate Lorenz ordinates. The specified values must be within [0,1]. Only one of
    {cmd:n()} and {cmd:at()} is allowed.

{marker share}{...}
{dlgtab:Subcommand share}

{phang}
    {opt proportion} estimates proportions instead of densities.

{phang}
    {opt percent} estimates percent instead of densities.

{phang}
    {opt generalized} estimates generalized shares instead of densities.

{phang}
    {opt average} estimates averages instead of densities.

{phang}
    {opth zvar(varname)} estimates the concentration shares with respect to
    {it:varname}.

{phang}
    {opt n(#)} sets the number of bins. A regular grid of {it:#} bins between
    0 an 1 will be used. The default is {cmd:n(20)}.

{phang}
    {opth at(numlist)} specifies custom cutpoints for the bins (in ascending
    order). The specified values must be within [0,1]. If {it:numlist} contains
    {it:n} numbers, {it:n}-1 bins will be created. Note that the constructed
    bins will cover all data only if the first cutpoint is 0 and the last
    cutpoint is 1.

{marker graph_options}{...}
{dlgtab:Graph options}

{phang}
    {cmd:flip} changes how results allocated to plots and subgraphs. This is
    only relevant if the results contain multiple equations. The behavior
    depends on whether equations represent one or two dimensions.

{phang2}
    {space 2}o One equation dimension (subpopulations or variables): By default, one subgraph is
    created for each equation. Specify option {cmd:flip} to include all results
    in a single graph without creating subgraphs. Equations will be rendered as
    separate "plots" (series of results displayed in a common style) in this
    case.

{phang2}
    {space 2}o Two equation dimensions: By default, subgraphs correspond to
    the secondary dimension (typically variables) and plots within subgraphs
    correspond to the main dimension (typically subpopulations). Specify
    {cmd:flip} to reverse this behavior.

{phang}
    {cmd:bystats}[{cmd:(}{it:arg}{cmd:)}] treats coefficients as equations and
    equations as coefficients. This is only relevant after
    {cmd:dstat summarize} and only has an effect if the results contain multiple
    equations. The effect of {cmd:bystats} typically is that results are grouped
    by statistics rather than by subpopulations or variables (the option may
    also have the opposite effect depending on how exactly {cmd:dstat} returned its
    results). Optional {it:arg} can be {cmdab:m:ain} (the default) or
    {cmdab:s:econdary} to specify wether coefficients should replace the
    main dimension or the secondary dimension of the equations, respectively. This
    is only relevant if the equations contain two dimensions
    (subpopulations and variables).

{phang}
    [{cmd:no}]{cmd:step} enforces or prevents using a step function to display
    the distribution function. This is only relevant after {cmd:dstat cdf}. The
    default is to display the CDF as a step function if option {cmd:discrete}
    has been specified, and else use straight lines. Specify
    {cmd:nostep} or {cmd:step}, respectively, to override the default.

{phang}
    {cmd:norefline} suppresses the equality line (diagonal) that is printed
    when plotting results from {cmd:dstat lorenz} (unless option
    {cmd:generalized}, {cmd:gap}, or {cmd:absolute} has been specified).

{phang}
    {opt refline(line_options)} specifies options to affect the rendition of
    the equality line; see {it:{help line_options}}. This is only relevant after
    {cmd:dstat lorenz}.

{phang}
    {it:coefplot_options} are options to be passed through to
    {helpb coefplot}. Use these options, for example, to set titles and axis
    labels or to affect the overall look and size of the graph. The options can
    also be used to change the rendering of the plotted results (e.g. colors,
    line patterns, marker symbols, etc.). If a graph contains multiple "plots"
    (multiple series of results displayed in a common style), option
    {cmd:p}{it:#}{cmd:()} can be used to address the {it:#}th plot.

{marker predict_options}{...}
{dlgtab:Predict options}

{phang}
    {opt rif} generates recentered influence functions (RIFs) instead of regular
    influence functions. RIFs are defined such that their mean is equal to the
    statistic in question (Firpo et al. 2009; also see Rios-Avila 2020)
    and the standard error of the mean (as computed by
    command {helpb mean}) provides an estimate of the standard error of the
    statistic. The default is to store influence functions defined in a way such
    that their total is zero and the standard error of the total (as computed by
    command {helpb total}) provides an estimate of the standard error of the
    statistic.

{phang}
    {opt com:pact} generates influence functions in compact form. {cmd:compact}
    only has an effect if {cmd:over()} has been specified and is not allowed
    if {cmd:balance()} has been specified.

{pmore}
    The default is to generate one influence function for each single parameter
    estimated by {cmd:dstat}. If {cmd:over()} is specified, this means that
    each statistic in each subpopulation has its own influence
    function. Specify {cmd:compact} to merge the influence functions across
    subpopulations. In this case, {cmd:over()} has to be specified when
    analyzing the influence functions.

{phang}
    {opt qui:etly} suppresses the list of generated variables that is displayed by
    default.

{pstd}
    Note that weights, if specified, will not be incorporated into the
    influence functions, so that the weights can be
    applied when analyzing the influence functions. The influence functions do,
    however, incorporate the balancing weights (net of base weights)
    from option {cmd:balance()}.


{marker examples}{...}
{title:Examples}

{dlgtab:Summary statistics}

{pstd}
    {cmd:dstat summarize} supports a long list of summary statistic. For example, the following
    command computes the arithmetic mean, geometric mean, median, 5% trimmed mean, 5%
    winsorized mean, 95%-efficiency Huber M estimate, and Hodges-Lehmann location of wages
    for unionized and nonunionized workers:

        . {stata sysuse nlsw88, clear}
{p 8 12 2}
        . {stata dstat (mean gmean median trim5 winsor5 huber95 hl) wage, over(union)}
        {p_end}

{pstd}
    Results can be computed for multiple variables, and statistics may
    differ across variables. The following command estimates the
    Gini coefficient, mean log deviation, and variance of logarithms of wages,
    the means of working hours and work experience, as well as the proportion of whites
    ({cmd:race}=1), blacks ({cmd:race}=2), and others ({cmd:race}=3):

{p 8 12 2}
        . {stata dstat (gini mld vlog) wage (mean) hours ttl_exp (pr1 pr2 pr3) race}
        {p_end}

{dlgtab:Distribution functions}

{pstd}
    {cmd:dstat} supports the estimation of several types of distribution
    functions. For example, the density function of wages by union status can
    be obtained as follows:

        . {stata sysuse nlsw88, clear}
        . {stata dstat density wage, over(union) ll(0) graph}

{pstd}
    Option {cmd:graph} has been specified so that a graph is drawn. The coefficients
    table will be suppressed in this case; specify option {cmd:table} to enforce displaying the
    coefficients table. An alternative would be to
    omit the {cmd:graph} option and then type {cmd:dstat graph} after estimation.
    Option {cmd:ll(0)} has been specified because wages can only be positive. The option
    causes density estimation to be restricted to the positive domain
    and applies appropriate boundary correction.

{pstd}
    In the example above, the density estimates for unionized and nonunionized
    workers have been displayed in two separate subgraphs. Apply graph option
    {cmd:flip} to overlay the two curves in a single coordinate system:

        . {stata dstat graph, flip}

{dlgtab:Covariate balancing}

{pstd}
    The {cmd:balance()} option can be used to adjust results for differences in
    covariate distributions when comparing subpopulations. By default,
    {cmd:dstat} employs inverse probability weighting (IPW) to balance the
    covariates and obtains the relevant reference distribution from the total
    sample. That is, in each subpopulation the covariate distribution is
    adjusted such that it resembles the covariate distribution observed in the
    total population. Use the {cmd:reference()} suboption to change the reference
    distribution.

{pstd}
    For example, the mean difference of average wages between nonunionized and
    unionized workers is as follows:

        . {stata sysuse nlsw88, clear}
        . {stata dstat (mean) wage, over(union)}
        . {stata lincom _b[1.union]-_b[0.union]}

{pstd}
    Controlling for education, working hours, work experience and tenure reduces
    the mean difference by about a third:

{p 8 12 2}
        . {stata dstat (mean) wage, over(union) balance(grade hours ttl_exp tenure)}
        {p_end}
        . {stata lincom _b[1.union]-_b[0.union]}

{pstd}
    (Note that there has been a small change in the estimation sample due to missing values; for a
    more valid comparison, the raw difference should be computed based on the same
    sample as the balanced difference.)

{pstd}
    To evaluate how successful the balancing was, you can use suboption {cmd:generate()}
    to store the balancing weights:

{p 8 12 2}
        . {stata dstat (mean) wage, over(union) balance(grade hours ttl_exp tenure, generate(wbal))}
        {p_end}
{p 8 12 2}
        . {stata tabstat grade hours ttl_exp tenure if wage<., by(union)} (unbalanced)
        {p_end}
{p 8 12 2}
        . {stata tabstat grade hours ttl_exp tenure [aw=wbal], by(union)} (balanced)
        {p_end}

{pstd}
    The balancing has only been partially successful. Perfect balancing
    (with respect to the means) can be achieved by entropy balancing:

        . {stata drop wbal}
{p 8 12 2}
        . {stata "dstat (mean) wage, over(union) balance(eb: grade hours ttl_exp tenure, generate(wbal))"}
        {p_end}
{p 8 12 2}
        . {stata tabstat grade hours ttl_exp tenure [aw=wbal], by(union)}
        {p_end}

{dlgtab:Influence functions}

{pstd}
    {cmd:dstat} can store the influence functions or the recentered
    influence functions (RIFs) of the computed statistics. The influence functions
    or RIFs can then be used in further analyses. Here is an example of
    RIF regressions (Firpo et al. 2009) for the Gini coefficient and the
    mean log deviation:

        . {stata sysuse nlsw88, clear}
        . {stata dstat (gini mld) wage, rif(gini mld)}
        . {stata regress gini union south smsa, robust}
        . {stata regress mld union south smsa, robust}

{pstd}
    The RIFs are also useful for decomposition analysis. In the following example
    the wage gap between unionized and non-unionized workers is decomposed into
    a part explained by differences in covariates and a residual (unexplained) part, using
    reweighting based on entropy balancing and using the covariate distribution
    of unionized workers as the reference distribution:

{p 8 12 2}
        . {stata "dstat (mean) wage if e(sample), over(union) balance(eb: grade hours ttl_exp tenure, reference(1)) rif(RIF0c)"}
        {p_end}
{p 8 12 2}
        . {stata "dstat (mean) wage if e(sample), over(union) rif(RIF0 RIF1)"}
        {p_end}
        . {stata generate difference  = RIF1  - RIF0}
        . {stata generate explained   = RIF0c - RIF0}
        . {stata generate unexplained = RIF1  - RIF0c}
        . {stata mean difference explained unexplained}


{marker methods}{...}
{title:Methods and formulas}

{pstd}
    (under construction)


{marker saved_results}{...}
{title:Saved results}

{pstd}
    Depending on options, {cmd:dstat} stores a selection of the following
    results in {cmd:e()}.

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(W)}}sum of weights{p_end}
{synopt:{cmd:e(N_over)}}number subpopulations{p_end}
{synopt:{cmd:e(N_clust)}}number of clusters{p_end}
{synopt:{cmd:e(N_vars)}}number of variables{p_end}
{synopt:{cmd:e(N_stats)}}number of (unique) summary statistics{p_end}
{synopt:{cmd:e(k_eq)}}number of equations in {cmd:e(b)}{p_end}
{synopt:{cmd:e(k_omit)}}number of omitted coefficients in {cmd:e(b)}{p_end}
{synopt:{cmd:e(df_r)}}sample degrees of freedom{p_end}
{synopt:{cmd:e(qdef)}}quantile definition{p_end}
{synopt:{cmd:e(adaptive)}}number of iterations of adaptive density estimator{p_end}
{synopt:{cmd:e(napprox)}}size of density estimation grid{p_end}
{synopt:{cmd:e(pad)}}padding of density estimation grid{p_end}
{synopt:{cmd:e(ll)}}lower boundary of the data support (density estimation){p_end}
{synopt:{cmd:e(ul)}}upper boundary of the data support (density estimation){p_end}
{synopt:{cmd:e(level)}}confidence level{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:dstat}{p_end}
{synopt:{cmd:e(subcmd)}}{cmd:summarize}, {cmd:density}, {cmd:histogram}, {cmd:proportion}, {cmd:cdf}, {cmd:quantile}, {cmd:lorenz}, or {cmd:share}{p_end}
{synopt:{cmd:e(predict)}}{cmd:dstat predict}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name(s) of analyzed variable(s){p_end}
{synopt:{cmd:e(over)}}name of {it:overvar}{p_end}
{synopt:{cmd:e(over_namelist)}}values of subpopulations{p_end}
{synopt:{cmd:e(over_labels)}}labels of subpopulations{p_end}
{synopt:{cmd:e(total)}}{cmd:total} or empty{p_end}
{synopt:{cmd:e(balance)}}list of balancing variables{p_end}
{synopt:{cmd:e(balmethod)}}balancing method{p_end}
{synopt:{cmd:e(balref)}}balancing reference{p_end}
{synopt:{cmd:e(balopts)}}options passed through to balancing procedure{p_end}
{synopt:{cmd:e(bwmethod)}}bandwidth selection as specified in {cmd:bwidth()}{p_end}
{synopt:{cmd:e(kernel)}}kernel as specified in {cmd:kernel()}{p_end}
{synopt:{cmd:e(exact)}}{cmd:exact} or empty{p_end}
{synopt:{cmd:e(boundary)}}boundary correction method{p_end}
{synopt:{cmd:e(novalues)}}{cmd:novalues} or empty{p_end}
{synopt:{cmd:e(vformat)}}display format specified in {cmd:vformat()}{p_end}
{synopt:{cmd:e(stats)}}list of (unique) summary statistics{p_end}
{synopt:{cmd:e(slist)}}normalized specification of statistics and variables{p_end}
{synopt:{cmd:e(percent)}}{cmd:percent} or empty{p_end}
{synopt:{cmd:e(proportion)}}{cmd:proportion} or empty{p_end}
{synopt:{cmd:e(frequency)}}{cmd:frequency} or empty{p_end}
{synopt:{cmd:e(mid)}}{cmd:mid} or empty{p_end}
{synopt:{cmd:e(discrete)}}{cmd:discrete} or empty{p_end}
{synopt:{cmd:e(categorical)}}{cmd:categorical} or empty{p_end}
{synopt:{cmd:e(ep)}}{cmd:ep} or empty{p_end}
{synopt:{cmd:e(gap)}}{cmd:gap} or empty{p_end}
{synopt:{cmd:e(generalized)}}{cmd:generalized} or empty{p_end}
{synopt:{cmd:e(absolute)}}{cmd:absolute} or empty{p_end}
{synopt:{cmd:e(average)}}{cmd:average} or empty{p_end}
{synopt:{cmd:e(zvar)}}name of sort variable specified in {cmd:zvar()}{p_end}
{synopt:{cmd:e(pline)}}poverty line variable specified in {cmd:pline()}{p_end}
{synopt:{cmd:e(generate)}}name(s) of generated variable(s){p_end}
{synopt:{cmd:e(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:e(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err.{p_end}
{synopt:{cmd:e(citype)}}type confidence interval stored in {cmd:e(ci)}{p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(properties)}}{cmd:b} or {cmd:b V}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}estimates{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of estimates{p_end}
{synopt:{cmd:e(se)}}standard errors of estimates{p_end}
{synopt:{cmd:e(ci)}}confidence intervals of estimates{p_end}
{synopt:{cmd:e(at)}}evaluation points of distribution function{p_end}
{synopt:{cmd:e(id)}}subpopulation IDs of estimates{p_end}
{synopt:{cmd:e(bwidth)}}kernel bandwidth(s) of density estimation{p_end}
{synopt:{cmd:e(_N)}}number of observations by subpopulation{p_end}
{synopt:{cmd:e(_W)}}sum of weights by subpopulation{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}estimation sample{p_end}
{p2colreset}{...}

{pstd}
    If {cmd:vce()} is {cmd:svy}, {cmd:bootstrap}, or {cmd:jackknife}, additional
    results are stored in {cmd:e()}; see {helpb svy}, {helpb bootstrap}, and
    {helpb jackknife}, respectively.


{marker references}{...}
{title:References}

{phang}
    Botev, Z.I., J.F. Grotowski, and D.P. Kroese (2010). Kernel density
    estimation via diffusion. Annals of Statistics
    38(5): 2916-2957. DOI: {browse "http://doi.org/10.1214/10-AOS799":10.1214/10-AOS799}.
    {p_end}
{phang}
    Brys, G., M. Hubert, A. Struyf (2004). A Robust Measure of Skewness.
    Journal of Computational and Graphical Statistics 13(4): 996-1017.
    {p_end}
{phang}
    Brys, G., M. Hubert, A. Struyf (2006). Robust measures of tail weight.
    Computational Statistics & Data Analysis 50: 733-759.
    {p_end}
{phang}
    Cwik, J., J. Mielniczuk (1993). Data-dependent bandwidth choice for a grade density
    kernel estimate. Statistics & Probability Letters 16: 397-405.
    {p_end}
{phang}
    Deville, Jean-Claude (1999). Variance estimation for complex statistics and
    estimators: Linearization and residual techniques. Survey Methodology 25: 193-203.
    {p_end}
{phang}
    DiNardo, J.E., N. Fortin, T. Lemieux (1996). Labour Market Institutions and
    the Distribution of Wages, 1973-1992: A Semiparametric Approach. Econometrica
    64(5): 1001-1046.
    {p_end}
{phang}
    Firpo, S., N.M. Fortin, T. Lemieux (2009). Unconditional Quantile
    Regressions. Econometrica 77: 953-973.
    {p_end}
{phang}
    Foster, J., J. Greer, E. Thorbecke (1984). A class of decomposable poverty
    measures. Econometrica 52(3): 761-766.
    {p_end}
{phang}
    Foster, J., J. Greer, E. Thorbecke (2010). The Foster–Greer–Thorbecke (FGT) poverty measures: 25 years
    later. The Journal of Economic Inequality 8: 491–524.
    {p_end}
{phang}
    Hainmueller, J. (2012). Entropy Balancing for Causal Effects: A Multivariate
    Reweighting Method to Produce Balanced Samples in Observational Studies.
    Political Analysis 20: 25-46.
    {p_end}
{phang}
    Hampel, F.R. (1974). The Influence Curve and Its Role in Robust
    Estimation. Journal of the American Statistical Association 69: 383-393.
    {p_end}
{phang}
    Hinkley, D. V. (1975). On power transformations to symmetry. Biometrika
    62(1): 101-111.
    {p_end}
{phang}
    Hodges, Jr., J.L., E.L. Lehmann (1963). Estimates of location based on
    rank tests. Annals of Mathematical Statistics 34(2): 598-611.
    {p_end}
{phang}
    Hyndman, R.J., Fan, Y. (1996). Sample Quantiles in Statistical
    Packages. The American Statistician 50: 361-365.
    {p_end}
{phang}
    Jann, B. (2007). Univariate kernel density
    estimation. DOI: {browse "http://boris.unibe.ch/69421/2/kdens.pdf":10.7892/boris.69421}.
    {p_end}
{phang}
    Jann, B. (2020). Influence functions continued. A framework for estimating standard errors in
    reweighting, matching, and regression adjustment. University of Bern Social Sciences
    Working Papers 35. Available from
    {browse "http://ideas.repec.org/p/bss/wpaper/35.html"}.
    {p_end}
{phang}
    Rios-Avila, F. (2020). Recentered influence functions (RIFs) in Stata: RIF
    regression and RIF decomposition. The Stata Journal 20(1): 51-94.
    {p_end}
{phang}
    Rousseeuw, P.J., C. Croux (1993). Alternatives to the Median
    Absolute Deviation. Journal of the American Statistical Association
    88(424): 1273-1283.
    {p_end}
{phang}
    Saisana M. (2014). Watts Poverty Index. In: A.C. Michalos (ed). Encyclopedia of Quality of Life and Well-Being
    Research. Dordrecht: Springer. DOI: {browse "http://doi.org/10.1007/978-94-007-0753-5_3197":10.1007/978-94-007-0753-5_3197}
    {p_end}
{phang}
    Shorrocks, A.F. (1980). The Class of Additively Decomposable Inequality Measures. Econometrica 48(3): 613-625.
    {p_end}
{phang}
    Wand, M.P., M.C. Jones (1995). Kernel Smoothing. London: Chapman and Hall.
    {p_end}


{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@soz.unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2020). dstat: Stata module to compute summary statistics and
    distribution functions including standard errors
    and optional covariate balancing. Available from
    {browse "http://github.com/benjann/dstat/"}.


{marker also_see}{...}
{title:Also see}

{psee}
    Online: help for {helpb mean}, {helpb proportion}, {helpb total}, {helpb ci},
    {helpb summarize}, {helpb tabstat}, {helpb centile}, {helpb pctile}, {helpb cumul}, {helpb kdensity},
    {helpb table}, {helpb histogram}, {helpb teffects ipw}

{psee}
    Commands from the SSC Archive (type {cmd:ssc describe} {it:name} for
    more information): {helpb rif}, {helpb kdens}, {helpb kmatch}, {helpb lorenz},
    {helpb pshare}, {helpb glcurve}, {helpb svylorenz}, {helpb robstat},
    {helpb fre}, {helpb catplot}, {helpb cdfplot}, {helpb distplot},
    {helpb reldist}, {helpb moremata}

