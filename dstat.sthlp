{smcl}
{* 08feb2026}{...}
{vieweralsosee "twoway dstat" "help twoway_dstat"}{...}
{viewerjumpto "Syntax" "dstat##syntax"}{...}
{viewerjumpto "Description" "dstat##description"}{...}
{viewerjumpto "Summary statistics" "dstat##stats"}{...}
{viewerjumpto "Options" "dstat##options"}{...}
{viewerjumpto "Examples" "dstat##examples"}{...}
{viewerjumpto "Saved results" "dstat##saved_results"}{...}
{viewerjumpto "References" "dstat##references"}{...}
{viewerjumpto "Author" "dstat##author"}{...}
{viewerjumpto "Also see" "dstat##also_see"}{...}
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
    Pairwise associations

{p 12 17 2}
{cmd:dstat} {cmdab:pw} [{cmd:(}{it:stat}{cmd:)}] {varlist} {ifin} {weight} [{cmd:,}
    {help dstat##opts:{it:options}} ]

{pmore2}
    where {it:stat} selects the association measure; see
    {help dstat##association:Association measures} and
    {help dstat##catbivar:Categorical data (bivariate)} for available measures
    (omitting argument {it:by}); default is {cmd:correlation}.

{pmore}
    Distribution functions

{p 12 17 2}
{cmd:dstat} {it:subcmd} {varlist} {ifin} {weight} [{cmd:,}  {help dstat##opts:{it:options}} ]

{pmore2}
    where {it:subcmd} is one of

{p2colset 15 28 30 2}{...}
{p2col:{opt d:ensity}}density function{p_end}
{p2col:{opt pdf}}same as {cmd:density}{p_end}
{p2col:{opt h:istogram}}histogram{p_end}
{p2col:{opt p:roportion}}probability distribution{p_end}
{p2col:{opt freq:uency}}same as {cmd:proportion} with option {cmd:frequency}{p_end}
{p2col:{opt c:df}}cumulative distribution function{p_end}
{p2col:{opt cc:df}}complementary CDF/survival function{p_end}
{p2col:{opt q:uantile}}quantile function{p_end}
{p2col:{opt l:orenz}}lorenz curve{p_end}
{p2col:{opt psh:are}}percentile shares{p_end}
{p2col:{opt tip}}TIP curve{p_end}

{pmore}
    {it:varlist} may contain factor variables (in most cases; an exception is {cmd:dstat pw}); see {help fvvarlist}.
    {p_end}
{pmore}
    {cmd:fweight}s, {cmd:pweight}s, and {cmd:iweight}s are allowed; see {help weight}.
    {p_end}
{pmore}
    The distribution functions are also available in {cmd:graph twoway}; see {helpb twoway dstat}.

{pstd}
    Postestimation

{pmore}
    Replay results

{p 12 17 2}
{cmd:dstat} [{cmd:,} {help dstat##repopts:{it:reporting_options}} ]

{pmore}
    Draw graph of results

{p 12 17 2}
{cmd:dstat} {cmdab:gr:aph}
    [{cmd:,} {help dstat##graph_opts:{it:graph_options}} ]

{pmore}
    Save results in data

{p 12 17 2}
{cmd:dstat} {cmd:save} {it:{help dstat##save_options:elements}} [{cmd:,} {help dstat##save_options:{it:save_options}} ]

{pmore}
    Generate (recentered) influence functions

{p 12 17 2}
    {cmd:predict} {c -(}{help newvarlist##stub*:{it:stub}}{cmd:*} |
        {it:{help newvar:newvar1}} {it:{help newvar:newvar2}} {cmd:...}{c )-} {ifin}
        [{cmd:,} {it:{help dstat##predict_opts:predict_options}} ]

{pmore}
    Include distribution estimates from {cmd:dstat} in two-way graph

{p 12 17 2}
    [{cmdab:gr:aph}] {helpb twoway_dstat:{ul:tw}oway dstat} {it:...}


{synoptset 26 tabbed}{...}
{marker opts}{col 5}{help dstat##options:{it:options}}{col 33}Description
{synoptline}
{syntab:{help dstat##mainopts:Main}}
{synopt:{opt nocase:wise}}do not perform casewise deletion of observations
    {p_end}
{synopt:{cmdab:o:ver(}{help dstat##over:{it:spec}}{cmd:)}}compute
    results for subpopulations; not allowed with {cmd:dstat pw}
    {p_end}
{synopt:{cmdab:tot:al}[{cmd:(}{help dstat##total:{it:totaltype}}{cmd:)}]}include
    results for total population
    {p_end}
{synopt:{cmdab:bal:ance(}{help dstat##balance:{it:spec}}{cmd:)}}balance
    covariates using reweighting; requires {cmd:over()}
    {p_end}
{synopt:{opt gr:aph}[{cmd:(}{help dstat##graph_options:{it:graph_options}}{cmd:)}]}displays
    results in a graph using {helpb coefplot}
    {p_end}
{synopt:{help dstat##repopts:{it:reporting_options}}}reporting options
    {p_end}
{synopt:{cmd:order(}{help dstat##order:{it:keywords}}{cmd:)}}set order in which
    results are arranged; not allowed with {cmd:dstat pw}
    {p_end}
{synopt:{opt noval:ues}}do not use values as coefficient names
    {p_end}
{synopt:{opth vf:ormat(fmt)}}format for coefficient name values
    {p_end}

{syntab:{help dstat##vce:SE/VCE}}
{synopt:{cmd:vce(}{it:{help dstat##vce:vcetype}}{cmd:)}}variance estimation method;
    {it:vcetype} may be {cmd:none} (skip variance estimation),
    {cmdab:a:nalytic}, {cmdab:cl:uster} {it:clustvar}, {cmdab:svy}, {cmdab:boot:strap},
    or {cmdab:jack:knife}
    {p_end}
{synopt:{cmd:nose}}alias for {cmd:vce(none)}
    {p_end}
{synopt:[{cmd:no}]{cmd:cov}}whether to store the full variance matrix or only the
    standard errors
    {p_end}
{synopt:{opt nobwfix:ed}}do not keep density bandwidth fixed across replications
    {p_end}
{synopt:{cmdab:g:enerate(}{it:names}[{cmd:,} {it:opts}]{cmd:)}}store influence functions
    {p_end}
{synopt:{cmd:rif(}{it:names}[{cmd:,} {it:opts}]{cmd:)}}store recentered influence functions
    {p_end}
{synopt:{opt r:eplace}}allow replacing existing variables
    {p_end}

{syntab:{help dstat##sum:Subcommand {bf:summarize}}}
{synopt:{cmd:at(}{it:{help varname:atvar}} [{cmd:=} {it:{help numlist}}]{cmd:)}}compute
    conditional statistics by levels of {it:atvar}
    {p_end}
{synopt:{opt range(a b)}}set range of included levels from {it:atvar}
    {p_end}
{synopt:{opt cat:egorical}}treat {it:atvar} as categorical
    {p_end}
{synopt:{opt relax}}compute a statistic even if observations are out of support
    {p_end}
{synopt:{opth by(varname)}}default secondary variable (for association and concentration measures)
    {p_end}
{synopt:{opt pl:ine(#|varname)}}default poverty line
    {p_end}
{synopt:{opt pstr:ong}}use "strong" poverty definition
    {p_end}
{synopt:{cmd:qdef(}{help dstat##qdef:{it:method}}{cmd:)}}quantile estimation method
    {p_end}
{synopt:{it:{help dstat##densopts:density_options}}}details of density estimation
    {p_end}

{syntab:{help dstat##pw:Subcommand {bf:pw}}}
{synopt:{opt lo:wer}}lower-triangle elements only
    {p_end}
{synopt:{opt up:per}}upper-triangle elements only
    {p_end}
{synopt:{opt diag:onal}}include diagonal elements
    {p_end}

{syntab:{help dstat##density:Subcommand {bf:density}}}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt:{opt com:mon}}use common evaluation points across subpopulations
    {p_end}
{synopt:[{cmd:l}|{cmd:r}]{cmd:tight}}use tight evaluation grid
    {p_end}
{synopt:{opt range(a b)}}set range of evaluation grid
    {p_end}
{synopt:{opth at(numlist)}}use custom evaluation grid
    {p_end}
{synopt:{cmdab:unc:onditional}}rescale results by relative size of subpopulation
    {p_end}
{synopt:{it:{help dstat##densopts:density_options}}}details of density estimation
    {p_end}

{syntab:{help dstat##hist:Subcommand {bf:histogram}}}
{synopt:{opt prop:ortion}}estimate proportions instead of densities
    {p_end}
{synopt:{opt per:cent}}estimate percentages instead of densities
    {p_end}
{synopt:{opt freq:uency}}estimate frequencies instead of densities
    {p_end}
{synopt:{cmd:n(}{cmd:#}|{it:{help dstat##hist:method}}{cmd:)}}number of
    histogram bins; default is {cmd:n(sqrt)}
    {p_end}
{synopt:{cmd:ep}}use equal probability bins instead of equal width bins
    {p_end}
{synopt:{opt com:mon}}use common bin definitions across subpopulations
    {p_end}
{synopt:{opt range(a b)}}set overall range of histogram bins
    {p_end}
{synopt:{opth at(numlist)}}use custom histogram bins
    {p_end}
{synopt:{opt disc:rete}}treat data as discrete (calls {cmd:dstat proportion})
    {p_end}
{synopt:{cmdab:unc:onditional}}rescale results by relative size of subpopulation
    {p_end}

{syntab:{help dstat##prop:Subcommand {bf:proportion}}}
{synopt:{opt per:cent}}estimate percentages instead of proportions
    {p_end}
{synopt:{opt freq:uency}}estimate frequencies instead of proportions
    {p_end}
{synopt:{opt range(a b)}}set range of included levels
    {p_end}
{synopt:{opth at(numlist)}}use custom list of levels
    {p_end}
{synopt:{opt nocat:egorical}}allow variables that do not comply to Stata's rules
    for factor variables
    {p_end}
{synopt:{cmdab:unc:onditional}}rescale results by relative size of subpopulation
    {p_end}

{syntab:{help dstat##cdf:Subcommands {bf:cdf} and {bf:ccdf}}}
{synopt:{opt per:cent}}estimate percentages instead of proportions
    {p_end}
{synopt:{opt freq:uency}}estimate frequencies instead of proportions
    {p_end}
{synopt:{opt mid}}apply midpoint adjustment
    {p_end}
{synopt:{opt fl:oor}}use "lower-than" definition
    {p_end}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt:{opt com:mon}}use common evaluation points across subpopulations
    {p_end}
{synopt:{opt range(a b)}}set range of evaluation grid
    {p_end}
{synopt:{opth at(numlist)}}use custom evaluation grid
    {p_end}
{synopt:{opt disc:rete}}treat data as discrete
    {p_end}
{synopt:{opt ip:olate}}obtain CDF by linear interpolation
    {p_end}
{synopt:{cmdab:unc:onditional}}rescale results by relative size of subpopulation
    {p_end}

{syntab:{help dstat##quantile:Subcommand {bf:quantile}}}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(99)}
    {p_end}
{synopt:{opt range(a b)}}set range of evaluation grid; {it:a} and {it:b} in [0,1]
    {p_end}
{synopt:{opth at(numlist)}}use custom evaluation grid
    {p_end}
{synopt:{cmd:qdef(}{help dstat##qdef:{it:method}}{cmd:)}}quantile estimation method
    {p_end}
{synopt:{it:{help dstat##densopts:density_options}}}details of density estimation
    {p_end}

{syntab:{help dstat##lorenz:Subcommand {bf:lorenz}}}
{synopt:{opt per:cent}}report percentages instead of proportions
    {p_end}
{synopt:{opt general:ized}}estimate generalized Lorenz curve
    {p_end}
{synopt:{opt sum}}estimate total (unnormalized) Lorenz curve
    {p_end}
{synopt:{opt gap}}estimate equality gap curve
    {p_end}
{synopt:{opt abs:olute}}estimate absolute Lorenz curve
    {p_end}
{synopt:{opth by(varname)}}estimate concentration curve with respect to specified variable
    {p_end}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(101)}
    {p_end}
{synopt:{opt range(a b)}}set range of evaluation grid; {it:a} and {it:b} in [0,1]
    {p_end}
{synopt:{opth at(numlist)}}use custom evaluation grid
    {p_end}

{syntab:{help dstat##pshare:Subcommand {bf:pshare}}}
{synopt:{opt prop:ortion}}estimate proportions instead of densities
    {p_end}
{synopt:{opt per:cent}}estimate percentages instead of densities
    {p_end}
{synopt:{opt general:ized}}estimate generalized shares instead of densities
    {p_end}
{synopt:{opt sum}}estimate totals instead of density
    {p_end}
{synopt:{opt ave:rage}}estimate averages instead of densities
    {p_end}
{synopt:{opth by(varname)}}estimate concentration shares with respect to specified variable
    {p_end}
{synopt:{opt n(#)}}number of bins; default is {cmd:n(20)}
    {p_end}
{synopt:{opt range(a b)}}set overall range of bins; {it:a} and {it:b} in [0,1]
    {p_end}
{synopt:{opth at(numlist)}}use custom bins
    {p_end}

{syntab:{help dstat##tip:Subcommand {bf:tip}}}
{synopt:{opt pl:ine(#|varname)}}poverty line (required)
    {p_end}
{synopt:{opt abs:olute}}estimate absolute TIP curve
    {p_end}
{synopt:{opt pstr:ong}}use "strong" poverty definition
    {p_end}
{synopt:{opt n(#)}}size of evaluation grid; default is {cmd:n(101)}
    {p_end}
{synopt:{opt range(a b)}}set range of evaluation grid; {it:a} and {it:b} in [0,1]
    {p_end}
{synopt:{opth at(numlist)}}use custom evaluation grid
    {p_end}
{synoptline}
{pstd}

{marker graph_opts}{col 5}{help dstat##graph_options:{it:graph_options}}{col 33}Description
{synoptline}
{syntab:Main}
{synopt:{cmdab:mer:ge}}merge results into a single subgraph
    {p_end}
{synopt:{cmdab:overl:ay}}synonym for {cmd:merge}
    {p_end}
{synopt:{cmd:flip}}change how results are allocated to plots and subgraphs
    {p_end}
{synopt:{help dstat##coefplot:{it:coefplot_options}}}options to be passed through to {helpb coefplot}
    {p_end}

{syntab:{help dstat##stack:Stacked bar charts}}
{synopt:{opt stack}}draw stacked bar chart; only allowed with {cmd:dstat proportion}
    {p_end}
{synopt:{opt div:erging}}align bars at midpoint (diverging stacked bar chart)
    {p_end}
{synopt:{opt barw:idth(#)}}set the width of the bars; default is {cmd:0.8}
    {p_end}
{synopt:{it:{help barlook_options}}}change overall look of the bars
    {p_end}
{synopt:{cmdab:col:ors(}{help dstat##colors:{it:palette}}{cmd:)}}assign colors
    to the bar segments; requires command {helpb colorpalette}
    {p_end}
{synopt:{cmd:p}{it:#}{cmd:(}{help dstat##pnum:{it:options}}{cmd:)}}change settings of {it:#}th bar segment
    {p_end}
{synopt:{opt val:ues}[{cmd:(}{it:min}{cmd:)}]}print the values of the bar segments as marker labels
    {p_end}

{syntab:{help dstat##notstack:Other graphs}}
{synopt:[{ul:{cmd:g}}|{ul:{cmd:p}}]{opt sel:ect(spec)}}select and order plots and subgraphs
    {p_end}
{synopt:{opt cref}}include results from the the reference (sub)population
    {p_end}
{synopt:{cmdab:bys:tats}[{cmd:(}{it:arg}{cmd:)}]}group results by statistics; only relevant for {cmd:dstat summarize}
    {p_end}
{synopt:[{cmd:no}]{cmd:step}}do/do not use step function; only relevant for {cmd:dstat cdf}
    {p_end}
{synopt:{cmdab:noref:line}}suppress equality line; only relevant for {cmd:dstat lorenz}
    {p_end}
{synopt:{opth ref:line(line_options)}}affect rendition of equality line; only relevant for {cmd:dstat lorenz}
    {p_end}
{synoptline}

{marker predict_opts}{col 5}{help dstat##predict_options:{it:predict_options}}{col 33}Description
{synoptline}
{synopt:{opt rif}}store recentered influence functions
    {p_end}
{synopt:{cmdab:sca:ling(}{cmdab:t:otal}|{cmdab:m:ean}{cmd:)}}set the scaling of the influence functions
    {p_end}
{synopt:{opt com:pact}}store influence functions in compact form; not allowed with {cmd:balance()} or {cmd:unconditional}
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
    Basic functionality for graphing results using {helpb coefplot} is provided through the
    {helpb dstat##gropt:graph()} option or by applying command {cmd:dstat graph}
    after estimation. Further graphing functionality is provided by
    {helpb twoway dstat}.

{pstd}
    {cmd:dstat} requires the {helpb moremata} package to be installed on the system; see
    {net "describe moremata, from(http://fmwww.bc.edu/repec/bocode/m/)":{bf:ssc describe moremata}}. Furthermore,
    {helpb coefplot} needs to be installed if you want to use
    {cmd:dstat graph} or the {cmd:graph()} option; see
    {net "describe coefplot, from(http://fmwww.bc.edu/repec/bocode/c/)":{bf:ssc describe coefplot}}.


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
    for {cmd:density}, and {cmd:f} for {cmd:frequency}). For example, to obtain the
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

{synoptset 27 tabbed}{...}
{marker stats}{col 5}{it:stats}{col 33}Description
{synoptline}
{syntab:Points in the distribution}
{synopt:{opt quantile}{cmd:(}{it:p}{cmd:)}}{it:p}/100 quantile; {it:p} in [0,100]
    {p_end}
{synopt:{opt p}{cmd:(}{it:p}{cmd:)}}same as {cmd:quantile()}
    {p_end}
{synopt:{opt hdquantile}{cmd:(}{it:p}{cmd:)}}{it:p}/100 Harrell/Davis (1982) quantile; {it:p} in [0,100]
    {p_end}
{synopt:{opt mquantile}{cmd:(}{it:p}{cmd:)}}{it:p}/100 mid-quantile; {it:p} in [0,100]
    {p_end}
{synopt:{opt density}{cmd:(}{it:x}{cmd:)}}kernel density at value {it:x}
    {p_end}
{synopt:{opt histogram}{cmd:(}{it:x1}{cmd:,}{it:x2}{cmd:)}}histogram density of data within ({it:x1},{it:x2}]
    {p_end}
{synopt:[*]{opt cdf}{cmd:(}{it:x}{cmd:)}}cumulative distribution (CDF) at value {it:x}; prefix {it:*} is empty for default,
    {cmd:m} for mid-adjusted CDF, {cmd:f} for floor CDF
    {p_end}
{synopt:[*]{opt ccdf}{cmd:(}{it:x}{cmd:)}}complementary CDF at value {it:x}; prefix {it:*} is empty for default,
    {cmd:m} for mid-adjusted CCDF, {cmd:f} for floor CCDF
    {p_end}
{synopt:{opt proportion}{cmd:(}{it:x1}[{cmd:,}{it:x2}]{cmd:)}}proportion of data equal to {it:x1} or within [{it:x1},{it:x2}]
    {p_end}
{synopt:{opt pct}{cmd:(}{it:x1}[{cmd:,}{it:x2}]{cmd:)}}percent of data equal to {it:x1} or within [{it:x1},{it:x2}]
    {p_end}
{synopt:{opt frequency}[{cmd:(}{it:x1}[{cmd:,}{it:x2}]{cmd:)}]}overall frequency, or frequency of data equal to {it:x1} or within [{it:x1},{it:x2}]
    {p_end}
{synopt:{opt count}[{cmd:(}{it:x1}[{cmd:,}{it:x2}]{cmd:)}]}same as {cmd:frequency()}
    {p_end}
{synopt:{opt total}[{cmd:(}{it:x1}[{cmd:,}{it:x2}]{cmd:)}]}overall total, or total of data equal to {it:x1} or within [{it:x1},{it:x2}]
    {p_end}
{synopt:{opt min}}observed minimum (standard error set to zero)
    {p_end}
{synopt:{opt max}}observed maximum (standard error set to zero)
    {p_end}
{synopt:{opt range}}{cmd:max}-{cmd:min} (standard error set to zero)
    {p_end}
{synopt:{opt midrange}}({cmd:min}+{cmd:max})/2 (standard error set to zero)
    {p_end}

{syntab:Location measures}
{synopt:{opt mean}}arithmetic mean
    {p_end}
{synopt:{opt gmean}}geometric mean (data must be positive)
    {p_end}
{synopt:{opt hmean}}harmonic mean (data must be positive)
    {p_end}
{synopt:{cmd:trim(}{it:p1}[{cmd:,}{it:p2}]{cmd:)}}trimmed mean with
    {it:p1}/100 lower trimming and {it:p2}/100 upper trimming; {it:p1} and {it:p2} in
    [0,50]; {it:p2}={it:p1} if omitted; default is {it:p1}={it:p2}=25
    {p_end}
{synopt:{cmd:winsor(}{it:p1}[{cmd:,}{it:p2}]{cmd:)}}winsorized mean with
    {it:p1}/100 lower winsorizing and {it:p2}/100 upper winsorizing; {it:p1} and {it:p2} in
    [0,50]; {it:p2}={it:p1} if omitted; default is {it:p1}={it:p2}=25
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
{synopt:{opt mse}[{cmd:(}{it:x}[{cmd:,}{it:df}]{cmd:)}]}mean squared deviation from value
    {it:x} (mean squared error); default is {it:x}=0 and {it:df}=0
    {p_end}
{synopt:{opt rmse}[{cmd:(}{it:x}[{cmd:,}{it:df}]{cmd:)}]}root mean
    squared deviation from value {it:x}; default is {it:x}=0 and {it:df}=0
    {p_end}
{synopt:{opt iqr}[{cmd:n}][{cmd:(}{it:p1}{cmd:,}{it:p2}{cmd:)}]}interquantile range; default
    is {cmd:iqr(25,75)} (interquartile range); specify {cmd:iqrn} for
    normalized IQR, equal to 1/(invnormal({it:p2}) - invnormal({it:p1})) * {cmd:iqr}
    {p_end}
{synopt:{opt mad}[{cmd:n}][{cmd:(}{it:l}[{cmd:,}{it:t}]{cmd:)}]}median (or mean if {it:l}!=0)
    absolute deviation from the median (or mean if {it:t}!=0); specify {cmd:madn} for
    normalized MAD, equal to 1/invnormal(0.75) * {cmd:mad} (or sqrt(pi/2) * {cmd:mad} if {it:l}!=0)
    {p_end}
{synopt:{opt mae}[{cmd:n}][{cmd:(}{it:l}[{cmd:,}{it:x}]{cmd:)}]}median (or mean if {it:l}!=0)
    absolute deviation from value {it:x}; default is {it:x}=0; specify {cmd:maen} for
    normalized MAE, equal to 1/invnormal(0.75) * {cmd:mae} or (sqrt(pi/2) * {cmd:mae} if {it:l}!=0)
    {p_end}
{synopt:{opt md}[{cmd:n}]}mean absolute pairwise difference; equal to 2 * {cmd:mean} * {cmd:gini}; specify {cmd:mdn} for
    normalized MD, equal to sqrt(pi)/2 * {cmd:md}
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
{synopt:{opt ekurtosis}}excess kurtosis; equal to {cmd:kurtosis}-3
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
{synopt:{opt hoover}}Hoover index (Robin Hood index, Ricci-Schutz, Pietra index)
    {p_end}
{synopt:[{cmd:a}]{opt gini}[{cmd:(}{it:df}{cmd:)}]}Gini coefficient; {it:df} applies
    small-sample adjustment; default is {it:df}=0; specify {cmd:agini} for the absolute Gini coefficient
    {p_end}
{synopt:{opt mld}}mean log deviation (MLD); equal to {cmd:ge(0)}
    {p_end}
{synopt:{opt theil}[{cmd:(}{it:nozero}{cmd:)}]}Theil index; equal to {cmd:ge(1)};
    argument {it:nozero} decides whether or not to include observations that are
    equal to zero; the default is to exclude them ({it:nozero}=1); type
    {cmd:theil0} or {cmd:theil(0)} to include them ({it:nozero}=0)
    {p_end}
{synopt:{opt ge}[{cmd:(}{it:alpha}{cmd:)}]}generalized entropy (Shorrocks 1980)
    with parameter {it:alpha}; default is {it:alpha}=1 (in which case
    {cmd:ge}={cmd:theil})
    {p_end}
{synopt:{opt atkinson}[{cmd:(}{it:epsilon}{cmd:)}]}Atkinson index with parameter
    {it:epsilon}>=0; default is {it:epsilon}=1
    {p_end}
{synopt:{opt cv}[{cmd:(}{it:df}{cmd:)}]}coefficient of variation; default is {it:df}=1;
    {cmd:cv(0)}=sqrt(2*{cmd:ge(2)})
    {p_end}
{synopt:{opt lvar}[{cmd:(}{it:df}{cmd:)}]}logarithmic variance; default is {it:df}=1
    {p_end}
{synopt:{opt vlog}[{cmd:(}{it:df}{cmd:)}]}variance of logarithm; default is {it:df}=1
    {p_end}
{synopt:{opt sdlog}[{cmd:(}{it:df}{cmd:)}]}standard deviation of logarithm; default is {it:df}=1
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
{synopt:{opt sratio}[{cmd:(}{it:l1}{cmd:,}{it:u1}{cmd:,}{it:l2}{cmd:,}{it:u2}{cmd:)}]}percentile
    share ratio; default is {it:l1}=0, {it:u1}=10, {it:l2}=90, {it:u2}=100; can also specify
    {cmd:sratio(}{it:u1}{cmd:,}{it:l2}{cmd:)}
    {p_end}
{synopt:[*]{cmd:lorenz}{cmd:(}{it:p}{cmd:)}}Lorenz ordinate, {it:p} in [0,100];
    prefix {it:*} is empty for default, {cmd:g} for generalized, {cmd:t} for total,
    {cmd:a} for absolute, {cmd:e} for equality gap
    {p_end}
{synopt:[*]{cmd:share}{cmd:(}{it:p1}{cmd:,}{it:p2}{cmd:)}}percentile
    share, {it:p1} and {it:p2} in [0,100]; prefix {it:*} is empty or {cmd:p} for proportion,
    {cmd:d} for density, {cmd:g} for generalized, {cmd:t} for total, {cmd:a} for average
    {p_end}

{syntab:Inequality decomposition}
{synopt:{it:d}{cmd:_gini}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:df}]{cmd:)}]}where
    {it:d} is {cmd:b} for the between-group Gini coefficient, or {cmd:gw} for the
    weighted average of group-specific Gini coefficients; {it:by} specifies the group
    variable (string allowed); default is as set by option {cmd:by()}; {it:df} applies
    small-sample adjustment; default is {it:df}=0;
    can also specify {it:d}{opt _gini(df)}
    {p_end}
{synopt:{it:d}{cmd:_mld}[{cmd:(}{it:{help varname:by}}{cmd:)}]}where {it:d} is
    {cmd:b} for the between-group MLD, {cmd:w} for the within-group MLD, or
    {cmd:gw} for the weighted average of group-specific MLDs ({cmd:gw_mld} is
    equivalent to {cmd:w_mld}); {it:by} as for {it:d}{cmd:_gini}
    {p_end}
{synopt:{it:d}{cmd:_theil}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:nozero}]{cmd:)}]}where {it:d}
    is {cmd:b} for the between-group Theil index, {cmd:w} for the
    within-group Theil index, or {cmd:gw} for the weighted average of
    group-specific Theil indices; {it:by} as for {it:d}{cmd:_gini}; {it:nozero} as
    for {cmd:theil} (default is {it:nozero}=1); can also specify {it:d}{opt _theil(nozero)}
    {p_end}
{synopt:{it:d}{cmd:_ge}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:alpha}]{cmd:)}]}where
    {it:d} is {cmd:b} for the between-group generalized entropy, {cmd:w} for the
    within-group generalized entropy, or {cmd:gw} for the weighted
    average of group-specific generalized entropy; {it:by} as for {it:d}{cmd:_gini};
    can also specify {it:d}{opt _ge(alpha)}
    {p_end}
{synopt:{it:d}{cmd:_vlog}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:df}]{cmd:)}]}where
    {it:d} is {cmd:b} for the between-group variance of logarithm, {cmd:w} for the
    within-group variance of logarithm, or {cmd:gw} for the weighted average of
    group-specific variance of logarithm; {it:by} as for {it:d}{cmd:_gini};
    can also specify {it:d}{opt _vlog(df)}
    {p_end}

{syntab:Concentration measures}
{synopt:{opt gci}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:df}]{cmd:)}]}Gini concentration index;
    {it:by} specifies the sort variable; default is as set by option {cmd:by()};
    {it:df} applies small-sample adjustment; default is {it:df}=0; can also
    specify {opt gci(df)}
    {p_end}
{synopt:{opt aci}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:df}]{cmd:)}]}absolute Gini concentration index; syntax
    as for {cmd:gci}
    {p_end}
{synopt:[*]{cmd:ccurve}{cmd:(}{it:p}[{cmd:,}{it:{help varname:by}}]{cmd:)}}concentration curve ordinate,
    {it:p} in [0,100]; prefix {it:*} is empty for default, {cmd:g} for generalized, {cmd:t} for total,
    {cmd:a} for absolute, {cmd:e} for equality gap;
    {it:by} as for {cmd:gci}
    {p_end}
{synopt:[*]{cmd:cshare}{cmd:(}{it:p1}{cmd:,}{it:p2}[{cmd:,}{it:{help varname:by}}]{cmd:)}}concentration share,
    {it:p1} and {it:p2} in [0,100]; prefix {it:*} is empty or {cmd:p} for proportion, {cmd:d} for density,
    {cmd:g} for generalized, {cmd:t} for total, {cmd:a} for average;
    {it:by} as for {cmd:gci}
    {p_end}

{syntab:Poverty measures}
{synopt:{opt hcr}[{cmd:(}{it:pline}{cmd:)}]}head count ratio (i.e. proportion poor); {it:pline} specifies the
    poverty line > 0; {it:pline} can be {varname} or {it:#}; default is as set by option {cmd:pline()}
    {p_end}
{synopt:[{cmd:a}]{opt pgap}[{cmd:(}{it:pline}{cmd:)}]}poverty gap (proportion by which mean outcome of poor
    is below {it:pline}); specify {cmd:apgap} for absolute poverty gap ({it:pline} - mean outcome of poor);
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:[{cmd:a}]{opt pgi}[{cmd:(}{it:pline}{cmd:)}]}poverty gap index; equal to {cmd:hcr}*{cmd:pgap}; specify
    {cmd:apgi} for absolute poverty gap index, equal to {cmd:hcr}*{cmd:apgap};
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:{opt fgt}[{cmd:(}{it:a}[{cmd:,}{it:pline}]{cmd:)}]}Foster–Greer–Thorbecke index with {it:a}>=0
    (Foster et al. 1984, 2010); default is {it:a}=0 (head count ratio); {it:a}=1 is equivalent to
    {cmd:pgi};
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:{opt sen}[{cmd:(}{it:pline}{cmd:)}]}Sen poverty index (Sen 1976; using the
    replication invariant version of the index, also see Shorrocks 1995);
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:{opt sst}[{cmd:(}{it:pline}{cmd:)}]}Sen-Shorrocks-Thon poverty index
    (see, e.g., Osberg and Xu 2008);
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:{opt takayama}[{cmd:(}{it:pline}{cmd:)}]}Takayama poverty index
    (Takayama 1979);
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:{opt watts}[{cmd:(}{it:pline}{cmd:)}]}Watts index (see, e.g., Saisana 2014);
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:{opt chu}[{cmd:(}{it:a}[{cmd:,}{it:pline}]{cmd:)}]}Clark-Hemming-Ulph poverty index with {it:a} in [0,100]
    (Clark et al. 1981); default is {it:a}=50; {it:a}=0 is equivalent to
    1-exp(-{cmd:watts}); {it:a}=100 is equivalent to {cmd:fgt(1)};
    {it:pline} as for {cmd:hci}
    {p_end}
{synopt:[{cmd:a}]{cmd:tip}{cmd:(}{it:p}[{cmd:,}{it:pline}]{cmd:)}}TIP ordinate,
    {it:p} in [0,100]; specify {cmd:atip()} for absolute TIP ordinates;
    {it:pline} as for {cmd:hci}
    {p_end}

{marker association}{...}
{syntab:Association measures}
{synopt:{opt correlation}[{cmd:(}{it:{help varname:by}}{cmd:)}]}correlation coefficient;
    {it:by} specifies the secondary variable; default is as set by option {cmd:by()}
    {p_end}
{synopt:{opt slope}[{cmd:(}{it:{help varname:by}}{cmd:)}]}regression slope
    (equal to mean difference if {it:by} is dichotomous); {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt b}[{cmd:(}{it:{help varname:by}}{cmd:)}]}same as {cmd:slope}
    {p_end}
{synopt:{opt cohend}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:df}]{cmd:)}]}Cohen's d
    (allowing unequal group sizes); {it:df} applies small-sample
    adjustment; default is {it:df}=2; can also specify {opt cohend(df)};
    {it:by} is assumed to be dichotomous (string allowed)
    {p_end}
{synopt:{opt covariance}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:df}]{cmd:)}]}covariance; {it:df} applies small-sample
    adjustment; default is {it:df}=1; can also specify {opt covariance(df)};
    {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt rsquared}[{cmd:(}{it:{help varname:by}}{cmd:)}]}R squared, equal to {cmd:correlation}^2;
     {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt spearman}[{cmd:(}{it:{help varname:by}}{cmd:)}]}Spearman's rank correlation;
    {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt taua}[{cmd:(}{it:{help varname:by}}{cmd:)}]}Kendall's tau-a (using fast algorithm by Newson 2006);
    {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt taub}[{cmd:(}{it:{help varname:by}}{cmd:)}]}Kendall's tau-b (using fast algorithm by Newson 2006);
    {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt somersd}[{cmd:(}{it:{help varname:by}}{cmd:)}]}Somers' D (using fast algorithm by Newson 2006);
    {it:by} as for {cmd:correlation}
    {p_end}
{synopt:{opt gamma}[{cmd:(}{it:{help varname:by}}{cmd:)}]}Goodman and Kruskal's gamma (using fast algorithm by Newson 2006);
    {it:by} as for {cmd:correlation}
    {p_end}

{syntab:Categorical data (univariate)}
{synopt:{opt hhi}[{cmd:n}]}Herfindahl–Hirschman index (Herfindahl index, Simpson index);
    specify {cmd:hhin} for normalization  ({cmd:hhi}-1/K)/(1-1/K), where
    K is the number of categories
    {p_end}
{synopt:{opt gimp}[{cmd:n}]}Gini impurity (Gini–Simpson index, Simpson's interaction index,
    Blau index, Gibbs–Martin index); {cmd:gimp} = 1-{cmd:hhi}; {cmd:gimpn} = 1-{cmd:hhin}
    {p_end}
{synopt:{opt entropy}[{cmd:(}{it:base}{cmd:)}]}Shannon entropy; {it:base} specifies
    the base of the logarithm (default is natural logarithm)
    {p_end}
{synopt:{opt hill}[{cmd:(}{it:q}{cmd:)}]}Hill number (true diversity,
    effective number of species); {it:q} specifies the order of the diversity;
    default is {it:q}=1 such that {cmd:hill} = exp({cmd:entropy}); if {it:q}=0,
    {cmd:hill} is equal to the observed number of categories
    {p_end}
{synopt:{opt renyi}[{cmd:(}{it:q}{cmd:)}]}Rényi entropy;
    equal to ln({cmd:hill(}{it:q}{cmd:)}); default is {it:q}=1 such that
    {cmd:renyi} = {cmd:entropy}
    {p_end}

{marker catbivar}{...}
{syntab:Categorical data (bivariate)}
{synopt:{opt mindex}[{cmd:(}{it:{help varname:by}}[{cmd:,}{it:base}]{cmd:)}]}mutual information index (M index);
    {it:by} specifies the secondary variable (string allowed); default is as set by option {cmd:by()};
    {it:base} specifies the base of the logarithm (default is natural logarithm);
    can also specify {opt mindex(base)}
    {p_end}
{synopt:{opt uc}[{cmd:l}|{cmd:r}][{cmd:(}{it:{help varname:by}}{cmd:)}]}uncertainty coefficient (H index);
    {cmd:ucl} returns the asymmetric coefficient with respect to the left-hand
    side variable (i.e. division by the entropy of the main variable),
    {cmd:ucr} is with respect to the right-hand side variable (i.e. division by
    the entropy of the secondary variable), {cmd:uc} returns the symmetric
    uncertainty coefficient (weighted average of {cmd:ucl} and {cmd:ucr});
    {it:by} as for {cmd:mindex}
    {p_end}
{synopt:{opt cramersv}[{cmd:(}{it:{help varname:by}}{cmd:)}]}Cramér's V;
    {it:by} as for {cmd:mindex}
    {p_end}
{synopt:{opt dissimilarity}[{cmd:(}{it:{help varname:by}}{cmd:)}]}(generalized)
    dissimilarity index (Duncan segregation index, Duncan's D);
    {it:by} as for {cmd:mindex}
    {p_end}
{synopt:{opt or}[{cmd:(}{it:{help varname:by}}{cmd:)}]}odds ratio; variables are
    interpreted as true/false indicators (false if 0, else true); {it:by} as for {cmd:mindex}
    {p_end}
{synopt:{opt rr}[{cmd:(}{it:{help varname:by}}{cmd:)}]}risk ratio; variables are
    interpreted as true/false indicators (false if 0, else true); {it:by} as for {cmd:mindex}
    {p_end}
{synoptline}

{pstd}
    {it:Note on output formatting in Stata 15 (or in Stata 16 prior to the update of March 30, 2021):} If
    statistics with parameters in parentheses are requested, {cmd:dstat summarize}
    may possibly display a somewhat disarranged output table. Type

        {cmd:. version 14: dstat summarize} {it:...}

{pstd}
    to obtain an improved table in such a case.


{marker options}{...}
{title:Options}

{marker mainopts}{...}
{dlgtab:Main}

{phang}
    {cmd:nocasewise} causes missing values to be excluded for each variable in
    {it:varlist} individually. The default is to perform casewise deletion of
    observations, that is, to restrict the sample to observations that are not
    missing for any of the variables. If {cmd:nocasewise} is specified, the
    overall estimation sample is still restricted by the {cmd:if} and {cmd:in}
    qualifiers, the weights, and the variables specified in {cmd:over()} and
    {cmd:balance()}, but not by missing values in the main {it:varlist} (or in
    {cmd:by()}, {it:by}, {cmd:pline()}, or {it:pline}). For each variable
    the subsample of all nonmissing values within the overall estimation sample
    will then be used in the relevant computations.

{marker over}{...}
{phang}
    {cmd:over(}{help varname:{it:overvar}}[{cmd:,} {it:options}]{cmd:)}
    computes results for each subpopulation defined by the values of
    {it:overvar}. {it:overvar} must be integer and nonnegative. {it:options}
    are as follows:

{phang2}
    {opth sel:ect(numlist)} selects (and orders) subpopulations. {it:numlist}
    specifies the values of the subpopulations to be included
    and also determines the order of the subpopulations in the output. The basis
    for estimation will always be the total sample including all subpopulations.

{phang2}
    {opt contr:ast}[{cmd:(}{it:#}|{cmd:lag}|{cmd:lead}{cmd:)}] computes
    contrasts between subpopulations. If {cmd:contrast} is specified without
    argument, the results from the first reported subpopulation or,
    if {helpb dstat##total:total()} is specified, the
    results from {cmd:total()} will be used as the basis for the
    contrasts. Alternatively, specify the value of the reference subpopulation
    in parentheses (this may also be a subpopulation that has been excluded by
    {cmd:select()}), or type {cmd:contrast(lag)} or {cmd:contrast(lead)} to take
    stepwise contrasts with respect to the previous or next reported
    subpopulation, respectively. {cmd:contrast} implies {cmd:common} (if
    relevant).

{pmore2}
    The estimates from the reference (sub)population will be included among the
    stored results (in logarithmic form if {cmd:lnratio} is specified), but
    their display will be suppressed. Specify display
    option {cmd:cref} to report these results in the output.

{phang2}
    {opt ratio} requests that the contrasts are expressed as ratios. The
    default is to express contrasts as differences. {cmd:ratio} implies
    {cmd:contrast}.

{phang2}
    {opt lnr:atio} requests that the contrasts are expressed as differences in
    logarithms. The default is to express contrasts as raw differences. {cmd:lnratio}
    implies {cmd:contrast} and takes precedence over {cmd:ratio}.

{pmore2}
    When applying {cmd:lnratio} you may also want to specify reporting option
    {helpb dstat##display_opts:eform} to display results that are transformed back to
    ratios. In fact, point estimates, standard errors, and confidence intervals
    from {cmd:lnratio} with {cmd:eform} are identical to results
    from {cmd:ratio} with option {cmd:citype(log)}. An advantage of
    {cmd:lnratio}, however, is that the null hypothesis for t-statistics and
    p-values is ratio = 1 or, more precisely, ln(ratio) = 0 (i.e. no group
    difference). For {cmd:ratio} the null hypothesis is ratio = 0, which
    does not appear useful (this is why {cmd:dstat} suppresses t-statistics and
    p-values in case of {cmd:ratio}). A disadvantage of {cmd:lnrange} is that it
    cannot represent cases in which the comparison estimate and, hence, the ratio
    is zero.

{phang2}
    {opt accum:ulate} accumulates results across subpopulations
    (running sum). Only one of {cmd:contrast} and {cmd:accumulate} is allowed.

{pmore}
    Option {cmd:over()} is not supported by {cmd:dstat pw}.

{marker total}{...}
{phang}
    {cmd:total}[{cmd:(}{it:totaltype}{cmd:)}] reports additional results across
    subpopulations, where {it:totaltype} is as follows.

{p2colset 13 22 24 2}{...}
{p2col:{opt p:ooled}}results based on pooled sample; this is the default
    {p_end}
{p2col:{opt m:ean}}mean of subpopulation results
    {p_end}
{p2col:{opt w:mean}}observation-weighted mean of subpopulation results
    {p_end}

{pmore}
    Note that {cmd:total(pooled)} will include the observations from all
    subpopulations, including subpopulations that may have been excluded by
    {cmd:over()}-suboption {cmd:select()}. In contrast, {cmd:total(mean)}
    and {cmd:total(wmean)} are based on the results selected by suboption
    {cmd:select()} only. Furthermore, if {cmd:nocasewise} or {cmd:relax} is
    applied, the weights used by {cmd:total(wmean)} reflect the relative
    subpopulation sizes before excluding observations due to missing values or
    values that are out of support. {cmd:total(mean)} and {cmd:total(wmean)}
    imply {cmd:common} (if relevant). {cmd:total()} only has an effect if
    {cmd:over()} is specified.

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
    use the total across all subpopulations as the reference distribution,
    including subpopulations that may have been excluded by {cmd:select()}. Specify
    {cmd:reference(}{it:#}{cmd:)} to obtain the reference distributions from
    observations for which {it:overvar}={it:#}; this may also be a subpopulation
    that has been excluded by {cmd:select()}.

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

{pmore}
    Balancing weights will only be computed once per subpopulation. If
    {cmd:casewise} is specified, balancing will be based on the overall estimation
    sample as defined in the description of the {cmd:casewise} option; the weights
    will not be recomputed for each variable individually.

{marker gropt}{...}
{phang}
    {opt graph}[{cmd:(}{help dstat##graph_options:{it:graph_options}}{cmd:)}]
    displays the results in a graph using {helpb coefplot}. The coefficients
    table will be suppressed in this case (unless option {cmd:table} is
    specified). Alternatively, use command {cmd:dstat graph} to display the
    graph after estimation.

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
    confidence interval limits. {it:type} can be:

{p2colset 17 28 30 2}{...}
{p2col:{cmdab:norm:al}}normal CIs
    {p_end}
{p2col:{cmd:logit}}logit transformed CIs; useful for statistics in [0,1]
    {p_end}
{p2col:{cmd:probit}}probit transformed CIs; useful for statistics in [0,1]
    {p_end}
{p2col:{cmd:atanh}}inverse hyperbolic tangent transformed CIs; useful for statistics in [-1,1]
    {p_end}
{p2col:{cmd:log}}log transformed CIs; useful for statistics > 0
    {p_end}
{p2col:{cmdab:agres:ti}}Agresti-Coull CIs; useful for proportions
    {p_end}
{p2col:{cmd:exact}}exact (Clopper-Pearson) CIs; useful for proportions
    {p_end}
{p2col:{cmdab:jeff:reys}}Jeffreys CIs; useful for proportions
    {p_end}
{p2col:{cmd:wilson}}Wilson CIs; useful for proportions
    {p_end}

{pmore2}
    The default depends on subcommand and options. Use {cmd:citype()} to
    override the default. For details on {cmd:agresti}, {cmd:exact},
    {cmd:jeffreys}, and {cmd:wilson} see the documentation of {helpb proportion}.

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

{phang2}
    [{ul:{cmd:no}}]{opt pv:alues} decides whether p-values and their test
    statistics are reported in the coefficient table or not. The default is
    {cmd:nopvalues} unless {cmd:over(, contrast())} has been specified or if
    {it:subcmd} is {cmd:pw}.

{phang2}
    {opt cref} causes the estimates from the reference (sub)population to be
    included in the coefficient tables. The default is to suppress these
    results. {cmd:cref} is only relevant if {cmd:over(, contrast())} has been
    specified.

{marker display_opts}{...}
{phang2}
    {it:display_options} are standard reporting options such as {cmd:eform},
    {cmd:cformat()}, or {cmd:coeflegend}; see {help eform_option:{bf:[R]} {it:eform_option}} and
    the Reporting options in {helpb estimation options:[R] Estimation options}.

{marker order}{...}
{phang}
    {opt order(keywords)} sets the order in which the results are
    arranged. Available keywords are {cmdab:o:ver}, {cmdab:v:ariables}, and
    {cmdab:s:tatistics}. Keyword {cmd:statistics} is only relevant for
    {cmd:dstat summarize}. Option {cmd:order()} is not supported by
    {cmd:dstat pw}.

{pmore}
    In most cases the default is {cmd:order(over variables)} which means that
    results are ordered by subpopulations and then by variables. Type
    {cmd:order(variables over)} to flip the order. For {cmd:dstat summarize}
    you could, for example, type {cmd:order(over statistics)} to order the
    results by subpopulations and then by statistics, and use the variables
    rather than the statistics as coefficient names.

{phang}
    {opt novalues} prevents using the values of the evaluation points as
    coefficient names. If {cmd:novalues}
    is specified, the coefficients will be named as {it:stub#}, where
    {it:#} is consecutive number and {it:stub} is
    {cmd:at} in case of {cmd:dstat summarize} with option {cmd:at()},
    {cmd:d} in case of {cmd:dstat density},
    {cmd:h} in case of {cmd:dstat histogram},
    {cmd:p} in case of {cmd:dstat proportion},
    {cmd:c} in case of {cmd:dstat cdf} and {cmd:dstat ccdf},
    {cmd:q} in case of {cmd:dstat quantile},
    {cmd:l} in case of {cmd:dstat lorenz},
    {cmd:s} in case of {cmd:dstat pshare} (and for {cmd:dstat histogram} and
    {cmd:dstat pshare} the last coefficient, i.e. the upper limit of last bin, will be named
    {cmd:_ul}).

{phang}
    {opth vformat(fmt)} sets the display format used to create coefficient names
    from evaluation points. See help {helpb format} for available formats.

{marker vce}{...}
{dlgtab:SE/VCE}

{phang}
    {opt vce(vcetype)} determines how standard errors are computed. {it:vcetype} may be:

            {opt none}
            {opt a:nalytic} [{cmd:,} {opt norm:al} {opt minus(#)} ]
            {opt cl:uster} {it:clustvar} [{cmd:,} {opt norm:al} {opt minus(#)} ]
            {opt svy} [{help svy##svy_vcetype:{it:svy_vcetype}}] [{cmd:,} {help svy##svy_options:{it:svy_options}} ]
            {opt boot:strap} [{cmd:,} {help bootstrap:{it:bootstrap_options}} ]
            {opt jack:knife} [{cmd:,} {help jackknife:{it:jackknife_options}} ]

{pmore}
    {cmd:vce(none)} omits the computation of standard errors. This saves computer
    time.

{pmore}
    {cmd:vce(analytic)}, the default, computes standard errors based on
    influence functions. Suboption {cmd:normal} omits {cmd:e(df_r)} from the
    saved results so that the normal distribution will be used for tests and
    confidence intervals rather than the t-distribution. Suboption
    {opt minus(#)} sets k={it:#} for the multiplier n/(n-k) of the variance
    matrix; the default is {cmd:minus(1)}.

{pmore}
    {bind:{cmd:vce(cluster} {it:clustvar}{cmd:)}} computes standard errors based
    on influence functions allowing for intragroup correlation, where
    {it:clustvar} specifies to which group each observation belongs. Suboptios
    {cmd:normal} and {cmd:minus()} are as for {cmd:vce(analytic)}, with n as the
    number of clusters.

{pmore}
    {cmd:vce(svy)} computes standard errors taking the survey design as set by
    {helpb svyset} into account. The syntax is equivalent to the syntax of the {helpb svy}
    prefix command; that is, {cmd:vce(svy)} is {cmd:dstat}'s way to support
    the {helpb svy} prefix.

{pmore}
    {cmd:vce(bootstrap)} and {cmd:vce(jackknife)} compute standard errors using
    {helpb bootstrap} or {helpb jackknife}, respectively; see help {it:{help vce_option}}.

{phang}
    {cmd:nose} is an alias for {cmd:vce(none)}. {cmd:nose} overrides {cmd:vce(analytic)} and
    {cmd:vce(cluster)}, but has no effect if specified together with
    {cmd:vce(svy)}, {cmd:vce(bootstrap)}, or {cmd:vce(jackknife)}.

{phang}
    [{cmd:no}]{cmd:cov} determines wether the full variance-covariance matrix
    of the estimates is stored in {cmd:e(V)}, or whether only the standard
    errors are stored in vector {cmd:e(se)}. The default is {cmd:cov} (full
    variance matrix) for subcommands {cmd:summarize} and {cmd:pw}, and
    {cmd:nocov} (standard errors only) for all other subcommands. {cmd:nocov}
    saves memory if the number of evaluation points is large (for example, if
    you estimate the density using 400 points across two subpopulations, the
    covariance matrix has 800 x 800 = 640'000 elements; the vector of standard
    errors has only 800 elements). For {cmd:vce(analytic)} and
    {cmd:vce(cluster)}, option {cmd:nocov} also saves computer time (since the
    computation of covariances is skipped; in the other cases, covariances are
    removed after estimation). For {cmd:vce(svy)}, option {cmd:nocov} also
    removes auxiliary variance matrices such as {cmd:e(V_srs)}. Note that
    post-estimation commands that rely on covariances (or on auxiliary variance
    matrices in case of {cmd:svy}) will not work after {cmd:nocov} has been
    applied; specify option {cmd:cov} if you intend to use such post-estimation
    commands (e.g., {helpb test} or {helpb lincom}) after subcommands other than
    {cmd:summarize} or {cmd:pw}.

{phang}
    {cmd:nobwfixed} allows the bandwidth(s) for density estimation to vary
    across replications. This is only relevant if density estimation is
    requested (subcommand {cmd:density} or {cmd:pdf}; subcommand
    {cmd:summarize} with at least one {cmd:density()} statistic), if the
    bandwidth is not set to a specific value (or a list of specific values)
    using option {cmd:bwidth()}, and if a replication technique is used for
    standard error estimation, i.e. {cmd:vce(bootstrap)}, {cmd:vce(jackknife)},
    or {cmd:vce(svy)} with {help svy##svy_vcetype:{it:svy_vcetype}} other than
    {cmd:linearized}. The default is to hold the bandwidth(s) fixed across
    replications.

{marker generate}{...}
{phang}
    {cmd:generate(}{it:names}[{cmd:,} {it:options}]{cmd:)} stores the influence
    functions that were used to compute the standard errors, where {it:names}
    is either a list of (new) variable names or
    {help newvarlist##stub*:{it:stub}}{cmd:*} to create names {it:stub}{cmd:1},
    {it:stub}{cmd:2}, etc. {it:options} are {cmd:rif} to store RIFs,
    {cmdab:sca:ling(}{cmdab:t:otal}{cmd:)} or {cmdab:sca:ling(}{cmdab:m:ean}{cmd:)}
    to determine the scaling, {cmdab:com:pact} to merge the influence functions
    across subpopulations, and {cmdab:qui:etly} to suppress output; see
    {it:{help dstat##predict_options:predict_options}} below.

{phang}
    {cmd:rif(}{it:names}[{cmd:,} {it:options}]{cmd:)} is an alias for
    {cmd:generate(}{it:names}{cmd:,} {cmd:rif} [{it:options}]{cmd:)}.

{phang}
    {opt replace} allows replacing existing variables.

{marker sum}{...}
{dlgtab:Subcommand summarize}

{phang}
    {cmd:at(}{it:{help varname:atvar}} [{cmd:=} {it:{help numlist}}]{cmd:)}
    computes conditional statistics by levels of {it:atvar}. Use
    {it:numlist} to specify a custom set of levels to be considered. If {it:numlist}
    is omitted, all observed levels of {it:atvar} are used. Restricting the
    considered levels does not change the overall estimation sample.

{phang}
    {opt range(a [b])} limits the range of levels from {it:atvar} to be
    considered. Argument {it:a} can be specified as {cmd:.} (missing) to impose no
    lower limit; argument {it:b} can be omitted or specified as
    {cmd:.} (missing) to impose no upper limit. Option {cmd:range()} has no effect
    if {it:numlist} is specified in {cmd:at()}.

{phang}
    {opt categorical} treats {it:atvar} as categorical. This requires that
    {it:atvar} complies to Stata's rules for factor variables
    (no negative or non-integer values) and affects how the coefficients are
    labeled in the output.

{phang}
    {opt relax} continues computations even if there are observations outside
    of the support for a specific statistic. Some statistics such as the
    geometric mean, the MLD, or the Theil index require observations to be
    within a specific domain (e.g. strictly positive). By default, {cmd:dstat} aborts
    with error if observations violating such requirements are encountered. Specify
    {cmd:relax} if you want to continue computations based on the valid
    observations in such a case. Exclusion of invalid observations will be
    applied to each statistic individually; that is, the invalid observations
    will not be dropped from the overall estimation sample.

{phang}
    {opth by(varname)} specifies a default secondary variable for
    inequality desomposition, concentration indices, and association measures.

{phang}
    {opt pline(#|varname)} specifies a default poverty line for poverty
    measures, either as a single value or as a variable containing observation-specific
    values.

{phang}
    {opt pstrong} selects the poverty definition to be applied (see Donaldson and
    Weymark 1986). The default is to use the "weak" definition, that is, to treat
    outcomes equal to the poverty line as non-poor. Specify {cmd:pstrong} to treat
    these cases as poor ("strong" definition). The choice of definition is relevant
    only for some of the poverty measures.

{marker pw}{...}
{dlgtab:Subcommand pw}

{phang}
    {opt lower} requests that the lower-triangle elements of the association
    matrix be computed. The default is to compute both the lower-triangle elements
    and the upper-triangle elements.

{pmore}
    Most supported statistics are symmetric in the sense that the upper and
    lower triangles of the association matrix (i.e. the matrix of pairwise
    associations among the variables in {it:varlist}) contain the same results
    (i.e. the association between X and Y is the same as the association between
    Y and X). For asymmetric statistics (e.g. {cmd:slope}) the column
    (i.e. equation) variable is treated as the dependent variable.

{phang}
    {opt upper} requests that the upper-triangle elements of the association
    matrix be computed. The default is to compute both the lower-triangle elements
    and the upper-triangle elements.

{phang}
    {opt diagonal} includes the diagonal elements of the association
    matrix (associations of the variables with themselves). By default,
    diagonal elements are omitted.

{marker density}{...}
{dlgtab:Subcommand density}

{phang}
    {opt n(#)} sets the number of points for which the density is to
    be estimated. A regular grid of {it:#} points spanning the
    data range (within subpopulation; plus some padding) will be used. The
    default is {cmd:n(99)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opt common} requests that a common set of evaluation points is used across
    all subpopulations. The default is to determine the evaluation points based on
    the data range within subpopulation. If {cmd:common} is specified, the
    evaluation points will be based on the data range in the total population.

{phang}
    [{cmd:l}|{cmd:r}]{cmd:tight} omits padding when determining the evaluation
    grid. Specify {cmd:tight} to omit padding on both sides, that is, to use a grid
    from the observed minimum to the observed maximum of the data. Specify
    {cmd:ltight} to omit padding only on the left, that is, to use the observed
    minimum as the lower bound of the grid. Specify {cmd:rtight} to omit padding
    only on the right, that is, to use the observed maximum as the upper bound
    of the grid. Option {cmd:tight} has no effect if {cmd:range()} or {cmd:at()}
    is specified.

{phang}
    {opt range(a [b])} specifies the range of the evaluation grid. The default
    is is to determine the range of the grid from the data; see option
    {cmd:n()}. Argument {it:a} can be specified as {cmd:.} (missing) to select
    the default lower limit; argument {it:b} can be omitted or specified as
    {cmd:.} (missing) to select the default upper limit. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} specifies a custom grid of evaluation points. Options
    {cmd:n()} and {cmd:range()} are not allowed if {cmd:at()} is specified.

{phang}
    {cmd:unconditional} rescales results such that the
    density function integrates to the relative size of the subpopulation
    instead of 1. This is only relevant if option {cmd:over()} has been
    specified.

{marker densopts}{...}
{phang}
    {it:density_options} set the details of density
    estimation. These settings are relevant for command
    {cmd:dstat density}/{cmd:pdf} and statistic {cmd:density()} as well as for
    the computation of influence functions that involve density
    estimation (e.g., the influence function of a quantile). For more information
    on density estimation see {helpb mf_mm_density:mm_density()},
    {browse "http://boris.unibe.ch/69421/2/kdens.pdf":Jann (2007)}, and
    Wand and Jones (1995). The options are as follows:

{phang2}
    {cmdab:bw:idth(}{it:method}[{cmd:,} {opt adj:ust(#)} {cmd:rd}]{cmd:)}
    specifies the type of automatic bandwidth selector for kernel density
    estimation. Possible choices for {it:method} are:

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
    The default is {cmd:bwidth(dpi(2))}. Suboption {opt adjust(#)}, with #>0, can be
    used to adjust the automatic bandwidth by factor {it:#}. Suboption {cmd:rd}
    applies relative-data correction to the automatic bandwidth (Cwik and Mielniczuk 1993).

{phang2}
    {opth bw:idth(numlist)} is an alternative to {opt bwidth(method)} and sets
    the bandwidth to a specific value. If {it:numlist} contains multiple values,
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
    is {cmd:napprox(1024)}.

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
    {opt common} requests that a common set of bin definitions is used across
    all subpopulations. The default is to determine the number of bins and the
    bin boundaries based on the data within subpopulation. If {cmd:common} is
    specified, the bin definitions will be based on the data in the total population.

{phang}
    {opt range(a [b])} specifies the overall range of the histogram bins. The
    default range is from the minimum to the maximum of the data. Argument
    {it:a} can be specified as {cmd:.} (missing) to select
    the default lower limit; argument {it:b} can be omitted or specified as
    {cmd:.} (missing) to select the default upper limit. Option {cmd:range()}
    does not restrict the data that will be used in the computations, it just
    restricts the range of reported histogram bins. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} specifies custom cutpoints for the bins (in ascending
    order). If {it:numlist} contains {it:n} numbers, {it:n}-1 bins will be
    created. Note that the constructed bins will cover all data only if the first
    cutpoint is smaller than or equal to the minimum of the data and the last
    cutpoint is larger than or equal to the maximum ({cmd:dstat} does {it:not} check
    this condition and does not display a warning if the condition is violated). Options
    {cmd:n()}, {cmd:ep}, and {cmd:range()} are not allowed if {cmd:at()} is specified.

{phang}
    {cmd:discrete} treats the data as discrete and estimates the probability of
    each observed level in the data. The option is implemented as a
    redirection to subcommand {cmd:proportion} with option {cmd:nocategorical}. Options
    {cmd:n()} and {cmd:ep} are not allowed together with {cmd:discrete}; the other
    options are as described for {help dstat##prop:subcommand {bf:proportion}}.

{phang}
    {cmd:unconditional} rescales results by the relative size of
    the subpopulation. This is only relevant if option {cmd:over()} has been
    specified. {cmd:unconditional} is not allowed together with {cmd:frequency}.

{marker prop}{...}
{dlgtab:Subcommand proportion}

{phang}
    {opt percent} estimates percent instead of proportions.

{phang}
    {opt frequency} estimates frequencies instead of proportions.

{phang}
    {opt range(a [b])} sets the range of levels for which to estimate
    proportions. The default range is from the minimum to the maximum of the
    data. Argument {it:a} can be specified as {cmd:.} (missing) to select
    the default lower limit; argument {it:b} can be omitted or specified as
    {cmd:.} (missing) to select the default upper limit. Option {cmd:range()}
    does not restrict the data that will be used in the computations, it just
    restricts the range of levels for which results are reported. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of levels for which to estimate
    proportions. The default is to use all levels observed in the data (across
    subpopulations). Only one of {cmd:at()} and {cmd:range()} is allowed.

{phang}
    {opt nocategorical} allows outcome variables that do not comply to
    Stata's rules for factor variables (e.g. variables that contain negative
    or noninteger values). This also affects how the coefficients are
    labeled in the output.

{phang}
    {cmd:unconditional} rescales proportions by the relative size of
    the subpopulation. This is only relevant if option {cmd:over()} has been
    specified. {cmd:unconditional} is not allowed together with {cmd:frequency}.

{marker cdf}{...}
{dlgtab:Subcommands cdf and ccdf}

{phang}
    {opt percent} estimates percent instead of proportions.

{phang}
    {opt frequency} estimates frequencies instead of proportions.

{phang}
    {opt mid} applies midpoint adjustment to the estimated CDF. By default, the
    CDF at evaluation point {it:x} is defined as the proportion of data that is
    lower than or equal to {it:x}. If {cmd:mid} is specified, the CDF at
    point {it:x} is reduced by one half the proportion of data equal to
    {it:x}. {cmd:mid} only has an effect on the results for evaluation points
    that have a match in the data (unless {cmd:ipolate} is specified; see below). Only
    one of {cmd:mid} and {cmd:floor} is allowed.

{phang}
    {opt floor} defines the CDF at evaluation point {it:x} as the proportion
    of data that is lower than {it:x}, rather than lower than or equal to
    {it:x}. {cmd:floor} only has an effect on the results for evaluation points
    that have a match in the data (unless {cmd:ipolate} is specified;
    see below). Only one of {cmd:floor} and {cmd:mid} is allowed.

{phang}
    {opt n(#)} sets the number of points at which the CDF is to be
    evaluated. A regular grid of {it:#} points spanning the
    observed data range (within subpopulation) will be used. The default is
    {cmd:n(99)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opt common} requests that a common set of evaluation points is used across
    all subpopulations. The default is to determine the evaluation points based on
    the data range within subpopulation. If {cmd:common} is specified, the
    evaluation points will be based on the data range in the total population.

{phang}
    {opt range(a [b])} specifies the range of the evaluation grid. The default
    is is to determine the range of the grid from the data; see option
    {cmd:n()}. Argument {it:a} can be specified as {cmd:.} (missing) to select
    the default lower limit; argument {it:b} can be omitted or specified as
    {cmd:.} (missing) to select the default upper limit. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of points at which to evaluate
    the CDF. Options {cmd:n()} and {cmd:range()} are not allowed if {cmd:at()}
    is specified.

{phang}
    {cmd:discrete} treats the data as discrete. In this case, the CDF will
    be estimated at each level observed in the data
    (across all subpopulations). Option {cmd:n()} is not allowed if
    {cmd:discrete} is specified.

{phang}
    {cmd:ipolate} obtains the estimates of the CDF by linearly interpolating
    the values of the empirical CDF. That is, the estimates will lie
    on the curve that linearly connects the points of the CDF if the CDF is
    evaluated at each observed level in the data (within subpopulation; options
    {cmd:mid} and {cmd:floor} have an effect on the location of these
    points). By default, the estimates of the CDF are obtained according to the definitions
    described above (see {cmd:mid} and {cmd:floor}).

{phang}
    {cmd:unconditional} rescales results by the relative size of
    the subpopulation. This is only relevant if option {cmd:over()} has been
    specified. {cmd:unconditional} is not allowed together with {cmd:frequency}.

{marker quantile}{...}
{dlgtab:Subcommand quantile}

{phang}
    {opt n(#)} sets the number of quantiles to be computed. A regular grid
    of {it:#} points from {it:a}+{it:h} to {it:b}-{it:h} will be used,
    with {it:h} = ({it:b}-{it:a})/({it:#}+1) and {it:a} and {it:b}
    as set by option {cmd:range()}. The default is
    {cmd:n(99)}. Only one of {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opt range(a b)} specifies the range of the evaluation grid, with arguments
    {it:a} and {it:b} in [0,1]. The default is {cmd:range(0 1)}. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of probabilities at which to
    compute quantiles. The specified values must be within [0,1]. Options
    {cmd:n()} and {cmd:range()} are not allowed if {cmd:at()} is specified.

{marker qdef}{...}
{phang}
    {cmd:qdef(}{it:method}[{cmd:,} {it:options}]{cmd:)} selects the method
    to be used when computing quantiles. This is relevant for command
    {cmd:dstat quantile} and various statistics in {cmd:dstat summarize}. Argument
    {it:method} can be one of the following.

{p2colset 14 31 33 2}{...}
{p2col:{cmd:0} or {cmdab:hi:gh}}high quantile
    {p_end}
{p2col:{cmd:1} or {cmdab:inv:cdf}}low quantile (inverse of the ECDF);
    can also type {cmdab:lo:w};
    definition 1 in HF96 (Hyndman and Fan 1996)
    {p_end}
{p2col:{cmd:2} or {cmdab:avg:invcdf}}averaged quantile;
    definition 2 in HF96
    {p_end}
{p2col:{cmd:3} or {cmd:closest}}nearest order statistic; definition 3 in HF96
    {p_end}
{p2col:{cmd:4} or {cmdab:parz:en}}interpolation with a=0 and b=1; definition 4 in HF96
    {p_end}
{p2col:{cmd:5} or {cmdab:haz:en}}interpolation with a=1/2 and b=1/2; definition 5 in HF96
    {p_end}
{p2col:{cmd:6} or {cmdab:weib:ull}}interpolation with a=0 and b=0; definition 6 in HF96
    {p_end}
{p2col:{cmd:7} or {cmdab:gumb:el}}interpolation with a=1 and b=1; definition 7 in HF96
    {p_end}
{p2col:{cmd:8} or {cmdab:tuk:ey}}interpolation with a=1/3 and b=1/3; definition 8 in HF96
    {p_end}
{p2col:{cmd:9} or {cmd:blom}}interpolation with a=3/8 and b=3/8; definition 9 in HF96
    {p_end}
{p2colset 13 31 33 2}{...}
{p2col:{cmd:10} or {cmd:hd}}Harrell-Davis quantile (Harrell and Davis 1982);
    the Harrell-Davis estimator typically leads to smoother quantile functions
    than classical quantile definitions; standard errors do not depend on
    density estimation and tend to be more reliable than for other quantile
    definitions if there is heaping in the data
    {p_end}
{p2col:{cmd:11} or {cmd:mid}}mid-quantile by Ma et al. (2011); the mid-quantile
    estimator typically leads to smoother quantile functions than classical
    quantile definitions; Ma et al. (2011) suggest using the mid-quantile
    estimator for discrete data
    {p_end}
{p2col:{cmd:12} or {cmdab:cali:fornia}}interpolation with a=1 and b=0
    {p_end}
{p2col:{cmd:13} or {cmd:beard}}interpolation with a=.31 and b=.31
    {p_end}
{p2col:{cmd:14} or {cmd:benard}}interpolation with a=.3 and b=.3
    {p_end}
{p2col:{cmd:15} or {cmd:cooper}}interpolation with a=.4075 and b=.4075
    {p_end}
{p2col:{cmd:16} or {cmdab:gring:orten}}interpolation with a=.44 and b=.44
    {p_end}

{pmore}
    The default {it:method} is {cmd:avginvcdf}; this is also the method used by
    {helpb summarize}. Methods 4-9 and 12-16 interpoate between points (p_i, X_(i))
    with p_i = (i - a) / (n - a - b + 1) and i = 1,...,n. Also see help
    {helpb mf_mm_quantile:mm_quantile()} for more information on the different
    methods. {it:options} are as follows.

{phang2}
    {opt t:rim}[{cmd:(}{it:width}{cmd:)}] applies trimming to the Harrell-Davis
    quantile estimator as suggested by Akinshin (2021). If {cmd:hdtrim} is specified without
    argument, the width of the evaluation interval is set to 1/sqrt(n), where n
    is the effective sample size. Alternatively, specify a custom {it:width}. Sensible values
    for {it:width} lie between 0 and 1 ({it:width}>=1 uses the untrimmed estimator;
    {it:width}<=0 sets the width to 1/sqrt(n)). Option {cmd:trim()} has an effect only if
    {it:method} is equal to {cmd:10} (or {cmd:hd}).

{phang2}
    {opt us:mooth(#)}, with {it:#}<1, sets the degree of undersmoothing that is
    applied when determining the sparsity function via density estimation for
    the standard errors of mid-quantiles. The default is
    {cmd:usmooth(0.2)}. The undersmoothing factor is computed as n^(1/5) /
    n^(1/(5*(1-#)), where n is the effective sample size. Set # to 0 to omit
    undersmoothing; #<0 leads to oversmoothing. Note that {help
    dstat##densopts:{it:density_options}} have no effect on density estimation
    for mid-quantiles. Option {cmd:usmooth()} has an effect only if
    {it:method} is equal to {cmd:11} (or {cmd:mid}).

{phang2}
    {cmd:cdf}[{cmd:(}{it:#}{cmd:})], with {it:#}>=0, determines the sparsity
    function for the standard errors of mid-quantiles by differencing the ECDF
    instead of employing density estimation. This may lead to somewhat more
    valid results in discrete data (i.e. data with relatively few distinct
    levels), but results may be unreliable in continuous data. Optional
    argument {it:#} sets the width of the integration window that is used to
    interpolate across jumps in the ECDF ({it:#} is on the probability scale;
    for example, a value of 0.01 is equivalent to a window covering 1 percent
    of data mass). The default is {it:#} = 1 / ceil(2 * n^(2/5)), where n is
    the effective sample size. Set {it:#}=0 to omit integration (this
    corresponds to the formulas given in Ma et al. 2011; the sparsity function
    will then have sharp jumps). Option {cmd:cdf()} has an effect only if
    {it:method} is equal to {cmd:11} (or {cmd:mid}).

{marker lorenz}{...}
{dlgtab:Subcommand lorenz}

{phang}
    {opt percent} expresses results in percent instead of
    proportions. {cmd:percent} is not allowed with
    {cmd:generalized} or {cmd:absolute}.

{phang}
    {opt generalized} estimates the generalized Lorenz curve.

{phang}
    {opt sum} estimates the total (unnormalized) Lorenz curve.

{phang}
    {opt gap} estimates the equality gap curve.

{phang}
    {opt absolute} estimates the absolute Lorenz curve.

{phang}
    {opth by(varname)} estimates the concentration curve with respect to
    {it:varname} instead of the Lorenz curve.

{phang}
    {opt n(#)} sets the number of ordinates to be estimated. A regular grid
    of {it:#} values from {it:a} to {it:b} will be used, with {it:a} and {it:b}
    as set by option {cmd:range()}. The default is {cmd:n(101)}. Only one of
    {cmd:n()} and {cmd:at()} is allowed.

{phang}
    {opt range(a b)} specifies the range of the evaluation grid, with arguments
    {it:a} and {it:b} in [0,1]. The default is {cmd:range(0 1)}. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of points at which to estimate
    Lorenz ordinates. The specified values must be within [0,1]. Options
    {cmd:n()} and {cmd:range()} are not allowed if {cmd:at()} is specified.

{marker pshare}{...}
{dlgtab:Subcommand pshare}

{phang}
    {opt proportion} estimates proportions instead of densities.

{phang}
    {opt percent} estimates percent instead of densities.

{phang}
    {opt generalized} estimates generalized shares instead of densities.

{phang}
    {opt sum} estimates totals instead of densities.

{phang}
    {opt average} estimates averages instead of densities.

{phang}
    {opth by(varname)} estimates the concentration shares with respect to
    {it:varname}.

{phang}
    {opt n(#)} sets the number of bins. A regular grid of {it:#} bins between
    {it:a} and {it:b} will be used, with {it:a} and {it:b}
    as set by option {cmd:range()}. The default is {cmd:n(20)}.

{phang}
    {opt range(a b)} specifies the overall range of the reported bins, with
    arguments {it:a} and {it:b} in [0,1]. The default is {cmd:range(0 1)}. Only
    one of {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} specifies custom cutpoints for the bins (in ascending
    order). The specified values must be within [0,1]. If {it:numlist} contains
    {it:n} numbers, {it:n}-1 bins will be created. Note that the constructed
    bins will cover all data only if the first cutpoint is 0 and the last
    cutpoint is 1. Options {cmd:n()} and {cmd:range()} are not allowed if
    {cmd:at()} is specified.

{marker tip}{...}
{dlgtab:Subcommand tip}

{phang}
    {opt pline(#|varname)} specifies the poverty line, either as a single
    value or as a variable containing observation-specific
    values. Option {cmd:pline()} is required.

{phang}
    {opt absolute} estimates the absolute TIP curve. Default is to estimate the
    relative TIP curve.

{phang}
    {opt pstrong} selects the poverty definition to be applied (see Donaldson and
    Weymark 1986). The default is to use the "weak" definition, that is, to treat
    outcomes equal to the poverty line as non-poor. Specify {cmd:pstrong} to treat
    these cases as poor ("strong" definition).

{phang}
    {opt n(#)} sets the number of ordinates to be estimated. A regular grid
    of {it:#} values from {it:a} to {it:b} will be used, with {it:a} and {it:b}
    as set by option {cmd:range()}. The default is {cmd:n(101)}. Only one of {cmd:n()}
    and {cmd:at()} is allowed.

{phang}
    {opt range(a b)} specifies the range of the evaluation grid, with arguments
    {it:a} and {it:b} in [0,1]. The default is {cmd:range(0 1)}. Only one of
    {cmd:range()} and {cmd:at()} is allowed.

{phang}
    {opth at(numlist)} provides a custom list of points at which to estimate
    the TIP ordinates. The specified values must be within [0,1]. Options
    {cmd:n()} and {cmd:range()} are not allowed if {cmd:at()} is specified.

{marker graph_options}{...}
{dlgtab:Graph options}

{phang}
    {cmd:merge} causes results from different equations to be placed
    in a single graph (as separate "plots", i.e. as separate series of results
    displayed in a common style) instead of creating a separate subgraph for
    each equation. This is only relevant if the results contain multiple
    equations and if the equations are one-dimensional
    (e.g. subpopulations); {cmd:merge} has no effect if the
    equations are two-dimensional (subpopulations and variables).

{phang}
    {cmd:overlay} is a synonym for {cmd:merge}.

{phang}
    {cmd:flip} changes how results are allocated to plots and subgraphs. This is
    only relevant if the results contain multiple equations. If the equations
    are two-dimensional (subpopulations and variables), the default is to
    create subgraphs by the secondary dimension (variables) and create
    "plots" (series of results displayed in a common style) within subgraphs by
    the main dimension (subpopulations). Specify {cmd:flip} to reverse this
    behavior. If equations are one-dimensional, {cmd:flip} has the same effect
    as {cmd:merge}.

{marker coefplot}{...}
{phang}
    {it:coefplot_options} are options to be passed through to
    {helpb coefplot}. Use these options, for example, to set titles and axis
    labels or to affect the overall look and size of the graph. The options can
    also be used to change the rendering of the plotted results (e.g. colors,
    line patterns, marker symbols, etc.).

{marker stack}{...}
{phang}
    {opt stack} draws a stacked bar chart; see
    {help dstat##ex_stack:below for some examples}. Option {cmd:stack} is only
    allowed with {cmd:dstat proportion}. The following extra options are
    available if {cmd:stack} is specified.

{phang2}
    {opt diverging} aligns the bars at the scale midpoint. That is, specify
    {cmd:diverging} id you want to draw a diverging stacked bar chart.

{phang2}
    {opt barwidth(#)} sets the width of the bars. Default is {cmd:barwidth(0.8)}.

{phang2}
    {it:barlook_options} are options to change the overall look of the
    bars; see help {it:{help barlook_options}}.

{marker colors}{...}
{phang2}
    {cmd:colors(}{help colorpalette##palette:{it:palette}}
    [{cmd:,} {help colorpalette##opts:{it:palette_options}}]{cmd:)}
    assigns colors to the bar segments. Command {helpb colorpalette} is
    required; type {cmd:ssc install colrspace} and {cmd:ssc install palettes}
    and to make {helpb colorpalette} available.

{phang2}
    {cmd:p}{it:#}{cmd:(}{it:options}{cmd:)} overrides the settings of the #th bar
    segment, where available {it:options} are {opt barw:idth(#)},
    {it:{help barlook_options}}, and {it:{help coefplot##plotopts:plotopts}}.

{phang2}
    {opt values}[{cmd:(}{it:min}{cmd:)}] prints the values of the bar segments
    (proportions, percentages, or frequencies) as marker labels. Specify
    argument {it:min} to omit values that are smaller than {it:min}. Use
    {it:{help marker_label_options}} such as {cmd:mlabcolor()} to change the
    look of the labels.

{marker notstack}{...}
{phang}
    The following extra options are available unless option {cmd:stack} is
    specified.

{phang2}
    [{cmd:g}|{cmd:p}]{cmdab:sel:ect}{cmd:(}{it:{help numlist}}|{cmdab:r:everse}{cmd:)}
    selects and orders subgraphs and plots within
    subgraphs. {it:numlist} specifies the indices of the subgraphs or plots to
    be included. For example, in a situation where the default graph has three
    subgraphs (containing one plot each), you could type {cmd:select(3 1)} to
    omit the 2nd subgraph and reverse the order such that the 3rd subgraph comes
    first. Instead of providing {it:numlist}, type {cmd:select(reverse)}
    to reverse the order of subgraphs or plots.

{pmore2}
    {cmd:select()} applies to both, subgraphs and plots within subgraphs. If a
    graph contains multiple subgraphs and multiple plots within subgraphs, use option
    {cmd:gselect()} to select and order subgraphs, and use option {cmd:pselect()}
    to select and order plots.

{pmore2}
    {cmd:select()}, {cmd:gselect()}, and {cmd:pselect()} only have an effect if
    there are multiple elements to choose from. That is,
    single subgraphs or single plots will always be displayed, irrespective of
    what you type in these options.

{phang2}
    {opt cref} causes results from the reference (sub)population to be
    included in the graph. The default is to suppress these
    results. {cmd:cref} is only relevant if {cmd:over(, contrast())} has been
    specified.

{phang2}
    {cmd:bystats}[{cmd:(}{cmdab:m:ain}|{cmdab:s:econdary}{cmd:)}] treats
    coefficients as equations and equations as coefficients. This is only
    relevant after {cmd:dstat summarize} and only has an effect if the results
    contain multiple equations and if option {cmd:at()} has not been
    specified. The effect of {cmd:bystats} typically is that results are grouped by
    statistics rather than by subpopulations or variables (the option may also
    have the opposite effect depending on how exactly {cmd:dstat} returned its
    results). Optional type {cmd:bystats(main)} (the default) or
    {cmd:bystats(secondary)} to specify wether coefficients should replace the
    main dimension or the secondary dimension of the equations,
    respectively. This is only relevant if the equations contain two dimensions
    (subpopulations and variables).

{phang2}
    [{cmd:no}]{cmd:step} enforces or prevents using a step function to display
    the distribution function. This is only relevant after {cmd:dstat cdf}
    and {cmd:dstat ccdf}. The default is to display the CDF as a step function
    if option {cmd:discrete} (but not {cmd:ipolate}) has been specified, and
    else use straight lines. Specify {cmd:nostep} or {cmd:step}, respectively,
    to override the default.

{phang2}
    {cmd:norefline} suppresses the equality line (diagonal) that is printed
    when plotting results from {cmd:dstat lorenz} (unless option
    {cmd:generalized}, {cmd:gap}, or {cmd:absolute} has been specified).

{phang2}
    {opt refline(line_options)} specifies options to affect the rendition of
    the equality line; see help {it:{help line_options}}. This is only relevant after
    {cmd:dstat lorenz}.

{marker save_options}{...}
{dlgtab:Arguments and options for dstat save}

{pstd}
    Argument {it:elements} specifies the results to be
    saved and, optionally, provides custom names for the target variables. The syntax is

        [{it:name} {cmd:=}] {it:element} [ [{it:name} {cmd:=}] {it:element} ... ]

{pstd}
    where {it:name} specifies the variable name and {it:element} selects the
    result. {it:element} will be used as variable name if {it:name} is
    omitted. For example, type {cmd:coef=b se tstat=t} to store point estimates in variable
    {cmd:coef}, standard errors in variable {cmd:se}, and {it:t} statistics in
    variable {cmd:tstat}. Available elements are as follows:

{p2colset 9 23 25 2}{...}
{p2col:{opt b}}Estimate{p_end}
{p2col:{opt se}}Standard error{p_end}
{p2col:{opt t}}{it:t} or {it:z} statistic; can use {cmd:z} as synonym{p_end}
{p2col:{opt p:value}}p-value{p_end}
{p2col:{opt lb}}Lower bound of confidence interval; can use {cmd:ll}, {cmd:ci_l}, {cmd:ci_ll}, and {cmd:ci_lb} as synonyms{p_end}
{p2col:{opt ub}}Lower bound of confidence interval; can use {cmd:ul}, {cmd:ci_u}, {cmd:ci_ul}, and {cmd:ci_ub} as synonyms{p_end}
{p2col:{opt at}}Evaluation point{p_end}
{p2col:{opt at_ll}}Lower bin limit; can use {cmd:at_l} and {cmd:at_lb} as synonyms{p_end}
{p2col:{opt at_ul}}Upper bin limit; can use {cmd:at_u} and {cmd:at_ub} as synonyms{p_end}
{p2col:{opt at_mid}}Bin midpoint; can use {cmd:at_m} as synonym{p_end}
{p2col:{opt at_h}}Bin width{p_end}
{p2col:{opt id}}Subpopulation ID{p_end}
{p2col:{opt omit}}Omitted coefficient flag{p_end}
{p2col:{opt sumw}}Sum of weights{p_end}
{p2col:{opt n:obs}}Number of observations{p_end}
{p2col:{opt name}}Coefficient name{p_end}
{p2col:{opt eq:name}}Equation name{p_end}
{p2col:{opt eqid}}Equation number; can use {cmd:eqnum} as synonym{p_end}

{pstd}
    Options:

{phang}
    {opt p:refix(str)} specifies a prefix for the names of the generated variables.

{phang}
    {opt r:eplace} allows overwriting existing variables.

{phang}
    {opt wide} stores a separate set of variables for each equation. The
    variables will be named as {it:name}{it:#}, where {it:#} is the equation
    number.

{phang}
    {opt sel:ect(eqlist)} selects the equations to be included in the saved results,
    where {it:eqlist} is a list of equation names. The
    {cmd:*} and {cmd:?} wildcards are allowed in {it:eqlist}. The
    default is to include all equations.

{phang}
    {opt cref} causes the estimates from the reference (sub)population to be
    included in the saved results; this is only relevant if {cmd:over(, contrast())}
    has been specified. The default is to omit these results. {cmd:cref} has no
    effect if {cmd:select()} is specified.

{phang}
    {opt eform} stores results in exponentiated form.

{phang}
    {opt rescale(#)} rescales the stored estimates by factor {it:#}.

{phang}
    {opt l:evel(#)} specifies the confidence level, as a percentage, for
    confidence intervals.

{phang}
    {opt citype(type)} specifies the method for the computation of the
    confidence interval limits; see {help dstat##repopts:above} for available
    methods.

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
    {opt scaling(spec)} determines the scaling of the generated
    influence functions. {it:spec} can be {cmdab:t:otal} (scaling for
    analysis by {helpb total}) or {cmdab:m:ean} (scaling for analysis by
    {helpb mean}). The default is {cmd:scaling(total)} for regular influence
    functions and {cmd:scaling(mean)} for recentered influence functions
    (i.e. if option {cmd:rif} is specified).

{phang}
    {opt compact} generates influence functions in compact form. {cmd:compact}
    only has an effect if {cmd:over()} has been specified and is not allowed
    with {cmd:balance()}, {cmd:unconditional}, {cmd:over(, contrast)}, or
    {cmd:over(, accumulate)}. Furthermore, {cmd:compact} is not supported
    for statistics that are not normalized by the sample size (i.e. frequencies
    or totals).

{pmore}
    The default is to generate one influence function for each single parameter
    estimated by {cmd:dstat}. If {cmd:over()} is specified, this means that
    each statistic in each subpopulation has its own influence
    function. Specify {cmd:compact} to merge the influence functions across
    subpopulations. In this case, {cmd:over()} has to be specified when
    analyzing the influence functions.

{phang}
    {opt quietly} suppresses the list of generated variables that is displayed by
    default.

{pstd}
    Note that weights, if specified, will not be incorporated into the
    influence functions, so that the weights can be
    applied when analyzing the influence functions. The influence functions do,
    however, incorporate the balancing weights (net of base weights)
    from option {cmd:balance()}.

{pstd}
    Furthermore, note that {cmd:dstat} generates scores instead of
    influence functions for statistics that are not normalized by the sample
    size (i.e. frequencies or totals). The difference is that the total of an influence function
    across the estimation sample is zero, whereas the total of the score is
    equal to the statistic in question. Returning scores for frequencies and totals
    ensures that standard errors obtained by {cmd:total} will be correct for these
    statistics in complex survey designs.


{marker examples}{...}
{title:Examples}

        {help dstat##ex_sum:Summary statistics}
        {help dstat##ex_corr:Correlation matrix}
        {help dstat##ex_dist:Distribution functions}
        {help dstat##ex_cov:Covariate balancing}
        {help dstat##ex_inf:Influence functions}
        {help dstat##ex_stack:Stacked bar charts}

{marker ex_sum}{...}
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

{marker ex_corr}{...}
{dlgtab:Correlation matrix}

{pstd}
    {cmd:dstat pw} computes all pairwise associations among a given set of
    variables. In addition to the usual returns, {cmd:dstat pw} stores the
    coefficients in a square matrix called {cmd:e(B)} (and the p-values in
    matrix {cmd:e(P)}) so that the results can be tabulated in compact form or,
    for example, plotted by the {helpb heatplot} command (see
    {net "describe heatplot, from(http://fmwww.bc.edu/repec/bocode/h/)":{bf:ssc describe heatplot}}). Here
    is an example using Kendall's rank correlation:

        . {stata sysuse auto, clear}
{p 8 12 2}
        . {stata dstat pw (taub) price mpg trunk weight length turn foreign, diagonal}
        {p_end}
        . {stata matlist e(B), format(%7.4f)}
{p 8 12 2}
        . {stata heatplot e(B), color(hcl diverging) cuts(-1.05(.1)1.05) aspect(1) legend(subti(Tau b))}
        {p_end}

{marker ex_dist}{...}
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
    {cmd:merge} to overlay the two curves in a single coordinate system:

        . {stata dstat graph, merge}

{pstd}
    To see how the overall wage distribution is composed by the two
    groups, we can, for example, rescale the density estimates by group size
    using option {cmd:unconditional} and include the total density using option
    {cmd:total}:

{p 8 12 2}
        . {stata dstat density wage, over(union) total unconditional ll(0) graph(merge)}
        {p_end}

{marker ex_cov}{...}
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
    the mean difference by about a third (note that there has been a small change
    in the estimation sample due to missing values; for a more valid comparison,
    the raw difference should be computed based on the same sample as the
    balanced difference):

{p 8 12 2}
        . {stata dstat (mean) wage, over(union) balance(grade hours ttl_exp tenure)}
        {p_end}
        . {stata lincom _b[1.union]-_b[0.union]}

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
        . {stata drop wbal}

{pstd}
    The balancing has only been partially successful. Perfect balancing
    (with respect to the means) can be achieved by entropy balancing:

{p 8 12 2}
        . {stata "dstat (mean) wage, over(union) balance(eb: grade hours ttl_exp tenure, generate(wbal))"}
        {p_end}
{p 8 12 2}
        . {stata tabstat grade hours ttl_exp tenure [aw=wbal], by(union)}
        {p_end}
        . {stata drop wbal}

{pstd}
    Note that, instead of using {helpb lincom} after estimation, you can also obtain group
    differences directly using suboption {cmd:contrast} within the {cmd:over()}
    option:

{p 8 12 2}
        . {stata "dstat (mean) wage, over(union, contrast(0)) balance(eb:grade hours ttl_exp tenure)"}
        {p_end}

{marker ex_inf}{...}
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
        . {stata "dstat (mean) wage, over(union) balance(eb: grade hours ttl_exp tenure, reference(1)) rif(RIF0c)"}
        {p_end}
{p 8 12 2}
        . {stata "dstat (mean) wage if e(sample), over(union) rif(RIF0 RIF1)"}
        {p_end}
        . {stata generate difference  = RIF1  - RIF0}
        . {stata generate explained   = RIF0c - RIF0}
        . {stata generate unexplained = RIF1  - RIF0c}
        . {stata mean difference explained unexplained}

{marker ex_stack}{...}
{dlgtab:Stacked bar charts}

{pstd}
    When plotting results from {cmd:dstat proportion} using {cmd:dstat graph},
    specify option {helpb dstat##stack:stack} to creat a stacked bar chart.

        . {stata webuse nhanes2f, clear}
        . {stata separate health, by(sex)}
{p 8 12 2}
        . {stata dstat proportion health?, over(agegrp) percent nocasewise}
        {p_end}
{p 8 12 2}
        . {stata dstat graph, stack vertical flip values(5) mlabcolor(black) colors(cet d03, reverse) bylabels("Mean" "Women")}

{pstd}
    Diverging stacked bar charts can be created by adding option {cmd:diverging}.

{p 8 12 2}
        . {stata dstat graph, stack diverging merge xline(0) colors(cet d03, reverse) coeflabels(health1 = "Men" health2 = "Women") legend(position(6) rows(1))}


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
{synopt:{cmd:e(k_omit)}}number of omitted estimates{p_end}
{synopt:{cmd:e(df_r)}}sample degrees of freedom{p_end}
{synopt:{cmd:e(vce_minus)}}value of k in VCE multiplier n/(n-k){p_end}
{synopt:{cmd:e(qdef)}}quantile definition{p_end}
{synopt:{cmd:e(adaptive)}}number of iterations of adaptive density estimator{p_end}
{synopt:{cmd:e(napprox)}}size of density estimation grid{p_end}
{synopt:{cmd:e(pad)}}padding of density estimation grid{p_end}
{synopt:{cmd:e(ll)}}lower boundary of the data support (density estimation){p_end}
{synopt:{cmd:e(ul)}}upper boundary of the data support (density estimation){p_end}
{synopt:{cmd:e(level)}}confidence level{p_end}

{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:dstat}{p_end}
{synopt:{cmd:e(subcmd)}}{cmd:summarize}, {cmd:pw}, {cmd:density},
    {cmd:histogram}, {cmd:proportion}, {cmd:cdf}, {cmd:ccdf}, {cmd:quantile},
    {cmd:lorenz}, or {cmd:pshare}{p_end}
{synopt:{cmd:e(predict)}}{cmd:dstat predict}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name(s) of analyzed variable(s){p_end}
{synopt:{cmd:e(nocasewise)}}{bf:nocasewise} or empty{p_end}
{synopt:{cmd:e(over)}}name of {it:overvar}{p_end}
{synopt:{cmd:e(over_namelist)}}values of subpopulations{p_end}
{synopt:{cmd:e(over_labels)}}labels of subpopulations{p_end}
{synopt:{cmd:e(over_select)}}values of selected subpopulations{p_end}
{synopt:{cmd:e(over_contrast)}}{cmd:total}, {it:#}, {cmd:lag}, {cmd:lead}, or empty{p_end}
{synopt:{cmd:e(over_ratio)}}{cmd:ratio} or {cmd:lnratio} or empty{p_end}
{synopt:{cmd:e(over_accumulate)}}{cmd:accumulate} or empty{p_end}
{synopt:{cmd:e(over_fixed)}}{cmd:fixed} or empty{p_end}
{synopt:{cmd:e(total)}}{cmd:pooled}, {cmd:mean}, {cmd:wmean}, or empty{p_end}
{synopt:{cmd:e(unconditional)}}{cmd:unconditional} or empty{p_end}
{synopt:{cmd:e(balance)}}list of balancing variables{p_end}
{synopt:{cmd:e(balmethod)}}balancing method{p_end}
{synopt:{cmd:e(balref)}}balancing reference{p_end}
{synopt:{cmd:e(balopts)}}options passed through to balancing procedure{p_end}
{synopt:{cmd:e(bwmethod)}}bandwidth selection as specified in {cmd:bwidth()}{p_end}
{synopt:{cmd:e(kernel)}}kernel as specified in {cmd:kernel()}{p_end}
{synopt:{cmd:e(exact)}}{cmd:exact} or empty{p_end}
{synopt:{cmd:e(boundary)}}boundary correction method{p_end}
{synopt:{cmd:e(qdef_trim)}}{cmd:trim()} as specified in {cmd:qdef()}{p_end}
{synopt:{cmd:e(qdef_usmooth)}}{cmd:usmooth()} as specified in {cmd:qdef()}{p_end}
{synopt:{cmd:e(qdef_cdf)}}{cmd:cdf()} as specified in {cmd:qdef()}{p_end}
{synopt:{cmd:e(csinfo)}}list of keywords providing info on column stripe{p_end}
{synopt:{cmd:e(novalues)}}{cmd:novalues} or empty{p_end}
{synopt:{cmd:e(vformat)}}display format specified in {cmd:vformat()}{p_end}
{synopt:{cmd:e(stats)}}list of (unique) summary statistics{p_end}
{synopt:{cmd:e(slist)}}normalized specification of statistics and variables{p_end}
{synopt:{cmd:e(atvar)}}name of variable specified in {cmd:at()} or empty{p_end}
{synopt:{cmd:e(percent)}}{cmd:percent} or empty{p_end}
{synopt:{cmd:e(proportion)}}{cmd:proportion} or empty{p_end}
{synopt:{cmd:e(frequency)}}{cmd:frequency} or empty{p_end}
{synopt:{cmd:e(mid)}}{cmd:mid} or empty{p_end}
{synopt:{cmd:e(floor)}}{cmd:floor} or empty{p_end}
{synopt:{cmd:e(ipolate)}}{cmd:ipolate} or empty{p_end}
{synopt:{cmd:e(discrete)}}{cmd:discrete} or empty{p_end}
{synopt:{cmd:e(categorical)}}{cmd:categorical} or empty{p_end}
{synopt:{cmd:e(ep)}}{cmd:ep} or empty{p_end}
{synopt:{cmd:e(gap)}}{cmd:gap} or empty{p_end}
{synopt:{cmd:e(generalized)}}{cmd:generalized} or empty{p_end}
{synopt:{cmd:e(absolute)}}{cmd:absolute} or empty{p_end}
{synopt:{cmd:e(average)}}{cmd:average} or empty{p_end}
{synopt:{cmd:e(relax)}}{cmd:relax} or empty{p_end}
{synopt:{cmd:e(byvar)}}name of variable specified in {cmd:by()}{p_end}
{synopt:{cmd:e(pline)}}poverty line variable specified in {cmd:pline()}{p_end}
{synopt:{cmd:e(pstrong)}}{cmd:pstrong} or empty{p_end}
{synopt:{cmd:e(generate)}}name(s) of generated variable(s){p_end}
{synopt:{cmd:e(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:e(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err.{p_end}
{synopt:{cmd:e(citype)}}type confidence interval stored in {cmd:e(ci)}{p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(properties)}}{cmd:b} or {cmd:b V}{p_end}

{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}estimates{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of estimates{p_end}
{synopt:{cmd:e(se)}}standard errors of estimates{p_end}
{synopt:{cmd:e(ci)}}confidence intervals of estimates{p_end}
{synopt:{cmd:e(nobs)}}number of observations per estimate{p_end}
{synopt:{cmd:e(sumw)}}sum of weights per estimate{p_end}
{synopt:{cmd:e(at)}}evaluation points of distribution function{p_end}
{synopt:{cmd:e(omit)}}indicator for omitted estimates{p_end}
{synopt:{cmd:e(id)}}subpopulation IDs of estimates{p_end}
{synopt:{cmd:e(cref)}}contrast reference indicators{p_end}
{synopt:{cmd:e(bwidth)}}kernel bandwidth(s) of density estimation{p_end}
{synopt:{cmd:e(_N)}}number of observations by subpopulation{p_end}
{synopt:{cmd:e(_W)}}sum of weights by subpopulation{p_end}

{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}estimation sample{p_end}

{pstd}{cmd:dstat pw} additionally stores the following square matrices.

{synopt:{cmd:e(B)}}estimates{p_end}
{synopt:{cmd:e(Nobs)}}number of observations per estimate{p_end}
{synopt:{cmd:e(P)}}two-sided p-values{p_end}

{pstd}
    If {cmd:vce()} is {cmd:svy}, {cmd:bootstrap}, or {cmd:jackknife}, additional
    results are stored in {cmd:e()}; see {helpb svy}, {helpb bootstrap}, and
    {helpb jackknife}, respectively.

{pstd}
   {cmd:dstat save} stores the following results in {cmd:r()}.

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:r(k)}}number of equations (only if {cmd:wide}){p_end}

{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:r(names)}}names of generated variables{p_end}
{synopt:{cmd:r(elements)}}names of stored results{p_end}
{synopt:{cmd:r(wide)}}{cmd:wide} or empty{p_end}
{synopt:{cmd:r(eq}{it:#}{cmd:)}}{it:#}th equation name (only if {cmd:wide}){p_end}
{synopt:{cmd:r(names}{it:#}{cmd:)}}names of variables used for {it:#}th equation (only if {cmd:wide}){p_end}


{marker references}{...}
{title:References}

{phang}
    Akinshin, A. (2021). Trimmed Harrell-Davis quantile estimator based on the
    highest density interval of the given
    width. {browse "http://arxiv.org/abs/2111.11776":arXiv:2111.11776} [stat.ME].
    {p_end}
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
    Clark, S., R. Hemming, D. Ulph (1981). On Indices for the Measurement of Poverty. The
    Economic Journal 91(362): 515-526
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
    Donaldson, D., J.A. Weymark (1986). Properties of Fixed-Population Poverty Indices. International
    Economic Review 27(3): 667-688.
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
    Harrell, F.E., C.E. Davis (1982). A New Distribution-Free Quantile Estimator. Biometrika
    69: 635-640.
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
    Ma, Y., M.G. Genton, E. Parzen (2011). Asymptotic properties of sample
    quantiles of discrete distributions. Annals of the Institute of Statistical
    Mathematics 63:227–243.
    {p_end}
{phang}
    Newson, R. (2006). Efficient Calculation of Jackknife Confidence
    Intervals for Rank Statistics. Journal of Statistical Software 15(1).
    {p_end}
{phang}
    Osberg, L., K. Xu (2008). How Should We Measure Poverty in a Changing World? Methodological
    Issues and Chinese Case Study. Review of Development Economics 12(2): 419–441.
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
    Sen, A. (1976). Poverty: An Ordinal Approach to Measurement. Econometrica 44(2): 219-231.
    {p_end}
{phang}
    Shorrocks, A.F. (1980). The Class of Additively Decomposable Inequality Measures. Econometrica 48(3): 613-625.
    {p_end}
{phang}
    Shorrocks, A.F. (1995). Revisiting the Sen Poverty Index. Econometrica 63(5): 1225-1230.
    {p_end}
{phang}
    Takayama, N. (1979). Poverty, income inequality, and their measures: Professor Sen's
    axiomatic approach reconsidered. Econometrica 47(3): 747-759.
    {p_end}
{phang}
    Wand, M.P., M.C. Jones (1995). Kernel Smoothing. London: Chapman and Hall.
    {p_end}


{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2020). dstat: Stata module to compute summary statistics and
    distribution functions including standard errors
    and optional covariate balancing. Available from
    {browse "http://ideas.repec.org/c/boc/bocode/s458874.html"}.


{marker also_see}{...}
{title:Also see}

{psee}
    Online: help for
    {helpb twoway dstat},
    {helpb centile},
    {helpb ci},
    {helpb correlate},
    {helpb cumul},
    {helpb histogram},
    {helpb kdensity},
    {helpb mean},
    {helpb pctile},
    {helpb proportion},
    {helpb spearman},
    {helpb summarize},
    {helpb table},
    {helpb tabstat},
    {helpb tabulate},
    {helpb teffects ipw},
    {helpb total}

{psee}
    Packages from the SSC Archive (type {cmd:ssc describe} {it:name} for
    more information):
    {helpb akdensity},
    {helpb apoverty},
    {helpb catplot},
    {helpb cdfplot},
    {helpb ci2},
    {helpb coefplot},
    {helpb dfl},
    {helpb distplot},
    {helpb duncan},
    {helpb eqprhistogram},
    {helpb fre},
    {helpb glcurve},
    {helpb heatplot},
    {helpb ineqdeco},
    {helpb kdens},
    {helpb kmatch},
    {helpb lorenz},
    {helpb moremata},
    {helpb povdeco},
    {helpb poverty},
    {helpb pshare},
    {helpb reldist},
    {helpb rif},
    {helpb robstat},
    {helpb seg},
    {helpb somersd},
    {helpb sumdist},
    {helpb svygei:svygei_svyatk},
    {helpb svylorenz}

