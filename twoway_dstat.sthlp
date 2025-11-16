{smcl}
{* 07nov2025}{...}
{vieweralsosee "[G-2] graph twoway" "help twoway"}{...}
{vieweralsosee "dstat" "help dstat"}{...}
{viewerjumpto "Syntax" "twoway_dstat##syntax"}{...}
{viewerjumpto "Description" "twoway_dstat##description"}{...}
{viewerjumpto "Options" "twoway_dstat##options"}{...}
{viewerjumpto "Examples" "twoway_dstat##examples"}{...}
{viewerjumpto "Author" "twoway_dstat##author"}{...}
{viewerjumpto "Also see" "twoway_dstat##also_see"}{...}
{hline}
help for {hi:twoway dstat}{...}
{right:{browse "http://github.com/benjann/dstat/"}}
{hline}

{title:Title}

{pstd}{hi:twoway dstat} {hline 2} Plot distribution estimates from {helpb dstat} in
{helpb graph_twoway:graph twoway}


{marker syntax}{...}
{title:Syntax}

{pstd}
    Syntax 1 (computing results on the fly)

{p 8 13 2}
[{cmdab:gr:aph}] {cmdab:tw:oway} {cmd:dstat} [{cmd:ci}] {it:subcmd}
    {varlist} {ifin} {weight}
    [{cmd:,} {help twoway_dstat##opts:{it:options}} ]

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
{p2col:{opt sh:are}}percentile shares{p_end}
{p2col:{opt tip}}TIP curve{p_end}

{pmore}
    {cmd:fweight}s, {cmd:pweight}s, {cmd:iweight}, and {cmd:aweight}s are allowed; see {help weight}.

{pstd}
    Syntax 2 (using stored results)

{p 8 13 2}
[{cmdab:gr:aph}] {cmdab:tw:oway} {cmd:dstat} [{cmd:ci}] {it:name}
    [{cmd:,} {help twoway_dstat##opts:{it:options}} ]

{pmore2}
    where {it:name} is a set of results from {bind:{cmd:dstat} {it:subcmd}}
    that has been stored by {helpb estimates store}. {it:name}
    may also be {cmd:.} for the current (active) estimates.

{pstd}
    In case of conflict, {it:subcmd} takes precedence over {it:name}. That is,
    Syntax 1 takes precedence over Syntax 2.


{synoptset 26 tabbed}{...}
{marker opts}{col 5}{help twoway_dstat##options:{it:options}}{col 33}Description
{synoptline}
{syntab:Syntax 1 only}
{synopt:{it:{help dstat##opts:dstat_options}}}any options allowed by {helpb dstat},
    depending on {it:subcmd}, except {cmd:generate()} and {cmd:rif()}
    {p_end}

{syntab:Main}
{synopt:{opt lab:els(textlist)}}label(s) to be used in legend
    {p_end}
{synopt:{opt sel:ect(eqlist)}}select results to be plotted
    {p_end}
{synopt:[{cmd:no}]{cmd:step}}do/do not use step function; only allowed with
    {cmd:cdf} and {cmd:ccdf}
    {p_end}
{synopt:{cmd:line}}use line plot rather than bar plot; only allowed with
    {cmd:histogram} and {cmd:pshare}
    {p_end}
{synopt:{opt hor:izontal}}flip plot orientation
    {p_end}
{synopt:{opt rescale(#)}}rescale plotted values by factor {it:#}
    {p_end}
{synopt:{opt area(#)}}synonym for {cmd:rescale()}
    {p_end}

{syntab:Rendering}
{synopt:{it:{help marker_options}}}change look of markers
    {p_end}
{synopt:{it:{help cline_options}}}change look of lines
    {p_end}
{synopt:{it:{help barlook_options}}}change look of bars
    {p_end}
{synopt:{it:{help area_options}}}change look of shaded areas
    {p_end}
{synopt:{it:{help axis_choice_options}}}associate plot with alternate axis
    {p_end}
{synopt:{it:{help twoway_options}}}general options for two-way graphs
    {p_end}
{synoptline}


{marker description}{...}
{title:Description}

{pstd}
    {cmd:graph twoway dstat} plots distribution estimates computed by {helpb dstat}
    using {helpb graph twoway line} or {helpb graph twoway bar}.

{pstd}
    {cmd:graph twoway dstat ci} plots corresponding confidence intervals using
    {helpb graph twoway rarea} or {helpb graph twoway rcap}.

{pstd}
    {cmd:graph twoway dstat} requires Stata 16 or newer.


{marker options}{...}
{title:Options}

{phang}
    {it:{help dstat##opts:dstat_options}} are any options allowed by
    {helpb dstat}, except {cmd:generate()} and {cmd:rif()}. The set of available
    options depends on {it:subcmd}. {it:dstat_options} are only allowed in
    Syntax 1.

{phang}
    {opt labels(textlist)} specifies custom legend labels for the plotted variables or
    subpopulations, where {it:textlist} is

            {cmd:"}{it:text}{cmd:"} [{cmd:"}{it:text}{cmd:"} ...]

{pmore}
    This may also affect the label that is printed on the y-axis.

{phang}
    {opt select(eqlist)} selects the equations (variables or subpopulations)
    to be included in the plotted results, where {it:eqlist} is a list of
    equation names. The {cmd:*} and {cmd:?} wildcards are allowed in
    {it:eqlist}. The default is to plot all available results.

{phang}
    [{cmd:no}]{cmd:step}, if specified with subcommand {cmd:cdf} or {cmd:ccdf},
    enforces or prevents using a step function to display the CDF. The default
    is to use a step function if option {helpb dstat##cdf:discrete} (but not
    {cmd:ipolate}) is specified, and else use straight lines. Specify
    {cmd:nostep} or {cmd:step}, respectively, to override this default behavior.

{phang}
    {cmd:line}, if specified with subcommand {cmd:histogram} or {cmd:pshare},
    prints a line tracing the top of the bars, rather than printing
    bars. CIs will be printed as areas rather than capped spikes.

{phang}
    {opt horizontal} flips the axes.

{phang}
    {opt rescale(#)} multiplies all estimates by {it:#}.

{phang}
    {opt area(#)} is a synonym for {cmd:rescale()}.

{phang}
    {it:{help marker_options}} are options affecting the look of the plotted
    markers.

{phang}
    {it:{help cline_options}} are options affecting the look of the plotted lines.

{phang}
    {it:{help barlook_options}} are options affecting the look of the plotted bars

{phang}
    {it:{help area_options}} are options affecting the look of the plotted areas.

{phang}
    {it:{help axis_choice_options}} are options for specifying the axes on which
    the plot appears.

{phang}
    {it:{help twoway_options}} are general options for two-way graphs.


{marker examples}{...}
{title:Examples}

{pstd}
    Distribution of a single variable:

        . {stata sysuse nlsw88, clear}
        . {stata twoway dstat pdf tenure, ll(0)}

{pstd}
    Distributions of multiple variables:

        . {stata twoway dstat pdf tenure ttl_exp wage, ll(0)}

{pstd}
    Distribution of a single variable in multiple subpopulations:

        . {stata twoway dstat pdf tenure, ll(0) over(union)}

{pstd}
    Distributions of multiple variables in multiple subpopulations:

        . {stata twoway dstat pdf tenure ttl_exp wage, ll(0) over(union)}

{pstd}
    Use {it:{help stylelists}} to affect the rendering in case of multiple variables
    or subpopulations:

{p 8 12 2}
    . {stata twoway dstat pdf tenure ttl_exp wage, ll(0) lpattern(l dash dash_dot) lwidth(medthick ..)}

{p 8 12 2}
    . {stata twoway dstat pdf tenure, ll(0) over(union) recast(area) fcolor(%50 ..) lcolor(%0 ..)}

{pstd}
    Add confidence intervals in the back or on top, depending on context:

{p 8 12 2}
    . {stata twoway dstat ci pdf tenure, ll(0) || dstat pdf tenure, ll(0)}

{p 8 12 2}
    . {stata twoway dstat histogram tenure, lalign(center) || dstat ci histogram tenure}

{pstd}
    Combine different types of plots:

{p 8 12 2}
    . {stata twoway dstat histogram tenure, lalign(center) || dstat pdf tenure, ll(0)}

{pstd}
    Alternatively, use {cmd:()}-binding rather than {cmd:||}-separator notation;
    see Syntax in {helpb graph_twoway##remarks2:graph twoway}:

{p 8 12 2}
    . {stata twoway (dstat histogram tenure, lalign(center)) (dstat pdf tenure, ll(0))}

{pstd}
    Combine different types of plots on separate axes:

{p 8 12 2}
    . {stata twoway (dstat pdf tenure, ll(0)) (dstat cdf tenure, yaxis(2))}

{pstd}
    Use with {cmd:by()}:

{p 8 12 2}
    . {stata twoway (dstat histogram tenure, color(*.5)) (dstat pdf tenure, ll(0)), by(union, legend(off))}

{pstd}
    Plot distributional differences between subpopulations using {cmd:over(, contrast)}:

{p 8 12 2}
    . {stata twoway (dstat ci quantile wage, over(union, contrast)) (dstat quantile wage, over(union, contrast)), legend(off) yline(0) ytitle(Difference in quantiles)}

{p 8 12 2}
    . {stata twoway (dstat hist wage, over(union, contrast)) (dstat pdf wage, ll(0) over(union, contrast)), legend(off) yline(0) ytitle(Density difference)}

{p 8 12 2}
    . {stata twoway (dstat pshare wage, over(union, contrast)) (dstat ci pshare wage, ll(0) over(union, contrast)), legend(off) yline(0) ytitle(Difference in percentile shares)}

{pstd}
    Using {cmd:twoway dstat} with stored results:

        . {stata sysuse nlsw88, clear}
        . {stata dstat hist tenure ttl_exp wage}
{p 8 12 2}
        . {stata twoway dstat ., fcolor(%50 ..) lcolor(%0 ..) label(tenure experience wage)}

        . {stata dstat pdf tenure, ll(0)}
        . {stata estimates store PDF}
        . {stata dstat cdf tenure}
        . {stata estimates store CDF}
        . {stata twoway (dstat ci PDF) (dstat PDF) (dstat CDF, yaxis(2))}


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
    {helpb graph twoway},
    {helpb dstat}

