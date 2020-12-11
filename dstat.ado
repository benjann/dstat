*! version 1.0.8  11dec2020  Ben Jann

capt findfile lmoremata.mlib
if _rc {
    di as error "-moremata- is required; type {stata ssc install moremata}"
    error 499
}

program dstat, eclass properties(svyb svyj)
    version 14
    if replay() {
        Replay `0'
        exit
    }
    gettoken subcmd 00 : 0, parse(", ")
    if `"`subcmd'"'==substr("graph",1,max(2,strlen(`"`subcmd'"'))) {
        Graph `00'
        exit
    }
    if `"`subcmd'"'=="predict" {
        Predict `00'
        exit
    }
    if substr(`"`subcmd'"',1,1)=="(" {  // omitted summarize
        local 00 `0'
        local subcmd summarize
    }
    local version : di "version " string(_caller()) ":"
    capt Parse_subcmd `subcmd' // expands subcmd
    if _rc==1 exit _rc
    if _rc { // try summarize
        local 00 `"`subcmd' `00'"'
        local subcmd summarize
    }
    Get_diopts `subcmd' `00' // returns 00, diopts, dioptshaslevel
    tempname BW AT
    Check_vce "`BW' `AT'" `subcmd' `00'
    if "`vcetype'"=="svyr" {
        if `"`svylevel'"'!="" {
            if `dioptshaslevel'==0 {
                local diopts `svylevel' `diopts'
            }
        }
        `version' dstat_SVYR `"`svyopts'"' `subcmd' `00'
        if "`vcenocov'"!="" Remove_Cov
    }
    else if "`vcetype'"=="svy" {
        `version' svy `svytype', noheader notable `svyopts': dstat `subcmd' `00'
        if "`vcenocov'"!="" Remove_Cov
    }
    else if "`vcetype'"!="" { // bootstrap/jackknife
        `version' _vce_parserun dstat, noeqlist wtypes(pw iw) ///
            bootopts(noheader notable force) ///
            jkopts(noheader notable force) : `subcmd' `00'
        if "`vcenocov'"!="" Remove_Cov
    }
    else {
        if c(stata_version)<15 {
            // active e()-returns can cause confusion of Mata views in some
            // special cases in Stata 14; I only observed this when applying 
            // balance(ipw:) while results from balance(eb:) are in memory; it
            // is puzzling where the problem comes from, but clearing e() solves
            // it; the problem does not seem to occur in Stata 15 or newer 
            tempname ecurrent
            _estimates hold `ecurrent', restore nullok
        }
        Estimate `subcmd' `00'
        if c(stata_version)<15 {
            _estimates unhold `ecurrent', not
        }
    }
    eret local cmdline `"dstat `0'"'
    Set_CI, `diopts'
    Replay, `diopts'
    if `"`e(generate)'"'!="" {
        if `"`generate_quietly'"'=="" {
            describe `e(generate)'
        }
    }
end

program Parse_subcmd
    if `"`0'"'==substr("density",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd density
        exit
    }
    if `"`0'"'==substr("histogram",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd histogram
        exit
    }
    if `"`0'"'==substr("cdf",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd cdf
        exit
    }
    if `"`0'"'==substr("ccdf",1,max(2,strlen(`"`0'"'))) {
        c_local subcmd ccdf
        exit
    }
    if `"`0'"'==substr("proportion",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd proportion
        exit
    }
    if `"`0'"'==substr("quantile",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd quantile
        exit
    }
    if `"`0'"'==substr("lorenz",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd lorenz
        exit
    }
    if `"`0'"'==substr("share",1,max(2,strlen(`"`0'"'))) {
        c_local subcmd share
        exit
    }
    if `"`0'"'==substr("summarize",1,max(2,strlen(`"`0'"'))) {
        c_local subcmd summarize
        exit
    }
    if `"`0'"'!="" {
        di as err `"invalid subcommand: `0'"'
        exit 198
    }
    di as err `"subcommand required"'
    exit 198
end

program Get_diopts
    gettoken subcmd 0 : 0
    _parse comma lhs 0 : 0
    syntax [, Level(passthru) citype(passthru) noHEADer NOTABle TABle ///
        GRaph GRaph2(passthru) NOCI * ]
    if "`noci'"!="" {
        di as err "option {bf:noci} not allowed"
        exit 198
    }
    _get_diopts diopts options, `options'
    local options `level' `options'
    if `"`options'"'!="" local lhs `lhs', `options'
    c_local diopts `diopts' `level' `citype' `header' `notable' `table' `graph' `graph2'
    c_local dioptshaslevel = `"`level'"'!=""
    c_local 00 `lhs'
end

program Check_vce
    gettoken BWAT 0 : 0
    gettoken subcmd 0 : 0
    _parse comma lhs 0 : 0
    syntax [, vce(str) NOSE * ]
    if `"`vce'"'=="" exit
    Parse_vceopt `vce' // returns vcetype, vcevars, svytype, svyopts, level, cov, nocov
    if "`vcetype'"=="" exit // no prefix command
    // cov/nocov
    if "`cov'"!="" & "`nocov'"!="" {
        di as err "{bf:vce()}: only one of {bf:cov} and {bf:nocov} allowed"
        exit 198
    }
    if `"`subcmd'"'!="summarize" & "`cov'"=="" local nocov nocov
    c_local vcenocov `nocov'
    // svy linearized
    if "`vcetype'"=="svyr" {
        c_local 00 `lhs', nose `options'
        c_local svyopts `svyopts'
        c_local svylevel `svylevel'
        c_local vcetype svyr
        exit
    }
    // check for options that are not allowed with replication techniques
    local 0 `", `options'"'
    syntax [, Generate(passthru) BWidth(passthru) BWADJust(passthru) ///
        n(passthru) at(passthru) BALance(passthru) * ]
    local options `balance' `options'
    if `"`generate'"'!="" {
        local vcetype `vcetype' `svytype'
        di as err `"option {bf:generate()} not allowed with {bf:vce(`vcetype')}"'
        exit 198
    }
    if `"`balance'"'!="" {
        capt Check_vce_balance_gen, `balance'
        if _rc==1 exit _rc
        if _rc {
            di as err `"option {bf:generate()} not allowed in {bf:balance()} with {bf:vce(`vcetype')}"'
            exit 498
        }
    }
    // obtain bandwidth and evaluation grid; may replace bwidth and at; may
    // clear bwadj and n
    Obtain_bwat "`BWAT'" `subcmd' `lhs', `bwidth' `bwadjust' `n' `at' nose `options' ///
            _vcevars(`vcevars') _vcetype(`vcetype') _svysubpop(`svysubpop')
    local options `bwidth' `bwadjust' `n' `at' `options'
    // svy
    if "`vcetype'"=="svy" {
        c_local 00 `lhs', nose `options'
        c_local svyopts `"`svyopts'"'
        c_local svytype `"`svytype'"'
        c_local vcetype `vcetype'
        exit
    }
    // bootstrap and jackknife
    c_local 00 `lhs', vce(`vce') nose `options'
    c_local vcetype `vcetype'
end

program Check_vce_balance_gen
    syntax [, BALance(str) ]
    _parse comma lhs 0 : balance
    syntax [, GENerate(str) * ]
    if `"`generate'"'!="" exit 198
end

program Parse_vceopt
    // handle cov/nocov
    _parse comma vcetype 0 : 0
    syntax [, NOCOV COV * ]
    if `"`options'"'!="" {
        local 0 `", `options'"'
    }
    else local 0 ""
    if "`cov'`nocov'"!="" { // return vce() with cov/nocov removed
        c_local vce `"`vcetype'`0'"'
    }
    c_local cov `cov'
    c_local nocov `nocov'
    // split vcetype [args]
    gettoken vcetype vcearg : vcetype
    mata: st_local("vcearg", strtrim(st_local("vcearg")))
    // vce(svy)
    if `"`vcetype'"'=="svy" {
        qui svyset
        if `"`r(settings)'"'==", clear" {
             di as err "data not set up for svy, use {helpb svyset}"
             exit 119
        }
        if `"`vcearg'"'=="" local vcearg `"`r(vce)'"'
        if `"`vcearg'"'== substr("linearized",1,max(3,strlen(`"`vcearg'"'))) {
            syntax [, Level(passthru) * ] 
            c_local svyopts `level' `options'
            c_local svylevel `level' // must pass level through to diopts
            c_local vcetype svyr
            exit
        }
        syntax [, SUBpop(passthru) * ]
        c_local svyopts `subpop' `options'
        c_local svysubpop `subpop'
        c_local svytype `"`vcearg'"'
        c_local vcetype svy
        exit
    }
    // vce(boot)
    if `"`vcetype'"'== substr("bootstrap",1,max(4,strlen(`"`vcetype'"'))) {
        if `"`vcearg'"'!="" {
            di as err `"'`vcearg'' not allowed"'
            di as err "error in option {bf:vce()}"
            exit 198
        }
        syntax [, STRata(varlist) CLuster(varlist) group(varname) JACKknifeopts(str) * ]
        Parse_vceopt_jack, `jackknifeopts'  // returns vcevars
        c_local vcevars `vcevars' `strata' `cluster' `group'
        c_local vcetype bootstrap
        exit
    }
    // vce(jack)
    if `"`vcetype'"'== substr("jackknife",1,max(4,strlen(`"`vcetype'"'))) {
        if `"`vcearg'"'!="" {
            di as err `"'`vcearg'' not allowed"'
            di as err "error in option {bf:vce()}"
            exit 198
        }
        Parse_vceopt_jack `0'  // returns vcevars
        c_local vcevars `vcevars'
        c_local vcetype jackknife
        exit
    }
end

program Parse_vceopt_jack
    syntax [, CLuster(varlist) * ]
    c_local vcevars `cluster'
end

program Obtain_bwat   // returns bwidth, bwadjust, n, at
    gettoken BWAT   0 : 0
    gettoken subcmd 0 : 0
    if "`subcmd'"=="quantile" exit
    if "`subcmd'"=="lorenz" exit
    if "`subcmd'"=="share" exit
    if "`subcmd'"=="summarize" local bwopt bwidth(str)
    else {
        if "`subcmd'"=="density" local bwopt bwidth(str)
        local atopt n(passthru) at(passthru)
    }
    syntax [anything] [if] [in] [fw iw pw] [, ///
        _vcevars(str) _vcetype(str) _svysubpop(str) ///
        `bwopt' `atopt' * ]
    local getat 0
    if "`atopt'"!="" {
        if `"`at'"'=="" local getat 1
        local options `n' `at' `options'
    }
    local getbw 0
    if "`bwopt'"!="" {
        local getbw 1
        if `"`bwidth'"'!="" {
            capt numlist `"`bwidth'"', range(>0)
            if _rc==1 exit _rc
            if _rc==0 {
                if `getat'==0 exit
                local getbw 0
            }
            else {
                capt confirm matrix `bwidth'
                if _rc==1 exit _rc
                if _rc==0 {
                    if `getat'==0 exit
                    local getbw 0
                }
            }
            local options bwidth(`bwidth') `options'
        }
    }
    if `getbw' {
        if "`subcmd'"=="summarize" {
            Parse_sum_hasdens `anything'
            if "`hasdens'"=="" local getbw 0
        }
    }
    if `getat'==0 & `getbw'==0 exit
    gettoken BW BWAT : BWAT
    gettoken AT      : BWAT
    if `getat' & `getbw' local tmp "evaluation grid and bandwidth"
    else if `getbw'      local tmp "bandwidth"
    else                 local tmp "evaluation grid"
    di as txt "(running {bf:dstat} to obtain `tmp')"
    marksample touse
    if `"`_vcetype'"'=="svy" {
        if `"`weight'"'!="" {
            di as err "weights not allowed with {bf:vce(svy)}"
            exit 198
        }
        tempvar svysub wvar
        _svy_setup `touse' `svysub' `wvar', svy `_svysubpop'
        qui replace `touse' = 0 if `svysub'==0
        local wgt [pw = `wvar']
    }
    else {
        loca wgt [`weight'`exp']
        markout `touse' `_vcevars', strok
    }
    qui Estimate `subcmd' `anything' if `touse' `wgt', `options'
    if `getat' { 
        matrix `AT' = e(at)
        c_local at at(`AT')
        c_local n
    }
    if `getbw' { 
        matrix `BW' = e(bwidth)
        c_local bwidth bwidth(`BW')
        c_local bwadjust
    }
end

program dstat_SVYR, eclass
    local version : di "version " string(_caller()) ":"
    gettoken svyopts cmdline : 0
    nobreak {
        _svyset get strata 1
        if `"`r(strata1)'"'=="" {
            // must set strata so that _robust (which is called by svy) does not 
            // assume the mean of the scores variables to be zero; this is relevant
            // for unnormalized statistics (totals, frequencies)
            tempname cons
            quietly gen byte `cons' = 1
            _svyset set strata 1 `cons'
        }
        capture noisily break {
            `version' svy linearized, noheader notable `svyopts': dstat_svyr `cmdline'
        }
        local rc = _rc
        if `"`cons'"'!="" {
            _svyset clear strata 1
            eret local strata1 ""
        }
        if `rc' exit `rc'
    }
    tempname b
    mat `b' = e(b)
    mata: dstat_svylbl_b_undo() // returns k_eq
    ereturn repost b=`b', rename
    eret scalar k_eq = `k_eq'
    eret local cmd "dstat"
    eret local cmdname ""
    eret local command
    eret local V_modelbased "" // remove matrix e(V_modelbased)
end

program Remove_Cov, eclass
    // type of estimates
    local issvy = (`"`e(prefix)'"'=="svy") // else: bootstrap, jackknife
    // matrices
    local tmp b V
    if `issvy' local tmp `tmp' V_srs V_srssub V_srswr V_srssubwr V_msp
    local emat: e(matrices)
    local emat: list emat - tmp
    foreach e of local emat {
        tempname emat_`e'
        matrix `emat_`e'' = e(`e')
    }
    // scalars
    local tmp N
    local esca: e(scalars)
    local esca: list esca - tmp
    foreach e of local esca {
        tempname esca_`e'
        scalar `esca_`e'' = e(`e')
    }
    // locals
    local tmp wtype wexp properties
    local eloc: e(macros)
    local eloc: list eloc - tmp
    foreach e of local eloc {
        local eloc_`e' `"`e(`e')'"'
    }
    // compute SEs (svy only)
    if `issvy' {
        tempname se
        matrix `se' = vecdiag(e(V))
        mata: st_replacematrix("`se'", sqrt(st_matrix("`se'")))
    }
    // post main results
    tempvar sample
    gen byte `sample' = e(sample)
    tempname b
    matrix `b' = e(b)
    if `"`e(wtype)'"'!="" {
        local wgt `"[`e(wtype)' `e(wexp)']"'
    }
    local N = e(N)
    eret post `b' `wgt', obs(`N') esample(`sample')
    if `issvy' {
        eret matrix se = `se'
    }
    // scalars
    foreach e of local esca {
        eret scalar `e' = scalar(`esca_`e'')
    }
    // locals
    foreach e of local eloc {
        eret local `e' `"`eloc_`e''"'
    }
    // matrices
    foreach e of local emat {
        eret matrix `e' = `emat_`e''
    }
end

program Predict
    // ignores types; always stores variables as double
    // by default, IFs will be set only within e(sample); when -if/in- is 
    // specified, IFs will be set within the scope of -if/in- (i.e. IFs will be
    // set to zero for obs outside e(sample) but within -if/in-)
    tempname b
    if `"`e(cmd)'"'=="dstat_svyr" {
        local svypfx svy
        mat `b' = e(b)
    }
    else if `"`e(cmd)'"'=="dstat" {
        mat `b' = e(b)
        mata: dstat_svylbl_b()
        local bmat b(`b')
    }
    else {
        di as err "last dstat results not found"
        exit 301
    }
    syntax [anything] [if] [in], [ SCores SVY RIF COMpact QUIetly ]
    if "`svypfx'"!="" local svy svy // enforce svy
    _score_spec `anything', scores `bmat'
    local vlist `s(varlist)'
    Predict_compute_IFs, generate(`vlist', `svy' `rif' `compact') // updates vlist
    if `"`if'`in'"'!="" {
        tempvar tmp
        foreach v of local vlist {
            qui gen double `tmp' = cond(`v'<., `v', 0) `if' `in'
            drop `v'
            rename `tmp' `v'
        }
    }
    if "`quietly'"=="" {
        describe `vlist'
    }
end

program Predict_compute_IFs
    syntax [, generate(passthru) ]
    // check subcommand
    local subcmd `e(subcmd)'
    Parse_subcmd `subcmd'
    
    // determine estimation sample
    tempvar touse
    qui gen byte `touse' = e(sample)==1
    if `"`e(subpop)'"'!="" {
        // restrict estimation sample to subpop; only relevant after survey estimation
        tempvar touse0
        qui gen byte `touse0' = `touse'
        local 0 `"`e(subpop)'"'
        syntax [varname(default=none)] [if]
        if `"`varlist'"'!="" {
            qui replace `touse' = `touse' & `varlist'!=0 & `varlist'<.
        }
        if `"`if'"'!="" {
            tempname tmp
            rename `touse' `tmp'
            qui gen byte `touse' = 0
            qui replace `touse' = `tmp' `if'
            drop `tmp'
        }
    }
    qui count if `touse'
    if r(N)==0 {
        di as err "could not identify estimation sample; computation of influence functions failed"
        exit 498
    }
    // compile commandline
    if "`subcmd'"=="summarize" {
        local cmdline `e(slist)'
    }
    else {
        local cmdline `e(depvar)'
    }
    if `"`e(wtype)'"'!="" {
        local cmdline `cmdline' [`e(wtype)'`e(wexp)'] // what about complex weights in svy?
    }
    local cmdline `cmdline' if `touse', nose `generate' /*
        */ qdef(`e(qdef)') vformat(`e(vformat)') `e(novalues)'
    // - over()
    if `"`e(over)'"'!="" {
        if `"`e(over_select)'"'!="" {
            local over select(`e(over_select)')
        }
        if `"`over'"'!="" {
            local over over(`e(over)', `over')
        }
        else {
            local over over(`e(over)')
        }
        local cmdline `cmdline' `over' `e(total)'
        if `"`e(balance)'"'!="" {
            local balance `e(balmethod)':`e(balance)'
            if `"`e(balopts)'`e(balref)'"'!="" {
                local balance `balance', `e(balopts)'
                if `"`e(balref)'"'!="" local balance `balance' reference(`e(balref)') 
            }
            local cmdline `cmdline' balance(`balance')
        }
    }
    // - density estimation
    if `"`e(kernel)'"'!="" {
        local cmdline `cmdline' kernel(`e(kernel)') adaptive(`e(adaptive)')/*
            */ napprox(`e(napprox)') pad(`e(pad)') `e(exact)'
        capt confirm matrix e(bwidth)
        if _rc==1 exit _rc
        if _rc==0 {
            tempname BW
            matrix `BW' = e(bwidth)
            local cmdline `cmdline' bwidth(`BW') 
        }
        else {
            local cmdline `cmdline' bwidth(`e(bwmethod)') 
        }
        if `"`e(boundary)'"'!="" {
            local cmdline `cmdline' ll(`e(ll)') ul(`e(ul)') boundary(`e(boundary)')
        }
    }
    // evaluation points
    capt confirm matrix e(at)
    if _rc==1 exit _rc
    if _rc==0 {
        tempname AT
        matrix `AT' = e(at)
        local cmdline `cmdline' at(`AT')
    }
    // - density
    if "`subcmd'"=="density" {
        local cmdline `cmdline' `e(unconditional)'
    }
    // - histogram
    else if "`subcmd'"=="histogram" {
        local cmdline `cmdline' `e(proportion)' `e(percent)' `e(frequency)'/*
            */ `e(unconditional)'
    }
    // - cdf/ccdf
    else if inlist("`subcmd'","cdf","ccdf") {
        local cmdline `cmdline' `e(percent)' `e(frequency)' `e(mid)'/*
            */ `e(floor)' `e(discrete)' `e(ipolate)' `e(unconditional)'
    }
    // - proportion
    else if "`subcmd'"=="proportion" {
        if `"`e(categorical)'"'=="" {
            local cmdline `cmdline' nocategorical
        }
        local cmdline `cmdline' `e(percent)' `e(frequency)' `e(unconditional)'
    }
    // - quantile
    else if "`subcmd'"=="quantile" {
        // (none)
    }
    // - lorenz
    else if "`subcmd'"=="lorenz" {
        local cmdline `cmdline' `e(gap)' `e(sum)' `e(generalized)'/*
            */ `e(absolute)' `e(percent)'
        if `"`e(zvar)'"'!="" {
            local cmdline `cmdline' zvar(`e(zvar)')
        }
    }
    // - share
    else if "`subcmd'"=="share" {
        local cmdline `cmdline' `e(proportion)' `e(percent)' `e(sum)'/*
            */ `e(average)' `e(generalized)' 
        if `"`e(zvar)'"'!="" {
            local cmdline `cmdline' zvar(`e(zvar)')
        }
    }
    // - summarize
    else if "`subcmd'"=="summarize" {
        if `"`e(zvar)'"'!="" {
            local cmdline `cmdline' zvar(`e(zvar)')
        }
        if `"`e(pline)'"'!="" {
            local cmdline `cmdline' pline(`e(pline)')
        }
    }
    // compute IFs
    tempname ecurrent b
    mat `b' = e(b)
    _estimates hold `ecurrent', restore
    qui Estimate `subcmd' `cmdline'
    mat `b' = mreldif(`b',e(b)) // returns missing if non-conformable
    capt assert (`b'[1,1]<1e-15)
    if _rc==1 exit _rc
    if _rc {
        di as err "inconsistent re-estimation results; computation of influence functions failed"
        exit 498
    }
    c_local vlist `e(generate)'
    if `"`touse0'"'!="" {
        // fill in zeros outside of subpop; only relevant after survey estimation
        foreach v in `e(generate)' {
            qui replace `v' = 0 if `touse0' & `touse'==0
        }
    }
end

program Set_CI, eclass
    syntax [, Level(cilevel) citype(str) * ]
    // determine citype
    if `"`citype'"'=="" local citype `"`e(citype)'"'
    if `"`citype'"'=="" {
        local subcmd `"`e(subcmd)'"'
        if      `"`subcmd'"'=="density" local citype log
        else if `"`subcmd'"'=="histogram"  {
            if `"`e(frequency)'"'!=""       local citype log
            else if `"`e(proportion)'"'!="" local citype logit
            else if `"`e(percent)'"'!=""    local citype logit
            else                            local citype log
        }
        else if inlist(`"`subcmd'"',"cdf","ccdf") {
            if `"`e(frequency)'"'!=""   local citype log
            else                        local citype logit
        }
        else if `"`subcmd'"'=="proportion" {
            if `"`e(frequency)'"'!=""   local citype log
            else                        local citype logit
        }
    }
    Parse_citype, `citype'
    // nothing to do if
    // - citype did not change
    // - level did not change
    // - e(ci) exists
    if "`citype'"==`"`e(citype)'"' & `level'==e(level) {
        capt confirm matrix e(ci)
        if _rc==1 exit _rc
        if _rc==0 exit
    }
    // noting to do if no variances/standard errors are available
    capt confirm matrix e(V)
    if _rc==1 exit _rc
    if _rc {
        capt confirm matrix e(se)
        if _rc==1 exit _rc
        if _rc    exit
    }
    // compute CIs
    if "`e(percent)'"!="" local scale 100
    else                  local scale 1
    tempname CI
    mata: dstat_Get_CI("`CI'", `level', "`citype'", `scale')
    eret matrix ci = `CI'
    eret local citype "`citype'"
    eret scalar level = `level'
end

program Parse_citype
    capt n syntax [, normal logit probit atanh log ]
    if _rc==1 exit _rc
    if _rc {
        di as error "error in option {bf:citype()}"
        exit 198
    }
    local citype `normal' `logit' `probit' `atanh' `log'
    if `:list sizeof citype'>1 {
        di ar error "only one {it:method} allowed in {bf:citype()}"
        exit 198
    }
    if "`citype'"=="" local citype "normal"
    c_local citype `citype'
end

program Replay
    if `"`e(cmd)'"'!="dstat" {
        di as err "last dstat results not found"
        exit 301
    }
    syntax [, Level(passthru) citype(passthru) GRaph GRaph2(str asis) * ]
    if `"`graph2'"'!="" local graph graph
    if `"`level'"'=="" {
        if `"`e(level)'"'!="" {
            local level level(`e(level)')
        }
    }
    local options `level' `citype' `options'
    Set_CI, `level' `citype'
    if c(noisily) {
        _Replay, `graph' `options'
    }
    if "`graph'"!="" {
        Graph, `graph2'
    }
end

program _Replay
    local subcmd `"`e(subcmd)'"'
    syntax [, citype(passthru) noHeader NOTABle TABle GRaph * ]
    if "`header'"=="" {
        _Replay_header
        if "`subcmd'"=="density" {
            di as txt _col(35) "Kernel" _col(51) "= " /*
                */as res %10s abbrev(e(kernel),10)
            mata: st_local("bwidth", ///
                mm_isconstant(st_matrix("e(bwidth)")) ? ///
                "%10.0g el(e(bwidth), 1, 1)" : ///
                `""{stata matrix list e(bwidth):{bf:{ralign 10:e(bwidth)}}}""')
            di as txt _col(35) "Bandwidth" _col(51) "= " as res `bwidth'
        }
        else if "`subcmd'"=="quantile" {
            di as txt _col(35) "Quantile type" _col(51) "= " as res %10.0g e(qdef)
        }
        else if "`subcmd'"=="lorenz" | "`subcmd'"=="share" {
            if `"`e(zvar)'"'!="" {
                di as txt _col(35) "Sort variable" _col(51) "= " as res %10s e(zvar)
            }
        }
        else if "`subcmd'"=="summarize" {
            if `"`e(zvar)'"'!="" {
                di as txt _col(35) "Sort variable" _col(51) "= " as res %10s e(zvar)
            }
            if `"`e(pline)'"'!="" {
                di as txt _col(35) "Poverty line" _col(51) "= " as res %10s e(pline)
            }
        }
        if `"`e(balance)'"'!="" {
            di as txt _col(35) "Balancing" _col(51) "= " as res %10s e(balmethod)
            di as txt _col(35) "Reference" _col(51) "= " _c
            if `"`e(balref)'"'!="" di as res %10s e(balref)
            else                   di as res %10s "total"
            mata: st_local("balance", ///
                udstrlen(st_global("e(balance)"))<=10 ? ///
                "%10s e(balance)" : ///
                `""{stata di as txt e(balance):{bf:e(balance)}}""')
            di as txt _col(35) "Controls" _col(51) "= " as res `balance'
        }
        if `"`e(over)'"'!="" {
            if "`subcmd'"=="summarize" {
                if !(`e(N_stats)'==1 & `e(N_vars)'==1) _svy_summarize_legend
            }
            else _svy_summarize_legend
        }
        else di ""
    }
    if ("`table'"!="" | "`graph'"=="") & "`notable'"=="" {
        capt confirm matrix e(V)
        if _rc==1 exit _rc
        if _rc {
            capt confirm matrix e(se)
            if _rc==1 exit _rc
            if _rc==0 {
                tempname V
                mat `V' = e(se)
                mata: st_replacematrix("`V'", st_matrix("`V'"):^2)
                mat `V' = diag(`V')
                local vmatrix vmatrix(`V')
            }
        }
        if c(stata_version)>=15 {
            local hasci 0
            capt confirm matrix e(ci)
            if _rc==1 exit _rc
            if _rc==0 local hasci 1
            if `hasci' {
                if `"`e(citype)'"'!="normal" {
                    local cititle cititle(`e(citype)' transformed)
                }
                _coef_table, nopvalue `vmatrix' cimat(e(ci)) `cititle' `options'
            }
            else {
                _coef_table, nopvalue `vmatrix' `options'
            }
        }
        else {
            _coef_table, nopvalue `vmatrix' `options'
            if `"`citype'"'!="" /// user specified citype()
                & `"`e(citype)'"'!="normal" {
                capt confirm matrix e(V)
                if _rc==1 exit _rc
                if _rc {
                    capt confirm matrix e(se)
                    if _rc==1 exit _rc
                }
                if _rc==0 {
                    di as txt "(table displays untransformed CIs; Stata 15 or" /*
                        */ " newer required for transformed CIs in output table)"
                }
            }
        }
        if `"`e(novalues)'"'!="" {
            capt confirm matrix e(at)
            if _rc==1 exit _rc
            if _rc==0 {
                di as txt "(evaluation grid stored in {stata matrix list e(at):{bf:e(at)}})"
            }
        }
    }
    else if "`notable'"=="" {
        di as txt "({stata dstat:coefficients table} suppressed)"
    }
end

prog _Replay_header, eclass // mimick header of -total-
    nobreak {
        ereturn local cmd "total"
        capture noisily break {
            _coef_table_header, nomodeltest
        }
        ereturn local cmd "dstat"
        if _rc exit _rc
    }
end

program Graph
    if `"`e(cmd)'"'!="dstat" {
        di as err "last {bf:dstat} results not found"
        exit 301
    }
    local subcmd `"`e(subcmd)'"'
    
    // syntax
    syntax [, Level(passthru) citype(passthru) VERTical HORizontal ///
        MERge flip BYStats BYStats2(str) NOSTEP STEP NOREFline REFline(str) ///
        SELect(numlist int >0) GSELect(numlist int >0) PSELect(numlist int >0) ///
        BYOPTs(str) PLOTLabels(str asis) * ]
    if `"`gselect'"'=="" local gselect `"`select'"'
    if `"`pselect'"'=="" local pselect `"`select'"'
    local options `vertical' `horizontal' `options'
    if `"`bystats2'"'=="" & "`bystats'"!="" local bystats2 main
    _Graph_parse_bystats, `bystats2'
    _Graph_parse_opts, `options'
    _Graph_parse_ciopts, `ciopts'
    if `"`ci_recast'"'=="" & `"`cirecast'"'!="" local ci_recast recast(`cirecast')
    if "`nostep'"=="" {
        if `"`e(discrete)'"'!="" & `"`e(ipolate)'"'=="" local step step
    }
    
    // obtain results: b, at, ci
    tempname B
    matrix `B' = e(b)
    local b matrix(`B')
    if `"`subcmd'"'!="summarize" {
        if !(`"`subcmd'"'=="proportion" & `"`e(novalues)'"'=="") {
            tempname AT
            matrix `AT' = e(at)
            local at at(`AT')
        }
    }
    if "`noci'"=="" {
        if `"`citype'"'=="" {
            local citype citype(`e(citype)')
        }
        if `"`level'"'=="" {
            if `"`e(level)'"'!="" {
                local level level(`e(level)')
            }
        }
        tempname CI
        _Graph_Get_CI `CI', `level' `citype' // may clear CI
        if "`CI'"!="" {
            local ci ci(`CI')
            if inlist(`"`subcmd'"', "histogram", "share") {
                tempname BCI ATCI
                mat `BCI' = `B'
                mat `ATCI' = `AT'[2,1...] // bin midpoints
                mata: dstat_graph_droplast(("`BCI'", "`CI'", "`ATCI'"))
                local bci matrix(`BCI')
                local atci at(`ATCI')
            }
            else {
                local bci `b'
                local atci `at'
            }
        }
        else local noci noci
    }
    
    // determine layout
    // - collect sets of results
    local overeq 0
    if (`"`e(over)'"'!="") {
        if `"`subcmd'"'!="summarize" local overeq 1
    }
    if `"`subcmd'"'=="summarize" {
        if `"`e(over)'"'!="" {
            if e(N_stats)==1 {
                if e(N_vars)>1 local overeq 1
            }
            else local overeq 1
        }
        local eqs: coleq `B'
        local eqs: list uniq eqs
        if "`bystats'"=="main" {
            if `"`eqs'"'!="_" {
                if strpos(`"`eqs'"',"~") {
                    mata: dstat_graph_swap(1)
                }
                else {
                    mata: dstat_graph_swap(0)
                }
            }
        }
        else if "`bystats'"=="secondary" {
            if strpos(`"`eqs'"',"~") {
                mata: dstat_graph_swap(2)
            }
        }
    }
    local eqs: coleq `B'
    local eqs: list uniq eqs
    local eqlist `"`eqs'"'
    if strpos(`"`eqs'"',"~") {
        mata: dstat_graph_eqsplit()
        local eqs: list uniq eqs
        local subeqs: list uniq subeqs
    }
    else local subeqs "_"
    local ni: list sizeof eqs
    local nj: list sizeof subeqs
    if `"`eqs'"'!="_" {
        if `overeq' {
            _Graph_overlabels // returns overlabels
        }
        local i 0
        foreach eq of local eqs {
            local ++i
            if `overeq' {
                local lbli_`i': word `i' of `overlabels'
            }
            else {
                local lbli_`i' `"`eq'"'
            }
        }
        if `"`subeqs'"'!="_" {
            local j 0
            foreach subeq of local subeqs {
                local ++j
                local lblj_`j' `"`subeq'"'
            }
        }
    }
    local i 0
    foreach eq of local eqs {
        local ++i
        local j 0
        foreach subeq of local subeqs {
            local ++j
            local eqij `"`eq'"'
            if `"`subeq'"'!="_" {
                local eqij `"`eqij'~`subeq'"'
            }
            if `: list eqij in eqlist' {
                local keep_`i'_`j' keep(`eqij':)
            }
            else {
                local keep_`i'_`j' "." // skip
            }
        }
    }
    // - assign sets to plots and subgraphs
    if (`nj'>1 & "`flip'"!="") | (`nj'==1 & "`flip'`merge'"=="") {
        local ii j
        local jj i
    }
    else {
        local ii i
        local jj j
    }
    // - compile list of indices for subgraphs and plots
    local jlist
    if `n`jj''>1 {
        forv j=1/`n`jj'' {
            // remove empty subgraphs (can happen with dstat summarize)
            local skip 1
            forv i=1/`n`ii'' {
                if `"`keep_``ii''_``jj'''"'!="." {
                    local skip 0
                    continue, break
                }
            }
            if `skip' continue
            local jlist `jlist' `j'
        }
        local n`jj': list sizeof jlist
    }
    else local jlist 1
    local ilist
    forv i=1/`n`ii'' {
        local ilist `ilist' `i'
    }
    // - apply select
    if `"`gselect'`pselect'"'!="" {
        if `"`gselect'"'!="" & `n`jj''>1 {
            mata: dstat_graph_select("jlist", "gselect")
            local n`jj': list sizeof jlist
        }
        if `"`pselect'"'!="" & `n`ii''>1 {
            mata: dstat_graph_select("ilist", "pselect")
            local n`ii': list sizeof ilist
        }
        if `n`jj''==0 | `n`ii''==0 {
            di as txt "(nothing to plot)"
            exit
        }
    }
    
    // (default) rendering
    if inlist(`"`subcmd'"', "density", "cdf", "ccdf", "quantile", "lorenz") {
        if `"`recast'"'=="" {
            local recast recast(line)
            if (inlist(`"`subcmd'"',"cdf","ccdf") & "`step'"!="") {
                local recast `recast' connect(J)
            }
            local ptype line
        }
        if `"`ci_recast'"'=="" {
            local ci_recast recast(rarea) pstyle(ci)
            if (inlist(`"`subcmd'"',"cdf","ccdf") & "`step'"!="") {
                local ci_recast `ci_recast' connect(J)
            }
            if c(stata_version)>=15 {
                local ci_recast `ci_recast' color(%50) lcolor(%0)
            }
        }
        if "`noci'"=="" local cipos -1 // print CIs separately upfront
        else            local cipos 0
    }
    else if inlist(`"`subcmd'"', "histogram", "share") {
        if `"`recast'"'=="" {
            local recast recast(bar) bartype(spanning)
            if "`horizontal'"=="" local options plotr(margin(b=0)) `options'
            else                  local options plotr(margin(l=0)) `options'
            if c(stata_version)>=15 {
                if `n`ii''>1 local recast `recast' color(%50)
            }
            local ptype bar
            if `"`base'"'=="" local base base(0)
        }
        if `"`ci_recast'"'=="" {
            local ci_recast recast(rcap)
        }
        if "`noci'"=="" local cipos 1 // print CIs separately on top of each plot
        else            local cipos 0
    }
    else if `"`subcmd'"'=="proportion" {
        if `"`recast'"'=="" {
            local recast recast(bar)
            if "`vertical'"=="" local options plotr(margin(l=0)) `options'
            else                local options plotr(margin(b=0)) `options'
            local ptype bar
            if `"`barwidth'"'=="" {
                if `n`ii''==1 local barwidth 0.7
                else          local barwidth = .5 / `n`ii''
                local barwidth barwidth(`barwidth')
            }
            if `"`base'"'=="" local base base(0)
            if "`noci'"==""   local citop citop
        }
        if `"`ci_recast'"'=="" {
            local ci_recast recast(rcap)
        }
        local cipos 0 // CI not separate
    }
    else {
        local ptype
        local cipos 0 // CI not separate
    }
    
    // collect plot options
    local pi 0
    foreach i of local ilist {
        local ++pi
        _Graph_parse_popt `i' `pi', `options'
        if `"`p`i'_recast'"'=="" {
            if `"`p`i'_pstyle'"'==""  local p`i'_pstyle pstyle(p`pi'`ptype')
            else                      local p`i'_ci_pstyle `p`i'_pstyle'
        }
        else if `"`p`i'_pstyle'"'=="" local p`i'_pstyle pstyle(p`pi')
        if `"`p`i'_ci_pstyle'"'==""   local p`i'_ci_pstyle `p`i'_pstyle'
        if `"`p`i'_recast'"'=="" {
            local p`i'_recast `recast'
            if `"`p`i'_barwidth'"'=="" local p`i'_barwidth `barwidth'
            if `"`p`i'_base'"'==""     local p`i'_base `base'
            if `"`p`i'_citop'"'==""    local p`i'_citop `citop'
        }
        if `"`p`i'_ci_recast'"'==""   local p`i'_ci_recast `ci_recast'
        if `"`p`i'_ci_options'"'==""  local p`i'_ci_options `ci_options'
        if `"`p`i'_noci'"'==""        local p`i'_noci `noci'
        if `"`p`i'_label'"'=="" {
            local p`i'_label: word `i' of `plotlabels'
            if `"`p`i'_label'"'=="" local p`i'_label label(`"`lbl`ii'_`i''"')
            else                    local p`i'_label label(`"`p`i'_label'"')
        }
        local p`i' `p`i'_label' `p`i'_pstyle' `p`i'_recast' `p`i'_barwidth'/*
             */ `p`i'_base' `p`i'_citop' `p`i'_options'
        local p`i'_ci `p`i'_ci_recast' `p`i'_ci_options'
        if `"`p`i'_ci'"'!="" local p`i'_ci ciopts(`p`i'_ci')
    }
    
    // refline for lorenz
    if `"`subcmd'"'=="lorenz" & "`norefline'"=="" {
        if `"`e(sum)'`e(gap)'`e(generalized)'`e(absolute)'"'=="" {
            tempname REF
            if `"`e(percent)'"'!="" {
                mat `REF' = (0, 100) \ (0, 1)
            }
            else {
                mat `REF' = (0, 1) \ (0, 1)
            }
            local reflineplot (matrix(`REF'), at(`REF'[2]) noci nokey /*
                */ recast(line) lstyle(yxline) `refline')
        }
    }
    
    // compile plot
    local plots
    foreach j of local jlist {
        if `"`reflineplot'"'!="" {
            local plots `plots' `reflineplot'
        }
        if `cipos'!=0 {
            if `cipos'==-1 {
                foreach i of local ilist {
                    if `"`keep_``ii''_``jj'''"'=="." continue 
                    if "`p`i'_noci'"=="" {
                        local plots `plots' (`bci', `keep_``ii''_``jj'''/*
                            */ `ci' `atci' cionly nokey/*
                            */ `p`i'_ci_pstyle' `p`i'_ci')
                    }
                }
            }
            foreach i of local ilist {
                if `"`keep_``ii''_``jj'''"'=="." {
                    local plots `plots' _skip
                    continue
                }
                local plots `plots' (`b', noci `keep_``ii''_``jj''' `p`i'')
                if `cipos'==1 & "`p`i'_noci'"=="" {
                    local plots `plots' (`bci', `keep_``ii''_``jj'''/*
                        */ `ci' `atci' cionly nokey/*
                        */ `p`i'_ci_pstyle' `p`i'_ci')
                }
            }
        }
        else {
            foreach i of local ilist {
                if `"`keep_``ii''_``jj'''"'=="." {
                    local plots `plots' _skip
                    continue
                }
                local plots `plots' (`b', `keep_``ii''_``jj''' `ci'/*
                    */ `p`i'' `p`i'_noci' `p`i'_ci')
            }
        }
        if `n`jj''>1 {
            local plots `plots', bylabel(`"`lbl`jj'_`j''"') ||
        }
    }
    // turn off legend
    if `n`ii''==1 {
        if `n`jj''==1 local legendoff legend(off)
        else          local byopts legend(off) `byopts'
    }
    
    // draw graph
    if `"`byopts'"'!="" local byopts byopts(`byopts')
    local plots `plots', `at' `bopts' `legendoff' `byopts' `options'
    // di `"`plots'"'
    coefplot `plots'
end

program _Graph_parse_bystats
    syntax [, Main Secondary ]
    local bystats `main' `secondary'
    if `:list sizeof bystats'>1 {
        di as err "{bf:bystats()}: only one of {bf:main} and {bf:secondary} allowed"
        exit 198
    }
    c_local bystats `bystats'
end

program _Graph_overlabels
    local overlabels `"`e(over_labels)'"'
    if `"`overlabels'"'==`"`e(over_namelist)'"' {
        local over `"`e(over)'"'
        local overlabels
        local space
        foreach oval in `e(over_namelist)' {
            local overlabels `"`overlabels'`space'`"`over'=`oval'"'"'
            local space " "
        }
    }
    if `"`e(total)'"'!="" {
        local overlabels `"`overlabels' Total"'
    }
    c_local overlabels `"`overlabels'"'
end

program _Graph_parse_opts
    local notallowed /*b at keep drop*/ Levels /*ci v se df*/ citype
    local notallowed2 SWAPnames BYCoefs
    local naopts
    foreach o of local notallowed {
        local naopts `naopts' `o'(passthru)
    }
    foreach o of local notallowed2 {
        local naopts `naopts' `o'
    }
    syntax [, `naopts' ///
        NOCI LABel(passthru) PSTYle(passthru) recast(passthru) ///
        BARWidth(passthru) base(passthru) citop ///
        CIREcast(str) CIOPts(str) * ]
    foreach o of local notallowed {
        local opt = strlower("`o'")
        if `"``opt''"'!="" {
            di as err "option {bf:`opt'()} not allowed"
            exit 198
        }
    }
    foreach o of local notallowed2 {
        local opt = strlower("`o'")
        if `"``opt''"'!="" {
            di as err "option {bf:`opt'} not allowed"
            exit 198
        }
    }
    c_local noci `noci'
    c_local label `label'
    c_local pstyle `pstyle'
    c_local recast `recast'
    c_local barwidth `barwidth'
    c_local base `base'
    c_local citop `citop'
    c_local cirecast `"`cirecast'"'
    c_local ciopts `ciopts'
    c_local options `options'
end

program _Graph_parse_ciopts
    syntax [, recast(passthru) * ]
    c_local ci_recast `"`recast'"'
    c_local ci_options `"`options'"'
    c_local ciopts ""
end

program _Graph_parse_popt
    gettoken i 0 : 0
    _parse comma pi 0 : 0
    syntax [, p`pi'(str) * ]
    c_local options `options'
    _Graph_parse_opts, `p`pi''
    _Graph_parse_ciopts, `ciopts'
    if `"`ci_recast'"'=="" & `"`cirecast'"'!="" local ci_recast recast(`cirecast')
    c_local p`i'_noci `noci'
    c_local p`i'_label `label'
    c_local p`i'_pstyle `pstyle'
    c_local p`i'_recast `recast'
    c_local p`i'_barwidth `barwidth'
    c_local p`i'_base `base'
    c_local p`i'_citop `citop'
    c_local p`i'_ci_recast `ci_recast'
    c_local p`i'_ci_options `ci_options'
    c_local p`i'_options `options'
end

program _Graph_Get_CI, eclass
    _parse comma CI 0 : 0
    syntax [, Level(cilevel) citype(str) ]
    // get CIs from e(ci) is appropriate
    if "`citype'"==`"`e(citype)'"' & `level'==e(level) {
        capt confirm matrix e(ci)
        if _rc==1 exit _rc
        if _rc==0 {
            matrix `CI' = e(ci)
        }
        else {
            c_local CI
        }
        exit
    }
    // compute CIs
    capt confirm matrix e(V)
    if _rc==1 exit _rc
    else {
        capt confirm matrix e(se)
        if _rc==1 exit _rc
        if _rc {
            c_local CI
            exit
        }
    }
    if "`e(percent)'"!="" local scale 100
    else                  local scale 1
    mata: dstat_Get_CI("`CI'", `level', "`citype'", `scale')
end

program Estimate
    _parse comma lhs 0 : 0
    syntax [, DECOMPose(passthru) * ]
    if `"`decompose'"'=="" {
        _Estimate `lhs' `0'
        c_local generate_quietly `generate_quietly'
        exit
    }
    di as err "{bf:decompose()} not implemented yet..."
    exit 499
end

program _Estimate, eclass
    // syntax
    gettoken subcmd 0 : 0
    local lhs varlist(numeric fv)
    if "`subcmd'"=="density" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ UNConditional UNConditional2(str)
    }
    else if "`subcmd'"=="histogram" {
        local opts n(passthru) at(str) ep PROPortion PERcent FREQuency /*
            */ UNConditional UNConditional2(str)
    }
    else if inlist("`subcmd'","cdf","ccdf") {
        local opts n(numlist int >=1 max=1) at(str) mid FLoor DISCrete /*
        */ PERcent FREQuency IPolate UNConditional UNConditional2(str)
    }
    else if "`subcmd'"=="proportion" {
        local opts NOCATegorical at(str) PERcent FREQuency /*
            */ UNConditional UNConditional2(str)
    }
    else if "`subcmd'"=="quantile" {
        local opts n(numlist int >=1 max=1) at(str)
    }
    else if "`subcmd'"=="lorenz" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ gap sum GENERALized ABSolute PERcent Zvar(varname)
    }
    else if "`subcmd'"=="share" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ PROPortion PERcent sum AVErage GENERALized Zvar(varname)
    }
    else if "`subcmd'"=="summarize" {
        local lhs anything(id="varlist")
        local opts Zvar(varname) PLine(passthru)
    }
    else exit 499
    syntax `lhs' [if] [in] [fw iw pw/], [ ///
            NOVALues VFormat(str) ///
            qdef(numlist max=1 int >=0 <=9) ///
            Over(str) TOTal BALance(str) ///
            vce(str) NOSE Level(cilevel) ///
            Generate(passthru) rif(str) Replace ///
            `opts' * ]
    Parse_over `over'
    Parse_uncond, `unconditional2'
    if `"`generate'"'!="" & `"`rif'"'!="" {
        di as err "{bf:generate()} and {bf:rif()} not both allowed"
        exit 198
    }
    if `"`vformat'"'!="" {
        confirm format `vformat'
    }
    else local vformat %9.0g
    if "`qdef'"=="" local qdef 2
    if "`subcmd'"=="proportion" {
        if "`nocategorical'"=="" local categorical "categorical"
    }
    Parse_densityopts, `options'
    Parse_at "`subcmd'" "`categorical'" `"`n'"' `"`at'"'
    if "`subcmd'"!="density" {
        Parse_varlist_unique `varlist'
    }
    if "`subcmd'"=="density" {
        if `"`at'`n'"'=="" local n 99
    }
    else if "`subcmd'"=="histogram" {
        if `"`at'"'=="" Parse_hist_n, `n' `ep'
        local tmp `proportion' `percent' `frequency'
        if `:list sizeof tmp'>1 {
            di as err "only one of {bf:percent}, {bf:proportion}, and {bf:frequency} allowed"
            exit 198
        }
        if "`frequency'"!="" & "`unconditional'"!="" {
            di as err "{bf:frequency} and {bf:unconditional} not both allowed"
            exit 198
        }
    }
    else if inlist("`subcmd'","cdf","ccdf") {
        if "`percent'"!="" & "`frequency'"!="" {
            di as err "{bf:percent} and {bf:frequency} not both allowed"
            exit 198
        }
        if "`frequency'"!="" & "`unconditional'"!="" {
            di as err "{bf:frequency} and {bf:unconditional} not both allowed"
            exit 198
        }
        if "`mid'"!="" & "`floor'"!="" {
            di as err "{bf:mid} and {bf:floor} not both allowed"
            exit 198
        }
        if "`discrete'"!="" {
            if "`n'"!="" {
                di as err "{bf:n()} and {bf:discrete} not both"
                exit 198
            }
        }
        else if `"`at'`n'"'=="" local n 99
    }
    else if "`subcmd'"=="proportion" {
        local discrete "discrete"
        if "`percent'"!="" & "`frequency'"!="" {
            di as err "{bf:percent} and {bf:frequency} not both allowed"
            exit 198
        }
        if "`frequency'"!="" & "`unconditional'"!="" {
            di as err "{bf:frequency} and {bf:unconditional} not both allowed"
            exit 198
        }
    }
    else if "`subcmd'"=="quantile" {
        if `"`at'`n'"'=="" local n 99
    }
    else if "`subcmd'"=="lorenz" {
        if `"`at'`n'"'=="" local n 101
        local tmp `gap' `sum' `generalized' `absolute'
        if `:list sizeof tmp'>1 {
            di as err "only one of {bf:gap}, {bf:sum}, {it:generalized}," /*
                */ " and {bf:absolute} allowed"
            exit 198
        }
        if "`percent'"!="" {
            foreach tmp in sum generalized absolute {
                if "``tmp''"!="" {
                    di as err "{bf:percent} and {bf:`tmp'} not both allowed"
                    exit 198
                }
            }
        }
        local zvars `zvar'
    }
    else if "`subcmd'"=="share" {
        if `"`at'`n'"'=="" local n 20
        local tmp `proportion' `percent' `sum' `average' `generalized'
        if `:list sizeof tmp'>1 {
            di as err "only one of {bf:percent}, {bf:proportion}, {it:sum}," /*
                */ " {it:average}, and {bf:generalized} allowed"
            exit 198
        }
        local zvars `zvar'
    }
    else if "`subcmd'"=="summarize" {
        Parse_pline, `pline' // returns pline, plvar
        Parse_sum "`zvar'" "`pline'" "`plvar'" /*
            */ `anything' // returns varlist, vlist, slist, zvars, plvars
    }
    Parse_vce "`subcmd'" `vce'
    if `"`balance'"'!="" {
        if "`over'"=="" {
            di as err "{bf:balance()} requires {bf:over()}"
            exit 198
        }
        Parse_balance `balance'
        if "`bal_ref'"!="" & "`total'"!="" {
            di as err "{bf:total} not allowed with {bf:balance(, reference())}"
            exit 198
        }
        if "`bal_wvar'"!="" {
            if "`replace'"=="" {
                confirm new variable `bal_wvar'
            }
        }
    }
    Parse_rif `rif'
    Parse_generate, `generate' `replace'
    if "`over'"=="" {
        // options that are only relevant if over() is specified
        local compact
        local unconditional
        local unconditional2
        local total
    }
    if "`compact'"!="" {
        if `"`bal_method'"'!="" {
            di as err "{bf:compact} not allowed  {bf:balance()}"
            exit 198
        }
        if "`unconditional'"!="" & "`unconditional2'"=="" {
            di as err "{bf:compact} not allowed with {bf:unconditional}"
            exit 198
        }
    }
    c_local generate_quietly `generate_quietly'
    
    // sample and weights
    marksample touse
    markout `touse' `varlist' `over' `bal_varlist' `zvars' `plvars'
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    if "`weight'"!="" {
        capt confirm variable `exp'
        if _rc==1 exit _rc
        if _rc {
            tempvar wvar
            qui gen double `wvar' = `exp' if `touse'
        }
        else {
            unab exp: `exp', min(1) max(1)
            local wvar `exp'
        }
        local wgt "[`weight'=`wvar']"
        local exp `"= `exp'"'
        local wgt "`wgt'"
    }
    else local wvar 1
    _nobs `touse' `wgt', min(1)
    if "`weight'"=="iweight" {
        su `wvar' if `touse', meanonly
        local N = r(sum)
    }
    else {
        local N = r(N)
    }
    tempname W // for sum of weights
    
    // expand factor variables (and process slist)
    if "`subcmd'"=="summarize" {
        Parse_slist `touse' `slist' // returns slist and varlist
    }
    else {
        fvexpand `varlist' if `touse'
        local varlist `r(varlist)'
    }
    
    // check poverty lines
    foreach plv of local plvars {
        capt assert(`plv'>0) if `touse'
        if _rc==1 exit _rc
        else if _rc {
            di as err `"`plv': poverty line must be positive"'
            exit _rc
        }
    }
    
    // over and balance
    if "`over'"!="" {
        capt assert ((`over'==floor(`over')) & (`over'>=0)) if `touse'
        if _rc==1 exit _rc
        if _rc {
            di as err "variable in over() must be integer and nonnegative"
            exit 452
        }
        qui levelsof `over' if `touse', local(overlevels)
        if `"`bal_method'"'!="" {
            if "`bal_ref'"!="" {
                if `:list bal_ref in overlevels'==0 {
                    di as err "{bf:balance()}: no observations in reference distribution"
                    exit 2000
                }
            }
            if "`bal_wvar'"!="" {
                tempname BAL_WVAR
                qui gen double `BAL_WVAR' = `wvar' if `touse'
            }
            // expand bal_varlist (so that it will be stable)
            fvexpand `bal_varlist' if `touse'
            local bal_varlist2 `r(varlist)'
        }
        if `"`select'"'!="" {
            local tmp
            foreach o of local select {
                if `:list o in overlevels' {
                    local tmp `tmp' `o'
                }
            }
            local overlevels: list uniq tmp
            if `:list sizeof overlevels'==0 {
                di as err "{bf:select()}: must select at least one existing group"
                exit 499
            }
            local select "`overlevels'"
        }
        local over_labels
        foreach o of local overlevels {
            local olab: label (`over') `o'
            local over_labels `"`over_labels' `"`olab'"'"'
        }
        local over_labels: list clean over_labels
        local N_over: list sizeof overlevels
    }
    
    // estimate
    tempname b id _N _W
    if "`subcmd'"!="summarize" tempname AT
    if inlist("`subcmd'", "density", "quantile", "summarize") tempname BW
    if "`svy'"!="" tempname istot
    mata: dstat()
    // process IFs
    if "`generate'"!="" {
        local coln: colfullnames `b'
        if "`svy'"!="" {
            local i 0
            local j 0
            local gid0 ""
            foreach IF of local IFs {
                gettoken nm coln : coln
                lab var `IF' "score of _b[`nm']"
                if "`svy'"=="done" continue // already done by dstat_vce()
                local ++i
                local gid = `id'[1, `i']
                if "`gid'"!="`gid0'" {
                    local ++j
                    local gid0 `gid'
                }
                if `istot'[1,`i']==0 continue // no adjustment required
                if "`gid'"=="." local ifexp `touse'
                else            local ifexp `touse' & `over'==`gid'
                qui replace `IF' = `IF' + `b'[1,`i']/`_W'[1,`j'] if `ifexp'
            }
        }
        else if "`compact'"!="" {
            local CIFs
            local i 0
            local j 0
            local gid0 ""
            foreach IF of local IFs {
                gettoken nm coln : coln
                local ++i
                local gid = `id'[1, `i']
                if "`gid'"!="`gid0'" {
                    local ++j
                    if "`gid'"=="." { // total
                        local k `i'
                        local ifexp `touse'
                    }
                    else {
                        local k 1
                        local ifexp `touse' & `over'==`gid'
                    }
                    local gid0 `gid' 
                }
                mata: st_local("CIF", tokens(st_local("IFs"))[`k'])
                if "`rif'"!="" {
                    qui replace `CIF' = `IF' * `_W'[1,`j'] + `b'[1,`i'] if `ifexp'
                }
                else if "`CIF'"!="`IF'" {
                    qui replace `CIF' = `IF' if `ifexp'
                }
                mata: dstat_fix_nm("nm", "`gid'", "`total'"!="")
                lab var `CIF' "IF of _b[`nm']"
                local CIFs `CIFs' `CIF'
                local ++k
            }
            local IFs: list uniq CIFs
        }
        else if "`rif'"!="" {
            local i 0
            foreach IF of local IFs {
                gettoken nm coln : coln
                local ++i
                qui replace `IF' = `IF' * `W' + `b'[1,`i'] if `touse'
                lab var `IF' "RIF of _b[`nm']"
            }
        }
        else {
            foreach IF of local IFs {
                gettoken nm coln : coln
                lab var `IF' "IF of _b[`nm']"
            }
        }
    }
    
    // expand generate_stub, if necessary
    if "`generate_stub'"!="" {
        local generate
        local i 0
        foreach IF of local IFs {
            local ++i
            if "`replace'"=="" confirm new variable `generate_stub'`i'
            local generate `generate' `generate_stub'`i'
        }
        local generate_stub
    }
    
    // returns
    eret post `b' `V' [`weight'`exp'], obs(`N') esample(`touse')
    eret local cmd "dstat"
    eret local subcmd "`subcmd'"
    eret local depvar "`varlist'"
    local N_vars: list sizeof varlist
    eret scalar N_vars = `N_vars'
    eret local predict "dstat predict"
    if "`nose'"=="" {
        eret local vcetype "`vcetype'"
        eret local vce "`vce'"
        eret local vcesvy "`vcesvy'"
        eret scalar df_r = `df_r'
        if "`vce'"=="cluster" {
            eret local clustvar "`clustvar'"
            eret scalar N_clust = `N_clust'
        }
        if "`SE'"!="" {
            eret matrix se = `SE'
        }
    }
    eret scalar W = `W'
    eret scalar k_eq = `k_eq'
    eret scalar k_omit = `k_omit'
    eret matrix _N = `_N'
    eret matrix _W = `_W'
    eret matrix id = `id' // contains value of over-group for each estimate
    eret scalar qdef = `qdef'
    eret local novalues "`novalues'"
    eret local vformat "`vformat'"
    eret local percent "`percent'"
    eret local proportion "`proportion'"
    eret local frequency "`frequency'"
    eret local mid "`mid'"
    eret local floor "`floor'"
    eret local ipolate "`ipolate'"
    eret local discrete "`discrete'"
    eret local categorical "`categorical'"
    eret local ep "`ep'"
    eret local gap "`gap'"
    eret local sum "`sum'"
    eret local generalized "`generalized'"
    eret local absolute "`absolute'"
    eret local average "`average'"
    eret local zvar "`zvar'"
    if "`plvar'"!="" eret local pline "`plvar'"
    else             eret local pline "`pline'"
    if "`over'"!="" {
        eret local total         "`total'"
        if "`unconditional2'"!="" {
            local unconditional "unconditional(`unconditional2')"
        }
        eret local unconditional "`unconditional'"
        eret local over          "`over'"
        eret local over_select   "`select'"
        eret local over_namelist `"`overlevels'"'
        eret local over_labels   `"`over_labels'"'
        ereturn scalar N_over    = `N_over'
        if "`bal_method'"!="" {
            eret local balance "`bal_varlist'"
            eret local balmethod "`bal_method'"
            eret local balref "`bal_ref'"
            eret local balopts "`bal_opts'"
        }
    }
    if "`AT'"!="" {
        eret matrix at = `AT'
    }
    if "`BW'"!="" {
        eret matrix bwidth = `BW'
        if "`bwmethod'"!="" {
            if "`bwmethod'"=="dpi" {
                if "`bwdpi'"!="" local bwmethod "`bwmethod'(`bwdpi')"
            }
            if "`bwadjust'`bwrd'"!="" {
                local bwmethod "`bwmethod',"
                if "`bwadjust'"!="" local bwmethod "`bwmethod' adjust(`bwadjust')"
                if "`bwrd'"!="" local bwmethod "`bwmethod' `bwrd'"
            }
        }
        eret local  bwmethod "`bwmethod'"
        eret local  kernel   "`kernel'"
        eret scalar adaptive = `adaptive'
        eret local  exact    "`exact'"
        eret scalar napprox  = `napprox'
        eret scalar pad      = `pad'
        if `ll'<. | `ul'<. {
            eret local  boundary "`boundary'"
            eret scalar ll       = `ll'
            eret scalar ul       = `ul'
        }
    }
    if "`subcmd'"=="density" {
        local title "Density"
        if "`unconditional'"!="" local title "`title' (unconditional)"
    }
    else if "`subcmd'"=="histogram" {
        local title "Histogram"
        if "`unconditional'"!=""   local title "`title' (unconditional "
        else                       local title "`title' ("
        if      "`percent'"!=""    local title "`title'percent)"
        else if "`proportion'"!="" local title "`title'proportion)"
        else if "`frequency'"!=""  local title "`title'frequency)"
        else                       local title "`title'density)"
    }
    else if inlist("`subcmd'","cdf","ccdf") {
        if "`mid'"!=""             local title "Mid "
        else if "`floor'"!=""      local title "Floor "
        else                       local title ""
        if "`subcmd'"=="ccdf"      local title "`title'C"
        if "`percent'"!=""         local title "`title'CDF in percent"
        else if "`frequency'"!=""  local title "`title'CDF in counts"
        else                       local title "`title'CDF"
        if "`ipolate'"!=""         local title "`title' (interpolated)"
        if "`unconditional'"!=""   local title "`title' (unconditional)"
    }
    else if "`subcmd'"=="proportion" {
        if "`percent'"!=""        local title "Percent"
        else if "`frequency'"!="" local title "Frequency"
        else                      local title "Proportion"
        if "`unconditional'"!=""  local title "`title' (unconditional)"
    }
    else if "`subcmd'"=="quantile" {
        local title "Quantiles"
    }
    else if "`subcmd'"=="lorenz" {
        if "`zvar'"!="" {
            if "`generalized'"!=""   local title "Generalized concentration curve"
            else if "`sum'"!=""      local title "Total concentration curve"
            else if "`absolute'"!="" local title "Absolute concentration curve"
            else if "`gap'"!=""      local title "Equality gap concentration curve"
            else                     local title "Concentration curve"
        }
        else {
            if "`generalized'"!=""   local title "Generalized Lorenz curve"
            else if "`sum'"!=""      local title "Total Lorenz curve"
            else if "`absolute'"!="" local title "Absolute Lorenz curve"
            else if "`gap'"!=""      local title "Equality gap curve"
            else                     local title "Lorenz curve"
        }
        if "`percent'"!="" local title "`title' (percent)"
    }
    else if "`subcmd'"=="share" {
        if "`zvar'"!="" local title "Concentration shares"
        else            local title "Percentile shares"
        if "`proportion'"!=""       local title "`title' (proportion)"
        else if "`percent'"!=""     local title "`title' (percent)"
        else if "`generalized'"!="" local title "`title' (generalized)"
        else if "`sum'"!=""         local title "`title' (total)"
        else if "`average'"!=""     local title "`title' (average)"
        else                        local title "`title' (density)"
    }
    else if "`subcmd'"=="summarize" {
        eret local slist `"`slist'"'
        eret local stats "`stats'"
        eret scalar N_stats = `N_stats'
        if `N_stats'==1 & ("`over'"!="" | `N_vars'>1) local title "`stats'"
        else local title "Summary statistics"
    }
    eret local title "`title'"

    // generate
    if "`generate'"!="" {
        local vlist
        foreach IF of local IFs {
            gettoken var generate : generate
            if "`var'"=="" continue, break
            capt confirm new variable `var'
            if _rc==1 exit _rc
            if _rc drop `var'
            rename `IF' `var'
            local vlist `vlist' `var'
        }
        eret local generate "`vlist'"
    }
    if "`over'"!="" {
        if "`bal_wvar'"!="" {
            capt confirm new variable `bal_wvar'
            if _rc==1 exit _rc
            if _rc drop `bal_wvar'
            rename `BAL_WVAR' `bal_wvar'
        }
    }
end

program Parse_over
    capt n syntax [varname(numeric default=none)] [, SELect(numlist int >=0) ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:over()}"
        exit _rc
    }
    if "`varlist'"=="" local select ""
    c_local over `varlist'
    c_local select `select'
end

program Parse_uncond
    capt n syntax [, Fixed ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:unconditional()}"
        exit _rc
    }
    if "`fixed'"!="" c_local unconditional unconditional
    c_local unconditional2 `fixed'
end

program Parse_rif
    syntax [anything(name=rif)] [, COMpact QUIetly ]
    c_local rif
    if `"`rif'"'=="" exit
    c_local generate generate(`rif', rif `compact' `quietly')
end

program Parse_generate
    syntax [, generate(str) replace ]
    local 0 `"`generate'"'
    syntax [anything(name=generate)] [, svy rif COMpact QUIetly ]
    if "`svy'"!="" {
        if "`compact'"!="" {
            di as err "{bf:svy} and {bf:compact} not both allowed"
            exit 198
        }
        if "`rif'"!="" {
            di as err "{bf:svy} and {bf:rif} not both allowed"
            exit 198
        }
    }
    if `"`generate'"'=="" {
        c_local generate
        exit
    }
    c_local generate_quietly `quietly'
    c_local svy `svy'
    c_local rif `rif'
    c_local compact `compact'
    if substr(`"`generate'"', -1, 1)=="*" {
        local generate = substr(`"`generate'"', 1, strlen(`"`generate'"')-1)
        confirm name `generate'
        c_local generate_stub `generate'
        exit
    }
    local 0 `" , generate(`generate')"'
    syntax [, generate(namelist) ]
    c_local generate `generate'
    if "`replace'"!="" exit
    foreach v of local generate {
        confirm new var `v'
    }
end

program Parse_at // returns atmat="matrix" if at is matrix, else expands numlist
    args subcmd cat n at
    if `"`at'"'=="" exit
    if `"`n'"'!="" {
        di as err "{bf:n()} and {bf:at()} not both allowed"
        exit 198
    }
    if `: list sizeof at'==1 {
        capt confirm matrix `at'
        if _rc==1 exit _rc
        if _rc==0 {
            c_local atmat "matrix"
            exit
        }
        capt confirm name `at'
        if _rc==1 exit _rc
        if _rc==0 { // is a valid name
            di as err `"at() invalid -- matrix '`at'' not found"'
            exit 111
        }
    }
    local 0 `", at(`at')"'
    if      "`cat'"!=""             local args "numlist int >=0"
    else if "`subcmd'"=="histogram" local args "numlist min=2 ascending"
    else if "`subcmd'"=="quantile"  local args "numlist >=0 <=1"
    else if "`subcmd'"=="lorenz"    local args "numlist >=0 <=1"
    else if "`subcmd'"=="share"     local args "numlist min=2 >=0 <=1 ascending"
    else                            local args "numlist"
    syntax [, at(`args') ]
    c_local at "`at'"
end

program Parse_varlist_unique
    local dups: list dups 0
    if `"`dups'"'!="" {
        di as err "{it:varlist} must be unique"
        exit 198
    }
end

program Parse_hist_n
    capt syntax, n(numlist int >=1 max=1) [ ep ]
    if _rc==1 exit _rc
    if _rc==0 {
        c_local n `n'
        exit
    }
    syntax [, n(str) ep ]
    local hasepopt = "`ep'"!=""
    local 0 `", `n'"'
    syntax [, SQrt STurges RIce DOane SCott fd ep ]
    local n `sqrt' `sturges' `rice' `doane' `scott' `fd' `ep'
    if `:list sizeof n'>1 {
        di ar error "only one {it:rule} allowed in {bf:n()}"
        exit 198
    }
    if "`n'"=="" local n sqrt
    c_local n `n'
end

program Parse_pline
    syntax [, pline(str) ]
    if `"`pline'"'=="" exit
    capt confirm number `pline'
    if _rc==1 exit _rc
    if _rc==0 {
        syntax [, pline(numlist max=1 >0) ]
        c_local pline `"`pline'"'
        exit
    }
    syntax [, pline(varname numeric) ]
    c_local pline `"`pline'"'
    c_local plvar `"`pline'"'
end

program Parse_sum
    // preprocess statistics
    gettoken zvar  0 : 0
    gettoken pline 0 : 0
    gettoken plvar 0 : 0
    local zvars `zvar'      // always include default zvar()
    local plvars `plvar'    // always include default pline() variable
    Parse_sum_tokenize `0'  // returns stats_#, vars_#, n
    mata: dstat_parse_stats(`n')
    // check/collect variables
    local vlist
    forv j=1/`n' {
        local 0 `"`vars_`j''"'
        syntax varlist(numeric fv)
        local vars_`j' `varlist'
        local vlist `vlist' `varlist'
    }
    // returns
    mata: dstat_slist_collect(`n')
    c_local slist `"`slist'"'
    c_local varlist: list uniq vlist
    c_local zvars: list uniq zvars
    c_local plvars: list uniq plvars
end

program Parse_sum_hasdens
    Parse_sum_tokenize `0'  // returns stats_#, vars_#, n
    mata: dstat_parse_stats_hasdens(`n')
    c_local hasdens `hasdens'
end

program Parse_sum_tokenize
    // tokenizes (stats) varlist [ (stats) varlist ... ]
    gettoken tmp : 0, match(par)
    if `"`par'"'!="(" { // no stats specified
        local 0 `"(mean) `0'"'
    }
    local j 0
    local stats
    local vars
    while (`"`0'"'!="") {
        gettoken tmp 0 : 0, match(par)
        // statistics
        if `"`par'"'=="(" {
            if (strtrim(`"`tmp'"')=="") local tmp "mean"
            if `"`stats'"'=="" {
                if `j' {
                    c_local vars_`j' `"`vars'"'
                    local vars
                }
                local ++j
                local stats `"`tmp'"'
                continue
            }
            local stats `"`stats' `tmp'"' // join repeated (stats)
            continue
        }
        // variables
        if `"`vars'"'=="" {
            c_local stats_`j' `"`stats'"'
            local stats
            local vars `"`tmp'"'
            continue
        }
        local vars `"`vars' `tmp'"'
    }
    c_local vars_`j' `"`vars'"'
    c_local n `j'
end

program Parse_slist
    gettoken touse slist : 0
    mata: dstat_slist_expand() // return vlist, slist
    c_local slist `"`slist'"'
    c_local varlist: list uniq vlist
end

program Parse_vce
    gettoken subcmd 0 : 0
    _parse comma vce 0 : 0
    // vce type
    if `"`vce'"'==substr("analytic", 1, strlen(`"`vce'"')) local vce "analytic"
    else if `"`vce'"'!="none" {
        gettoken vce arg : vce
        if `"`vce'"'==substr("cluster", 1, max(2,strlen(`"`vce'"'))) local vce "cluster"
        else {
            di as err `"vce(`vce'`arg') not allowed"'
            exit 198
        }
    }
    // options
    capt n syntax [, NOCOV COV svy]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:vce()}"
        exit _rc
    }
    // vce: none
    if `"`vce'"'=="none" {
        c_local nose "nose"
        c_local vce
        c_local vcetype
        c_local clustvar
        c_local vcenocov
        c_local vcesvy
        exit
    }
    // vce: other
    c_local vce `vce'
    // options
    if "`cov'"!="" & "`nocov'"!="" {
        di as err "{bf:vce()}: only one of {bf:cov} and {bf:nocov} allowed"
        exit 198
    }
    if `"`subcmd'"'!="summarize" & "`cov'"=="" local nocov nocov
    c_local vcenocov `nocov'
    c_local vcesvy `svy'
    // cluster
    if "`vce'"=="cluster" {
        local 0 `"`arg'"'
        capt n syntax varname
        if _rc==1 exit _rc
        if _rc {
            di as err "error in option {bf:vce()}"
            exit 198
        }
        local arg
        c_local vcetype Robust
        c_local clustvar `varlist'
    }
    else {
        c_local vcetype
        c_local clustvar
    }
end

program Parse_densityopts
    syntax [, ///
        BWidth(str) Kernel(string) ADAPTive(numlist int >=0 max=1) ///
        exact NApprox(numlist int >1 max=1) pad(numlist >=0 max=1) ///
        ll(numlist max=1 missingok) ul(numlist max=1 missingok) ///
        BOundary(str) ]
    if "`ll'"=="" local ll .
    if "`ul'"=="" local ul .
    if `ll'<. & `ll'>`ul' {
        di as err "{bf:ll()} may not be larger than {bf:ul}"
        exit 198
    }
    Parse_bwopt `bwidth'
    if `"`kernel'"'=="" local kernel "gaussian"
    if "`adaptive'"=="" local adaptive 0
    Parse_boundary, `boundary'
    if "`napprox'"=="" local napprox 512
    if "`pad'"==""     local pad .1
    c_local bwidth `bwidth'
    c_local bwmat `bwmat'
    c_local bwmethod `bwmethod'
    c_local bwdpi `bwdpi'
    c_local bwadjust `bwadjust'
    c_local bwrd `bwrd'
    c_local kernel `"`kernel'"'
    c_local adaptive `adaptive'
    c_local exact    `exact'
    c_local napprox `napprox'
    c_local pad `pad'
    c_local ll `ll'
    c_local ul `ul'
    c_local boundary `boundary'
end

program Parse_bwopt  // returns: bwidth, bwmat, bwmethod, bwdpi, bwadjust, bwrd
    // bwidth(numlist) => returns bwidth
    capt numlist `"`0'"'
    if _rc==1 exit _rc
    if _rc==0 {
        local 0 `", bwidth(`0')"'
        syntax, bwidth(numlist >0)
        c_local bwidth `"`bwidth'"'
        exit
    }
    // bwidth(matname) => returns bwidth, bwmat
    capt confirm matrix `0'
    if _rc==1 exit _rc
    if _rc==0 {
        mata: st_local("invalid", any(st_matrix("`0'"):<=0) ? "1" : "0")
        if `invalid' {
            di as error "{bf:bwidth()} must be strictly positive"
            exit 198
        }
        c_local bwidth `0'
        c_local bwmat "matrix"
        exit
    }
    // bwidth(method, adjust(#) rd) => returns bwmethod, bwdpi, bwadjust, bwrd
    _parse comma bwidth 0 : 0
    capt n syntax [, ADJust(numlist >0 max=1) rd ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:bwidth()}"
        exit 198
    }
    Parse_bwmethod, `bwidth'
    c_local bwidth
    c_local bwmethod `bwmethod'
    c_local bwdpi `bwdpi'
    c_local bwadjust `adjust'
    c_local bwrd `rd'
end

program Parse_bwmethod  // returns: bwmethod, bwdpi
    capt n syntax [, Silverman Normalscale Oversmoothed SJpi Dpi Dpi2(numlist int >=0 max=1) ISJ ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:bwidth()}"
        exit 198
    }
    if "`dpi2'"!="" local dpi dpi
    local bwmethod `silverman' `normalscale' `oversmoothed' `sjpi' `dpi' `isj'
    if "`bwmethod'"=="" local bwmethod "sjpi"
    if `: list sizeof bwmethod'>1 {
        di as err "too many methods specified"
        di as err "error in option {bf:bwidth()}"
        exit 198
    }
    c_local bwmethod `bwmethod'
    c_local bwdpi `dpi2'
end

program Parse_boundary // returns: boundary
    capt n syntax [, RENorm REFlect lc ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:boundary()}"
        exit 198
    }
    local boundary `renorm' `reflect' `lc'
    if "`boundary'"=="" local boundary "renorm"
    if `: list sizeof boundary'>1 {
        di as err "too many methods specified"
        di as err "error in option {bf:boundary()}"
        exit 198
    }
    c_local boundary `boundary'
end

program Parse_balance
    // parse "[method:] ..."
    _parse comma lhs 0 : 0
    capt _on_colon_parse `lhs'
    if _rc==0 {
        local method `"`s(before)'"'
        local lhs `"`s(after)'"'
    }
    if `"`method'"'=="" local method "ipw"
    if !inlist(`"`method'"', "ipw", "eb") {
        di as err "method '" `"`method'"' "' not allowed"
        di as err "error in option {bf:balance()}"
        exit 198
    }
    // parse "varlist [, options]"
    local 0 `"`lhs'`0'"'
    capt n syntax varlist(fv) [, NOIsily REFerence(numlist int max=1 >=0) ///
        GENerate(name) * ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:balance()}"
        exit 198
    }
    c_local bal_varlist  `"`varlist'"'
    c_local bal_method   `method'
    c_local bal_noisily  `noisily'
    c_local bal_ref      `reference'
    c_local bal_opts     `"`options'"'
    c_local bal_wvar     `generate'
    if "`method'"=="eb" {
        local 0 `", `options'"'
        syntax [, BTOLerance(numlist max=1 >=0) DIFficult ///
            MAXIter(numlist integer max=1 >=0 <=16000) ///
            PTOLerance(numlist max=1 >=0) ///
            VTOLerance(numlist max=1 >=0) ]
        if "`btolerance'"=="" local btolerance 1e-5
        local difficult = "`difficult'"!=""
        c_local bal_ebopts `btolerance' `difficult' /*
            */ "`maxiter'" "`ptolerance'" "`vtolerance'"
    }
end

version 14
// struct
local DATA   dstat_data
local Data   struct `DATA' scalar
local GRP    dstat_grp
local Grp    struct `GRP' scalar
local BAL    dstat_bal
local Bal    struct `BAL' scalar
// string
local SS     string scalar
local SR     string rowvector
local SC     string colvector
local SM     string matrix
// real
local RS     real scalar
local RV     real vector
local RC     real colvector
local RR     real rowvector
local RM     real matrix
// counters
local Int    real scalar
local IntC   real colvector
local IntR   real rowvector
// boolean
local Bool   real scalar
local BoolR  real rowvector
local BoolC  real colvector
local TRUE   1
local FALSE  0
// transmorphic
local T      transmorphic
local TS     transmorphic scalar
local TC     transmorphic colvector
// pointer
local PDF    class mm_density scalar
local PS     pointer scalar
local PR     pointer rowvector
// dstat_stats_lib()
local f &dstat_sum_
local p &_dstat_parse_stats_

mata:
mata set matastrict on

// --------------------------------------------------------------------------
// parsing for -dstat summarize-
// --------------------------------------------------------------------------

void dstat_parse_stats(`Int' n)
{
    `Int' i
    `SS'  nm
    `T'   A
    `SC'  list
    
    A = dstat_stats_lib()
    list = sort(asarray_keys(A), 1)
    for (i=1;i<=n;i++) {
        nm = "stats_" + strofreal(i)
        st_local(nm, _dstat_parse_stats(A, list, 
            _dstat_parse_stats_tok(st_local(nm))))
    }
}

void dstat_parse_stats_hasdens(`Int' n)
{
    `Int' i
    `T'   A
    `SC'  list
    
    A = dstat_stats_lib()
    list = sort(asarray_keys(A), 1)
    for (i=1;i<=n;i++) {
        if (_dstat_parse_stats_hasdens(list, 
            _dstat_parse_stats_tok(st_local("stats_" + strofreal(i))))) {
            st_local("hasdens", "1")
            return
        }
    }
}

`SS' _dstat_parse_stats(`T' A, `SC' list, `SR' stats)
{
    `Int' i, n
    `SS'  s0, s, o
    `PR'  P
    
    n = length(stats)
    for (i=1; i<=n; i++) {
        s0 = s = stats[i]
        o = _dstat_parse_stats_split(s) // replaces s
        P = asarray(A, _dstat_parse_stats_match(s, list, s0))
        stats[i] = (*P[2])(s0, s, o, *P[3])
    }
    return(invtokens(stats))
}

`Bool' _dstat_parse_stats_hasdens(`SC' list, `SR' stats)
{
    `Int' i, n
    `SR'  dens
    `SS'  s0, s
    
    dens = ("d", "density")
    n = length(stats)
    for (i=1; i<=n; i++) {
        s0 = s = stats[i]
        (void) _dstat_parse_stats_split(s) // replaces s
        s = _dstat_parse_stats_match(s, list, s0)
        if (anyof(dens, s)) return(1)
    }
    return(0)
}

`SS' _dstat_parse_stats_match(`SS' s, `SC' list, `SS' s0)
{
    `Int' rc
    `SS'  s1
    pragma unset s1
    
    if (strtrim(s)=="") rc = 1
    else                rc = _mm_strexpand(s1, strlower(s), list)
    if (rc) {
        printf("{err}invalid statistic: %s\n", s0)
        exit(198)
    }
    return(s1)
}

`SR' _dstat_parse_stats_tok(`SS' s)
{
    `Int' i, j, n
    `T'   t
    `SR'  S
    `SR'  p
    
    p = ("{", "(")
    t = tokeninit(" ", "", ("()", "{}"))
    tokenset(t, s)
    S = tokengetall(t)
    n = cols(S)
    j = 1
    for (i=2;i<=n;i++) {
        if (anyof(p, substr(S[i],1,1))) {
            S[j] = S[j] + S[i]
            continue
        }
        S[++j] = S[i]
    }
    if (j<n) S = S[|1\j|]
    return(S)
}

`SS' _dstat_parse_stats_split(`SS' s)
{
    `Int' p
    `SS'  o
    `SS'  rexp
    
    // regex to fetch floating point number at end of string
    // (see https://www.regular-expressions.info/floatingpoint.html)
    rexp = "[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$"
    // without exponents: rexp = "[-+]?[0-9]*\.?[0-9]+$"
    if ((p = strpos(s,"("))) {
        if (substr(s, strlen(s), 1)==")") {
            o = substr(s, p, .)
            s = substr(s, 1, p-1)
        }
    }
    else if (regexm(s, rexp)) {
        o = regexs(0)
        s = substr(s, 1, strlen(s)-strlen(o))
    }
    return(o)
}

`T' dstat_stats_lib()
{
    `T' A
    
    A = asarray_create()
    // general
    asarray(A, "q"       , (`f'q(),        `p'1(),    &(0, 100, .)))            // quantile at #
    asarray(A, "p"       , (`f'p(),        `p'1(),    &(0, 100, .)))            // quantile at #
    asarray(A, "quantile", (`f'quantile(), `p'1(),    &(0, 100, .)))            // quantile at #
    asarray(A, "d"       , (`f'd(),        `p'1(),    &(., ., .)))              // kernel density at #
    asarray(A, "density" , (`f'density(),  `p'1(),    &(., ., .)))              // kernel density at #
    asarray(A, "hist"    , (`f'hist(),     `p'2(),    &(., ., .)))              // histogram density within [#,#]
    asarray(A, "cdf"     , (`f'cdf(),      `p'1(),    &(., ., .)))              // cdf at #
    asarray(A, "cdfm"    , (`f'cdfm(),     `p'1(),    &(., ., .)))              // mid cdf at #
    asarray(A, "cdff"    , (`f'cdff(),     `p'1(),    &(., ., .)))              // floor cdf at #
    asarray(A, "ccdf"    , (`f'ccdf(),     `p'1(),    &(., ., .)))              // complementary cdf at #
    asarray(A, "ccdfm"   , (`f'ccdfm(),    `p'1(),    &(., ., .)))              // mid ccdf at #
    asarray(A, "ccdff"   , (`f'ccdff(),    `p'1(),    &(., ., .)))              // floor ccdf at #
    asarray(A, "prop"    , (`f'prop(),     `p'1or2(), &(., ., .)))              // proportion equal to # or within [#,#]
    asarray(A, "pct"     , (`f'pct(),      `p'1or2(), &(., ., .)))              // percent equal to # or within [#,#]
    asarray(A, "f"       , (`f'f(),        `p'1or2(), &(., ., .)))              // frequency equal to # or within [#,#]
    asarray(A, "freq"    , (`f'freq(),     `p'1or2(), &(., ., .)))              // frequency equal to # or within [#,#]
    asarray(A, "total"   , (`f'total(),    `p'1or2(), &(., ., 0)))              // overall total or total equal to # or within [#,#]
    asarray(A, "min"     , (`f'min(),      `p'0(),    &.))                      //
    asarray(A, "max"     , (`f'max(),      `p'0(),    &.))                      //
    asarray(A, "range"   , (`f'range(),    `p'0(),    &.))                      //
    asarray(A, "midrange", (`f'midrange(), `p'0(),    &.))                      // (max+min)/2
    // location
    asarray(A, "mean"    , (`f'mean(),     `p'0(),    &.))                      // arithmetic mean
    asarray(A, "gmean"   , (`f'gmean(),    `p'0(),    &.))                      // geometric mean
    asarray(A, "hmean"   , (`f'hmean(),    `p'0(),    &.))                      // harmonic mean
    asarray(A, "trim"    , (`f'trim(),     `p'1or2(), &(  0,  50, 25)))         // trimmed mean
    asarray(A, "winsor"  , (`f'winsor(),   `p'1or2(), &(  0,  50, 25)))         // winsorized mean
    asarray(A, "median"  , (`f'median(),   `p'0(),    &.))                      // median
    asarray(A, "huber"   , (`f'huber(),    `p'1(),    &(63.7, 99.9, 95)))       // huber m-estimate of location
    asarray(A, "biweight", (`f'biweight(), `p'1(),    &(0.1, 99.9, 95)))        // biweight m-estimate of location
    asarray(A, "hl"      , (`f'hl(),       `p'0(),    &.))                      // Hodges-Lehmann estimator
    // scale
    asarray(A, "sd"      , (`f'sd(),       `p'1(),    &(., ., 1)))              // standard deviation
    asarray(A, "variance", (`f'variance(), `p'1(),    &(., ., 1)))              // variance
    asarray(A, "mse"     , (`f'mse(),      `p'1or2(), &(., ., 0, 0)))           // mean squared error
    asarray(A, "smse"    , (`f'smse(),     `p'1or2(), &(., ., 0, 0)))           // square-root of MSE
    asarray(A, "iqr"     , (`f'iqr(),      `p'2(),    &(0, 100, 25, 75)))       // inter quantile range
    asarray(A, "iqrn"    , (`f'iqrn(),     `p'0(),    &.))                      // normalized inter quartile range
    asarray(A, "mad"     , (`f'mad(),      `p'1or2(), &(., ., 0, 0)))           // median (or mean) absolute deviation
    asarray(A, "madn"    , (`f'madn(),     `p'1or2(), &(., ., 0, 0)))           // normalized MAD
    asarray(A, "mae"     , (`f'mae(),      `p'1or2(), &(., ., 0, 0)))           // median (or mean) absolute error
    asarray(A, "maen"    , (`f'maen(),     `p'1or2(), &(., ., 0, 0)))           // normalized MAE
    asarray(A, "md"      , (`f'md(),       `p'0(),    &.))                      // mean absolute pairwise difference
    asarray(A, "mdn"     , (`f'mdn(),      `p'0(),    &.))                      // normalized mean absolute pairwise difference
    asarray(A, "mscale"  , (`f'mscale(),   `p'1(),    &(1, 50, 50)))            // m-estimate of scale
    asarray(A, "qn"      , (`f'qn(),       `p'0(),    &.))                      // Qn coefficient
    // skewness
    asarray(A, "skewness", (`f'skewness(), `p'0(),    &.))                      // classical skewness
    asarray(A, "qskew"   , (`f'qskew(),    `p'1(),    &(0, 50, 25)))            // quantile skewness measure
    asarray(A, "mc"      , (`f'mc(),       `p'0(),    &.))                      // medcouple
    // kurtosis
    asarray(A, "kurtosis", (`f'kurtosis(), `p'0(),    &.))                      // classical kurtosis
    asarray(A, "qw"      , (`f'qw(),       `p'1(),    &(0, 50, 25)))            // quantile tail weight
    asarray(A, "lqw"     , (`f'lqw(),      `p'1(),    &(0, 50, 25)))            // left quantile tail weight
    asarray(A, "rqw"     , (`f'rqw(),      `p'1(),    &(0, 50, 25)))            // right quantile tail weight
    asarray(A, "lmc"     , (`f'lmc(),      `p'0(),    &.))                      // left medcouple tail weight
    asarray(A, "rmc"     , (`f'rmc(),      `p'0(),    &.))                      // right medcouple tail weight
    // inequality
    asarray(A, "gini"    , (`f'gini(),     `p'1(),    &(., ., 0)))              // Gini coefficient
    asarray(A, "agini"   , (`f'agini(),    `p'1(),    &(., ., 0)))              // absolute Gini coefficient
    asarray(A, "mld"     , (`f'mld(),      `p'0(),    &.))                      // mean log deviation
    asarray(A, "theil"   , (`f'theil(),    `p'0(),    &.))                      // Theil index
    asarray(A, "cv"      , (`f'cv(),       `p'1(),    &(., ., 1)))              // coefficient of variation
    asarray(A, "ge"      , (`f'ge(),       `p'1(),    &(., ., 1)))              // generalized entropy
    asarray(A, "atkinson", (`f'atkinson(), `p'1(),    &(0, ., 1)))              // Atkinson inequality measure
    asarray(A, "lvar"    , (`f'lvar(),     `p'1(),    &(., ., 1)))              // logarithmic variance
    asarray(A, "vlog"    , (`f'vlog(),     `p'1(),    &(., ., 1)))              // variance of logarithm
    asarray(A, "top"     , (`f'top(),      `p'1(),    &(0, 100, 10)))           // top share
    asarray(A, "bottom"  , (`f'bottom(),   `p'1(),    &(0, 100, 40)))           // bottom share
    asarray(A, "mid"     , (`f'mid(),      `p'2(),    &(0, 100, 40, 90)))       // mid share
    asarray(A, "palma"   , (`f'palma(),    `p'0(),    &.))                      // palma ratio
    asarray(A, "qratio"  , (`f'qratio(),   `p'2(),    &(0, 100, 10, 90)))       // quantile ratio
    asarray(A, "sratio"  , (`f'sratio(),   `p'2or4(), &(0, 100, 0, 10, 90, 100))) // percentile share ratio
    asarray(A, "lorenz"  , (`f'lorenz(),   `p'1(),    &(0, 100, .)))            // lorenz ordinate
    asarray(A, "tlorenz" , (`f'tlorenz(),  `p'1(),    &(0, 100, .)))            // total (sum) lorenz ordinate
    asarray(A, "glorenz" , (`f'glorenz(),  `p'1(),    &(0, 100, .)))            // generalized lorenz ordinate
    asarray(A, "alorenz" , (`f'alorenz(),  `p'1(),    &(0, 100, .)))            // absolute lorenz ordinate
    asarray(A, "elorenz" , (`f'elorenz(),  `p'1(),    &(0, 100, .)))            // equality gap lorenz ordinate
    asarray(A, "share"   , (`f'share(),    `p'2(),    &(0, 100, ., .)))         // percentile share (proportion)
    asarray(A, "dshare"  , (`f'dshare(),   `p'2(),    &(0, 100, ., .)))         // percentile share (density)
    asarray(A, "tshare"  , (`f'tshare(),   `p'2(),    &(0, 100, ., .)))         // percentile share (total/sum)
    asarray(A, "gshare"  , (`f'gshare(),   `p'2(),    &(0, 100, ., .)))         // percentile share (generalized)
    asarray(A, "ashare"  , (`f'ashare(),   `p'2(),    &(0, 100, ., .)))         // percentile share (average)
    // concentration
    asarray(A, "gci"     , (`f'gci(),      `p'z1(),   &(., ., 0)))              // Gini concentration index
    asarray(A, "aci"     , (`f'aci(),      `p'z1(),   &(., ., 0)))              // absolute Gini concentration index
    asarray(A, "ccurve"  , (`f'ccurve(),   `p'1z(),   &(0, 100, .)))            // lorenz concentration ordinate
    asarray(A, "tccurve" , (`f'tccurve(),  `p'1z(),   &(0, 100, .)))            // total concentration ordinate
    asarray(A, "gccurve" , (`f'gccurve(),  `p'1z(),   &(0, 100, .)))            // generalized concentration ordinate
    asarray(A, "accurve" , (`f'accurve(),  `p'1z(),   &(0, 100, .)))            // absolute concentration ordinate
    asarray(A, "eccurve" , (`f'eccurve(),  `p'1z(),   &(0, 100, .)))            // equality gap concentration ordinate
    asarray(A, "cshare"  , (`f'cshare(),   `p'2z(),   &(0, 100, ., .)))         // concentration share
    asarray(A, "dcshare" , (`f'dcshare(),  `p'2z(),   &(0, 100, ., .)))         // concentration share as density
    asarray(A, "gcshare" , (`f'gcshare(),  `p'2z(),   &(0, 100, ., .)))         // generalized concentration share
    asarray(A, "tcshare" , (`f'tcshare(),  `p'2z(),   &(0, 100, ., .)))         // concentration share as total
    asarray(A, "acshare" , (`f'acshare(),  `p'2z(),   &(0, 100, ., .)))         // concentration share as average
        // more concentration measures...
    // poverty
    asarray(A, "watts"   , (`f'watts(),    `p'pl(),   &(0, .)))                 // Watts poverty
    asarray(A, "fgt"     , (`f'fgt(),      `p'1pl(),  &(0., ., 0)))             // FGT poverty measure
        // Sen, Hoover index/robin hood index, TIP
    return(A)
}

`SS' _dstat_parse_stats_0(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name (without arguments)
    pragma unused O
    
    if (o=="") return(s)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`SS' _dstat_parse_stats_1(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name# or name(#)
    if (__dstat_parse_stats_1(o, O[1], O[2], O[3])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_1(`SS' o, `RS' min, `RS' max, `RS' def)
{   // replaces argument o
    `Int'  l
    `RR'   n
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    n = strtoreal(s)
    l = length(n)
    if (l>1) return(0)
    if (!l) {
        if (missing(def)) return(0)
        o = ""
        return(1)
    }
    if (hasmissing(n)) return(0)
    if ((min<. & n[1]<min) | (max<. & n[1]>max)) return(0)
    o = _dstat_parse_stats_brace(s, br)
    return(1)
}

`SS' _dstat_parse_stats_1or2(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name#, name(#), or name(#,#)
    if (__dstat_parse_stats_1or2(o, O[1], O[2], O[|3\.|])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_1or2(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `RR'   n
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    n = strtoreal(s)
    l = length(n)
    if (l>2) return(0)
    if (!l) {
        if (missing(def)) return(0)
        o = ""
        return(1)
    }
    if (hasmissing(n)) return(0)
    if ((min<. & n[1]<min) | (max<. & n[1]>max))  return(0)
    if (l==2) {
        if ((min<. & n[2]<min) | (max<. & n[2]>max)) return(0)
    }
    o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_2(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#,#)
    if (__dstat_parse_stats_2(o, O[1], O[2], O[|3\.|])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_2(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `RR'   n
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    n = strtoreal(s)
    l = length(n)
    if (l>2 | l==1) return(0)
    if (!l) {
        if (missing(def)) return(0)
        o = ""
        return(1)
    }
    if (hasmissing(n)) return(0)
    if ((min<. & n[1]<min) | (max<. & n[1]>max))  return(0)
    if ((min<. & n[2]<min) | (max<. & n[2]>max)) return(0)
    o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_2or4(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#,#) or name(#,#,#,#)
    if (__dstat_parse_stats_2or4(o, O[1], O[2], O[|3\.|])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_2or4(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `RR'   n
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    n = strtoreal(s)
    l = length(n)
    if (l>4 | l==1 | l==3) return(0)
    if (!l) {
        if (missing(def)) return(0)
        o = ""
        return(1)
    }
    if (hasmissing(n)) return(0)
    if ((min<. & n[1]<min) | (max<. & n[1]>max)) return(0)
    if ((min<. & n[2]<min) | (max<. & n[2]>max)) return(0)
    if (l>2) {
        if ((min<. & n[3]<min) | (max<. & n[3]>max)) return(0)
        if ((min<. & n[4]<min) | (max<. & n[4]>max)) return(0)
    }
    o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_z1(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(zvar), name(#), or name(zvar,#)
    if (__dstat_parse_stats_z1(o, O[1], O[2], O[3])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_z1(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `SS'   z, zvars
    `SR'   s
    `RS'   o1
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l==0) {
        z  = st_local("zvar")
        o1 = def
    }
    else if (l==1) {
        o1 = strtoreal(s)
        if (o1<.) z = st_local("zvar")
        else {
            z  = s
            o1 = def
        }
    }
    else if (l==2) {
        z  = s[1]
        o1 = strtoreal(s[2])
    }
    else return(0)
    if (z=="") {
        printf("{err}{it:zvar} or {bf:zvar()} required\n")
        return(0)
    }
    if (z!=st_local("zvar")) {
        if (_st_varindex(z, 1)>=.) {
            printf("{err}variable {bf:%s} not found\n", z)
            return(0)
        }
    }
    zvars = st_local("zvars")
    st_local("zvars", (zvars!="" ? zvars + " " : "") + z)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_1z(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#) or name(#, zvar)
    if (__dstat_parse_stats_1z(o, O[1], O[2], O[3])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_1z(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `SS'   z, zvars
    `RS'   o1
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l<2) {
        if (l==0) o1 = def
        else      o1 = strtoreal(s)
        if (st_local("zvar")=="") {
            printf("{err}{it:zvar} or {bf:zvar()} required\n")
            return(0)
        }
        z  = st_local("zvar")
    }
    else if (l==2) {
        o1 = strtoreal(s[1])
        z  = s[2]
        if (_st_varindex(z, 1)>=.) {
            printf("{err}variable {bf:%s} not found\n", z)
            return(0)
        }
    }
    else return(0)
    zvars = st_local("zvars")
    st_local("zvars", (zvars!="" ? zvars + " " : "") + z)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_2z(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#, #) or name(#, #, zvar)
    if (__dstat_parse_stats_2z(o, O[1], O[2], O[|3\.|])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_2z(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `SS'   z, zvars
    `RS'   o1, o2
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l<3) {
        if (l<1) o1 = def[1]
        else     o1 = strtoreal(s[1])
        if (l<2) o2 = def[2]
        else     o2 = strtoreal(s[2])
        if (st_local("zvar")=="") {
            printf("{err}{it:zvar} or {bf:zvar()} required\n")
            return(0)
        }
        z  = st_local("zvar")
    }
    else if (l==3) {
        o1 = strtoreal(s[1])
        o2 = strtoreal(s[2])
        z  = s[3]
        if (_st_varindex(z, 1)>=.) {
            printf("{err}variable {bf:%s} not found\n", z)
            return(0)
        }
    }
    else return(0)
    zvars = st_local("zvars")
    st_local("zvars", (zvars!="" ? zvars + " " : "") + z)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (o2>=.) return(0)
    if ((min<. & o2<min) | (max<. & o2>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_pl(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name or name(pline)
    if (__dstat_parse_stats_pl(o, O[1], O[2])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_pl(`SS' o, `RS' min, `RS' max)
{   // replaces argument o
    `Int'  l
    `SS'   plv, plvs
    `RS'   pl
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l==0) {
        if (st_local("pline")=="") {
            printf("{err}{it:pline} or {bf:pline()} required\n")
            return(0)
        }
        plv = st_local("plvar")
        o = ""
    }
    else if (l==1) {
        if ((pl = strtoreal(s))<.) {
            if ((min<. & pl<min) | (max<. & pl>max)) return(0)
        }
        else {
            plv = s
            if (_st_varindex(plv, 1)>=.) {
                printf("{err}variable {bf:%s} not found\n", plv)
                return(0)
            }
        }
        o = _dstat_parse_stats_brace(s, br)
    }
    else return(0)
    if (plv!="") {
        plvs = st_local("plvars")
        st_local("plvars", (plvs!="" ? plvs + " " : "") + plv)
    } 
    return(1)
}

`SS' _dstat_parse_stats_1pl(`SS' s0, `SS' s, `SS' o, `RR' O)
{   // name, name(#), or name(#, pline)
    if (__dstat_parse_stats_1pl(o, O[1], O[2], O[3])) return(s+o)
    printf("{err}'%s' not allowed; invalid statistic\n", s0)
    exit(198)
}

`Bool' __dstat_parse_stats_1pl(`SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `SS'   plv, plvs
    `RS'   o1, pl
    `SR'   s
    `Bool' br
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l<2) {
        if (l==0) o1 = def
        else      o1 = strtoreal(s)
        if (st_local("pline")=="") {
            printf("{err}{it:pline} or {bf:pline()} required\n")
            return(0)
        }
        plv = st_local("plvar")
    }
    else if (l==2) {
        o1 = strtoreal(s[1])
        if ((pl = strtoreal(s[2]))<.) {
            if (pl<=0) return(0)
        }
        else {
            plv = s[2]
            if (_st_varindex(plv, 1)>=.) {
                printf("{err}variable {bf:%s} not found\n", plv)
                return(0)
            }
        }
    }
    else return(0)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    if (plv!="") {
        plvs = st_local("plvars")
        st_local("plvars", (plvs!="" ? plvs + " " : "") + plv)
    } 
    return(1)
}

`SR' _dstat_parse_stats_args(`SS' o, `Bool' br)
{
    `SR'   s
    `Int'  r
    `IntR' p

    s = o
    if (substr(s,1,1)=="(") {
        s = substr(s, 2, strlen(s)-2)
        br = `TRUE'
    }
    s = tokens(s, ",")
    if (length(s)) { // remove commas at even positions (except at end)
        r = length(s)
        p = 1..r
        s = strtrim(select(s, s:!="," :| mod(p,2) :| p:==r))
    }
    return(s)
}

`SS' _dstat_parse_stats_brace(`SS' s, `Bool' br)
{
    if (s=="") return(s)
    if (br)                       return("("+s+")")
    if (!(regexm(s, "^[0-9]+$"))) return("("+s+")")
    return(s)
}

void dstat_slist_collect(`Int' n)
{
    `Int' i
    `SR'  slist
    
    slist = J(1, 2*n, "")
    for (i=1;i<=n;i++) {
        slist[2*i-1] = st_local("stats_"+strofreal(i))
        slist[2*i]   = st_local("vars_"+strofreal(i))
    }
    slist = ("`" + `"""') :+ slist :+  (`"""' + "'")
    st_local("slist", invtokens(slist))
}

void dstat_slist_expand()
{
    `Int' i, n, rc
    `SR'  S, V, s, v

    S = tokens(st_local("slist"))
    n = length(S)
    V = S[mm_seq(2, n, 2)]
    S = S[mm_seq(1, n, 2)]
    n = length(S)
    for (i=1;i<=n;i++) {
        rc = _stata("fvexpand " + V[i] + " if \`touse'")
        if (rc) exit(rc)
        v = tokens(st_global("r(varlist)"))
        s = tokens(S[i])
        V[i] = invtokens(mm_expand(v, 1, length(s), 1))
        S[i] = invtokens(J(1,length(v),s))
    }
    st_local("vlist", V = invtokens(V))
    st_local("slist", _dstat_slist_clean((tokens(V)', tokens(invtokens(S))')))
}

`SS' _dstat_slist_clean(`SM' S)
{
    `Int' i, j, k, n

    // step 1: sort groups (keeping original order within groups)
    S = S[_dstat_order_sgrp(S[,1]),]
    // step 2: remove duplicates (keeping original order)
    S = mm_uniqrows(S, 1)
    // step 3: recompile
    n = rows(S)
    k = 0
    for (i=1;i<=n;i++) {
        j = i
        while (S[j,1]==S[i,1]) {
            i++
            if (i>n) break
        }
        i--
        S[++k,1] = "("+invtokens(S[|j,2 \ i,2|]')+") " + S[i,1]
    }
    return(invtokens(S[|1,1 \ k,1|]'))
}

`RC' _dstat_order_sgrp(`SC' S)
{   // sort groups in order of first appearance
    `Int' i, j, n
    `RC'  p
    `T'   A
    
    A = asarray_create()
    n = rows(S)
    p = J(n, 1, .)
    j = 0
    for (i=1;i<=n;i++) {
        if (!asarray_contains(A, S[i])) asarray(A, S[i], ++j)
        p[i] = asarray(A, S[i])
    }
    return(mm_order(p, 1, 1))
}

// --------------------------------------------------------------------------
// support for svy
// --------------------------------------------------------------------------

void dstat_svylbl_b()
{
    `SM' cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    cstripe[,1] = cstripe[,1] :+ "@" :+ cstripe[,2]
    cstripe[,2] = J(rows(cstripe), 1, "_cons")
    dstat_cstripe(st_local("b"), cstripe)
}

void dstat_svylbl_b_undo()
{
    `RC' pos
    `SM' cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    pos = strpos(cstripe[,1], "@")
    cstripe[,2] = substr(cstripe[,1],pos:+1,.)
    cstripe[,1] = substr(cstripe[,1],1,pos:-1)
    st_local("k_eq", strofreal(rows(uniqrows(cstripe[,1]))))
    dstat_cstripe(st_local("b"), cstripe)
}

// --------------------------------------------------------------------------
// computation of transformed CIs
// --------------------------------------------------------------------------

void dstat_Get_CI(`SS' cimat, `RS' level0, `SS' citype, `RC' scale)
{
    `Int' i, r 
    `RS'  level
    `RR'  b, df, se, z
    `RM'  CI
    `SM'  cstripe
    
    // obtain b and se
    b = st_matrix("e(b)")
    cstripe = st_matrixcolstripe("e(b)")
    se = sqrt(diagonal(st_matrix("e(V)")))'
    if (length(se)==0) se = st_matrix("e(se)")
    r = length(b)
    // obtain critical value
    level = 1 - (1 - level0/100)/2
    df = .
    if (st_global("e(mi)")=="mi") {
        if      (st_matrix("e(df_mi)")!=J(0,0,.))   df = st_matrix("e(df_mi)")
        else if (st_numscalar("e(df_r)")!=J(0,0,.)) df = st_numscalar("e(df_r)")
    }
    else if (st_numscalar("e(df_r)")!=J(0,0,.))     df = st_numscalar("e(df_r)")
    if (length(df)==1) z = df>2e17 ? invnormal(level) : invttail(df, 1-level)
    else {
        z = J(1, r, .)
        for (i=1; i<=r; i++) {
            z[i] = df[i]>2e17 ? invnormal(level) : invttail(df[i], 1-level)
        }
    }
    // compute ci
    if (citype=="normal") {
        CI = (b :- z:*se \ b :+ z:*se)
    }
    else {
        if (scale!=1) {
            b = b / scale
            se = se / scale
        }
        if (citype=="logit") { // logit
            z = z :* se :/ (b:* (1 :- b))
            CI = invlogit(logit(b) :- z \ logit(b) :+ z)
        }
        else if (citype=="probit") { // probit
            z = z :* se :/ normalden(invnormal(b))
            CI = normal(invnormal(b) :- z \ invnormal(b) :+ z)
        }
        else if (citype=="atanh") { // atanh
            z = z :* se :/ (1 :- b:^2)
            CI = tanh(atanh(b) :- z \ atanh(b) :+ z)
        }
        else if (citype=="log") { // log
            z = z :* se :/ b
            CI = exp(ln(b) :- z \ ln(b) :+ z) 
        }
        else exit(499)
        if (scale!=1) CI = CI * scale
    }
    // fill in missings
    if (hasmissing(CI)) {
        if (citype!="normal" & scale!=1) b = b * scale
        for (i=1; i<=r; i++) {
            if (CI[1,i]>=.) CI[1,i] = b[i]
            if (CI[2,i]>=.) CI[2,i] = b[i]
        }
    }
    // return result
    st_matrix(cimat, CI)
    dstat_cstripe(cimat, cstripe)
}

// --------------------------------------------------------------------------
// helper function for labeling compact IFs
// --------------------------------------------------------------------------

void dstat_fix_nm(`SS' nm, `SS' id, `Bool' total)
{
    `SS' s
    `Int' p
    
    if (id==".") return // total
    s = st_local(nm)
    if ((p = strpos(s, ":"))) { // has equation
        if (substr(s, 1, p)==(id+":")) {
            if (total) s = "#"+substr(s, p, .)
            else       s = substr(s, p+1, .)
        }
        else {
            if (total) s = "#"+substr(s, strpos(s, "_"), .)
            else       s = substr(s, strpos(s, "_")+1, .)
        }
    }
    else s = "#"  // gid = coefficient name
    st_local(nm, s)
}

// --------------------------------------------------------------------------
// helper functions for graph
// --------------------------------------------------------------------------

void dstat_graph_swap(`Int' sub)
{
    `IntC' p
    `SC'   eqs
    `SM'   S
    
    S = st_matrixcolstripe(st_local("B"))
    // swap equations and coefficients
    if (sub==0) S = S[,(2,1)] // simple case: swap equations and coefficients
    else {
        eqs = S[,1]
        p = strpos(eqs, "~")
        // swap subequations and coefficients
        if (sub==2) S = (substr(eqs, 1, p) :+ S[,2], substr(eqs, p:+1, .))
        // swap main equations and coefficients
        else S = (S[,2] :+ substr(eqs, p, .), substr(eqs, 1, p:-1))
    }
    if (sub!=2) {
        if (st_local("overeq")=="1") {
            S[,2] = mm_cond(S[,2]:!="total", 
                S[,2] :+ ("."+ st_global("e(over)")), S[,2])
            st_local("overeq", "0")
        }
    }
    // store
    st_matrixcolstripe(st_local("B"), S)
    if (st_local("CI")!="") st_matrixcolstripe(st_local("CI"), S)
}

void dstat_graph_eqsplit()
{
    `SR'   eqs
    `IntR' p
    
    eqs = tokens(st_local("eqs"))
    p   = strpos(eqs, "~")
    st_local("subeqs", invtokens(substr(eqs, p:+1, .)))
    st_local("eqs", invtokens(substr(eqs, 1, p:-1)))
}

void dstat_graph_droplast(`SR' M)
{   // remove last element from each equation
    `Int'  i
    `IntR' p
    `SM'   S
    
    S = st_matrixcolstripe(M[1])
    p = selectindex(!_mm_unique_tag(S[,1], 1))
    S = S[p,]
    for (i=length(M); i; i--) {
        st_matrix(M[i], st_matrix(M[i])[,p])
        st_matrixcolstripe(M[i], S)
    }
}

void dstat_graph_select(`SS' list, `SS' select)
{
    `Int'  n
    `IntR' p
    `SR'   L
    
    L = tokens(st_local(list))
    n = length(L)
    p = strtoreal(tokens(st_local(select)))
    p = select(p, p:<=n)
    if (length(p)==0) {
        st_local(list, "")
        return
    }
    L = L[p]
    st_local(list, invtokens(L))
}

// --------------------------------------------------------------------------
// helper function for setting row/column stripes using Stata 14 behavior
// --------------------------------------------------------------------------

void dstat_cstripe(`SS' nm, `SM' S) _dstat_cstripe(nm, S, 0)

void dstat_rstripe(`SS' nm, `SM' S) _dstat_cstripe(nm, S, 1)

void _dstat_cstripe(`SS' nm, `SM' S, `Bool' row)
{
    `RS' br, uv

    br = setbreakintr(0)
    uv = st_numscalar("c(userversion)")
    if (uv>14.2) stata("version 14.2, user")
    if (row) st_matrixrowstripe(nm, S)
    else     st_matrixcolstripe(nm, S)
    if (uv>14.2) stata("version "+strofreal(uv)+", user")
    (void) setbreakintr(br)
}

// --------------------------------------------------------------------------
// dstat core
// --------------------------------------------------------------------------

struct `BAL' {
    `SS'    zvars      // names of balancing variables
    `SS'    method     // balancing method
    `Int'   ref        // value of reference group
    `Bool'  noisily    // display output
    `SS'    opts       // balancing options
    `RR'    ebopts     // entropy balancing options
    `RC'    wvar       // view on variable to store balancing weights
}

struct `GRP' {
    `Int'   id         // value of group
    `IntC'  p          // observations of current group
    `Int'   N          // number of obs
    `RC'    w          // total weight (including balancing)
    `RS'    W          // sum of weights
    `RC'    wb         // total weight / base weight
    `RC'    wc         // total weight - "pooled" component
    `RM'    Z          // current group's balancing variables
    `RM'    IFZ        // current group's balancing IFs
    `IntC'  p1         // observations of reference group
    `RM'    IFZ1       // reference group's balancing IFs
    // current variable
    `RC'    j          // counter of current variable
    `RC'    k          // counter of current group x variable
    `RC'    Y          // current variable
    `RS'    m          // mean of current variable
    `RC'    Ys         // sorted current variable
    `Int'   z          // optional variable determining sort order of Y
    `RC'    Zs         // sorted z
    `RC'    ws         // sorted weights
    `RC'    ps         // permutation vector
    `RC'    Yc         // cumulative outcome at unique Y (for lorenz ordinates)
    `RC'    Wc         // cumulative weights at unique Y (for lorenz ordinates)
    `Int'   Nc         // rows of Yc
    `RC'    EY         // E(Y|Z) (for IFs of concentration curve)
    `RC'    EYat       // evaluation points of E(Y|Z)
}

struct `DATA' {
    `Bool'  nose       // do not compute standard errors
    `Bool'  noIF       // do not compute influence functions
    `Int'   touse      // Stata variable marking sample
    `SR'    yvars      // names of outcome variables
    `RM'    Y          // view on outcome data
    `Int'   nvars      // number of outcome variables
    `Int'   N          // number of obs
    `SS'    wvar       // name of variable containing weights
    `RC'    w          // view on weights
    `RS'    W          // sum of weights
    `Int'   wtype      // weights: 0 none, 1 fw, 2 iw, 3 pw
    `SS'    ovar       // name of over variable
    `RC'    over       // view on over() variable
    `RR'    overlevels // levels of over()
    `Int'   nover      // number of over groups
    `Bool'  total      // include total across over groups
    `IntR'  _N         // number of obs per group
    `RR'    _W         // sum of weights per group
    `Bal'   bal        // balancing settings
    `RM'    IF         // view on influence functions
    `Int'   K          // length of results vector
    `RC'    b          // vector estimates
    `BoolC' omit       // flags omitted estimates (summarize only)
    `BoolC' istot      // flag estimates that are totals
    `RC'    id         // ID of relevant subsample (group) for each estimate
    `SM'    cstripe    // column stripe for results
    `SR'    eqs        // equation names
    // command-specific settings
    `SS'    cmd        // subcommand
    `Bool'  novalues   // do not use values as coefficient names
    `SS'    vfmt       // display format for values used as coefficient names
    `Int'   qdef       // quantile definition
    `IntR'  n          // number of evaluation points (if relevant)
    `RM'    at         // vector of evaluation points (if relevant)
    `PDF'   S          // density estimation object (density, summarize)
    `Bool'  exact      // use exact density estimator (density, summarize)
    `RR'    bwidth     // container for bandwidth matrix (density, summarize)
    `PR'    AT         // pointer rowvector of sets of evaluation points
    `Bool'  ipolate    // interpolate ECDF (cdf/ccdf)
    `Bool'  mid        // midpoint adjustment (cdf/ccdf)
    `Bool'  floor      // use lower-than definition (cdf/ccdf)
    `Bool'  discr      // discrete (cdf/ccdf)
    `Bool'  cat        // categorical (prop)
    `Bool'  prop       // report proportions (histogram, share)
    `Bool'  pct        // report percent (histogram, proportion, cdf, ccdf, share, lorenz)
    `Int'   uncond     // 0 = condition on subsample, 1 = condition on total sample, 
                       // 2 = like 1 but assume subsample size fixed (density, histogram, 
                       // proportion, cdf, ccdf)
    `Bool'  freq       // report frequencies (histogram, proportion, cdf, ccdf)
    `Bool'  ep         // use equal probability bins (histogram)
    `Bool'  gl         // generalized lorenz ordinates (lorenz, share)
    `Bool'  gap        // equality gap curve (lorenz)
    `Bool'  sum        // total (lorenz, share)
    `Bool'  abs        // absolute (lorenz)
    `Bool'  ave        // average (share)
    `SS'    zvar       // default auxiliary variable (summarize, lorenz, share)
    `SR'    zvars      // names of auxiliary variables (summarize, lorenz, share)
    `RM'    Z          // view on auxiliary variables (summarize, lorenz, share)
    `RS'    pline      // default poverty line (summarize)
    `SS'    plvar      // default poverty line variable (summarize)
    `SR'    plvars     // names of poverty line variables (summarize)
    `RM'    PL         // view on poverty line variables (summarize)
}

void dstat()
{
    `Data' D
    `SC'   stats
    
    // settings
    D.nose     = st_local("nose")!=""
    D.noIF     = D.nose & (st_local("generate")=="")
    D.cmd      = st_local("subcmd")
    D.novalues = st_local("novalues")!=""
    D.vfmt     = st_local("vformat")
    D.qdef     = strtoreal(st_local("qdef"))
    D.ipolate  = st_local("ipolate")!=""
    D.mid      = st_local("mid")!=""
    D.floor    = st_local("floor")!=""
    D.discr    = st_local("discrete")!=""
    D.cat      = st_local("categorical")!=""
    D.prop     = st_local("proportion")!=""
    D.pct      = st_local("percent")!=""
    D.freq     = st_local("frequency")!=""
    D.ep       = st_local("ep")!=""
    D.gl       = st_local("generalized")!=""
    D.gap      = st_local("gap")!=""
    D.sum      = st_local("sum")!=""
    D.abs      = st_local("absolute")!=""
    D.ave      = st_local("average")!=""
    
    // get data
    D.touse = st_varindex(st_local("touse"))
    D.yvars = tokens(st_local("varlist"))
    st_view(D.Y, ., D.yvars, D.touse)
    D.N     = rows(D.Y)
    D.nvars = cols(D.Y)
    D.wtype = (st_local("weight")=="fweight" ? 1 :
              (st_local("weight")=="iweight" ? 2 :
              (st_local("weight")=="pweight" ? 3 : 0)))
              // iweights are treated like pweights for computation of
              // statistics and IFs; for VCE they are treated like
              // (non-integer) fweights (consistent with how -mean- and 
              // -total- treat iweights)
    if (D.wtype) {
        D.wvar = st_local("wvar")
        st_view(D.w, ., D.wvar, D.touse)
        D.W = quadsum(D.w)
    }
    else {
        D.w = 1
        D.W = D.N
    }
    st_numscalar(st_local("W"), D.W)
    D.zvar  = st_local("zvar")
    D.zvars = tokens(st_local("zvars"))
    if (length(D.zvars)) st_view(D.Z, ., D.zvars, D.touse)
    D.pline  = strtoreal(st_local("pline"))
    D.plvar  = st_local("plvar")
    D.plvars = tokens(st_local("plvars"))
    if (length(D.plvars)) st_view(D.PL, ., D.plvars, D.touse)
    
    // over
    D.ovar = st_local("over")
    if (D.ovar!="") {
        st_view(D.over, ., D.ovar, D.touse)
        D.overlevels = strtoreal(tokens(st_local("overlevels")))
        D.nover  = cols(D.overlevels)
        D.total  = st_local("total")!=""
        D.nover  = D.nover + D.total
        D.uncond = (st_local("unconditional")!="") + (st_local("unconditional2")!="")
    }
    else {
        D.nover  = 1
        D.total  = 0
        D.uncond = 0
    }
    D._N = D._W = J(1, D.nover, .)
    
    // balancing settings
    D.bal.method = st_local("bal_method")
    if (D.bal.method!="") {
        D.bal.zvars = st_local("bal_varlist2")
        D.bal.ref     = strtoreal(st_local("bal_ref"))
        D.bal.noisily = st_local("bal_noisily")!=""
        D.bal.opts    = st_local("bal_opts")
        D.bal.ebopts  = strtoreal(tokens(st_local("bal_ebopts")))
        st_view(D.bal.wvar, .,  st_local("BAL_WVAR"), D.touse)
    }
    
    // collect evaluation point/list of statistics
    if (D.cmd=="histogram")  D.n = J(1, D.nvars*D.nover, .)
    else                     D.n = .
    dstat_get_at(D)
    
    // determine dimension of results vector and initialize containers
    dstat_set_K(D)
    
    // density estimation setup
    if (anyof(("density","quantile","summarize"), D.cmd)) {
        D.S.kernel(st_local("kernel"), strtoreal(st_local("adaptive")))
        st_local("kernel", D.S.kernel())
        D.S.support(strtoreal((st_local("ll"), st_local("ul"))), 
            st_local("boundary")=="lc" ? "linear" : st_local("boundary"),
            st_local("bwrd")!="")
        D.S.n(strtoreal(st_local("napprox")), strtoreal(st_local("pad")))
        D.exact = st_local("exact")!=""
        if (st_local("bwidth")!="") {
            if (st_local("bwmat")!="") D.bwidth = st_matrix(st_local("bwidth"))[1,]
            else D.bwidth = strtoreal(tokens(st_local("bwidth")))
            D.bwidth = J(1, ceil(D.nvars*D.nover/cols(D.bwidth)), 
                D.bwidth)[|1 \ D.nvars*D.nover|]
        }
        else {
            D.S.bw(st_local("bwmethod"), strtoreal(st_local("bwadjust")), 
                strtoreal(st_local("bwdpi")))
            D.bwidth = J(1, D.nvars*D.nover, .)
        }
    }
    
    // estimation
    if      (D.cmd=="density")    dstat_density(D)
    else if (D.cmd=="histogram")  dstat_hist(D)
    else if (D.cmd=="proportion") dstat_prop(D)
    else if (D.cmd=="cdf")        dstat_cdf(D, 0)
    else if (D.cmd=="ccdf")       dstat_cdf(D, 1)
    else if (D.cmd=="quantile")   dstat_quantile(D)
    else if (D.cmd=="lorenz")     dstat_lorenz(D)
    else if (D.cmd=="share")      dstat_share(D)
    else if (D.cmd=="summarize")  dstat_sum(D)
    
    // return results
    // - b, id, at, k_eq, k_omit
    st_matrix(st_local("b"), D.b')
    st_matrix(st_local("id"), D.id')
    if (st_local("svy")!="") st_matrix(st_local("istot"), D.istot')
    if (D.cmd=="summarize") {
        stats = mm_unique(D.cstripe[,2], 1) // uniq list of statistics
        st_local("stats", invtokens(stats'))
        st_local("N_stats", strofreal(length(stats)))
        if (length(D.eqs)>1 & length(D.eqs)==rows(D.cstripe)) {
            if (length(stats)==1) {
                if (D.ovar!="" & D.nvars>1) {
                    D.cstripe = 
                        substr(D.cstripe[,1], 1, strpos(D.cstripe[,1],"~"):-1),
                        substr(D.cstripe[,1], strpos(D.cstripe[,1],"~"):+1, .)
                }
                else if (D.ovar!="") D.cstripe = (J(cols(D.eqs),1,""), 
                    mm_cond(D.eqs:!="total", D.eqs:+("."+D.ovar), D.eqs)')
                else D.cstripe = (J(cols(D.eqs),1,""), D.eqs')
            }
        }
        if (any(D.omit)) D.cstripe[,2] = "o.":*D.omit + D.cstripe[,2]
        st_local("k_eq", strofreal(mm_nunique(D.cstripe[,1])))
    }
    else {
        //if (any(D.omit)) D.cstripe[,2] = "o.":*D.omit + D.cstripe[,2]
        st_local("k_eq", strofreal(length(D.eqs)))
        st_matrix(st_local("AT"), D.at')
        dstat_cstripe(st_local("AT"), D.cstripe)
        if (D.cmd=="histogram" | D.cmd=="share") {
            dstat_rstripe(st_local("AT"), (J(3,1,""), ("ll", "mid", "h")'))
        }
    }
    dstat_cstripe(st_local("b"), D.cstripe)
    dstat_cstripe(st_local("id"), D.cstripe)
    st_local("k_omit", strofreal(sum(D.omit)))
    // - bwidth
    if (length(D.bwidth)) {
        st_matrix(st_local("BW"), D.bwidth)
        dstat_cstripe(st_local("BW"), (D.eqs', J(cols(D.eqs),1,"bwidth")))
    }
    // - _N and _W
    D.eqs = strofreal(D.overlevels)
    if (D.total | length(D.eqs)==0) D.eqs = D.eqs, "total"
    st_matrix(st_local("_N"), D._N)
    dstat_cstripe(st_local("_N"), (J(cols(D.eqs),1,""), D.eqs'))
    st_matrix(st_local("_W"), D._W)
    dstat_cstripe(st_local("_W"), (J(cols(D.eqs),1,""), D.eqs'))
    
    // check IFs and compute VCE
    if (D.noIF) return
    if (hasmissing(D.IF)) {
        D.IF[.,.] = editmissing(D.IF, 0)
        display("{err}warning: influence function(s) contain missing;" + 
            " missing reset to zero")
    }
    dstat_recenter_IF(D)
    if (D.nose) return
    dstat_vce(D, st_local("clustvar"), st_local("vcenocov")!="", 
        st_local("vcesvy")!="")
}

void dstat_vce(`Data' D, `SS' clust, `Bool' seonly, `Bool' svy)
{   // svy assumed fleeting
    `Bool' undo
    `RS'   df_r, c, N_clust
    `RR'   m
    `RM'   V
    
    // transform IFs due to svy option (if needed)
    undo = 0
    if (svy) {
        if (allof(D.istot, 0)) svy = 0
    }
    if (svy) {
        _dstat_vce_score(D, 0)
        m = (D.b :* D.istot)'
        // check whether transformation must be undone at end: no need if 
        // generate()/rif() not specified or if generate(..., svy) specified
        if (st_local("generate")!="") {
            if (st_local("svy")=="") undo = 1
            else st_local("svy", "done")
        }
    }
    // no clusters
    if (clust=="") {
        if (D.wtype==0) { // no weights
            if (svy) {
                if (seonly) V = _dstat_vce_cross(1, D.IF, m/D.N)
                else        V = _dstat_vce_cdev(D.IF, m/D.N)
            }
            else {
                if (seonly) V = _dstat_vce_cross(1, D.IF)
                else        V = cross(D.IF, D.IF)
            }
            df_r = D.N - 1
            c = (df_r>0 & df_r<. ? D.N/df_r : 0)
        }
        else if (D.wtype==3) { // pw
            if (svy) {
                if (seonly) V = _dstat_vce_cross2(D.w, D.IF, m/D.N)
                else        V = _dstat_vce_cdev(D.w:*D.IF, m/D.N)
            }
            else {
                if (seonly) V = _dstat_vce_cross(D.w:^2, D.IF)
                else        V = cross(D.IF, D.w:^2, D.IF)
            }
            df_r = D.N - 1
            c = (df_r>0 & df_r<. ? D.N/df_r : 0)
        }
        else { // iw or fw
            if (svy) {
                if (seonly) V = _dstat_vce_cross(D.w, D.IF, m/D.W)
                else        V = _dstat_vce_cdev(D.IF, m/D.W, D.w)
            }
            else {
                if (seonly) V = _dstat_vce_cross(D.w, D.IF)
                else        V = cross(D.IF, D.w, D.IF)
            }
            df_r = D.W - 1
            c = (df_r>0 & df_r<. ? D.W/df_r : 0)
        }
        V = V * c
    }
    // with clusters
    else {
        V = _dstat_vce_csum(D.IF, D.w, (st_isstrvar(clust) ? 
            st_sdata(., clust, D.touse) : st_data(., clust, D.touse)))
        N_clust = rows(V)
        if (svy) {
            if (seonly) V = _dstat_vce_cross(1, V, m/N_clust)
            else        V = _dstat_vce_cdev(V, m/N_clust)
        }
        else {
            if (seonly) V = _dstat_vce_cross(1, V)
            else        V = cross(V, V)
        }
        df_r = N_clust - 1
        V = V * (df_r>0 & df_r<. ? N_clust/df_r : 0)
        st_local("N_clust", st_tempname())
        st_numscalar(st_local("N_clust"), N_clust)
    }
    // undo transform IFs due to svy option
    if (undo) _dstat_vce_score(D, 1)
    // return results
    st_local("df_r", st_tempname())
    st_numscalar(st_local("df_r"), df_r)
    if (seonly) {
        st_local("SE", st_tempname())
        st_matrix(st_local("SE"), sqrt(V))
        dstat_cstripe(st_local("SE"), D.cstripe)
    }
    else {
        st_local("V", st_tempname())
        st_matrix(st_local("V"), V)
        dstat_cstripe(st_local("V"), D.cstripe)
        dstat_rstripe(st_local("V"), D.cstripe)
    }
}

void _dstat_vce_score(`Data' D, `Bool' undo)
{
    `Int'  i, id, id0, j
    `IntC' p
    `RC'   b
    
    if (undo) b = -D.b
    else      b =  D.b
    i   = cols(D.IF)
    id0 = .a // D.id cannot be .a
    j   = cols(D._W) + 1
    for (;i;i--) {
        id = D.id[i]
        if (id!=id0) {
            j--
            p = J(0,1,.) // reset group index
            id0 = id
        }
        if (!D.istot[i]) continue
        if (id==.) {
            D.IF[,i] = D.IF[,i] :+ b[i]/D.W
            continue
        }
        if (rows(p)==0) p = selectindex(D.over:==id)
        D.IF[p,i] = D.IF[p,i] :+ b[i]/D._W[j]
    }
}

`RR' _dstat_vce_cross(`RC' w, `RM' X, | `RR' m)
{   // applies cross() to each column individually so that less memory is used
    `Int' i
    `RR'  V
    
    i = cols(X)
    V = J(1,i,.)
    if (args()<3) {
        for (;i;i--) V[i] = cross(w, X[,i]:^2)
    }
    else {
        for (;i;i--) {
            if (m[i]!=0) V[i] = cross(w, (X[,i]:-m[i]):^2)
            else         V[i] = cross(w, X[,i]:^2)
        }
    }
    return(V)
}

`RR' _dstat_vce_cross2(`RC' w, `RM' X, `RR' m)
{   // like _dstat_vce_cross(), but for m!=0 with pweights 
    `Int' i
    `RR'  V
    
    i = cols(X)
    V = J(1,i,.)
    for (;i;i--) {
        if (m[i]!=0) V[i] = cross(1, (w:*X[,i] :- m[i]):^2)
        else         V[i] = cross(1, (w:*X[,i]):^2)
    }
    return(V)
}

`RM' _dstat_vce_cdev(`RM' X, `RR' m, | `RC' w)
{
    if (args()<3) return(crossdev(X, m, X, m))
    return(crossdev(X, m, w, X, m))
}

`RM' _dstat_vce_csum(`RM' X, `RC' w, `TC' C)
{   // aggregate X*w by clusters
    `Int' i, a, b
    `RC'  p, nc
    `RM'  S
    
    p  = order(C, 1)
    nc = selectindex(_mm_unique_tag(C[p])) // tag first obs in each cluster
    i  = rows(nc)
    S  = J(i, cols(X), .)
    a  = rows(C) + 1
    if (rows(w)==1) {
        for (;i;i--) {
            b = a - 1
            a = nc[i]
            S[i,.] = cross(w, X[p[|a\b|],.]) 
        }
    }
    else {
        for (;i;i--) {
            b = a - 1
            a = nc[i]
            S[i,.] = cross(w[p[|a\b|]], X[p[|a\b|],.])
        }
    }
    return(S)
}

void dstat_get_at(`Data' D)
{
    D.AT = NULL
    if (D.cmd=="summarize") dstat_get_stats(D)
    else if (st_local("atmat")!="") _dstat_get_atmat(D, st_local("at"))
    else if (st_local("at")!="") D.AT = &(strtoreal(tokens(st_local("at")))')
    else if (D.cmd=="proportion")     dstat_get_levels(D)
    else if (D.cmd=="cdf"  & D.discr) dstat_get_levels(D)
    else if (D.cmd=="ccdf" & D.discr) dstat_get_levels(D)
    else if (D.cmd=="histogram") dstat_get_hist_n(D, st_local("n"))
    else if (st_local("n")!="") {
        D.n = strtoreal(st_local("n"))
        if      (D.cmd=="quantile") D.AT = &((1::D.n)   / (D.n+1))
        else if (D.cmd=="lorenz")   D.AT = &((0::D.n-1) / (D.n-1))
        else if (D.cmd=="share")    D.AT = &((0::D.n) / D.n)
    }
    D.AT = J(1, ceil(D.nvars*D.nover/cols(D.AT)), D.AT)[|1 \ D.nvars*D.nover|]
}

void _dstat_get_atmat(`Data' D, `SS' matnm)
{   // splits vector by equations
    `Int'  i
    `RR'   M
    `SR'   eqs
    `IntR' from, to

    M = st_matrix(matnm)[1,.]
    if (D.cat) { // proportion
        assert(M==trunc(M)) // integer
        assert(all(M:>=0))  // positive
    }
    else if (anyof(("quantile", "lorenz", "share"), D.cmd)) {
        assert(all(M:>=0 :& M:<=1))  // within [0,1]
    }
    eqs  = st_matrixcolstripe(matnm)[.,1]'
    from = selectindex(_mm_unique_tag(eqs, 0))
    to   = selectindex(_mm_unique_tag(eqs, 1))
    i    = length(from)
    D.AT = J(1, i, NULL)
    for (;i;i--) {
        if (D.cmd=="histogram" | D.cmd=="share") {
            // ul must not be smaller than ll
            assert(all(mm_diff(M[|from[i] \ to[i]|]):>=0))
        }
        D.AT[i] = &(M[|from[i] \ to[i]|]')
    }
}

void dstat_get_stats(`Data' D)
{
    `Int' j
    `SS'  s
    `T'   t
    
    t = tokeninit(" ", "", "()")
    tokenset(t, st_local("slist"))
    D.AT = J(1, length(D.yvars), NULL)
    j = 0
    while ((s = tokenget(t)) != "") {
        D.AT[++j] = &(tokens(substr(s,2,strlen(s)-2))')
        s = tokenget(t)
        assert(s==D.yvars[j])
    }
}

void dstat_get_levels(`Data' D)
{   // obtains levels of each Y across full sample
    `Int' j

    D.AT = J(1, length(D.yvars), NULL)
    for (j=1;j<=D.nvars;j++) {
        D.AT[j] = &(mm_unique(D.Y[,j])) 
        //D.AT[j] = &(_dstat_get_levels(D.Y[,j]))
        if (!D.cat) continue
        if (any(*D.AT[j]:<0)) {
            printf("{err}%s: negative values not allowed in this context\n", D.yvars[j])
            exit(452)
        }
        if (any(*D.AT[j]:!=trunc(*D.AT[j]))) {
            printf("{err}%s: noninteger values not allowed in this context\n", D.yvars[j])
            exit(452)
        }
    }
}

/*
`RC' _dstat_get_levels(`RC' Y)
{   // obtain levels of Y without sorting; not faster than mm_unique() even
    // on large datasets of 1 mio observations or so
    `RC'  y
    `Int' i
    `T'   A
    
    A = asarray_create("real")
    for (i=rows(Y); i; i--) {
        y = Y[i]
        if (!asarray_contains(A, y)) asarray(A, y, .)
    }
    return(sort(asarray_keys(A),1))
}
*/

void dstat_get_hist_n(`Data' D, `SS' rule)
{
    `Int' i, j, n
    `RS'  N, h
    `Grp' G
    
    if (strtoreal(rule)<.) {   // n=#
        D.n = J(1, D.nvars*D.nover, strtoreal(rule))
        return
    }
    if (rule=="") rule = "sqrt"
    G.k = 0
    for (i=1; i<=D.nover; i++) {
        _dstat_init_grp(D, G, i)
        if (D.wtype==1) N = G.W
        else            N = G.N
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            if (rule=="sqrt") {
                n = ceil(min((sqrt(N), 10*ln(N)/ln(10)))) - 1
                // this is the rule used by official Stata's -histogram-; for the 
                // other rules below see https://en.wikipedia.org/wiki/Histogram
            }
            else if (rule=="sturges") {
                n = ceil(ln(N)/ln(2))
            }
            else if (rule=="rice") {
                n = ceil(2 * N^(1/3)) - 1
            }
            else if (rule=="doane") {
                n = ceil(ln(N)/ln(2) + ln(1 + abs(_dstat_skewness(G.Y, G.w)) / 
                    sqrt(6 * (N-2) / ((N+1) * (N+3))))/ln(2))
            }
            else if (rule=="scott") {
                h = 3.5 * sqrt(_dstat_variance(D, G, G.Y)) / N^(1/3)
                n = ceil(_dstat_range(G.Y)/h) - 1
            }
            else if (rule=="fd") {
                h = 2 * mm_iqrange(G.Y, G.w) / N^(1/3)
                n = ceil(_dstat_range(G.Y)/h) - 1
            }
            else if (rule=="ep") {
                n = ceil(2 * N^(2/5))
            }
            else exit(499)
            if (n<1) n = 1 // make sure that never zero
            D.n[G.k] = n
        }
    }
}

`RS' _dstat_range(`RC' Y)
{
    `RR' minmax
    
    minmax = minmax(Y)
    return(minmax[2] - minmax[1])
}

void dstat_set_K(`Data' D)
{
    `Int' k, i, j
    
    // determine K
    if (D.AT[1]==NULL) {
        if (D.cmd=="histogram") D.K = sum(D.n:+1)
        else D.K = D.nover * D.nvars * D.n
    }
    else {
        D.K = k = 0
        for (i=1; i<=D.nover; i++) {
            for (j=1; j<=D.nvars; j++) {
                k++
                D.K = D.K + rows(*D.AT[k])
            }
        }
    }
    // initialize containers
    D.b = D.id = J(D.K, 1, .)
    D.omit = J(D.K, 1, 0)
    if (D.freq | D.sum) D.istot = J(D.K, 1, 1)
    else                D.istot = J(D.K, 1, 0)
    if (length(D.AT)) {
        if (D.cmd=="histogram" | D.cmd=="share") D.at = J(D.K, 3, .)
        else                                     D.at = J(D.K, 1, .)
    }
    D.cstripe = J(D.K, 2, "")
    D.eqs = J(1, D.nover*D.nvars, "")
    if (D.noIF==0) dstat_init_IF(D)
}

void dstat_init_IF(`Data' D)
{
    `SR' vnm
    
    vnm = st_tempname(D.K)
    st_view(D.IF, ., st_addvar("double", vnm), D.touse)
    st_local("IFs", invtokens(vnm))
}

void dstat_set_IF(`Data' D, `Grp' G, `Int' k, `RC' h, | `RS' h0)
{
    if (args()<5) h0 = 0
    D.IF[,k] = _dstat_set_IF(D, G, h, h0)
}

`RC' _dstat_set_IF(`Data' D, `Grp' G, `RC' h, `RS' h0)
{
    `RR' delta
    `RC' IF
    
    IF = J(D.N, 1, h0)
    if (cols(G.Z)) { // has balancing
        // (assuming that G.p and G.p1 are distinct)
        delta = quadcolsum(G.wc :* h :* G.Z)'
        IF[G.p ] = G.wb :* h + G.IFZ  * delta
        if (h0) IF[G.p1] =  IF[G.p1] + G.IFZ1 * delta
        else    IF[G.p1] =             G.IFZ1 * delta
    }
    else {
        IF[G.p] = h
    }
    return(IF)
}

void dstat_recenter_IF(`Data' D)
{
    `RS'  mrd, tol, tol0
    `RR'  m
    
    tol  = 1e-6     // issue warning if relative error is larger than 1e-6
    tol0 = 1e-14    // recenter IFs if relative error is larger than 1e-14
    m   = quadcross(D.w, D.IF)
    mrd = mreldif(m+D.b', D.b')
    if (mrd>tol) {
        display("{err}warning: total of influence function(s) deviates from zero")
        printf("{err}         maximum relative error = %g\n", mrd)
    }
    if (mrd>tol0) {
        D.IF[.,.] = D.IF :- m/D.W
    }
    if (mrd>tol) {
        display("{err}         influence function(s) recentered at zero")
    }
}

void dstat_set_cstripe(`Data' D, `Grp' G, `Int' i, `Int' j, `Int' a, `Int' b, `SC' coefs)
{
    if (!(D.cat&!D.novalues) & D.nvars>1) {
        if (D.ovar=="") D.eqs[G.k] = D.yvars[j]
        else if (i>cols(D.overlevels)) D.eqs[G.k] = "total~" + D.yvars[j]
        else D.eqs[G.k] = strofreal(D.overlevels[i]) + "~" + D.yvars[j]
    }
    else if (D.ovar!="") {
        if (i>cols(D.overlevels)) D.eqs[G.k] = "total"
        else D.eqs[G.k] = strofreal(D.overlevels[i])
    }
    else D.eqs[G.k] = "_"
    D.cstripe[|a,1 \ b,1|] = J(b-a+1, 1, D.eqs[G.k])
    if (D.cat&!D.novalues) D.cstripe[|a,2 \ b,2|] = coefs :+ ("."+D.yvars[j])
    else                   D.cstripe[|a,2 \ b,2|] = coefs
    D.id[|a \ b|] = J(b-a+1, 1, G.id)
}

void dstat_init_grp(`Data' D, `Grp' G, `Int' i)
{
    // group data
    _dstat_init_grp(D, G, i)
    D._W[i] = G.W
    D._N[i] = G.N
    // balancing
    if (D.bal.method=="") return
    G.Z = J(0,0,.) // reset
    if (D.bal.ref==G.id) return // do not balance reference group
    if (D.bal.ref<.) G.p1 = selectindex(D.over:==D.bal.ref)
    else             G.p1 = selectindex(D.over:!=G.id)
    if (D.bal.noisily) printf("{txt}\n==> balance: {it:groupvar} = %g\n", G.id)
    if      (D.bal.method=="ipw") dstat_balance_ipw(D, D.bal, G)
    else if (D.bal.method=="eb")  dstat_balance_eb(D, D.bal, G)
    else exit(499) // cannot be reached
    dstat_balance_rescale(D, G) // update weights
    if (length(D.bal.wvar)) D.bal.wvar[G.p] = G.w
}

void _dstat_init_grp(`Data' D, `Grp' G, `Int' i)
{
    if (i>cols(D.overlevels)) { // total
        G.id = G.p = .
        G.N  = D.N
        G.w  = D.w
        G.W  = D.W
    }
    else {
        G.id = D.overlevels[i]
        G.p  = selectindex(D.over:==G.id)
        G.N  = rows(G.p)
        if (D.w==1) {; G.w = 1;        G.W = G.N;          }
        else        {; G.w = D.w[G.p]; G.W = quadsum(G.w); }
    }
}

void dstat_init_Y(`Data' D, `Grp' G, `Int' j)
{
    G.j = j
    G.k = G.k + 1
    G.Y = D.Y[G.p, j]                // or use a view?
    G.Ys = G.Zs = G.ws = G.Yc = G.Wc = J(0,1,.)
    G.z = .
    G.Nc = 0
    G.m  = .
    D.S.data(J(0,1,.)) // clear density object
}

void dstat_init_Ys(`Data' D, `Grp' G, | `Int' z)
{
    // sort Y and store in G
    if (rows(G.Ys)) {
        if (G.z==z) return // dstat_init_Ys() already applied
    }
    G.ws = G.w
    if (z<.) {
        G.z = z
        G.Zs = D.Z[G.p, G.z]
        if (rows(G.ws)!=1) {
            G.ps = mm_order((G.Zs, G.Y), (1,2), 1) // stable sort
            G.ws = G.ws[G.ps]
        }
        else G.ps = order((G.Zs, G.Y), (1,2))
        G.Zs = G.Zs[G.ps]
        G.Ys = G.Y[G.ps]
    }
    else {
        G.z = .; G.Zs = J(0,1,.)
        if (rows(G.ws)!=1) {
            G.ps = mm_order(G.Y, 1, 1) // stable sort
            G.ws = G.ws[G.ps]
        }
        else G.ps = order(G.Y, 1)
        G.Ys = G.Y[G.ps]
    }
    // reset Yc
    G.Yc = G.Wc = J(0,1,.)
    G.Nc = 0
}

void dstat_init_Ycum(`Data' D, `Grp' G, | `Int' z)
{   // computes running sum of (weighted) outcome at unique values; 
    // also includes an origin
    // this is used to compute lorenz ordinates
    `RM' cdf
    
    if (G.Nc) {
        if (G.z==z) return // dstat_init_Ycum() already applied
    }
    dstat_init_Ys(D, G, z)
    if (G.z<.) {
        G.Wc = 0 \ _mm_ecdf2(G.Zs, G.ws, 0, 1)[,2]
        G.Yc = 0 \ _mm_ecdf2(G.Zs, G.ws:*G.Ys, 0, 1)[,2]
        if (D.noIF==0) dstat_init_EY(D, G)
    }
    else {
        cdf = _mm_ecdf2(G.Ys, G.ws, 0, 1)
        G.Wc = 0 \ cdf[,2]
        G.Yc = 0 \ quadrunningsum(cdf[,1] :* mm_diff(G.Wc))
    }
    G.Nc = rows(G.Yc)
}

void dstat_init_EY(`Data' D, `Grp' G)
{   // estimates E(Y|Z=q) using local linear regression on a 100-point grid
    // (using default lpoly settings for kernel and bandwidth); this will 
    // be used for the IFs of concentration curve ordinates
    `RS'   bw
    `IntC' p
    pragma unset bw
    
    if (D.wtype==1) {
        // first compute bandwidth using rounded fweights
        if (G.ws!=ceil(G.ws)) {
            (void) _dstat_lpoly(G.Ys, G.Zs, round(G.ws), 0, 1, bw)
        }
    }
    // lpoly fit
    G.EYat = rangen(min(G.Zs), max(G.Zs), 100)
    G.EY   = _dstat_lpoly(G.Ys, G.Zs, G.ws, G.EYat, D.wtype==1, bw)
    if (hasmissing(G.EY)) { // restrict grid to nonmissing points
        p = selectindex(G.EY:<.)
        assert(length(p)) // lpoly failed
        G.EYat = G.EYat[p]
        G.EY   = G.EY[p]
    }
}

`RC' _dstat_lpoly(`RC' Y, `RC' Z, `RC' W, `RC' AT, `Bool' fw, `RS' bw)
{   // obtain local linear fit using default settings; bw will be replaced
    `Int'  n, r, N
    `Bool' preserve
    `SS'   y, z, w, at, ey, cmd
    `RC'   EY

    // write data
    n = rows(Y)
    r = rows(AT)
    N = max((n, r))
    preserve = (st_nobs()<N)
    if (preserve) {
        stata("preserve")
        stata("quietly set obs "+strofreal(N,"%18.0g"))
    }
    (void) st_addvar("double", y  = st_tempname()); st_store((1,n), y, Y)
    (void) st_addvar("double", z  = st_tempname()); st_store((1,n), z, Z)
    (void) st_addvar("double", at = st_tempname()); st_store((1,r), at, AT)
    // set weights (note: fweights are relevant only for bandwidth estimation)
    if (rows(W)==1) {
        if (fw & bw>=.) w = "[fw = "+strofreal(W)+"]"
        else            w = ""
    }
    else {
        (void) st_addvar("double", w  = st_tempname()); st_store((1,n), w, W)
        if (fw & bw>=.) w = sprintf("[fw = %s]", w)
        else            w = sprintf("[aw = %s]", w)
    }
    // run lpoly
    ey = st_tempname()
    cmd = sprintf("lpoly %s %s %s in 1/"+strofreal(n,"%18.0g"), y, z, w) +
          sprintf(", nograph degree(1) at(%s) generate(%s)", at, ey) +
          (bw<. ? (" bwidth("+strofreal(bw)+")") : "")
    (void) _stata(cmd)
    if (_st_varindex(ey)<.) {
        EY = st_data((1,r), ey)
        bw = st_numscalar("r(bwidth)")
    }
    else {
        EY = J(r, 1, .)
        bw = .
    }
    if (preserve) stata("restore")
    return(EY)
}

`RS' _dstat_mean(`Grp' G)
{
    if (G.m>=.) G.m = mean(G.Y, G.w)
    return(G.m)
}

void dstat_set_density(`Data' D, `Grp' G)
{
    if (rows(D.S.X())) return // dstat_set_density() already applied
    if (rows(G.Ys)) D.S.data(G.Ys, G.ws, D.wtype>=2, 1) // use sorted data
    else            D.S.data(G.Y , G.w , D.wtype>=2, 0) // use unsorted data
    if (D.bwidth[G.k]<.) D.S.bw(D.bwidth[G.k])
    if (D.S.h()>=.)      D.S.bw(epsilon(1))  // missing bandwidth
    D.bwidth[G.k] = D.S.h()
}

// --------------------------------------------------------------------------
// balancing
// --------------------------------------------------------------------------

void dstat_balance_rescale(`Data' D, `Grp' G)
{
    `RS' c

    // at this point sum(G.w) is equal to the size of the reference group
    if (D.noIF==0) G.wc = G.w
    if (D.wtype) {
        // add base weight if reference is pooled sample
        if (D.bal.ref>=.) G.w = G.w :+ G.wb
        // rescaling factor
        c = quadsum(G.wb) / quadsum(G.w)
    }
    else {
        // add one if reference is pooled sample
        if (D.bal.ref>=.) G.w = G.w :+ 1
        // rescaling factor
        c = rows(G.w) / quadsum(G.w)
    }
    G.w  = G.w * c                  // rescaled total weight
    G.wb = G.w :/ G.wb              // total weight / base weight
    if (D.noIF==0) G.wc = G.wc * c  // total weight - "pooled" component
}

void dstat_balance_ipw(`Data' D, `Bal' bal, `Grp' G)
{
    `SS' treat, ps
    `RC' p
    
    // run logit and obtain ps
    treat = st_tempname(); ps = st_tempname()
    dstat_balance_ipw_Y(D, G, treat)
    stata("quietly "
        + (bal.noisily ? "noisily " : "")
        + "logit " + treat + " " + bal.zvars
        + (D.wtype ? " ["+("fw","iw","pw")[D.wtype]+"="+D.wvar+"]" : "")
        + (bal.opts!="" ? ", "+bal.opts : ""))
    stata("quietly predict double " + ps + " if e(sample), pr")
    
    // compute balancing weights and rescale to size of reference group
    p = st_data(., ps, D.touse)
    G.wb = G.w // backup base weights
    G.w = p[G.p]
    G.w = G.w :/ (1 :- G.w) :* G.wb
    if (D.wtype) G.w = G.w * (quadsum(D.w[G.p1]) / quadsum(G.w))
    else         G.w = G.w * (rows(G.p1)         / quadsum(G.w))
    _editmissing(G.w, 0) // can occur in case of perfect predictors
    if (hasmissing(p[G.p1])) {
        printf("{err}warning: some reference observations discarded in"
            + " balancing of {it:groupvar} = %g\n", G.id)
        printf("{err}         balancing may be poor\n")
        _editmissing(G.w, 0)
    }
    
    // fillin IFs
    if (D.noIF==0) dstat_balance_ipw_IF(D, G, treat, p) // modifies p
    
    // cleanup
    st_dropvar(treat); st_dropvar(ps)
}

void dstat_balance_ipw_Y(`Data' D, `Grp' G, `SS' treat)
{
    `RC' Y
    
    Y       = J(D.N,  1, .)
    Y[G.p1] = J(rows(G.p1), 1, 1)
    Y[G.p]  = J(G.N,  1, 0)
    st_store(., st_addvar("byte", treat), D.touse, Y)
}

void dstat_balance_ipw_IF(`Data' D, `Grp' G, `SS' treat, `RC' p)
{
    `Bool' cons
    `IntC' idx
    `RC'   Y, h, w
    `RM'   Z, IFZ
    `SR'   zvars
    pragma unset Z
    
    // get data
    idx = G.p1 \ G.p
    Y = st_data(., treat, D.touse)[idx]
    p = p[idx]
    _dstat_replacemissing(p, Y) // perfect predictions
    zvars = st_matrixcolstripe("e(b)")[,2]'
    cons = anyof(zvars, "_cons")
    if (cons) zvars = zvars[|1\length(zvars)-1|]
    st_view(Z, ., zvars, D.touse)
    st_subview(Z, Z, idx, _dstat_non_omitted(length(zvars)))
    
    // compute IFs
    h = (Y :- p)
    if (cons) h = Z :* h, h
    else      h = Z :* h
    w = p :* (1 :- p)
    if (D.wtype) w = w :* D.w[idx]
    IFZ = h * invsym(quadcross(Z, cons, w, Z, cons))'
    
    // store results
    G.IFZ1 = IFZ[|1,1 \ rows(G.p1),.|]
    G.IFZ  = IFZ[|rows(G.p1)+1,1 \ .,.|]
    G.Z    = (cols(Z) ? Z[|rows(G.p1)+1,1 \ .,.|] : J(G.N, 0, .)) // no longer a view
    if (cons) G.Z = G.Z, J(G.N, 1, 1) 
}

void _dstat_replacemissing(`RC' X, `RC' Y)
{
    `IntC' p

    if (hasmissing(X)) {
        p = selectindex(X:>=.)
        X[p] = Y[p]
    }
}

`RR' _dstat_non_omitted(`Int' c)
{
    real scalar k

    stata("_ms_omit_info e(b)")
    k = st_numscalar("r(k_omit)")
    if (k==0) return(1..c)
    if (k==c) return(J(1, 0, .))
    return(selectindex(!st_matrix("r(omit)")[|1\c|]))
}

void dstat_balance_eb(`Data' D, `Bal' bal, `Grp' G)
{
    `RM' Z1
    `RC' w1
    `T'  S
    pragma unset Z1
    
    // data
    st_view(G.Z, ., D.bal.zvars, D.touse)
    st_subview(Z1,  G.Z, G.p1, .)
    st_subview(G.Z, G.Z, G.p,  .)
    if (D.wtype) w1 = D.w[G.p1]
    else         w1 = D.w
    
    // settings
    S = mm_ebal_init(Z1, w1, G.Z, G.w)
    mm_ebal_btol(S, bal.ebopts[1])
    mm_ebal_difficult(S, bal.ebopts[2])
    if (bal.ebopts[3]<.) mm_ebal_maxiter(S, bal.ebopts[3])
    if (bal.ebopts[4]<.) mm_ebal_ptol(S, bal.ebopts[4])
    if (bal.ebopts[5]<.) mm_ebal_vtol(S, bal.ebopts[5])
    if (bal.noisily==0)  mm_ebal_trace(S, "none")

    // estimation
    if (mm_ebal(S)==0) {
        printf("{err}warning: balancing tolerance not achieved for {it:groupvar} = %g\n", G.id)
        printf("{err}         balancing may be poor\n")
    }
    G.wb = G.w // backup base weights
    G.w  = mm_ebal_W(S)
    if (hasmissing(G.w)) {
        display("{err}unexpected error; balancing weights contain missing values")
        exit(499)
    }

    // fillin IFs
    if (D.noIF==0) dstat_balance_eb_IF(G, Z1, w1)

}

void dstat_balance_eb_IF(`Grp' G, `RM' Z1, `RC' w1)
{
    `RS' W, odds
    `RR' M
    `RC' h
    `RM' Q
    
    // beta
    M      = mean(Z1, w1) // target moments
    h      = G.Z :- M
    Q      = invsym(quadcross(h, G.w, G.Z))
    G.IFZ  = -(G.w:/G.wb) :* h * Q'
    G.IFZ1 =  ((Z1 :- M) * Q')

    // alpha
    odds   = (rows(w1)==1 ? w1*rows(Z1) : quadsum(w1)) / G.W
    h      = G.w:/G.wb :- odds
    Q      = quadcolsum(G.Z :* G.w)
    W      = quadsum(G.w)
    G.IFZ  = G.IFZ, -(h :+ odds :+ G.IFZ * Q')/W
    G.IFZ1 = G.IFZ1, (1 :- G.IFZ1 * Q')/W
    G.Z    = G.Z, J(G.N, 1, 1) // no longer a view
}

// --------------------------------------------------------------------------
// dstat density
// --------------------------------------------------------------------------

void dstat_density(`Data' D)
{
    `Int'  i, j, a, b
    `Grp'  G
    
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            dstat_set_density(D, G)
            a = b + 1
            if (D.n<.) {
                b = a + D.n - 1
                D.b[|a \ b|] = D.S.d(D.n, ., ., D.exact)
            }
            else {
                b = a + rows(*D.AT[G.k]) - 1
                D.b[|a \ b|] = D.S.d(*D.AT[G.k], D.exact)
            }
            if (D.uncond) D.b[|a \ b|] = D.b[|a \ b|] * (G.W/D.W)
            D.at[|a \ b|] = D.S.at()
            // compute IFS
            if (D.noIF==0) dstat_density_IF(D, G, a)
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "d":+strofreal(1::b-a+1) : strofreal(D.S.at(), D.vfmt)))
        }
    }
}

void dstat_density_IF(`Data' D, `Grp' G, `Int' a)
{
    `Int' i
    `RS'  W, b, f
    `RC'  at, h, z
    
    W  = (D.uncond ? D.W : G.W)
    f  = (D.uncond==2 ? W/G.W : 1)
    at = D.S.at()
    h  = D.S.h() * D.S.l()
    for (i=rows(at); i; i--) {
        z = D.S.K(D.S.X(), at[i], h)
        if (D.exact)     b = D.b[a+i-1] * f
        else             b = quadcross(G.w, z) / W * f
        dstat_set_IF(D, G, a+i-1, (z :- b)/W, (D.uncond==1 ? -b/W : 0))
    }
}

// --------------------------------------------------------------------------
// dstat histogram
// --------------------------------------------------------------------------

void dstat_hist(`Data' D)
{
    `Int'  i, j, a, b
    `RC'   AT, B, h, c
    `RR'   minmax
    `Grp'  G
    
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            minmax = minmax(G.Y)
            if (D.n[G.k]<.) AT = dstat_hist_AT(D, G, D.n[G.k], minmax)
            else AT = *D.AT[G.k]
            h = mm_diff(AT) // bin width
            a = b + 1
            b = a + rows(AT) - 1
            B = mm_diff(mm_relrank(G.Y, G.w, AT, 0, 1))
            if (minmax[1]==AT[1]) {
                // make first bin left-inclusive
                if (D.wtype) B[1] = B[1] + quadsum(select(G.w, (G.Y:==AT[1])))
                else         B[1] = B[1] + sum(G.Y:==AT[1])
            }
            if (!D.freq) {
                if      (D.prop) c = 1 / G.W
                else if (D.pct)  c = 100 / G.W
                else {
                    c = 1 :/ (h * G.W)
                    _editmissing(c, 0) // zero width bins
                }
                B = B :* c
                if (D.uncond) B = B * (G.W/D.W)
            }
            D.b[|a \ b|] = B \ B[rows(B)]
            D.at[|a,1 \ b,.|] = AT, AT + (h/2 \ .), (h \ .)
            // compute IFS
            if (D.noIF==0) dstat_hist_IF(D, G, a, b, minmax)
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "h":+strofreal(1::b-a)\"_ul" : strofreal(AT, D.vfmt)))
        }
    }
}

`RM' dstat_hist_AT(`Data' D, `Grp' G, `Int' n, `RR' minmax)
{
    if (D.ep==0) return(rangen(minmax[1], minmax[2], n+1))
    else         return(mm_quantile(G.Y, G.w, (0::n)/n, 1)) // def=1
}

void dstat_hist_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `RR' minmax)
{
    `Int' i
    `RS'  W, c, f
    `RC'  z
    
    W = (D.uncond    ? D.W   : G.W)
    f = (D.uncond==2 ? W/G.W : 1)
    for (i=a; i<=b; i++) {  // assuming b-a >= 1
        if (i==b) {
            D.IF[,i] = J(D.N, 1, 0)
            break
        }
        if (i==a) {
            if (D.at[i,1]<=minmax[1]) z = G.Y:<=D.at[i+1,1]
            else  z = (G.Y:>D.at[i,1] :& G.Y:<=D.at[i+1,1])
        }
        else z = (G.Y:>D.at[i,1] :& G.Y:<=D.at[i+1,1])
        if      (D.prop) z = (z :- D.b[i]*f    ) / W
        else if (D.pct)  z = (z :- D.b[i]*f/100) * (100/W)
        else if (D.freq) z = (z :- D.b[i]/W)
        else {
            c = 1 / (D.at[i,3] * W)
            if (c>=.) c = 0 // zero width bin
            z = (z :- D.b[i]*f*D.at[i,3]) * c
        }
        dstat_set_IF(D, G, i, z, (D.uncond==1 ? -D.b[i]/W : 0))
    }
}

// --------------------------------------------------------------------------
// dstat cdf
// --------------------------------------------------------------------------

void dstat_cdf(`Data' D, `Bool' cc)
{
    `Int'  i, j, a, b
    `RC'   AT
    `Grp'  G
    
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            if (D.n<.) AT = dstat_cdf_AT(G.Y, D.n)
            else       AT = *D.AT[G.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.b[|a \ b|] = _dstat_cdf(G.Y, G.w, AT, D.mid+D.floor*2, D.freq, D.ipolate)
            if (cc) {
                if (D.freq) D.b[|a \ b|] = G.W :- D.b[|a \ b|]
                else        D.b[|a \ b|] = 1   :- D.b[|a \ b|]
            }
            if (D.pct)  D.b[|a \ b|] = D.b[|a \ b|] * 100
            if (D.uncond & !D.freq) D.b[|a \ b|] = D.b[|a \ b|] * (G.W/D.W)
            D.at[|a \ b|] = AT
            // compute IFS
            if (D.noIF==0) dstat_cdf_IF(D, G, a, b, cc)
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "c":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
        }
    }
}

`RC' dstat_cdf_AT(`RC' Y, `Int' n)
{
    `RR' minmax
    
    minmax = minmax(Y)
    return(rangen(minmax[1], minmax[2], n))
}

`RC' _dstat_cdf(`RC' Y, `RC' w, `RC' at, `Int' t, `Bool' nonorm, `Bool' ipolate)
{   // t = 2 floor, t = 1 mid, t = 0 regular
    `Int'  r, W
    `RC'   c
    `IntC' p
    `RM'   cdf
    
    if (rows(Y)==0 | rows(at)==0) return(J(rows(at),1,.))
    cdf = mm_ecdf2(Y, w, 0, 1) // cumulative frequency at unique values of Y
    r = rows(cdf)
    if (ipolate) {
        W = cdf[r,2]
        if (t==2)      cdf[,2] = (0\cdf[,2])[|1 \ r|]           // floor
        else if (t==1) cdf[,2] = cdf[,2] - mm_diff(0\cdf[,2])/2 // mid
        if (nonorm==0) {
            cdf[,2] = cdf[,2] / W
            W = 1
        }
        if (mm_issorted(at)) c = mm_fastipolate(cdf[,1], cdf[,2], at)
        else {
            p = order(at, 1)
            c = mm_fastipolate(cdf[,1], cdf[,2], at[p], 1)[invorder(p)]
        }
        if (hasmissing(c)) {
            // handle evaluation points outside data range
            p = selectindex(c:>=. :& at:>cdf[r,1])
            if (rows(p)) c[p] = J(rows(p),1,W)
            p = selectindex(c:>=. :& at:<cdf[1,1])
            if (rows(p)) c[p] = J(rows(p),1,0)
        }
        return(c)
    }
    if (nonorm==0) cdf[,2] = cdf[,2] / cdf[r,2]
    if (mm_issorted(at)) return(__dstat_cdf(cdf[,1], cdf[,2], at, t))
    p = order(at, 1)
    return(__dstat_cdf(cdf[,1], cdf[,2], at[p], t)[invorder(p)])
}

`RC' __dstat_cdf(`RC' y, `RC' cdf, `RC' x, `Int' t)
{
    `Int' i, j
    `RS'  xi
    `RC'  r, h
    
    i = rows(x)
    r = J(i, 1, 0)
    j = rows(y)
    if (t==2) {         // floor
        for (; i; i--) {
            xi = x[i]
            for (; j; j--) {
                if (y[j]<xi) break
            }
            if (j) r[i] = cdf[j]
            else break // x[i] <= min(y)
        }
    }
    else if (t==1) {    // mid
        h = mm_diff(0\cdf)/2
        for (; i; i--) {
            xi = x[i]
            for (; j; j--) {
                if (y[j]<=xi) break
            }
            if (j) {
                if (y[j]==xi) r[i] = cdf[j] - h[j]
                else          r[i] = cdf[j]
            }
            else break // x[i] is smaller than min(y)
        }
        return(r)
    }
    else {          // regular
        for (; i; i--) {
            xi = x[i]
            for (; j; j--) {
                if (y[j]<=xi) break
            }
            if (j) r[i] = cdf[j]
            else break // x[i] < min(y)
        }
    }
    return(r)
}

void dstat_cdf_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Bool' cc)
{
    `Int' i, j, k, K
    `RS'  W, f
    `RR'  C
    `RC'  zP
    `RM'  z, AT
    
    W = (D.uncond    ? D.W   : G.W)
    f = (D.uncond==2 ? W/G.W : 1)
    if (D.ipolate) {
        // compute IF as weighted average of the IFs of the next lower and
        // upper observed Y-values
        AT = _dstat_cdf_IF_AT(G.Y, D.at[|a\b|])
        j = 0
        for (i=a; i<=b; i++) {
            j++
            K = 1 + (AT[j,1]<AT[j,2])
            z = J(G.N, K, .)
            C = J(1, K, .)
            for (k=K; k; k--) {
                if (D.mid) {
                    zP = (G.Y :== AT[j,k]) / 2
                    if (cc) z[,k] = (G.Y :> AT[j,k])
                    else    z[,k] = (G.Y :< AT[j,k])
                    C[k]  = sum(quadcross(G.w, (z[,k], zP))) / W * f
                    z[,k] = (z[,k] + zP) :- C[k]
                }
                else {
                    if (D.floor) {
                        if (cc) z[,k] = (G.Y :>= AT[j,k])
                        else    z[,k] = (G.Y :<  AT[j,k])
                    }
                    else {
                        if (cc) z[,k] = (G.Y :> AT[j,k])
                        else    z[,k] = (G.Y :<= AT[j,k])
                    }
                    C[k]  = quadcross(G.w, z[,k]) / W * f
                    z[,k] = (z[,k] :- C[k])
                }
                if (D.pct)         z[,k] = z[,k] * (100/W)
                else if (!D.freq)  z[,k] = z[,k] / W
            }
            if (K==2) {
                k = (AT[j,2] - D.at[i]) / (AT[j,2]-AT[j,1])
                z = k*z[,1] + (1-k)*z[,2]
                C = k*C[1]  + (1-k)*C[2]
            }
            if (D.pct) C = C * 100
            dstat_set_IF(D, G, i, z, (D.uncond==1 ? -C/W : 0))
        }
        return
    }
    for (i=a; i<=b; i++) {
        if (D.pct)       C = D.b[i]*f / 100
        else if (D.freq) C = D.b[i] / W
        else             C = D.b[i]*f
        if (D.mid) {
            zP = (G.Y :== D.at[i]) / 2
            if (cc) z = (G.Y :> D.at[i])
            else    z = (G.Y :< D.at[i])
            z  = (z + zP) :- C
        }
        else {
            if (D.floor) {
                if (cc) z = (G.Y :>= D.at[i])
                else    z = (G.Y :<  D.at[i])
            }
            else {
                if (cc) z = (G.Y :>  D.at[i])
                else    z = (G.Y :<= D.at[i])
            }
            z = (z :- C)
        }
        if (D.pct)           z = z * (100/W)
        else if (!D.freq)    z = z / W
        dstat_set_IF(D, G, i, z, (D.uncond==1 ? -D.b[i]/W : 0))
    }
}

`RM' _dstat_cdf_IF_AT(`RC' Y, `RC' x) // x assumed fleeting
{
    `Int' i, j, n
    `RS'  xi
    `RC'  y, p
    `RM'  AT
    
    p = order(x, 1)
    x = x[p]
    y = mm_unique(Y)
    n = j = rows(y)
    i = rows(x)
    AT = (x, x)
    for (; i; i--) {
        xi = x[i]
        for (; j; j--) {
            if (y[j]<=xi) break
        }
        if (j) {
            if (y[j]==xi) continue
            AT[i,1] = y[j]
            if (j<n) AT[i,2] = y[j+1]
        }
        else AT[i,2] = y[1]
    }
    return(AT[invorder(p),])
}

// --------------------------------------------------------------------------
// dstat proportion
// --------------------------------------------------------------------------

void dstat_prop(`Data' D)
{
    `Int'  i, j, a, b
    `RC'   AT
    `Grp'  G
    
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            AT = *D.AT[G.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.b[|a \ b|]  = _dstat_prop(D, G, AT)
            D.at[|a \ b|] = AT
            // compute IFS
            if (D.noIF==0) dstat_prop_IF(D, G, a, b)
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "p":+strofreal(1::b-a+1) : (D.cat ? strofreal(AT) :
                strofreal(AT, D.vfmt))))
        }
    }
    if (D.cat & !D.novalues) D.eqs = D.eqs[mm_seq(1, G.k, D.nvars)]
}

`RC' _dstat_prop(`Data' D, `Grp' G, `RC' AT)
{
    `Int' i
    `RS'  W
    `RM'  H
    `RC'  F
    `T'   A
    
    // obtain frequency table
    H = dstat_prop_freq(D, G)
    // post H as hash table
    A = asarray_create("real")
    asarray_notfound(A, 0)
    for (i=rows(H); i; i--) asarray(A, H[i,1], H[i,2])
    // copy frequencies
    i = rows(AT)
    F = J(i, 1, .)
    for (; i; i--) F[i] = asarray(A, AT[i])
    // return
    if (D.uncond) W = D.W
    else           W = G.W
    if (D.pct)  return(F * (100/W))
    if (D.freq) return(F)
                return(F / W)
}

`RM' dstat_prop_freq(`Data' D, `Grp' G)
{
    if (G.N==0) return(J(G.N, 2, .))
    dstat_init_Ys(D, G)
    return(_dstat_prop_freq(G.Ys, G.ws, G.N))
}

`RM' _dstat_prop_freq(`RC' Y, `RC' w, `Int' n)
{
    `Int' i, j, i0
    `RS'  y0
    `RM'  H
    
    H = J(n, 2, .)
    j = 0
    i0 = 1
    y0 = Y[i0]
    if (rows(w)==1) {
        for (i=2;i<=n; i++) {
            if (Y[i]!=y0) {
                H[++j,] = (y0, (i-i0)*w)
                i0 = i
                y0 = Y[i0]
            }
        }
        H[++j,] = (y0, (i-i0)*w)
        return(H[|1,1 \ j,2|])
    }
    for (i=2;i<=n; i++) {
        if (Y[i]!=y0) {
            H[++j,] = (y0, quadsum(w[|i0\i-1|]))
            i0 = i
            y0 = Y[i0]
        }
    }
    H[++j,] = (y0, quadsum(w[|i0\i-1|]))
    return(H[|1,1 \ j,2|])
}

/*
`RC' dstat_freq(`RC' x0, `RC' w, `RC' at)
{   // assumes that x0 and at are integer
    // also assumes that values are within a "reasonable" range, such that
    // f will not consume excessive memory
    `Int'  i, xi, r, offset
    `IntR' minmax
    `IntC' x, c
    `RC'   f
    
    minmax  = minmax(x0\at)
    offset = minmax[1] - 1
    r = minmax[2] - minmax[1] + 1
    x = x0 :- offset  // faster than applying offset within loop
    if (rows(w)==1) {
        f = J(r,1,0)
        for (i=rows(x);i;i--) {
            xi = x[i]
            f[xi] = f[xi] + w
        }
        return(f[at:-offset])
    }
    if (any(w:!=trunc(w))) { // use mean updating if weights are noninteger
        f = c = J(r,1,0)
        for (i=rows(x);i;i--) {
            xi = x[i]
            c[xi] = c[xi] + 1
            f[xi] = f[xi] + (w[i]-f[xi])/c[xi] 
        }
        return(f[at:-offset] :* c[at:-offset])
    }
    f = J(r,1,0)
    for (i=rows(x);i;i--) {
        xi = x[i]
        if (xi>r) continue
        if (xi<1) continue
        f[xi] = f[xi] + w[i]
    }
    return(f[at:-offset])
}
*/

void dstat_prop_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i
    `RS'  W, f
    `RC'  h
    
    W = (D.uncond    ? D.W   : G.W)
    f = (D.uncond==2 ? W/G.W : 1)
    for (i=a; i<=b; i++) {
        if      (D.pct)   h = ((G.Y :== D.at[i]) :- D.b[i]*f/100) * (100/W)
        else if (D.freq)  h = ((G.Y :== D.at[i]) :- D.b[i]/W    )
        else              h = ((G.Y :== D.at[i]) :- D.b[i]*f    ) / W
        dstat_set_IF(D, G, i, h, (D.uncond==1 ? -D.b[i]/W : 0))
    }
}

// --------------------------------------------------------------------------
// dstat quantile
// --------------------------------------------------------------------------

void dstat_quantile(`Data' D)
{
    `Int'  i, j, a, b
    `Grp'  G
    
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            dstat_init_Ys(D, G)
            a = b + 1
            b = a + rows(*D.AT[G.k]) - 1
            D.b[|a \ b|]  = _mm_quantile(G.Ys, G.ws, *D.AT[G.k], D.qdef, D.wtype==1)
            D.at[|a \ b|] = *D.AT[G.k]
            // compute IFS
            if (D.noIF==0) {
                dstat_set_density(D, G)
                dstat_quantile_IF(D, G, a, b)
            }
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "q":+strofreal(1::b-a+1) : strofreal(*D.AT[G.k], D.vfmt)))
        }
    }
}

void dstat_quantile_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i, l
    `RC'  z, fx
    
    fx = D.S.d(D.b[|a \ b|], D.exact)
    l = 0
    for (i=a; i<=b; i++) {
        z = (G.Y :<= D.b[i])
        dstat_set_IF(D, G, i, (mean(z, G.w) :- z) / (G.W * fx[++l]))
    }
}

// --------------------------------------------------------------------------
// dstat lorenz
// --------------------------------------------------------------------------

void dstat_lorenz(`Data' D)
{
    `Int'  i, j, a, b, t, z
    `Grp'  G
    
    if (D.gap & D.pct) t = 6 // equality gap in percent
    else if (D.gap)    t = 5 // equality gap
    else if (D.abs)    t = 4 // absolute
    else if (D.gl)     t = 3 // generalized
    else if (D.sum)    t = 2 // total
    else if (D.pct)    t = 1 // ordinary in percent
    else               t = 0 // ordinary
    if (D.zvar!="") z = selectindex(D.zvars:==D.zvar)
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            a = b + 1
            b = a + rows(*D.AT[G.k]) - 1
            D.b[|a \ b|]  = _dstat_lorenz(D, G, *D.AT[G.k], t, z)
            D.at[|a \ b|] = *D.AT[G.k]
            // compute IFS
            if (D.noIF==0) dstat_lorenz_IF(D, G, a, b, t)
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "l":+strofreal(1::b-a+1) : strofreal(*D.AT[G.k], D.vfmt)))
        }
    }
}

`RC' _dstat_lorenz(`Data' D, `Grp' G, `RC' P, `Int' t, | `Int' z)
{   // determine lorenz ordinates using interpolation
    `RC' p, L

    dstat_init_Ycum(D, G, z)
    p = order(P, 1)
    L = P[p] * G.Wc[G.Nc]
        // using G.Wc[G.Nc] instead of G.W to avoid precision issues at P=1
    L[p] = mm_fastipolate(G.Wc, G.Yc, L)
        // (assuming that sum of weights>0 for each unique Y-value)
    if (t==6) return((P - L/G.Yc[G.Nc]) * 100)  // equality gap in %
    if (t==5) return(P - L/G.Yc[G.Nc])          // equality gap
    if (t==4) return((L - P:*G.Yc[G.Nc]) / G.W) // absolute
    if (t==3) return(L / G.W)                   // generalized
    if (t==2) return(L)                         // total
    if (t==1) return(L * (100/G.Yc[G.Nc]))      // ordinary in %
              return(L = L / G.Yc[G.Nc])        // ordinary
}

void dstat_lorenz_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Int' t)
{
    D.IF[|1,a \ .,b|] = _dstat_lorenz_IF(D, G, D.at[|a \ b|], t)
}

`RM' _dstat_lorenz_IF(`Data' D, `Grp' G, `RC' at, `Int' t)
{
    `Int' i, n
    `RC'  q, ey, Z
    `RM'  IF
    
    n = rows(at)
    if (G.z<.) {
        q = _mm_quantile(G.Zs, G.ws, at, 2)
        ey = _dstat_lorenz_IF_EY(G, q)
        Z = D.Z[G.p, G.z]
    }
    else ey = q = _mm_quantile(G.Ys, G.ws, at, 2)
    IF = J(D.N, n, 0)
    for (i=1; i<=n; i++) {
        if (at[i]==0)               continue
        if (at[i]==1 & t!=2 & t!=3) continue
        IF[,i] = __dstat_lorenz_IF(D, G, (G.z<. ? Z : G.Y):<=q[i], ey[i], t)
    }
    return(IF)
}

`RC' _dstat_lorenz_IF_EY(`Grp' G, `RC' at)
{
    `IntC' p
    `RC'   ey
    
    p  = order(at, 1)
    ey = J(rows(at), 1, .)
    ey[p] = mm_fastipolate(G.EYat, G.EY, at[p], 1)
    return(ey)
}

`RC' __dstat_lorenz_IF(`Data' D, `Grp' G, `RC' Zq, `RS' ey, `Int' t)
{
    `RC' z, p, L, T, W, h
    
    T   = G.Yc[G.Nc]
    W   = G.Wc[G.Nc]
    z   = G.Y :* Zq
    p   = mean(Zq, G.w)   // set p=mean(zq) to ensure mean(IF)=0
    L   = quadsum(z:*G.w) // set L=sum(z) to ensure mean(IF)=0
    h   = ey * (p :- Zq)  // part of IF due to quantile
    if      (t==6) h = (z :- L/T:*G.Y :+ h) * (100/-T) // equality gap in %
    else if (t==5) h = (z :- L/T:*G.Y :+ h) / -T       // equality gap
    else if (t==4) h = (z :- L/W :+ p*(T/W :- G.Y) :+ h) / G.W // absolute
    else if (t==3) h = (z :- L/W :+ h) / G.W           // generalized
    else if (t==2) h =  z :- L/W :+ h                  // total
    else if (t==1) h = (z :- L/T:*G.Y :+ h) * (100/T)  // ordinary in %
    else           h = (z :- L/T:*G.Y :+ h) / T        // ordinary
    return(_dstat_set_IF(D, G, h, 0))
}

// --------------------------------------------------------------------------
// dstat share
// --------------------------------------------------------------------------

void dstat_share(`Data' D)
{
    `Int'  i, j, a, b, d, t, z
    `RC'   AT, B, h
    `Grp'  G
    
    if      (D.prop) {; d = 0; t = 0; } // proportion
    else if (D.ave)  {; d = 1; t = 3; } // average
    else if (D.gl)   {; d = 0; t = 3; } // generalized
    else if (D.sum)  {; d = 0; t = 2; } // total
    else if (D.pct)  {; d = 0; t = 1; } // percent
    else             {; d = 1; t = 0; } // density
    if (D.zvar!="") z = selectindex(D.zvars:==D.zvar)
    G.k = b = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            AT = *D.AT[G.k]
            h = mm_diff(AT) // bin width
            a = b + 1
            b = a + rows(AT) - 1
            B = mm_diff(_dstat_lorenz(D, G, AT, t, z))
            if (d) B = B :* editmissing(1 :/ h, 0)
            D.b[|a \ b|]  = B \ B[rows(B)]
            D.at[|a,1 \ b,.|] = AT, AT + (h/2 \ .), (h \ .)
            // compute IFS
            if (D.noIF==0) dstat_share_IF(D, G, a, b, t, d)
            // labels
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "s":+strofreal(1::b-a)\"_ul" : strofreal(AT, D.vfmt)))
        }
    }
}

void dstat_share_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Int' t, `Int' d)
{
    D.IF[|1,a \ .,b-1|] = _dstat_share_IF(D, G, D.at[|a,1 \ b,1|], t, d)
    D.IF[,b] = J(D.N, 1, 0)
}

`RM' _dstat_share_IF(`Data' D, `Grp' G, `RC' at, `Int' t, `Int' d)
{
    `Int' i, n
    `RM'  IF
    
    n = rows(at)
    IF = _dstat_lorenz_IF(D, G, at, t)
    for (i=1; i<n; i++) {
        if (d) IF[,i] = (IF[,i+1] - IF[,i]) * editmissing(1/(at[i+1]-at[i]), 0)
        else   IF[,i] =  IF[,i+1] - IF[,i]
    }
    return(IF[|1,1 \ .,n-1|])
}

// --------------------------------------------------------------------------
// dstat summarize
// --------------------------------------------------------------------------

void dstat_sum(`Data' D)
{
    `Int' i, j, l, ii, nn
    `SC'  AT
    `SS'  s, o
    `SC'  list
    `Grp' G
    `T'   A
    `PR'  P
    
    A = dstat_stats_lib()
    list = sort(asarray_keys(A), 1)
    G.k = l = 0
    for (i=1; i<=D.nover; i++) {
        dstat_init_grp(D, G, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            dstat_init_Ys(D, G)
            AT = *D.AT[G.k]
            nn = rows(AT)
            for (ii=1; ii<=nn; ii++) {
                l++
                s = AT[ii]
                o = _dstat_sum_split(s) // replaces s
                P = asarray(A, mm_strexpand(s, list))
                (*P[1])(D, G, l, o, *P[3])
            }
            dstat_set_cstripe(D, G, i, j, l-nn+1, l, AT)
        }
    }
}

`SS' _dstat_sum_split(`SS' s)
{
    `Int' p
    `SS'  o
    `SS'  rexp
    
    rexp = "[0-9]+$"
    if ((p=strpos(s,"("))) {
        o = substr(s, p+1, .)
        o = subinstr(substr(o, 1, strlen(o)-1), ",", " ") // remove pars and ,
        s = substr(s, 1, p-1)
    }
    else if (regexm(s, rexp)) {
        o = regexs(0)
        s = substr(s, 1, strlen(s)-strlen(o))
    }
    s = strlower(s)
    return(o)
}

`Bool' _dstat_sum_omit(`Data' D, `Int' i)
{
    if (D.b[i]>=.) {
        D.omit[i] = 1
        D.b[i] = 0
        if (D.noIF) return(1)
        D.IF[,i] = J(D.N, 1, 0)
        return(1)
    }
    return(0)
}

void dstat_sum_q(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    dstat_sum_quantile(D, G, i, o, O)
}

void dstat_sum_p(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    dstat_sum_quantile(D, G, i, o, O)
}

void dstat_sum_quantile(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_q(D, G, i, strtoreal(o)/100)
}

void _dstat_sum_q(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    `RS' fx
    `RC' z
    
    dstat_init_Ys(D, G)
    D.b[i] = _mm_quantile(G.Ys, G.ws, p, D.qdef, D.wtype==1)
    if (D.noIF) return
    fx = _dstat_sum_d(D, G, D.b[i])
    z = (G.Y :<= D.b[i])
    dstat_set_IF(D, G, i, (mean(z, G.w) :- z) / (G.W * fx))
}

void dstat_sum_d(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    dstat_sum_density(D, G, i, o, O)
}

void dstat_sum_density(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at
    `RC' z
    pragma unused O
    
    at = strtoreal(o)
    dstat_init_Ys(D, G)
    D.b[i] = _dstat_sum_d(D, G, at)
    if (D.noIF) return
    z = J(G.N, 1, .)
    z[G.ps] = D.S.K(D.S.X(), at, D.S.h()*D.S.l())
    if (D.exact) dstat_set_IF(D, G, i, (z :- D.b[i])/G.W)
    else         dstat_set_IF(D, G, i, (z :- mean(z, G.w))/G.W)
}

`RC' _dstat_sum_d(`Data' D, `Grp' G, `RC' at)
{
    dstat_set_density(D, G) // prepare density object if needed
    return(D.S.d(at, D.exact))
}

void dstat_sum_hist(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' c
    `RR' at
    `RC' z
    pragma unused O
    
    at = strtoreal(tokens(o))
    if (at[1]>at[2]) D.b[i] = .
    else {
        c = 1 / (at[2] - at[1])
        if (c>=.) c = 0 // zero width bin
        z = G.Y:>at[1] :& G.Y:<=at[2]
        D.b[i] = mean(z, G.w) * c
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z*c :- D.b[i])/G.W)
}

void dstat_sum_cdf(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at
    `RC' z
    pragma unused O
    
    at = strtoreal(o)
    z = G.Y:<=at
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_cdfm(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at, C, P
    `RC' z, zp
    pragma unused O
    
    at = strtoreal(o)
    z  = G.Y:<at
    zp = G.Y:==at
    C  = mean(z, G.w)
    P  = mean(zp, G.w)
    D.b[i] = C + P/2
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((z :- C) + (zp :- P)/2) / G.W)
}

void dstat_sum_cdff(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at
    `RC' z
    pragma unused O
    
    at = strtoreal(o)
    z = G.Y:<at
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_ccdf(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at
    `RC' z
    pragma unused O
    
    at = strtoreal(o)
    z = G.Y:>at
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_ccdfm(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at, C, P
    `RC' z, zp
    pragma unused O
    
    at = strtoreal(o)
    z  = G.Y:>at
    zp = G.Y:==at
    C  = mean(z, G.w)
    P  = mean(zp, G.w)
    D.b[i] = C + P/2
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((z :- C) + (zp :- P)/2) / G.W)
}

void dstat_sum_ccdff(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' at
    `RC' z
    pragma unused O
    
    at = strtoreal(o)
    z = G.Y:>=at
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_prop(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_freq(D, G, i, o, O)
    if (D.omit[i]) return
    D.b[i] = D.b[i] / G.W
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / G.W
}

void dstat_sum_pct(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_freq(D, G, i, o, O)
    if (D.omit[i]) return
    D.b[i] = D.b[i] * (100 / G.W)
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * (100/G.W)
}

void dstat_sum_f(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    dstat_sum_freq(D, G, i, o, O)
}

void dstat_sum_freq(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    D.istot[i] = 1
    _dstat_sum_freq(D, G, i, o, O)
}

void _dstat_sum_freq(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' at
    `RC' z
    pragma unused O
    
    at = strtoreal(tokens(o))
    if (length(at)==1) {
        z = G.Y:==at
        D.b[i] = quadsum(G.w:*z)
    }
    else {
        if (at[1]>at[2]) D.b[i] = .
        else {
            z = G.Y:>=at[1] :& G.Y:<=at[2]
            D.b[i] = quadsum(G.w:*z)
        }
        if (_dstat_sum_omit(D, i)) return
    }
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- (D.b[i]/G.W)))
}

void dstat_sum_total(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' at
    `RC' z
    pragma unused O
    
    D.istot[i] = 1
    at = strtoreal(tokens(o))
    if      (length(at)==0) z = G.Y
    else if (length(at)==1) z = G.Y * (G.Y:==at)
    else {
        if (at[1]>at[2]) z = J(G.N, 1, .) // so that D.b[i]=.
        else             z = G.Y * (G.Y:>=at[1] :& G.Y:<=at[2])
    }
    D.b[i] = quadsum(G.w:*z, 1)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- (D.b[i]/G.W)))
}

void dstat_sum_min(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    D.b[i] = min(G.Y)
    if (D.noIF) return
    D.IF[,i] = J(D.N, 1, 0)
}

void dstat_sum_max(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    D.b[i] = max(G.Y)
    if (D.noIF) return
    D.IF[,i] = J(D.N, 1, 0)
}

void dstat_sum_range(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    D.b[i] = mm_diff(minmax(G.Y))
    if (D.noIF) return
    D.IF[,i] = J(D.N, 1, 0)
}

void dstat_sum_midrange(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    D.b[i] = sum(minmax(G.Y))/2
    if (D.noIF) return
    D.IF[,i] = J(D.N, 1, 0)
}

void dstat_sum_mean(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    D.b[i] = _dstat_mean(G)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (G.Y :- D.b[i]) / G.W)
}

void dstat_sum_gmean(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' z
    pragma unused o
    pragma unused O
    
    z = ln(G.Y)
    _dstat_sum_isvalid(z, D.yvars[G.j], "gmean")
    D.b[i] = exp(mean(z, G.w))
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- ln(D.b[i])) * (D.b[i] / G.W))
}

void dstat_sum_hmean(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' z
    pragma unused o
    pragma unused O
    
    z = 1 :/ G.Y
    _dstat_sum_isvalid(z, D.yvars[G.j], "hmean")
    D.b[i] = 1 / mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- 1/D.b[i]) * (-D.b[i]^2 / G.W))
}

void dstat_sum_trim(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR'   args
    `RS'   p1, p2, plo, pup
    `RC'   z, q
    `IntC' lo, up, mid
    
    args = strtoreal(tokens(o))
    if (!length(args)) args = O[3]
    p1 = args[1]/100
    if (length(args)==1) p2 = 1 - p1
    else                 p2 = 1 - args[2]/100
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p1\p2, D.qdef, D.wtype==1)
    lo = (p1>0 ? G.Y:<=q[1] : J(G.N, 1, 0)) // tag obs <= lower quantile
    up = (p2<1 ? G.Y:>=q[2] : J(G.N, 1, 0)) // tag obs >= upper quantile
    mid = !lo :& !up  // tag obs in (lower quantile, upper quantile)
    D.b[i] = mean(G.Y, G.w:*mid)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    plo = (p1>0 ? quadsum(G.w:*lo)/G.W : 0) // exact proportion excluded from below
    pup = (p2<1 ? quadsum(G.w:*up)/G.W : 0) // exact proportion excluded from above
    z = G.Y:*mid
    if (p1>0) z = z + q[1]*(lo :- plo)
    if (p2<1) z = z + q[2]*(up :- pup)
    z = z / (1-plo-pup)
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
    // main part:      (Y:*mid/(1-plo-pup) :- D.b[i]) / G.W
    // lower quantile: (q[1]*(lo :- plo)) / ((1-plo-pup)*G.W)
    // upper quantile: (q[2]*(up :- pup)) / ((1-plo-pup)*G.W)
}

void dstat_sum_winsor(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR'   args
    `RS'   p1, p2, plo, pup
    `RC'   z, q, fx
    `IntC' lo, up, mid
    
    args = strtoreal(tokens(o))
    if (!length(args)) args = O[3]
    p1 = args[1]/100
    if (length(args)==1) p2 = 1 - p1
    else                 p2 = 1 - args[2]/100
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p1\p2, D.qdef, D.wtype==1)
    lo = (p1>0 ? G.Y:<=q[1] : J(G.N, 1, 0)) // tag obs <= lower quantile
    up = (p2<1 ? G.Y:>=q[2] : J(G.N, 1, 0)) // tag obs >= upper quantile
    mid = !lo :& !up  // tag obs in (lower quantile, upper quantile)
    plo = (p1>0 ? quadsum(G.w:*lo)/G.W : 0) // exact proportion excluded from below
    pup = (p2<1 ? quadsum(G.w:*up)/G.W : 0) // exact proportion excluded from above
    z = G.Y:*mid
    if (p1>0) z = z + q[1]:*lo
    if (p2<1) z = z + q[2]:*up
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    if (p1>0) z = z :- q[1]*plo
    if (p2<1) z = z :- q[2]*pup
    z = z / (1-plo-pup)
    fx = _dstat_sum_d(D, G, q)
    // note: winsorized mean = weighted sum of trimmed mean and quantiles
    z = (z :- mean(z, G.w)) * ((1-plo-pup)/G.W)                // trimmed mean
    if (p1>0) z = z + (plo :- lo) * (plo / (G.W * fx[1]))      // lower quantile
    if (p2<1) z = z + ((1-pup) :- !up) * (pup / (G.W * fx[2])) // upper quantile
    dstat_set_IF(D, G, i, z)
}

void dstat_sum_median(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    _dstat_sum_q(D, G, i, .5)
}

void dstat_sum_huber(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' eff, s, k, med, mad
    `RC' d, z, phi, zmed, zmad
    `T'  S
    
    if (o!="") eff = strtoreal(o)
    else       eff = O[3]
    dstat_init_Ys(D, G)
    med = _mm_median(G.Ys, G.ws)
    S = mm_mloc(G.Y, G.w, eff, "huber", med)
    D.b[i] = mm_mloc_b(S)
    if (D.noIF) return
    s = mm_mloc_s(S)
    k = mm_mloc_k(S)
    z = (G.Y :- D.b[i]) / s
    phi = mm_huber_phi(z, k)
    mad = s * invnormal(0.75)
    zmad = (abs(G.Y :- med) :<= mad)
    zmed = (G.Y :<= med)
    d = _dstat_sum_d(D, G, (med-mad, med, med+mad)')
    dstat_set_IF(D, G, i, (s * mm_huber_psi(z, k) 
        - (mean(phi:*z, G.w) / ((d[1]+d[3]) * invnormal(0.75)))
            * ((mean(zmad, G.w) :- zmad)
               + ((d[1]-d[3])/d[2]) * (mean(zmed, G.w) :- zmed)))
        / (mean(phi, G.w) * G.W))
}

void dstat_sum_biweight(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' eff, s, k, med, mad
    `RC' d, z, phi, zmed, zmad
    `T'  S
    
    if (o!="") eff = strtoreal(o)
    else       eff = O[3]
    dstat_init_Ys(D, G)
    med = _mm_median(G.Ys, G.ws)
    S = mm_mloc(G.Y, G.w, eff, "biweight", med)
    D.b[i] = mm_mloc_b(S)
    if (D.noIF) return
    s = mm_mloc_s(S)
    k = mm_mloc_k(S)
    z = (G.Y :- D.b[i]) / s
    phi = mm_biweight_phi(z, k)
    mad = s * invnormal(0.75)
    zmad = (abs(G.Y :- med) :<= mad)
    zmed = (G.Y :<= med)
    d = _dstat_sum_d(D, G, (med-mad, med, med+mad)')
    dstat_set_IF(D, G, i, (s * mm_biweight_psi(z, k) 
        - (mean(phi:*z, G.w) / ((d[1]+d[3]) * invnormal(0.75)))
            * ((mean(zmad, G.w) :- zmad)
               + ((d[1]-d[3])/d[2]) * (mean(zmed, G.w) :- zmed)))
        / (mean(phi, G.w) * G.W))
}

void dstat_sum_hl(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' z, F, d
    pragma unused o
    pragma unused O
    
    dstat_init_Ys(D, G)
    D.b[i] = mm_hl(G.Ys, G.ws, D.wtype==1)
    if (D.noIF) return
    z = 2*D.b[i] :- G.Ys
    F = _mm_relrank(G.Ys, G.ws, z[G.N::1], 1)[G.N::1]
    d = _dstat_sum_d(D, G, z)
    F[G.ps] = F
    dstat_set_IF(D, G, i, (mean(F, G.w) :- F) / (mean(d, G.ws) * G.W))
        // note: using mean(F, G.w) instead of 0.5 so that total of IF is
        // exactly zero
}

void dstat_sum_sd(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    dstat_sum_variance(D, G, i, o, O)
    if (D.b[i]==0) return // too few observations
    D.b[i] = sqrt(D.b[i])
    if (D.noIF) return
    if (D.b[i]==0) return
    D.IF[,i] = D.IF[,i] / (2 * D.b[i])
}

void dstat_sum_variance(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_variance(D, G, i, G.Y, o, O)
}

void _dstat_sum_variance(`Data' D, `Grp' G, `Int' i, `RC' Y, `SS' o, `RR' O)
{
    `RS' m, c, df
    pragma unset m
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    D.b[i] = _dstat_variance(D, G, Y, df, m)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    dstat_set_IF(D, G, i, (c*(Y :- m):^2 :- D.b[i]) / G.W)
}

`RS' _dstat_variance(`Data' D, `Grp' G, `RC' Y, | `RS' df, `RS' m)
{    // replaces m
    `RC' v
    
    if (args()<4) df = 1
    v = mm_meanvariance0(Y, G.w)
    m = v[1]
    v = v[2]
    if (df!=0) {
        if (D.wtype==1) v = v * (G.W / (G.W - df))
        else            v = v * (G.N / (G.N - df))
    }
    return(v)
}

void dstat_sum_mse(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' args
    `RS' m, df, c
    
    if (o!="") {
        args = strtoreal(tokens(o))
        m = args[1]
        if (length(args)>1) df = args[2]
        else                df = O[4]
    }
    else {; m = O[3]; df = O[4]; }
    D.b[i] = mm_mse(G.Y, G.w, m)
    if (df!=0) {
        if (D.wtype==1)  D.b[i] = D.b[i] * (G.W / (G.W - df))
        else             D.b[i] = D.b[i] * (G.N / (G.N - df))
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    dstat_set_IF(D, G, i, (c*(G.Y :- m):^2 :- D.b[i]) / G.W)
}

void dstat_sum_smse(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    dstat_sum_mse(D, G, i, o, O)
    if (D.b[i]==0) return // too few observations
    D.b[i] = sqrt(D.b[i])
    if (D.noIF) return
    if (D.b[i]==0) return
    D.IF[,i] = D.IF[,i] / (2 * D.b[i])
}

void dstat_sum_iqr(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' p, q, fx, z1, z2
    
    if (o!="") p = strtoreal(tokens(o)')
    else       p = O[(3,4)]'
    p = p / 100
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p, D.qdef, D.wtype==1)
    D.b[i] = q[2] - q[1]
    if (D.noIF) return
    fx = _dstat_sum_d(D, G, q)
    z1 = (G.Y :<= q[1])
    z2 = (G.Y :<= q[2])
    dstat_set_IF(D, G, i,
        (mean(z2, G.w) :- z2) / (G.W * fx[2])   // upper quartile
      - (mean(z1, G.w) :- z1) / (G.W * fx[1])   // lower quartile
        )
}

void dstat_sum_iqrn(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' c
    pragma unused o
    
    dstat_sum_iqr(D, G, i, "25 75", O)
    c = 1 / (invnormal(0.75) - invnormal(0.25))
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void dstat_sum_mad(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' args
    `RS' l, t
    
    if (o!="") {
        args = strtoreal(tokens(o))
        l = args[1]
        if (length(args)>1) t = args[2]
        else                t = O[4]
    }
    else {; l = O[3]; t = O[4]; }
    if (l & t)  _dstat_sum_mad_mean_mean(D, G, i)
    else if (l) _dstat_sum_mad_mean_med(D, G, i)
    else if (t) _dstat_sum_mad_med_mean(D, G, i)
    else        _dstat_sum_mad_med_med(D, G, i)
}

void _dstat_sum_mad_mean_mean(`Data' D, `Grp' G, `Int' i)
{
    `RS' t
    `RC' z, z2
    
    t  = _dstat_mean(G)
    z  = G.Y :- t
    z2 = abs(z)
    D.b[i] = mean(z2, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((z2 :- D.b[i]) :- mean(sign(z), G.w) * z) / G.W)
}

void _dstat_sum_mad_med_mean(`Data' D, `Grp' G, `Int' i)
{
    `RS' t
    `RC' z, z2, q, d
    
    dstat_init_Ys(D, G)
    t = _dstat_mean(G)
    z = G.Y :- t
    z2 = abs(z)
    D.b[i] = mm_median(z2, G.w) // z not sorted!
    if (D.noIF) return
    q = (t - D.b[i]) \ (t + D.b[i])
    d = _dstat_sum_d(D, G, q)
    z2 = (z2 :<= D.b[i]) 
    dstat_set_IF(D, G, i, ((mean(z2, G.w) :- z2)/(d[1]+d[2])
        :+ (d[1]-d[2])/(d[1]+d[2]) * z) / G.W)
}

void _dstat_sum_mad_mean_med(`Data' D, `Grp' G, `Int' i)
{
    `RS' q, d
    `RC' z, z2, z3

    dstat_init_Ys(D, G)
    q = _mm_median(G.Ys, G.ws)
    z = G.Y :- q
    z2 = abs(z)
    D.b[i] = mean(z2, G.w)
    if (D.noIF) return
    d = _dstat_sum_d(D, G, q)
    z3 = (G.Y :<= q)
    dstat_set_IF(D, G, i, ((z2 :- D.b[i]) 
        :- mean(sign(z), G.w)/d * (mean(z3, G.w) :- z3)) / G.W)
}

void _dstat_sum_mad_med_med(`Data' D, `Grp' G, `Int' i)
{
    `RC' z, z2, q, d

    dstat_init_Ys(D, G)
    q    = J(3,1,.)
    q[2] = _mm_median(G.Ys, G.ws)
    z = abs(G.Y :- q[2])
    D.b[i] = mm_median(z, G.ws) // z not sorted!
    if (D.noIF) return
    q[1] = q[2] - D.b[i]
    q[3] = q[2] + D.b[i]
    d = _dstat_sum_d(D, G, q)
    z  = (z :<= D.b[i]) 
    z2 = (G.Y :<= q[2])
    dstat_set_IF(D, G, i, 
        (mean(z, G.w) :- z) / (G.W * (d[1]+d[3]))                 // main part
      + (mean(z2, G.w) :- z2) * ((d[1]-d[3])/(G.W*d[2]*(d[1]+d[3]))) // median
        )
}

void dstat_sum_madn(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' args
    `RS' l, c
    
    if (o!="") {
        args = strtoreal(tokens(o))
        l = args[1]
    }
    else l = O[3]
    dstat_sum_mad(D, G, i, o, O)
    if (l) c = sqrt(pi() / 2)
    else   c = 1 / invnormal(0.75)
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void dstat_sum_mae(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' args
    `RS' l, t
    
    if (o!="") {
        args = strtoreal(tokens(o))
        l = args[1]
        if (length(args)>1) t = args[2]
        else                t = O[4]
    }
    else {; l = O[3]; t = O[4]; }
    if (l) _dstat_sum_mae_mean(D, G, i, t)
    else   _dstat_sum_mae_med(D, G, i, t)
}

void _dstat_sum_mae_mean(`Data' D, `Grp' G, `Int' i, `RS' t)
{
    `RC' z
    
    z = abs(G.Y :- t)
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void _dstat_sum_mae_med(`Data' D, `Grp' G, `Int' i, `RS' t)
{
    `RC' q, z, d
    
    dstat_init_Ys(D, G)
    z = abs(G.Y :- t)
    D.b[i] = mm_median(z, G.ws) // z not sorted!
    if (D.noIF) return
    q = (t - D.b[i]) \ (t + D.b[i])
    d = _dstat_sum_d(D, G, q)
    z = (z :<= D.b[i]) 
    dstat_set_IF(D, G, i, (mean(z, G.w) :- z) / (G.W * (d[1]+d[2])))
}

void dstat_sum_maen(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' args
    `RS' l, c
    
    if (o!="") {
        args = strtoreal(tokens(o))
        l = args[1]
    }
    else l = O[3]
    dstat_sum_mae(D, G, i, o, O)
    if (l) c = sqrt(pi() / 2)
    else   c = 1 / invnormal(0.75)
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void dstat_sum_md(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m, cov
    `RC' F, B
    pragma unused o
    pragma unused O
    
    m   = _dstat_mean(G)
    F   = _mm_ranks(G.Ys, G.ws, 3, 1, 1)
    cov = cross(G.Ys, G.ws, F) / G.W
    D.b[i] = 4*cov - 2*m // note: MD = 2 * mean * Gini
    if (D.noIF) return
    // In analogy to the Gini coefficient the main moment condition of the MD
    // can be written as h = 2*(2*F - 1)*Y - MD
    B = J(G.N, 1, .)
    B[G.ps] = _dstat_sum_gini_topsum(G.Ys, G.Ys:*G.ws, G.W)
    F[G.ps] = F
    dstat_set_IF(D, G, i, ((4*F :- 2):*G.Y :- D.b[i] :+ 4*(B :- cov)) / G.W)
}

void dstat_sum_mdn(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' c
    
    dstat_sum_md(D, G, i, o, O)
    c = sqrt(pi())/2
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void dstat_sum_mscale(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' bp, k, med, d, delta
    `RC' z, psi, zmed
    `T'  S
    
    if (o!="") bp = strtoreal(o)
    else       bp = O[3]
    dstat_init_Ys(D, G)
    med = _mm_median(G.Ys, G.ws)
    S = mm_mscale(G.Y, G.w, bp, ., med)
    D.b[i] = mm_mscale_b(S)
    if (D.noIF) return
    k = mm_mscale_k(S)
    delta = mm_mscale_delta(S)
    z = (G.Y :- med) / D.b[i]
    psi = mm_biweight_psi(z, k)
    zmed = (G.Y :<= med)
    d = _dstat_sum_d(D, G, med)
    dstat_set_IF(D, G, i, (D.b[i] * (mm_biweight_rho(z, k) :- delta)
        - mean(psi, G.w)/d * (mean(zmed, G.w) :- zmed))
        / (mean(psi:*z, G.w) * G.W))
}

void dstat_sum_qn(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' c
    `RC' z1, z2, F, d
    pragma unused o
    pragma unused O
    
    dstat_init_Ys(D, G)
    D.b[i] = mm_qn(G.Ys, G.ws, D.wtype==1)
    if (D.noIF) return
    c  = sqrt(2) * invnormal(5/8)
    z1 = G.Ys :+ D.b[i]*c
    z2 = G.Ys :- D.b[i]*c
    F  = _mm_relrank(G.Ys, G.ws, z1, 1) :- _mm_relrank(G.Ys, G.ws, z2, 1)
    d  = _dstat_sum_d(D, G, z1)
    F[G.ps] = F
    dstat_set_IF(D, G, i, (mean(F, G.w) :- F) * (1 / (c * mean(d, G.ws) * G.W)))
}

void dstat_sum_skewness(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m, sd
    `RC' z
    pragma unused o
    pragma unused O
    pragma unset m
    pragma unset sd
    
    D.b[i] = _dstat_skewness(G.Y, G.w, m, sd)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    z = (G.Y :- m) / sd 
    dstat_set_IF(D, G, i, (z:^3 :- 3*z :- D.b[i]*(3/2)*(z:^2:-1) :- D.b[i]) / G.W)
}

`RS' _dstat_skewness(`RC' Y, `RC' w, | `Rs' m, `Rs' sd) 
{   // replaces m and sd
    `RC' mv
    
    mv = mm_meanvariance0(Y, w) // variance without df correction
    m  = mv[1]
    sd = sqrt(mv[2])
    return(mean((Y:-m):^3, w) / sd^3)
}

void dstat_sum_qskew(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' p,    p1, p2, p3
    `RC' q, d, z1, z2, z3
    
    if (o!="") p = strtoreal(o) / 100
    else       p = O[3] / 100
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p \ 0.5 \ 1-p, D.qdef, D.wtype==1)
    D.b[i] = (q[1] + q[3] - 2*q[2]) / (q[3] - q[1])
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    d = _dstat_sum_d(D, G, q)
    z1 = (G.Y :<= q[1]); p1 = mean(z1, G.w)
    z2 = (G.Y :<= q[2]); p2 = mean(z2, G.w)
    z3 = (G.Y :<= q[3]); p3 = mean(z3, G.w)
    dstat_set_IF(D, G, i, ((q[3]-q[2])*((p1 :- z1)/d[1] - (p2 :- z2)/d[2])
                         - (q[2]-q[1])*((p2 :- z2)/d[2] - (p3 :- z3)/d[3])) 
                         * (2 / ((q[3] - q[1])^2 * G.W)))
}

void dstat_sum_mc(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' mc, q, dq, dH
    `RC' z, F1, F2, d, IF
    pragma unused o
    pragma unused O
    
    dstat_init_Ys(D, G)
    D.b[i] = mm_mc(G.Ys, G.ws, D.wtype==1)
    if (D.noIF) return
    mc = D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Ys, G.ws)
    dq = _dstat_sum_d(D, G, q)
    F1 = _dstat_sum_mc_F(G, z = (G.Y * (mc-1) :+ 2*q) / (mc+1))
    F2 = _dstat_sum_mc_F(G,     (G.Y * (mc+1) :- 2*q) / (mc-1))
    d  = _dstat_sum_d(D, G, z) :* (G.Y :>= q)
    dH = mean(8 * d :* ((G.Y :- q) / (mc+1)^2), G.w)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    IF = (1 :- 4*F1:*(G.Y:>q) :- 4*(F2:-.5):*(G.Y:<q) 
        :+ sign(G.Y:-q)*(1 - 4*mean(d, G.w)/(dq*(mc+1)))) / dH
    IF = IF :- mean(IF, G.w) // make sure that IF is centered at zero
    dstat_set_IF(D, G, i, IF / G.W)
}

`RC' _dstat_sum_mc_F(`Grp' G, `RC' z)
{
    `RC' F, p
    
    p = order(z, 1)
    F = J(rows(z), 1, .)
    F[p] = _mm_relrank(G.Ys, G.ws, z[p], 1)
    return(F)
}

void dstat_sum_kurtosis(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' sd, sk
    `RC' m, z
    pragma unused o
    pragma unused O
    
    m  = mm_meanvariance0(G.Y, G.w) // variance without df correction
    sd = sqrt(m[2])
    m  = m[1]
    z = G.Y :- m
    D.b[i] = mean(z:^4, G.w) / sd^4
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    sk = mean(z:^3, G.w) / sd^3
    z = z / sd
    dstat_set_IF(D, G, i, ((z:^2 :- D.b[i]):^2 :- 4*sk*z :- D.b[i]*(D.b[i]-1)) / G.W) 
}

void dstat_sum_qw(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' P
    `RC' p, q, d
    `RM' Z
    
    if (o!="") P = strtoreal(o) / 100
    else       P = O[3] / 100
    p = P/2 \ P \ .5 - P/2 \ .5 + P/2 \ 1 - P \ 1 - P/2
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p, D.qdef, D.wtype==1)
    D.b[i] = (q[6] - q[4] + q[3] - q[1])/ (q[5] - q[2])
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    d = _dstat_sum_d(D, G, q)
    Z = (G.Y :<= q[1]), (G.Y :<= q[2]), (G.Y :<= q[3]),
        (G.Y :<= q[4]), (G.Y :<= q[5]), (G.Y :<= q[6])
    p = mean(Z, G.w)' // replace p by empirical values
    dstat_set_IF(D, G, i, ((q[5]-q[2])*((p[6]:-Z[,6])/d[6] - (p[4]:-Z[,4])/d[4]
                                + (p[3]:-Z[,3])/d[3] - (p[1]:-Z[,1])/d[1])
                         - (q[6] - q[4] + q[3] - q[1]) * ((p[5]:-Z[,5])/d[5]
                                - (p[2]:-Z[,2])/d[2])) / ((q[5]-q[2])^2 * G.W))
}

void dstat_sum_lqw(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' P
    `RC' p, q, d
    `RM' Z
    
    if (o!="") P = strtoreal(o) / 100
    else       P = O[3] / 100
    p = P/2 \ .25 \ .5 - P/2
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p, D.qdef, D.wtype==1)
    D.b[i] = - (q[1] + q[3] - 2*q[2]) / (q[3] - q[1])
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    d = _dstat_sum_d(D, G, q)
    Z = (G.Y :<= q[1]), (G.Y :<= q[2]), (G.Y :<= q[3])
    p = mean(Z, G.w)' // replace p by empirical values
    dstat_set_IF(D, G, i, 
        ((q[2]-q[1])*((p[2]:-Z[,2])/d[2] - (p[3]:-Z[,3])/d[3])
       - (q[3]-q[2])*((p[1]:-Z[,1])/d[1] - (p[2]:-Z[,2])/d[2]))
       * (2 / ((q[3] - q[1])^2 * G.W)))
}

void dstat_sum_rqw(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' P
    `RC' p, q, d
    `RM' Z
    
    if (o!="") P = strtoreal(o) / 100
    else       P = O[3] / 100
    p = .5 + P/2 \ .75 \ 1 - P/2
    dstat_init_Ys(D, G)
    q = _mm_quantile(G.Ys, G.ws, p, D.qdef, D.wtype==1)
    D.b[i] = (q[1] + q[3] - 2*q[2]) / (q[3] - q[1])
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    d = _dstat_sum_d(D, G, q)
    Z = (G.Y :<= q[1]), (G.Y :<= q[2]), (G.Y :<= q[3])
    p = mean(Z, G.w)' // replace p by empirical values
    dstat_set_IF(D, G, i, 
        ((q[3]-q[2])*((p[1]:-Z[,1])/d[1] - (p[2]:-Z[,2])/d[2])
       - (q[2]-q[1])*((p[2]:-Z[,2])/d[2] - (p[3]:-Z[,3])/d[3]))
       * (2 / ((q[3] - q[1])^2 * G.W)))
}

void dstat_sum_lmc(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' med, mc, q, dq, dH, gmed, Fmed
    `RC' p, z, F1, F2, d, IF
    pragma unused o
    pragma unused O
    
    dstat_init_Ys(D, G)
    med = _mm_median(G.Ys, G.ws)
    p = selectindex(G.Ys:<med)
    if (rows(p)==0) D.b[i] = . // no obs
    else D.b[i] = -mm_mc(G.Ys[p], (rows(G.ws)!=1 ? G.ws[p]: G.ws), D.wtype==1)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    mc = -D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Ys[p], (rows(G.ws)!=1 ? G.ws[p]: G.ws))
    dq = _dstat_sum_d(D, G, q)
    F1 = _dstat_sum_mc_F(G, z = (G.Y * (mc-1) :+ 2*q) / (mc+1))
    F2 = _dstat_sum_mc_F(G,     (G.Y * (mc+1) :- 2*q) / (mc-1))
    d  = _dstat_sum_d(D, G, z) :* (G.Y:>=q :& G.Y:<=med)
    dH = mean(32 * d :* ((q :- G.Y) / (mc+1)^2), G.w)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    gmed = (med*(mc-1) + 2*q) / (mc+1)
    Fmed = _mm_relrank(G.Ys, G.ws, gmed, 1)
    IF = (1 :- 16*F1:*(G.Y:>q :& G.Y:<med) 
            :- 16*(F2:-.25):*(G.Y:<q :& G.Y:>gmed)
            :-  4*(G.Y:<gmed)
            :-  8*sign(G.Y:-med)*Fmed
            :+ (.25:-(G.Y:<q))*(4 - 32*mean(d, G.w)/(dq*(mc+1)))) / dH
    IF = IF :- mean(IF, G.w) // make sure that IF is centered at zero
    dstat_set_IF(D, G, i, IF / G.W)
}

void dstat_sum_rmc(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' med, mc, q, dq, dH, gmed, Fmed
    `RC' p, z, F1, F2, d, IF
    pragma unused o
    pragma unused O
    
    dstat_init_Ys(D, G)
    med = _mm_median(G.Ys, G.ws)
    p = selectindex(G.Ys:>med)
    if (rows(p)==0) D.b[i] = . // no obs
    else D.b[i] = mm_mc(G.Ys[p], (rows(G.ws)!=1 ? G.ws[p]: G.ws), D.wtype==1)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    mc = D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Ys[p], (rows(G.ws)!=1 ? G.ws[p]: G.ws))
    dq = _dstat_sum_d(D, G, q)
    F1 = 1 :- _dstat_sum_mc_F(G, z = (G.Y * -(mc+1) :+ 2*q) /  (1-mc))
    F2 = 1 :- _dstat_sum_mc_F(G,     (G.Y *  (1-mc) :- 2*q) / -(mc+1))
    d  = _dstat_sum_d(D, G, z) :* (G.Y:<=q :& G.Y:>=med)
    dH = mean(32 * d :* ((G.Y :- q) / (1-mc)^2), G.w)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    gmed = (med*(mc+1) - 2*q) / (mc-1)
    Fmed = 1 - _mm_relrank(G.Ys, G.ws, gmed, 1)
    IF = (1 :- 16*F1:*(G.Y:<q :& G.Y:>med) 
            :- 16*(F2:-.25):*(G.Y:>q :& G.Y:<gmed)
            :-  4*(G.Y:>gmed)
            :-  8*sign(med:-G.Y)*Fmed
            :+ (.25:-(G.Y:>q))*(4 - 32*mean(d, G.w)/(dq*(1-mc)))) / dH
    IF = IF :- mean(IF, G.w) // make sure that IF is centered at zero
    dstat_set_IF(D, G, i, IF / G.W)
}

void dstat_sum_gini(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_gini(0, D, G, i, o, O)
}

void dstat_sum_agini(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_gini(1, D, G, i, o, O)
}

void _dstat_sum_gini(`Bool' abs, `Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' df, m, cp, c
    `RC' F, B
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    dstat_init_Ys(D, G)
    m  = _dstat_mean(G)
    F  = _mm_ranks(G.Ys, G.ws, 3, 1, 1)
    cp = cross(G.Ys, G.ws, F) / G.W
    if (df!=0) { // small-sample adjustment
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    if (abs) D.b[i] = c * (cp * 2 - m) // = c * (2 * cov)  with cov = cp - m/2
    else     D.b[i] = c * (cp * 2 / m - 1) // = c * (2 * cov / m)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    // The main moment condition of the Gini coefficient can be written as
    // h = (2*F - 1)*Y - G*Y (see Binder & Kovacevic 1995)
    // (the IF of the Gini coefficient has the same structure as the IF of the
    // relative PDF; see -reldist-)
    B = J(G.N, 1, .)
    B[G.ps] = _dstat_sum_gini_topsum(G.Ys, G.Ys:*G.ws, G.W)
    //B[G.ps] = _dstat_sum_gini_topsum(G.Ys, G.Ys:*G.ws, G.W, G.N)
    F[G.ps] = F
    if (abs) {
        dstat_set_IF(D, G, i, (((2*F :- 1):*G.Y + 2*(B :- cp))*c :- D.b[i]) / G.W)
    }
    else {
        dstat_set_IF(D, G, i, ((F :- cp/m):*G.Y :+ B :- cp) * ((2*c) / (m*G.W)))
        // = 1/m * (c*(2*F :- 1):*G.Y :- D.b[i]:*G.Y :+ c*2*(B :- cp)) / G.W
    }
}

`RC' _dstat_sum_gini_topsum(`RC' Y, `RC' Yw, `RS' W)
{
    return((quadsum(Yw) :- _mm_ranks(Y, Yw, 3, 1)) / W)
}
/*
`RC' _dstat_sum_gini_topsum(`RC' Y, `RC' Yw, `RS' W, `RS' N)
{
    `RC' p
    
    p = N::1
    return(_mm_ranks(-Y[p], Yw[p], 3, 1)[p]/W)
}
`RC' _dstat_sum_gini_topsum(`RC' Y, `RC' Yw, `RS' W, `RS' N)
{
    `Int' i, i1
    `RS'  y1
    `RC'  T
    
    T = quadrunningsum(Yw[N::1])[N::1] // reverse running sum of Yw
    for (i=N; i; i--) {
        i1 = i
        y1 = Y[i1]
        for (; i; i--) {
            if (Y[i]!=y1) break
        }
        i++
        if (i==i1) T[i] = T[i] - Yw[i]/2
        else       T[|i\i1|] = J(i1-i+1, 1, T[i] - quadsum(Yw[|i\i1|])/2)
    }
    return(T/W)
}
*/

void dstat_sum_mld(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m
    `RC' lnY
    pragma unused o
    pragma unused O
    
    m   = _dstat_mean(G)
    lnY = ln(G.Y)
    _dstat_sum_isvalid(lnY, D.yvars[G.j], "mld")
    D.b[i] = ln(m) - mean(lnY, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (((ln(m):-lnY) :- D.b[i]) + (G.Y :- m)/m) / G.W)
}

void _dstat_sum_isvalid(`RC' Y, `SS' v, `SS' s)
{
    if (hasmissing(Y)) {
        printf("{err}cannot compute %s for %s;" + 
            " nonpositive values not allowed\n", s, v)
        exit(411)
    }
}

void dstat_sum_theil(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m, delta
    `RC' lnY, z
    pragma unused o
    pragma unused O
    
    m   = _dstat_mean(G)
    lnY = ln(G.Y)
    _dstat_sum_isvalid(lnY, D.yvars[G.j], "theil")
    z   = G.Y :* lnY
    D.b[i] = mean(z, G.w)/m - ln(m)
    if (D.noIF) return
    delta = mean(z/m^2, G.w) :+ 1/m
    dstat_set_IF(D, G, i, (((z/m:-ln(m)) :- D.b[i]) - delta*(G.Y :- m)) / G.W)
}

void dstat_sum_cv(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m, sd, c, df
    pragma unset m
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    sd = sqrt(_dstat_variance(D, G, G.Y, df, m))
    D.b[i] = sd / m
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    dstat_set_IF(D, G, i, 
        (c*(G.Y :- m):^2 :- sd^2) / (2 * sd * m * G.W)   // sd
      + (G.Y :- m) * (-sd / (m^2 * G.W))                 // mean
        )
}

void dstat_sum_ge(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' a, c, m, mz, delta
    `RC' z
    
    if (o!="") a = strtoreal(o)
    else       a = O[3]
    if (a==0) {
        dstat_sum_mld(D, G, i, o, O)
        return
    }
    if (a==1) {
        dstat_sum_theil(D, G, i, o, O)
        return
    }
    m = _dstat_mean(G)
    c = 1 / (a * (a - 1))
    z = G.Y:^a
    _dstat_sum_isvalid(z, D.yvars[G.j], "ge("+strofreal(a)+")")
    mz = mean(z, G.w)
    D.b[i] = c * (mz/m^a - 1)
    if (D.noIF) return
    delta = 1 / ((a - 1) * m^(a + 1)) * mz
    dstat_set_IF(D, G, i, ((c*(z/m^a:-1) :- D.b[i]) - delta*(G.Y :- m)) / G.W)
}

void dstat_sum_atkinson(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' eps
    
    if (o!="") eps = strtoreal(o)
    else       eps = O[3]
    if (eps==1) _dstat_sum_atkinson1(D, G, i)
    else        _dstat_sum_atkinson(D, G, i, eps)
    
}

void _dstat_sum_atkinson1(`Data' D, `Grp' G, `Int' i)
{
    `RS' m, lm
    `RC' z
    
    m  = _dstat_mean(G)
    z  = ln(G.Y)
    _dstat_sum_isvalid(z, D.yvars[G.j], "atkinson(1)")
    lm = mean(z, G.w)
    D.b[i] = 1 - exp(lm) / m
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((G.Y:-m)/m :- (z :- lm)) * (exp(lm) / (m * G.W)))
}

void _dstat_sum_atkinson(`Data' D, `Grp' G, `Int' i, `RS' e)
{
    `RS' m, lm
    `RC' z
    
    m  = _dstat_mean(G)
    z  = G.Y:^(1-e)
    _dstat_sum_isvalid(z, D.yvars[G.j], "atkinson("+strofreal(e)+")")
    lm = mean(z, G.w)
    D.b[i] = 1 - lm^(1/(1-e)) / m
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((G.Y:-m) * (lm^(1/(1-e)) / m^2)
        :- (z :- lm) * (lm^(e/(1-e)) / ((1-e)*m))) / G.W)
}

void dstat_sum_lvar(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m, c, df
    `RC' z
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    z = ln(G.Y)
    _dstat_sum_isvalid(z, D.yvars[G.j], "lvar")
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    m = _dstat_mean(G)
    D.b[i] = mm_mse(z, G.w, ln(m)) * c
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    z = z :- ln(m)
    dstat_set_IF(D, G, i, (c*z:^2 :- D.b[i] :- 2*mean(z, G.w)/m:*(G.Y:-m)) / G.W)
}

void dstat_sum_vlog(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' z
    
    z = ln(G.Y)
    _dstat_sum_isvalid(z, D.yvars[G.j], "vlog")
    _dstat_sum_variance(D, G, i, z, o, O)
}

void dstat_sum_top(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' p
    
    if (o!="") p = strtoreal(o) / 100
    else       p = O[3] / 100
    _dstat_sum_share(D, G, i, (1-p)\1)
}

void dstat_sum_bottom(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' p
    
    if (o!="") p = strtoreal(o) / 100
    else       p = O[3] / 100
    _dstat_sum_share(D, G, i, 0\p)
}

void dstat_sum_mid(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' p
    
    if (o!="") p = strtoreal(tokens(o)') / 100
    else       p = O[(3,4)]' / 100
    _dstat_sum_share(D, G, i, p)
}

void dstat_sum_palma(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' b1, b2
    `RC' IF1
    
    pragma unused o
    pragma unused O
    
    _dstat_sum_share(D, G, i, 0\.4) // bottom 40%
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _dstat_sum_share(D, G, i, .9\1) // top 10%
    b2 = D.b[i]
    D.b[i] = b2 / b1
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / b1 - b2/b1^2 * IF1
}

void dstat_sum_qratio(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' b1, b2
    `RC' p, IF1
    
    if (o!="") p = strtoreal(tokens(o)') / 100
    else       p = O[(3,4)]' / 100
    _dstat_sum_q(D, G, i, p[1]) // lower quantile
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _dstat_sum_q(D, G, i, p[2]) // upper quantile
    b2 = D.b[i]
    D.b[i] = b2 / b1
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / b1 - b2/b1^2 * IF1
}

void dstat_sum_sratio(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' b1, b2
    `RC' p, IF1
    
    if (o!="") {
        p = strtoreal(tokens(o)')
        if (length(p)==2) p = O[3] \ p \ O[6]
    }
    else p = O[|3\6|]'
    p = p / 100
    _dstat_sum_share(D, G, i, p[|1\2|]) // lower quantile
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _dstat_sum_share(D, G, i, p[|3\4|]) // upper quantile
    b2 = D.b[i]
    D.b[i] = b2 / b1
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / b1 - b2/b1^2 * IF1
}

void dstat_sum_lorenz(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_lorenz(D, G, i, strtoreal(o) / 100, 0)
}

void _dstat_sum_lorenz(`Data' D, `Grp' G, `Int' i, `RC' at, `Int' t)
{
    D.b[i] = _dstat_lorenz(D, G, at, t)
    if (D.noIF) return
    D.IF[,i] = _dstat_lorenz_IF(D, G, at, t)
}

void dstat_sum_tlorenz(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    D.istot[i] = 1
    _dstat_sum_lorenz(D, G, i, strtoreal(o) / 100, 2)
}

void dstat_sum_glorenz(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_lorenz(D, G, i, strtoreal(o) / 100, 3)
}

void dstat_sum_alorenz(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_lorenz(D, G, i, strtoreal(o) / 100, 4)
}

void dstat_sum_elorenz(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_lorenz(D, G, i, strtoreal(o) / 100, 5)
}

void dstat_sum_share(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_share(D, G, i, strtoreal(tokens(o)') / 100)
}

void _dstat_sum_share(`Data' D, `Grp' G, `Int' i, `RC' at, | `Int' t, `Bool' d)
{
    if (args()<5) t = 0
    if (args()<6) d = 0
    
    if (at[1]>at[2]) D.b[i] = .
    else {
        D.b[i] = mm_diff(_dstat_lorenz(D, G, at, t))
        if (d) D.b[i] = D.b[i] * editmissing(1/(at[2]-at[1]), 0)
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = _dstat_share_IF(D, G, at, t, d)
}

void dstat_sum_dshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_share(D, G, i, strtoreal(tokens(o)') / 100, 0, 1)
}

void dstat_sum_tshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    D.istot[i] = 1
    _dstat_sum_share(D, G, i, strtoreal(tokens(o)') / 100, 2, 0)
}

void dstat_sum_gshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_share(D, G, i, strtoreal(tokens(o)') / 100, 3, 0)
}

void dstat_sum_ashare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_share(D, G, i, strtoreal(tokens(o)') / 100, 3, 1)
}

void dstat_sum_gci(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_gci(0, D, G, i, o, O)
}

void dstat_sum_aci(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_gci(1, D, G, i, o, O)
}

void _dstat_sum_gci(`Bool' abs, `Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Int' l, z
    `SR'  args
    `RS'  df, m, mF, cov, c
    `RM'  mv
    `RC'  F, B

    // settings
    args = tokens(o)
    l = length(args)
    if (l==0) {
        z  = selectindex(D.zvars:==D.zvar)
        df = O[3]
    }
    else if (l==1) {
        df = strtoreal(args[1])
        if (df<.) z = selectindex(D.zvars:==D.zvar)
        else {
            z  = selectindex(D.zvars:==args[1])
            df = O[3]
        }
    }
    else {
        z  = selectindex(D.zvars:==args[1])
        df = strtoreal(args[2])
    }
    // compute concentration index
    dstat_init_Ys(D, G, z)
    F   = _mm_ranks(G.Zs, G.ws, 3, 1, 1)
    mv  = mm_meanvariance0((G.Ys, F), G.ws)
    m   = mv[1,1]; mF  = mv[1,2]
    cov = mv[3,1]
    if (df!=0) { // small-sample adjustment
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    if (abs) D.b[i] = c * 2 * cov
    else     D.b[i] = c * 2 * cov / m
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    B = J(G.N, 1, .)
    B[G.ps] = _dstat_sum_gini_topsum(G.Zs, (G.Ys:-m):*G.ws, G.W)
    F[G.ps] = F
    if (abs) {
        dstat_set_IF(D, G, i, ((F:-mF):*(G.Y:-m) :+ B :- 2*cov) * ((2*c)/G.W))
        // = (2*c*F:*z :- D.b[i] :+ 2*c*(B :- cov) :- 2*c*mF:*z) / G.W)
        // with z = G.Y :- m
    }
    else {
        dstat_set_IF(D, G, i, ((F:-mF):*(G.Y:-m) :- cov*(1:+G.Y/m) :+ B)
            * ((2*c)/(m*G.W)))
        // = (2*c*F:*z :- D.b[i]:*G.Y :+ 2*c*(B :- cov) :- 2*c*mF:*z) / (m*G.W))
        // with z = G.Y :- m
    }
}

void dstat_sum_ccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_ccurve(D, G, i, 0, o)
}

void _dstat_sum_ccurve(`Data' D, `Grp' G, `Int' i, `Int' t, `SS' o)
{
    `SR'  args
    `RS'  at
    `Int' z
    
    args = tokens(o)
    at   = strtoreal(args[1]) / 100
    if (length(args)>1) z = selectindex(D.zvars:==args[2])
    else                z = selectindex(D.zvars:==D.zvar)
    D.b[i] = _dstat_lorenz(D, G, at, t, z)
    if (D.noIF) return
    D.IF[,i] = _dstat_lorenz_IF(D, G, at, t)
}

void dstat_sum_tccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    D.istot[i] = 1
    _dstat_sum_ccurve(D, G, i, 2, o)
}

void dstat_sum_gccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_ccurve(D, G, i, 3, o)
}

void dstat_sum_accurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_ccurve(D, G, i, 4, o)
}

void dstat_sum_eccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_ccurve(D, G, i, 5, o)
}

void dstat_sum_cshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_cshare(D, G, i, 0, 0, o)
}

void _dstat_sum_cshare(`Data' D, `Grp' G, `Int' i, `Int' t, `Bool' d, `SS' o)
{
    `SR'  args
    `RC'  at
    `Int' z
    
    args = tokens(o)
    at   = strtoreal(args[|1\2|]') / 100
    if (length(args)>2) z = selectindex(D.zvars:==args[3])
    else                z = selectindex(D.zvars:==D.zvar)
    if (at[1]>at[2]) D.b[i] = .
    else {
        D.b[i] = mm_diff(_dstat_lorenz(D, G, at, t, z))
        if (d) D.b[i] = D.b[i] * editmissing(1/(at[2]-at[1]), 0)
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = _dstat_share_IF(D, G, at, t, d)
}

void dstat_sum_dcshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_cshare(D, G, i, 0, 1, o)
}

void dstat_sum_tcshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    D.istot[i] = 1
    _dstat_sum_cshare(D, G, i, 2, 0, o)
}

void dstat_sum_gcshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_cshare(D, G, i, 3, 0, o)
}

void dstat_sum_acshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_cshare(D, G, i, 3, 1, o)
}

void dstat_sum_watts(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC'  pl, Z
    pragma unused O

    if (o!="") {
        pl = strtoreal(o)
        if (pl>=.) pl = D.PL[G.p, selectindex(D.plvars:==o)]
    }
    else {
        if (D.plvar!="") pl = D.PL[G.p, selectindex(D.plvars:==D.plvar)]
        else             pl = D.pline
    }
    Z = (ln(pl) :-  ln(G.Y)) :* (G.Y :< pl)
    _dstat_sum_isvalid(Z, D.yvars[G.j], "watts")
    D.b[i] = mean(Z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (Z :- D.b[i]) / G.W)
}

void dstat_sum_fgt(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `SR'  args
    `Int' l
    `RS'  pl, a
    `RC'  Z

    args = tokens(o)
    l = length(args)
    if (l==0) {
        a  = O[3]
        if (D.plvar!="") pl = D.PL[G.p, selectindex(D.plvars:==D.plvar)]
        else             pl = D.pline
    }
    else if (l==1) {
        a  = strtoreal(o)
        if (D.plvar!="") pl = D.PL[G.p, selectindex(D.plvars:==D.plvar)]
        else             pl = D.pline
    }
    else {
        a  = strtoreal(args[1])
        pl = strtoreal(args[2])
        if (pl>=.) pl = D.PL[G.p, selectindex(D.plvars:==args[2])]
    }
    Z = (pl :- G.Y) :* (G.Y :<= pl)
    Z = (Z:/pl):^a :* (G.Y :<= pl)
    if (hasmissing(Z)) D.b[i] = .
    else               D.b[i] = mean(Z, G.w)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (Z :- D.b[i]) / G.W)
}

end

exit




