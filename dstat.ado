*! version 1.2.2  27nov2021  Ben Jann

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
    local version : di "version " string(_caller()) ":"
    if `"`subcmd'"'==substr("frequency",1,max(4,strlen(`"`subcmd'"'))) {
        // alias for -proportion, frequency-
        local subcmd proportion
        Parse_frequency `00'
    }
    else {
        capt Parse_subcmd `subcmd' // expands subcmd
        if _rc==1 exit _rc
        if _rc { // try summarize
            local 00 `"`subcmd' `00'"'
            local subcmd summarize
        }
        else if "`subcmd'"=="histogram" {
            Parse_hist_discrete `00' // redirect to proportion if nocategprical
        }
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

program Parse_frequency
    _parse comma lhs 0 : 0
    syntax [, FREQuency * ]
    c_local 00 `lhs', frequency `options'
end

program Parse_hist_discrete
    _parse comma lhs 0 : 0
    syntax [, DISCrete NOCATegorical PROPortion * ]
    if "`discrete'"=="" exit
    c_local subcmd proportion
    c_local 00 `lhs', nocategorical `options'
end

program Parse_subcmd
    if `"`0'"'==substr("density",1,max(1,strlen(`"`0'"'))) {
        c_local subcmd density
        exit
    }
    if `"`0'"'=="pdf" {
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
    if `"`0'"'=="tip" {
        c_local subcmd tip
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
        GRaph GRaph2(passthru) NOCI NOPValues PValues cref * ]
    if "`noci'"!="" {
        di as err "option {bf:noci} not allowed"
        exit 198
    }
    _get_diopts diopts options, `options'
    _get_eformopts, soptions eformopts(`options') allowed(__all__)
    local diopts `diopts' `s(eform)'
    local options `level' `s(options)'
    if `"`options'"'!="" local lhs `lhs', `options'
    c_local diopts `diopts' `level' `citype' `header' `notable' `table'/*
        */ `nopvalues' `pvalues' `cref' `graph' `graph2'
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
    if "`subcmd'"=="tip" exit
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
    syntax [anything] [if] [in], [ SCores RIF COMpact QUIetly ]
    _score_spec `anything', scores `bmat'
    local vlist `s(varlist)'
    Predict_compute_IFs, generate(`vlist', `rif' `compact') // updates vlist
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
        */ qdef(`e(qdef)') vformat(`e(vformat)') `e(novalues)' `e(nocasewise)'
    // - over()
    if `"`e(over)'"'!="" {
        local over
        if `"`e(over_select)'"'!="" {
            local over select(`e(over_select)')
        }
        if `"`e(over_contrast)'"'!="" {
            if `"`e(over_contrast)'"'=="total" {
                local over `over' contrast
            }
            else {
                local over `over' contrast(`e(over_contrast)')
            }
            local over `over' `e(over_ratio)'
        }
        local over `over' `e(over_accumulate)'
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
    // - tip
    else if "`subcmd'"=="tip" {
        local cmdline `cmdline' `e(pstrong)' `e(absolute)' pline(`e(pline)')
    }
    // - summarize
    else if "`subcmd'"=="summarize" {
        local cmdline `cmdline' `e(relax)' `e(pstrong)'
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
    /*qui*/ Estimate `subcmd' `cmdline'
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
            if `"`e(frequency)'"'!=""                   local citype log
            else if `"`e(proportion)'`e(percent)'"'!="" local citype logit
            else                                        local citype log
        }
        else if inlist(`"`subcmd'"',"cdf","ccdf") {
            if `"`e(frequency)'"'!="" local citype log
            else                      local citype logit
        }
        else if `"`subcmd'"'=="proportion" {
            if `"`e(frequency)'"'!="" local citype log
            else                      local citype logit
        }
        if "`citype'"=="logit" {
            // relevant for proportion, cdf, histogram
            if `"`e(over_accumulate)'"'!="" & `"`e(unconditional)'"'=="" {
                local citype log
            }
        }
        if `"`e(over_contrast)'"'!="" local citype normal
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
    if "`e(percent)'"!="" local scale = 100
    else                  local scale = 1
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
    syntax [, citype(passthru) noHeader NOTABle TABle ///
        NOPValues PValues cref GRaph vsquish * ]
    local contrast `"`e(over_contrast)'"'
    if (`"`contrast'"'=="" | `"`e(over_ratio)'"'=="ratio") & "`pvalues'"=="" {
        local nopvalues nopvalues
    }
    if "`header'"=="" {
        if "`nopvalues'"!="" {
            local w1 15
            local c1 35
        }
        else {
            local w1 17
            local c1 49
        }
        local c2 = `c1' + `w1' + 1
        local w2 10
        _Replay_header, `nopvalues' head2left(`w1') head2right(`w2')
        if "`subcmd'"=="density" {
            di as txt _col(`c1') "Kernel" _col(`c2') "= " /*
                */as res %10s abbrev(e(kernel),10)
            mata: st_local("bwidth", ///
                mm_isconstant(st_matrix("e(bwidth)")) ? ///
                "%10.0g el(e(bwidth), 1, 1)" : ///
                `""{stata matrix list e(bwidth):{bf:{ralign 10:e(bwidth)}}}""')
            di as txt _col(`c1') "Bandwidth" _col(`c2') "= " as res `bwidth'
        }
        else if "`subcmd'"=="quantile" {
            local qdef = e(qdef)
            if `qdef'==10 local qdef "hdquantile"
            else          local qdef "qdef(`qdef')"
            di as txt _col(`c1') "Quantile type" _col(`c2') "= " as res %10s "`qdef'"
        }
        else if "`subcmd'"=="lorenz" | "`subcmd'"=="share" {
            if `"`e(zvar)'"'!="" {
                di as txt _col(`c1') "Sort variable" _col(`c2') "= " as res %10s e(zvar)
            }
        }
        else if "`subcmd'"=="tip" {
            di as txt _col(`c1') "Poverty line" _col(`c2') "= " as res %10s e(pline)
        }
        else if "`subcmd'"=="summarize" {
            if `"`e(zvar)'"'!="" {
                di as txt _col(`c1') "Secondary var." _col(`c2') "= " as res %10s e(zvar)
            }
            if `"`e(pline)'"'!="" {
                di as txt _col(`c1') "Poverty line" _col(`c2') "= " as res %10s e(pline)
            }
        }
        if `"`contrast'"'!="" {
            if real(`"`contrast'"')>=. local clbl `"`contrast'"'
            else local clbl = abbrev(`"`contrast'.`e(over)'"', 10)
            di as txt _col(`c1') "Contrast" _col(`c2') "= " as res %10s `"`clbl'"'
        }
        if `"`e(balance)'"'!="" {
            di as txt _col(`c1') "Balancing:"
            local wd = `c2' - `c1' - 1
            di as txt _col(`c1') "{ralign `wd':method}" _col(`c2') "= " /*
                */ as res %10s e(balmethod)
            if `"`e(balref)'"'=="" local balref "total"
            else local balref = abbrev(`"`e(balref)'.`e(over)'"', 10)
            di as txt _col(`c1') "{ralign `wd':reference}" _col(`c2') "= " /*
                */ as res %10s `"`balref'"'
            mata: st_local("balance", ///
                udstrlen(st_global("e(balance)"))<=10 ? ///
                "%10s e(balance)" : ///
                `""{stata di as txt e(balance):{bf:e(balance)}}""')
            di as txt _col(`c1') "{ralign `wd':controls}" _col(`c2') "= " as res `balance'
        }
    }
    if ("`table'"!="" | "`graph'"=="") & "`notable'"=="" {
        if "`header'"=="" {
            if `"`e(over)'"'!="" {
                if "`subcmd'"=="summarize" {
                    if !(`e(N_stats)'==1 & `e(N_vars)'==1) _svy_summarize_legend
                    else di ""
                }
                else _svy_summarize_legend
            }
            else di ""
        }
        if "`subcmd'"=="summarize" {
            if e(N_stats)>1 local vsquish vsquish
            local options `vsquish' `options'
        }
        if "`cref'"!="" local contrast
        local vmatrix
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
        else {
            if `"`contrast'"'!="" {
                tempname V
                matrix `V' = e(V)
            }
        }
        local cimatrix
        if c(stata_version)>=15 {
            capt confirm matrix e(ci)
            if _rc==1 exit _rc
            if _rc==0 {
                if `"`contrast'"'!="" {
                    tempname CI
                    matrix `CI' = e(ci)
                    local cimatrix cimatrix(`CI')
                }
                else local cimatrix cimatrix(e(ci))
                if `"`e(citype)'"'!="normal" {
                    local cimatrix `cimatrix' cititle(`e(citype)' transformed)
                }
            }
        }
        if `"`contrast'"'!="" {
            tempname B
            matrix `B' = e(b)
            mata: dstat_drop_cref("`B'", 0, 1)
            if "`V'"!=""  mata: dstat_drop_cref("`V'", 1, 1)
            if "`CI'"!="" mata: dstat_drop_cref("`CI'", 0, 1)
        }
        _Replay_table "`B'" "`V'" `nopvalues' `vmatrix' `cimatrix' `options'
        if c(stata_version)<15 {
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
        if "`header'"=="" di ""
        di as txt "({stata dstat:coefficients table} suppressed)"
    }
    else if "`header'"=="" di ""
end

prog _Replay_header, eclass
    syntax [, nopvalues * ]
    if      c(stata_version)<17            local options
    else if d(`c(born_date)')<d(13jul2021) local options
    if "`pvalues'"=="" { // table includes p-values
        _coef_table_header, nomodeltest `options'
        exit
    }
    nobreak { // no p-values: mimic header of -total-
        ereturn local cmd "total"
        capture noisily break {
            _coef_table_header, nomodeltest `options'
        }
        local rc = _rc
        ereturn local cmd "dstat"
        if `rc' exit `rc'
    }
end

prog _Replay_table, eclass
    gettoken B 0 : 0
    gettoken V 0 : 0
    if "`B'"=="" {
        _coef_table, showeqns `0'
        exit
    }
    tempname ecurrent
    _estimates hold `ecurrent', copy restore
    local eqs: coleq `B', quoted
    local eqs: list uniq eqs
    local k_eq: list sizeof eqs
    capt confirm matrix e(V)
    if _rc==1 exit _rc
    if _rc ereturn repost b = `B', resize
    else   ereturn repost b = `B' V = `V', resize
    eret scalar k_eq = `k_eq'
    eret scalar k_eform = `k_eq'
    _coef_table, showeqns `0'
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
        SELect(str) GSELect(str) PSELect(str) cref ///
        BYOPTs(str) PLOTLabels(str asis) * ]
    _Graph_parse_select "" `"`select'"'
    _Graph_parse_select g `"`gselect'"'
    _Graph_parse_select p `"`pselect'"'
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
    local contrast `"`e(over_contrast)'"'
    if "`cref'"!="" local contrast
    tempname B
    matrix `B' = e(b)
    if `"`contrast'"'!="" mata: dstat_drop_cref("`B'", 0, 1)
    local b matrix(`B')
    if `"`subcmd'"'!="summarize" {
        if !(`"`subcmd'"'=="proportion" & `"`e(novalues)'"'=="" & `"`e(categorical)'"'!="") {
            tempname AT
            matrix `AT' = e(at)
            if `"`contrast'"'!="" mata: dstat_drop_cref("`AT'", 0, 1)
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
            if `"`contrast'"'!="" mata: dstat_drop_cref("`CI'", 0, 1)
            local ci ci(`CI')
            if inlist(`"`subcmd'"', "histogram", "share") {
                tempname BCI ATCI
                mat `BCI' = `B'
                mat `ATCI' = `AT'[2,1...] // bin midpoints
                if `"`contrast'"'!="" mata: dstat_drop_cref("`ATCI'", 0, 1)
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
            _Graph_overlabels `"`contrast'"' // returns overlabels
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
            if `"`gselect'"'=="reverse" {
                mata: st_local("gselect", ///
                    invtokens(tokens(st_local("jlist"))[`n`jj''::1]))
            }
            mata: dstat_graph_select("jlist", "gselect")
            local n`jj': list sizeof jlist
        }
        if `"`pselect'"'!="" & `n`ii''>1 {
            if `"`pselect'"'=="reverse" {
                mata: st_local("pselect", ///
                    invtokens(tokens(st_local("ilist"))[`n`ii''::1]))
            }
            mata: dstat_graph_select("ilist", "pselect")
            local n`ii': list sizeof ilist
        }
        if `n`jj''==0 | `n`ii''==0 {
            di as txt "(nothing to plot)"
            exit
        }
    }
    
    // (default) rendering
    if inlist(`"`subcmd'"', "density", "cdf", "ccdf", "quantile", "lorenz", "tip") {
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

program _Graph_parse_select
    args p args
    local 0 `", `args'"'
    capt syntax [, Reverse ]
    if _rc==1 exit _rc
    if _rc {
        local 0 `", `p'select(`args')"'
        syntax [, `p'select(numlist int >0) ]
        c_local `p'select `"``p'select'"'
        exit
    }
    c_local `p'select `reverse'
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
    args contrast
    local overlabels `"`e(over_labels)'"'
    local overlevels `"`e(over_namelist)'"'
    if `"`overlabels'"'==`"`overlevels'"' {
        local over `"`e(over)'"'
        local overlabels
        local space
        foreach oval of local overlevels {
            local overlabels `"`overlabels'`space'`"`over'=`oval'"'"'
            local space " "
        }
    }
    if `"`e(total)'"'!="" {
        local overlabels `"`overlabels' Total"'
    }
    if `"`contrast'"'!="" {
        if      `"`contrast'"'=="lag"   local idrop 1
        else if `"`contrast'"'=="lead"  local idrop: list sizeof overlabels
        else if `"`contrast'"'=="total" local idrop: list sizeof overlabels
        else local idrop: list posof `"`contrast'"' in overlevels
        local olab0: copy local overlabels
        local overlabels
        local space
        local i 0
        foreach olab of local olab0 {
            local ++i
            if `i'==`idrop' continue
            local overlabels `"`overlabels'`space'`"`olab'"'"'
            local space " "
        }
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
    // get CIs from e(ci) if appropriate
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
        local opts n(numlist int >=1 max=1) COMmon at(str)/*
            */ range(numlist min=2 max=2) /*
            */ UNConditional
    }
    else if "`subcmd'"=="histogram" {
        local opts n(passthru) COMmon at(str) /*
            */ ep PROPortion PERcent FREQuency UNConditional
    }
    else if inlist("`subcmd'","cdf","ccdf") {
        local opts n(numlist int >=1 max=1) COMmon at(str) /*
            */ range(numlist min=2 max=2) /*
            */ mid FLoor DISCrete PERcent FREQuency IPolate UNConditional
    }
    else if "`subcmd'"=="proportion" {
        local opts NOCATegorical at(str) PERcent FREQuency /*
            */ UNConditional
    }
    else if "`subcmd'"=="quantile" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist min=2 max=2 >=0 <=1)
    }
    else if "`subcmd'"=="lorenz" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist min=2 max=2 >=0 <=1) /*
            */ gap sum GENERALized ABSolute PERcent Zvar(varname)
    }
    else if "`subcmd'"=="share" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ PROPortion PERcent sum AVErage GENERALized Zvar(varname)
    }
    else if "`subcmd'"=="tip" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist min=2 max=2 >=0 <=1) /*
            */ ABSolute PLine(passthru) PSTRong
    }
    else if "`subcmd'"=="summarize" {
        local lhs anything(id="varlist")
        local opts relax Zvar(varname) PLine(passthru) PSTRong
    }
    else exit 499
    syntax `lhs' [if] [in] [fw iw pw/], [ ///
            NOVALues VFormat(str) NOCASEwise ///
            qdef(numlist max=1 int >=0 <=10) HDQuantile ///
            Over(str) TOTal BALance(str) ///
            vce(str) NOSE Level(cilevel) ///
            Generate(passthru) rif(str) Replace ///
            `opts' * ]
    Parse_over `over'
    if "`over_accumulate'"!="" local common common
    if "`over_contrast'"!=""   local common common
    if `"`generate'"'!="" & `"`rif'"'!="" {
        di as err "{bf:generate()} and {bf:rif()} not both allowed"
        exit 198
    }
    if `"`vformat'"'!="" {
        confirm format `vformat'
    }
    else local vformat %9.0g
    if "`hdquantile'"!="" {
        if "`qdef'"!="" & "`qdef'"!="10" {
            di as err "{bf:qdef()} and {bf:hdquantile} not both allowed"
        }
        local qdef 10
    }
    else {
        if "`qdef'"=="" local qdef 2
    }
    if "`subcmd'"=="proportion" {
        if "`nocategorical'"=="" local categorical "categorical"
    }
    Parse_densityopts, `options'
    Parse_at "`subcmd'" "`categorical'" `"`n'"' `"`at'"' `"`range'"' 
    if "`subcmd'"!="summarize" {
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
    else if "`subcmd'"=="tip" {
        if `"`at'`n'"'=="" local n 101
        if `"`pline'"'=="" {
            di as err "option {bf:pline()} is required"
            exit 198
        }
        Parse_pline, `pline' // returns pline, plvar
        local zvars `plvar'
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
        local total
        local common
    }
    if "`compact'"!="" {
        if `"`bal_method'"'!="" {
            di as err "{bf:compact} not allowed with {bf:balance()}"
            exit 198
        }
        if "`unconditional'"!="" {
            di as err "{bf:compact} not allowed with {bf:unconditional}"
            exit 198
        }
        if "`over_contrast'"!="" {
            di as err "{bf:compact} not allowed with {bf:over(, contrast)}"
            exit 198
        }
        if "`over_accumulate'"!="" {
            di as err "{bf:compact} not allowed with {bf:over(, accumulate)}"
            exit 198
        }
    }
    c_local generate_quietly `generate_quietly'
    
    // sample and weights
    if "`nocasewise'"=="" {
        marksample touse
        markout `touse' `zvars'
    }
    else marksample touse, novarlist
    markout `touse' `over' `bal_varlist'
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
        if !inlist("`over_contrast'","","contrast","lag","lead") {
            if `:list over_contrast in overlevels'==0 {
                di as err "{bf:over(, contrast())}: no observations in reference distribution"
                exit 2000
            }
            if `"`over_select'"'!="" {
                if `:list over_contrast in over_select'==0 {
                    local over_select: list over_contrast | over_select
                }
            }
        }
        if `"`over_select'"'!="" {
            local tmp
            foreach o of local over_select {
                if `:list o in overlevels' {
                    local tmp `tmp' `o'
                }
            }
            local overlevels: list uniq tmp
            if `:list sizeof overlevels'==0 {
                di as err "{bf:over(, select())}: must select at least one existing group"
                exit 499
            }
            local over_select "`overlevels'"
        }
        if "`over_contrast'"=="contrast" {
            if `"`total'"'!="" local over_contrast "total"
            else gettoken over_contrast : overlevels // use first subpop
        }
        if "`over_contrast'"!="" {
            if (`:list sizeof overlevels' + ("`total'"!=""))<2 {
                di as err "{bf:over(, contrast)}: need at least one comparison group"
                exit 499
            }
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
    tempname b nobs sumw cref id omit IFtot AT BW _N _W /*HCR PGI*/
    mata: dstat()
    // process IFs
    if "`generate'"!="" {
        local coln: colfullnames `b'
        if "`compact'"!="" {
            local CIFs
            local i 0
            local j 0
            local gid0 ""
            foreach IF of local IFs {
                gettoken nm coln : coln
                local ++i
                if `IFtot'[1,`i']!=0 {
                    di as err "{bf:compact} not supported for statistics" ///
                        " that are not normalized by the sample size" ///
                        " (frequencies, totals)"
                    exit 499
                }
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
                if "`rif'"!="" lab var `CIF' "RIF of _b[`nm']"
                else           lab var `CIF' "IF of _b[`nm']"
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
                qui replace `IF' = `IF' * `W' ///
                    + (`b'[1,`i'] - `IFtot'[1,`i']) if `touse'
                lab var `IF' "RIF of _b[`nm']"
            }
        }
        else {
            local i 0
            foreach IF of local IFs {
                gettoken nm coln : coln
                local ++i
                if `IFtot'[1,`i']!=0 lab var `IF' "score of _b[`nm']"
                else                 lab var `IF' "IF of _b[`nm']"
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
    eret scalar k_eform = `k_eq'
    eret scalar k_omit = `k_omit'
    eret matrix _N = `_N'
    eret matrix _W = `_W'
    eret matrix nobs = `nobs'
    eret matrix sumw = `sumw'
    eret matrix omit = `omit'
    eret scalar qdef = `qdef'
    eret local novalues "`novalues'"
    eret local vformat "`vformat'"
    eret local nocasewise "`nocasewise'"
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
    eret local relax "`relax'"
    eret local zvar "`zvar'"
    if "`plvar'"!="" eret local pline "`plvar'"
    else             eret local pline "`pline'"
    eret local pstrong "`pstrong'"
    if "`over'"!="" {
        eret matrix id = `id'
        eret local total "`total'"
        eret local unconditional "`unconditional'"
        eret local over          "`over'"
        eret local over_select    "`over_select'"
        if "`over_contrast'"!="" {
            eret local over_contrast "`over_contrast'"
            eret local over_ratio    "`over_ratio'"
            eret matrix cref = `cref'
        }
        eret local over_accumulate "`over_accumulate'"
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
    if `hasdens' {
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
    if "`subcmd'"!="summarize" {
        eret matrix at = `AT'
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
    else if "`subcmd'"=="tip" {
        if "`absolute'"!="" local title "Absolute TIP curve"
        else                local title "TIP curve"
        // eret matrix hcr = `HCR'
        // eret matrix pgi = `PGI'
    }
    else if "`subcmd'"=="summarize" {
        eret local slist `"`slist'"'
        eret local stats "`stats'"
        eret scalar N_stats = `N_stats'
        if `N_stats'==1 & ("`over'"!="" | `N_vars'>1) local title "`stats'"
        else local title "Summary statistics"
    }
    if "`over_accumulate'`over_contrast'"!="" {
        if substr("`title'",2,1)==strlower(substr("`title'",2,1)) {
            // second character not uppercase
            if substr("`title'",1,6)!="Lorenz" {
                local title = strlower(substr("`title'",1,1)) + substr("`title'",2,.)
            }
        }
        if "`over_ratio'"=="lnratio"  local title "Ln(ratio) of `title'"
        else if "`over_ratio'"!=""    local title "Ratio of `title'"
        else if "`over_contrast'"!="" local title "Difference in `title'"
        else                          local title "Accumulated `title'"
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
    capt n syntax [varname(numeric default=none)] [, SELect(numlist int >=0) ///
        ACCUMulate CONTRast CONTRast2(str) ratio LNRatio ]
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:over()}"
        exit _rc
    }
    if "`lnratio'"!=""   local ratio lnratio
    if "`ratio'"!=""     local contrast contrast
    if "`contrast2'"!="" local contrast `"`contrast2'"'
    if `"`contrast'"'!="" {
        if "`accumulate'"!="" {
            di as err "{bf:contrast} and {bf:accumulate} not both allowed"
            di as err "error in option {bf:over()}"
            exit 198
        }
        if !inlist(`"`contrast'"',"contrast","lag","lead") {
            capt numlist `"`contrast'"', int max(1) range(>=0)
            if _rc==1 exit _rc
            if _rc {
                di as err "invalid argument in {bf:contrast()}"
                di as err "error in option {bf:over()}"
                exit 198
            }
        }
    }
    if "`varlist'"=="" {
        local select
        local accumulate
        local contrast
    }
    c_local over `varlist'
    c_local over_select `select'
    c_local over_accumulate `accumulate'
    c_local over_contrast `contrast'
    c_local over_ratio `ratio'
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
    syntax [anything(name=generate)] [, rif COMpact QUIetly ]
    if `"`generate'"'=="" {
        c_local generate
        exit
    }
    c_local generate_quietly `quietly'
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
    args subcmd cat n at range
    if `"`at'"'=="" exit
    if `"`n'"'!="" {
        di as err "{bf:n()} and {bf:at()} not both allowed"
        exit 198
    }
    if `"`range'"'!="" {
        di as err "{bf:range()} and {bf:at()} not both allowed"
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
    else if "`subcmd'"=="tip"       local args "numlist >=0 <=1"
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
        di as error "only one {it:rule} allowed in {bf:n()}"
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
    c_local plvar `"`pline'"'
    c_local pline ""
end

program Parse_sum
    // preprocess statistics
    gettoken zvar  0 : 0
    gettoken pline 0 : 0
    gettoken plvar 0 : 0
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
    c_local zvars: list zvars | plvars  // [sic!]
    c_local plvars `plvars'
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
    capt n syntax [, NOCOV COV ]
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
local Grp    class `GRP' scalar
local GRP0   dstat_grp0
local Grp0   struct `GRP0' scalar
local YTMP   dstat_ytmpres
local Ytmp   struct `YTMP' scalar
local ZTMP   dstat_ztmpres
local Ztmp   struct `ZTMP' scalar
local YZTMP  dstat_yztmpres
local YZtmp  struct `YZTMP' scalar
local BAL    dstat_bal
local Bal    struct `BAL' scalar
local ZVAR   dstat_parse_stats_zvars
local Zvar   struct `ZVAR' scalar
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
local IntM   real matrix
// boolean
local Bool   real scalar
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

struct `ZVAR' {
    `SS' zvar, plvar, pline
    `SR' zvars, plvars
}

void dstat_parse_stats(`Int' n)
{
    `Int'  i
    `SS'   nm
    `T'    A
    `SC'   list
    `Zvar' Z
    
    A = dstat_stats_lib()
    list = sort(asarray_keys(A), 1)
    Z.zvar = st_local("zvar")
    if (Z.zvar!="") Z.zvars = Z.zvar
    Z.pline = st_local("pline")
    Z.plvar = st_local("plvar")
    if (Z.plvar!="") Z.plvars = Z.plvar
    for (i=1;i<=n;i++) {
        nm = "stats_" + strofreal(i)
        st_local(nm, _dstat_parse_stats(A, list, Z,
            _dstat_parse_stats_tok(st_local(nm))))
    }
    st_local("zvars", invtokens(Z.zvars))
    st_local("plvars", invtokens(Z.plvars))
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

`SS' _dstat_parse_stats(`T' A, `SC' list, `Zvar' Z, `SR' stats)
{
    `Int' i, n
    `SS'  s0, s, o
    `PR'  P
    
    n = length(stats)
    for (i=1; i<=n; i++) {
        s0 = s = stats[i]
        o = _dstat_parse_stats_split(s) // replaces s
        P = asarray(A, _dstat_parse_stats_match(s, list, s0))
        stats[i] = (*P[2])(Z, s0, s, o, *P[3])
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
    // experimental
    // general
    asarray(A, "q"         , (`f'q(),         `p'1(),    &(0, 100, .)))         // quantile at #
    asarray(A, "p"         , (`f'p(),         `p'1(),    &(0, 100, .)))         // quantile at #
    asarray(A, "quantile"  , (`f'quantile(),  `p'1(),    &(0, 100, .)))         // quantile at #
    asarray(A, "hdquantile", (`f'hdquantile(),`p'1(),    &(0, 100, .)))         // Harrell&Davis (1982) quantile at #
    asarray(A, "d"         , (`f'd(),         `p'1(),    &(., ., .)))           // kernel density at #
    asarray(A, "density"   , (`f'density(),   `p'1(),    &(., ., .)))           // kernel density at #
    asarray(A, "hist"      , (`f'hist(),      `p'2(),    &(., ., .)))           // histogram density within [#,#]
    asarray(A, "cdf"       , (`f'cdf(),       `p'1(),    &(., ., .)))           // cdf at #
    asarray(A, "cdfm"      , (`f'cdfm(),      `p'1(),    &(., ., .)))           // mid cdf at #
    asarray(A, "cdff"      , (`f'cdff(),      `p'1(),    &(., ., .)))           // floor cdf at #
    asarray(A, "ccdf"      , (`f'ccdf(),      `p'1(),    &(., ., .)))           // complementary cdf at #
    asarray(A, "ccdfm"     , (`f'ccdfm(),     `p'1(),    &(., ., .)))           // mid ccdf at #
    asarray(A, "ccdff"     , (`f'ccdff(),     `p'1(),    &(., ., .)))           // floor ccdf at #
    asarray(A, "prop"      , (`f'prop(),      `p'1or2(), &(., ., .)))           // proportion equal to # or within [#,#]
    asarray(A, "pct"       , (`f'pct(),       `p'1or2(), &(., ., .)))           // percent equal to # or within [#,#]
    asarray(A, "f"         , (`f'f(),         `p'1or2(), &(., ., .)))           // frequency equal to # or within [#,#]
    asarray(A, "freq"      , (`f'freq(),      `p'1or2(), &(., ., .)))           // frequency equal to # or within [#,#]
    asarray(A, "total"     , (`f'total(),     `p'1or2(), &(., ., 0)))           // overall total or total equal to # or within [#,#]
    asarray(A, "min"       , (`f'min(),       `p'0(),    &.))                   //
    asarray(A, "max"       , (`f'max(),       `p'0(),    &.))                   //
    asarray(A, "range"     , (`f'range(),     `p'0(),    &.))                   //
    asarray(A, "midrange"  , (`f'midrange(),  `p'0(),    &.))                   // (max+min)/2
    // location
    asarray(A, "mean"      , (`f'mean(),      `p'0(),    &.))                   // arithmetic mean
    asarray(A, "gmean"     , (`f'gmean(),     `p'0(),    &.))                   // geometric mean
    asarray(A, "hmean"     , (`f'hmean(),     `p'0(),    &.))                   // harmonic mean
    asarray(A, "trim"      , (`f'trim(),      `p'1or2(), &(  0,  50, 25)))      // trimmed mean
    asarray(A, "winsor"    , (`f'winsor(),    `p'1or2(), &(  0,  50, 25)))      // winsorized mean
    asarray(A, "median"    , (`f'median(),    `p'0(),    &.))                   // median
    asarray(A, "huber"     , (`f'huber(),     `p'1(),    &(63.7, 99.9, 95)))    // huber m-estimate of location
    asarray(A, "biweight"  , (`f'biweight(),  `p'1(),    &(0.1, 99.9, 95)))     // biweight m-estimate of location
    asarray(A, "hl"        , (`f'hl(),        `p'0(),    &.))                   // Hodges-Lehmann estimator
    // scale
    asarray(A, "sd"        , (`f'sd(),        `p'1(),    &(., ., 1)))           // standard deviation
    asarray(A, "variance"  , (`f'variance(),  `p'1(),    &(., ., 1)))           // variance
    asarray(A, "mse"       , (`f'mse(),       `p'1or2(), &(., ., 0, 0)))        // mean squared error
    asarray(A, "smse"      , (`f'smse(),      `p'1or2(), &(., ., 0, 0)))        // square-root of MSE
    asarray(A, "iqr"       , (`f'iqr(),       `p'2(),    &(0, 100, 25, 75)))    // inter quantile range
    asarray(A, "iqrn"      , (`f'iqrn(),      `p'2(),    &(0, 100, 25, 75)))    // normalized inter quantile range
    asarray(A, "mad"       , (`f'mad(),       `p'1or2(), &(., ., 0, 0)))        // median (or mean) absolute deviation
    asarray(A, "madn"      , (`f'madn(),      `p'1or2(), &(., ., 0, 0)))        // normalized MAD
    asarray(A, "mae"       , (`f'mae(),       `p'1or2(), &(., ., 0, 0)))        // median (or mean) absolute error
    asarray(A, "maen"      , (`f'maen(),      `p'1or2(), &(., ., 0, 0)))        // normalized MAE
    asarray(A, "md"        , (`f'md(),        `p'0(),    &.))                   // mean absolute pairwise difference
    asarray(A, "mdn"       , (`f'mdn(),       `p'0(),    &.))                   // normalized mean absolute pairwise difference
    asarray(A, "mscale"    , (`f'mscale(),    `p'1(),    &(1, 50, 50)))         // m-estimate of scale
    asarray(A, "qn"        , (`f'qn(),        `p'0(),    &.))                   // Qn coefficient
    // skewness
    asarray(A, "skewness"  , (`f'skewness(),  `p'0(),    &.))                   // classical skewness
    asarray(A, "qskew"     , (`f'qskew(),     `p'1(),    &(0, 50, 25)))         // quantile skewness measure
    asarray(A, "mc"        , (`f'mc(),        `p'0(),    &.))                   // medcouple
    // kurtosis
    asarray(A, "kurtosis"  , (`f'kurtosis(),  `p'0(),    &.))                   // classical kurtosis
    asarray(A, "qw"        , (`f'qw(),        `p'1(),    &(0, 50, 25)))         // quantile tail weight
    asarray(A, "lqw"       , (`f'lqw(),       `p'1(),    &(0, 50, 25)))         // left quantile tail weight
    asarray(A, "rqw"       , (`f'rqw(),       `p'1(),    &(0, 50, 25)))         // right quantile tail weight
    asarray(A, "lmc"       , (`f'lmc(),       `p'0(),    &.))                   // left medcouple tail weight
    asarray(A, "rmc"       , (`f'rmc(),       `p'0(),    &.))                   // right medcouple tail weight
    // inequality
    asarray(A, "hoover"    , (`f'hoover(),    `p'0(),    &.))                   // Hoover index
    asarray(A, "gini"      , (`f'gini(),      `p'1(),    &(., ., 0)))           // Gini coefficient
    asarray(A, "agini"     , (`f'agini(),     `p'1(),    &(., ., 0)))           // absolute Gini coefficient
    asarray(A, "mld"       , (`f'mld(),       `p'0(),    &.))                   // mean log deviation
    asarray(A, "theil"     , (`f'theil(),     `p'0(),    &.))                   // Theil index
    asarray(A, "cv"        , (`f'cv(),        `p'1(),    &(., ., 1)))           // coefficient of variation
    asarray(A, "ge"        , (`f'ge(),        `p'1(),    &(., ., 1)))           // generalized entropy
    asarray(A, "atkinson"  , (`f'atkinson(),  `p'1(),    &(0, ., 1)))           // Atkinson inequality measure
    asarray(A, "lvar"      , (`f'lvar(),      `p'1(),    &(., ., 1)))           // logarithmic variance
    asarray(A, "vlog"      , (`f'vlog(),      `p'1(),    &(., ., 1)))           // variance of logarithm
    asarray(A, "top"       , (`f'top(),       `p'1(),    &(0, 100, 10)))        // top share
    asarray(A, "bottom"    , (`f'bottom(),    `p'1(),    &(0, 100, 40)))        // bottom share
    asarray(A, "mid"       , (`f'mid(),       `p'2(),    &(0, 100, 40, 90)))    // mid share
    asarray(A, "palma"     , (`f'palma(),     `p'0(),    &.))                   // palma ratio
    asarray(A, "qratio"    , (`f'qratio(),    `p'2(),    &(0, 100, 10, 90)))    // quantile ratio
    asarray(A, "sratio"    , (`f'sratio(),    `p'2or4(), &(0, 100, 0, 10, 90, 100))) // percentile share ratio
    asarray(A, "lorenz"    , (`f'lorenz(),    `p'1(),    &(0, 100, .)))         // lorenz ordinate
    asarray(A, "tlorenz"   , (`f'tlorenz(),   `p'1(),    &(0, 100, .)))         // total (sum) lorenz ordinate
    asarray(A, "glorenz"   , (`f'glorenz(),   `p'1(),    &(0, 100, .)))         // generalized lorenz ordinate
    asarray(A, "alorenz"   , (`f'alorenz(),   `p'1(),    &(0, 100, .)))         // absolute lorenz ordinate
    asarray(A, "elorenz"   , (`f'elorenz(),   `p'1(),    &(0, 100, .)))         // equality gap lorenz ordinate
    asarray(A, "share"     , (`f'share(),     `p'2(),    &(0, 100, ., .)))      // percentile share (proportion)
    asarray(A, "dshare"    , (`f'dshare(),    `p'2(),    &(0, 100, ., .)))      // percentile share (density)
    asarray(A, "tshare"    , (`f'tshare(),    `p'2(),    &(0, 100, ., .)))      // percentile share (total/sum)
    asarray(A, "gshare"    , (`f'gshare(),    `p'2(),    &(0, 100, ., .)))      // percentile share (generalized)
    asarray(A, "ashare"    , (`f'ashare(),    `p'2(),    &(0, 100, ., .)))      // percentile share (average)
    // concentration
    asarray(A, "gci"       , (`f'gci(),       `p'z1(),   &(., ., 0)))           // Gini concentration index
    asarray(A, "aci"       , (`f'aci(),       `p'z1(),   &(., ., 0)))           // absolute Gini concentration index
    asarray(A, "ccurve"    , (`f'ccurve(),    `p'1z(),   &(0, 100, .)))         // lorenz concentration ordinate
    asarray(A, "tccurve"   , (`f'tccurve(),   `p'1z(),   &(0, 100, .)))         // total concentration ordinate
    asarray(A, "gccurve"   , (`f'gccurve(),   `p'1z(),   &(0, 100, .)))         // generalized concentration ordinate
    asarray(A, "accurve"   , (`f'accurve(),   `p'1z(),   &(0, 100, .)))         // absolute concentration ordinate
    asarray(A, "eccurve"   , (`f'eccurve(),   `p'1z(),   &(0, 100, .)))         // equality gap concentration ordinate
    asarray(A, "cshare"    , (`f'cshare(),    `p'2z(),   &(0, 100, ., .)))      // concentration share
    asarray(A, "dcshare"   , (`f'dcshare(),   `p'2z(),   &(0, 100, ., .)))      // concentration share as density
    asarray(A, "gcshare"   , (`f'gcshare(),   `p'2z(),   &(0, 100, ., .)))      // generalized concentration share
    asarray(A, "tcshare"   , (`f'tcshare(),   `p'2z(),   &(0, 100, ., .)))      // concentration share as total
    asarray(A, "acshare"   , (`f'acshare(),   `p'2z(),   &(0, 100, ., .)))      // concentration share as average
    // poverty
    asarray(A, "hcr"       , (`f'hcr(),       `p'pl(),   &.))                   // Head count ratio
    asarray(A, "pgap"      , (`f'pgap(),      `p'pl(),   &.))                   // poverty gap
    asarray(A, "apgap"     , (`f'apgap(),     `p'pl(),   &.))                   // absolute poverty gap
    asarray(A, "pgi"       , (`f'pgi(),       `p'pl(),   &.))                   // poverty gap index
    asarray(A, "apgi"      , (`f'apgi(),      `p'pl(),   &.))                   // absolute poverty gap index
    asarray(A, "fgt"       , (`f'fgt(),       `p'1pl(),  &(0,   ., 0)))         // FGT poverty measure
    asarray(A, "chu"       , (`f'chu(),       `p'1pl(),  &(0, 100, 50)))        // Clark-Hemming-Ulph poverty measure
    asarray(A, "watts"     , (`f'watts(),     `p'pl(),   &.))                   // Watts index
    asarray(A, "sen"       , (`f'sen(),       `p'pl(),   &.))                   // Sen poverty index
    asarray(A, "sst"       , (`f'sst(),       `p'pl(),   &.))                   // Sen-Shorrocks-Thon poverty index
    asarray(A, "takayama"  , (`f'takayama(),  `p'pl(),   &.))                   // Takayama poverty index
    asarray(A, "tip"       , (`f'tip(),       `p'1pl(),  &(0, 100, .)))         // TIP ordinate
    asarray(A, "atip"      , (`f'atip(),      `p'1pl(),  &(0, 100, .)))         // absolute TIP ordinate
    // association
    asarray(A, "corr"      , (`f'corr(),      `p'z(),    &.))                   // correlation
    asarray(A, "cov"       , (`f'cov(),       `p'z1(),   &(., ., 1)))           // covariance
    asarray(A, "spearman"  , (`f'spearman(),  `p'z(),    &.))                   // spearman rank correlation
    asarray(A, "taua"      , (`f'taua(),      `p'z1(),   &(., ., 0)))           // Kendall's tau-a
    asarray(A, "taub"      , (`f'taub(),      `p'z1(),   &(., ., 0)))           // Kendall's tau-b
    asarray(A, "somersd"   , (`f'somersd(),   `p'z1(),   &(., ., 0)))           // Somers' D
    asarray(A, "gamma"     , (`f'gamma(),     `p'z1(),   &(., ., 0)))           // Goodman and Kruskal's gamma
    // categorical
    asarray(A, "hhi"       , (`f'hhi(),       `p'0(),    &.))                   // Herfindahl–Hirschman index
    asarray(A, "hhin"      , (`f'hhin(),      `p'0(),    &.))                   // normalized hhi
    asarray(A, "gimp"      , (`f'gimp(),      `p'0(),    &.))                   // Gini impurity
    asarray(A, "entropy"   , (`f'entropy(),   `p'1(),    &(0, ., 0)))           // Shannon entropy
    asarray(A, "hill"      , (`f'hill(),      `p'1(),    &(0, ., 1)))           // Hill number
    asarray(A, "renyi"     , (`f'renyi(),     `p'1(),    &(0, ., 1)))           // Rényi entropy
    asarray(A, "mindex"    , (`f'mindex(),    `p'z1(),   &(0, ., 0)))           // mutual information
    asarray(A, "uc"        , (`f'uc(),        `p'z1(),   &(0, ., 0)))           // uncertainty coefficient (symmetric)
    asarray(A, "ucl"       , (`f'ucl(),       `p'z1(),   &(0, ., 0)))           // uncertainty coefficient (left)
    asarray(A, "ucr"       , (`f'ucr(),       `p'z1(),   &(0, ., 0)))           // uncertainty coefficient (right)
    asarray(A, "cramer"    , (`f'cramer(),    `p'z1(),   &(0, ., 0)))           // Cramér's V
    return(A)
}

void _dstat_parse_error(`SS' s)
{
    printf("{err}'%s' invalid\n", s)
    exit(198)
}

`SS' _dstat_parse_stats_0(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name (without arguments)
    pragma unused O
    pragma unused Z
    
    if (o=="") return(s)
    _dstat_parse_error(s0)
}

`SS' _dstat_parse_stats_1(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name# or name(#)
    pragma unused Z
    if (__dstat_parse_stats_1(o, O[1], O[2], O[3])) return(s+o)
    _dstat_parse_error(s0)
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

`SS' _dstat_parse_stats_1or2(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name#, name(#), or name(#,#)
    pragma unused Z
    
    if (__dstat_parse_stats_1or2(o, O[1], O[2], O[|3\.|])) return(s+o)
    _dstat_parse_error(s0)
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

`SS' _dstat_parse_stats_2(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#,#)
    pragma unused Z
    
    if (__dstat_parse_stats_2(o, O[1], O[2], O[|3\.|])) return(s+o)
    _dstat_parse_error(s0)
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

`SS' _dstat_parse_stats_2or4(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#,#) or name(#,#,#,#)
    pragma unused Z
    
    if (__dstat_parse_stats_2or4(o, O[1], O[2], O[|3\.|])) return(s+o)
    _dstat_parse_error(s0)
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

`SS' _dstat_parse_stats_z(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name[(zvar)]
    pragma unused O
    
    if (__dstat_parse_stats_z(Z, o)) return(s+o)
    _dstat_parse_error(s0)
}

`Bool' __dstat_parse_stats_z(`Zvar' Z, `SS' o)
{   // replaces argument o
    `Int'  l
    `SR'   s
    `Bool' br, rc
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l==0)      rc = _dstat_parse_stats_get_z(Z, "")
    else if (l==1) rc = _dstat_parse_stats_get_z(Z, s)
    else           rc = 1
    if (rc)        return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(s, br)
    return(1)
}

`Bool' _dstat_parse_stats_get_z(`Zvar' Z, `SS' z0)
{
    `Int' j
    `SS'  z
    
    if (z0=="") z = Z.zvar
    else {
        j = _st_varindex(z0, st_global("c(varabbrev)")=="on")
        if (j>=.) {
            printf("{err}variable {bf:%s} not found\n", z0)
            return(1)
        }
        z = st_varname(j)
        if (!anyof(Z.zvars, z)) Z.zvars = (Z.zvars, z)
    }
    if (z=="") {
        printf("{err}{it:zvar} or {bf:zvar()} required\n")
        return(1)
    }
    return(0)
}

`SS' _dstat_parse_stats_z1(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(zvar), name(#), or name(zvar,#)
    if (__dstat_parse_stats_z1(Z, o, O[1], O[2], O[3])) return(s+o)
    _dstat_parse_error(s0)
}

`Bool' __dstat_parse_stats_z1(`Zvar' Z, `SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `SR'   s
    `RS'   o1
    `Bool' br, rc
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l==0) {
        rc = _dstat_parse_stats_get_z(Z, "")
        o1 = def
    }
    else if (l==1) {
        o1 = strtoreal(s)
        if (o1<.) rc = _dstat_parse_stats_get_z(Z, "")
        else {
            rc = _dstat_parse_stats_get_z(Z, s)
            o1 = def
        }
    }
    else if (l==2) {
        rc = _dstat_parse_stats_get_z(Z, s[1])
        o1 = strtoreal(s[2])
    }
    else rc = 1
    if (rc)    return(0)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_1z(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#) or name(#, zvar)
    if (__dstat_parse_stats_1z(Z, o, O[1], O[2], O[3])) return(s+o)
    _dstat_parse_error(s0)
}

`Bool' __dstat_parse_stats_1z(`Zvar' Z, `SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `RS'   o1
    `SR'   s
    `Bool' br, rc
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l<2) {
        if (l==0) o1 = def
        else      o1 = strtoreal(s)
        rc = _dstat_parse_stats_get_z(Z, "")
    }
    else if (l==2) {
        o1 = strtoreal(s[1])
        rc = _dstat_parse_stats_get_z(Z, s[2])
    }
    else rc = 1
    if (rc)    return(0)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_2z(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name(#, #) or name(#, #, zvar)
    if (__dstat_parse_stats_2z(Z, o, O[1], O[2], O[|3\.|])) return(s+o)
    _dstat_parse_error(s0)
}

`Bool' __dstat_parse_stats_2z(`Zvar' Z, `SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `RS'   o1, o2
    `SR'   s
    `Bool' br, rc
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l<3) {
        if (l<1) o1 = def[1]
        else     o1 = strtoreal(s[1])
        if (l<2) o2 = def[2]
        else     o2 = strtoreal(s[2])
        rc = _dstat_parse_stats_get_z(Z, "")
    }
    else if (l==3) {
        o1 = strtoreal(s[1])
        o2 = strtoreal(s[2])
        rc = _dstat_parse_stats_get_z(Z, s[3])
    }
    else rc = 1
    if (rc)    return(0)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (o2>=.) return(0)
    if ((min<. & o2<min) | (max<. & o2>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
    return(1)
}

`SS' _dstat_parse_stats_pl(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name or name(pline)
    pragma unused O
    
    if (__dstat_parse_stats_pl(Z, o)) return(s+o)
    _dstat_parse_error(s0)
}

`Bool' __dstat_parse_stats_pl(`Zvar' Z, `SS' o)
{   // replaces argument o
    `Int'  l
    `SR'   s
    `Bool' br, rc
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l==0) {
        rc = _dstat_parse_stats_get_pl(Z, "")
        o = ""
    }
    else if (l==1) {
        rc = _dstat_parse_stats_get_pl(Z, s)
        o = _dstat_parse_stats_brace(s, br)
    }
    else rc = 1
    if (rc) return(0)
    return(1)
}

`Bool' _dstat_parse_stats_get_pl(`Zvar' Z, `SS' pl0)
{
    `Int' j
    `SS'  pl
    
    if (pl0=="") pl = Z.pline!="" ? Z.pline : Z.plvar
    else {
        if (strtoreal(pl0)<.) {
            pl = pl0
            if (strtoreal(pl)<=0) {
                printf("{err}poverty line must be positive\n")
                return(1)
            }
        }
        else {
            j = _st_varindex(pl0, st_global("c(varabbrev)")=="on")
            if (j>=.) {
                printf("{err}variable {bf:%s} not found\n", pl0)
                return(1)
            }
            pl = st_varname(j)
            if (!anyof(Z.plvars, pl)) Z.plvars = (Z.plvars, pl)
        }
    }
    if (pl=="") {
        printf("{err}{it:pline} or {bf:pline()} required\n")
        return(1)
    }
    return(0)
}

`SS' _dstat_parse_stats_1pl(`Zvar' Z, `SS' s0, `SS' s, `SS' o, `RR' O)
{   // name, name(#), or name(#, pline)
    if (__dstat_parse_stats_1pl(Z, o, O[1], O[2], O[3])) return(s+o)
    _dstat_parse_error(s0)
}

`Bool' __dstat_parse_stats_1pl(`Zvar' Z, `SS' o, `RS' min, `RS' max, `RR' def)
{   // replaces argument o
    `Int'  l
    `RS'   o1
    `SR'   s
    `Bool' br, rc
    
    s = _dstat_parse_stats_args(o, br=`FALSE')
    l = length(s)
    if (l<2) {
        if (l==0) o1 = def
        else      o1 = strtoreal(s)
        rc = _dstat_parse_stats_get_pl(Z, "")
    }
    else if (l==2) {
        o1 = strtoreal(s[1])
        rc = _dstat_parse_stats_get_pl(Z, s[2])
    }
    else rc = 1
    if (rc)    return(0)
    if (o1>=.) return(0)
    if ((min<. & o1<min) | (max<. & o1>max)) return(0)
    if (l==0) o = ""
    else      o = _dstat_parse_stats_brace(invtokens(s,","), br)
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
    st_matrixcolstripe(st_local("b"), cstripe)
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
    st_matrixcolstripe(st_local("b"), cstripe)
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
            if (hasmissing(CI)) {
                _dstat_Get_CI_editif(CI, b, 0)
                _dstat_Get_CI_editif(CI, b, 1)
            }
        }
        else if (citype=="probit") { // probit
            z = z :* se :/ normalden(invnormal(b))
            CI = normal(invnormal(b) :- z \ invnormal(b) :+ z)
            if (hasmissing(CI)) {
                _dstat_Get_CI_editif(CI, b, 0)
                _dstat_Get_CI_editif(CI, b, 1)
            }
        }
        else if (citype=="atanh") { // atanh
            z = z :* se :/ (1 :- b:^2)
            CI = tanh(atanh(b) :- z \ atanh(b) :+ z)
            if (hasmissing(CI)) {
                _dstat_Get_CI_editif(CI, b, -1)
                _dstat_Get_CI_editif(CI, b,  1)
            }
        }
        else if (citype=="log") { // log
            z = z :* se :/ b
            CI = exp(ln(b) :- z \ ln(b) :+ z)
            if (hasmissing(CI)) _dstat_Get_CI_editif(CI, b, 0)
        }
        else exit(499)
        if (scale!=1) CI = CI * scale
    }
    // return result
    st_matrix(cimat, CI)
    st_matrixcolstripe(cimat, cstripe)
}

void _dstat_Get_CI_editif(`RM' CI, `RR' b, `RS' v)
{
    `Int' p
    
    p = selectindex(b:==v)
    if (length(p)) {
        CI[,p] = J(1, length(p), J(2,1,v))
    }
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
// helper function for removing coefficients from reference (contrast)
// --------------------------------------------------------------------------

void dstat_drop_cref(`SS' m, `Bool' r, `Bool' c)
{
    `IntR' p
    `RM'   M
    `SM'   rstripe, cstripe
    
    p = selectindex(st_matrix("e(cref)"):==0)
    M = st_matrix(m)
    rstripe = st_matrixrowstripe(m)
    cstripe = st_matrixcolstripe(m)
    if (r) rstripe = rstripe[p,]
    if (c) cstripe = cstripe[p,]
    if (r & c)  M = M[p, p]
    else if (r) M = M[p, .] 
    else if (c) M = M[., p]
    st_matrix(m, M)
    st_matrixrowstripe(m, rstripe)
    st_matrixcolstripe(m, cstripe)
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
// other helper functions
// --------------------------------------------------------------------------

`SS' dstat_unab(`SS' v)
{
    return(st_varname(st_varindex(v, st_global("c(varabbrev)")=="on")))
}

`RC' dstat_invp(`Int' N, `IntC' p, `RC' x, | `RS' x0)
{
    `RC' y
    
    y = J(N, 1, x0)
    y[p] = x
    return(y)
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

struct `YTMP' {
    `IntC'  p          // sort order of Y
    `RC'    Ys         // sorted Y
    `RC'    ws         // weights sorted by Y
    `RS'    mean       // mean of Y
    `RC'    Yc, Wc     // running sums for Lorenz ordinates
    `RS'    cd_K, cd_T // concordant/discordant pairs
    `RC'    cd_k, cd_t // concordant/discordant pairs
}

struct `ZTMP' {
    `Int'   z          // index of Z
    `RC'    Z          // values of Z
    `IntC'  p          // sort order of Z
    `RC'    Zs         // sorted values of Z
    `RS'    cd_U       // concordant/discordant pairs
    `RC'    cd_u       // concordant/discordant pairs
}

struct `YZTMP' {
    `RC'    Ys_z, Zs_y // Y (Z) sorted by order of Z (Y)
    `RC'    ws_z       // w sorted by oder of Z
    `RC'    Yc_z, Wc_z // running sums by order of Z for ccurve ordinates
    `RC'    EY, EYat   // E(Y|Z=q) for ccurve IFs
    `Bool'  cd_fast    // concordant/discordant pairs: type of algorithm
    `RS'    cd_S, cd_V // concordant/discordant pairs
    `RC'    cd_s, cd_v // concordant/discordant pairs
}

struct `GRP0' {
    `IntC'  p          // observations of current group
    `Int'   N          // number of obs
    `RC'    w          // total weight (including balancing)
    `RS'    W          // sum of weights
    `RC'    wb         // total weight / base weight
    `RC'    wc         // total weight - "pooled" component
}

class `GRP' {
    `Grp0'  G0         // backup overall group info if nocasewise
    `Int'   id         // value of group
    `IntC'  p          // observations of current group
    `IntC'  pp         // valid observation within current group (if nocw)
    `Int'   N          // number of obs
    `RC'    w          // total weight (including balancing)
    `Int'   wtype      // weights: 0 none, 1 fw, 2 iw, 3 pw
    `RS'    W          // sum of weights
    `RC'    wb         // total weight / base weight
    `RC'    wc         // total weight - "pooled" component
    `RM'    Z          // current group's balancing variables
    `RM'    IFZ        // current group's balancing IFs
    `IntC'  p1         // observations of reference group
    `RM'    IFZ1       // reference group's balancing IFs
    // current variable
    `Bool'  reset      // flag for whether sample requires resetting (dstat sum)
    `Int'   j          // counter of current variable
    `RC'    Y          // current variable
    void    reset()    // clear ytmp, ztmp, yztmp
    void    Zset()     // set Z variable and clear ztmp, yztmp
    `Int'   z()        // index of Z
    `RC'    Z()        // retrieve Z
    `IntC'  pY(), pZ() // sort order of Y or Z
    `RC'    Ys(), Zs() // sorted Y or Z
    `RC'    ws(), wsZ()  // w sorted by Y or by Z
    `RC'    YsZ(), ZsY() // Y sorted by Z, Z sorted by Y
    `RS'    mean()       // mean of Y
    `RC'    Yc(), Wc(), YcZ(), WcZ() // running sums for Lorenz ordinates
    `RC'    EY(), EYat() // E(Y|Z=q) for ccurve IFs
    void    cd_fast()    // set type of CD algorithm and reset results if needed
    `RS'    cd_K(), cd_S(), cd_T(), cd_U(), cd_V()
    `RC'    cd_k(), cd_s(), cd_t(), cd_u(), cd_v()
    
    private:
    `Ytmp'  ytmp       // struct holding temporary results related to Y
    `Ztmp'  ztmp       // struct holding temporary results related to Z
    `YZtmp' yztmp      // struct holding temporary results related to Y x Z
}

void `GRP'::reset()
{
    ytmp  = `YTMP'()
    ztmp  = `ZTMP'()
    yztmp = `YZTMP'()
}

void `GRP'::Zset(`Int' z0, `RC' Z0)
{
    if (ztmp.z==z0) return // still same z
    if (ztmp.z<.) {
        ztmp  = `ZTMP'()
        yztmp = `YZTMP'()
    }
    ztmp.z = z0
    ztmp.Z = Z0
}

`IntC' `GRP'::pY()
{
    if (rows(ytmp.p)) return(ytmp.p)
    ytmp.p = mm_order(Y,1,1) // stable sort
    return(ytmp.p)
}

`RC' `GRP'::Ys()
{
    if (rows(ytmp.Ys)) return(ytmp.Ys)
    ytmp.Ys = Y[pY()]
    return(ytmp.Ys)
}

`RC' `GRP'::ws()
{
    if (rows(ytmp.ws)) return(ytmp.ws)
    if (rows(w)==1) ytmp.ws = w
    else            ytmp.ws = w[pY()]
    return(ytmp.ws)
}

`Int' `GRP'::z() return(ztmp.z)

`RC' `GRP'::Z() return(ztmp.Z)

`IntC' `GRP'::pZ()
{
    if (rows(ztmp.p)) return(ztmp.p)
    ztmp.p = mm_order(Z(),1,1) // stable sort
    return(ztmp.p)
}

`RC' `GRP'::Zs()
{
    if (rows(ztmp.Zs)) return(ztmp.Zs)
    ztmp.Zs = Z()[pZ()]
    return(ztmp.Zs)
}

`RC' `GRP'::YsZ()
{
    if (rows(yztmp.Ys_z)) return(yztmp.Ys_z)
    yztmp.Ys_z = Y[pZ()]
    return(yztmp.Ys_z)
}

`RC' `GRP'::ZsY()
{
    if (rows(yztmp.Zs_y)) return(yztmp.Zs_y)
    yztmp.Zs_y = Z()[pY()]
    return(yztmp.Zs_y)
}

`RC' `GRP'::wsZ()
{
    if (rows(yztmp.ws_z)) return(yztmp.ws_z)
    if (rows(w)==1) yztmp.ws_z = w
    else            yztmp.ws_z = w[pZ()]
    return(yztmp.ws_z)
}

`RS' `GRP'::mean()
{
    if (ytmp.mean>=.) ytmp.mean = ::mean(Y, w)
    return(ytmp.mean)
}

`RC' `GRP'::Yc()
{   // running sum of (weighted) outcome at unique values; including an origin
    `RC' cdf
    
    if (rows(ytmp.Yc)) return(ytmp.Yc)
    cdf = _mm_ecdf2(Ys(), ws(), 0, 1)
    ytmp.Wc = 0 \ cdf[,2]
    ytmp.Yc = 0 \ quadrunningsum(cdf[,1] :* mm_diff(ytmp.Wc))
    return(ytmp.Yc)
}

`RC' `GRP'::Wc()
{
    if (rows(ytmp.Wc)) return(ytmp.Wc)
    (void) Yc()
    return(ytmp.Wc)
}

`RC' `GRP'::YcZ()
{   // running sum of (weighted) outcome at unique values; including an origin
    if (rows(yztmp.Yc_z)) return(yztmp.Yc_z)
    yztmp.Yc_z = 0 \ _mm_ecdf2(Zs(), wsZ():*YsZ(), 0, 1)[,2]
    return(yztmp.Yc_z)
}

`RC' `GRP'::WcZ()
{
    if (rows(yztmp.Wc_z)) return(yztmp.Wc_z)
    yztmp.Wc_z = 0 \ _mm_ecdf2(Zs(), wsZ(), 0, 1)[,2]
    return(yztmp.Wc_z)
}

`RC' `GRP'::EY()
{   // estimates E(Y|Z=q) using local linear regression on a 100-point grid
    // (using default lpoly settings for kernel and bandwidth); this will 
    // be used for the IFs of concentration curve ordinates
    `RS'   bw
    `IntC' p
    pragma unset bw

    if (rows(yztmp.EY)) return(yztmp.EY)
    if (wtype==1) {
        // first compute bandwidth using rounded fweights
        if (wsZ()!=ceil(wsZ())) {
            (void) _dstat_lpoly(YsZ(), Zs(), round(wsZ()), 0, 1, bw)
        }
    }
    // lpoly fit
    yztmp.EYat = rangen(min(Zs()), max(Zs()), 100)
    yztmp.EY   = _dstat_lpoly(YsZ(), Zs(), wsZ(), yztmp.EYat, wtype==1, bw)
    if (hasmissing(yztmp.EY)) { // restrict grid to nonmissing points
        p = selectindex(yztmp.EY:<.)
        assert(length(p)) // lpoly failed
        yztmp.EYat = yztmp.EYat[p]
        yztmp.EY   = yztmp.EY[p]
    }
    return(yztmp.EY)
}

`RC' `GRP'::EYat()
{
    if (rows(yztmp.EYat)) return(yztmp.EYat)
    (void) EY()
    return(yztmp.EYat)
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

void `GRP'::cd_fast(`RC' naive)
{
    `Bool' fast
    
    fast = (naive==0)
    if (fast==yztmp.cd_fast) return
    yztmp.cd_fast = fast
    yztmp.cd_S = .
    yztmp.cd_s = J(0,1,.)
}

`RS' `GRP'::cd_K()
{
    if (ytmp.cd_K<.) return(ytmp.cd_K)
    if (rows(w)==1)  ytmp.cd_K = N - 1
    else             ytmp.cd_K = quadsum(w :* cd_k()) / W
    return(ytmp.cd_K)
}

`RS' `GRP'::cd_T()
{
    if (ytmp.cd_T<.) return(ytmp.cd_T)
    if (rows(w)==1)  ytmp.cd_T = sum(cd_t()) / N
    else             ytmp.cd_T = quadsum(w :* cd_t()) / W
    return(ytmp.cd_T)
}

`RS' `GRP'::cd_U()
{
    if (ztmp.cd_U<.) return(ztmp.cd_U)
    if (rows(w)==1)  ztmp.cd_U = sum(cd_u()) / N
    else             ztmp.cd_U = quadsum(w :* cd_u()) / W
    return(ztmp.cd_U)
}

`RS' `GRP'::cd_S()
{
    if (yztmp.cd_S<.) return(yztmp.cd_S)
    if (rows(w)==1)   yztmp.cd_S = sum(cd_s()) / N
    else              yztmp.cd_S = quadsum(w :* cd_s()) / W
    return(yztmp.cd_S)
}

`RS' `GRP'::cd_V()
{
    if (yztmp.cd_V<.) return(yztmp.cd_V)
    if (rows(w)==1)   yztmp.cd_V = sum(cd_v()) / N
    else              yztmp.cd_V = quadsum(w :* cd_v()) / W
    return(yztmp.cd_V)
}

`RC' `GRP'::cd_k()
{
    if (rows(ytmp.cd_k)) return(ytmp.cd_k)
    if (rows(w)==1) ytmp.cd_k = J(N, 1, N-1)
    else            ytmp.cd_k = W :- w
    return(ytmp.cd_k)
}

`RC' `GRP'::cd_t()
{
    if (rows(ytmp.cd_t)) return(ytmp.cd_t)
    ytmp.cd_t = J(N,1,.)
    if (rows(w)==1) ytmp.cd_t[pY()] = __dstat_ifreq(Ys(), 1, N)  :- 1
    else            ytmp.cd_t[pY()] = __dstat_ifreq(Ys(), ws(), N) - ws()
    return(ytmp.cd_t)
}

`RC' `GRP'::cd_u()
{
    if (rows(ztmp.cd_u)) return(ztmp.cd_u)
    ztmp.cd_u = J(N,1,.)
    if (rows(w)==1) ztmp.cd_u[pZ()] = __dstat_ifreq(Zs(), 1, N)  :- 1
    else {
        ztmp.cd_u[pZ()] = __dstat_ifreq(Zs(), wsZ(), N)
        ztmp.cd_u = ztmp.cd_u - w
    }
    return(ztmp.cd_u)
}

`RC' `GRP'::cd_s()
{
    if (rows(yztmp.cd_s)) return(yztmp.cd_s)
    yztmp.cd_s = J(N,1,.)
    if (yztmp.cd_fast) {
        if (rows(w)==1) yztmp.cd_s[pY()] = _dstat_CD(Ys(), ZsY())
        else            yztmp.cd_s[pY()] = _dstat_CD(Ys(), ZsY(), ws())
    }
    else {
        if (rows(w)==1) yztmp.cd_s = _dstat_CD0(Y, Z(), N)
        else            yztmp.cd_s = _dstat_CD0_w(Y, Z, w, N)
    }
    return(yztmp.cd_s)
}

`RC' `GRP'::cd_v()
{
    if (rows(yztmp.cd_v)) return(yztmp.cd_v)
    yztmp.cd_v = J(N,1,.)
    if (rows(w)==1) yztmp.cd_v = _dstat_ifreq((Y, Z()), 1, N) :- 1
    else            yztmp.cd_v = _dstat_ifreq((Y, Z()), w, N) - w
    return(yztmp.cd_v)
}

`IntC' _dstat_CD0(`RC' X, `RC' Y, `Int' n)
{   // concordant-discordant sum; no weights; naive algorithm
    `Int'  i
    `IntC' s
    
    s = J(n,1,0)
    for (i=n;i;i--) s[i] = sum(sign((X:-X[i]) :* (Y:-Y[i])))
    return(s)
}

`IntC' _dstat_CD0_w(`RC' X, `RC' Y, `RC' w, `Int' n)
{   // concordant-discordant sum; weighted; naive algorithm
    `Int' i
    `RC'  s
    
    s = J(n,1,0)
    for (i=n;i;i--) s[i] = quadsum(w :* sign((X:-X[i]) :* (Y:-Y[i])))
    return(s)
}

`RC' _dstat_CD(`RC' x, `RC' y, | `RC' w)
{   // concordant-discordant sum; optionally weighted; fast algorithm
    // adaption of tidottree() by Roger Newson, 15 September 2018
    // data assumed sorted by x
    `Int'  n, nx, ny, i, ii, a, b, root
    `RC'   t, yval, wlt, weq, wgt
    `IntC' xpanel 
    `IntM' tree

    // setup
    n = rows(x)
    if (n<1) return(J(0,1,0))
    if (args()<3) w = J(n,1,1)
    xpanel = _mm_panels(x)
    nx     = rows(xpanel)
    yval   = mm_unique(y)
    ny     = rows(yval)
    tree   = _dstat_CD_btree(ny)
    root   = trunc((1 + ny) / 2)
    t      = J(n,1,0)
    
    // 1 -> nx
    wlt = weq = wgt = J(ny,1,0)
    b = 0
    for (i=1; i<=nx; i++) {
        a = b + 1
        b = b + xpanel[i]
        for (ii=a; ii<=b; ii++) t[ii] = quadsum((t[ii],
            _dstat_CD_l1(tree, root, y[ii], yval, wlt, weq, wgt)))
        for (ii=a; ii<=b; ii++)
            _dstat_CD_l2(tree, root, y[ii], yval, w[ii], wlt, weq, wgt)
    }
    
    // nx -> 1
    wlt = weq = wgt = J(ny,1,0)
    a = b + 1
    for (i=nx; i; i--) {
        b = a - 1
        a = a - xpanel[i]
        for (ii=b; ii>=a; ii--) t[ii] = quadsum((t[ii],
            -_dstat_CD_l1(tree, root, y[ii], yval, wlt, weq, wgt)))
        for (ii=b; ii>=a; ii--) 
            _dstat_CD_l2(tree, root, y[ii], yval, w[ii], wlt, weq, wgt)
    }
    return(t)
}

`RS' _dstat_CD_l1(`IntM' tree, `Int' root, `RS' yi, `RC' yval,
    `RC' wlt, `RC' weq, `RC' wgt)
{
    `Int' node
    `RS'  yj, l, g
    
    l = g = 0
    node = root
    while (node>0) {
        yj = yval[node]
        if (yi<yj) {
            g = quadsum((g, wgt[node], weq[node]))
            node = tree[node,1]
        }
        else if (yi>yj) {
            l = quadsum((l, wlt[node], weq[node]))
            node = tree[node,2]
        }
        else {
            g = quadsum((g, wgt[node]))
            l = quadsum((l, wlt[node]))
            node = 0
        }
    }
    return(l-g)
}

void _dstat_CD_l2(`IntM' tree, `Int' root, `RS' yi, `RC' yval, `RC' wi,
    `RC' wlt, `RC' weq, `RC' wgt)
{
    `Int' node
    `RS'  yj
    
    node = root
    while (node>0) {
        yj = yval[node]
        if (yi<yj) {
            wlt[node] = quadsum((wlt[node], wi))
            node = tree[node,1]
        }
        else if (yi>yj) {
            wgt[node] = quadsum((wgt[node], wi))
            node = tree[node,2]
        }
        else {
            weq[node] = quadsum((weq[node], wi))
            node = 0
        }
    }
}

`IntM' _dstat_CD_btree(`Int' n)
{   // adaption of blncdtree() by Roger Newson, 11 August 2005
    `IntM' tree
    
    tree = J(n, 2, 0)
    if (n<1) return(tree)
    __dstat_CD_btree(tree, 1, n)
    return(tree)
}

void __dstat_CD_btree(`IntM' tree, `Int' imin, `Int' imax)
{   // adaption of _blncdtree() by Roger Newson, 11 August 2005
    `Int' imid, inext

    imid = trunc((imin + imax) / 2)
    if (imid<=imin) { // left
         tree[imid,1] = 0
    }
    else {
      inext = imid - 1
      tree[imid,1] = trunc((imin + inext) / 2)
      __dstat_CD_btree(tree, imin, inext)
    }
    if (imid>=imax) {
      tree[imid, 1] = 0
    }
    else {
      inext = imid + 1
      tree[imid,2] = trunc((inext + imax) / 2)
      __dstat_CD_btree(tree, inext, imax)
    }
}

`RC' _dstat_ifreq(`RM' Y, `RC' w, `Int' n)
{   // returns for each row of Y the number (sum of weights) of duplicates;
    // Y not assumed sorted
    `IntC' p
    `RC'   h

    p = mm_order(Y, ., 1) // stable sort order
    h = J(n, 1, .)
    if (rows(w)==1) h[p] = __dstat_ifreq(Y[p,], w, n)
    else            h[p] = __dstat_ifreq(Y[p,], w[p], n)
    return(h)
}

`RC' __dstat_ifreq(`RM' Y, `RC' w, `Int' n)
{   // returns for each row of Y the number (sum of weights) of duplicates;
    // Y assumed sorted
    `Int' i, j
    `RC'  h

    h = J(n, 1, .)
    if (rows(w)==1) {
        j = 1
        for (i=2; i<=n; i++) {
            if (Y[i,]!=Y[i-1,]) {
                h[|j \ i-1|] = J(i-j, 1, i-j)
                j = i
            }
        }
        h[|j \ i-1|] = J(i-j, 1, i-j)
        if (w!=1) return(h*w)
        return(h)
    }
    j = 1
    for (i=2; i<=n; i++) {
        if (Y[i,]!=Y[i-1,]) {
            h[|j \ i-1|] = J(i-j, 1, quadsum(w[|j \ i-1|]))
            j = i
        }
    }
    h[|j \ i-1|] = J(i-j, 1, quadsum(w[|j \ i-1|]))
    return(h)
}

struct `DATA' {
    `Bool'  nose       // do not compute standard errors
    `Bool'  noIF       // do not compute influence functions
    `Bool'  nocw       // 1 nocasewise, 0 else
    `Int'   touse      // Stata variable marking sample
    `SR'    yvars      // names of outcome variables
    `RM'    Y          // view on outcome data
    `Int'   nvars      // number of outcome variables
    `Int'   N          // number of obs
    `SS'    wvar       // name of variable containing weights
    `RC'    w          // view on weights
    `RS'    W          // sum of weights
    `RR'    Wj         // sum of weights per variable (relevant if nocw & uncond)
    `PR'    mvj        // index of missing values per variable (relevant if nocw & uncond)
    `Int'   wtype      // weights: 0 none, 1 fw, 2 iw, 3 pw
    `SS'    ovar       // name of over variable
    `RC'    over       // view on over() variable
    `IntR'  overlevels // levels of over()
    `Int'   nover      // number of over groups (including total)
    `Bool'  total      // include total across over groups
    `IntR'  _N         // number of obs per group
    `RR'    _W         // sum of weights per group
    `Bal'   bal        // balancing settings
    `Bool'  accum      // accumulate results across subpopulations
    `Int'   contrast   // compute contrasts: .=no contrast, #=group, .a=total, .b=lag, .c=lead
    `Int'   ratio      // type of contrast: 0=difference, 1=ratio, 2=lnratio
    `RM'    IF         // view on influence functions
    `RC'    IFtot      // target value of sum(IF) (typically 0)
    `Int'   K          // length of results vector
    `Int'   k          // index of current group x variable
    `RC'    b          // vector estimates
    `BoolC' omit       // flags omitted estimates (summarize only)
    `BoolC' cref       // flag contrast reference
    `RC'    id         // ID of relevant subsample (group) for each estimate
    `IntR'  nobs       // number of obs per estimate
    `RR'    sumw       // sum of weights per estimate
    `SM'    cstripe    // column stripe for results
    `SR'    eqs        // equation names
    // command-specific settings
    `SS'    cmd        // subcommand
    `Bool'  novalues   // do not use values as coefficient names
    `SS'    vfmt       // display format for values used as coefficient names
    `Int'   qdef       // quantile definition
    `Int'   n          // number of evaluation points (if relevant)
    `Bool'  common     // common points across subpops (if relevant)
    `RM'    at         // vector of evaluation points (if relevant)
    `Bool'  hasdens    // whether density estimation has been employed
    `PDF'   S          // density estimation object (density, quantile, summarize)
    `Bool'  exact      // use exact density estimator (density, quantile, summarize)
    `RR'    bwidth     // container for bandwidth matrix (density, quantile, summarize)
    `PR'    AT         // pointer rowvector of sets of evaluation points
    `Bool'  ipolate    // interpolate ECDF (cdf/ccdf)
    `Bool'  mid        // midpoint adjustment (cdf/ccdf)
    `Bool'  floor      // use lower-than definition (cdf/ccdf)
    `Bool'  discr      // discrete (cdf/ccdf)
    `Bool'  cat        // categorical (prop)
    `Bool'  prop       // report proportions (histogram, share)
    `Bool'  pct        // report percent (histogram, proportion, cdf, ccdf, share, lorenz)
    `Bool'  uncond     // condition on total sample (density, histogram, proportion, cdf, ccdf)
    `Bool'  freq       // report frequencies (histogram, proportion, cdf, ccdf)
    `Bool'  ep         // use equal probability bins (histogram)
    `Bool'  gl         // generalized lorenz ordinates (lorenz, share)
    `Bool'  gap        // equality gap curve (lorenz)
    `Bool'  sum        // total (lorenz, share)
    `Bool'  abs        // absolute (lorenz)
    `Bool'  ave        // average (share)
    `Bool'  relax      // relax option (summarize)
    `SS'    zvar       // default auxiliary variable (summarize, lorenz, share)
    `SR'    zvars      // names of auxiliary variables (summarize, lorenz, share)
    `RM'    Z          // view on auxiliary variables (summarize, lorenz, share)
    `Bool'  pstrong    // use strong poverty definition (summarize, tip)
    `RS'    pline      // default poverty line (summarize, tip)
    `SS'    plvar      // default poverty line variable (summarize, tip)
    // `RR'    hcr, pgi   // containers to store HCR and PGI (tip)
}

void dstat()
{
    `Data' D
    `SC'   stats
    
    // settings
    D.nose     = st_local("nose")!=""
    D.noIF     = D.nose & (st_local("generate")=="")
    D.nocw     = st_local("nocasewise")!=""
    D.cmd      = st_local("subcmd")
    D.novalues = st_local("novalues")!=""
    D.vfmt     = st_local("vformat")
    D.qdef     = strtoreal(st_local("qdef"))
    D.common   = st_local("common")!=""
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
    D.relax    = st_local("relax")!=""
    D.pstrong  = st_local("pstrong")!=""
    
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
    if (D.nocw & D.uncond) {
        D.Wj = J(1, D.nvars, .)
        if (D.noIF==0) D.mvj = J(1, D.nvars, NULL)
    }
    st_numscalar(st_local("W"), D.W)
    D.zvar  = st_local("zvar")
    D.zvars = tokens(st_local("zvars"))
    if (length(D.zvars)) st_view(D.Z, ., D.zvars, D.touse)
    D.pline  = strtoreal(st_local("pline"))
    D.plvar  = st_local("plvar")
    
    // over
    D.ovar = st_local("over")
    if (D.ovar!="") {
        st_view(D.over, ., D.ovar, D.touse)
        D.overlevels = strtoreal(tokens(st_local("overlevels")))
        D.nover    = cols(D.overlevels)
        D.total    = st_local("total")!=""
        D.nover    = D.nover + D.total
        D.uncond   = (st_local("unconditional")!="")
        D.accum    = st_local("over_accumulate")!=""
        if (st_local("over_contrast")=="total")     D.contrast = .a
        else if (st_local("over_contrast")=="lag")  D.contrast = .b
        else if (st_local("over_contrast")=="lead") D.contrast = .c
        else D.contrast = strtoreal(st_local("over_contrast"))
        D.ratio    = (st_local("over_ratio")!="") + (st_local("over_ratio")=="lnratio")
    }
    else {
        D.nover    = 1
        D.total    = `FALSE'
        D.uncond   = 0
        D.accum    = `FALSE'
        D.ratio    = 0
    }
    D._N = D._W = J(1, D.nover, .)
    
    // balancing settings
    D.bal.method = st_local("bal_method")
    if (D.bal.method!="") {
        D.bal.zvars   = st_local("bal_varlist2")
        D.bal.ref     = strtoreal(st_local("bal_ref"))
        D.bal.noisily = st_local("bal_noisily")!=""
        D.bal.opts    = st_local("bal_opts")
        D.bal.ebopts  = strtoreal(tokens(st_local("bal_ebopts")))
        st_view(D.bal.wvar, .,  st_local("BAL_WVAR"), D.touse)
    }
    
    // density estimation setup
    D.hasdens = `FALSE' // will be reset to TRUE once density estimation is employed
    if (anyof(("density","quantile","summarize"), D.cmd)) {
        D.S.kernel(st_local("kernel"), strtoreal(st_local("adaptive")))
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
    
    // collect evaluation point/list of statistics
    dstat_get_at(D)
    
    // determine dimension of results vector and initialize containers
    dstat_set_K(D)
    
    // estimation
    if      (D.cmd=="density")    dstat_density(D)
    else if (D.cmd=="histogram")  dstat_hist(D)
    else if (D.cmd=="proportion") dstat_prop(D)
    else if (D.cmd=="cdf")        dstat_cdf(D, 0)
    else if (D.cmd=="ccdf")       dstat_cdf(D, 1)
    else if (D.cmd=="quantile")   dstat_quantile(D)
    else if (D.cmd=="lorenz")     dstat_lorenz(D)
    else if (D.cmd=="share")      dstat_share(D)
    else if (D.cmd=="tip")        dstat_tip(D)
    else if (D.cmd=="summarize")  dstat_sum(D)
    
    // compute contrasts
    if (D.contrast!=.) {
        if (D.ratio==2)   dstat_lnratio(D)
        else if (D.ratio) dstat_ratio(D)
        else              dstat_contrast(D)
    }
    // accumulate results across subpopulations
    if (D.accum) dstat_accum(D)
    // return results
    // - b, id, at, k_eq, omit, k_omit
    st_matrix(st_local("b"), D.b')
    st_matrix(st_local("nobs"), D.nobs')
    st_matrix(st_local("sumw"), D.sumw')
    st_matrix(st_local("id"), D.id')
    st_matrix(st_local("omit"), D.omit')
    st_local("k_omit", strofreal(sum(D.omit)))
    st_matrix(st_local("IFtot"), D.IFtot')
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
        //st_local("k_omit", strofreal(sum(dstat_put_omit(D.cstripe, D.omit))))
        st_local("k_eq", strofreal(mm_nunique(D.cstripe[,1])))
    }
    else {
        //st_local("k_omit", strofreal(sum(dstat_put_omit(D.cstripe, D.omit))))
        st_local("k_eq", strofreal(length(D.eqs)))
        st_matrix(st_local("AT"), D.at')
        st_matrixcolstripe(st_local("AT"), D.cstripe)
        if (D.cmd=="histogram" | D.cmd=="share") {
            st_matrixrowstripe(st_local("AT"), (J(3,1,""), ("ll", "mid", "h")'))
        }
    }
    st_matrixcolstripe(st_local("b"), D.cstripe)
    st_matrixcolstripe(st_local("nobs"), D.cstripe)
    st_matrixcolstripe(st_local("sumw"), D.cstripe)
    st_matrixcolstripe(st_local("id"), D.cstripe)
    st_matrixcolstripe(st_local("omit"), D.cstripe)
    if (D.contrast!=.) {
        st_matrix(st_local("cref"), D.cref')
        st_matrixcolstripe(st_local("cref"), D.cstripe)
    }
    // - density estimation
    if (D.hasdens) {
        st_local("hasdens", "1")
        st_local("kernel", D.S.kernel())
        dstat_store_eqvec(D, D.bwidth, st_local("BW"))
    }
    else st_local("hasdens", "0")
    // // HCR and PIG for dstat tip
    // if (D.cmd=="tip") {
    //     dstat_store_eqvec(D, D.hcr, st_local("HCR"))
    //     dstat_store_eqvec(D, D.pgi, st_local("PGI"))
    // }
    // - _N and _W
    D.eqs = strofreal(D.overlevels)
    if (D.total | length(D.eqs)==0) D.eqs = D.eqs, "total"
    st_matrix(st_local("_N"), D._N)
    st_matrixcolstripe(st_local("_N"), (J(cols(D.eqs),1,""), D.eqs'))
    st_matrix(st_local("_W"), D._W)
    st_matrixcolstripe(st_local("_W"), (J(cols(D.eqs),1,""), D.eqs'))
    
    // check IFs and compute VCE
    if (D.noIF) return
    if (hasmissing(D.IF)) {
        D.IF[.,.] = editmissing(D.IF, 0)
        display("{err}warning: influence function(s) contain missing;" + 
            " missing reset to zero")
    }
    dstat_recenter_IF(D)
    if (D.nose) return
    dstat_vce(D, st_local("clustvar"), st_local("vcenocov")!="")
}

void dstat_store_eqvec(`Data' D, `RR' vec, `SS' nm)
{
    `SM' cs
    
    cs = J(D.nover, 1, D.yvars')
    if (D.ovar=="") cs = (J(rows(cs), 1, ""), cs)
    else cs = (mm_expand((strofreal(D.overlevels), (D.total ? "total" : 
               J(1,0,"")))', D.nvars, 1, 1), cs)
    st_matrix(nm, vec)
    st_matrixcolstripe(nm, cs)
}

void dstat_accum(`Data' D)
{
    `Int' i, k, l, a, a0, b, b0
    
    k = D.nover
    l = D.K / k
    k = k - D.total
    a0 = 1; b0 = l
    for (i=2;i<=k;i++) {
        a = a0 + l
        b = b0 + l
        D.b[|a \ b|] = D.b[|a \ b|] + D.b[|a0 \ b0|]
        if (D.noIF==0) {
            D.IF[|1,a \ .,b|] = D.IF[|1,a \ .,b|] + D.IF[|1,a0 \ .,b0|]
            D.IFtot[|a \ b|] = D.IFtot[|a \ b|] + D.IFtot[|a0 \ b0|]
        }
        a0 = a; b0 = b
    }
}

void dstat_contrast(`Data' D)
{
    `Int'  i, k, l, r, a, a0, b, b0
    
    k = D.nover
    l = D.K / k
    if (D.contrast!=.b & D.contrast!=.c)  {
        if (D.contrast==.a) r = D.nover  // total
        else                r = selectindex(D.overlevels:==D.contrast)
        a0 = (r-1)*l + 1; b0 = r*l
    }
    for (i=1;i<=k;i++) {
        if (D.contrast==.b | D.contrast==.c) {
            if (i==k) break
            if (D.contrast==.b) { // lag
                a = (k-i)*l + 1
                b = a + l - 1
                a0 = a - l; b0 = a - 1
            }
            else { // lead
                a = (i-1)*l + 1
                b = a + l - 1
                a0 = b + 1; b0 = b + l
            }
        }
        else {
            if (i==r) continue
            a = (i-1)*l + 1
            b = a + l - 1
        }
        D.b[|a \ b|] = D.b[|a \ b|] - D.b[|a0 \ b0|]
        if (D.noIF==0) {
            D.IF[|1,a \ .,b|] = D.IF[|1,a \ .,b|] - D.IF[|1,a0 \ .,b0|]
            D.IFtot[|a \ b|] = D.IFtot[|a \ b|] - D.IFtot[|a0 \ b0|]
        }
    }
    D.cref = J(D.K, 1, 0)
    D.cref[|a0 \ b0|] = J(l, 1, 1)
}

void dstat_ratio(`Data' D)
{
    `Bool' hasmis
    `Int'  i, j, k, l, r, a, a0, b, b0
    `RC'   B0, B1, IFtot
    `RM'   IF0, IF1

    hasmis = 0
    k = D.nover
    l = D.K / k
    if (D.contrast!=.b & D.contrast!=.c)  {
        if (D.contrast==.a) r = D.nover  // total
        else                r = selectindex(D.overlevels:==D.contrast)
        a0 = (r-1)*l + 1; b0 = r*l
        a0 = (r-1)*l + 1; b0 = r*l
        B0 = D.b[|a0 \ b0|]
        if (D.noIF==0) {
            IF0 = D.IF[|1,a0 \ .,b0|]
            IFtot = D.IFtot[|a0 \ b0|]
            if (any(IFtot)) { // recenter IFs of unnormalized stats at zero
                IF0 = IF0 :- (IFtot'/D.W)
            }
        }
    }
    // take ratios
    for (i=1;i<=k;i++) {
        if (D.contrast==.b | D.contrast==.c) {
            if (i==k) break
            if (D.contrast==.b) { // lag
                a = (k-i)*l + 1
                b = a + l - 1
                a0 = a - l; b0 = a - 1
            }
            else { // lead
                a = (i-1)*l + 1
                b = a + l - 1
                a0 = b + 1; b0 = b + l
            }
            B0 = D.b[|a0 \ b0|]
            if (D.noIF==0) {
                IF0 = D.IF[|1,a0 \ .,b0|]
                IFtot = D.IFtot[|a0 \ b0|]
                if (any(IFtot)) { // recenter IFs of unnormalized stats at zero
                    IF0 = IF0 :- (IFtot'/D.W)
                }
            }
        }
        else {
            if (i==r) continue
            a = (i-1)*l + 1
            b = a + l - 1
        }
        B1 = D.b[|a \ b|]
        if (D.noIF==0) {
            IF1 = D.IF[|1,a \ .,b|]
            IFtot = D.IFtot[|a \ b|]
            if (any(IFtot)) { // recenter IFs of unnormalized stats at zero
                IF1 = IF1 :- (IFtot'/D.W)
                D.IFtot[|a \ b|] = J(b-a+1,1,0)
            }
            D.IF[|1,a \ .,b|] = (1:/B0)' :* IF1 - (B1:/(B0:^2))' :* IF0
        }
        D.b[|a \ b|] = B1 :/ B0
        if (hasmissing(D.b[|a \ b|])) {
            hasmis = 1
            for (j=a;j<=b;j++) {
                if (D.b[j]<.) continue
                D.b[j] = 0
                D.omit[j] = 1
                if (D.noIF==0) D.IF[.,j] = J(D.N, 1, 0)
            }
        }
    }
    D.cref = J(D.K, 1, 0)
    D.cref[|a0 \ b0|] = J(l, 1, 1)
    if (hasmis) {
        display("{txt}(missing estimates reset to zero)")
    }
}

void dstat_lnratio(`Data' D)
{
    `Bool' shift
    `Int'  j
    
    shift = `FALSE'
    if (D.noIF==0) {
        if (any(D.IFtot)) {
            // recenter IFs of unnormalized statistics at zero
            shift = `TRUE'
            D.IF = D.IF :- (D.IFtot'/D.W)
            D.IFtot = J(D.K, 1, 0)
        }
        D.IF = (1:/D.b)' :* D.IF
    }
    D.b = ln(D.b)
    dstat_contrast(D)
    if (shift) {
        // adjust IFs of unnormalized statistics in reference group
        for (j=1;j<=D.K;j++) {
            if (D.cref[j]==0) continue
            D.IFtot[j] = 1
            D.IF[,j] = D.IF[,j] :+ (1/D.W)
        }
    }
    if (hasmissing(D.b)) {
        for (j=1;j<=D.K;j++) {
            if (D.b[j]<.) continue
            D.b[j] = 0
            D.omit[j] = 1
            if (D.noIF==0) D.IF[.,j] = J(D.N, 1, 0)
        }
        display("{txt}(missing estimates reset to zero)")
    }
}

void dstat_vce(`Data' D, `SS' clust, `Bool' seonly)
{
    `Bool' dev
    `RS'   df_r, c, N_clust
    `RR'   m
    `RM'   V
    
    m = D.IFtot'
    dev = any(m) // use crossdev() if there are any score-type IFs
    // no clusters
    if (clust=="") {
        if (D.wtype==0) { // no weights
            if (seonly)   V = _dstat_vce_cross(1, D.IF, m/D.N)
            else if (dev) V = _dstat_vce_cdev(D.IF, m/D.N)
            else          V = cross(D.IF, D.IF)
            df_r = D.N - 1
            c = (df_r>0 & df_r<. ? D.N/df_r : 0)
        }
        else if (D.wtype==3) { // pw
            if (seonly)   V = _dstat_vce_cross2(D.w, D.IF, m/D.N)
            else if (dev) V = _dstat_vce_cdev(D.w:*D.IF, m/D.N)
            else          V = cross(D.IF, D.w:^2, D.IF)
            df_r = D.N - 1
            c = (df_r>0 & df_r<. ? D.N/df_r : 0)
        }
        else { // iw or fw
            if (seonly)   V = _dstat_vce_cross(D.w, D.IF, m/D.W)
            else if (dev) V = _dstat_vce_cdev(D.IF, m/D.W, D.w)
            else          V = cross(D.IF, D.w, D.IF)
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
        if (seonly)   V = _dstat_vce_cross(1, V, m/N_clust)
        else if (dev) V = _dstat_vce_cdev(V, m/N_clust)
        else          V = cross(V, V)
        df_r = N_clust - 1
        V = V * (df_r>0 & df_r<. ? N_clust/df_r : 0)
        st_local("N_clust", st_tempname())
        st_numscalar(st_local("N_clust"), N_clust)
    }
    // return results
    st_local("df_r", st_tempname())
    st_numscalar(st_local("df_r"), df_r)
    if (seonly) {
        st_local("SE", st_tempname())
        st_matrix(st_local("SE"), sqrt(V))
        st_matrixcolstripe(st_local("SE"), D.cstripe)
    }
    else {
        _makesymmetric(V)
        st_local("V", st_tempname())
        st_matrix(st_local("V"), V)
        st_matrixcolstripe(st_local("V"), D.cstripe)
        st_matrixrowstripe(st_local("V"), D.cstripe)
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
    
    if (rows(w)!=1) p = mm_order(C, 1, 1) // stable sort
    else            p = order(C, 1)
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
    `RR' r
    
    D.AT = NULL
    if      (D.cmd=="summarize")    _dstat_get_stats(D)
    else if (st_local("atmat")!="") _dstat_get_atmat(D, st_local("at"))
    else if (st_local("at")!="")    D.AT = &(strtoreal(tokens(st_local("at")))')
    else if (D.cmd=="proportion")   _dstat_get_levels(D)
    else if (D.cmd=="histogram")    _dstat_get_at_hist(D, st_local("n"))
    else if (D.cmd=="density") {
        D.n = strtoreal(st_local("n"))
        r   = strtoreal(tokens(st_local("range")))
        if (length(r)) D.AT = &(rangen(r[1], r[2], D.n))
        else if (D.common) _dstat_get_at_dens(D)
    }
    else if (D.cmd=="cdf" | D.cmd=="ccdf") {
        if (D.discr) _dstat_get_levels(D)
        else {
            D.n = strtoreal(st_local("n"))
            r   = strtoreal(tokens(st_local("range")))
            if (length(r)) D.AT = &(rangen(r[1], r[2], D.n))
            else if (D.common) _dstat_get_at_cdf(D)
        }
    }
    else if (D.cmd=="quantile") {
        D.n = strtoreal(st_local("n"))
        r   = strtoreal(tokens(st_local("range")))
        if (!length(r)) r = (0,1)
        D.AT = &(rangen(r[1]+(r[2]-r[1])/(D.n+1), r[2]-(r[2]-r[1])/(D.n+1), D.n))
    }
    else if (D.cmd=="lorenz" | D.cmd=="tip") {
        D.n = strtoreal(st_local("n"))
        r   = strtoreal(tokens(st_local("range")))
        if (!length(r)) r = (0,1)
        D.AT = &(rangen(r[1], r[2], D.n))
    }
    else if (D.cmd=="share") {
        D.n  = strtoreal(st_local("n"))
        D.AT = &((0::D.n) / D.n)
        D.n  = D.n + 1 // one additional point for upper limit
    }
    D.AT = J(1, ceil(D.nvars*D.nover/cols(D.AT)), D.AT)[|1 \ D.nvars*D.nover|]
}

void _dstat_get_atmat(`Data' D, `SS' matnm)
{   // splits vector by equations
    `Int'  i
    `RR'   M
    `SR'   eqs, coefs
    `IntR' from, to
    
    M = st_matrix(matnm)[1,.]
    if (D.cat) { // proportion
        assert(M==trunc(M)) // integer
        assert(all(M:>=0))  // positive
    }
    else if (anyof(("quantile", "lorenz", "share", "tip"), D.cmd)) {
        assert(all(M:>=0 :& M:<=1))  // within [0,1]
    }
    eqs = st_matrixcolstripe(matnm)[.,1]'
    if (D.cmd=="proportion" & D.cat & !D.novalues) {
        // group:#.variable => group~variable:#
        coefs = st_matrixcolstripe(matnm)[.,2]'
        eqs = eqs :+ "~" :+  substr(coefs, strrpos(coefs, "."):+1, .)
    }
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

void _dstat_get_stats(`Data' D)
{
    `Int' j
    `SS'  s
    `T'   t
    
    t = tokeninit(" ", "", "()")
    tokenset(t, st_local("slist"))
    D.AT = J(1, D.nvars, NULL)
    j = 0
    while ((s = tokenget(t)) != "") {
        D.AT[++j] = &(tokens(substr(s,2,strlen(s)-2))')
        s = tokenget(t)
        assert(s==D.yvars[j])
    }
}

void _dstat_get_levels(`Data' D)
{   // obtains levels of each Y across full sample
    `Int' j

    D.AT = J(1, D.nvars, NULL)
    for (j=1;j<=D.nvars;j++) {
        if (D.nocw) {
            D.AT[j] = &(mm_unique(select(D.Y[,j], D.Y[,j]:<.)))
            if (length(*D.AT[j])==0) { // no observations
                if (D.cmd=="proportion") D.AT[j] = &0
                else                     D.AT[j] = &.z
            }
        }
        else D.AT[j] = &(mm_unique(D.Y[,j])) 
        //D.AT[j] = &(__dstat_get_levels(D.Y[,j]))
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
`RC' __dstat_get_levels(`RC' Y)
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

void _dstat_get_at_dens(`Data' D)
{   // generate evaluation grid based on full sample
    `Int' j
    `RS'  tau
    `RR'  bw, range, minmax
    `RC'  Y
    
    // if lb() and ub() specified: no bandwidth required
    if (D.S.lb()<. & D.S.ub()<.) {
        D.AT = &(rangen(D.S.lb(), D.S.ub(), D.n))
        return
    }
    // obtain bandwidth if specified
    j = cols(D.overlevels) * D.nvars
    if (D.total) bw = D.bwidth[|j+1 \ .|]
    else if (st_local("bwidth")!="") {
        // reread because original specification may include bandwidth for total
        if (st_local("bwmat")!="") bw = st_matrix(st_local("bwidth"))[1,]
        else bw = strtoreal(tokens(st_local("bwidth")))
        bw = J(1, ceil((j+D.nvars)/cols(bw)), bw)[|j+1 \ j+D.nvars|]
    }
    else bw = J(1, D.nvars, .)
    // determine grid for each variable
    D.AT = J(1, D.nvars, NULL)
    for (j=1;j<=D.nvars;j++) {
        if (D.nocw) {
            Y = select(D.Y[,j], D.Y[,j]:<.)
            if (length(Y)==0) { // no observations
                D.AT[j] = &(J(D.n, 1, .z))
                continue
            }
        }
        else Y = D.Y[,j]
        if (bw[j]>=.) {
            D.S.data(Y, D.w, D.wtype>=2, 0)
            bw[j] = D.S.h()
            if (bw[j]>=.) bw[j] = epsilon(1)
        }
        // mimic grid definition from mm_density()
        range = (D.S.lb(), D.S.ub())
        minmax = minmax(Y)
        tau = mm_diff(minmax) * D.S.pad()
        tau = min((tau, bw[j] * (D.S.kernel()=="epanechnikov" ? sqrt(5) : 
            (D.S.kernel()=="cosine" ? .5 : (D.S.kernel()=="gaussian" ? 3 : 1)))))
        if (range[1]>=.) range[1] = minmax[1] - tau
        if (range[2]>=.) range[2] = minmax[2] + tau
        D.AT[j] = &(rangen(range[1], range[2], D.n)) 
    }
    if (D.total) { // store bandwidth to avoid double work
        D.bwidth[|cols(D.overlevels)*D.nvars + 1 \ .|] = bw
    }
}

void _dstat_get_at_cdf(`Data' D)
{   // generate evaluation grid based on full sample
    `Int' j

    D.AT = J(1, D.nvars, NULL)
    for (j=1;j<=D.nvars;j++) {
        if (D.nocw) D.AT[j] = &(dstat_cdf_AT(select(D.Y[,j], D.Y[,j]:<.), D.n))
        else        D.AT[j] = &(dstat_cdf_AT(D.Y[,j], D.n))
    }
}

void _dstat_get_at_hist(`Data' D, `SS' rule)
{
    `Int' i, I, j, n
    `RS'  N, h
    `RR'  minmax
    `Grp' G
    
    if (rule=="")          rule = "sqrt"
    if (strtoreal(rule)<.) D.n = strtoreal(rule) + 1
    if (D.common) {
        I = i = cols(D.overlevels) + 1 // total only
        D.AT = J(1, D.nvars, NULL)
    }
    else {
        i = 1
        I = D.nover
        D.AT = J(1, D.nvars*D.nover, NULL)
    }
    D.k = 0
    for (; i<=I; i++) {
        G = _dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            if (G.N==0) { // no observations
                if (D.n<.) D.AT[D.k] = &(J(D.n,1,.z))
                else       D.AT[D.k] = &(J(2,1,.z))
                continue
            }
            if (D.wtype==1) N = G.W
            else            N = G.N
            minmax = minmax(G.Y)
            if (D.n>=.) {
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
                    h = 3.5 * sqrt(_dstat_variance(G.Y, G.w, N)) / N^(1/3)
                    n = ceil(mm_diff(minmax)/h) - 1
                }
                else if (rule=="fd") {
                    h = 2 * mm_iqrange(G.Y, G.w) / N^(1/3)
                    n = ceil(mm_diff(minmax)/h) - 1
                }
                else if (rule=="ep") {
                    n = ceil(2 * N^(2/5))
                }
                else exit(499)
                if (n<1) n = 1 // make sure that never zero
            }
            else n = D.n - 1
            if (D.ep==0) D.AT[D.k] = &(rangen(minmax[1], minmax[2], n+1))
            else         D.AT[D.k] = &(mm_quantile(G.Y, G.w, (0::n)/n, 1)) // qdef=1
        }
    }
}

void dstat_set_K(`Data' D)
{
    `Int' i, j, k
    
    // determine K
    if (D.n<.) D.K = D.nover * D.nvars * D.n
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
    D.b = D.id = D.nobs = D.sumw = J(D.K, 1, .)
    D.omit = J(D.K, 1, 0)
    D.IFtot = J(D.K, 1, 0)
    if (D.cmd!="summarize") {
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

void dstat_set_IF(`Data' D, `Grp' G, `Int' k, `RC' h, | `RS' h0, `RS' offset)
{
    D.IF[,k] = _dstat_set_IF(D, G, h, h0, (D.IFtot[k] ? offset : 0))
}

`RC' _dstat_set_IF(`Data' D, `Grp' G, `RC' h, `RS' h0, `RS' offset)
{
    `RR'   delta
    `RC'   IF
    
    if (D.uncond) {
        // h0 is the value of the IF for observations outside the current
        // subpop (the IF is initialized with h0 for all observations, but will
        // be replaced later on for observations within the current subpop)
        IF = J(D.N, 1, h0)
        if (D.nocw) {
            // IF must be set to zero for missing observations
            if (length(dstat_mvj(D, G))) {
                IF[dstat_mvj(D, G)] = IF[dstat_mvj(D, G)] :* 0
            }
        }
    }
    else IF = J(D.N, 1, 0)
    if (cols(G.Z)) { // has balancing
        if (rows(G.pp)) {
            // Y has missings
            delta = quadcolsum(G.wc :* h :* G.Z[G.pp,])'
            IF[G.p] = G.wb :* h
            IF[G.G0.p] = IF[G.G0.p] + G.IFZ * delta
        }
        else {
            delta = quadcolsum(G.wc :* h :* G.Z)'
            IF[G.p] = G.wb :* h + G.IFZ  * delta
        }
        IF[G.p1] = IF[G.p1] + G.IFZ1 * delta
    }
    else        IF[G.p] = h
    if (offset) IF[G.p] = IF[G.p] :+ offset
    return(IF)
}

void dstat_recenter_IF(`Data' D)
{    // issue warning and recenter if relative error is larger than 1e-6
    `RS'  mrd, tol
    `RR'  m
    
    tol = 1e-6
    m   = quadcross(D.w, D.IF) - D.IFtot'
    mrd = mreldif(m+D.b', D.b')
    if (mrd<=tol) return
    display("{err}warning: total of influence function(s) deviates from zero")
     printf("{err}         maximum relative error = %g\n", mrd)
    D.IF[.,.] = D.IF :- m/D.W
    display("{err}         influence function(s) recentered at zero")
}

void dstat_set_cstripe(`Data' D, `Grp' G, `Int' i, `Int' j, `Int' a, `Int' b, `SC' coefs)
{
    if (!(D.cat&!D.novalues) & D.nvars>1) {
        if (D.ovar=="") D.eqs[D.k] = D.yvars[j]
        else if (i>cols(D.overlevels)) D.eqs[D.k] = "total~" + D.yvars[j]
        else D.eqs[D.k] = strofreal(D.overlevels[i]) + "~" + D.yvars[j]
    }
    else if (D.ovar!="") {
        if (i>cols(D.overlevels)) D.eqs[D.k] = "total"
        else D.eqs[D.k] = strofreal(D.overlevels[i])
    }
    else D.eqs[D.k] = "_"
    D.cstripe[|a,1 \ b,1|] = J(b-a+1, 1, D.eqs[D.k])
    if (D.cat&!D.novalues) D.cstripe[|a,2 \ b,2|] = coefs :+ ("."+D.yvars[j])
    else                   D.cstripe[|a,2 \ b,2|] = coefs
    D.id[|a \ b|] = J(b-a+1, 1, G.id)
}

`Grp' dstat_init_grp(`Data' D, `Int' i)
{
    `Grp' G

    // group data
    G = _dstat_init_grp(D, i)
    D._N[i] = G.N
    D._W[i] = G.W
    // balancing
    if (D.bal.method=="") return(G)
    if (D.bal.ref==G.id) return(G) // do not balance reference group
    if (D.bal.ref<.) G.p1 = selectindex(D.over:==D.bal.ref)
    else             G.p1 = selectindex(D.over:!=G.id)
    if (D.bal.noisily) printf("{txt}\n==> balance: {it:groupvar} = %g\n", G.id)
    if      (D.bal.method=="ipw") dstat_balance_ipw(D, D.bal, G)
    else if (D.bal.method=="eb")  dstat_balance_eb(D, D.bal, G)
    else exit(499) // cannot be reached
    dstat_balance_rescale(D, G) // update weights
    if (length(D.bal.wvar)) D.bal.wvar[G.p] = G.w
    if (D.nocw | D.relax) {
        G.G0.w  = G.w
        G.G0.wb = G.wb
        G.G0.wc = G.wc
    }
    return(G)
}

`Grp' _dstat_init_grp(`Data' D, `Int' i)
{
    `Grp' G
    
    G.reset = 0
    G.wtype = D.wtype
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
        if (rows(D.w)==1) {; G.w = D.w;      G.W = D.w*G.N;      }
        else              {; G.w = D.w[G.p]; G.W = quadsum(G.w); }
    }
    if (D.nocw | D.relax) {
        G.G0.p = G.p
        G.G0.N = G.N
        G.G0.w = G.w
        G.G0.W = G.W
    }
    return(G)
}

void dstat_init_Y(`Data' D, `Grp' G, `Int' j, | `Int' zj)
{
    `Bool' hasmis
    `RC'   y, z, touse
    
    G.j = j
    D.k = D.k + 1
    if (D.nocw) {
        y = D.Y[G.G0.p,G.j]
        hasmis = hasmissing(y)
        if (zj<.) {
            z = D.Z[G.G0.p,zj]
            hasmis = hasmis + hasmissing(z)
        }
        if (hasmis) {
            if (zj<.) touse = (y:<. :& z:<.)
            else      touse = (y:<.)
        }
    }
    else hasmis = 0
    _dstat_init_Y(D, G, D.nocw | G.reset, hasmis, touse)
    G.reset = 0
}

void _dstat_init_Y(`Data' D, `Grp' G, `Bool' reset, `Bool' hasmis, `BoolC' touse)
{
    if (reset) __dstat_init_Y(D, G, hasmis, touse) // update index and weights
    G.Y = D.Y[G.p, G.j]
    G.reset()
    D.S.data(J(0,1,.)) // clear density object
}

void __dstat_init_Y(`Data' D, `Grp' G, `Bool' hasmis, `BoolC' touse)
{
    `RS' c
    
    if (hasmis) {
        G.pp = selectindex(touse)
        if (G.G0.p==.) G.p = G.pp
        else           G.p = G.G0.p[G.pp]
        G.N = rows(G.p)
        if (rows(D.w)==1)    G.W = D.w*G.N
        else                 G.W = quadsum(D.w[G.p]) // raw sum of weights
        if (rows(G.G0.w)==1) G.w = G.G0.w
        else                 G.w = G.G0.w[G.pp]
        if (cols(G.Z)==0) return // exit if no balancing
        // renormalize weights; alternative would be to set c = 1 and
        // G.W = quadsum(G.w); SEs would be the same, but normalizing to the
        // raw sum of weights within the subsample is used here so that
        // overall W of nonmissing Y across subpops is not affected by
        // the balancing
        c = G.W / quadsum(G.w)
        G.w = G.w * c
        G.wb = G.G0.wb[G.pp] * c
        if (D.noIF==0) G.wc = G.G0.wc[G.pp] * c
        return
    }
    // no missings (full sample)
    G.pp = J(0,1,.)
    G.p = G.G0.p; G.N = G.G0.N; G.w = G.G0.w; G.W = G.G0.W
    G.wb = G.G0.wb; G.wc = G.G0.wc
}

void dstat_noobs(`Data' D, `Int' a, `Int' b)
{
    `Int' n
    
    n = b - a + 1
    D.b[|a \ b|] = J(n, 1, 0)
    D.omit[|a \ b|] = J(n, 1, 1)
    if (D.noIF) return
    D.IF[|1,a \ .,b|] = J(D.N, n, 0)
}

void dstat_set_density(`Data' D, `Grp' G, | `Bool' nosort)
{
    if (args()<3) nosort = 0
    if (rows(D.S.X())) return // dstat_set_density() already applied
    if (nosort) D.S.data(G.Y   , G.w   , D.wtype>=2, 0) // use unsorted data
    else        D.S.data(G.Ys(), G.ws(), D.wtype>=2, 1) // sorted data
    if (D.bwidth[D.k]<.) D.S.bw(D.bwidth[D.k])
    if (D.S.h()>=.)      D.S.bw(epsilon(1))  // missing bandwidth
    D.bwidth[D.k] = D.S.h()
}

void dstat_set_nobs_sumw(`Data' D, `Grp' G, `Int' a, | `Int' b)
{
    if (args()<4) {
        D.nobs[a] = G.N
        D.sumw[a] = G.W
        return
    }
    D.nobs[|a \ b|] = J(b-a+1, 1, G.N)
    D.sumw[|a \ b|] = J(b-a+1, 1, G.W)
}

`RS' dstat_Wj(`Data' D, `Grp' G)
{
    if (D.nocw==0) return(D.W)
    if (D.Wj[G.j]<.) return(D.Wj[G.j]) // already computed
    if (D.wtype) D.Wj[G.j] = quadsum(select(D.w, D.Y[,G.j]:<.))
    else         D.Wj[G.j] = sum(D.Y[,G.j]:<.)
    return(D.Wj[G.j])
}

`RC' dstat_mvj(`Data' D, `Grp' G)
{
    if (D.mvj[G.j]==NULL) {
        D.mvj[G.j] = &selectindex(D.Y[,G.j]:>=.)
    }
    return(*D.mvj[G.j])
}

`RM' dstat_freq(`Grp' G)
{
    if (G.N==0) return(J(G.N, 2, .))
    return(_dstat_freq(G.Ys(), G.ws(), G.N))
}

`RM' _dstat_freq(`RC' Y, `RC' w, `Int' n)
{   // return frequency table of Y (single variable); Y assumed sorted
    `Int' i, j, i0
    `RS'  y0
    `RM'  H
    
    H = J(n, 2, .)
    j = i0 = 1
    y0 = Y[i0]
    if (rows(w)==1) {
        for (i=2;i<=n; i++) {
            if (Y[i]!=y0) {
                H[j++,] = (y0, (i-i0)*w)
                i0 = i
                y0 = Y[i0]
            }
        }
        H[j,] = (y0, (i-i0)*w)
    }
    else {
        for (i=2;i<=n; i++) {
            if (Y[i]!=y0) {
                H[j++,] = (y0, quadsum(w[|i0\i-1|]))
                i0 = i
                y0 = Y[i0]
            }
        }
        H[j,] = (y0, quadsum(w[|i0\i-1|]))
    }
    return(H[|1,1 \ j,2|])
}

void _dstat_freq2(`RM' Y, `RC' w, `Int' n) // Y will be replaced
{   // return frequency table of Y (multiple variables); Y not assumed sorted
    `Int'  i, j, i0, k
    `IntR' kk
    `RR'   y0
    
    k = cols(Y); kk = 1..k
    j = i0 = 1
    if (rows(w)==1) {
        Y = mm_sort(Y, kk, 1), J(n,1,.) // stable sort oder
        y0 = Y[i0,kk]
        for (i=2;i<=n; i++) {
            if (Y[i,kk]!=y0) {
                Y[j++,] = (y0, (i-i0)*w)
                i0 = i
                y0 = Y[i0,kk]
            }
        }
        Y[j,] = (y0, (i-i0)*w)
    }
    else {
        Y = mm_sort((Y,w), kk, 1) // stable sort oder
        y0 = Y[i0,kk]
        for (i=2;i<=n; i++) {
            if (Y[i,kk]!=y0) {
                Y[j++,] = (y0, quadsum(Y[|i0,k+1 \ i-1,.|]))
                i0 = i
                y0 = Y[i0,kk]
            }
        }
        Y[j,] = (y0, quadsum(Y[|i0,k+1 \ i-1,.|]))
    }
    Y = Y[|1,1 \ j,.|]
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
    `RC'   AT
    `Grp'  G
    
    D.hasdens = `TRUE'
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            if (G.N) dstat_set_density(D, G, 1)
            a = b + 1
            if (D.AT[D.k]==NULL) {
                b = a + D.n - 1
                if (G.N) {
                    D.b[|a \ b|] = D.S.d(D.n, ., ., D.exact)
                    AT = D.S.at()
                }
                else {
                    if (D.S.lb()<. & D.S.ub()<.) {
                        AT = rangen(D.S.lb(), D.S.ub(), D.n)
                    }
                    else AT = J(D.n, 1, .z)
                }
            }
            else {
                AT = *D.AT[D.k]
                b = a + rows(AT) - 1
                if (G.N) D.b[|a \ b|] = D.S.d(AT, D.exact)
            }
            D.at[|a \ b|] = AT
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "d":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) dstat_noobs(D, a, b)
            else {
                if (D.uncond) D.b[|a \ b|] = D.b[|a \ b|] * (G.W/dstat_Wj(D, G))
                if (D.noIF==0) dstat_density_IF(D, G, a, b)
            }
        }
    }
}

void dstat_density_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i
    `RS'  W, B
    `RC'  h, z
    
    W  = (D.uncond ? dstat_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b|] = D.b[|a \ b|]
    h  = D.S.h() * D.S.l()
    for (i=a; i<=b; i++) {
        z = D.S.K(D.S.X(), D.at[i], h)
        if (D.exact) B = D.b[i]
        else         B = quadcross(G.w, z) / W
        dstat_set_IF(D, G, i, (z :- B)/W, -B/W, D.b[i]/W)
    }
}

// --------------------------------------------------------------------------
// dstat histogram
// --------------------------------------------------------------------------

void dstat_hist(`Data' D)
{
    `Int'  i, j, a, b
    `RC'   AT, B, h, c
    `RS'   min
    `Grp'  G
    
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            AT = *D.AT[D.k]
            h = mm_diff(AT) // bin width
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a,1 \ b,.|] = AT, AT + (h/2 \ .), (h \ .)
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "h":+strofreal(1::b-a)\"_ul" : strofreal(AT, D.vfmt)))
            if (G.N==0) {
                dstat_noobs(D, a, b)
                continue
            }
            B = mm_diff(mm_relrank(G.Y, G.w, AT, 0, 1))
            min = min(G.Y)
            if (min==AT[1]) {
                if (G.w!=1) B[1] = B[1] + quadsum(select(G.w, (G.Y:==AT[1])))
                else        B[1] = B[1] + sum(G.Y:==AT[1])
            }
            if (!D.freq) {
                if      (D.prop) c = 1 / G.W
                else if (D.pct)  c = 100 / G.W
                else {
                    c = 1 :/ (h * G.W)
                    _editmissing(c, 0) // zero width bins
                }
                B = B :* c
                if (D.uncond) B = B * (G.W/dstat_Wj(D, G))
            }
            D.b[|a \ b|] = B \ B[rows(B)]
            if (D.noIF==0) dstat_hist_IF(D, G, a, b, min)
        }
    }
}

void dstat_hist_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `RS' min)
{
    `Int' i
    `RS'  W, c
    `RC'  z
    
    W = (D.uncond ? dstat_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b-1|] = D.b[|a \ b-1|]
    for (i=a; i<=b; i++) {  // assuming b-a >= 1
        if (i==b) {
            D.IF[,i] = J(D.N, 1, 0)
            break
        }
        if (i==a) {
            if (D.at[i,1]<=min) z = G.Y:<=D.at[i+1,1]
            else  z = (G.Y:>D.at[i,1] :& G.Y:<=D.at[i+1,1])
        }
        else z = (G.Y:>D.at[i,1] :& G.Y:<=D.at[i+1,1])
        if      (D.prop) z = (z :- D.b[i]    ) / W
        else if (D.pct)  z = (z :- D.b[i]/100) * (100/W)
        else if (D.freq) z = (z :- D.b[i]/W  )
        else {
            c = 1 / (D.at[i,3] * W)
            if (c>=.) c = 0 // zero width bin
            z = (z :- D.b[i]*D.at[i,3]) * c
        }
        dstat_set_IF(D, G, i, z, -D.b[i]/W, D.b[i]/W)
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
    
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            if (D.AT[D.k]==NULL) AT = dstat_cdf_AT(G.Y, D.n)
            else                 AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "c":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) dstat_noobs(D, a, b)
            else {
                D.b[|a \ b|] = 
                    _dstat_cdf(G.Y, G.w, AT, D.mid+D.floor*2, D.freq, D.ipolate)
                if (cc) {
                    if (D.freq) D.b[|a \ b|] = G.W :- D.b[|a \ b|]
                    else        D.b[|a \ b|] = 1   :- D.b[|a \ b|]
                }
                if (D.pct)  D.b[|a \ b|] = D.b[|a \ b|] * 100
                if (D.uncond & !D.freq)
                    D.b[|a \ b|] = D.b[|a \ b|] * (G.W/dstat_Wj(D, G))
                if (D.noIF==0) dstat_cdf_IF(D, G, a, b, cc)
            }
        }
    }
}

`RC' dstat_cdf_AT(`RC' Y, `Int' n)
{
    `RR' minmax
    
    if (rows(Y)==0) return(J(n,1,.z))
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
    `RS'  W
    `RR'  C
    `RC'  zP
    `RM'  z, AT
    
    W = (D.uncond ? dstat_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b|] = D.b[|a \ b|]
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
                    C[k]  = sum(quadcross(G.w, (z[,k], zP))) / W
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
                    C[k]  = quadcross(G.w, z[,k]) / W
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
            dstat_set_IF(D, G, i, z, -C/W, D.b[i]/W)
        }
        return
    }
    for (i=a; i<=b; i++) {
        if (D.pct)       C = D.b[i] / 100
        else if (D.freq) C = D.b[i] / W
        else             C = D.b[i]
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
        dstat_set_IF(D, G, i, z, -D.b[i]/W, D.b[i]/W)
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
    
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "p":+strofreal(1::b-a+1) : (D.cat ? strofreal(AT) :
                strofreal(AT, D.vfmt))))
            if (G.N==0) dstat_noobs(D, a, b)
            else {
                D.b[|a \ b|] = _dstat_prop(D, G, AT)
                if (D.noIF==0) dstat_prop_IF(D, G, a, b)
            }
        }
    }
    if (D.cat & !D.novalues) D.eqs = D.eqs[mm_seq(1, D.k, D.nvars)]
}

`RC' _dstat_prop(`Data' D, `Grp' G, `RC' AT)
{
    `Int' i
    `RS'  W
    `RM'  H
    `RC'  F
    `T'   A
    
    // obtain frequency table
    H = dstat_freq(G)
    // post H as hash table
    A = asarray_create("real")
    asarray_notfound(A, 0)
    for (i=rows(H); i; i--) asarray(A, H[i,1], H[i,2])
    // copy frequencies
    i = rows(AT)
    F = J(i, 1, .)
    for (; i; i--) F[i] = asarray(A, AT[i])
    // return
    W = (D.uncond ? dstat_Wj(D, G) : G.W)
    if (D.pct)  return(F * (100/W))
    if (D.freq) return(F)
                return(F / W)
}

void dstat_prop_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i
    `RS'  W
    `RC'  h
    
    W = (D.uncond ? dstat_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b|] = D.b[|a \ b|]
    for (i=a; i<=b; i++) {
        if (D.pct)       h = ((G.Y :== D.at[i]) :- D.b[i]/100) * (100/W)
        else if (D.freq) h = ((G.Y :== D.at[i]) :- D.b[i]/W  )
        else             h = ((G.Y :== D.at[i]) :- D.b[i]    ) / W
        dstat_set_IF(D, G, i, h, -D.b[i]/W, D.b[i]/W)
    }
}

// --------------------------------------------------------------------------
// dstat quantile
// --------------------------------------------------------------------------

void dstat_quantile(`Data' D)
{
    `Int'  i, j, a, b
    `RC'   AT
    `Grp'  G
    
    if (D.noIF==0 & D.qdef!=10) D.hasdens = `TRUE'
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "q":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) dstat_noobs(D, a, b)
            else {
                if (D.qdef==10) {
                    _dstat_sum_hdquantile(D, G, a, b, AT)
                    continue
                }
                D.b[|a \ b|] = _mm_quantile(G.Ys(), G.ws(), AT, D.qdef, D.wtype==1)
                if (D.noIF==0) {
                    dstat_set_density(D, G)
                    dstat_quantile_IF(D, G, a, b)
                }
            }
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
        l++
        if (D.at[i]==0)      D.IF[,i] = J(D.N, 1, 0)
        else if (D.at[i]==1) D.IF[,i] = J(D.N, 1, 0)
        else {
            z = (G.Y :<= D.b[i])
            dstat_set_IF(D, G, i, (mean(z, G.w) :- z) / (G.W * fx[l]))
        }
    }
}

// --------------------------------------------------------------------------
// dstat lorenz
// --------------------------------------------------------------------------

void dstat_lorenz(`Data' D)
{
    `Int'  i, j, a, b, t, z
    `RC'   AT
    `Grp'  G
    
    if (D.gap & D.pct) t = 6 // equality gap in percent
    else if (D.gap)    t = 5 // equality gap
    else if (D.abs)    t = 4 // absolute
    else if (D.gl)     t = 3 // generalized
    else if (D.sum)    t = 2 // total
    else if (D.pct)    t = 1 // ordinary in percent
    else               t = 0 // ordinary
    if (D.zvar!="") z = 1
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j, z)
            if (z<.) G.Zset(z, D.Z[G.p,z])
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "l":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) dstat_noobs(D, a, b)
            else {
                D.b[|a \ b|]  = _dstat_lorenz(G, AT, t, z)
                if (D.noIF==0) dstat_lorenz_IF(D, G, a, b, t, z)
            }
        }
    }
}

`RC' _dstat_lorenz(`Grp' G, `RC' P, `Int' t, | `Int' z)
{   // determine Lorenz ordinates using interpolation
    `RS'  Ytot
    `PS'  Yc, Wc
    `RC'  p, L

    if (z<.) {
        Yc = &(G.YcZ())
        Wc = &(G.WcZ())
    }
    else {
        Yc = &(G.Yc())
        Wc = &(G.Wc())
    }
    Ytot = (*Yc)[rows(*Yc)]
    p = order(P, 1)
    L = P[p] * (*Wc)[rows(*Wc)]
        // using Wc[rows(Wc)] instead of G.W to avoid precision issues at P=1
    L[p] = mm_fastipolate(*Wc, *Yc, L)
        // (assuming that sum of weights>0 for each unique Y-value)
    if (t==6) return((P - L/Ytot) * 100)   // equality gap in %
    if (t==5) return(P - L/Ytot)           // equality gap
    if (t==4) return((L - P:*Ytot) / G.W)  // absolute
    if (t==3) return(L / G.W)              // generalized
    if (t==2) return(L)                    // total
    if (t==1) return(L * (100/Ytot))       // ordinary in %
              return(L = L / Ytot)         // ordinary
}

void dstat_lorenz_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Int' t, | `Int' z)
{
    if (t==2) D.IFtot[|a \ b|] = D.b[|a \ b|] // total
    D.IF[|1,a\.,b|] = _dstat_lorenz_IF(D, G, D.at[|a \ b|], t, D.b[|a \ b|], z)
}

`RM' _dstat_lorenz_IF(`Data' D, `Grp' G, `RC' at, `Int' t, `RC' B, | `Int' z)
{
    `Int' i, n
    `RC'  q, ey
    `RM'  IF
    
    n = rows(at)
    if (z<.) {
        q = _mm_quantile(G.Zs(), G.wsZ(), at, 2)
        ey = _dstat_lorenz_IF_EY(G, q)
    }
    else ey = q = _mm_quantile(G.Ys(), G.ws(), at, 2)
    IF = J(D.N, n, 0)
    for (i=1; i<=n; i++) {
        if (at[i]==0)               continue
        if (at[i]==1 & t!=2 & t!=3) continue
        IF[,i] = __dstat_lorenz_IF(D, G, (z<. ? G.Z() : G.Y):<=q[i],
            ey[i], t, B[i], z)
    }
    return(IF)
}

`RC' _dstat_lorenz_IF_EY(`Grp' G, `RC' at)
{
    `IntC' p
    `RC'   ey
    
    p  = order(at, 1)
    ey = J(rows(at), 1, .)
    ey[p] = mm_fastipolate(G.EYat(), G.EY(), at[p], 1)
    return(ey)
}

`RC' __dstat_lorenz_IF(`Data' D, `Grp' G, `RC' Zq, `RS' ey, `Int' t, `RS' B,
    `Int' zvar)
{
    `RS' p, L, T, W
    `RC' z, h
    
    if (zvar<.) {
        T = (G.YcZ())[rows(G.YcZ())]
        W = (G.WcZ())[rows(G.WcZ())]
    }
    else {
        T = (G.Yc())[rows(G.Yc())]
        W = (G.Wc())[rows(G.Wc())]
    }
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
    return(_dstat_set_IF(D, G, h, 0, t==2 ? B/G.W : 0))
}

// --------------------------------------------------------------------------
// dstat share
// --------------------------------------------------------------------------

void dstat_share(`Data' D)
{
    `Int'  i, j, a, b, d, t, z
    `RC'   L, AT, B, h
    `Grp'  G
    
    if      (D.prop) {; d = 0; t = 0; } // proportion
    else if (D.ave)  {; d = 1; t = 3; } // average
    else if (D.gl)   {; d = 0; t = 3; } // generalized
    else if (D.sum)  {; d = 0; t = 2; } // total
    else if (D.pct)  {; d = 0; t = 1; } // percent
    else             {; d = 1; t = 0; } // density
    if (D.zvar!="") z = 1
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j, z)
            if (z<.) G.Zset(z, D.Z[G.p,z])
            AT = *D.AT[D.k]
            h = mm_diff(AT) // bin width
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a,1 \ b,.|] = AT, AT + (h/2 \ .), (h \ .)
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "s":+strofreal(1::b-a)\"_ul" : strofreal(AT, D.vfmt)))
            if (G.N==0) dstat_noobs(D, a, b)
            else {
                L = _dstat_lorenz(G, AT, t, z)
                B = mm_diff(L)
                if (d) B = B :* editmissing(1 :/ h, 0)
                D.b[|a \ b|]  = B \ B[rows(B)]
                if (D.noIF==0) dstat_share_IF(D, G, a, b, t, d, L, z)
            }
        }
    }
}

void dstat_share_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Int' t, `Int' d, 
    `RC' L, | `Int' z)
{
    if (t==2) D.IFtot[|a \ b-1|] = D.b[|a \ b-1|] // total
    D.IF[|1,a \ .,b-1|] = _dstat_share_IF(D, G, D.at[|a,1 \ b,1|], t, d, L, z)
    D.IF[,b] = J(D.N, 1, 0)
}

`RM' _dstat_share_IF(`Data' D, `Grp' G, `RC' at, `Int' t, `Int' d, `RC' L, 
    | `Int' z)
{
    `Int' i, n
    `RM'  IF
    
    n = rows(at)
    IF = _dstat_lorenz_IF(D, G, at, t, L, z)
    for (i=1; i<n; i++) {
        if (d) IF[,i] = (IF[,i+1] - IF[,i]) * editmissing(1/(at[i+1]-at[i]), 0)
        else   IF[,i] =  IF[,i+1] - IF[,i]
    }
    return(IF[|1,1 \ .,n-1|])
}

// --------------------------------------------------------------------------
// dstat tip
// --------------------------------------------------------------------------

void dstat_tip(`Data' D)
{
    `Int'  i, j, a, b, t, z
    `RC'   AT
    `Grp'  G
    
    // D.hcr = D.pgi = J(1, D.nvars*D.nover, .)
    if   (D.abs) t = 1 // absolute
    else         t = 0 // relative
    if (D.plvar!="") z = 1
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j, z)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            dstat_set_nobs_sumw(D, G, a, b)
            dstat_set_cstripe(D, G, i, j, a, b, (D.novalues ? 
                "tip":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) dstat_noobs(D, a, b)
            else D.b[|a \ b|] = _dstat_tip(D, G, AT, t, a, b, 
                                z<. ? D.Z[G.p, z] : D.pline)
        }
    }
}

`RC' _dstat_tip(`Data' D, `Grp' G, `RC' P, `Int' t, `Int' a, `Int' b, `RC' pl)
{   // determine tip ordinates
    `RC' pg, p, p2, L, hc
    `RM' cdf
    
    hc = D.pstrong ? G.Y:<=pl : G.Y:<pl
    if (t==1) pg =  (pl :- G.Y)        :* hc
    else      pg = ((pl :- G.Y) :/ pl) :* hc
    p = mm_order(-pg, 1, 1)
    cdf = _mm_ecdf2(pg[p], rows(G.w)==1 ? G.w : G.w[p], 0, 1)
    cdf = (0,0) \ (quadrunningsum(cdf[,1] :* mm_diff(0 \ cdf[,2])), cdf[,2])
    p2 = order(P, 1)
    L = P[p2] * cdf[rows(cdf),2]
        // using cdf[rows(cdf),2] instead of G.W to avoid precision issues at P=1
    L[p2] = mm_fastipolate(cdf[,2], cdf[,1], L)
    // D.hcr[D.k] = mean(hc, G.w)
    // D.pgi[D.k] = cdf[rows(cdf),1] / G.W
    if (D.noIF==0) dstat_tip_IF(D, G, a, b, P, pg)
    return(L / G.W)
}

void dstat_tip_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `RC' P, `RC' pg)
{
    `Int' i, j
    `RS'  p, t
    `RC'  q, zq, z
    
    q = -mm_quantile(-pg, G.w, P, 2)
    j = 0
    for (i=a; i<=b; i++) {
        j++
        if (P[j]==0) {
            dstat_set_IF(D, G, i, J(G.N, 1, 0))
            continue
        }
        zq = (pg:>=q[j])
        p  = mean(zq, G.w)   // set p=mean(zq) to ensure mean(IF)=0
        z  = pg :* zq
        t  = mean(z, G.w)    // set t=mean(z) to ensure mean(IF)=0
        dstat_set_IF(D, G, i, (z :- t :+ q[j] * (p :- zq)) / G.W)
    }
}

// --------------------------------------------------------------------------
// dstat summarize
// --------------------------------------------------------------------------

void dstat_sum(`Data' D)
{
    `Int' i, j, a, b, ii, nn
    `SC'  AT
    `SS'  s, o
    `SC'  list
    `Grp' G
    `T'   A
    `PR'  P
    
    A = dstat_stats_lib()
    list = sort(asarray_keys(A), 1)
    D.k = b = 0
    for (i=1; i<=D.nover; i++) {
        G = dstat_init_grp(D, i)
        for (j=1; j<=D.nvars; j++) {
            dstat_init_Y(D, G, j)
            AT = *D.AT[D.k]
            nn = rows(AT)
            a = b + 1
            b = a + nn - 1
            dstat_set_cstripe(D, G, i, j, a, b, AT)
            if (G.N==0) {
                dstat_noobs(D, a, b)
                dstat_set_nobs_sumw(D, G, a, b)
            }
            else {
                for (ii=1; ii<=nn; ii++) {
                    _dstat_sum_reset_Y(D, G) // (sample might have changed)
                    s = AT[ii]
                    o = _dstat_sum_split(s) // replaces s
                    P = asarray(A, mm_strexpand(s, list))
                    (*P[1])(D, G, a, o, *P[3])
                    dstat_set_nobs_sumw(D, G, a)
                    a++
                }
            }
        }
    }
}

void _dstat_sum_reset_Y(`Data' D, `Grp' G)
{
    `Bool' hasmis
    `RC'   y, touse
    
    if (G.reset==0) return
    if (D.nocw) {
        y = D.Y[G.G0.p,G.j]
        hasmis = hasmissing(y)
        if (hasmis) touse = (y:<.)
    }
    else hasmis = 0
    _dstat_init_Y(D, G, 1, hasmis, touse)
    G.reset = 0
}

void _dstat_sum_update_Y(`Data' D, `Grp' G, `RC' y)
{
    if (rows(G.pp)==0) _dstat_init_Y(D, G, 1, 1, y:<.)
    else _dstat_init_Y(D, G, 1, 1, dstat_invp(G.G0.N, G.pp, (y:<.), 0))
    G.reset = 1
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

void _dstat_sum_fixed(`Data' D, `Int' i, `RS' b)
{
    D.b[i] = b
    if (D.noIF) return(1)
    D.IF[,i] = J(D.N, 1, 0)
}

void _dstat_sum_invalid(`Data' D, `Grp' G, `SS' s, `Int' N)
{
    `SS' t
    
    if (D.ovar!="") {
        if (G.id<.) t = sprintf("%s in %s=%g", D.yvars[G.j], D.ovar, G.id)
        else        t = sprintf("%s in total", D.yvars[G.j])
    }
    else t = sprintf("%s", D.yvars[G.j])
    t = sprintf("%s of %s: %g observations out of support", s, t, G.N-N)
    if (D.relax) {
        printf("{txt}(%s)\n", t)
    }
    else {
        printf("{err}%s\n", t)
        printf("{err}specify {bf:relax} to use valid observations only\n")
        exit(459)
    }
}

void _dstat_sum_set_z(`Data' D, `Grp' G, `SS' o, `RR' O, | `RC' o1)
{
    `Int' z
    `RC'  Z
    
    if (args()<5) {
        if (o=="") z = selectindex(D.zvars:==D.zvar)
        else       z = selectindex(D.zvars:==dstat_unab(o))
    }
    else           z = __dstat_sum_set_z(D, o, O, o1) // parse z1-syntax
    Z = D.Z[G.p, z]
    if (D.nocw) {
        if (hasmissing(Z)) {
            _dstat_sum_update_Y(D, G, Z) // update sample
            G.Zset(z, Z[G.pp])
            return
        }
    }
    G.Zset(z, Z)
}

`Int' __dstat_sum_set_z(`Data' D, `SS' o, `RR' O, `RC' o1)
{
    `Int' l, z
    `SR'  args
    
    args = tokens(o)
    l = length(args)
    if (l==0) {
        z = selectindex(D.zvars:==D.zvar)
        o1 = O[3]
    }
    else if (l==1) {
        o1 = strtoreal(args[1])
        if (o1<.) z = selectindex(D.zvars:==D.zvar)
        else {
            z = selectindex(D.zvars:==dstat_unab(args[1]))
            o1 = O[3]
        }
    }
    else {
        z = selectindex(D.zvars:==dstat_unab(args[1]))
        o1 = strtoreal(args[2])
    }
    return(z)
}

`RC' _dstat_sum_get_pl(`Data' D, `Grp' G, `SS' o, `RR' O, | `RC' o1)
{
    `Int' z
    `SS'  s
    `RC'  pl
    
    // collect arguments
    if (args()<5) s = o
    else          s = __dstat_sum_get_pl(o, O, o1) // parse 1pl-syntax
    // determine whether pl is a variable
    if (s!="") {
        if (strtoreal(s)>=.) {
            z = selectindex(D.zvars:==dstat_unab(s))
        }
    }
    else if (D.plvar!="") {
        z = selectindex(D.zvars:==D.plvar)
    }
    // if pl is not a variable: return scalar
    if (z>=.) {
        if (s!="") return(strtoreal(s))
        return(D.pline)
    }
    // if pl is a variables: possibly update sample and return variable
    pl = D.Z[G.p, z]
    if (D.nocw==0) return(pl)
    if (hasmissing(pl)) {
        _dstat_sum_update_Y(D, G, pl) // update sample
        pl = pl[G.pp]
    }
    return(pl)
}

`SS' __dstat_sum_get_pl(`SS' o, `RR' O, `RC' o1)
{
    `SR'  args
    `Int' l

    args = tokens(o)
    l = length(args)
    if (l==0) {
        o1 = O[3]
        return("")
    }
    o1 = strtoreal(args[1])
    if (l==1) return("")
    return(args[2])
}

`RC' _dstat_sum_ccdf(`RC' Y, `RC' w, `RS' W)
{
    return((quadsum(w) :- _mm_ranks(Y, w, 3, 1)) / W)
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
    
    if (p==0)      {; _dstat_sum_fixed(D, i, G.Ys()[1]);   return; }
    else if (p==1) {; _dstat_sum_fixed(D, i, G.Ys()[G.N]); return; }
    if (D.qdef==10) {
        _dstat_sum_hdquantile(D, G, i, i, p)
        return
    }
    D.b[i] = _mm_quantile(G.Ys(), G.ws(), p, D.qdef, D.wtype==1)
    if (D.noIF) return
    fx = _dstat_sum_d(D, G, D.b[i])
    z = (G.Y :<= D.b[i])
    dstat_set_IF(D, G, i, (mean(z, G.w) :- z) / (G.W * fx))
}

void dstat_sum_hdquantile(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_hdquantile(D, G, i, i, strtoreal(o)/100)
}

void _dstat_sum_hdquantile(`Data' D, `Grp' G, `Int' a, `Int' b, `RC' p)
{
    D.b[|a \ b|] = _mm_hdq(G.Ys(), G.ws(), p, D.wtype==1)
    if (D.noIF) return
    _dstat_sum_hdq_IF(0, D, G, a, b, p)
}

`T' _dstat_sum_hdq_IF(`Bool' noset, `Data' D, `Grp' G, `Int' i0, `Int' i1, `RC' p)
{   // obtain IFs in analogy to the jackknife approach by Harrell and Davis
    // (1982); in the unweighted case, results are equivalent to
    // hdquantiles_sd() from Python's -scipy-
    `RS'  i, k, n
    `RC'  F, F0
    `RM'  IF
    
    if      (rows(G.w)==1) n = G.N * G.w
    else if (D.wtype==1)   n = G.W
    else                   n = G.W^2 / quadsum(G.w:^2)
    if (G.w==1) F = 0 \ (1::n)/(n-1)
    else {
        F  = _mm_ranks(G.Ys(), G.ws(), 0, 0, 1) * (n/(n-1))
        F0 = F :- (1 / (n-1))
    }
    k = 0
    if (noset) {
        IF = J(G.N, i1-i0+1, .)
        for (i=i0; i<=i1; i++) {
            k++
            IF[,k] = __dstat_sum_hdq_IF(G, p[k], F, F0, n)
        }
        return(IF)
    }
    for (i=i0; i<=i1; i++) {
        dstat_set_IF(D, G, i, __dstat_sum_hdq_IF(G, p[++k], F, F0, n))
    }
}

`RC' __dstat_sum_hdq_IF(`Grp' G, `RS' p, `RC' F, `RC' F0, `RS' n)
{
    `RS'  a, b
    `RC'  d, h
    
    if (p==0) return(J(G.N, 1, 0))
    if (p==1) return(J(G.N, 1, 0))
    a = p * (n + 1)
    b = (1 - p) * (n + 1)
    if (G.w==1) d = mm_diff(ibeta(a, b, F)) * (1-n)
    else        d = (ibeta(a, b, F)-ibeta(a, b, F0)) * (1-n)
    h = quadrunningsum(d:*(G.Ys()[|2\G.N|]\0))
    h = (0\quadrunningsum(d:*G.Ys()))[|1\G.N|] + (h[G.N] :- (0\h[|1\G.N-1|]))
    return(dstat_invp(G.N, G.pY(), h :- mean(h, G.ws())) / G.W)
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
    D.b[i] = _dstat_sum_d(D, G, at)
    if (D.noIF) return
    z = dstat_invp(G.N, G.pY(), D.S.K(D.S.X(), at, D.S.h()*D.S.l()))
    if (D.exact) dstat_set_IF(D, G, i, (z :- D.b[i])/G.W)
    else         dstat_set_IF(D, G, i, (z :- mean(z, G.w))/G.W)
}

`RC' _dstat_sum_d(`Data' D, `Grp' G, `RC' at)
{
    D.hasdens = `TRUE'
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
    _dstat_sum_freq(D, G, i, o, O, 0)
    if (D.omit[i]) return
    D.b[i] = D.b[i] / G.W
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / G.W
}

void dstat_sum_pct(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_freq(D, G, i, o, O, 0)
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
    _dstat_sum_freq(D, G, i, o, O, 1)
}

void _dstat_sum_freq(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O, `Bool' tot)
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
    if (tot) D.IFtot[i] = D.b[i]
    dstat_set_IF(D, G, i, (z :- (D.b[i]/G.W)), 0, D.b[i]/G.W)
}

void dstat_sum_total(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RR' at
    `RC' z
    pragma unused O
    
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
    D.IFtot[i] = D.b[i]
    dstat_set_IF(D, G, i, (z :- (D.b[i]/G.W)), 0, D.b[i]/G.W)
}

void dstat_sum_min(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    
    _dstat_sum_fixed(D, i, min(G.Y))
}

void dstat_sum_max(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O

    _dstat_sum_fixed(D, i, max(G.Y))
}

void dstat_sum_range(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O

    _dstat_sum_fixed(D, i, mm_diff(minmax(G.Y)))
}

void dstat_sum_midrange(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O

    _dstat_sum_fixed(D, i, sum(minmax(G.Y))/2)
}

void dstat_sum_mean(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    D.b[i] = G.mean()
    if (D.noIF) return
    dstat_set_IF(D, G, i, (G.Y :- D.b[i]) / G.W)
}

void dstat_sum_gmean(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' z
    pragma unused o
    pragma unused O
    
    z = ln(G.Y)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "gmean", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = ln(G.Y)
    }
    D.b[i] = exp(mean(z, G.w))
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- ln(D.b[i])) * (D.b[i] / G.W))
}

void dstat_sum_hmean(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC'   z
    pragma unused o
    pragma unused O
    
    z = 1 :/ G.Y
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "hmean", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = 1 :/ G.Y
    }
    D.b[i] = 1 / mean(z, G.w)
    if (D.noIF) return
    if (_dstat_sum_omit(D, i)) return
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
    q = _mm_quantile(G.Ys(), G.ws(), p1\p2, D.qdef, D.wtype==1)
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
    q = _mm_quantile(G.Ys(), G.ws(), p1\p2, D.qdef, D.wtype==1)
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
    med = _mm_median(G.Ys(), G.ws())
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
    med = _mm_median(G.Ys(), G.ws())
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
    
    D.b[i] = mm_hl(G.Ys(), G.ws(), D.wtype==1)
    if (D.noIF) return
    z = 2*D.b[i] :- G.Ys()
    F = _mm_relrank(G.Ys(), G.ws(), z[G.N::1], 1)[G.N::1]
    d = _dstat_sum_d(D, G, z)
    F[G.pY()] = F
    dstat_set_IF(D, G, i, (mean(F, G.w) :- F) / (mean(d, G.ws()) * G.W))
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
    `RS' m, c, df
    pragma unset m
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    D.b[i] = _dstat_variance(G.Y, G.w, D.wtype==1 ? G.W : G.N, df, m)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    dstat_set_IF(D, G, i, (c*(G.Y :- m):^2 :- D.b[i]) / G.W)
}

`RS' _dstat_variance(`RC' Y, `RC' w, `RS' W, | `RS' df, `RS' m)
{    // replaces m
    `RC' v
    
    if (args()<4) df = 1
    v = mm_meanvariance0(Y, w)
    m = v[1]
    v = v[2]
    if (df!=0) v = v * (W / (W - df))
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
    `RS' b1
    `RC' p, IF1
    
    if (o!="") p = strtoreal(tokens(o)') / 100
    else       p = O[(3,4)]' / 100
    _dstat_sum_q(D, G, i, p[1]) // lower quantile
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _dstat_sum_q(D, G, i, p[2]) // upper quantile
    D.b[i] = D.b[i] - b1
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] - IF1
}

void dstat_sum_iqrn(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' c
    `RC' p

    dstat_sum_iqr(D, G, i, o, O)
    if (o!="") p = strtoreal(tokens(o)') / 100
    else       p = O[(3,4)]' / 100
    c = 1 / (invnormal(p[2]) - invnormal(p[1]))
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
    
    t  = G.mean()
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
    
    t = G.mean()
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

    q = _mm_median(G.Ys(), G.ws())
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

    q    = J(3,1,.)
    q[2] = _mm_median(G.Ys(), G.ws())
    z = abs(G.Y :- q[2])
    D.b[i] = mm_median(z, G.w) // z not sorted!
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
    
    z = abs(G.Y :- t)
    D.b[i] = mm_median(z, G.w) // z not sorted!
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
    
    m   = G.mean()
    F   = _mm_ranks(G.Ys(), G.ws(), 3, 1, 1)
    cov = cross(G.Ys(), G.ws(), F) / G.W
    D.b[i] = 4*cov - 2*m // note: MD = 2 * mean * Gini
    if (D.noIF) return
    // In analogy to the Gini coefficient the main moment condition of the MD
    // can be written as h = 2*(2*F - 1)*Y - MD
    B = dstat_invp(G.N, G.pY(), _dstat_sum_ccdf(G.Ys(), G.Ys():*G.ws(), G.W))
    F[G.pY()] = F
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
    med = _mm_median(G.Ys(), G.ws())
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
    
    D.b[i] = mm_qn(G.Ys(), G.ws(), D.wtype==1)
    if (D.noIF) return
    c  = sqrt(2) * invnormal(5/8)
    z1 = G.Ys() :+ D.b[i]*c
    z2 = G.Ys() :- D.b[i]*c
    F  = _mm_relrank(G.Ys(), G.ws(), z1, 1) :- _mm_relrank(G.Ys(), G.ws(), z2, 1)
    d  = _dstat_sum_d(D, G, z1)
    F[G.pY()] = F
    dstat_set_IF(D, G, i, (mean(F, G.w) :- F) * (1 / (c * mean(d, G.ws()) * G.W)))
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
    q = _mm_quantile(G.Ys(), G.ws(), p \ 0.5 \ 1-p, D.qdef, D.wtype==1)
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
    
    D.b[i] = mm_mc(G.Ys(), G.ws(), D.wtype==1)
    if (D.noIF) return
    mc = D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Ys(), G.ws())
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
    F[p] = _mm_relrank(G.Ys(), G.ws(), z[p], 1)
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
    q = _mm_quantile(G.Ys(), G.ws(), p, D.qdef, D.wtype==1)
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
    q = _mm_quantile(G.Ys(), G.ws(), p, D.qdef, D.wtype==1)
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
    q = _mm_quantile(G.Ys(), G.ws(), p, D.qdef, D.wtype==1)
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
    
    med = _mm_median(G.Ys(), G.ws())
    p = selectindex(G.Ys():<med)
    if (rows(p)==0) D.b[i] = . // no obs
    else D.b[i] = -mm_mc(G.Ys()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()), D.wtype==1)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    mc = -D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Ys()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()))
    dq = _dstat_sum_d(D, G, q)
    F1 = _dstat_sum_mc_F(G, z = (G.Y * (mc-1) :+ 2*q) / (mc+1))
    F2 = _dstat_sum_mc_F(G,     (G.Y * (mc+1) :- 2*q) / (mc-1))
    d  = _dstat_sum_d(D, G, z) :* (G.Y:>=q :& G.Y:<=med)
    dH = mean(32 * d :* ((q :- G.Y) / (mc+1)^2), G.w)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    gmed = (med*(mc-1) + 2*q) / (mc+1)
    Fmed = _mm_relrank(G.Ys(), G.ws(), gmed, 1)
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
    
    med = _mm_median(G.Ys(), G.ws())
    p = selectindex(G.Ys():>med)
    if (rows(p)==0) D.b[i] = . // no obs
    else D.b[i] = mm_mc(G.Ys()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()), D.wtype==1)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    mc = D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Ys()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()))
    dq = _dstat_sum_d(D, G, q)
    F1 = 1 :- _dstat_sum_mc_F(G, z = (G.Y * -(mc+1) :+ 2*q) /  (1-mc))
    F2 = 1 :- _dstat_sum_mc_F(G,     (G.Y *  (1-mc) :- 2*q) / -(mc+1))
    d  = _dstat_sum_d(D, G, z) :* (G.Y:<=q :& G.Y:>=med)
    dH = mean(32 * d :* ((G.Y :- q) / (1-mc)^2), G.w)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    gmed = (med*(mc+1) - 2*q) / (mc-1)
    Fmed = 1 - _mm_relrank(G.Ys(), G.ws(), gmed, 1)
    IF = (1 :- 16*F1:*(G.Y:<q :& G.Y:>med) 
            :- 16*(F2:-.25):*(G.Y:>q :& G.Y:<gmed)
            :-  4*(G.Y:>gmed)
            :-  8*sign(med:-G.Y)*Fmed
            :+ (.25:-(G.Y:>q))*(4 - 32*mean(d, G.w)/(dq*(1-mc)))) / dH
    IF = IF :- mean(IF, G.w) // make sure that IF is centered at zero
    dstat_set_IF(D, G, i, IF / G.W)
}

void dstat_sum_hoover(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' t
    `RC' z, z2
    pragma unused o
    pragma unused O
    
    t  = G.mean()
    z  = G.Y :- t
    z2 = abs(z)
    D.b[i] = mean(z2, G.w) / (2 * t)
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((z2 / (2 * t) :- D.b[i])
        - mean(sign(z) * t + z2, G.w) / (2 * t^2) * z) / G.W)
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
    m  = G.mean()
    F  = _mm_ranks(G.Ys(), G.ws(), 3, 1, 1)
    cp = cross(G.Ys(), G.ws(), F) / G.W
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
    B = dstat_invp(G.N, G.pY(), _dstat_sum_ccdf(G.Ys(), G.Ys():*G.ws(), G.W))
    F[G.pY()] = F
    if (abs) {
        dstat_set_IF(D, G, i, (((2*F :- 1):*G.Y + 2*(B :- cp))*c :- D.b[i]) / G.W)
    }
    else {
        dstat_set_IF(D, G, i, ((F :- cp/m):*G.Y :+ B :- cp) * ((2*c) / (m*G.W)))
        // = 1/m * (c*(2*F :- 1):*G.Y :- D.b[i]:*G.Y :+ c*2*(B :- cp)) / G.W
    }
}

void dstat_sum_mld(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O, | `SS' lbl)
{
    `RS'   m
    `RC'   z
    pragma unused o
    pragma unused O
    
    z = ln(G.Y)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, lbl!="" ? lbl : "mld", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = ln(G.Y)
    }
    m = G.mean()
    D.b[i] = ln(m) - mean(z, G.w)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (((ln(m):-z) :- D.b[i]) + (G.Y :- m)/m) / G.W)
}

void dstat_sum_theil(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O, | `SS' lbl)
{
    `RS'   m, delta
    `RC'   z
    pragma unused o
    pragma unused O
    
    z = ln(G.Y)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, lbl!="" ? lbl : "theil", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = ln(G.Y)
    }
    m = G.mean()
    z = G.Y :* z
    D.b[i] = mean(z, G.w)/m - ln(m)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    delta = mean(z/m^2, G.w) :+ 1/m
    dstat_set_IF(D, G, i,(((z/m:-ln(m)) :- D.b[i]) - delta*(G.Y :- m)) / G.W)
}

void dstat_sum_cv(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS' m, sd, c, df
    pragma unset m
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    sd = sqrt(_dstat_variance(G.Y, G.w, D.wtype==1 ? G.W : G.N, df, m))
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
    `RS'   a, c, m, mz, delta
    `RC'   z
    
    if (o!="") a = strtoreal(o)
    else       a = O[3]
    if (a==0) {
        dstat_sum_mld(D, G, i, o, O, "ge(0)")
        return
    }
    if (a==1) {
        dstat_sum_theil(D, G, i, o, O, "ge(1)")
        return
    }
    c = 1 / (a * (a - 1))
    z = G.Y:^a
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "ge("+strofreal(a)+")", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = G.Y:^a
    }
    m = G.mean()
    mz = mean(z, G.w)
    D.b[i] = c * (mz/m^a - 1)
    if (_dstat_sum_omit(D, i)) return
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
    `RS'   m, lm
    `RC'   z
    
    z = ln(G.Y)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "atkinson(1)", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = ln(G.Y)
    }
    m = G.mean()
    lm = mean(z, G.w)
    D.b[i] = 1 - exp(lm) / m
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, ((G.Y:-m)/m :- (z :- lm)) * (exp(lm) / (m * G.W)))
}

void _dstat_sum_atkinson(`Data' D, `Grp' G, `Int' i, `RS' e)
{
    `RS'   m, lm
    `RC'   z
    
    z = G.Y:^(1-e)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "atkinson("+strofreal(e)+")", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = G.Y:^(1-e)
    }
    m = G.mean()
    lm = mean(z, G.w)
    D.b[i] = 1 - lm^(1/(1-e)) / m
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i,((G.Y:-m) * (lm^(1/(1-e)) / m^2) :- (z :- lm) *
        (lm^(e/(1-e)) / ((1-e)*m))) / G.W)
}

void dstat_sum_lvar(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   m, c, df
    `RC'   z
    
    if (o!="") df = strtoreal(o)
    else       df = O[3]
    z = ln(G.Y)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "lvar", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = ln(G.Y)
    }
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    m = G.mean()
    D.b[i] = mm_mse(z, G.w, ln(m)) * c
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    z = z :- ln(m)
    dstat_set_IF(D, G, i, (c*z:^2 :- D.b[i] :- 2*mean(z, G.w)/m:*(G.Y:-m)) / G.W)
}

void dstat_sum_vlog(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   m, c, df
    `RC'   z
    pragma unset m

    if (o!="") df = strtoreal(o)
    else       df = O[3]
    c = 1
    z = ln(G.Y)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "vlog", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        z = ln(G.Y)
    }
    D.b[i] = _dstat_variance(z, G.w, D.wtype==1 ? G.W : G.N, df, m)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    dstat_set_IF(D, G, i, (c*(z :- m):^2 :- D.b[i]) / G.W)
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
    _dstat_sum_share(D, G, i, p[|1\2|]) // lower share
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _dstat_sum_share(D, G, i, p[|3\4|]) // upper share
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
    D.b[i] = _dstat_lorenz(G, at, t)
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _dstat_lorenz_IF(D, G, at, t, D.b[i])
}

void dstat_sum_tlorenz(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
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
    `RC' L
    
    if (args()<5) t = 0
    if (args()<6) d = 0
    if (at[1]>at[2]) D.b[i] = .
    else {
        L = _dstat_lorenz(G, at, t)
        D.b[i] = mm_diff(L)
        if (d) D.b[i] = D.b[i] * editmissing(1/(at[2]-at[1]), 0)
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _dstat_share_IF(D, G, at, t, d, L)
}

void dstat_sum_dshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
    _dstat_sum_share(D, G, i, strtoreal(tokens(o)') / 100, 0, 1)
}

void dstat_sum_tshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused O
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
    `RS'  df, m, mF, cov, c
    `RM'  mv
    `RC'  F, B

    _dstat_sum_set_z(D, G, o, O, df=.)
    F   = _mm_ranks(G.Zs(), G.wsZ(), 3, 1, 1)
    mv  = mm_meanvariance0((G.YsZ(), F), G.wsZ())
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
    B = dstat_invp(G.N, G.pZ(), _dstat_sum_ccdf(G.Zs(), (G.YsZ():-m):*G.wsZ(), G.W))
    F[G.pZ()] = F
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
    _dstat_sum_ccurve(D, G, i, 0, o, O)
}

void _dstat_sum_ccurve(`Data' D, `Grp' G, `Int' i, `Int' t, `SS' o, `RR' O)
{
    `SR'  args
    `RS'  at
    
    args = tokens(o)
    at   = strtoreal(args[1]) / 100
    _dstat_sum_set_z(D, G, length(args)>1 ? args[2] : "", O)
    D.b[i] = _dstat_lorenz(G, at, t, G.z())
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _dstat_lorenz_IF(D, G, at, t, D.b[i], G.z())
}

void dstat_sum_tccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_ccurve(D, G, i, 2, o, O)
}

void dstat_sum_gccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_ccurve(D, G, i, 3, o, O)
}

void dstat_sum_accurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_ccurve(D, G, i, 4, o, O)
}

void dstat_sum_eccurve(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_ccurve(D, G, i, 5, o, O)
}

void dstat_sum_cshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_cshare(D, G, i, 0, 0, o, O)
}

void _dstat_sum_cshare(`Data' D, `Grp' G, `Int' i, `Int' t, `Bool' d,
    `SS' o, `RR' O)
{
    `SR'  args
    `RC'  L, at
    
    args = tokens(o)
    at   = strtoreal(args[|1\2|]') / 100
    _dstat_sum_set_z(D, G, length(args)>2 ? args[3] : "", O)
    if (at[1]>at[2]) D.b[i] = .
    else {
        L = _dstat_lorenz(G, at, t, G.z())
        D.b[i] = mm_diff(L)
        if (d) D.b[i] = D.b[i] * editmissing(1/(at[2]-at[1]), 0)
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _dstat_share_IF(D, G, at, t, d, L, G.z())
}

void dstat_sum_dcshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_cshare(D, G, i, 0, 1, o, O)
}

void dstat_sum_tcshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_cshare(D, G, i, 2, 0, o, O)
}

void dstat_sum_gcshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_cshare(D, G, i, 3, 0, o, O)
}

void dstat_sum_acshare(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_cshare(D, G, i, 3, 1, o, O)
}

void dstat_sum_hcr(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC' z, pl
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    z = (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_pgap(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_pgap(0, D, G, i, o, O)
}

void dstat_sum_apgap(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_pgap(1, D, G, i, o, O)
}

void _dstat_sum_pgap(`Bool' abs, `Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   N
    `RC'   z, h, w, pl
    `IntC' p
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    p = selectindex(D.pstrong ? G.Y:<=pl : G.Y:<pl)
    N = length(p)
    if (N==0) { // no poor
        D.b[i] = 0
        if (D.noIF) return
        dstat_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    w = (rows(G.w)==1 ? G.w : G.w[p])
    if (abs==0) z = ((pl :- G.Y) :/ pl)[p]
    else        z = (pl :- G.Y)[p]
    D.b[i] = mean(z, w)
    if (D.noIF) return
    h = J(G.N, 1, 0)
    h[p] = (z :- D.b[i]) / (rows(G.w)!=1 ? quadsum(G.w[p]) : N)
    dstat_set_IF(D, G, i, h)
}

void dstat_sum_pgi(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_pgi(0, D, G, i, o, O)
}

void dstat_sum_apgi(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_pgi(1, D, G, i, o, O)
}

void _dstat_sum_pgi(`Bool' abs, `Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC'   z, pl
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    z = (pl :- G.Y) :* (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    if (abs==0) z = z :/ pl
    D.b[i] = mean(z, G.w)
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_fgt(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'  a
    `RC'  z, pl
    
    pl = _dstat_sum_get_pl(D, G, o, O, a=.)
    z = (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    z = ((pl :- G.Y) :* z :/ pl):^a :* z
    if (hasmissing(z)) D.b[i] = .
    else               D.b[i] = mean(z, G.w)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_chu(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   a, m
    `RC'   z, pl, h
    
    pl = _dstat_sum_get_pl(D, G, o, O, a=.)
    a  = a/100
    z = (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    if (a==0) z = (ln(G.Y) :- ln(pl)) :* z
    else      z = (1 :- (G.Y:/pl):^a) :* z
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "chu("+strofreal(a*100)+")", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        if (rows(pl)==rows(z)) pl = select(pl, z:<.)
        z = select(z, z:<.)
    }
    m = mean(z, G.w)
    if (a==0) {
        D.b[i] = 1 - exp(mean(z, G.w))
        if (_dstat_sum_omit(D, i)) return
        if (D.noIF) return
        h = -exp(m)*(z :- m)
    }
    else {
        D.b[i] = mean(z, G.w) / a
        if (_dstat_sum_omit(D, i)) return
        if (D.noIF) return
        h = z / a :- D.b[i]
    }
    dstat_set_IF(D, G, i, h / G.W)
}

void dstat_sum_watts(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RC'   z, pl
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    z = (ln(pl) :-  ln(G.Y)) :* (G.Y :< pl)
    if (hasmissing(z)) {
        _dstat_sum_invalid(D, G, "watts", G.N-missing(z))
        _dstat_sum_update_Y(D, G, z) // update sample
        if (rows(pl)==rows(z)) pl = select(pl, z:<.)
        z = select(z, z:<.)
    }
    D.b[i] = mean(z, G.w)
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    dstat_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void dstat_sum_sen(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   hcr, pgi, Gp
    `RC'   z_hcr, z_pgi, z_Gp, pl
    `RS'   m, cp, N, W
    `RC'   Ys, ws, F, B
    `IntC' p
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    // head count ratio
    z_hcr  = (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    hcr    = mean(z_hcr, G.w)
    // poverty gap index
    z_pgi  = ((pl :- G.Y) :/ pl) :* z_hcr
    pgi    = mean(z_pgi, G.w)
    // Gini among poor
    p = mm_order(G.Y, 1, rows(G.w)!=1)
    p = select(p, z_hcr[p])
    N = length(p)
    if (N==0) Gp = 0 // no poor
    else {
        Ys = G.Y[p]
        ws = (rows(G.w)!=1 ? G.w[p] : G.w)
        m  = mean(Ys, ws)
        F  = _mm_ranks(Ys, ws, 3, 1, 1)
        W = (rows(ws)==1 ? N*ws : quadsum(ws))
        cp = cross(Ys, ws, F) / W
        Gp = cp * 2 / m - 1
    }
    // sen poverty index
    D.b[i] = pgi + (hcr - pgi) * Gp
    if (D.noIF) return
    if (N==0) z_Gp = J(G.N, 1, 0) // no poor
    else {
        B = z_Gp = J(G.N, 1, 0)
        B[p] = _dstat_sum_ccdf(Ys, Ys:*ws, W)
        z_Gp[p] = F
        z_Gp = z_hcr :* ((z_Gp :- cp/m):*G.Y :+ B :- cp) * (2 / m) * (G.W/W)
    }
    z_hcr = Gp             * (z_hcr :- hcr)
    z_pgi = (1+(hcr-1)*Gp) * (z_pgi :- pgi)
    dstat_set_IF(D, G, i, (z_pgi + z_hcr + (hcr-pgi) * z_Gp) / G.W)
}

void dstat_sum_sst(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   pgi, Gp, m, cp
    `RC'   pg, pl, Ys, ws, F, B, h
    `IntC' p
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    // poverty gap index
    pg  = ((pl :- G.Y) :/ pl) :* (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    pgi = mean(pg, G.w)
    // Gini of poverty gaps
    p  = mm_order(pg, 1, rows(G.w)!=1)
    Ys = pg[p]
    ws = (rows(G.w)!=1 ? G.w[p] : G.w)
    m  = mean(Ys, ws)
    F  = _mm_ranks(Ys, ws, 3, 1, 1)
    cp = cross(Ys, ws, F) / G.W
    Gp = cp * 2 / m - 1 
    // SST 
    D.b[i] = pgi * (1 + Gp) 
    if (D.noIF) return
    // part of IF related to Gini
    B = _dstat_sum_ccdf(Ys, Ys:*ws, G.W)
    h = J(G.N, 1, 0)
    h[p] = ((F :- cp/m):*Ys :+ B :- cp) * (2 / m)
    // add part related to PGI
    dstat_set_IF(D, G, i, ((1+Gp) * (pg :- pgi) + pgi * h) / G.W)
}

void dstat_sum_takayama(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'   m, cp
    `RC'   pl, Ys, ws, F, B, h
    `IntC' p
    
    pl = _dstat_sum_get_pl(D, G, o, O)
    // generate censored outcomes
    Ys = (D.pstrong ? G.Y:<=pl : G.Y:<pl)
    Ys = (G.Y :* Ys) + (pl :* (1:-Ys))
    // compute Gini from censored outcome
    p  = mm_order(Ys, 1, rows(G.w)!=1)
    Ys = Ys[p]
    ws = (rows(G.w)!=1 ? G.w[p] : G.w)
    m  = mean(Ys, ws)
    F  = _mm_ranks(Ys, ws, 3, 1, 1)
    cp = cross(Ys, ws, F) / G.W
    D.b[i] = cp * 2 / m - 1 
    if (D.noIF) return
    B = _dstat_sum_ccdf(Ys, Ys:*ws, G.W)
    h = J(G.N, 1, 0)
    h[p] = ((F :- cp/m):*Ys :+ B :- cp) * (2 / m)
    dstat_set_IF(D, G, i, h / G.W)
}

void dstat_sum_tip(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_tip(D, G, i, 0, o, O)
}

void dstat_sum_atip(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_tip(D, G, i, 1, o, O)
}

void _dstat_sum_tip(`Data' D, `Grp' G, `Int' i, `Int' t, `SS' o, `RR' O)
{
    `RS'  at
    `RC'  pl
    
    pl = _dstat_sum_get_pl(D, G, o, O, at=.)
    D.b[i] = _dstat_tip(D, G, at/100, t, i, i, pl) // also computes IF
}

void dstat_sum_corr(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'  mY, sdY, mZ, sdZ, cov
    `RM'  mv
    `RC'  zY, zZ
    
    _dstat_sum_set_z(D, G, o, O)
    mv  = mm_meanvariance0((G.Y, G.Z()), G.w)
    sdY = sqrt(mv[2,1]); sdZ = sqrt(mv[3,2]); cov = mv[3,1]
    D.b[i] = cov / (sdY * sdZ)
    if (_dstat_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    mY = mv[1,1];   mZ = mv[1,2]
    zY = (G.Y:-mY); zZ = (G.Z():-mZ)
    dstat_set_IF(D, G, i, ((zY:*zZ :- cov) - cov*((zY:^2:-sdY^2)/(2*sdY^2) 
        + (zZ:^2:-sdZ^2)/(2*sdZ^2))) / (sdY*sdZ*G.W))
}

void dstat_sum_cov(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RS'  df, c, mY, mZ
    `RM'  mv
    
    _dstat_sum_set_z(D, G, o, O, df=.)
    mv  = mm_meanvariance0((G.Y, G.Z()), G.w)
    if (df!=0) { // small-sample adjustment
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    D.b[i] = mv[3,1] * c
    if (_dstat_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    mY = mv[1,1]; mZ = mv[1,2]
    dstat_set_IF(D, G, i, (c*((G.Y:-mY) :* (G.Z():-mZ)) :- D.b[i]) / G.W)
}

void dstat_sum_spearman(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `RM' mv, cov, vy, vz
    `RC' FY, FZ, hY, hZ, hYZ
    
    _dstat_sum_set_z(D, G, o, O)
    FY = FZ = J(G.N, 1, .)
    FY[G.pY()] = _mm_ranks(G.Ys(), G.ws(), 3, 1, 1)
    FZ[G.pZ()] = _mm_ranks(G.Zs(), G.wsZ(), 3, 1, 1)
    mv = mm_variance0((FY, FZ), G.w)
    cov = mv[2,1]; vy = mv[1,1]; vz = mv[2,2]
    D.b[i] = cov / (sqrt(vy) * sqrt(vz))
    if (_dstat_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    hY = hZ = J(G.N, 1, .)
    hY[G.pY()] = _dstat_sum_ccdf(FY[G.pY()], (FZ:*G.w)[G.pY()], G.W)
    hZ[G.pZ()] = _dstat_sum_ccdf(FZ[G.pZ()], (FY:*G.w)[G.pZ()], G.W)
    hYZ        = (FY:*FZ + hY + hZ) :- 3*(cov + 0.25)
    hY[G.pY()] = _dstat_sum_ccdf(FY[G.pY()], (2*FY:*G.w)[G.pY()], G.W)
    hY         = (FY:^2 + hY) :- 3*(vy + 0.25)
    hZ[G.pZ()] = _dstat_sum_ccdf(FZ[G.pZ()], (2*FZ:*G.w)[G.pZ()], G.W)
    hZ         = (FZ:^2 + hZ) :- 3*(vz + 0.25)
    dstat_set_IF(D, G, i, (hYZ - cov * (hY/(2*vy) + hZ/(2*vz))) /
                          (sqrt(vy)*sqrt(vz) * G.W))
}

void dstat_sum_taua(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Bool' naive
    `RS'   K, S
    `RC'   h
    
    _dstat_sum_set_z(D, G, o, O, naive=.)
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    D.b[i] = S / K
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = (1/K)*(G.cd_s() :- S) - (S/K^2)*(G.cd_k() :- K)
    dstat_set_IF(D, G, i, h * (2/G.W))
}

void dstat_sum_taub(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Bool' naive
    `RS'   K, S, T, U, Q
    `RC'   h
    
    _dstat_sum_set_z(D, G, o, O, naive=.)
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    T = G.cd_T()
    U = G.cd_U()
    Q = sqrt((K - T) * (K - U))
    D.b[i] = S / Q
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = (2*K-T-U)*(G.cd_k():-K) + (U-K)*(G.cd_t():-T) + (T-K)*(G.cd_u():-U)
    h = (1/Q)*(G.cd_s() :- S) - (S/(2*Q^3))*h 
    dstat_set_IF(D, G, i, h * (2/G.W))
}

void dstat_sum_somersd(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Bool' naive
    `RS'   K, S, T, Q
    `RC'   h
    
    _dstat_sum_set_z(D, G, o, O, naive=.)
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    T = G.cd_T()
    Q = K - T
    D.b[i] = S / Q
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = (1/Q)*(G.cd_s() :- S) - (S/Q^2)*((G.cd_k() - G.cd_t()) :- Q)
    dstat_set_IF(D, G, i, h * (2/G.W))
}

void dstat_sum_gamma(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Bool' naive
    `RS'   K, S, T, U, V, Q
    `RC'   h
    
    _dstat_sum_set_z(D, G, o, O, naive=.)
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    T = G.cd_T()
    U = G.cd_U()
    V = G.cd_V()
    Q = K - (T + U - V)
    D.b[i] = S / Q
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = G.cd_k() - G.cd_t() - G.cd_u() + G.cd_v()
    h = (1/Q)*(G.cd_s() :- S) - (S/Q^2)*(h :- Q)
    dstat_set_IF(D, G, i, 2*h / G.W)
}

void dstat_sum_hhi(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    (void) _dstat_sum_hhi(D, G, i)
}

void dstat_sum_hhin(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Int' K
    pragma unused o
    pragma unused O
    
    K = _dstat_sum_hhi(D, G, i)
    if (D.omit[i]) return
    if (K==1) {
        D.b[i] = 1
        return
    }
    D.b[i] = (D.b[i] - 1/K) / (1 - 1/K)
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / (1 - 1/K)
}

`Int' _dstat_sum_hhi(`Data' D, `Grp' G, `Int' i)
{
    `Int' K, j
    `RM' H
    `RC' h
    
    H = dstat_freq(G)
    H[,2] = (H[,2]/G.W)
    K = rows(H)
    D.b[i] = quadsum(H[,2]:^2)
    if (_dstat_sum_omit(D, i)) return(K)
    if (D.noIF) return(K)
    h = J(G.N, 1, 0)
    for (j=K;j;j--) h = h + H[j,2]*((G.Y:==H[j,1]) :- H[j,2])
    dstat_set_IF(D, G, i, h * (2/G.W))
    return(K)
}

void dstat_sum_gimp(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    pragma unused o
    pragma unused O
    
    (void) _dstat_sum_hhi(D, G, i)
    if (D.omit[i]) return
    D.b[i] = 1 - D.b[i]
    if (D.noIF) return
    D.IF[,i] = -D.IF[,i]
}

void dstat_sum_entropy(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Int' base, j
    `RS'  c
    `RM'  H
    `RC'  h
    
    if (o!="") base = strtoreal(o)
    else       base = O[3] // base=0 => ln()
    H = dstat_freq(G)
    H[,2] = (H[,2]/G.W)
    if (base==0) D.b[i] = -quadsum(H[,2] :* ln(H[,2]))
    else         D.b[i] = -quadsum(H[,2] :* (ln(H[,2])/ln(base)))
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    h = J(G.N, 1, 0)
    for (j=rows(H);j;j--) {
        c = ln(H[j,2]) + 1
        if (base!=0) c = c / ln(base)
        h = h + c*((G.Y:==H[j,1]) :- H[j,2])
    }
    dstat_set_IF(D, G, i, -h/G.W)
}

void dstat_sum_hill(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Int' q, j
    `RS'  b0
    `RM'  H
    `RC'  h
    
    if (o!="") q = strtoreal(o)
    else       q = O[3]
    H = dstat_freq(G)
    H[,2] = (H[,2]/G.W)
    if (q==1) {
        b0 = -quadsum(H[,2] :* ln(H[,2]))
        D.b[i] = exp(b0)
    }
    else {
        b0 = quadsum(H[,2]:^q)
        D.b[i] = b0^(1/(1-q))
    }
    if (_dstat_sum_omit(D, i)) return
    if (D.noIF) return
    h = J(G.N, 1, 0)
    j = rows(H)
    if (q==1) {
        for (;j;j--) h = h + (ln(H[j,2]) + 1) * ((G.Y:==H[j,1]) :- H[j,2])
        h = exp(b0) :* -h
    }
    else {
        for (;j;j--) h = h + (q * H[j,2]:^(q-1)) * ((G.Y:==H[j,1]) :- H[j,2])
        h = ((1/(1-q))*b0^(1/(1-q)-1)) :* h 
    }
    dstat_set_IF(D, G, i, h/G.W)
}

void dstat_sum_renyi(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Int' q
    `RS'  b0
    
    if (o!="") q = strtoreal(o)
    else       q = O[3]
    if (q==1) {
        dstat_sum_entropy(D, G, i, "", J(1,3,0))
        return
    }
    dstat_sum_hill(D, G, i, "", J(1,3,q))
    if (D.omit[i]) return
    b0 = D.b[i]
    D.b[i] = ln(b0)
    if (D.noIF) return
    D.IF[,i] = (1/b0) :* D.IF[,i]
}

void dstat_sum_mindex(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_minfo(1, D, G, i, o, O)
}

void dstat_sum_uc(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_minfo(2, D, G, i, o, O)
}

void dstat_sum_ucl(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_minfo(3, D, G, i, o, O)
}

void dstat_sum_ucr(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    _dstat_sum_minfo(4, D, G, i, o, O)
}

void _dstat_sum_minfo(`Int' stat, `Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{   // stat: 0 = joint entropy [not used]
    //       1 = M index
    //       2 = uncertainty coefficient (symmetric)
    //       3 = uncertainty coefficient (left)
    //       4 = uncertainty coefficient (right)
    // could also add support for conditional entropy:
    //     H(Y|Z) = H(Y,Z) - H(Z)  and  H(Z|Y) = H(Y,Z) - H(Y)
    `Int' base, j
    `RS'  b, bY, bZ, c
    `RM'  H, HY, HZ
    `RC'  h, hY, hZ
    
    _dstat_sum_set_z(D, G, o, O, base=.)
    _dstat_freq2(H = (G.Y, G.Z()), G.w, G.N)
    H[,3]  = (H[,3]/G.W)
    if (base==0) b = -quadsum(H[,3] :* ln(H[,3]))
    else         b = -quadsum(H[,3] :* (ln(H[,3])/ln(base)))
    if (stat) {
        _dstat_freq2(HY = H[,1], H[,3], rows(H))
        if (base==0) bY = -quadsum(HY[,2] :* ln(HY[,2]))
        else         bY = -quadsum(HY[,2] :* (ln(HY[,2])/ln(base)))
        _dstat_freq2(HZ = H[,2], H[,3], rows(H))
        if (base==0) bZ = -quadsum(HZ[,2] :* ln(HZ[,2]))
        else         bZ = -quadsum(HZ[,2] :* (ln(HZ[,2])/ln(base)))
    }
    if      (stat==1)  D.b[i] =    bY + bZ - b              // M index
    else if (stat==2)  D.b[i] = 2*(bY + bZ - b) / (bY + bZ) // UC symmetric
    else if (stat==3)  D.b[i] =   (bY + bZ - b) / bY        // UC left
    else if (stat==4)  D.b[i] =   (bY + bZ - b) / bZ        // UC right
    else               D.b[i] =              b              // joint entropy
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = J(G.N, 1, 0)
    for (j=rows(H);j;j--) {
        c = ln(H[j,3]) + 1
        if (base!=0) c = c / ln(base)
        h = h + c*((G.Y:==H[j,1]:&G.Z():==H[j,2]) :- H[j,3])
    }
    if (stat) {
        hY = J(G.N, 1, 0)
        for (j=rows(HY);j;j--) {
            c = ln(HY[j,2]) + 1
            if (base!=0) c = c / ln(base)
            hY = hY + c*((G.Y:==HY[j,1]) :- HY[j,2])
        }
        hZ = J(G.N, 1, 0)
        for (j=rows(HZ);j;j--) {
            c = ln(HZ[j,2]) + 1
            if (base!=0) c = c / ln(base)
            hZ = hZ + c*((G.Z():==HZ[j,1]) :- HZ[j,2])
        }
    }
    if      (stat==1)  h = hY + hZ - h                           // M index
    else if (stat==2)  h = 2*(b/(bY+bZ)^2*(hY + hZ) - h/(bY+bZ)) // UC symmetric
    else if (stat==3)  h = (b-bZ)/bY^2*hY + hZ/bY - h/bY         // UC left
    else if (stat==4)  h = hY/bZ + (b-bY)/bZ^2*hZ - h/bZ         // UC right
    dstat_set_IF(D, G, i, -h/G.W)
}

void dstat_sum_cramer(`Data' D, `Grp' G, `Int' i, `SS' o, `RR' O)
{
    `Int' bc
    `Int' j, c, C, r, R
    `RS'  b, pij, pi, pj, d
    `T'   A
    `RM'  H, HY, HZ
    `RC'  h
    pragma unset bc     // bias correction not implemented yet
    pragma unused bc
    
    _dstat_sum_set_z(D, G, o, O, bc=.)
    _dstat_freq2(H = (G.Y, G.Z()), G.w, G.N)
    H[,3]  = (H[,3]/G.W)
    _dstat_freq2(HY = H[,1], H[,3], rows(H))
    _dstat_freq2(HZ = H[,2], H[,3], rows(H))
    A = asarray_create("real",2) // post in asarray() to fill empty cells 
    asarray_notfound(A, 0)
    for (j=rows(H); j; j--) asarray(A, H[j,(1,2)], H[j,3])
    R = rows(HY); C = rows(HZ)
    H = J(R*C,1,.)
    j = 0
    for (r=R;r;r--) {
        for (c=C;c;c--) {
            pi  = HY[r,2]
            pj  = HZ[c,2]
            pij = asarray(A, (HY[r,1], HZ[c,1]))
            H[++j] = (pij - pi*pj)^2 / (pi*pj)
        }
    }
    b = quadsum(H)
    D.b[i] = sqrt(b / min((R-1, C-1)))
    if (_dstat_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = J(G.N, 1, 0)
    // - contributions of cells
    for (r=R;r;r--) {
        for (c=C;c;c--) {
            pi  = HY[r,2]
            pj  = HZ[c,2]
            pij = asarray(A, (HY[r,1], HZ[c,1]))
            d = 2 * (pij/(pi*pj) - 1)
            h = h + d * ((G.Y:==HY[r,1]:&G.Z():== HZ[c,1]) :- pij)
        }
    }
    // - contributions of rows
    for (r=R;r;r--) {
        d = 0
        pi = HY[r,2]
        for (c=C;c;c--) {
            pj  = HZ[c,2]
            pij = asarray(A, (HY[r,1], HZ[c,1]))
            d = d + (pij - pi*pj)*(pij + pi*pj) / (pi^2*pj)
        }
        h = h - d * ((G.Y:==HY[r,1]) :- pi)
    }
    // - contributions of columns
    for (c=C;c;c--) {
        d = 0
        pj = HZ[c,2]
        for (r=R;r;r--) {
            pi  = HY[r,2]
            pij = asarray(A, (HY[r,1], HZ[c,1]))
            d = d + (pij - pi*pj)*(pij + pi*pj) / (pi*pj^2)
        }
        h = h - d * ((G.Z():==HZ[c,1]) :- pj)
    }
    h = h / (2 * sqrt(b * min((R-1, C-1))))
    dstat_set_IF(D, G, i, h/G.W)
}

end

exit




