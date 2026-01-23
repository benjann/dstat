*! version 1.5.6  23jan2026  Ben Jann

capt mata: assert(mm_version()>=206)
if _rc {
    di as error "{bf:moremata} version 2.0.6 or newer is required; " _c
    di as error "type {stata ssc install moremata, replace}"
    error 499
}

program dstat, eclass
    version 14
    local version : di "version " string(_caller()) ":"
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
        `version' Predict `00'
        exit
    }
    if `"`subcmd'"'=="save" {
        Save `00'
        exit
    }
    if `"`subcmd'"'==substr("frequency",1,max(4,strlen(`"`subcmd'"'))) {
        // alias for -proportion, frequency-
        local subcmd proportion
        Parse_frequency `00'
    }
    else if "`subcmd'"!="pw" {
        capt Parse_subcmd `subcmd' // expands subcmd
        if _rc==1 exit _rc
        if _rc { // try summarize
            local 00 `"`subcmd'`00'"'
            local subcmd summarize
        }
        else if "`subcmd'"=="histogram" {
            Parse_hist_discrete `00' // redirect to proportion if nocategprical
        }
    }
    Get_diopts `subcmd' `00' // returns 00, diopts, dioptshaslevel
    tempname BW AT
    `version' Check_vce "`BW' `AT'" `subcmd' `00'
    if "`vcetype'"=="svyr" { // svy linearized
        if `"`svylevel'"'!="" & `dioptshaslevel'==0 {
            local diopts `svylevel' `diopts'
        }
        `version' SVY_linearized `"`svyopts'"' `subcmd' `00'
        SVY_cleanup `nocov'
    }
    else if "`vcetype'"=="svy" { // svy brr etc.
        `version' svy `svytype', noheader notable `svyopts': /*
            */ dstat_svyr `subcmd' `00'
        SVY_cleanup `nocov'
    }
    else if "`vcetype'"!="" { // bootstrap/jackknife
        `version' _vce_parserun dstat, noeqlist wtypes(pw iw) ///
            bootopts(noheader notable force) ///
            jkopts(noheader notable force) : `subcmd' `00'
        if "`nocov'"!="" Remove_Cov
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
        if `"`markesample'"'!="" exit
        if c(stata_version)<15 {
            _estimates unhold `ecurrent', not
        }
    }
    eret local cmdline `"dstat `0'"'
    Set_CI, `diopts'
    Replay, `diopts'
    if `"`e(generate)'"'!="" {
        if `"`generate_quietly'"'=="" {
            tempname rcurrent
            _return hold `rcurrent'
            describe `e(generate)'
            _return restore `rcurrent'
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
    if `"`0'"'==substr("pshare",1,max(3,strlen(`"`0'"'))) {
        c_local subcmd pshare
        exit
    }
    if `"`0'"'==substr("share",1,max(2,strlen(`"`0'"'))) { // old syntax
        c_local subcmd pshare
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
    version 14
    local version : di "version " string(_caller()) ":"
    gettoken BWAT 0 : 0
    gettoken subcmd 0 : 0
    _parse comma lhs 0 : 0
    syntax [, vce(str) NOSE NOCOV COV * ]
    if `"`vce'"'=="" exit
    Parse_vceopt `vce' /* returns vcetype and, depending on situation, vcevars,
        svyopts, svylevel, svytype, svysubpop */
    if "`vcetype'"=="" exit // no prefix command
    // cov/nocov
    if "`cov'"!="" & "`nocov'"!="" {
        di as err "only one of {bf:cov} and {bf:nocov} allowed"
        exit 198
    }
    if !inlist(`"`subcmd'"',"summarize","pw") & "`cov'"=="" local nocov nocov
    c_local nocov `nocov'
    // svy linearized
    if "`vcetype'"=="svyr" {
        c_local 00 `lhs', `options'
        c_local svyopts `svyopts'
        c_local svylevel `svylevel'
        c_local vcetype svyr
        exit
    }
    // check for options that are not allowed with replication techniques and
    // parse options related to grid and bandwidth
    local 0 `", `options'"'
    syntax [, Generate(passthru) BALance(passthru) /*
        */ BWidth(passthru) BWADJust(passthru) noBWFIXed /*
        */ n(passthru) range(passthru) at(passthru) * ]
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
    `version' Obtain_bwat "`BWAT'" `subcmd' `lhs', /*
        */ `bwidth' `bwadjust' `bwfixed' `n' `range' `at' nose `options' /*
        */ _vcevars(`vcevars') _vcetype(`vcetype') _svysubpop(`svysubpop')
    local options `bwidth' `bwadjust' `n' `at' `range' `options'
    // svy brr etc.
    if "`vcetype'"=="svy" {
        c_local 00 `lhs', `options'
        c_local svyopts `svyopts'
        c_local svytype `svytype'
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
    // split vcetype [args]
    _parse comma vcetype 0 : 0
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

program Obtain_bwat // may update bwidth, bwadjust, n, at, range
    version 14
    local version : di "version " string(_caller()) ":"
    // determine whether grid or bandwidth needs to be determined
    gettoken BWAT   0 : 0
    gettoken subcmd 0 : 0
    if "`subcmd'"=="quantile" exit
    if "`subcmd'"=="lorenz" exit
    if "`subcmd'"=="pshare" exit
    if "`subcmd'"=="tip" exit
    if "`subcmd'"=="pw" exit
    if inlist("`subcmd'","summarize","density") local bwopt bwidth(str)
    local atopt n(passthru) range(passthru) at(passthru)
    _parse comma lhs 0 : 0
    syntax [, `bwopt' `atopt' noBWFIXed ///
        _vcevars(str) _vcetype(str) _svysubpop(str) * ]
    local getat 0
    if "`subcmd'"=="summarize" {
        if `"`at'"'!="" local getat 1
    }
    else if `"`at'"'=="" local getat 1
    local options `n' `range' `at' `options'
    local getbw 0
    if "`bwfixed'"!="" {
        local bwopt
        if `"`bwidth'"'!="" {
            local options bwidth(`bwidth') `options'
        }
    }
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
    // run dstat to determine grid and bandwidth
    gettoken BW BWAT : BWAT
    gettoken AT      : BWAT
    if `getat' & `getbw' local tmp "evaluation grid and bandwidth"
    else if `getbw'      local tmp "bandwidth"
    else                 local tmp "evaluation grid"
    if `"`_vcetype'"'=="svy" {
        di as txt "(running {bf:dstat_svyr} to obtain `tmp')"
        quietly `version' svy, `_svysubpop' vce(linearized): /*
            */ dstat_svyr `subcmd' `lhs', `options'
    }
    else if `"`_vcevars'"'!="" {
        di as txt "(running {bf:dstat} to obtain `tmp')"
        local 0 `lhs', `options'
        syntax [anything] [if] [in] [fw iw pw] [, * ]
        marksample touse
        markout `touse' `_vcevars', strok
        quietly dstat `subcmd' `anything' if `touse' [`weight'`exp'], `options'
    }
    else {
        di as txt "(running {bf:dstat} to obtain `tmp')"
        quietly dstat `subcmd' `lhs', `options'
    }
    if `getat' { 
        matrix `AT' = e(at)
        if "`subcmd'"=="summarize" {
            local AT `e(atvar)' = `AT'
        }
        c_local at at(`AT')
        c_local n
        c_local range
    }
    if `getbw' { 
        matrix `BW' = e(bwidth)
        c_local bwidth bwidth(`BW')
        c_local bwadjust
    }
end

program SVY_linearized, eclass
    local version : di "version " string(_caller()) ":"
    gettoken svyopts cmdline : 0
    nobreak {
        _svyset get strata 1
        if `"`r(strata1)'"'=="" {
            // must set strata so that _robust (which is called by svy) does
            // not assume the mean of the scores variables to be zero; this
            // is relevant for unnormalized statistics (totals, frequencies)
            tempname cons
            quietly gen byte `cons' = 1
            _svyset set strata 1 `cons'
        }
        capture noisily break {
            `version' svy linearized, noheader notable `svyopts': /*
                */ dstat_svyr `cmdline'
        }
        local rc = _rc
        if `"`cons'"'!="" {
            _svyset clear strata 1
            eret local strata1 ""
        }
        if `rc' exit `rc'
    }
end

program SVY_cleanup, eclass
    args nocov
    eret local cmd "dstat"
    eret local cmd0 ""
    eret local cmdname ""
    eret local command ""
    eret local V_modelbased "" // remove matrix e(V_modelbased)
    if "`nocov'"!="" Remove_Cov
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
    version 14
    local version : di "version " string(_caller()) ":"
    if `"`e(cmd)'"'!="dstat" {
        if `"`e(cmd0)'"'!="dstat_svyr" {
            di as err "last dstat results not found"
            exit 301
        }
    }
    tempname rcurrent
    _return hold `rcurrent'
    syntax [anything] [if] [in], [ SCores RIF SCAling(passthru) COMpact QUIetly ]
    _score_spec `anything', ignoreeq
    local vlist `s(varlist)'
    `version' Predict_IFs, generate(`vlist', `rif' `scaling' `compact')
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
    _return restore `rcurrent'
end

program Predict_IFs
    version 14
    local version : di "version " string(_caller()) ":"
    syntax [, generate(passthru) ]
    local predict `generate'
    local prefix `"`e(prefix)'"'
    
    // estimation sample (and weights in case of svy)
    tempname touse
    local TOUSE `touse'
    qui gen byte `touse' = e(sample)==1
    qui count if `touse'
    if r(N)==0 {
        di as err "computation of influence functions failed; " /*
            */ "cannot identify estimation sample"
        exit 498
    }
    if `"`prefix'"'=="svy" {
        if `"`e(subpop)'"'!="" local subpop subpop(`e(subpop)')
        tempvar subuse wvar
        _svy_setup `touse' `subuse' `wvar', svy calibrate `subpop'
        if `"`r(poststrata)'"'!="" {
            local wvar0 `wvar'
            tempname wvar
            svygen post double `wvar' `wgt' [iw=`wvar0'] if `touse'==1, /*
                */ posts(`r(poststrata)') postw(`r(postweight)')
        }
        local wtexp [iw=`wvar']
        local TOUSE `subuse'
    }
    else local wtopt [fw iw pw]
    
    // compile command line
    local 0 `"`e(cmdline)'"'
    if inlist(`"`prefix'"', "bootstrap", "jackknife") {
        capt _on_colon_parse `0' // cmdline may include prefix
        if _rc==0 {
            local 0 `"`s(after)'"'
        }
    }
    gettoken cmd 0 : 0 // remove dstat or dstat_svyr
    syntax [anything] [if] [in] `wtopt' [, /*
        */ NOSE vce(passthru) Generate(passthru) rif(passthru) * ]
    if `"`weight'"'!="" local wtexp [`weight'`exp']
    local cmdline dstat `anything' if `TOUSE' `wtexp', nose `predict' `options'
    
    // compute IFs
    tempname ecurrent b
    mat `b' = e(b)
    _estimates hold `ecurrent', restore
    di as txt "(rerunning {bf:dstat} to obtain IFs)"
    quietly `cmdline'
    mat `b' = mreldif(`b',e(b)) // returns missing if non-conformable
    capt assert (`b'[1,1]<1e-15)
    if _rc==1 exit _rc
    if _rc {
        di as err "computation of influence functions failed; " /*
            */ "could not replicate original results"
        exit 498
    }
    c_local vlist `e(generate)'
    if `"`subpop'"'!="" {
        // fill in zeros outside of subpop (only relevant after svy)
        foreach v in `e(generate)' {
            qui replace `v' = 0 if `touse' & !`subuse'
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
    mata: ds_Get_CI("`CI'", `level', "`citype'", `scale')
    eret matrix ci = `CI'
    eret local citype "`citype'"
    eret scalar level = `level'
end

program Parse_citype
    capt n syntax [, NORMal logit probit atanh log ///
        AGRESti exact JEFFreys wilson ]
    if _rc==1 exit _rc
    if _rc {
        di as error "error in option {bf:citype()}"
        exit 198
    }
    local citype `normal' `logit' `probit' `atanh' `log' `agresti' `exact'/*
        */ `jeffreys' `wilson'
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
    _Replay, `graph' `options'
    if "`graph'"!="" {
        tempname rcurrent
        _return hold `rcurrent'
        Graph, `graph2'
        _return restore `rcurrent'
    }
end

program _Replay, rclass
    local subcmd `"`e(subcmd)'"'
    syntax [, citype(passthru) noHeader NOTABle TABle ///
        NOPValues PValues cref GRaph vsquish * ]
    local contrast `"`e(over_contrast)'"'
    if (`"`contrast'"'=="" | `"`e(over_ratio)'"'=="ratio") & "`subcmd'"!="pw"/*
        */ & "`pvalues'"=="" {
        local nopvalues nopvalues
    }
    local csinfo `"`e(csinfo)'"'
    local cssize: list sizeof csinfo
    local sinfo: list posof "statistics" in csinfo // 0 if not found
    local ovrlgnd: list posof "over" in csinfo
    if `ovrlgnd' local ovrlgnd = (`ovrlgnd'<`cssize')
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
            local j = e(qdef) + 1
            local qdef: word `j' of high invcdf avginvcdf closest parzen hazen/*
                */ weibull gumbel tukey blom hd mid california beard benard/*
                */ cooper gringorten
            di as txt _col(`c1') "Quantile method" _col(`c2') "= " as res %10s "`qdef'"
        }
        else if "`subcmd'"=="lorenz" | "`subcmd'"=="pshare" {
            if `"`e(byvar)'"'!="" {
                di as txt _col(`c1') "Sort variable" _col(`c2') "= " as res %10s e(byvar)
            }
        }
        else if "`subcmd'"=="tip" {
            di as txt _col(`c1') "Poverty line" _col(`c2') "= " as res %10s e(pline)
        }
        else if "`subcmd'"=="summarize" {
            if e(N_stats)==1 & `sinfo'==0 {
                di as txt _col(`c1') "Statistic" _col(`c2') "= " as res %10s e(stats)
            }
            if `"`e(atvar)'"'!="" & `"`e(categorical)'"'=="" {
                di as txt _col(`c1') "Condition" _col(`c2') "= " as res %10s e(atvar)
            }
            if `"`e(byvar)'"'!="" {
                di as txt _col(`c1') "By variable" _col(`c2') "= " as res %10s e(byvar)
            }
            if `"`e(pline)'"'!="" {
                di as txt _col(`c1') "Poverty line" _col(`c2') "= " as res %10s e(pline)
            }
        }
        if inlist(`"`e(total)'"',"mean","wmean") {
            di as txt _col(`c1') "Type of total" _col(`c2') "= " as res %10s /*
                */ `"`e(total)'"'
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
            if `ovrlgnd' _svy_summarize_legend
            else di ""
        }
        if `sinfo'==`cssize' local vsquish vsquish // stats as coefnames
        local options `vsquish' `options'
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
                    local citi `"`e(citype)'"'
                    if      `"`citi'"'=="agresti"  local citi "Agresti-Coull"
                    else if `"`citi'"'=="exact"    local citi "Clopper–Pearson"
                    else if `"`citi'"'=="jeffreys" local citi "Jeffreys"
                    else if `"`citi'"'=="wilson"   local citi "Wilson"
                    else local citi `"`citi' transformed"'
                    local cimatrix `cimatrix' cititle(`citi')
                }
            }
        }
        if `"`contrast'"'!="" {
            tempname B
            matrix `B' = e(b)
            mata: ds_drop_cref("`B'", 0, 1)
            if "`V'"!=""  mata: ds_drop_cref("`V'", 1, 1)
            if "`CI'"!="" mata: ds_drop_cref("`CI'", 0, 1)
        }
        _Replay_table "`B'" "`V'" `nopvalues' `vmatrix' `cimatrix' `options'
        return add
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
                    di as txt "(table displays normal CIs; Stata 15 or" /*
                        */ " newer required for other CI types in output table)"
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
        MERge OVERLay flip BYStats BYStats2(str) NOSTEP STEP ///
        NOREFline REFline(str) SELect(str) GSELect(str) PSELect(str) cref ///
        LEGend(passthru) BYOPTs(str) PLOTLabels(str asis) * ]
    if "`overlay'"!="" local merge merge
    _Graph_parse_select "" `"`select'"'
    _Graph_parse_select g `"`gselect'"'
    _Graph_parse_select p `"`pselect'"'
    if `"`gselect'"'=="" local gselect `"`select'"'
    if `"`pselect'"'=="" local pselect `"`select'"'
    local options `vertical' `horizontal' `options'
    if `"`bystats2'"'=="" & "`bystats'"!="" local bystats2 main
    _Graph_parse_bystats, `bystats2'
    _Graph_parse_byopts, `byopts'
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
    if `"`contrast'"'!="" mata: ds_drop_cref("`B'", 0, 1)
    local b matrix(`B')
    if !inlist(`"`subcmd'"',"summarize","pw") | `"`e(atvar)'"'!="" {
        if !((`"`subcmd'"'=="proportion" | `"`e(atvar)'"'!="")/*
            */ & `"`e(novalues)'"'=="" & `"`e(categorical)'"'!="") {
            tempname AT
            matrix `AT' = e(at)
            if `"`contrast'"'!="" mata: ds_drop_cref("`AT'", 0, 1)
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
            if `"`contrast'"'!="" mata: ds_drop_cref("`CI'", 0, 1)
            local ci ci(`CI')
            if inlist(`"`subcmd'"', "histogram", "pshare") {
                tempname BCI ATCI
                mat `BCI' = `B'
                mat `ATCI' = `AT'[2,1...] // bin midpoints
                mata: ds_graph_droplast(("`BCI'", "`CI'", "`ATCI'"))
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
    local csinfo `"`e(csinfo)'"'
    local overeq: list posof "over" in csinfo
    if `overeq'>2 local overeq 0
    else if `overeq'==2 & `:list sizeof csinfo'>3 local overeq 0
    if inlist(`"`subcmd'"',"summarize","pw") & `"`e(atvar)'"'=="" {
        local eqs: coleq `B'
        local eqs: list uniq eqs
        if "`bystats'"=="main" {
            if `"`eqs'"'!="_" {
                if strpos(`"`eqs'"',"~") {
                    mata: ds_graph_swap(1, `overeq')
                }
                else {
                    mata: ds_graph_swap(0, `overeq')
                }
            }
        }
        else if "`bystats'"=="secondary" {
            if strpos(`"`eqs'"',"~") {
                mata: ds_graph_swap(2, `overeq')
            }
        }
    }
    local eqs: coleq `B'
    local eqs: list uniq eqs
    local eqlist `"`eqs'"'
    if strpos(`"`eqs'"',"~") {
        mata: ds_graph_eqsplit()
        local eqs: list uniq eqs
        local subeqs: list uniq subeqs // may contain two elements
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
            if `overeq'==1 {
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
                if `overeq'==2 {
                    local lblj_`j': word `j' of `overlabels'
                }
                else {
                    local lblj_`j' `"`subeq'"'
                }
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
            mata: ds_graph_select("jlist", "gselect")
            local n`jj': list sizeof jlist
        }
        if `"`pselect'"'!="" & `n`ii''>1 {
            if `"`pselect'"'=="reverse" {
                mata: st_local("pselect", ///
                    invtokens(tokens(st_local("ilist"))[`n`ii''::1]))
            }
            mata: ds_graph_select("ilist", "pselect")
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
    else if inlist(`"`subcmd'"', "histogram", "pshare") {
        if `"`recast'"'=="" {
            local recast recast(bar) bartype(spanning)
            if `"`contrast'"'=="" {
                if "`horizontal'"=="" local options plotr(margin(b=0)) `options'
                else                  local options plotr(margin(l=0)) `options'
            }
            if c(stata_version)>=15 {
                if `n`ii''>1 local recast `recast' color(%50)
            }
            local ptype bar
            if `"`base'"'=="" {
                if `"`e(over_ratio)'"'=="ratio" local base base(1)
                else                            local base base(0)
            }
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
            if `"`contrast'"'=="" {
                if "`vertical'"=="" local options plotr(margin(l=0)) `options'
                else                local options plotr(margin(b=0)) `options'
            }
            local ptype bar
            if `"`barwidth'"'=="" {
                if `n`ii''==1 local barwidth 0.7
                else          local barwidth = .5 / `n`ii''
                local barwidth barwidth(`barwidth')
            }
            if `"`base'"'=="" {
                if `"`e(over_ratio)'"'=="ratio" local base base(1)
                else                            local base base(0)
            }
            if "`noci'"=="" local citop citop
        }
        if `"`ci_recast'"'=="" {
            local ci_recast recast(rcap)
        }
        local cipos 0 // CI not separate
    }
    else if `"`e(atvar)'"'!=""/*
        */ & !(`"`e(novalues)'"'=="" & `"`e(categorical)'"'!="") {
        if `"`recast'"'=="" {
            local recast recast(connect)
            local ptype
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
        if `"`legend'"'=="" {
            if `n`jj''==1               local legend legend(off)
            else if !`byopts_haslegend' local byopts legend(off) `byopts'
        }
    }
    
    // draw graph
    if `"`byopts'"'!="" local byopts byopts(`byopts')
    local plots `plots', `at' `bopts' `legend' `byopts' `options'
    //di `"`plots'"'
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

program _Graph_parse_byopts
    syntax [, LEGend(passthru) * ]
    c_local byopts_haslegend = `"`legend'"'!=""
    c_local byopts `legend' `options'
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
    mata: ds_Get_CI("`CI'", `level', "`citype'", `scale')
end

program Save, rclass
    if `"`e(cmd)'"'!="dstat" {
        di as err "last dstat results not found"
        exit 301
    }
    local subcmd `"`e(subcmd)'"'
    
    // syntax
    syntax [anything(equalok)] [, Prefix(name) Replace Level(passthru)/*
        */ citype(passthru) eform rescale(real 1) cref SELect(str) wide/*
        */ twoway HORizontal LABels(str asis) ] // undocumented
    if "`twoway'"=="" {
        local horizontal
        local labels
    }
    local nms
    local j 0
    while (`"`anything'"'!="") {
        gettoken el anything : anything, parse(" =")
        if `: list sizeof el'!=1 error 198
        confirm name `prefix'`el'
        local ++j
        local nms `nms' `prefix'`el'
        local el`j' = strlower("`el'")
        gettoken el : anything, parse(" =")
        if `"`el'"'!="=" continue
        gettoken el anything : anything, parse(" =") // skip "="
        gettoken el anything : anything, parse(" =")
        if `: list sizeof el'!=1 error 198
        local el`j' = strlower(`"`el'"')
    }
    local J `j'
    if !`J' {
        di as txt "(nothing to do; must specify at least one element to save)"
        exit
    }
    if `:list sizeof nms'!=`J' exit 198
    if "`:list dups nms'"!="" {
        di as error "names must be unique"
        exit 198
    }
    
    // collect results
    local els
    tempname D table tmp
    qui dstat, noheader cref `level' `eform' // make r(table) available
    mat `table' = r(table)
    mat `D' = e(b)' * . // initialize with missing column
    forv j=1/`J' {
        local l = strlen(`"`el`j''"')
        if `"`el`j''"'=="b" {
            local el`j'lab `"`e(title)'"'
            local el`j'lab: subinstr local el`j'lab "Survey: " ""
            local r = rownumb(`table', "b")
            if `r'<. mat `D' = `D', `table'[`r',1...]' * `rescale'
            else     mat `D' = `D', `D'[1...,1]
        }
        else if `"`el`j''"'=="se" {
            local el`j'lab "Standard error"
            local r = rownumb(`table', "se")
            if `r'<. mat `D' = `D', `table'[`r',1...]' * `rescale'
            else     mat `D' = `D', `D'[1...,1]
        }
        else if inlist(`"`el`j''"',"t","z") {
            local el`j'lab "`el`j'' statistic"
            local el`j' "t"
            local r = rownumb(`table', "t")
            if `r'>=. local r = rownumb(`table', "z")
            if `r'<. mat `D' = `D', `table'[`r',1...]'
            else     mat `D' = `D', `D'[1...,1]
        }
        else if `"`el`j''"'==substr("pvalue", 1, max(1,`l')) {
            local el`j' "pvalue"
            local el`j'lab "p-value"
            local r = rownumb(`table', "pvalue")
            if `r'<. mat `D' = `D', `table'[`r',1...]'
            else     mat `D' = `D', `D'[1...,1]
        }
        else if inlist(`"`el`j''"',"ll","lb","ci_l","ci_ll","ci_lb") {
            local el`j' "lb"
            local r = rownumb(`table', "ll")
            if `r'<. {
                local el`j'lab "`e(level)'% CI"
                if "`twoway'"=="" local el`j'lab "Lower bound of `el`j'lab'"
                mat `D' = `D', `table'[`r',1...]' * `rescale'
            }
            else mat `D' = `D', `D'[1...,1]
        }
        else if inlist(`"`el`j''"',"ul","ub","ci_u","ci_ul","ci_ub") {
            local el`j' "ub"
            local r = rownumb(`table', "ul")
            if `r'<. {
                local el`j'lab "`e(level)'% CI"
                if "`twoway'"=="" local el`j'lab "Upper bound  of `el`j'lab'"
                mat `D' = `D', `table'[`r',1...]' * `rescale'
            }
            else mat `D' = `D', `D'[1...,1]
        }
        else if substr(`"`el`j''"',1,2)=="at" {
            // check existence and type of e(at)
            capt confirm matrix e(at)
            if _rc==1 exit 1
            local hasat = _rc==0
            if `hasat' {
                mat `tmp' = e(at)'
                local atbin = colsof(`tmp')!=1
            }
            else {
                mat `D' = `D', `D'[1...,1]
                local atbin 0
            }
            // check el
            local el `"`el`j''"'
            if      inlist(`"`el'"',"at_l","at_ll","at_lb") local el "at_ll"
            else if inlist(`"`el'"',"at_u","at_ul","at_ub") local el "at_ul"
            else if inlist(`"`el'"',"at_m","at_mid")        local el "at_mid"
            else if !inlist(`"`el'"', "at", "at_h") {
                di as err `"{bf:`el'} is not a valid element"'
                exit 198
            }
            local el`j' `el'
            // labels
            if "`twoway'"!="" {
                if "`horizontal'"!="" {
                    local el`j'lab `"`e(title)'"'
                    local el`j'lab: subinstr local el`j'lab "Survey: " ""
                }
                else if inlist(`"`subcmd'"',"quantile","lorenz","pshare")/*
                    */ local el`j'lab "Population proportion"
                else local el`j'lab "" // set later
            }
            else {
                if      "`el'"=="at_ll"  local el`j'lab "Lower bin limit"
                else if "`el'"=="at_mid" local el`j'lab "Bin midpoint"
                else if "`el'"=="at_ul"  local el`j'lab "Upper bin limit"
                else if "`el'"=="at_h"   local el`j'lab "Bin width"
                else                     local el`j'lab "Evaluation point"
            }
            // collect results
            if `hasat' {
                if `atbin' {
                    if "`el'"=="at_ul" mat `tmp' = `tmp'[1...,1] + `tmp'[1...,3]
                    else if "`el'"=="at_mid" mat `tmp' = `tmp'[1...,2]
                    else if "`el'"=="at_h"   mat `tmp' = `tmp'[1...,3]
                    else /* at_ll */         mat `tmp' = `tmp'[1...,1]
                }
                else if "`el`j''"=="at_h"  mat `tmp' = `tmp' * 0
                mat `D' = `D', `tmp'
            }
        }
        else if `"`el`j''"'=="id" {
            local el`j'lab "Subpopulation ID"
            capt confirm matrix e(id)
            if _rc==1 exit 1
            if _rc mat `D' = `D', `D'[1...,1]
            else   mat `D' = `D', e(id)'
        }
        else if `"`el`j''"'=="omit" {
            local el`j'lab "Omitted coefficient flag"
            mat `D' = `D', e(omit)'
        }
        else if `"`el`j''"'=="sumw" {
            local el`j'lab "Sum of weights"
            mat `D' = `D', e(sumw)'
        }
        else if `"`el`j''"'==substr("nobs", 1, max(1,`l')) {
            local el`j' "nobs"
            local el`j'lab "Number of observations"
            mat `D' = `D', e(nobs)'
        }
        else if `"`el`j''"'=="name" {
            local el`j'lab "Coefficient name"
            mat `D' = `D', `D'[1...,1]
        }
        else if `"`el`j''"'==substr("eqname", 1, max(2,`l')) {
            local el`j' "eqname"
            local el`j'lab "Equation name"
            mat `D' = `D', `D'[1...,1]
        }
        else if inlist(`"`el`j''"',"eqnum","eqid") {
            local el`j' "eqid"
            local el`j'lab "Equation number"
            mata: st_matrix("`tmp'",/*
                */ runningsum(_mm_unique_tag(st_matrixrowstripe("`D'")[,1])))
            mat `D' = `D', `tmp'
        }
        else {
            di as err `"{bf:`el`j''} is not a valid element"'
            exit 198
        }
        local els `els' `el`j''
    }
    mat `D' = `D'[1...,2...] // remove initialization column
    
    // select and cref
    if `"`select'"'!="" {
        mata: ds_save_select("`D'", tokens(st_local("select")))
    }
    else if "`cref'"=="" {
        capt confirm matrix e(cref)
        if _rc==1 exit 1
        if !_rc {
            mata: ds_drop_cref("`D'", 1, 0)
        }
    }
    
    // store results
    mata: ds_save() // if wide: returns K, nms#, and eq#
    
    // returns
    if "`wide'"!="" {
        local nms
        return scalar k = `K'
        forv k = 1/`K' {
            return local names`k' "`nms`k''"
            return local eq`k' "`eq`k''"
            local nms `nms' `nms`k''
        }
    }
    return local names "`nms'"
    return local elements "`els'"
    return local wide "`wide'"

    // display
    describe `nms'
end

program Estimate
    gettoken subcmd 0 : 0
    if "`subcmd'"=="pw" {
        _Estimate_PW `0'
    }
    else {
        _Estimate `subcmd' `0'
    }
    c_local generate_quietly `generate_quietly'
    c_local markesample `markesample'
end

program _Estimate_PW, eclass
    // syntax
    gettoken STAT : 0, match(PAR)
    if `"`PAR'"'=="(" {
        gettoken STAT 0 : 0, match(PAR)
        local STAT = strtrim(`"`STAT'"')
    }
    else local STAT
    syntax varlist(numeric) [if] [in] [fw iw pw] [, ///
        Statistic(str) LOwer UPper DIAGonal Over(passthru) order(passthru) * ]
    if `"`STAT'"'!="" local statistic `"`STAT'"' // statistic() is old syntax
    if `"`statistic'"'=="" local statistic correlation // default
    else local statistic: subinstr local statistic " " "", all // remove blanks
    if `"`over'"'!="" {
        di as err "{bf:over()} not allowed for {bf:dstat pw}"
        exit 198
    }
    if `"`order'"'!="" {
        di as err "{bf:order()} not allowed for {bf:dstat pw}"
        exit 198
    }
    if "`lower'"!="" & "`upper'"!="" {
        local lower
        local upper
    }
    
    // compile argument for dstat summarize
    local varlist: list uniq varlist
    local n: list sizeof varlist
    if "`diagonal'"=="" {
        if `n'<2 {
            di as err "must specify at least 2 variables"
            exit 102
        }
    }
    local stat `"`statistic'"'
    if strpos(`"`stat'"',"(") {
        // parse stat(arg)
        local rest = substr(`"`stat'"', strpos(`"`stat'"',"(")+1, .)
        local rest `",`rest'"'
        local stat = substr(`"`stat'"', 1, strpos(`"`stat'"',"(")-1)
    }
    else if regexm(`"`stat'"', "[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$") {
        // parse stat#, where # is a (floating point) number 
        local rest = regexs(0)
        local stat = substr(`"`stat'"', 1,/*
            */strlen(`"`stat'"')-strlen(`"`rest'"'))
        local rest `",`rest')"'
    }
    else local rest ")"
    if `"`stat'"'=="" {
        di as err `"{bf:`statistic'} invalid; statistic not found"'
        exit 198
    }
    local stats
    local cnames
    forv i = 1/`n' {
        local v1: word `i' of `varlist'
        if      "`lower'"!="" local range `i'/`n'
        else if "`upper'"!="" local range 1/`i'
        else                  local range 1/`n'
        local istats
        forv j = `range' {
            if "`diagonal'"=="" {
                if `i'==`j' continue
            }
            local v2: word `j' of `varlist'
            local istats `istats' `stat'(`v2'`rest'
            local cnames `cnames' `v2'
        }
        if `"`istats'"'!="" {
            local stats `stats' (`istats') `v1'
        }
    }
    
    // run dstat summarize
    _Estimate summarize `stats' `if' `in' [`weight'`exp'], `options'
    c_local generate_quietly `generate_quietly'
    if `"`markesample'"'!="" {
        c_local markesample `markesample'
        exit
    }
    
    // relabel results
    tempname b
    matrix `b' = e(b)
    matrix coln `b' = `cnames'
    eret repost b=`b', rename
    foreach m in se nobs sumw omit {
        capt confirm matrix e(`m')
        if _rc==1 exit _rc
        if _rc continue
        mat `b' = e(`m')
        mat coln `b' = `cnames'
        eret matrix `m' = `b'
    }
    eret local subcmd "pw"
    eret local title  `"Pairwise `statistic'"'
    eret local stats  `"`statistic'"'
    eret scalar N_stats = 1
    eret local depvar "`varlist'"
    eret scalar N_vars = `n'
    eret local slist ""  // remove macro e(slist)
    eret local csinfo "" // remove macro e(csinfo)
    
    // post results as matrices
    tempname b B nobs Nobs se P
    matrix `b' = e(b)
    matrix `nobs' = e(nobs)
    matrix `B' = J(`n', `n', .)
    mat coln `B' = `varlist'
    mat rown `B' = `varlist'
    matrix `Nobs' = `B'
    local hasSE 1
    capt confirm matrix e(V)
    if _rc==1 exit 1
    if _rc {
        capt confirm matrix e(se)
        if _rc==1 exit 1
        if _rc local hasSE 0
        else matrix `se' = e(se)
    }
    else {
        matrix `se' = vecdiag(e(V))
        mata: st_replacematrix("`se'", sqrt(st_matrix("`se'")))
    }
    if `hasSE' {
        local df = e(df_r)
        matrix `P' = `B'
    }
    local k 0
    forv i = 1/`n' {
        if      "`lower'"!="" local range `i'/`n'
        else if "`upper'"!="" local range 1/`i'
        else                  local range 1/`n'
        forv j = `range' {
            if "`diagonal'"=="" {
                if `i'==`j' continue
            }
            local ++k
            matrix `B'[`j',`i'] = `b'[1,`k']
            matrix `Nobs'[`j',`i'] = `nobs'[1,`k']
            if `hasSE' {
                matrix `P'[`j',`i'] = 2 * cond(`df'<=2e+17,/*
                    */ ttail(`df', abs(`b'[1,`k']/`se'[1,`k'])), /*
                    */ 1-normal(abs(`b'[1,`k']/`se'[1,`k'])))
            }
        }
    }
    eret matrix B = `B'
    eret matrix Nobs = `Nobs'
    if `hasSE' {
        eret matrix P = `P'
    }
    
    // fix variable labels of influence functions
    if `"`e(generate)'"'!="" {
        local cnames: colfullnames e(b)
        foreach IF in `e(generate)' {
            gettoken cname cnames : cnames
            local lbl: var lab `IF'
            local lbl = substr(`"`lbl'"',1,strpos(`"`lbl'"',"_b[")-1) +/*
                */ `"_b[`cname']"'
            lab var `IF' `"`lbl'"'
        }
    }
end

program _Estimate, eclass
    // syntax
    gettoken subcmd 0 : 0
    local lhs varlist(numeric fv)
    if "`subcmd'"=="density" {
        local opts n(numlist int >=1 max=1) COMmon at(str)/*
            */ range(numlist max=2 missingok) tight ltight rtight /*
            */ UNConditional
    }
    else if "`subcmd'"=="histogram" {
        local opts n(passthru) COMmon at(str) /*
            */ range(numlist max=2 missingok) /*
            */ ep PROPortion PERcent FREQuency UNConditional
    }
    else if inlist("`subcmd'","cdf","ccdf") {
        local opts n(numlist int >=1 max=1) COMmon at(str) /*
            */ range(numlist max=2 missingok) /*
            */ mid FLoor DISCrete PERcent FREQuency IPolate UNConditional
    }
    else if "`subcmd'"=="proportion" {
        local opts NOCATegorical at(str) PERcent FREQuency /*
            */ range(numlist max=2 missingok) UNConditional
    }
    else if "`subcmd'"=="quantile" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist max=2 >=0 missingok)
    }
    else if "`subcmd'"=="lorenz" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist max=2 >=0 missingok) /*
            */ gap sum GENERALized ABSolute PERcent /*
            */ BYvar(varname numeric) Zvar(varname numeric) // zvar() is old syntax
    }
    else if "`subcmd'"=="pshare" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist max=2 >=0 missingok) /*
            */ PROPortion PERcent sum AVErage GENERALized /*
            */ BYvar(varname numeric) Zvar(varname numeric) // zvar() is old syntax
    }
    else if "`subcmd'"=="tip" {
        local opts n(numlist int >=1 max=1) at(str) /*
            */ range(numlist max=2 >=0 missingok) /*
            */ ABSolute PLine(passthru) PSTRong
    }
    else if "`subcmd'"=="summarize" {
        local lhs anything(id="varlist")
        local opts NOCLEAN /* undocumented; do not remove stat-var duplicates
            */ at(str) range(numlist max=2 >=0 missingok) CATegorical /*
            */ relax /*
            */ BYvar(varname) Zvar(varname) /* zvar() is old syntax
            */ PLine(passthru) PSTRong
    }
    else exit 499
    syntax `lhs' [if] [in] [fw iw pw/], [ ///
        NOVALues VFormat(str) NOCASEwise ///
        qdef(str) HDQuantile HDTrim HDTrim2(str) MQuantile MQOPTs(str) ///
        Over(str) TOTal TOTal2(str) BALance(str) order(str) ///
        vce(str) NOSE NOCOV COV Level(cilevel) ///
        Generate(passthru) rif(str) Replace ///
        noBWFIXed /// does nothing at this point
        markesample(name) /// undocumented: mark estimation sample and exit
        `opts' * ]
    if "`byvar'"=="" local byvar `zvar' // support for old syntax
    if "`hdquantile'"!="" { // support for old syntax
        if "`qdef'"=="" {
            if `"`hdtrim2'"'!=""   local qdef 10, trim(`hdtrim2')
            else if "`hdtrim'"!="" local qdef 10, trim
            else                   local qdef 10
        }
        local hdquantile
        local hdtrim
        local hdtrim2
    }
    else if "`mquantile'"!="" { // support for old syntax
        if "`qdef'"=="" {
            if `"`mqopts'"'!="" local qdef 11, `mqopts'
            else                local qdef 11
        }
        local mquantile
        local mqopts
    }
    Parse_qdef `qdef'
    Parse_over `over'
    if "`over_accumulate'"!="" local common common
    if "`over_contrast'"!=""   local common common
    if "`total'"!="" & `"`total2'"'=="" local total2 "pooled"
    Parse_total, `total2'
    if inlist("`total'","mean","wmean") local common common
    if `"`generate'"'!="" & `"`rif'"'!="" {
        di as err "{bf:generate()} and {bf:rif()} not both allowed"
        exit 198
    }
    Parse_order `order'
    if `"`vformat'"'!="" {
        confirm format `vformat'
    }
    else local vformat %9.0g
    if "`subcmd'"=="proportion" {
        if "`nocategorical'"=="" local categorical "categorical"
    }
    Parse_densityopts, `options'
    if "`subcmd'"=="summarize" Parse_at_sum `at' // returns atvar and at
    Parse_at "`subcmd'" "`categorical'" `"`n'"' `"`at'"' `"`range'"' "`ep'"
    if `"`range'"'!="" {
        if inlist("`subcmd'","quantile","lorenz","pshare","tip") {
            // confirm that number in range() <= 1
            foreach limit of local range {
                if `limit'>1 & `limit'<. {
                    di as err "range() invalid -- invalid numlist has"/*
                        */ " elements outside of allowed range"
                    exit 125
                }
            }
        }
    }
    if "`subcmd'"!="summarize" {
        Parse_varlist_unique `varlist'
    }
    if "`subcmd'"=="density" {
        if `"`at'`n'"'=="" local n 99
        if "`tight'"!="" {
            local ltight ltight
            local rtight rtight
        }
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
        local yvars `byvar'
    }
    else if "`subcmd'"=="pshare" {
        if `"`at'`n'"'=="" local n 20
        local tmp `proportion' `percent' `sum' `average' `generalized'
        if `:list sizeof tmp'>1 {
            di as err "only one of {bf:percent}, {bf:proportion}, {it:sum}," /*
                */ " {it:average}, and {bf:generalized} allowed"
            exit 198
        }
        local yvars `byvar'
    }
    else if "`subcmd'"=="tip" {
        if `"`at'`n'"'=="" local n 101
        if `"`pline'"'=="" {
            di as err "option {bf:pline()} is required"
            exit 198
        }
        Parse_pline, `pline' // returns pline, plvar
        local yvars `plvar'
    }
    else if "`subcmd'"=="summarize" {
        Parse_pline, `pline' // returns pline, plvar
        Parse_sum "`byvar'" "`pline'" "`plvar'" `anything' 
            // => varlist, vlist, slist, slist2, yvars, plvars, stats
        local N_stats: list sizeof stats
    }
    if "`cov'"!="" & "`nocov'"!="" {
        di as err "only one of {bf:cov} and {bf:nocov} allowed"
        exit 198
    }
    if `"`subcmd'"'!="summarize" & "`cov'"=="" {
        local nocov nocov
    }
    Parse_vce `vce'
    if `"`balance'"'!="" {
        if "`over'"=="" {
            di as err "{bf:balance()} requires {bf:over()}"
            exit 198
        }
        Parse_balance `balance'
        if "`bal_ref'"!="" & "`total'"=="pooled" {
            di as err "{bf:total(pooled)} not allowed with {bf:balance(, reference())}"
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
        if inlist("`subcmd'","density","histogram","cdf","ccdf") & /*
            */ "`common'"=="" {
            di as err "{bf:compact} not allowed with {bf:dstat `subcmd'}"/*
                */ " unless option {bf:common} has been specified"
            exit 198
        }
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
        if "`yvars'"!="" {
            markout `touse' `yvars', strok
        }
    }
    else marksample touse, novarlist
    if "`over'`atvar'"!="" {
        markout `touse' `over' `atvar'
    }
    if "`bal_varlist'"!="" {
        markout `touse' `bal_varlist'
    }
    if "`clustvar'"!="" {
        markout `touse' `clustvar', strok
    }
    if `"`markesample'"'!="" {
        rename `touse' `markesample'
        c_local markesample `markesample'
        exit
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
        mata: ds_slist_expand("`noclean'"!="") // returns varlist, slist, slist2
        local varlist: list uniq varlist
    }
    else {
        fvexpand `varlist' if `touse'
        local varlist `r(varlist)'
        if "`subcmd'"=="proportion" & "`nocategorical'"=="" {
            if `"`r(fvops)'"'=="true" {
                di as err "{bf:dstat proportion} does not allow " ///
                    "factor-variable operators unless option " ///
                    "{bf:nocategorical} is specified"
                exit 101
            }
        }
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
        local N_over0: list sizeof overlevels
        if `"`bal_method'"'!="" {
            if "`bal_ref'"!="" {
                if `:list bal_ref in overlevels'==0 {
                    di as err "{bf:balance()}: no observations in reference"/*
                        */ " distribution"
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
                di as err "{bf:over(, contrast())}: no observations in"/*
                    */ " reference subpopulation"
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
            local N_over: list sizeof overlevels
            if `N_over'==0 {
                di as err "{bf:over(, select())}: must select at least one existing group"
                exit 499
            }
            local over_select "`overlevels'"
        }
        else local N_over `N_over0'
        if "`over_contrast'"=="contrast" {
            if `"`total'"'!="" local over_contrast "total"
            else gettoken over_contrast : overlevels // use first subpop
        }
        if "`over_contrast'"!="" {
            if (`N_over' + ("`total'"!=""))<2 {
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
    }
    
    // estimate
    tempname b nobs sumw cref id omit IFtot AT BW _N _W /*HCR PGI*/
    mata: dstat()
    
    // process IFs
    if "`generate'"!="" {
        local coln: colfullnames `b'
        if "`compact'"!="" {
            local CIF
            local CIFtot
            local i 0
            local j = colsof(`b') / (`N_over' + ("`total'"!=""))
            foreach IF of local IFs {
                local ++i
                if `IFtot'[1,`i']!=0 {
                    di as err "{bf:compact} not supported for statistics" ///
                        " that are not normalized by the sample size" ///
                        " (frequencies, totals)"
                    exit 499
                }
                if `i'<=`j' {
                    local CIF `CIF' `IF'
                    continue
                }
                if `i' <= (`N_over'*`j') continue
                local CIFtot `CIFtot' `IF'
            }
            local i 0
            local j 0
            local gid0 ""
            local IFS: copy local IFs
            local IFs
            foreach IF of local IFS {
                local ++i
                gettoken nm coln : coln
                local gid = `id'[1, `i']
                if "`gid'"!="`gid0'" {
                    local ++j
                    if "`gid'"=="." { // total
                        local ifexp `touse'
                        local Wexp  `W'
                        local CIFs: copy local CIFtot
                    }
                    else {
                        local ifexp `touse' & `over'==`gid'
                        local Wexp  `_W'[1,`j']
                        local CIFs: copy local CIF
                    }
                    local gid0 `gid' 
                }
                gettoken cIF CIFs : CIFs
                if "`rif'"!="" {
                    if "`scaling'"=="total" {
                        qui replace `cIF' = `IF' + `b'[1,`i']/`Wexp' if `ifexp'
                    }
                    else {
                        qui replace `cIF' = `IF'*`Wexp' + `b'[1,`i'] if `ifexp'
                    }
                }
                else {
                    if "`scaling'"=="mean" {
                        qui replace `cIF' = `IF'*`Wexp' if `ifexp'
                    }
                    else if "`cIF'"!="`IF'" {
                        qui replace `cIF' = `IF' if `ifexp'
                    }
                }
                if `j'==1 | "`gid'"=="." {
                    mata: ds_fix_nm("nm", "`gid'", "`total'"!="")
                    if "`rif'"!="" lab var `cIF' "RIF of _b[`nm']"
                    else           lab var `cIF' "IF of _b[`nm']"
                    local IFs `IFs' `cIF'
                }
            }
        }
        else if "`rif'"!="" {
            local i 0
            foreach IF of local IFs {
                gettoken nm coln : coln
                local ++i
                if "`scaling'"=="total" {
                    qui replace `IF' = `IF' ///
                        + (`b'[1,`i'] - `IFtot'[1,`i']) / `W' if `touse'
                }
                else {
                    qui replace `IF' = `IF' * `W' ///
                        + (`b'[1,`i'] - `IFtot'[1,`i']) if `touse'
                }
                lab var `IF' "RIF of _b[`nm']"
            }
        }
        else {
            local i 0
            foreach IF of local IFs {
                if "`scaling'"=="mean" {
                    qui replace `IF' = `IF' * `W' if `touse'
                }
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
    eret local csinfo "`csinfo'"
    eret local predict "dstat predict"
    if "`nose'"=="" {
        eret local vcetype "`vcetype'"
        eret local vce "`vce'"
        if "`vce_normal'"=="" eret scalar df_r = `df_r'
        eret scalar vce_minus = `vce_minus'
        if "`vce'"=="cluster" {
            eret local clustvar "`clustvar'"
            eret scalar N_clust = `N_clust'
        }
        if "`SE'"!="" {
            eret matrix se = `SE'
        }
    }
    eret            scalar sum_w = `W' // renamed in v1.4.9
    eret historical scalar W     = `W'
    eret scalar k_eq = `k_eq'
    eret scalar k_eform = `k_eq'
    eret scalar k_omit = `k_omit'
    eret matrix _N = `_N'
    eret matrix _W = `_W'
    eret matrix nobs = `nobs'
    eret matrix sumw = `sumw'
    eret matrix omit = `omit'
    eret scalar qdef = `qdef'
    eret local qdef_trim "`qdef_TRIM'"
    eret local qdef_usmooth "`qdef_USMOOTH'"
    eret local qdef_cdf "`qdef_CDF'"
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
    eret local byvar "`byvar'"
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
        if "`byvar'"!="" {
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
    else if "`subcmd'"=="pshare" {
        if "`byvar'"!="" local title "Concentration shares"
        else             local title "Percentile shares"
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
    if "`subcmd'"=="summarize" {
        eret local slist `"`slist'"'
        eret local stats "`stats'"
        eret scalar N_stats = `N_stats'
        if `"`atvar'"'!="" {
            eret local novalues "`novalues'"
            eret local vformat "`vformat'"
            eret local atvar "`atvar'"
            eret matrix at = `AT'
            local title "Conditional statistics"
        }
        else local title "Summary statistics"
    }
    else {
        eret local novalues "`novalues'"
        eret local vformat "`vformat'"
        eret matrix at = `AT'
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

program Parse_qdef
    _parse comma qdef 0 : 0
    if `"`qdef'"'=="" local qdef 2
    if `: list sizeof qdef'>1 {
        di as err "only one {it:method} allowed"
        di as err "error in option {bf:qdef()}"
        exit 198
    }
    capt confirm number `qdef'
    if _rc==1 exit _rc
    if !_rc {
        capt n numlist `"`qdef'"', max(1) int range(>=0 <=16)
        if _rc==1 exit _rc
        if _rc {
            di as err "error in option {bf:qdef()}"
            exit 198
        }
        local qdef = r(numlist)
    }
    else {
        capt n _Parse_qdef, `qdef'
        if _rc==1 exit _rc
        if _rc {
            di as err "error in option {bf:qdef()}"
            exit 198
        }
    }
    syntax [, Trim Trim2(numlist max=1) USmooth(numlist max=1 <1)/*
        */ CDF CDF2(numlist max=1 >=0) ]
    if `"`trim2'"'!=""   local TRIM trim(`trim2')
    else if "`trim'"!="" local TRIM trim
    else                 local TRIM
    if `"`cdf2'"'!=""    local CDF cdf(`cdf2')
    else if "`cdf'"!=""  local CDF cdf
    else                 local CDF
    if "`usmooth'"!=""   local USMOOTH usmooth(`usmooth')
    else                 local USMOOTH
    if "`trim'"!="" & "`trim2'"=="" local trim2 0 // use 1/sqrt(n)
    else if "`trim2'"==""           local trim2 1 // untrimmed
    if "`cdf'"!="" & "`cdf2'"=="" local cdf2 .
    if "`cdf2'"!="" {
        if "`usmooth'"!="" {
            di as err "{bf:qdef()}: {bf:cdf()} and {bf:usmooth()} not both allowed"
            exit 198
        }
    }
    if "`usmooth'"=="" local usmooth .2 // default
    c_local qdef_TRIM    `TRIM'
    c_local qdef_CDF     `CDF'
    c_local qdef_USMOOTH `USMOOTH'
    c_local qdef         `qdef'
    c_local qdef_trim    `trim2'
    c_local qdef_cdf     `cdf2'
    c_local qdef_usmooth `usmooth'
end

program _Parse_qdef
    local opts HIgh INVcdf AVGinvcdf closest PARZen HAZen WEIBull GUMBel/*
        */ TUKey blom hd mid CALIfornia beard benard cooper GRINGorten
    syntax [, LOw `opts' ]
    if "`low'"!="" local invcdf invcdf
    local opts = strlower("`opts'")
    local j 0
    foreach opt of local opts {
        if "``opt''"!="" local qdef `j'
        local ++j
    }
    c_local qdef `qdef'
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

program Parse_total
    syntax [, Pooled Mean Wmean ]
    local total `pooled' `mean' `wmean'
    if `:list sizeof total'>1 {
        di as err "{bf:total()}: only one of {bf:pooled}, {bf:mean}, and"/*
            */ " {bf:wmean} allowed"
        exit 198
    }
    c_local total `total'
end

program Parse_order
    local order
    foreach o in `0' {
        local 0 `", `o'"'
        capt n syntax [, Over Variables Statistics ]
        if _rc==1 exit 1
        if _rc {
            di as err "error in option {bf:order()}"
            exit _rc
        }
        local order `order' `over' `variables' `statistics'
    }
    c_local order: list uniq order
end

program Parse_rif
    syntax [anything(name=rif)] [, SCAling(passthru) COMpact QUIetly ]
    c_local rif
    if `"`rif'"'=="" exit
    c_local generate generate(`rif', rif `scaling' `compact' `quietly')
end

program Parse_generate
    syntax [, generate(str) replace ]
    local 0 `"`generate'"'
    syntax [anything(name=generate)] [, rif SCAling(str) COMpact QUIetly ]
    if `"`generate'"'=="" {
        c_local generate
        exit
    }
    Parse_generate_scaling, `scaling'
    if "`scaling'"=="" {
        if "`rif'"!="" local scaling mean
        else           local scaling total
    }
    c_local scaling `scaling'
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

program Parse_generate_scaling
    syntax [, Total Mean ]
    local scaling `total' `mean'
    if `:list sizeof scaling'>1 {
        di as err "scaling(): only one of total and mean allowed"
        exit 198
    }
    c_local scaling `scaling'
end

program Parse_at_sum
    if `"`0'"'=="" exit
    gettoken atvar 0 : 0, parse("= ")
    capt n confirm numeric variable `atvar'
    if _rc==1 exit _rc
    if _rc {
        di as err "error in option {bf:at()}"
        exit _rc
    }
    if `"`0'"'!="" {
        gettoken eq 0 : 0, parse("= ")
        if `"`eq'"'!="=" {
            di as err "invalid syntax in {bf:at()}"
            exit 198
        }
        local 0 `", at(`0')"'
        syntax [, at(str) ]
    }
    c_local atvar `atvar'
    c_local at    `"`at'"'
end

program Parse_at // returns atmat="matrix" if at is matrix, else expands numlist
    args subcmd cat n at range ep
    if `"`at'"'=="" exit
    if "`subcmd'"!="summarize" {
        if `"`n'"'!="" {
            di as err "{bf:n()} and {bf:at()} not both allowed"
            exit 198
        }
        if `"`range'"'!="" {
            di as err "{bf:range()} and {bf:at()} not both allowed"
            exit 198
        }
        if `"`ep'"'!="" {
            di as err "{bf:ep} and {bf:at()} not both allowed"
            exit 198
        }
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
    else if "`subcmd'"=="pshare"    local args "numlist min=2 >=0 <=1 ascending"
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
    gettoken yvar  0 : 0
    gettoken pline 0 : 0
    gettoken plvar 0 : 0
    Parse_sum_tokenize `0'    // stats_#, vars_#, n
    mata: ds_parse_stats(`n') // stats, slist, slist2, yvars, plvars
    // check/collect variables
    local vlist
    local vrlist
    local space
    forv j=1/`n' {
        local 0 `"`vars_`j''"'
        syntax varlist(numeric fv)
        local vlist `"`vlist'`space'`"`varlist'"'"'
        local space " "
        local vrlist `vrlist' `varlist'
    }
    // returns
    c_local varlist: list uniq vrlist // unique list of variables
    c_local stats: list uniq stats    // list of unique stats (as specified)
    c_local vlist  `"`vlist'"'        // grouped variables
    c_local stats  `"`stats'"'        // list of unique stats (as specified)
    c_local slist  `"`slist'"'        // grouped stats (as specified)
    c_local slist2 `"`slist2'"'       // grouped stats (expanded)
    c_local yvars  `yvars'            // (also includes plvars)
    c_local plvars `plvars'
end

program Parse_sum_hasdens
    Parse_sum_tokenize `0'  // returns stats_#, vars_#, n
    mata: ds_parse_stats_hasdens(`n')
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

program Parse_vce
    // options
    _parse comma vce 0 : 0
    capt n syntax [, minus(int 1) NORMal ]
    if _rc==1 exit 1
    if _rc {
        di as err "error in option {bf:vce()}"
        exit _rc
    }
    c_local vce_minus `minus'
    c_local vce_normal `normal'
    // vce: none
    if `"`vce'"'=="none" {
        c_local nose "nose"
        c_local vce
        c_local vcetype
        c_local clustvar
        c_local vce_minus
        c_local vce_normal
        exit
    }
    // vce type
    if `"`vce'"'==substr("analytic", 1, strlen(`"`vce'"')) { // includes ""
        local vce "analytic"
    }
    else {
        gettoken vce arg : vce
        if `"`vce'"'==substr("cluster", 1, max(2,strlen(`"`vce'"'))) {
            local vce "cluster"
        }
        else {
            di as err `"vce(`vce'`arg') not allowed"'
            exit 198
        }
    }
    c_local vce `vce'
    // clustvar
    if "`vce'"=="cluster" {
        local 0 `"`arg'"'
        capt n syntax varname
        if _rc==1 exit _rc
        if _rc {
            di as err "error in option {bf:vce()}"
            exit 198
        }
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
    if "`napprox'"=="" local napprox 1024
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
    if "`bwmethod'"=="" {
        local bwmethod "dpi"
        local dpi2 2
    }
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
local DATA   ds_data
local Data   struct `DATA' scalar
local LEVEL  ds_overlevel
local Level  struct `LEVEL' scalar
local GRP    ds_grp
local Grp    class `GRP' scalar
local XTMP   ds_xtmp
local Xtmp   struct `XTMP' scalar
local YTMP   ds_ytmp
local Ytmp   struct `YTMP' scalar
local XYTMP  ds_xytmp
local XYtmp  struct `XYTMP' scalar
local PLTMP  ds_pltmp
local PLtmp  struct `PLTMP' scalar
local MQOPT  ds_mqopt
local MQopt  struct `MQOPT' scalar
local BAL    ds_bal
local Bal    struct `BAL' scalar
local STATS  ds_stats
local Stats  struct `STATS' scalar
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
local BoolR  real rowvector
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
// ds_stats_lib()
local f &ds_sum_
local p &_ds_parse_stats_

mata:
mata set matastrict on

// --------------------------------------------------------------------------
// parsing for -dstat summarize-
// --------------------------------------------------------------------------

struct `STATS' {
    `SM' stats              // row1: as specified; row2: fully expanded
    `SS' yvar, plvar, pline // defaults
    `SR' yvars, plvars      // yvars: all aux variables (including plvars)
}

void ds_parse_stats(`Int' n)
{
    `Int'   i
    `SS'    nm
    `T'     A
    `SC'    list
    `Stats' S
    `SM'    stats
    
    A = _ds_parse_stats_lib()
    list = sort(asarray_keys(A), 1)
    S.yvar = st_local("yvar")
    if (S.yvar!="") S.yvars = S.yvar
    S.pline = st_local("pline")
    S.plvar = st_local("plvar")
    if (S.plvar!="") {
        S.yvars = (S.yvars, S.plvar)
        S.plvars = S.plvar
    }
    stats = J(2, n, "")
    for (i=1;i<=n;i++) {
        nm = "stats_" + strofreal(i)
        _ds_parse_stats(A, list, S, _ds_parse_stats_tok(st_local(nm)))
        stats[1,i] = invtokens(S.stats[1,])
        stats[2,i] = invtokens(S.stats[2,])
    }
    st_local("stats",  invtokens(stats[1,]))
    stats = ("`" + `"""') :+ stats :+  (`"""' + "'")
    st_local("slist",  invtokens(stats[1,]))
    st_local("slist2", invtokens(stats[2,]))
    st_local("yvars",  invtokens(S.yvars))
    st_local("plvars", invtokens(S.plvars))
}

`T' _ds_parse_stats_lib()
{   // library of statistics and argument masks
    // see _ds_parse_stats_mask() for mask syntax
    `T' A
    
    A = asarray_create()
    // general
    asarray(A, "q"          , ".,0,100") // quantile at #
    asarray(A, "p"          , ".,0,100") // same as q
    asarray(A, "quantile"   , ".,0,100") // same as q
    asarray(A, "hdquantile" , ".,0,100") // Harrell&Davis (1982) quantile at #
    asarray(A, "mquantile"  , ".,0,100") // mid-quantile at #
    asarray(A, "d"          , ".")       // kernel density at #
    asarray(A, "density"    , ".")       // same as d
    asarray(A, "histogram"  , ".:.")     // histogram density within [#,#]
    asarray(A, "cdf"        , ".")       // cdf at #
    asarray(A, "mcdf"       , ".")       // mid cdf at #
    asarray(A, "fcdf"       , ".")       // floor cdf at #
    asarray(A, "ccdf"       , ".")       // complementary cdf at #
    asarray(A, "mccdf"      , ".")       // mid ccdf at #
    asarray(A, "fccdf"      , ".")       // floor ccdf at #
    asarray(A, "proportion" , ".;.z")    // proportion equal to # or within [#,#]
    asarray(A, "pct"        , ".;.z")    // percent equal to # or within [#,#]
    asarray(A, "f"          , ".z;.z")   // frequency: overall, equal to #, or within [#,#]
    asarray(A, "frequency"  , ".z;.z")   // same as f
    asarray(A, "count"      , ".z;.z")   // same as f
    asarray(A, "total"      , ".z;.z")   // overall total or total equal to # or within [#,#]
    asarray(A, "min"        , "")        //
    asarray(A, "max"        , "")        //
    asarray(A, "range"      , "")        //
    asarray(A, "midrange"   , "")        // (max+min)/2
    // location
    asarray(A, "mean"       , "")        // arithmetic mean
    asarray(A, "gmean"      , "")        // geometric mean
    asarray(A, "hmean"      , "")        // harmonic mean
    asarray(A, "trim"       , "25,0,50;.z,0,50") // trimmed mean
    asarray(A, "winsor"     , "25,0,50;.z,0,50") // winsorized mean
    asarray(A, "median"     , "")        // median
    asarray(A, "huber"      , "95,63.7,99.9") // huber m-estimate of location
    asarray(A, "biweight"   , "95,0.1,99.9")  // biweight m-estimate of location
    asarray(A, "hl"         , "")        // Hodges-Lehmann estimator
    // scale
    asarray(A, "sd"         , "1")       // standard deviation
    asarray(A, "variance"   , "1")       // variance
    asarray(A, "mse"        , "0;0")     // mean squared error
    asarray(A, "rmse"       , "0;0")     // root mean squared error
    asarray(A, "iqr"        , "25,0,100:75,0,100") // inter quantile range
    asarray(A, "iqrn"       , "25,0,100:75,0,100") // normalized inter quantile range
    asarray(A, "mad"        , "0;0")     // median (or mean) absolute deviation
    asarray(A, "madn"       , "0;0")     // normalized MAD
    asarray(A, "mae"        , "0;0")     // median (or mean) absolute error
    asarray(A, "maen"       , "0;0")     // normalized MAE
    asarray(A, "md"         , "")        // mean absolute pairwise difference
    asarray(A, "mdn"        , "")        // normalized mean absolute pairwise difference
    asarray(A, "mscale"     , "50,1,50") // m-estimate of scale
    asarray(A, "qn"         , "")        // Qn coefficient
    // skewness
    asarray(A, "skewness"   , "")        // classical skewness
    asarray(A, "qskew"      , "25,0,50") // quantile skewness measure
    asarray(A, "mc"         , "")        // medcouple
    // kurtosis
    asarray(A, "kurtosis"   , "")        // classical kurtosis
    asarray(A, "ekurtosis"  , "")        // excess kurtosis (kurtosis - 3)
    asarray(A, "qw"         , "25,0,50") // quantile tail weight
    asarray(A, "lqw"        , "25,0,50") // left quantile tail weight
    asarray(A, "rqw"        , "25,0,50") // right quantile tail weight
    asarray(A, "lmc"        , "")        // left medcouple tail weight
    asarray(A, "rmc"        , "")        // right medcouple tail weight
    // inequality
    asarray(A, "hoover"     , "")        // Hoover index
    asarray(A, "gini"       , "0")       // Gini coefficient
    asarray(A, "agini"      , "0")       // absolute Gini coefficient
    asarray(A, "mld"        , "")        // mean log deviation (MLD)
    asarray(A, "theil"      , "1")       // Theil index
    asarray(A, "ge"         , "1")       // generalized entropy
    asarray(A, "atkinson"   , "1,0,.")   // Atkinson inequality measure
    asarray(A, "cv"         , "1")       // coefficient of variation
    asarray(A, "lvar"       , "1")       // logarithmic variance
    asarray(A, "vlog"       , "1")       // variance of logarithm
    asarray(A, "sdlog"      , "1")      // standard deviation of logarithm
    asarray(A, "top"        , "10,0,100") // top share
    asarray(A, "bottom"     , "40,0,100") // bottom share
    asarray(A, "mid"        , "40,0,100:90,0,100") // mid share
    asarray(A, "palma"      , "")        // palma ratio
    asarray(A, "qratio"     , "10,0,100:90,0,100") // quantile ratio
    asarray(A, "sratio"     , ".z,0,100:.z,0,100;.z,0,100:.z,0,100") // percentile share ratio
    asarray(A, "lorenz"     , ".,0,100") // lorenz ordinate
    asarray(A, "tlorenz"    , ".,0,100") // total (sum) lorenz ordinate
    asarray(A, "glorenz"    , ".,0,100") // generalized lorenz ordinate
    asarray(A, "alorenz"    , ".,0,100") // absolute lorenz ordinate
    asarray(A, "elorenz"    , ".,0,100") // equality gap lorenz ordinate
    asarray(A, "share"      , ".,0,100;.,0,100") // percentile share (proportion)
    asarray(A, "pshare"     , ".,0,100;.,0,100") // percentile share (proportion)
    asarray(A, "dshare"     , ".,0,100;.,0,100") // percentile share (density)
    asarray(A, "tshare"     , ".,0,100;.,0,100") // percentile share (total/sum)
    asarray(A, "gshare"     , ".,0,100;.,0,100") // percentile share (generalized)
    asarray(A, "ashare"     , ".,0,100;.,0,100") // percentile share (average)
    // inequality decomposition
    asarray(A, "gw_gini"    , "bys;0")   // weighted avg of within-group Gini
    asarray(A, "b_gini"     , "bys;0")   // between Gini coefficient
    asarray(A, "gw_mld"     , "bys")     // weighted avg of within-group MLD
    asarray(A, "w_mld"      , "bys")     // within MLD
    asarray(A, "b_mld"      , "bys")     // between MLD
    asarray(A, "gw_theil"   , "bys;1")   // weighted avg of within-group Theil
    asarray(A, "w_theil"    , "bys;1")   // within Theil index
    asarray(A, "b_theil"    , "bys;1")   // between Theil index
    asarray(A, "gw_ge"      , "bys;1")   // weighted avg of within-group GE
    asarray(A, "w_ge"       , "bys;1")   // within generalized entropy
    asarray(A, "b_ge"       , "bys;1")   // between generalized entropy
    asarray(A, "gw_vlog"    , "bys;1")   // weighted avg of within-group vlog
    asarray(A, "w_vlog"     , "bys;1")   // within vlog
    asarray(A, "b_vlog"     , "bys;1")   // between vlog
    // concentration
    asarray(A, "gci"        , "by;0")    // Gini concentration index
    asarray(A, "aci"        , "by;0")    // absolute Gini concentration index
    asarray(A, "ccurve"     , ".,0,100;by") // lorenz concentration ordinate
    asarray(A, "tccurve"    , ".,0,100;by") // total concentration ordinate
    asarray(A, "gccurve"    , ".,0,100;by") // generalized concentration ordinate
    asarray(A, "accurve"    , ".,0,100;by") // absolute concentration ordinate
    asarray(A, "eccurve"    , ".,0,100;by") // equality gap concentration ordinate
    asarray(A, "cshare"     , ".,0,100;.,0,100;by") // concentration share (proportion)
    asarray(A, "pcshare"    , ".,0,100;.,0,100;by") // concentration share (proportion)
    asarray(A, "dcshare"    , ".,0,100;.,0,100;by") // concentration share as density
    asarray(A, "gcshare"    , ".,0,100;.,0,100;by") // generalized concentration share
    asarray(A, "tcshare"    , ".,0,100;.,0,100;by") // concentration share as total
    asarray(A, "acshare"    , ".,0,100;.,0,100;by") // concentration share as average
    // poverty
    asarray(A, "hcr"        , "pl")      // Head count ratio
    asarray(A, "pgap"       , "pl")      // poverty gap
    asarray(A, "apgap"      , "pl")      // absolute poverty gap
    asarray(A, "pgi"        , "pl")      // poverty gap index
    asarray(A, "apgi"       , "pl")      // absolute poverty gap index
    asarray(A, "fgt"        , "0,0,.;pl")    // FGT poverty measure
    asarray(A, "chu"        , "50,0,100;pl") // Clark-Hemming-Ulph poverty measure
    asarray(A, "watts"      , "pl")      // Watts index
    asarray(A, "sen"        , "pl")      // Sen poverty index
    asarray(A, "sst"        , "pl")      // Sen-Shorrocks-Thon poverty index
    asarray(A, "takayama"   , "pl")      // Takayama poverty index
    asarray(A, "tip"        , ".,0,100;pl") // TIP ordinate
    asarray(A, "atip"       , ".,0,100;pl") // absolute TIP ordinate
    // association
    asarray(A, "correlation", "by")      // correlation
    asarray(A, "rsquared"   , "by")      // R squared
    asarray(A, "slope"      , "by")      // slope of linear regression
    asarray(A, "b"          , "by")      // same as slope
    asarray(A, "cohend"     , "bys;2")   // Cohen's d
    asarray(A, "covariance" , "by;1")    // covariance
    asarray(A, "spearman"   , "by")      // spearman rank correlation
    asarray(A, "taua"       , "by;0")    // Kendall's tau-a
    asarray(A, "taub"       , "by;0")    // Kendall's tau-b
    asarray(A, "somersd"    , "by;0")    // Somers' D
    asarray(A, "gamma"      , "by;0")    // Goodman and Kruskal's gamma
    // categorical
    asarray(A, "hhi"        , "")        // Herfindahl–Hirschman index
    asarray(A, "hhin"       , "")        // normalized hhi
    asarray(A, "gimp"       , "")        // Gini impurity
    asarray(A, "gimpn"      , "")        // normalized  Gini impurity
    asarray(A, "entropy"    , "0,0,.")   // Shannon entropy
    asarray(A, "hill"       , "1,0,.")   // Hill number
    asarray(A, "renyi"      , "1,0,.")   // Rényi entropy
    asarray(A, "mindex"     , "bys;0,0,.") // mutual information
    asarray(A, "uc"         , "bys")     // uncertainty coefficient (symmetric)
    asarray(A, "ucl"        , "bys")     // uncertainty coefficient (left)
    asarray(A, "ucr"        , "bys")     // uncertainty coefficient (right)
    asarray(A, "cramersv"   , "bys;0,0,.") // Cramér's V
    asarray(A, "dissimilarity", "bys")   // (generalized) dissimilarity index
    asarray(A, "or"         , "by")      // odds ratio (for 2x2 table)
    asarray(A, "rr"         , "by")      // risk ratio (for 2x2 table)
    return(A)
}

`SR' _ds_parse_stats_tok(`SS' s)
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

void _ds_parse_stats(`T' A, `SC' list, `Stats' S, `SR' stats)
{
    `Int' k, n
    `SS'  s0, s, o
    
    n = length(stats)
    S.stats = J(2,n,"")
    for (k=1; k<=n; k++) {
        s0 = stats[k]
        o  = _ds_parse_stats_split(s0) // replaces s0
        s  = _ds_parse_stats_match(list, s0, o)
        _ds_parse_stats_k(S, k, s0, s, o, asarray(A, s))
    }
}

`SS' _ds_parse_stats_split(`SS' s)
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

`SS' _ds_parse_stats_match(`SC' list, `SS' s0, `SS' o)
{
    `SS'  s
    pragma unset s
    
    if (strtrim(s0)=="") _ds_parse_stats_err(s0+o, "missing name")
    if (_mm_strexpand(s, strlower(s0), list))
        _ds_parse_stats_err(s0+o, "statistic not found")
    return(s)
}

void _ds_parse_stats_err(`SS' s, | `SS' msg)
{
    if (args()<2) errprintf("{bf:%s} invalid\n", s)
    else errprintf("{bf:%s} invalid; %s\n", s, msg)
    exit(198)
}

void _ds_parse_stats_k(`Stats' S, `Int' k, `SS' s0, `SS' s, `SS' o0, `SS' mask)
{   // s0 name of statistic as specified
    // s expanded name
    // o arguments as specified (including parentheses)
    `Bool' br
    `Int'  i, n, j, nj
    `SS'   o, y
    `RS'   mi
    `SR'   args, opts
    `RM'   M
    
    o = subinstr(o0," ","")    // remove blanks
    if (substr(o,1,1)=="(") {  // remove ()
        o = substr(o, 2, strlen(o)-2)
        br = `TRUE'
    }
    else br = `FALSE'
    // statistic without arguments
    if (mask=="") { 
        if (o!="") _ds_parse_stats_err(s0+o0, "too many arguments")
        S.stats[,k] = s0 + _ds_parse_stats_brace(o, br) \ s
        return
    }
    M = _ds_parse_stats_mask(mask)
    n = rows(M)
    opts = J(1, n, "")
    args = tokens(o,",")
    j = length(args)
    args = select(args, args:!=",")
    nj = length(args)
    if (j & j!=(nj+nj-1)) _ds_parse_stats_err(s0+o0, 
                          "empty argument(s) not allowed")
    j = 1
    for (i=1;i<=n;i++) {
        // tie: argument required
        if (M[i,4]) {
            if (j==(nj+1)) _ds_parse_stats_err(s0+o0, "too few arguments")
        }
        mi = M[i,1]
        // by
        if (mi==.a | mi==.b) {
            if (j>nj) y = S.yvar
            else {
                if (strtoreal(args[j])<.) y = S.yvar 
                else {
                    y = _ds_parse_stats_getvar(args[j], s0, o0)
                    j++  // [move to next argument only of by is provided!]
                }
            }
            if (y=="") _ds_parse_stats_err(s0+o0, 
                       "{it:by} or {bf:by()} required")
            _ds_parse_stats_isnumvar(y, s0, o0, mi==.b)
            if (!anyof(S.yvars, y)) S.yvars = (S.yvars, y)
            opts[i] = "by" + strofreal(selectindex(S.yvars:==y))
            continue
        }
        // pline (use negative index if plvar)
        if (mi==.c) {
            if (j>nj) {
                if (S.plvar=="") {
                    if (S.pline=="") _ds_parse_stats_err(s0+o0,
                                    "{it:pline} or {bf:pline()} required")
                    opts[i] = "pl" + S.pline
                }
                else {
                    y = S.plvar
                    if (!anyof(S.yvars, y)) {
                        S.yvars  = (S.yvars, y)
                        S.plvars = (S.plvars, y)
                    }
                    opts[i] = "pl" + strofreal(-selectindex(S.yvars:==y))
                }
                j++
                continue
            }
            mi = strtoreal(args[j])
            if (mi<.) {
                if (mi<0) _ds_parse_stats_err(s0+o0, 
                          "poverty line must be positive")
                opts[i] = "pl" + args[j]
            }
            else {
                y = _ds_parse_stats_getvar(args[j], s0, o0)
                _ds_parse_stats_isnumvar(y, s0, o0)
                if (!anyof(S.yvars, y)) {
                    S.yvars  = (S.yvars, y)
                    S.plvars = (S.plvars, y)
                }
                opts[i] = "pl" + strofreal(-selectindex(S.yvars:==y))
            }
            j++
            continue
        }
        // numeric argument
        if (j>nj) {
            if (mi==.) _ds_parse_stats_err(s0+o0, "too few arguments")
            opts[i] = strofreal(mi) // use default value
            j++
            continue
        }
        mi = strtoreal(args[j])
        if (mi>=.) _ds_parse_stats_err(s0+o0, "numeric argument expected")
        if (M[i,2]<.) { // check min
            if (mi<M[i,2]) _ds_parse_stats_err(s0+o0, "argument out of range")
        }
        if (M[i,3]<.) { // check max
            if (mi>M[i,3]) _ds_parse_stats_err(s0+o0, "argument out of range")
        }
        opts[i] = args[j]
        j++
    }
    if (nj>=j) _ds_parse_stats_err(s0+o0, "too many arguments")
    S.stats[,k] = s0 + _ds_parse_stats_brace(o, br) \ 
                  s  + "(" + invtokens(opts, ",") + ")"
}

`SS' _ds_parse_stats_getvar(`SS' y, `SS' s0, `SS' o0)
{
    `Int' j
    
    j = _st_varindex(y, st_global("c(varabbrev)")=="on")
    if (j>=.) {
        _ds_parse_stats_err(s0+o0, sprintf("variable {bf:%s} not found", y))
    }
    return(st_varname(j))
}

void _ds_parse_stats_isnumvar(`SS' y, `SS' s0, `SS' o0, | `Bool' strok)
{
    if (strok==`TRUE')  return
    if (st_isnumvar(y)) return
    _ds_parse_stats_err(s0+o0, sprintf("string variables not allowed" +
            "; {bf:%s} is a string variable", y))
}

`RM' _ds_parse_stats_mask(`SS' mask)
{   //      mask = grp1;grp2;...      (grp can also just be a single arg)
    // with grp# = arg1:arg2:...      (group of tied arguments)
    // with arg# = value[,min[,max]]  (min=. mean no min)
    // with value = "by"   (optional byvar)  => will be coded as .a
    //            = "bys"  (strvar allowed)  => will be coded as .b
    //            = "pl"   (pl)              => will be coded as .c
    //            = .      (required numeric argument)
    //            = #      (optional argument; # serves as default)
    //            = .z     (optional argument; no default)
    `Int'  n, i, j
    `Bool' tie
    `SR'   args
    `RM'   M
    
    args = tokens(mask, ";:")
    n = length(args)
    M = J(n, 4, .) // cols: arg, min, max, tie
    j = tie = 0
    for (i=1;i<=n;i++) {
        if (args[i]==";") {
            tie = 0
            continue
        }
        if (args[i]==":") {
            tie = 1
            continue
        }
        M[++j,] = __ds_parse_stats_mask(args[i]), tie
    }
    if (j) return(M[|1,1 \ j,.|])
    return(J(0,4,.))
}

`RR' __ds_parse_stats_mask(`SS' arg)
{
    `Int' n
    `SR'  args
    
    if (arg=="by")  return((.a, ., .))    // byvar
    if (arg=="bys") return((.b, ., .))    // byvar (str ok)
    if (arg=="pl")  return((.c, ., .))    // pline
    args = tokens(subinstr(arg,","," ")) // #,#,# (assuming 0 to 3 elements)
    n = length(args)
    if (n==0) return((.,.,.))
    if (n==1) return((strtoreal(args),.,.))
    if (n==2) return((strtoreal(args),.))
              return(strtoreal(args)) 
}

`SS' _ds_parse_stats_brace(`SS' s, `Bool' br)
{
    if (br)                       return("("+s+")")
    if (s=="")                    return(s)
    if (!(regexm(s, "^[0-9]+$"))) return("("+s+")")
    return(s)
}

void ds_parse_stats_hasdens(`Int' n)
{
    `Int' i
    `T'   A
    `SC'  list
    
    A = _ds_parse_stats_lib()
    list = sort(asarray_keys(A), 1)
    for (i=1;i<=n;i++) {
        if (_ds_parse_stats_hasdens(list, 
            _ds_parse_stats_tok(st_local("stats_" + strofreal(i))))) {
            st_local("hasdens", "1")
            return
        }
    }
}

`Bool' _ds_parse_stats_hasdens(`SC' list, `SR' stats)
{
    `Int' k, n
    `SR'  dens
    `SS'  s0, s, o
    
    dens = ("d", "density")
    n = length(stats)
    for (k=1; k<=n; k++) {
        s0 = stats[k]
        o = _ds_parse_stats_split(s0) // replaces s0
        s = _ds_parse_stats_match(list, s0, o)
        if (anyof(dens, s)) return(1)
    }
    return(0)
}

void ds_slist_expand(`Bool' keepdup)
{
    `Int' i, n, rc
    `SR'  V, v
    `SM'  S

    V  = tokens(st_local("vlist"))
    S  = tokens(st_local("slist")) \ tokens(st_local("slist2"))
    n = length(V)
    for (i=1;i<=n;i++) {
        rc = _stata("fvexpand " + V[i] + " if \`touse'")
        if (rc) exit(rc)
        v = tokens(st_global("r(varlist)"))
        V[i]   = invtokens(mm_expand(v, 1, length(tokens(S[1,i])), 1))
        S[1,i] = invtokens(J(1,length(v),S[1,i]))
        S[2,i] = invtokens(J(1,length(v),S[2,i]))
    }
    V = invtokens(V)
    S = invtokens(S[1,]) \ invtokens(S[2,])
    S = _ds_slist_clean((tokens(V)', tokens(S[1])', tokens(S[2])'), keepdup)
    st_local("varlist", V)
    st_local("slist",   S[1]) // (stats) varname (stats) varname ...
    st_local("slist2",  S[2]) // (stats) (stats) ...
}

`SC' _ds_slist_clean(`SM' S,`Bool' keepdup)
{
    `Int' i, j, k, n

    // step 1: sort groups (keeping original order within groups)
    S = S[_ds_order_sgrp(S[,1]),]
    // step 2: remove duplicates (keeping original order)
    if (!keepdup) S = mm_uniqrows(S, 1)
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
        S[++k,2] = "("+invtokens(S[|j,2 \ i,2|]')+") " + S[i,1]
        S[k,3]   = "("+invtokens(S[|j,3 \ i,3|]')+")"
    }
    return(invtokens(S[|1,2 \ k,2|]') \ invtokens(S[|1,3 \ k,3|]'))
}

`RC' _ds_order_sgrp(`SC' S)
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
// computation of transformed CIs
// --------------------------------------------------------------------------

void ds_Get_CI(`SS' cimat, `RS' level, `SS' citype, `RC' scale)
{
    `RS'   p
    `RR'   b, df, se
    `RM'   CI
    `SM'   cstripe
    
    // obtain b and se
    b = st_matrix("e(b)")
    cstripe = st_matrixcolstripe("e(b)")
    se = sqrt(diagonal(st_matrix("e(V)")))'
    if (length(se)==0) se = st_matrix("e(se)")
    // obtain df
    df = .
    if (st_global("e(mi)")=="mi") {
        if      (st_matrix("e(df_mi)")!=J(0,0,.))   df = st_matrix("e(df_mi)")
        else if (st_numscalar("e(df_r)")!=J(0,0,.)) df = st_numscalar("e(df_r)")
    }
    else if (st_numscalar("e(df_r)")!=J(0,0,.))     df = st_numscalar("e(df_r)")
    // alpha / 2
    p = (100 - level)/200
    // compute ci
    if (citype=="normal") CI = _ds_Get_CI_normal(b, se, df, p)
    else {
        if (scale!=1) {
            b = b / scale
            se = se / scale
        }
        if      (citype=="logit")    CI = _ds_Get_CI_logit(b, se, df, p)
        else if (citype=="probit")   CI = _ds_Get_CI_probit(b, se, df, p)
        else if (citype=="atanh")    CI = _ds_Get_CI_atanh(b, se, df, p)
        else if (citype=="log")      CI = _ds_Get_CI_log(b, se, df, p)
        else if (citype=="agresti")  CI = _ds_Get_CI_agresti(b, se, df, p)
        else if (citype=="exact")    CI = _ds_Get_CI_exact(b, se, df, p)
        else if (citype=="jeffreys") CI = _ds_Get_CI_jeffreys(b, se, df, p)
        else if (citype=="wilson")   CI = _ds_Get_CI_wilson(b, se, df, p)
        else exit(499)
        if (scale!=1) CI = CI * scale
    }
    // return result
    st_matrix(cimat, CI)
    st_matrixcolstripe(cimat, cstripe)
}

`RM' _ds_Get_CI_normal(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' z
    `RM' CI
    
    z  = _ds_Get_CI_t(df, p)
    CI = b :- z:*se \ b :+ z:*se
    return(CI)
}

`RM' _ds_Get_CI_logit(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' z
    `RM' CI
    
    z  = _ds_Get_CI_t(df, p) :* se :/ (b:* (1 :- b))
    CI = invlogit(logit(b) :- z \ logit(b) :+ z)
    _ds_Get_CI_edit01(CI, b)
    return(CI)
}

`RM' _ds_Get_CI_probit(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' z
    `RM' CI
    
    z  = _ds_Get_CI_t(df, p) :* se :/ normalden(invnormal(b))
    CI = normal(invnormal(b) :- z \ invnormal(b) :+ z)
    _ds_Get_CI_edit01(CI, b)
    return(CI)
}

`RM' _ds_Get_CI_atanh(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' z
    `RM' CI
    
    z  = _ds_Get_CI_t(df, p) :* se :/ (1 :- b:^2)
    CI = tanh(atanh(b) :- z \ atanh(b) :+ z)
    if (hasmissing(CI)) {
        _ds_Get_CI_editif(CI, b, -1)
        _ds_Get_CI_editif(CI, b,  1)
    }
    return(CI)
}

`RM' _ds_Get_CI_log(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' z
    `RM' CI
    
    z  = _ds_Get_CI_t(df, p) :* se :/ b
    CI = exp(ln(b) :- z \ ln(b) :+ z)
    if (hasmissing(CI)) _ds_Get_CI_editif(CI, b, 0)
    return(CI)
}

`RM' _ds_Get_CI_agresti(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' n, k, pr, z
    `RM' CI
    
    n  = _ds_Get_CI_n(b, se, df, p)
    k  = n :* b
    z  = invnormal(1-p)
    n  = n :+ z^2
    k  = k :+ z^2/2
    pr = k :/ n
    z  = z * sqrt((pr :* (1 :- pr)) :/ n)
    CI = pr - z \ pr + z
    _ds_Get_CI_edit01(CI, b)
    return(CI)
}

`RM' _ds_Get_CI_exact(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' n, k, v1, v2, v3, v4, Fa, Fb
    `RM' CI
    
    n  = _ds_Get_CI_n(b, se, df, p)
    k  = n :* b
    v1 = 2 * k
    v2 = 2 * (n :- k :+ 1)
    v3 = 2 * (k :+ 1)
    v4 = 2 * (n - k)
    Fa = v1 :* invF(v1, v2, p)
    Fb = v3 :* invF(v3, v4, 1-p)
    CI = Fa :/ (v2 :+ Fa) \ Fb :/ (v4 :+ Fb)
    _ds_Get_CI_edit01(CI, b)
    return(CI)
}

`RM' _ds_Get_CI_jeffreys(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' n, k, a1, b1
    `RM' CI
    
    n  = _ds_Get_CI_n(b, se, df, p)
    k  = n :* b
    a1 = k :+ 0.5
    b1 = n :- k :+ 0.5
    CI = invibeta(a1, b1, p) \ invibetatail(a1, b1, p)
    _ds_Get_CI_edit01(CI, b)
    return(CI)
}

`RM' _ds_Get_CI_wilson(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RS' z
    `RR' n, n1, n2, d
    `RM' CI
    
    n  = _ds_Get_CI_n(b, se, df, p)
    z  = invnormal(1-p)
    n1 = b + z^2 :/ (2 :* n)
    n2 = z * sqrt((b :* (1 :- b)) :/ n :+ (z :/ (2 :* n)):^2)
    d  = 1 :+ z^2 :/ n
    CI = (n1 - n2) :/ d \ (n1 + n2) :/ d
    _ds_Get_CI_edit01(CI, b)
    return(CI)
}

`RR' _ds_Get_CI_t(`RV' df, `RS' p)
{
    `Int' i
    `RR'  z
    
    i = length(df)
    if (i==1) {
        return(df>2e17 ? invnormal(1-p) : invttail(df, p))
    }
    z = J(1, i, .)
    for (; i; i--) {
        z[i] = df[i]>2e17 ? invnormal(1-p) : invttail(df[i], p)
    }
    return(z)
}

`RR' _ds_Get_CI_n(`RR' b, `RR' se, `RV' df, `RS' p)
{
    `RR' n
    
    if      (st_global("e(wtype)")=="fweight") n = st_matrix("e(sumw)")
    else if (st_global("e(wtype)")=="iweight") n = st_matrix("e(sumw)")
    else                                       n = st_matrix("e(nobs)")
    if (st_global("e(vce)")=="analytic") {
        if (st_global("e(wtype)")!="pweight") return(n)
    }
    n = (b:* (1 :- b)) :/ se:^2
    n = n :* (invnormal(1-p) :/ _ds_Get_CI_t(df, p)):^2
    return(n)
}

void _ds_Get_CI_edit01(`RM' CI, `RR' b)
{
    if (hasmissing(CI)) {
        _ds_Get_CI_editif(CI, b, 0)
        _ds_Get_CI_editif(CI, b, 1)
    }
}

void _ds_Get_CI_editif(`RM' CI, `RR' b, `RS' v)
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

void ds_fix_nm(`SS' nm, `SS' id, `Bool' total)
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

void ds_drop_cref(`SS' m, `Bool' r, `Bool' c)
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

void ds_graph_swap(`Int' sub, `Int' overeq)
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
    if (overeq) {
        if ((sub==2 & overeq==2) | (sub!=2 & overeq==1)) {
            S[,2] = mm_cond(S[,2]:!="total", 
                S[,2] :+ ("."+ st_global("e(over)")), S[,2])
            st_local("overeq", "0")
        }
    }
    // store
    st_matrixcolstripe(st_local("B"), S)
    if (st_local("CI")!="") st_matrixcolstripe(st_local("CI"), S)
}

void ds_graph_eqsplit()
{
    `SR'   eqs
    `IntR' p
    
    eqs = tokens(st_local("eqs"))
    p   = strpos(eqs, "~")
    st_local("subeqs", invtokens(substr(eqs, p:+1, .)))
    st_local("eqs", invtokens(substr(eqs, 1, p:-1)))
}

void ds_graph_droplast(`SR' M)
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

void ds_graph_select(`SS' list, `SS' select)
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
// helper functions for save
// --------------------------------------------------------------------------

void ds_save()
{
    `Bool' replace, wide, hor
    `Int'  n, N, j, J, k, K, a, b
    `IntC' p
    `SS'   dv, over, subpop, var, atvar, stat, stat0
    `SR'   els, nms, lbls, csinfo
    `SM'   cstripe, OVER, TMP, NMS
    `RM'   D
    pragma unset subpop
    pragma unset var
    pragma unset stat
    
    // setup
    replace = st_local("replace")!=""
    wide    = st_local("wide")!=""
    els     = tokens(st_local("els"))
    nms     = tokens(st_local("nms"))
    dv      = st_global("e(depvar)")
    atvar   = st_global("e(atvar)")
    stat0   = st_numscalar("e(N_stats)")==1 ? st_global("e(stats)") : ""
    if (length(tokens(dv))!=1) dv = "" // omit if multiple vars
    D = st_matrix(st_local("D"))
    cstripe = st_matrixrowstripe(st_local("D"))
    if (wide) {
        csinfo = tokens(st_global("e(csinfo)"))
        over = st_global("e(over)")
        if (over!="") {
            OVER = tokens(st_global("e(over_namelist)"))',
                   tokens(st_global("e(over_labels)"))'
            if (OVER[,1]==OVER[,2]) OVER = OVER[,1]
        }
        p = selectindex(_mm_unique_tag(cstripe[,1], 1))
        K = rows(p)
        st_local("K", strofreal(K))
        for (k=1;k<=K;k++) st_local("eq"+strofreal(k), cstripe[p[k],1])
    }
    else K = 1
    hor  = st_local("horizontal")!=""
    lbls = tokens(st_local("labels"))
    if (length(lbls)<K) lbls = lbls, J(1, K-length(lbls), "")
    
    // check names
    J = length(nms)
    if (wide) {
        NMS = TMP = J(K, J, "")
        for (k=1;k<=K;k++) {
            NMS[k,] = nms :+ strofreal(k)
            st_local("nms" + strofreal(k), invtokens(NMS[k,]))
            _ds_save_confirm("name", NMS[k,])
            TMP[k,] = st_tempname(J)
        }
        for (k=1;k<=K;k++) {
            if (!replace) _ds_save_confirm("new variable", NMS[k,])
        }
    }
    else {
        NMS = nms
        TMP = st_tempname(J)
        if (!replace) _ds_save_confirm("new variable", NMS)
    }
    
    // set number of obs
    if (wide) n = max(mm_diff(0\p))
    else      n = rows(D)
    N = st_nobs()
    if (N<n) {
        st_addobs(n-N)
        printf("{txt}(%g observations added)\n", n-N)
    }
    
    // generate variables (using tempnames)
    if (wide) {
        b = 0
        for (k=1;k<=K;k++) {
            a = b + 1; b = p[k]
            _ds_save_parse_eq(cstripe[b,1], csinfo, over, OVER, subpop, var,
                stat)
            if (var=="") var = dv
            if (stat=="") stat = stat0
            _ds_save(D[|a,1\b,.|], cstripe[|a,1\b,2|], TMP[k,], els,
                subpop, var, atvar!="" ? atvar : var, stat, hor, lbls[k])
        }
    }
    else _ds_save(D, cstripe, TMP, els, "", dv, atvar!="" ? atvar : dv, stat0,
        hor, lbls[1])
    
    // rename variables
    for (k=1;k<=K;k++) {
        for (j=1;j<=J;j++) {
            if (_st_varindex(NMS[k,j])<.) st_dropvar(NMS[k,j])
            st_varrename(TMP[k,j], NMS[k,j])
        }
    }
}

void _ds_save(`RM' D, `SM' cstripe, `SR' nms, `SR' els, `SS' subpop, `SS' var,
    `SS' atvar, `SS' stat, `Bool' hor, `SS' lbl0)
{
    `Int' n, j, J, l
    `SS'  lbl
    
    n = rows(D)
    J = length(els)
    for (j=1;j<=J;j++) {
        if (els[j]=="name") {
            l = min((2045,max(1\strlen(cstripe[,2]))))
            st_sstore((1,n), st_addvar(l, nms[j]), cstripe[,2])
        }
        else if (els[j]=="eqname") {
            l = min((2045,max(1\strlen(cstripe[,1]))))
            st_sstore((1,n), st_addvar(l, nms[j]), cstripe[,1])
        }
        else st_store((1,n), st_addvar("double", nms[j]), D[,j])
        lbl = st_local("el"+strofreal(j)+"lab")
        if (hor ? substr(els[j],1,2)=="at" : els[j]=="b") {
            if (lbl0!="") lbl = lbl0
            else {
                if (stat!="")   lbl = stat
                if (subpop!="") lbl = subpop + ": " + lbl
                if (var!="")    lbl = lbl + " of " + var
            }
        }
        else if (substr(els[j],1,2)=="at") {
            if (lbl=="") {
                if (atvar!="") {
                    if (_st_varindex(var)) lbl = st_varlabel(atvar)
                    if (lbl=="") lbl = atvar
                }
                else lbl = "Evaluation point"
            }
        }
        else if (hor ? 0 : (els[j]=="lb" | els[j]=="ub")) {
            if (lbl0!="") lbl = lbl0
        }
        st_varlabel(nms[j], lbl)
    }
}

void _ds_save_confirm(`SS' spec, `SR' nms)
{
    `Int' j, J, rc
    
    J = length(nms)
    for (j=1;j<=J;j++) {
        rc = _stata("confirm " + spec + " " + nms[j])
        if (rc) exit(rc)
    }
}

void _ds_save_parse_eq(`SS' eq, `SR' csinfo, `SS' over, `SM' OVER, `SS' subpop,
    `SS' var, `SS' stat)
{   // set subpop, var, and stat
    `Int' i, l
    `SS'  o
    `SR'  EQ
    
    subpop = var = stat = ""
    if (eq=="") return
    EQ = tokens(eq, "~")
    EQ = select(EQ, EQ:!="~")
    for (i = length(EQ);i;i--) {
        if (csinfo[i]=="over") {
            o = EQ[i]
            if (o=="total") subpop = o
            else if (cols(OVER)==2) {
                l = selectindex(OVER[,1]:==o)
                if (length(l)==1) subpop = OVER[l,2]
                else              subpop = over + "=" + o
            }
            else subpop = over + "=" + o
        }
        else if (csinfo[i]=="variables")      var  = EQ[i]
        else /*if (scinfo[i]=="statistics")*/ stat = EQ[i]
    }
}

void ds_save_select(`SS' d, `SR' sel)
{
    `Int'  j
    `IntC' p
    `SM'   rstripe
    `RM'   D
    
    D = st_matrix(d)
    rstripe = st_matrixrowstripe(d)
    p = J(rows(D),1,0)
    for (j=length(sel);j;j--) p = p :| strmatch(rstripe[,1], sel[j])
    p = selectindex(p)
    if (!length(p)) {
        errprintf("select(): no match found; specified selection is empty\n")
        exit(111)
    }
    st_matrix(d, D[p,])
    st_matrixrowstripe(d, rstripe[p,])
}

// --------------------------------------------------------------------------
// other helper functions
// --------------------------------------------------------------------------

`RC' ds_invp(`Int' N, `IntC' p, `RC' x, | `RS' x0)
{
    `RC' y
    
    y = J(N, 1, x0)
    y[p] = x
    return(y)
}

`RS' ds_mean(`RC' X, `RC' w, | `RS' W)
{   // faster than mean()
    if (rows(w)==1) return(quadsum(X) / rows(X)) // assuming sum(w)!=0
    if (args()<3) W = quadsum(w)
    return(quadsum(w :* X) / W)
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

struct `LEVEL' {
    `Int'   id         // value of current level
    `IntC'  p          // observations of current level
    `Int'   N          // number of obs in current level
    `RS'    W          // sum of weights in current level
    `RC'    w          // total weight (including balancing)
    `RC'    wb         // total weight / base weight
    `RC'    wc         // total weight - "pooled" component
    `RM'    Z          // current level's balancing variables
    `RM'    IFZ        // current level's balancing IFs
    `IntC'  p1         // observations of reference level
    `RM'    IFZ1       // reference levels's balancing IFs
}

struct `MQOPT' {
    `Bool'  cdf       // sparsity function: 1 cdf-based; 0 density-based
    `RS'    bw        // smoothing bandwidth for cdf-based sparsity
    `RS'    us        // undersmoothing for density-based sparsity
}

struct `XTMP' {
    `RC'    X          // values of X
    `IntC'  p          // sort order of X
    `RC'    Xs         // sorted X
    `RC'    ws         // weights sorted by X
    `RS'    mean       // mean of X
    `RS'    Neff       // effective sample size
    `RC'    hdq_F      // hdquantile: CDF at unique values
    `RC'    hdq_dx     // hdquantile: differenced unique X
    `IntC'  hdq_p      // hdquantile: permutation vector to expand results
    `RC'    mq_x       // mid-quantiles: unique values of X
    `RC'    mq_F       // mid-quantiles: mid CDF at unique values
    `RC'    mq_sp      // mid-quantiles: sparsity function at unique values
    `RS'    mq_bw      // mid-quantiles: smoothing bandwidth for sparsity
    `T'     mq_S       // mid-quantiles: density estimation object
    `RS'    mq_c       // mid-quantiles: density scaling factor
    `RC'    Xc, Wc     // running sums for Lorenz ordinates
    `IntC'  tag        // tag unique levels in X
    `RC'    pr         // probabilities of levels in X 
    `RS'    cd_K, cd_T // concordant/discordant pairs
    `RC'    cd_k, cd_t // concordant/discordant pairs
}

struct `YTMP' {
    `Int'   y          // index of Y
    `RC'    Y          // values of Y
    `IntC'  p          // sort order of Y
    `RC'    Ys         // sorted values of Y
    `IntC'  tag        // tag unique levels in Y
    `RC'    pr         // probabilities of levels in Y 
    `RS'    cd_U       // concordant/discordant pairs
    `RC'    cd_u       // concordant/discordant pairs
}

struct `XYTMP' {
    `IntC'  p          // sort order of (X,Y)
    `RC'    Xs_y, Ys_x // X (Y) sorted by order of Y (X)
    `RC'    ws_y       // w sorted by oder of Y
    `RC'    Xc_y, Wc_y // running sums by order of Y for ccurve ordinates
    `RC'    EX, EXat   // E(X|Y=q) for ccurve IFs
    `IntC'  tag        // tag unique levels in (X,Y)
    `RC'    pr         // probabilities of levels in (X,Y)
    `T'     GW         // asarray() containing Y-group indices (sorted by X)
    `Bool'  cd_fast    // concordant/discordant pairs: type of algorithm
    `RS'    cd_S, cd_V // concordant/discordant pairs
    `RC'    cd_s, cd_v // concordant/discordant pairs
}

struct `PLTMP' {
    `Int'   plvar      // index of plvar
    `RC'    pl         // scalar pline or values of plvar
    `BoolC' poor       // 1 poor, 0 non-poor
}

class `GRP' {
    public:
    `Int'   N          // number of obs in current estimation set
    `RS'    W          // sum of weights in current estimation set
    `Int'   wtype      // weights: 0 none, 1 fw, 2 iw, 3 pw
    `Bool'  pstrong    // use strong poverty definition
    `MQopt' mqopt      // options for mid-quantiles
    `T'     p()        // current observations
    `IntC'  pp         // valid observation within current level (if nocw)
    `T'     w()        // total weight (including balancing)
    `RC'    wb         // adjusted L.wb (only if rows(pp)>0)
    `RC'    wc         // adjusted L.wc (only if rows(pp)>0)
    `RS'    Neff()     // effective sample size
    `Bool'  y_is_x     // Y is the same variable as X and Y are the same variable
    `Int'   j          // index of current X variable
    void    Xset()     // set xtmp and clear xytmp amd pltmp.poor
    void    Yset()     // set ytmp and clear xytmp
    void    PLset()    // set pltmp
    `RC'    X(), Y()   // retrieve Y
    `IntC'  pX(), pY(), pXY() // sort order of X, Y, or (X,Y)
    `RC'    Xs(), Ys()   // sorted X or Y
    `RC'    ws(), wsY()  // w sorted by X or by Y
    `RC'    XsY(), YsX() // X sorted by Y, Y sorted by X
    `RS'    mean()     // mean of X
    `BoolC' poor()     // 1 = poor, 0 = else
    `RC'    pl()       // poverty line
    `RC'    hdq_F()    // ECDF at unique values
    `RC'    hdq_dx()   // differenced unique X
    `IntC'  hdq_p()    // permutation vector to expand results
    `Int'   mq_j       // current position in mq_F()
    `RC'    mq_x()     // unique values of X
    `RC'    mq_F()     // mid ECDF at unique values
    `RS'    mq_s()     // sparsity function
    `RC'    Xc(), Wc(), XcY(), WcY() // running sums for Lorenz ordinates
    `RC'    EX(), EXat()             // E(X|Y=q) for ccurve IFs
    `IntC'  tagX(), tagY(), tagXY()  // tag unique levels in X, Y, (X,Y)
    `RC'    prX(), prY(), prXY()     // probabilities of levels in X, Y, (X,Y)
    `RS'    gw_K()          // number of groups in Y
    `IntC'  gw_p()          // (sorted) indices of group k
    `RC'    gw_X(), gw_w()  // X and w of group k
    `RS'    gw_W()          // size of group k
    void    cd_fast()       // set type of CD algorithm and reset results if needed
    `RS'    cd_K(), cd_S(), cd_T(), cd_U(), cd_V()
    `RC'    cd_k(), cd_s(), cd_t(), cd_u(), cd_v()
    private:
    `PS'    p          // pointer to current observations
    `PS'    w          // pointer to total weight (including balancing)
    void    gw_build() // build index of Y-groups
    `Xtmp'  xtmp       // struct holding temporary results related to X
    `Ytmp'  ytmp       // struct holding temporary results related to Y
    `XYtmp' xytmp      // struct holding temporary results related to X x Y
    `PLtmp' pltmp      // struct holding temporary results related to pline
}

`T' `GRP'::p(| `IntC' p0)
{
    if (!args()) return(*p)
    p = &p0
}

`T' `GRP'::w(| `RC' w0)
{
    if (!args()) return(*w)
    w = &w0
}

`RS' `GRP'::Neff()
{
    if (xtmp.Neff<.) return(xtmp.Neff)
    if (wtype<=1) xtmp.Neff = W                     // no weights or fweights
    else          xtmp.Neff = W^2 / quadsum(w():^2) // pweights or iweights
    return(xtmp.Neff)
}

void `GRP'::Xset(`Int' x, `RC' X)
{
    xtmp  = `XTMP'()
    xytmp = `XYTMP'()
    pltmp.poor = J(0,1,.)
    mq_j = 1 // (for mid-quantiles)
    j = x
    xtmp.X = X
}

void `GRP'::Yset(`Int' y, `RC' Y)
{
    if (ytmp.y==y) return // still same y
    if (ytmp.y<.) {
        ytmp  = `YTMP'()
        xytmp = `XYTMP'()
    }
    ytmp.y = y
    ytmp.Y = Y
}

void `GRP'::PLset(`Int' pl, | `RC' PL)
{
    if (args()==1) {
        if (pltmp.plvar>=.) {
            if (pltmp.plvar==pl) return // still same pline
        }
        pltmp = `PLTMP'()
        pltmp.pl = pl
        return
    }
    if (pltmp.plvar==pl) return // still same plvar
    pltmp = `PLTMP'()
    pltmp.plvar = pl
    pltmp.pl = PL
}

`RC' `GRP'::X() return(xtmp.X)

`RC' `GRP'::Y() return(ytmp.Y)

`IntC' `GRP'::pX()
{
    if (rows(xtmp.p)) return(xtmp.p)
    xtmp.p = mm_order(xtmp.X,1,1) // stable sort
    return(xtmp.p)
}

`IntC' `GRP'::pY()
{
    if (rows(ytmp.p)) return(ytmp.p)
    ytmp.p = mm_order(ytmp.Y,1,1) // stable sort
    return(ytmp.p)
}

`IntC' `GRP'::pXY()
{
    if (rows(xytmp.p)) return(xytmp.p)
    xytmp.p = mm_order((xtmp.X,ytmp.Y),.,1) // stable sort
    return(xytmp.p)
}

`RC' `GRP'::Xs()
{
    if (rows(xtmp.Xs)) return(xtmp.Xs)
    xtmp.Xs = xtmp.X[pX()]
    return(xtmp.Xs)
}

`RC' `GRP'::Ys()
{
    if (rows(ytmp.Ys)) return(ytmp.Ys)
    ytmp.Ys = ytmp.Y[pY()]
    return(ytmp.Ys)
}

`RC' `GRP'::ws()
{
    if (rows(xtmp.ws)) return(xtmp.ws)
    if (rows(w())==1) xtmp.ws = w()
    else              xtmp.ws = w()[pX()]
    return(xtmp.ws)
}

`RC' `GRP'::wsY()
{
    if (rows(xytmp.ws_y)) return(xytmp.ws_y)
    if (rows(w())==1) xytmp.ws_y = w()
    else              xytmp.ws_y = w()[pY()]
    return(xytmp.ws_y)
}

`RC' `GRP'::XsY()
{
    if (rows(xytmp.Xs_y)) return(xytmp.Xs_y)
    xytmp.Xs_y = xtmp.X[pY()]
    return(xytmp.Xs_y)
}

`RC' `GRP'::YsX()
{
    if (rows(xytmp.Ys_x)) return(xytmp.Ys_x)
    xytmp.Ys_x = ytmp.Y[pX()]
    return(xytmp.Ys_x)
}

`RS' `GRP'::mean()
{
    if (xtmp.mean>=.) xtmp.mean = ds_mean(xtmp.X, w(), W)
    return(xtmp.mean)
}

`BoolC' `GRP'::poor()
{
    if (rows(pltmp.poor)) return(pltmp.poor)
    pltmp.poor = pstrong ? xtmp.X:<=pltmp.pl : xtmp.X:<pltmp.pl
    return(pltmp.poor)
}

`RC' `GRP'::pl() return(pltmp.pl)

`RC' `GRP'::hdq_F()
{
    if (rows(xtmp.hdq_F)) return(xtmp.hdq_F)
    _ds_hdq_cdf(Xs(), ws(), xtmp.hdq_F, xtmp.hdq_dx)
    return(xtmp.hdq_F)
}

`RC' `GRP'::hdq_dx()
{
    if (rows(xtmp.hdq_dx)) return(xtmp.hdq_dx)
    _ds_hdq_cdf(Xs(), ws(), xtmp.hdq_F, xtmp.hdq_dx)
    return(xtmp.hdq_dx)
}

void _ds_hdq_cdf(`RC' X, `RC' w, `RC' F, `RC' dx)
{   // X assumed sorted
    `RM' M
    
    M  = _mm_ecdf2(X, w)
    F  = M[,2]
    dx = -mm_diff(M[,1]\0)
}

`IntC' `GRP'::hdq_p()
{   // permutation vector to expand results back to the original sample; results
    // have been computed at unique levels of X in reverse order
    `Int' k
    
    if (rows(xtmp.hdq_p)) return(xtmp.hdq_p)
    k = rows(hdq_F())
    if (k==N) xtmp.hdq_p = invorder(revorder(pX()))
    else {
        xtmp.hdq_p = J(N,1,.)
        xtmp.hdq_p[pX()] = runningsum(k \ -(Xs()[|2\N|]:!=Xs()[|1\N-1|]))
    }
    return(xtmp.hdq_p)
}

`RC' `GRP'::mq_x()
{
    if (rows(xtmp.mq_x)) return(xtmp.mq_x)
    _ds_mq_mcdf(Xs(), ws(), xtmp.mq_x, xtmp.mq_F)
    return(xtmp.mq_x)
}

`RC' `GRP'::mq_F()
{
    if (rows(xtmp.mq_F)) return(xtmp.mq_F)
    _ds_mq_mcdf(Xs(), ws(), xtmp.mq_x, xtmp.mq_F)
    return(xtmp.mq_F)
}

void _ds_mq_mcdf(`RC' X, `RC' w, `RC' x, `RC' F)
{   // X assumed sorted
    `RM' M
    
    M = _mm_ecdf2(X, w, 1)
    x = M[,1]
    F = M[,2]
}

`RS' `GRP'::mq_s(`Int' j, `RS' p, `RS' q)
{   // assuming xtmp.mq_mcdf already exists
    if (mqopt.cdf) {
        if (rows(xtmp.mq_sp)==0) {
            // set up sparsity function
            xtmp.mq_sp = _ds_mq_sp(mq_x(), mq_F())
        }
        if (xtmp.mq_bw>=.) {
            if (mqopt.bw<.) xtmp.mq_bw = mqopt.bw / 2
            else            xtmp.mq_bw = .5 / ceil(2 * Neff()^(2/5))
        }
        if (xtmp.mq_bw==0) return(xtmp.mq_sp[j])
        return(__ds_mq_sp(xtmp.mq_sp, mq_F(), p, xtmp.mq_bw, j))
    }
    if (length(xtmp.mq_S)==0) {
        // set up density estimation
        xtmp.mq_S = _ds_mq_d_init(xtmp.mq_c, mq_x(), mq_F(), Neff(), mqopt.us)
    }
    return(1 / (_ds_mq_d(xtmp.mq_S, q) * xtmp.mq_c))
}

`RC' _ds_mq_sp(`RC' x, `RC' F)
{
    `Int' r
    
    r = rows(x)
    return((x[|2\r|] - x[|1\r-1|]) :/ (F[|2\r|] - F[|1\r-1|]))
}

`RS' __ds_mq_sp(`RC' S, `RC' F, `RS' p, `RS' bw, `Int' j)
{    // equivalent to mean(S, mm_diff(mm_clip(F, ll, ul))) but more efficient
    `Int' ll, ul, a, b, r
    `RC'  f
    
    // lower bound
    ll = p - bw
    a = j
    while (ll<F[a]) {
        if (a==1) break
        a--
    }
    // upper bound
    ul = p + bw
    r = rows(F)
    b = j + 1
    while (ul>F[b]) {
        if (b==r) break
        b++
    }
    // integrate
    r = b - a
    if (r==1) return(S[j]) // no integration needed
    f = F[|a \ b|]
    if (f[1]<ll)   f[1]   = ll // clip from below
    if (f[r+1]>ul) f[r+1] = ul // clip from above
    f = mm_diff(f)
    return(quadsum(S[|a \ b-1|] :* f)/quadsum(f))
}

`PDF' _ds_mq_d_init(`RS' c, // will be replaced
    `RC' x, `RC' F, `RS' neff, `RS' us)
{
    `Int' r, g
    `RC'  X, w
    `PDF' S
    
    g  = 1024 + 1   // grid size
    r  = rows(x)
    c  = F[r] - F[1]
    X  = rangen(x[1], x[r], g)
    w = mm_fastipolate(x, F, X)
    w  = mm_diff(w) * (neff / c)
    X  = X[|2\.|] // remove first element
    S.data(X, w, 0, 1)
    S.support((x[1], x[r]))
    S.bw("dpi", neff^(1/5) / neff^(1/ (5*(1-us)))) // apply undersmoothing
    return(S)
}

`RS' _ds_mq_d(`PDF' S, `RS' q)
{
    return(S.d(q))
}

`RC' `GRP'::Xc()
{   // running sum of (weighted) outcome at unique values; including an origin
    `RC' cdf
    
    if (rows(xtmp.Xc)) return(xtmp.Xc)
    cdf = _mm_ecdf2(Xs(), ws(), 0, 1)
    xtmp.Wc = 0 \ cdf[,2]
    xtmp.Xc = 0 \ quadrunningsum(cdf[,1] :* mm_diff(xtmp.Wc))
    return(xtmp.Xc)
}

`RC' `GRP'::Wc()
{
    if (rows(xtmp.Wc)) return(xtmp.Wc)
    (void) Xc()
    return(xtmp.Wc)
}

`RC' `GRP'::XcY()
{   // running sum of (weighted) outcome at unique values; including an origin
    if (rows(xytmp.Xc_y)) return(xytmp.Xc_y)
    xytmp.Xc_y = 0 \ _mm_ecdf2(Ys(), wsY():*XsY(), 0, 1)[,2]
    return(xytmp.Xc_y)
}

`RC' `GRP'::WcY()
{
    if (rows(xytmp.Wc_y)) return(xytmp.Wc_y)
    xytmp.Wc_y = 0 \ _mm_ecdf2(Ys(), wsY(), 0, 1)[,2]
    return(xytmp.Wc_y)
}

`RC' `GRP'::EX()
{   // estimates E(X|Y=q) using local linear regression on a 100-point grid
    // (using same defaults as lpoly for kernel and bandwidth); this will 
    // be used for the IFs of concentration curve ordinates
    `RS'   bw
    `SS'   kern
    `IntC' p

    if (rows(xytmp.EX)) return(xytmp.EX)
    kern       = "epanechnikov"
    xytmp.EXat = rangen(Ys()[1], Ys()[N], 100)
    bw         = mm_loclin_bw(X(), Y(), w(), kern, wtype==1)
    xytmp.EX   = mm_loclin(X(), Y(), w(), xytmp.EXat, bw, kern)
    if (hasmissing(xytmp.EX)) { // restrict grid to nonmissing points
        p = selectindex(xytmp.EX:<.)
        assert(length(p)) // mm_loclin() failed
        xytmp.EXat = xytmp.EXat[p]
        xytmp.EX   = xytmp.EX[p]
    }
    return(xytmp.EX)
}

`RC' `GRP'::EXat()
{
    if (rows(xytmp.EXat)) return(xytmp.EXat)
    (void) EX()
    return(xytmp.EXat)
}

`RC' `GRP'::tagX()
{
    if (rows(xtmp.tag)) return(xtmp.tag)
    xtmp.tag = J(N,1,.)
    xtmp.tag[pX()] = _mm_unique_tag(Xs())
    return(xtmp.tag)
}

`RC' `GRP'::prX()
{
    if (rows(xtmp.pr)) return(xtmp.pr)
    xtmp.pr = J(N,1,.)
    xtmp.pr[pX()] = __ds_ifreq(Xs(), ws(), N)/W
    return(xtmp.pr)
}

`RC' `GRP'::tagY()
{
    if (rows(ytmp.tag)) return(ytmp.tag)
    ytmp.tag = J(N,1,.)
    ytmp.tag[pY()] = _mm_unique_tag(Ys())
    return(ytmp.tag)
}

`RC' `GRP'::prY()
{
    if (rows(ytmp.pr)) return(ytmp.pr)
    ytmp.pr = J(N,1,.)
    ytmp.pr[pY()] = __ds_ifreq(Ys(), wsY(), N)/W
    return(ytmp.pr)
}

`RC' `GRP'::tagXY()
{
    if (rows(xytmp.tag)) return(xytmp.tag)
    xytmp.tag = J(N,1,.)
    xytmp.tag[pXY()] = _mm_uniqrows_tag((xtmp.X,ytmp.Y)[pXY(),])
    return(xytmp.tag)
}

`RC' `GRP'::prXY()
{
    if (rows(xytmp.pr)) return(xytmp.pr)
    xytmp.pr = _ds_ifreq((xtmp.X,ytmp.Y), w(), N, pXY())/W
    return(xytmp.pr)
}

`RS' `GRP'::gw_K()
{
    if (length(xytmp.GW)==0) gw_build()
    return(asarray_elements(xytmp.GW))
}

`IntC' `GRP'::gw_p(`Int' key)
{
    return(asarray(xytmp.GW, key))
}

`RC' `GRP'::gw_X(`Int' key)
{
    return(X()[asarray(xytmp.GW, key)])
}

`RC' `GRP'::gw_w(`Int' key)
{
    if (rows(w())==1) return(w())
    return(w()[asarray(xytmp.GW, key)])
}

`RS' `GRP'::gw_W(`Int' key)
{
    if (rows(w())==1) return(w()*rows(asarray(xytmp.GW, key)))
                      return(quadsum(w()[asarray(xytmp.GW, key)]))
}

void `GRP'::gw_build()
{
    `Int'  i
    `RC'   p
    `IntM' ginfo
    
    xytmp.GW = asarray_create("real", 1)
    p = mm_order((ytmp.Y, xtmp.X), (1,2), 1) // stable sort
    ginfo = selectindex(_mm_uniqrows_tag(ytmp.Y[p], 0)),
            selectindex(_mm_uniqrows_tag(ytmp.Y[p], 1))
    for (i = rows(ginfo); i; i--) asarray(xytmp.GW, i, p[|ginfo[i,]'|])
}

void `GRP'::cd_fast(`RC' naive)
{
    `Bool' fast
    
    fast = (naive==0)
    if (fast==xytmp.cd_fast) return
    xytmp.cd_fast = fast
    xytmp.cd_S = .
    xytmp.cd_s = J(0,1,.)
}

`RS' `GRP'::cd_K()
{
    if (xtmp.cd_K<.)  return(xtmp.cd_K)
    if (rows(w())==1) xtmp.cd_K = N - 1
    else              xtmp.cd_K = quadsum(w() :* cd_k()) / W
    return(xtmp.cd_K)
}

`RS' `GRP'::cd_T()
{
    if (xtmp.cd_T<.)  return(xtmp.cd_T)
    if (rows(w())==1) xtmp.cd_T = sum(cd_t()) / N
    else              xtmp.cd_T = quadsum(w() :* cd_t()) / W
    return(xtmp.cd_T)
}

`RS' `GRP'::cd_U()
{
    if (ytmp.cd_U<.)  return(ytmp.cd_U)
    if (rows(w())==1) ytmp.cd_U = sum(cd_u()) / N
    else              ytmp.cd_U = quadsum(w() :* cd_u()) / W
    return(ytmp.cd_U)
}

`RS' `GRP'::cd_S()
{
    if (xytmp.cd_S<.) return(xytmp.cd_S)
    if (rows(w())==1) xytmp.cd_S = sum(cd_s()) / N
    else              xytmp.cd_S = quadsum(w() :* cd_s()) / W
    return(xytmp.cd_S)
}

`RS' `GRP'::cd_V()
{
    if (xytmp.cd_V<.) return(xytmp.cd_V)
    if (rows(w())==1) xytmp.cd_V = sum(cd_v()) / N
    else              xytmp.cd_V = quadsum(w() :* cd_v()) / W
    return(xytmp.cd_V)
}

`RC' `GRP'::cd_k()
{
    if (rows(xtmp.cd_k)) return(xtmp.cd_k)
    if (rows(w())==1)    xtmp.cd_k = J(N, 1, N-1)
    else if (wtype==1)   xtmp.cd_k = J(N, 1, W-1)
    else                 xtmp.cd_k = W :- w()
    return(xtmp.cd_k)
}

`RC' `GRP'::cd_t()
{
    if (rows(xtmp.cd_t)) return(xtmp.cd_t)
    xtmp.cd_t = J(N,1,.)
    if (rows(w())==1)  xtmp.cd_t[pX()] = __ds_ifreq(Xs(), 1, N)    :- 1
    else if (wtype==1) xtmp.cd_t[pX()] = __ds_ifreq(Xs(), ws(), N) :- 1
    else               xtmp.cd_t[pX()] = __ds_ifreq(Xs(), ws(), N) - ws()
    return(xtmp.cd_t)
}

`RC' `GRP'::cd_u()
{
    if (rows(ytmp.cd_u)) return(ytmp.cd_u)
    ytmp.cd_u = J(N,1,.)
    if (rows(w())==1) ytmp.cd_u[pY()] = __ds_ifreq(Ys(), 1, N)  :- 1
    else {
        ytmp.cd_u[pY()] = __ds_ifreq(Ys(), wsY(), N)
        if (wtype==1) ytmp.cd_u = ytmp.cd_u :- 1
        else          ytmp.cd_u = ytmp.cd_u - w()
    }
    return(ytmp.cd_u)
}

`RC' `GRP'::cd_s()
{
    if (rows(xytmp.cd_s)) return(xytmp.cd_s)
    xytmp.cd_s = J(N,1,.)
    if (xytmp.cd_fast) {
        if (rows(w())==1) xytmp.cd_s[pX()] = _ds_CD(Xs(), YsX())
        else              xytmp.cd_s[pX()] = _ds_CD(Xs(), YsX(), ws())
    }
    else {
        if (rows(w())==1) xytmp.cd_s = _ds_CD0(xtmp.X, ytmp.Y, N)
        else              xytmp.cd_s = _ds_CD0_w(xtmp.X, ytmp.Y, w(), N)
    }
    return(xytmp.cd_s)
}

`RC' `GRP'::cd_v()
{
    if (rows(xytmp.cd_v)) return(xytmp.cd_v)
    xytmp.cd_v = J(N,1,.)
    if (rows(w())==1)  xytmp.cd_v = _ds_ifreq((xtmp.X,ytmp.Y),   1, N, pXY()) :- 1
    else if (wtype==1) xytmp.cd_v = _ds_ifreq((xtmp.X,ytmp.Y), w(), N, pXY()) :- 1
    else               xytmp.cd_v = _ds_ifreq((xtmp.X,ytmp.Y), w(), N, pXY()) - w()
    return(xytmp.cd_v)
}

`IntC' _ds_CD0(`RC' X, `RC' Y, `Int' n)
{   // concordant-discordant sum; no weights; naive algorithm
    `Int'  i
    `IntC' s
    
    s = J(n,1,0)
    for (i=n;i;i--) s[i] = sum(sign((X:-X[i]) :* (Y:-Y[i])))
    return(s)
}

`IntC' _ds_CD0_w(`RC' X, `RC' Y, `RC' w, `Int' n)
{   // concordant-discordant sum; weighted; naive algorithm
    `Int' i
    `RC'  s
    
    s = J(n,1,0)
    for (i=n;i;i--) s[i] = quadsum(w :* sign((X:-X[i]) :* (Y:-Y[i])))
    return(s)
}

`RC' _ds_CD(`RC' x, `RC' y, | `RC' w)
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
    tree   = _ds_CD_btree(ny)
    root   = trunc((1 + ny) / 2)
    t      = J(n,1,0)
    
    // 1 -> nx
    wlt = weq = wgt = J(ny,1,0)
    b = 0
    for (i=1; i<=nx; i++) {
        a = b + 1
        b = b + xpanel[i]
        for (ii=a; ii<=b; ii++) t[ii] = quadsum((t[ii],
            _ds_CD_l1(tree, root, y[ii], yval, wlt, weq, wgt)))
        for (ii=a; ii<=b; ii++)
            _ds_CD_l2(tree, root, y[ii], yval, w[ii], wlt, weq, wgt)
    }
    
    // nx -> 1
    wlt = weq = wgt = J(ny,1,0)
    a = b + 1
    for (i=nx; i; i--) {
        b = a - 1
        a = a - xpanel[i]
        for (ii=b; ii>=a; ii--) t[ii] = quadsum((t[ii],
            -_ds_CD_l1(tree, root, y[ii], yval, wlt, weq, wgt)))
        for (ii=b; ii>=a; ii--) 
            _ds_CD_l2(tree, root, y[ii], yval, w[ii], wlt, weq, wgt)
    }
    return(t)
}

`RS' _ds_CD_l1(`IntM' tree, `Int' root, `RS' yi, `RC' yval,
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

void _ds_CD_l2(`IntM' tree, `Int' root, `RS' yi, `RC' yval, `RC' wi,
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

`IntM' _ds_CD_btree(`Int' n)
{   // adaption of blncdtree() by Roger Newson, 11 August 2005
    `IntM' tree
    
    tree = J(n, 2, 0)
    if (n<1) return(tree)
    __ds_CD_btree(tree, 1, n)
    return(tree)
}

void __ds_CD_btree(`IntM' tree, `Int' imin, `Int' imax)
{   // adaption of _blncdtree() by Roger Newson, 11 August 2005
    `Int' imid, inext

    imid = trunc((imin + imax) / 2)
    if (imid<=imin) { // left
         tree[imid,1] = 0
    }
    else {
      inext = imid - 1
      tree[imid,1] = trunc((imin + inext) / 2)
      __ds_CD_btree(tree, imin, inext)
    }
    if (imid>=imax) {
      tree[imid, 1] = 0
    }
    else {
      inext = imid + 1
      tree[imid,2] = trunc((inext + imax) / 2)
      __ds_CD_btree(tree, inext, imax)
    }
}

`RC' _ds_ifreq(`RM' X, `RC' w, `Int' n, | `IntC' p)
{   // returns for each row of X the number (sum of weights) of duplicates;
    // Y not assumed sorted
    `RC'   h

    if (args()<4) p = mm_order(X, ., 1) // stable sort order
    h = J(n, 1, .)
    if (rows(w)==1) h[p] = __ds_ifreq(X[p,], w, n)
    else            h[p] = __ds_ifreq(X[p,], w[p], n)
    return(h)
}

`RC' __ds_ifreq(`RM' X, `RC' w, `Int' n)
{   // returns for each row of X the number (sum of weights) of duplicates;
    // X assumed sorted
    `Int' i, j
    `RC'  h

    h = J(n, 1, .)
    if (rows(w)==1) {
        j = 1
        for (i=2; i<=n; i++) {
            if (X[i,]!=X[i-1,]) {
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
        if (X[i,]!=X[i-1,]) {
            h[|j \ i-1|] = J(i-j, 1, quadsum(w[|j \ i-1|]))
            j = i
        }
    }
    h[|j \ i-1|] = J(i-j, 1, quadsum(w[|j \ i-1|]))
    return(h)
}

struct `DATA' {
    // general settings
    `Bool'  nose       // do not compute standard errors
    `Bool'  noIF       // do not compute influence functions
    `Bool'  nocw       // 1 nocasewise, 0 else
    `Int'   wtype      // weights: 0 none, 1 fw, 2 iw, 3 pw
    // data
    `Int'   touse      // Stata variable marking sample
    `SR'    xvars      // names of outcome variables
    `RM'    X          // view on outcome data
    `Int'   nvars      // number of outcome variables
    `Int'   N          // number of obs
    `SS'    wvar       // name of variable containing weights
    `RC'    w          // view on weights
    `RS'    W          // sum of weights
    `RR'    Wj         // sum of weights per variable (relevant if nocw & uncond)
    `PR'    mvj        // index of missing values per variable (relevant if nocw & uncond)
    // over levels and balancing
    `SS'    ovar       // name of over variable
    `RC'    over       // view on over() variable
    `IntR'  overlevels // levels of over()
    `SR'    overlbls   // values of over levels (as string, including total)
    `Int'   nover      // number of (selected) over groups (excluding total)
    `Int'   Nover      // number of (selected) over groups (including total)
    `Int'   novernot   // number of excluded over groups
    `Int'   total      // include total across groups (1:pooled,2:mean,3:wmean)
    `IntR'  _N         // number of obs per group
    `RR'    _W         // sum of weights per group
    `Bool'  accum      // accumulate results across subpopulations
    `Int'   contrast   // compute contrasts: .=no contrast, #=group, .a=total, .b=lag, .c=lead
    `Int'   ratio      // type of contrast: 0=difference, 1=ratio, 2=lnratio
    `Bal'   bal        // balancing settings
    `Level' L          // structure for current level
    `Int'   k          // index of current level x variable
     // estimates
    `Int'   K          // length of results vector
    `RC'    b          // vector estimates
    `RM'    at         // vector of evaluation points (if relevant)
    `BoolC' omit       // flags omitted estimates
    `IntR'  nobs       // number of obs per estimate
    `RR'    sumw       // sum of weights per estimate
    `RC'    id         // ID of relevant over() level for each estimate
    `BoolC' cref       // flag contrast reference
    `SM'    cstripe    // column stripe for results
    `SS'    csinfo     // info on contents of cstripe
    `RM'    IF         // view on influence functions
    `RC'    IFtot      // target value of sum(IF) (typically 0)
    // command-specific settings
    `SS'    cmd        // subcommand
    `Bool'  novalues   // do not use values as coefficient names
    `SS'    vfmt       // display format for values used as coefficient names
    `Int'   qdef       // quantile definition
    `RS'    hdtrim     // apply trimming to hdquantile
    `MQopt' mqopt      // options for mid-quantiles
    `Int'   n          // number of evaluation points (if relevant)
    `Bool'  common     // use common points across subpops (if relevant)
    `RR'    ra         // custom range of evaluation points (if relevant)
    `SS'    atname     // name of atvar, if specified (summarize)
    `RC'    atvar      // view on atvar (summarize)
    `RC'    atvals     // atvar values to be used (summarize)
    `RS'    atval      // current level of atvar (summarize)
    `Int'   nstats     // number of unique stats (summarize)
    `Bool'  hasdens    // whether density estimation has been employed
    `PDF'   S          // density estimation object (density, quantile, summarize)
    `Bool'  exact      // use exact density estimator
    `Bool'  ltight     // use tight lower bound for density evaluation grid
    `Bool'  rtight     // use tight upper bound for density evaluation grid
    `RR'    bwidth     // container for density-estimation bandwidth matrix
    `PR'    AT         // pointer rowvector of sets of eval points / statistics
    `Bool'  ipolate    // interpolate ECDF (cdf/ccdf)
    `Bool'  mid        // midpoint adjustment (cdf/ccdf)
    `Bool'  floor      // use lower-than definition (cdf/ccdf)
    `Bool'  discr      // discrete (cdf/ccdf)
    `Bool'  cat        // categorical (prop, summarize)
    `Bool'  prop       // report proportions (histogram, pshare)
    `Bool'  pct        // report percent (histogram, proportion, cdf, ccdf, pshare, lorenz)
    `Bool'  uncond     // condition on total sample (density, histogram, proportion, cdf, ccdf)
    `Bool'  freq       // report frequencies (histogram, proportion, cdf, ccdf)
    `Bool'  ep         // use equal probability bins (histogram)
    `Bool'  gl         // generalized lorenz ordinates (lorenz, pshare)
    `Bool'  gap        // equality gap curve (lorenz)
    `Bool'  sum        // total (lorenz, pshare)
    `Bool'  abs        // absolute (lorenz)
    `Bool'  ave        // average (pshare)
    `Bool'  relax      // relax option (summarize)
    `SR'    yvars      // names of auxiliary variables (summarize, lorenz, pshare)
    `RM'    Y          // view on auxiliary variables (summarize, lorenz, pshare)
    `Bool'  pstrong    // use strong poverty definition (summarize, tip)
    `RS'    pline      // default poverty line (summarize, tip)
    // `RR'    hcr, pgi   // containers to store HCR and PGI (tip)
}

void dstat()
{
    `Data' D
    
    // settings
    D.nose      = st_local("nose")!=""
    D.noIF      = D.nose & (st_local("generate")=="")
    D.nocw      = st_local("nocasewise")!=""
    D.cmd       = st_local("subcmd")
    D.novalues  = st_local("novalues")!=""
    D.vfmt      = st_local("vformat")
    D.qdef      = strtoreal(st_local("qdef"))
    D.hdtrim    = strtoreal(st_local("qdef_trim"))
    D.mqopt.cdf = st_local("qdef_cdf")!=""
    D.mqopt.bw  = strtoreal(st_local("qdef_cdf"))
    D.mqopt.us  = strtoreal(st_local("qdef_usmooth"))
    D.common    = st_local("common")!=""
    D.ipolate   = st_local("ipolate")!=""
    D.mid       = st_local("mid")!=""
    D.floor     = st_local("floor")!=""
    D.discr     = st_local("discrete")!=""
    D.cat       = st_local("categorical")!=""
    D.prop      = st_local("proportion")!=""
    D.pct       = st_local("percent")!=""
    D.freq      = st_local("frequency")!=""
    D.ep        = st_local("ep")!=""
    D.gl        = st_local("generalized")!=""
    D.gap       = st_local("gap")!=""
    D.sum       = st_local("sum")!=""
    D.abs       = st_local("absolute")!=""
    D.ave       = st_local("average")!=""
    D.relax     = st_local("relax")!=""
    D.pstrong   = st_local("pstrong")!=""
    D.pline     = strtoreal(st_local("pline"))
    D.atname    = st_local("atvar")
    
    // get data
    D.touse = st_varindex(st_local("touse"))
    D.xvars = tokens(st_local("varlist"))
    st_view(D.X, ., D.xvars, D.touse)
    D.N     = rows(D.X)
    D.nvars = cols(D.X)
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
    if (D.atname!="") st_view(D.atvar, ., D.atname, D.touse)
    D.yvars = tokens(st_local("yvars"))
    ds_view(D.Y, D.yvars, D.touse)
    
    // over
    D.ovar = st_local("over")
    if (D.ovar!="") {
        st_view(D.over, ., D.ovar, D.touse)
        D.overlbls = tokens(st_local("overlevels"))
        D.overlevels = strtoreal(D.overlbls)
        D.nover = D.Nover = cols(D.overlevels)
        D.novernot = strtoreal(st_local("N_over0")) - D.nover
        D.total    = (st_local("total")=="pooled" ? 1 :
                     (st_local("total")=="mean"   ? 2 :
                     (st_local("total")=="wmean"  ? 3 : 0)))
        D.uncond   = (st_local("unconditional")!="")
        D.accum    = st_local("over_accumulate")!=""
        if (st_local("over_contrast")=="total")     D.contrast = .a
        else if (st_local("over_contrast")=="lag")  D.contrast = .b
        else if (st_local("over_contrast")=="lead") D.contrast = .c
        else D.contrast = strtoreal(st_local("over_contrast"))
        D.ratio = (st_local("over_ratio")!="") + (st_local("over_ratio")=="lnratio")
    }
    else {
        D.nover    = 0
        D.total    = 1
        D.uncond   = 0
        D.accum    = `FALSE'
        D.ratio    = 0
    }
    D.Nover = D.nover + (D.total==1)
    if (D.total==1) D.overlbls = D.overlbls, "total"
    D._N = D._W = J(1, D.Nover, .)
    
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
        D.ltight = (st_local("ltight")!="" ? .z : .)
        D.rtight = (st_local("rtight")!="" ? .z : .)
        if (st_local("bwidth")!="") {
            if (st_local("bwmat")!="") D.bwidth = st_matrix(st_local("bwidth"))[1,]
            else D.bwidth = strtoreal(tokens(st_local("bwidth")))
            D.bwidth = J(1, ceil(D.nvars*D.Nover/cols(D.bwidth)), 
                D.bwidth)[|1 \ D.nvars*D.Nover|]
        }
        else {
            D.S.bw(st_local("bwmethod"), strtoreal(st_local("bwadjust")), 
                strtoreal(st_local("bwdpi")))
            D.bwidth = J(1, D.nvars*D.Nover, .)
        }
    }
    
    // collect evaluation point/list of statistics
    ds_get_at(D)
    
    // determine dimension of results vector and initialize containers
    ds_set_K(D)
    
    // estimation
    if      (D.cmd=="density")    dstat_density(D)
    else if (D.cmd=="histogram")  dstat_hist(D)
    else if (D.cmd=="proportion") dstat_prop(D)
    else if (D.cmd=="cdf")        dstat_cdf(D, 0)
    else if (D.cmd=="ccdf")       dstat_cdf(D, 1)
    else if (D.cmd=="quantile")   dstat_quantile(D)
    else if (D.cmd=="lorenz")     dstat_lorenz(D)
    else if (D.cmd=="pshare")     dstat_pshare(D)
    else if (D.cmd=="tip")        dstat_tip(D)
    else if (D.cmd=="summarize")  dstat_sum(D)
    
    // add grand mean across subpopulations
    if (D.total>1) ds_grandmean(D)
    
    // compute contrasts
    if (D.contrast!=.) {
        if (D.ratio==2)   ds_lnratio(D)
        else if (D.ratio) ds_ratio(D)
        else              ds_contrast(D)
    }
    
    // accumulate results across subpopulations
    if (D.accum) ds_accum(D)
    
    // arrange results prepare cstripe
    ds_arrange(D)
    
    // return results
    st_local("csinfo", D.csinfo)
    st_local("k_eq", strofreal(length(_mm_unique(D.cstripe[,1]))))
    st_matrix(st_local("b"), D.b')
    st_matrixcolstripe(st_local("b"), D.cstripe)
    st_matrix(st_local("nobs"), D.nobs')
    st_matrixcolstripe(st_local("nobs"), D.cstripe)
    st_matrix(st_local("sumw"), D.sumw')
    st_matrixcolstripe(st_local("sumw"), D.cstripe)
    st_matrix(st_local("id"), D.id')
    st_matrixcolstripe(st_local("id"), D.cstripe)
    st_matrix(st_local("omit"), D.omit')
    st_matrixcolstripe(st_local("omit"), D.cstripe)
    st_local("k_omit", strofreal(sum(D.omit)))
    st_matrix(st_local("IFtot"), D.IFtot') // internal, cstripe not needed
    st_matrix(st_local("_N"), D._N)
    st_matrixcolstripe(st_local("_N"), (J(cols(D.overlbls),1,""), D.overlbls'))
    st_matrix(st_local("_W"), D._W)
    st_matrixcolstripe(st_local("_W"), (J(cols(D.overlbls),1,""), D.overlbls'))
    if (rows(D.at)) {
        st_matrix(st_local("AT"), D.at')
        st_matrixcolstripe(st_local("AT"), D.cstripe)
        if (D.cmd=="histogram" | D.cmd=="pshare") {
            st_matrixrowstripe(st_local("AT"), (J(3,1,""), ("ll", "mid", "h")'))
        }
    }
    if (D.contrast!=.) {
        st_matrix(st_local("cref"), D.cref')
        st_matrixcolstripe(st_local("cref"), D.cstripe)
    }
    if (D.hasdens) {
        st_local("hasdens", "1")
        st_local("kernel", D.S.kernel())
        ds_store_eqvec(D, D.bwidth, st_local("BW"))
    }
    else st_local("hasdens", "0")
    // if (D.cmd=="tip") {
    //     ds_store_eqvec(D, D.hcr, st_local("HCR"))
    //     ds_store_eqvec(D, D.pgi, st_local("PGI"))
    // }
    
    // check IFs and compute VCE
    if (D.noIF) return
    if (hasmissing(D.IF)) {
        D.IF[.,.] = editmissing(D.IF, 0)
        errprintf("warning: influence function(s) contain missing;" +
            " missing reset to zero\n")
    }
    ds_recenter_IF(D)
    if (D.nose) return
    ds_vce(D, st_local("clustvar"), strtoreal(st_local("vce_minus")),
        st_local("nocov")!="")
}

void ds_view(`RM' Y, `SR' yvars, `Int' touse)
{
    `Int' i
    
    i = length(yvars)
    if (i==0) return
    for (;i;i--) {
        if (st_isstrvar(yvars[i])) {
            // replace string variable with numeric tempvar containing
            // group IDs
            yvars[i] = _ds_view_destring(yvars[i], touse)
        }
    }
    st_view(Y, ., yvars, touse)
}

`SS' _ds_view_destring(`SS' yvar0, `Int' touse)
{
    `SS' yvar
    
    yvar = st_tempname()
    st_store(., st_addvar("double", yvar), touse,
        mm_group(st_sdata(., yvar0, touse)))
    return(yvar)
}

void ds_store_eqvec(`Data' D, `RR' vec, `SS' nm)
{
    `SM' cs
    
    cs = J(D.Nover, 1, D.xvars')
    if (D.ovar=="") cs = (J(rows(cs), 1, ""), cs)
    else            cs = (mm_expand(D.overlbls', D.nvars, 1, 1), cs)
    st_matrix(nm, vec)
    st_matrixcolstripe(nm, cs)
}

void ds_grandmean(`Data' D)
{
    `Int'   i, k, l, a, b, N
    `RS'    W
    `RR'    w
    `RC'    B
    `BoolC' omit, touse
    `IntC'  nobs
    `RC'    sumw, IFtot
    `RM'    IF, IFp
    `SR'    vnm

    if (D.novernot) {; N = sum(D._N); W = sum(D._W); } // over(, select())
    else            {; N = D.N; W = D.W; }
    k = D.nover
    l = D.K / k
    if (D.total==3) { // wmean
        // b     = sum_i (w_i * b_i)
        // IF(b) = sum_i (w_i * IF(b_i) + b_i * IF(w_i))
        // w_i   = W_i / W
        w = D._W / W
        if (D.noIF==0) {
            if (D.novernot) {   // over(, select())
                touse = J(D.N,1,0)
                for (i=1;i<=k;i++) touse = touse :| (D.over:==D.overlevels[i])
            }
            else touse = 1
            IFp = J(D.N,k,.)
            for (i=1;i<=k;i++) 
                IFp[,i] = touse :* ((D.over:==D.overlevels[i]) :- w[i]) / W
        }
        nobs = J(l,1,N); sumw = J(l,1,W)
    }
    else {
        w = J(1, k, 1/k)
        nobs = sumw = J(l,1,0)
    }
    B = J(l,1,0)
    omit = J(l,1,`FALSE')
    if (D.noIF==0) {
        IF = J(D.N,l,0)
        IFtot = J(l,1,0)
    }
    b = 0
    for (i=1;i<=k;i++) {
        a = b + 1; b = b + l
        B = B + w[i] * D.b[|a \ b|]
        if (D.noIF==0) {
            IF = IF + w[i] * D.IF[|1,a \ .,b|]
            if (D.total==3) IF = IF + IFp[,i] * D.b[|a \ b|]'
            IFtot = IFtot + w[i] * D.IFtot[|a \ b|]
        }
        omit = omit :| D.omit[|a \ b|]
        if (D.total==2) {
            nobs = nobs + D.nobs[|a \ b|]
            sumw = sumw + D.sumw[|a \ b|]
        }
    }
    
    // reset if statistic is missing in any subpop
    if (any(omit)) _ds_reset_omit(omit, 1, l, B, IF, IFtot, D.noIF)
    
    // update all containers 
    D.Nover = D.Nover + 1
    D.overlbls = D.overlbls, "total"
    D.cstripe = D.cstripe \ (J(l,1,"total"), D.cstripe[|1,2\l,.|])
    D.K = D.K + l
    D._N = D._N, N
    D._W = D._W, W
    D.id = D.id \ J(l,1,.)
    D.omit = D.omit \ omit
    D.nobs = D.nobs \ nobs
    D.sumw = D.sumw \ sumw
    D.b = D.b \ B
    if (D.noIF==0) {
        vnm = st_tempname(l)
        st_store(., st_addvar("double", vnm), D.touse, IF)
        vnm = tokens(st_local("IFs")), vnm
        st_view(D.IF, ., vnm, D.touse)
        st_local("IFs", invtokens(vnm))
        D.IFtot = D.IFtot \ IFtot
    }
    if (length(D.at)) D.at = D.at \ D.at[|1,1 \ l,1|]
    if (length(D.bwidth)) D.bwidth = D.bwidth, J(1, length(D.bwidth)/k,.)
}

void ds_accum(`Data' D)
{
    `Int' i, k, l, a, a0, b, b0
    
    l = D.K / D.Nover
    k = D.nover
    a0 = 1; b0 = l
    for (i=2;i<=k;i++) {
        a = a0 + l
        b = b0 + l
        D.b[|a \ b|] = D.b[|a \ b|] + D.b[|a0 \ b0|]
        if (D.noIF==0) {
            D.IF[|1,a \ .,b|] = D.IF[|1,a \ .,b|] + D.IF[|1,a0 \ .,b0|]
            D.IFtot[|a \ b|] = D.IFtot[|a \ b|] + D.IFtot[|a0 \ b0|]
        }
        ds_reset_omit(D, a, b, D.omit[|a0 \ b0|])
        a0 = a; b0 = b
    }
}

void ds_reset_omit(`Data' D, `Int' a, `Int' b, `BoolC' omit)
{
    D.omit[|a \ b|] = D.omit[|a \ b|] :| omit
    if (!any(D.omit[|a \ b|])) return
    _ds_reset_omit(D.omit, a, b, D.b, D.IF, D.IFtot, D.noIF)
}

void _ds_reset_omit(`BoolC' omit, `Int' a, `Int' b, `RC' B, `RM' IF,
    `RC' IFtot, `Bool' noIF)
{
    `Int' i
    
    for (i=a;i<=b;i++) {
        if (!omit[i]) continue
        B[i] = 0
        if (noIF==0) {
            IF[,i] = J(rows(IF), 1, 0)
            IFtot[i] = 0
        }
    }
}

void ds_contrast(`Data' D)
{
    `Int'  i, k, l, r, a, a0, b, b0
    
    k = D.Nover
    l = D.K / k
    if (D.contrast!=.b & D.contrast!=.c)  {
        if (D.contrast==.a) r = D.Nover  // total
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
        ds_reset_omit(D, a, b, D.omit[|a0 \ b0|])
    }
    D.cref = J(D.K, 1, 0)
    D.cref[|a0 \ b0|] = J(l, 1, 1)
}

void ds_ratio(`Data' D)
{
    `Bool' hasmis
    `Int'  i, k, l, r, a, a0, b, b0
    `RC'   B0, B1, IFtot
    `RM'   IF0, IF1

    hasmis = 0
    k = D.Nover
    l = D.K / k
    // take ratios
    for (i=1;i<=k;i++) {
        if (D.contrast<.b ? i==1 : i!=k) {
            if (D.contrast<.b) {
                if (D.contrast==.a) r = D.Nover  // total
                else                r = selectindex(D.overlevels:==D.contrast)
                a0 = (r-1)*l + 1; b0 = r*l
                a0 = (r-1)*l + 1; b0 = r*l
            }
            else if (D.contrast==.b) { // lag
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
        if (D.contrast<.b) {
            if (i==r) continue
            a = (i-1)*l + 1
            b = a + l - 1
            
        }
        else if (i==k) break
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
        D.omit[|a \ b|] = D.omit[|a \ b|] :| (D.b[|a \ b|]:>=.) // missing
        ds_reset_omit(D, a, b, D.omit[|a0 \ b0|])
    }
    D.cref = J(D.K, 1, 0)
    D.cref[|a0 \ b0|] = J(l, 1, 1)
    if (hasmis) display("{txt}(missing estimates reset to zero)")
}

void ds_lnratio(`Data' D)
{
    `Bool' shift
    `Int'  j
    
    shift = `FALSE'
    if (D.noIF==0) {
        if (any(D.IFtot)) {
            // recenter IFs of unnormalized statistics at zero
            shift = `TRUE'
            D.IF[.,.] = D.IF :- (D.IFtot'/D.W)
            D.IFtot = J(D.K, 1, 0)
        }
        D.IF[.,.] = (1:/D.b)' :* D.IF
    }
    D.b = ln(D.b)
    ds_contrast(D)
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

void ds_arrange(`Data' D)
{
    `Int'   i, n
    `BoolR' keep
    `IntC'  p
    `IntR'  ordr
    `SR'    order, info, vnm
    
    // setup
    info = "over", "variables"
    if (D.cmd=="summarize") info = info, "statistics"
    else                    info = info, "at"
    ordr = (1,2,3)
    keep = J(1,3,`TRUE')
    if (D.ovar=="")   keep[1] = `FALSE'
    if (D.nvars==1)   keep[2] = `FALSE'
    if (D.nstats==1)  keep[3] = `FALSE' // (summarize only)
    
    // summarize with at(): add 4th column containing at-levels
    if (D.atname!="") {
        info = info, "at"
        ordr = ordr, 4
        keep = keep, `TRUE'
        D.cstripe = D.cstripe, (D.novalues ?
            J(D.K/D.n, 1, "at" :+ strofreal(1::D.n)) :
            (D.cat ? strofreal(D.at) :+ ("." + D.atname) :
            strofreal(D.at, D.vfmt)))
    }
    
    // determine order
    order = tokens(st_local("order"))
    if (i=length(order)) {
        for (;i;i--) {
            p = selectindex(info:==order[i])
            if (length(p)) ordr = p, ordr
        }
        if (D.ovar=="") {
            // place over first, unless explicitly specified
            if (!anyof(order,"over")) ordr = 1, ordr
        }
        else if (D.nstats==1 & D.atname=="") {
            // place stats first, unless explicitly specified
            if (!anyof(order,"statistics")) ordr = 3, ordr
        }
        ordr = mm_unique(ordr, 1)
    }
    else if (D.nstats==1 & D.atname=="") {
        // change default if single statistic
        if (D.nvars>1)       ordr = (3,1,2) // vars as coefnames
        else if (D.ovar!="") ordr = (3,2,1) // over as coefnames
    }
    
    // add factor-variable notation (if relevant)
    if (D.cmd=="summarize") {
        if (D.atname=="") {
            if (ordr[cols(ordr)]==1) { // over as coefnames
                p = selectindex(D.cstripe[,1] :!= "total")
                D.cstripe[p,1] = D.cstripe[p,1] :+ ("."+D.ovar)
            }
        }
    }
    else if (D.cmd=="proportion") {
        if (D.cat & !D.novalues) {
            D.cstripe[,3] = D.cstripe[,3] :+ "." :+ D.cstripe[,2]
            keep[2] = `FALSE'
        }
    }
    
    // rearrange results
    if (ordr'!=sort(ordr',1)) {
        keep = keep[ordr]
        info = info[ordr]
        D.cstripe = D.cstripe[,ordr]
        p = _ds_arrange_order(D.cstripe)
        if (p!=(1::rows(p))) {
            _collate(D.cstripe, p)
            _collate(D.b, p)
            _collate(D.nobs, p)
            _collate(D.sumw, p)
            _collate(D.id, p)
            _collate(D.omit, p)
            _collate(D.IFtot, p)
            if (rows(D.at))    _collate(D.at, p)
            if (D.contrast!=.) _collate(D.cref, p)
            if (D.noIF==0) {
                vnm = tokens(st_local("IFs"))[p]
                st_view(D.IF, ., vnm, D.touse)
                st_local("IFs", invtokens(vnm))
            }
        }
    }
    keep[cols(keep)] = `TRUE' // always keep coefnames
    
    // finalize cstripe
    if (!all(keep)) {
        D.cstripe = D.cstripe[, selectindex(keep)]
        info      = select(info, keep)
    }
    D.csinfo = invtokens(info)
    i = cols(D.cstripe) - 1
    if (i==0) D.cstripe = J(rows(D.cstripe), 1, "_"), D.cstripe
    else if (i>1) { // merge columns
        for (--i;i;i--) D.cstripe[,i] = D.cstripe[,i] :+ "~" :+ D.cstripe[,i+1]
        D.cstripe = D.cstripe[,(1,cols(D.cstripe))]
    }
}

`IntC' _ds_arrange_order(`SM' S)
{
    `Int'  i, j, r, c, g, a
    `IntM' G
    `T'    A
    
    // generate matrix containing column-wise group indicators (with IDs
    // based on first occurence rather then sort order)
    r = rows(S); c = cols(S)
    G = J(r, c, .)
    for (j=c;j;j--) {
        g = 0
        A = asarray_create()
        asarray_notfound(A, .)
        for (i=1;i<=r;i++) {
            a = asarray(A, S[i,j])
            if (a>=.) asarray(A, S[i,j], a = ++g)
            G[i,j] = a
        }
    }
    // return order based on group indicators (permutation vector)
    return(mm_order(G, ., 1))
}

void ds_vce(`Data' D, `SS' clust, `Int' minus, `Bool' seonly)
{
    `Bool' dev
    `RS'   df_r, c, N_clust
    `RR'   m
    `RM'   V
    
    // compute V
    m = D.IFtot'
    dev = any(m) // use crossdev() if there are any score-type IFs
    if (clust=="") { // no clusters
        if (D.wtype==0) { // no weights
            if (seonly)   V = _ds_vce_cross(1, D.IF, m/D.N)
            else if (dev) V = _ds_vce_cdev(D.IF, m/D.N)
            else          V = cross(D.IF, D.IF)
            df_r = D.N - minus
            c = (df_r>0 & df_r<. ? D.N/df_r : 0)
        }
        else if (D.wtype==3) { // pw
            if (seonly)   V = _ds_vce_cross2(D.w, D.IF, m/D.N)
            else if (dev) V = _ds_vce_cdev(D.w:*D.IF, m/D.N)
            else          V = cross(D.IF, D.w:^2, D.IF)
            df_r = D.N - minus
            c = (df_r>0 & df_r<. ? D.N/df_r : 0)
        }
        else { // iw or fw
            if (seonly)   V = _ds_vce_cross(D.w, D.IF, m/D.W)
            else if (dev) V = _ds_vce_cdev(D.IF, m/D.W, D.w)
            else          V = cross(D.IF, D.w, D.IF)
            df_r = D.W - minus
            c = (df_r>0 & df_r<. ? D.W/df_r : 0)
        }
    }
    else { // with clusters
        V = _ds_vce_csum(D.IF, D.w, (st_isstrvar(clust) ? 
            st_sdata(., clust, D.touse) : st_data(., clust, D.touse)))
        N_clust = rows(V)
        if (seonly)   V = _ds_vce_cross(1, V, m/N_clust)
        else if (dev) V = _ds_vce_cdev(V, m/N_clust)
        else          V = cross(V, V)
        df_r = N_clust - minus
        c = (df_r>0 & df_r<. ? N_clust/df_r : 0)
        st_local("N_clust", st_tempname())
        st_numscalar(st_local("N_clust"), N_clust)
    }
    V = V * c
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

`RR' _ds_vce_cross(`RC' w, `RM' X, | `RR' m)
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

`RR' _ds_vce_cross2(`RC' w, `RM' X, `RR' m)
{   // like _ds_vce_cross(), but for m!=0 with pweights 
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

`RM' _ds_vce_cdev(`RM' X, `RR' m, | `RC' w)
{
    if (args()<3) return(crossdev(X, m, X, m))
    return(crossdev(X, m, w, X, m))
}

`RM' _ds_vce_csum(`RM' X, `RC' w, `TC' C)
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

void ds_get_at(`Data' D)
{
    `RS' d
    
    D.AT = NULL
    if (D.cmd=="summarize") {
        _ds_get_stats(D)
        if (D.atname!="") D.atvals = _ds_get_atvals(D)
    }
    else if (st_local("atmat")!="") _ds_get_atmat(D, st_local("at"))
    else if (st_local("at")!="") D.AT = &(strtoreal(tokens(st_local("at")))')
    else {
        D.ra = strtoreal(tokens(st_local("range")))
        if (cols(D.ra)<2) D.ra = D.ra, J(1,2-cols(D.ra),.)
        if      (D.cmd=="proportion") _ds_get_levels(D)
        else if (D.cmd=="histogram")  _ds_get_at_hist(D, st_local("n"))
        else if (D.cmd=="density") {
            D.n = strtoreal(st_local("n"))
            if (!hasmissing(D.ra)) D.AT = &(rangen(D.ra[1], D.ra[2], D.n))
            else if (D.S.lb()<. & D.S.ub()<. & D.ltight!=.z & D.rtight!=.z)
                D.AT = &(rangen(D.ra[1]<. ? D.ra[1] : D.S.lb(),
                                D.ra[2]<. ? D.ra[2] : D.S.ub(), D.n))
            else if (D.common) _ds_get_at_dens(D)
            else {
                if (D.ra[1]>=.) D.ra[1] = D.ltight
                if (D.ra[2]>=.) D.ra[2] = D.rtight
            }
        }
        else if (D.cmd=="cdf" | D.cmd=="ccdf") {
            if (D.discr) _ds_get_levels(D)
            else {
                D.n = strtoreal(st_local("n"))
                if (!hasmissing(D.ra)) D.AT = &(rangen(D.ra[1], D.ra[2], D.n))
                else if (D.common) _ds_get_at_cdf(D)
            }
        }
        else if (D.cmd=="quantile") {
            D.n = strtoreal(st_local("n"))
            if (D.ra[1]>=.) D.ra[1] = 0
            if (D.ra[2]>=.) D.ra[2] = 1
            d = (D.ra[2] - D.ra[1]) / (D.n + 1)
            D.AT = &(rangen(D.ra[1]+d, D.ra[2]-d, D.n))
        }
        else if (D.cmd=="lorenz" | D.cmd=="tip") {
            D.n = strtoreal(st_local("n"))
            if (D.ra[1]>=.) D.ra[1] = 0
            if (D.ra[2]>=.) D.ra[2] = 1
            D.AT = &(rangen(D.ra[1], D.ra[2], D.n))
        }
        else if (D.cmd=="pshare") {
            D.n  = strtoreal(st_local("n")) + 1 // extra point for upper limit
            if (D.ra[1]>=.) D.ra[1] = 0
            if (D.ra[2]>=.) D.ra[2] = 1
            if (D.ra[1]>D.ra[2]) D.ra = D.ra[(2,1)] // ascending order
            D.AT = &(rangen(D.ra[1], D.ra[2], D.n))
        }
    }
    D.AT = J(1, ceil(D.nvars*D.Nover/cols(D.AT)), D.AT)[|1 \ D.nvars*D.Nover|]
}

`RC' _ds_get_atvals(`Data' D)
{
    `RR' ra
    `RC' at
    
    if (st_local("atmat")!="") { // select first equation in first row
        at = st_matrix(st_local("at"))[|1,1 \ 1, selectindex(_mm_unique_tag(
            st_matrixcolstripe(st_local("at"))[,1], 1))|]'
        if (D.cat) {
            assert(at==trunc(at)) // integer
            assert(all(at:>=0))   // positive
        }
        return(at)
    }
    if (st_local("at")!="") return(strtoreal(tokens(st_local("at")))')
    at = mm_unique(D.atvar)
    if (D.cat) {
        if (any(at:<0)) {
            errprintf("{bf:%s} has negative values; omit option " + 
            "{bf:categorical} to allow negative values\n", D.atname)
            exit(452)
        }
        if (any(at:!=trunc(at))) {
            errprintf("{bf:%s} has noninteger values; omit option " + 
                "{bf:categorical} to allow noninteger values\n", D.atname)
            exit(452)
        }
    }
    if (st_local("range")!="") {
        ra = strtoreal(tokens(st_local("range")))
        if (cols(ra)<2) ra = ra, J(1,2-cols(ra),.)
        _ds_get_atvals_range(at, ra[1], ra[2])
    }
    return(at)
}

void _ds_get_atvals_range(`RC' at, `RS' ll, `RS' ul) // ll assumed fleeting
{
    if (ll>=.) ll = at[1]
    if (ll>ul) {
        at = at[rows(at)::1] // use reverse order
        at = select(at, at:>=ul :& at:<=ll)
    }
    else at = select(at, at:>=ll :& at:<=ul)
    if (!length(at)) at = .z // no observations
}

void _ds_get_atmat(`Data' D, `SS' matnm)
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
    else if (anyof(("quantile", "lorenz", "pshare", "tip"), D.cmd)) {
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
        if (D.cmd=="histogram" | D.cmd=="pshare") {
            // ul must not be smaller than ll
            assert(all(mm_diff(M[|from[i] \ to[i]|]):>=0))
        }
        D.AT[i] = &(M[|from[i] \ to[i]|]')
    }
}

void _ds_get_stats(`Data' D)
{
    `Int' j
    `SS'  s, s2
    `T'   t, t2
    
    t = t2 = tokeninit(" ", "", "()")
    tokenset(t, st_local("slist"))
    tokenset(t2, st_local("slist2"))
    D.AT = J(1, D.nvars, NULL)
    j = 0
    while ((s = tokenget(t)) != "") {
        s2 = tokenget(t2)
        D.AT[++j] = &(tokens(substr(s2,2,strlen(s2)-2))', // expanded
                      tokens(substr(s,2,strlen(s)-2))')   // as specified
        s = tokenget(t) // varname
        assert(s==D.xvars[j])
    }
    D.nstats = strtoreal(st_local("N_stats"))
}

void _ds_get_levels(`Data' D)
{   // obtains levels of each X across full sample
    `Bool' hasrange
    `Int'  j

    hasrange = D.ra[1]<. | D.ra[2]<.
    D.AT = J(1, D.nvars, NULL)
    for (j=1;j<=D.nvars;j++) {
        if (D.nocw)
             D.AT[j] = &(mm_unique(select(D.X[,j], D.X[,j]:<.)))
        else D.AT[j] = &(mm_unique(D.X[,j]))
        if (!length(*D.AT[j])) *D.AT[j] = .z // no observations
        else if (hasrange) _ds_get_atvals_range(*D.AT[j], D.ra[1], D.ra[2])
        if (!D.cat) continue
        if (any(*D.AT[j]:<0)) {
            errprintf("{bf:%s} has negative values; specify option " + 
                "{bf:nocategorical} to allow negative values\n", D.xvars[j])
            exit(452)
        }
        if (any(*D.AT[j]:!=trunc(*D.AT[j]))) {
            errprintf("{bf:%s} has noninteger values; specify option " + 
                "{bf:nocategorical} to allow noninteger values\n", D.xvars[j])
            exit(452)
        }
    }
}

void _ds_get_at_dens(`Data' D)
{   // generate evaluation grid based on full sample
    `Int' j
    `RS'  tau
    `RR'  bw, ra, minmax
    `RC'  X
    
    // obtain bandwidth if specified
    j = D.nover * D.nvars
    if (D.total==1) bw = D.bwidth[|j+1 \ .|]
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
            X = select(D.X[,j], D.X[,j]:<.)
            if (length(X)==0) { // no observations
                D.AT[j] = &(J(D.n, 1, .z))
                continue
            }
        }
        else X = D.X[,j]
        // mimic grid definition from mm_density()
        ra = D.ra
        minmax = minmax(X)
        if (ra[1]>=.) {
            if (D.ltight==.z)    ra[1] = minmax[1]
            else if (D.S.lb()<.) ra[1] = D.S.lb()
        }
        if (ra[2]>=.) {
            if (D.rtight==.z)    ra[2] = minmax[2]
            else if (D.S.ub()<.) ra[2] = D.S.ub()
        }
        if (missing(ra)) {
            if (bw[j]>=.) {
                D.S.data(X, D.w, D.wtype>=2, 0)
                bw[j] = D.S.h()
                if (bw[j]>=.) bw[j] = epsilon(1)
            }
            tau = mm_diff(minmax) * D.S.pad()
            tau = min((tau, bw[j] * (D.S.kernel()=="epanechnikov" ? sqrt(5) : 
                (D.S.kernel()=="cosine" ? .5 : 
                (D.S.kernel()=="gaussian" ? 3 : 1)))))
            if (ra[1]>=.) ra[1] = minmax[1] - tau
            if (ra[2]>=.) ra[2] = minmax[2] + tau
        }
        D.AT[j] = &(rangen(ra[1], ra[2], D.n)) 
    }
    if (D.total==1) { // store bandwidth to avoid double work
        D.bwidth[|D.nover*D.nvars + 1 \ .|] = bw
    }
}

void _ds_get_at_cdf(`Data' D)
{   // generate evaluation grid based on full sample
    `Int' j

    D.AT = J(1, D.nvars, NULL)
    for (j=1;j<=D.nvars;j++) {
        if (D.nocw)
             D.AT[j] = &(ds_cdf_AT(select(D.X[,j], D.X[,j]:<.), D.n, D.ra))
        else D.AT[j] = &(ds_cdf_AT(D.X[,j], D.n, D.ra))
    }
}

void _ds_get_at_hist(`Data' D, `SS' rule)
{
    `Int'  i, I, j, n
    `IntC' p
    `RS'   N, h
    `RR'   minmax
    `RC'   at
    `Grp'  G
    
    if (rule=="")          rule = "sqrt"
    if (strtoreal(rule)<.) D.n = strtoreal(rule) + 1
    if (D.common) {
        I = i = D.nover + 1 // total only
        D.AT = J(1, D.nvars, NULL)
    }
    else {
        i = 1
        I = D.Nover
        D.AT = J(1, D.nvars*D.Nover, NULL)
    }
    D.k = 0
    for (; i<=I; i++) {
        D.L = _ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j)
            if (G.N==0) { // no observations
                if (D.n<.) D.AT[D.k] = &(J(D.n,1,.z))
                else       D.AT[D.k] = &(J(2,1,.z))
                continue
            }
            N = G.Neff() // effective sample size
            minmax = minmax(G.X())
            if (D.n>=.) {
                // sqrt is the rule used by official Stata's -histogram-; for the 
                // other rules below see https://en.wikipedia.org/wiki/Histogram
                if (rule=="sqrt") {
                    n = ceil(min((sqrt(N), 10*ln(N)/ln(10)))) - 1
                }
                else if (rule=="sturges") {
                    n = ceil(ln(N)/ln(2))
                }
                else if (rule=="rice") {
                    n = ceil(2 * N^(1/3)) - 1
                }
                else if (rule=="doane") {
                    n = ceil(ln(N)/ln(2) + 
                        ln(1 + abs(_ds_sum_skewness(G.X(), G.w(), G.W)) / 
                        sqrt(6 * (N-2) / ((N+1) * (N+3))))/ln(2))
                }
                else if (rule=="scott") {
                    h = 3.5 * sqrt(_ds_variance(G.X(), G.w(), G.W, D.wtype)) /
                        N^(1/3)
                    n = ceil(mm_diff(minmax)/h) - 1
                }
                else if (rule=="fd") {
                    h = 2 * mm_iqrange(G.X(), G.w()) / N^(1/3)
                    n = ceil(mm_diff(minmax)/h) - 1
                }
                else if (rule=="ep") {
                    n = ceil(2 * N^(2/5))
                }
                else exit(499)
                if (n<1) n = 1 // make sure that never zero
            }
            else n = D.n - 1
            if (D.ra[1]<.) minmax[1] = D.ra[1]
            if (D.ra[2]<.) minmax[2] = D.ra[2]
            if (minmax[1]>minmax[2]) minmax = minmax[(2,1)]
            if (n<2)          D.AT[D.k] = &(minmax[1] \ minmax[2])
            else if (D.ep==0) D.AT[D.k] = &(rangen(minmax[1], minmax[2], n+1))
            else { // ep
                if (any(D.ra:<.)) {
                    p = J(G.N,1,0)
                    if (D.ra[1]<.) p = p + (G.X():<D.ra[1])
                    if (D.ra[2]<.) p = p + (G.X():>D.ra[2])
                    p = selectindex(!p)
                    if (length(p)) {
                        at = _mm_unique(mm_quantile(G.X()[p],
                            rows(G.w())==1 ? G.w() : G.w()[p], (0::n)/n, 1))
                        at[1] = minmax[1]
                        if (rows(at)==1) at = at \ minmax[2]
                        else             at[rows(at)] = minmax[2]
                    }
                    else at = minmax'
                }
                else {
                    at = _mm_unique(mm_quantile(G.X(), G.w(), (0::n)/n, 1))
                    if (rows(at)==1) at = at \ at
                }
                D.AT[D.k] = &(at*1) // store pointer to copy
            }
        }
    }
}

void ds_set_K(`Data' D)
{
    `Int' i, j, k, r
    
    // determine K
    if (D.n<. & !D.ep) D.K = D.Nover * D.nvars * D.n
    else {
        if (D.cmd=="summarize") {
            if (D.atname!="") D.n = rows(D.atvals)
            else              D.n = 1
            r = D.n
        }
        else r = 1
        D.K = k = 0
        for (i=1; i<=D.Nover; i++) {
            for (j=1; j<=D.nvars; j++) {
                k++
                D.K = D.K + rows(*D.AT[k]) * r
            }
        }
    }
    // initialize containers
    D.b = D.id = D.nobs = D.sumw = J(D.K, 1, .)
    D.omit = J(D.K, 1, 0)
    D.IFtot = J(D.K, 1, 0)
    if (D.cmd=="histogram" | D.cmd=="pshare") D.at = J(D.K, 3, .)
    else if (D.cmd!="summarize")              D.at = J(D.K, 1, .)
    else if (D.atname!="")                    D.at = J(D.K/r, 1, D.atvals)
    D.cstripe = J(D.K, 3, "")
//    D.eqs = J(1, D.Nover*D.nvars, "")
    if (D.noIF==0) ds_init_IF(D)
}

void ds_init_IF(`Data' D)
{
    `SR' vnm
    
    vnm = st_tempname(D.K)
    st_view(D.IF, ., st_addvar("double", vnm), D.touse)
    st_local("IFs", invtokens(vnm))
}

void ds_set_IF(`Data' D, `Grp' G, `Int' k, `RC' h, | `RS' h0, `RS' offset)
{
    D.IF[,k] = _ds_set_IF(D, G, h, h0, (D.IFtot[k] ? offset : 0))
}

`RC' _ds_set_IF(`Data' D, `Grp' G, `RC' h, `RS' h0, `RS' offset)
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
            if (length(ds_mvj(D, G))) {
                IF[ds_mvj(D, G)] = IF[ds_mvj(D, G)] :* 0
            }
        }
    }
    else IF = J(D.N, 1, 0)
    if (cols(D.L.Z)) {     // current level has balancing
        if (rows(G.pp)) {  // current obs are subsample of level
            delta = quadcolsum(G.wc :* h :* D.L.Z[G.pp,])'
            IF[G.p()] = G.wb :* h
            IF[D.L.p] = IF[D.L.p] + D.L.IFZ * delta
        }
        else {
            delta = quadcolsum(D.L.wc :* h :* D.L.Z)'
            IF[D.L.p] = D.L.wb :* h + D.L.IFZ  * delta
        }
        IF[D.L.p1] = IF[D.L.p1] + D.L.IFZ1 * delta
    }
    else        IF[G.p()] = h
    if (offset) IF[G.p()] = IF[G.p()] :+ offset
    return(IF)
}

void ds_recenter_IF(`Data' D)
{    // issue warning and recenter if relative error is larger than 1e-6
    `RS'  mrd, tol
    `RR'  m
    
    tol = 1e-6
    m   = quadcross(D.w, D.IF) - D.IFtot'
    mrd = mreldif(m+D.b', D.b')
    if (mrd<=tol) return
    errprintf("warning: total of influence function(s) deviates from zero\n")
    errprintf("         maximum relative error = %g\n", mrd)
    D.IF[.,.] = D.IF :- m/D.W
    errprintf("         influence function(s) recentered at zero\n")
}

void ds_set_cstripe(`Data' D, `Int' i, `Int' j, `IntM' p, `SC' coefs)
{
    `Int' r
    
    // if (D.nvars>1) {
    //     if (D.ovar!="") D.eqs[D.k] = D.overlbls[i] + "~" + D.xvars[j]
    //     else            D.eqs[D.k] = D.xvars[j]
    // }
    // else if (D.ovar!="") D.eqs[D.k] = D.overlbls[i]
    // else                 D.eqs[D.k] = "_"
    if (cols(p)>1) { // p = (a,b) => use range subscipts
        r = p[2] - p[1] + 1
        D.cstripe[|p',(1\.)|] = J(r, 1, (D.overlbls[i], D.xvars[j])), coefs
        D.id[|p'|] = J(r, 1, D.L.id)
        return
    }
    r = rows(p)
    D.cstripe[p,] = J(r, 1, (D.overlbls[i], D.xvars[j])), coefs
    D.id[p] = J(r, 1, D.L.id)
}

void ds_init_L(`Data' D, `Int' i)
{
    // group data
    D.L = _ds_init_L(D, i)
    D._N[i] = D.L.N
    D._W[i] = D.L.W
    // balancing
    if (D.bal.method=="")  return
    if (D.bal.ref==D.L.id) return // do not balance reference group
    if (D.bal.ref<.) D.L.p1 = selectindex(D.over:==D.bal.ref)
    else             D.L.p1 = selectindex(D.over:!=D.L.id)
    if (D.bal.noisily) printf("{txt}\n==> balance: {it:groupvar} = %g\n", D.L.id)
    if      (D.bal.method=="ipw") ds_balance_ipw(D, D.bal, D.L)
    else if (D.bal.method=="eb")  ds_balance_eb(D, D.bal, D.L)
    else exit(499) // cannot be reached
    ds_balance_rescale(D, D.L) // update weights
    if (length(D.bal.wvar)) D.bal.wvar[D.L.p] = D.L.w
}

`Level' _ds_init_L(`Data' D, `Int' i)
{
    `Level' L
    
    if (i>D.nover) { // total
        L.id = L.p = .
        L.N  = D.N
        L.w  = D.w
        L.W  = D.W
    }
    else {
        L.id = D.overlevels[i]
        L.p  = selectindex(D.over:==L.id)
        L.N  = rows(L.p)
        if (rows(D.w)==1) {; L.w = D.w;      L.W = D.w*L.N;      }
        else              {; L.w = D.w[L.p]; L.W = quadsum(L.w); }
    }
    return(L)
}

void ds_init_G(`Grp' G, `Data' D, `Level' L, `Int' j, | `Int' y, `RS' pl,
    `IntC' q, `PS' f, `RR' o)
{
    `RS'    c
    `BoolC' touse
    `PS'    p
    
    if (args()<7) q = .
    p = (q!=. ? &q : &.)
    // find missings
    if (D.nocw) {
        // - main variable
        if (*p==.) {
            touse = _ds_init_G_tagmis(D.X[L.p, j])
            if (length(touse)) p = &selectindex(touse)
        }
        else {
            touse = _ds_init_G_tagmis(D.X[L.p==. ? *p : L.p[*p], j])
            if (length(touse)) p = &select(*p, touse)
        }
        // - byvar
        if (y<.) {
            if (*p==.) {
                touse = _ds_init_G_tagmis(D.Y[L.p, y])
                if (length(touse)) p = &selectindex(touse)
            }
            else {
                touse = _ds_init_G_tagmis(D.Y[L.p==. ? *p : L.p[*p], y])
                if (length(touse)) p = &select(*p, touse)
            }
        }
        // - plvar
        if (pl<0) {
            if (*p==.) {
                touse = _ds_init_G_tagmis(D.Y[L.p, -pl])
                if (length(touse)) p = &selectindex(touse)
            }
            else {
                touse = _ds_init_G_tagmis(D.Y[L.p==. ? *p : L.p[*p], -pl])
                if (length(touse)) p = &select(*p, touse)
            }
        }
    }
    // find observations out of support
    if (f!=NULL) {
        if (*p==.) {
            if (length(o)) touse = (*f)(D, j, D.X[L.p, j], o)
            else           touse = (*f)(D, j, D.X[L.p, j])
            if (length(touse)) p = &selectindex(touse)
        }
        else {
            if (length(o)) touse = (*f)(D, j, D.X[L.p==. ? *p : L.p[*p], j], o)
            else           touse = (*f)(D, j, D.X[L.p==. ? *p : L.p[*p], j])
            if (length(touse)) p = &select(*p, touse)
        }
    }
    // restricted sample
    if (*p!=.) {
        if (G.N<.) {
            if (*p!=G.pp) G = `GRP'() // subsample changed
        }
        if (G.N>=.) {
            G.wtype = D.wtype; G.pstrong = D.pstrong; G.mqopt = D.mqopt
            G.p(L.p==. ? *p : L.p[*p])
            G.N = rows(*p)
            G.w(rows(L.w)==1 ? L.w : L.w[*p])
            G.W = rows(D.w)==1 ? D.w*G.N : quadsum(D.w[G.p()]) // [use raw w]
            if (cols(L.Z)) { // has balancing
                // renormalize weights; alternative would be to set c = 1 and
                // G.W = quadsum(G.w()); SEs would be the same, but normalizing
                // to the raw sum of weights within the subsample is used here
                // so that overall W of nonmissing obs across subpops is not
                // affected by the balancing
                c = G.W / quadsum(G.w())
                G.w(G.w() * c)
                if (D.noIF==0) {
                    G.wb = L.wb[*p] * c
                    G.wc = L.wc[*p] * c
                }
            }
            G.pp = *p
        }
    }
    // full sample
    else {
        if (G.N<.) {
            if (L.p!=G.p()) G = `GRP'() // subsample changed
        }
        if (G.N>=.) {
            G.wtype = D.wtype; G.pstrong = D.pstrong; G.mqopt = D.mqopt
            G.p(L.p); G.N = L.N; G.w(L.w); G.W = L.W
            G.pp = J(0,1,.)
        }
    }
    // copy data
    if (j!=G.j) {
        G.Xset(j, D.X[G.p(),j])
        if (D.S.nobs()) D.S.data(J(0,1,.)) // clear density object
    }
    if (y<.) {
        G.y_is_x = (D.xvars[j]==D.yvars[y])
        G.Yset(y, D.Y[G.p(),y])
    }
    if (pl<.) {
        if (pl<0) G.PLset(-pl, D.Y[G.p(),-pl])
        else      G.PLset(pl)
    }
}

`BoolC' _ds_init_G_tagmis(`RC' z)
{
    if (!hasmissing(z)) return(J(0,1,.)) // no missings
    return(z :< .)
}

void ds_set_density(`Data' D, `Grp' G, | `Bool' nosort)
{
    if (args()<3) nosort = 0
    if (D.S.nobs()) return // ds_set_density() already applied
    if (nosort) {
        // use unsorted data
        D.S.data(G.X() , G.w() , D.wtype>=2, 0) 
    }
    else {
        // use sorted data
        D.S.data(G.Xs(), G.ws(), D.wtype>=2, 1) 
        // mm_density() does not check support if sort==1
        if (D.S.lb()<.) {
            if (G.Xs()[1]<D.S.lb()) {
                errprintf("{it:X} contains values out of support\n")
                exit(3300)
            }
        }
        if (D.S.ub()<.) {
            if (G.Xs()[G.N]>D.S.ub()) {
                errprintf("{it:X} contains values out of support\n")
                exit(3300)
            }
        }
    }
    if (D.bwidth[D.k]<.) D.S.bw(D.bwidth[D.k])
    if (D.S.h()>=.)      D.S.bw(epsilon(1))  // missing bandwidth
    D.bwidth[D.k] = D.S.h()
    D.hasdens = `TRUE'
}

void ds_noobs(`Data' D, `Int' a, | `Int' b)
{
    `Int' n
    
    if (args()<3) {
        D.b[a]    = 0
        D.omit[a] = 1
        if (D.noIF) return
        D.IF[,a] = J(D.N, 1, 0)
        return
    }
    n = b - a + 1
    D.b[|a \ b|] = J(n, 1, 0)
    D.omit[|a \ b|] = J(n, 1, 1)
    if (D.noIF) return
    D.IF[|1,a \ .,b|] = J(D.N, n, 0)
}

void ds_set_nobs_sumw(`Data' D, `Grp' G, `Int' a, | `Int' b)
{
    if (args()<4) {
        D.nobs[a] = G.N
        D.sumw[a] = G.W
        return
    }
    D.nobs[|a \ b|] = J(b-a+1, 1, G.N)
    D.sumw[|a \ b|] = J(b-a+1, 1, G.W)
}

`RS' ds_Wj(`Data' D, `Grp' G)
{
    if (D.nocw==0) return(D.W)
    if (D.Wj[G.j]<.) return(D.Wj[G.j]) // already computed
    if (D.wtype) D.Wj[G.j] = quadsum(select(D.w, D.X[,G.j]:<.))
    else         D.Wj[G.j] = sum(D.X[,G.j]:<.)
    return(D.Wj[G.j])
}

`RC' ds_mvj(`Data' D, `Grp' G)
{
    if (D.mvj[G.j]==NULL) {
        D.mvj[G.j] = &selectindex(D.X[,G.j]:>=.)
    }
    return(*D.mvj[G.j])
}

// --------------------------------------------------------------------------
// balancing
// --------------------------------------------------------------------------

void ds_balance_rescale(`Data' D, `Level' L)
{
    `RS' c

    // at this point sum(L.w) is equal to the size of the reference group
    if (D.noIF==0) L.wc = L.w
    if (D.wtype) {
        // add base weight if reference is pooled sample
        if (D.bal.ref>=.) L.w = L.w :+ L.wb
        // rescaling factor
        c = quadsum(L.wb) / quadsum(L.w)
    }
    else {
        // add one if reference is pooled sample
        if (D.bal.ref>=.) L.w = L.w :+ 1
        // rescaling factor
        c = rows(L.w) / quadsum(L.w)
    }
    L.w  = L.w * c          // rescaled total weight
    if (D.noIF) L.wb = J(0,1,.)
    else {
        L.wb = L.w :/ L.wb  // total weight / base weight
        L.wc = L.wc * c     // total weight - "pooled" component
    }
}

void ds_balance_ipw(`Data' D, `Bal' bal, `Level' L)
{
    `SS' treat, ps
    `RC' p
    
    // run logit and obtain ps
    treat = st_tempname(); ps = st_tempname()
    ds_balance_ipw_T(D, L, treat)
    stata("quietly "
        + (bal.noisily ? "noisily " : "")
        + "logit " + treat + " " + bal.zvars
        + (D.wtype ? " ["+("fw","iw","pw")[D.wtype]+"="+D.wvar+"]" : "")
        + (bal.opts!="" ? ", "+bal.opts : ""))
    stata("quietly predict double " + ps + " if e(sample), pr")
    
    // compute balancing weights and rescale to size of reference group
    p = st_data(., ps, D.touse)
    L.wb = L.w // backup base weights
    L.w = p[L.p]
    L.w = L.w :/ (1 :- L.w) :* L.wb
    if (D.wtype) L.w = L.w * (quadsum(D.w[L.p1]) / quadsum(L.w))
    else         L.w = L.w * (rows(L.p1)         / quadsum(L.w))
    _editmissing(L.w, 0) // can occur in case of perfect predictors
    if (hasmissing(p[L.p1])) {
        printf("{err}warning: some reference observations discarded in"
            + " balancing of {it:groupvar} = %g\n", L.id)
        printf("{err}         balancing may be poor\n")
        _editmissing(L.w, 0)
    }
    
    // fillin IFs
    if (D.noIF==0) ds_balance_ipw_IF(D, L, treat, p) // modifies p
    
    // cleanup
    st_dropvar(treat); st_dropvar(ps)
}

void ds_balance_ipw_T(`Data' D, `Level' L, `SS' treat)
{
    `RC' T
    
    T       = J(D.N,  1, .)
    T[L.p1] = J(rows(L.p1), 1, 1)
    T[L.p]  = J(L.N,  1, 0)
    st_store(., st_addvar("byte", treat), D.touse, T)
}

void ds_balance_ipw_IF(`Data' D, `Level' L, `SS' treat, `RC' p)
{
    `Bool' cons
    `IntC' idx
    `RC'   T, h, w
    `RM'   Z, IFZ
    `SR'   zvars
    pragma unset Z
    
    // get data
    idx = L.p1 \ L.p
    T = st_data(., treat, D.touse)[idx]
    p = p[idx]
    _ds_replacemissing(p, T) // perfect predictions
    zvars = st_matrixcolstripe("e(b)")[,2]'
    cons = anyof(zvars, "_cons")
    if (cons) zvars = zvars[|1\length(zvars)-1|]
    st_view(Z, ., zvars, D.touse)
    st_subview(Z, Z, idx, _ds_non_omitted(length(zvars)))
    
    // compute IFs
    h = (T :- p)
    if (cons) h = Z :* h, h
    else      h = Z :* h
    w = p :* (1 :- p)
    if (D.wtype) w = w :* D.w[idx]
    IFZ = h * invsym(quadcross(Z, cons, w, Z, cons))'
    
    // store results
    L.IFZ1 = IFZ[|1,1 \ rows(L.p1),.|]
    L.IFZ  = IFZ[|rows(L.p1)+1,1 \ .,.|]
    L.Z    = (cols(Z) ? Z[|rows(L.p1)+1,1 \ .,.|] : J(L.N, 0, .)) // no longer a view
    if (cons) L.Z = L.Z, J(L.N, 1, 1) 
}

void _ds_replacemissing(`RC' X, `RC' Y)
{
    `IntC' p

    if (hasmissing(X)) {
        p = selectindex(X:>=.)
        X[p] = Y[p]
    }
}

`RR' _ds_non_omitted(`Int' c)
{
    real scalar k

    stata("_ms_omit_info e(b)")
    k = st_numscalar("r(k_omit)")
    if (k==0) return(1..c)
    if (k==c) return(J(1, 0, .))
    return(selectindex(!st_matrix("r(omit)")[|1\c|]))
}

void ds_balance_eb(`Data' D, `Bal' bal, `Level' L)
{
    `RM' Z1
    `RC' w1
    `T'  S
    pragma unset Z1
    
    // data
    st_view(L.Z, ., D.bal.zvars, D.touse)
    st_subview(Z1,  L.Z, L.p1, .)
    st_subview(L.Z, L.Z, L.p,  .)
    if (D.wtype) w1 = D.w[L.p1]
    else         w1 = D.w
    
    // settings
    S = mm_ebal_init(Z1, w1, L.Z, L.w)
    mm_ebal_btol(S, bal.ebopts[1])
    mm_ebal_difficult(S, bal.ebopts[2])
    if (bal.ebopts[3]<.) mm_ebal_maxiter(S, bal.ebopts[3])
    if (bal.ebopts[4]<.) mm_ebal_ptol(S, bal.ebopts[4])
    if (bal.ebopts[5]<.) mm_ebal_vtol(S, bal.ebopts[5])
    if (bal.noisily==0)  mm_ebal_trace(S, "none")

    // estimation
    if (mm_ebal(S)==0) {
        printf("{err}warning: balancing tolerance not achieved for {it:groupvar} = %g\n", L.id)
        printf("{err}         balancing may be poor\n")
    }
    L.wb = L.w // backup base weights
    L.w  = mm_ebal_W(S)
    if (hasmissing(L.w)) {
        errprintf("unexpected error; balancing weights contain missing values\n")
        exit(499)
    }

    // fillin IFs
    if (D.noIF==0) ds_balance_eb_IF(L, Z1, w1)

}

void ds_balance_eb_IF(`Level' L, `RM' Z1, `RC' w1)
{
    `RS' W, odds
    `RR' M
    `RC' h
    `RM' Q
    
    // beta
    M      = mean(Z1, w1) // target moments
    h      = L.Z :- M
    Q      = invsym(quadcross(h, L.w, L.Z))
    L.IFZ  = -(L.w:/L.wb) :* h * Q'
    L.IFZ1 =  ((Z1 :- M) * Q')

    // alpha
    odds   = (rows(w1)==1 ? w1*rows(Z1) : quadsum(w1)) / L.W
    h      = L.w:/L.wb :- odds
    Q      = quadcolsum(L.Z :* L.w)
    W      = quadsum(L.w)
    L.IFZ  = L.IFZ, -(h :+ odds :+ L.IFZ * Q')/W
    L.IFZ1 = L.IFZ1, (1 :- L.IFZ1 * Q')/W
    L.Z    = L.Z, J(L.N, 1, 1) // no longer a view
}

// --------------------------------------------------------------------------
// dstat density
// --------------------------------------------------------------------------

void dstat_density(`Data' D)
{
    `Int' i, j, a, b
    `RC'  AT
    `Grp' G
    
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j)
            if (G.N) ds_set_density(D, G, 1)
            a = b + 1
            if (D.AT[D.k]==NULL) {
                b = a + D.n - 1
                if (G.N) {
                    D.b[|a \ b|] = D.S.d(D.n, D.ra[1], D.ra[2], D.exact)
                    AT = D.S.at()
                }
                else AT = J(D.n, 1, .z)
            }
            else {
                AT = *D.AT[D.k]
                b = a + rows(AT) - 1
                if (G.N) D.b[|a \ b|] = D.S.d(AT, D.exact)
            }
            D.at[|a \ b|] = AT
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "d":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) ds_noobs(D, a, b)
            else {
                if (D.uncond) D.b[|a \ b|] = D.b[|a \ b|] * (G.W/ds_Wj(D, G))
                if (D.noIF==0) ds_density_IF(D, G, a, b)
            }
        }
    }
}

void ds_density_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i
    `RS'  W, B
    `RC'  h, z
    
    W  = (D.uncond ? ds_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b|] = D.b[|a \ b|]
    h  = D.S.h() * D.S.l()
    for (i=a; i<=b; i++) {
        z = D.S.K(D.S.X(), D.at[i], h)
        if (D.exact) B = D.b[i]
        else         B = quadcross(G.w(), z) / W
        ds_set_IF(D, G, i, (z :- B)/W, -B/W, D.b[i]/W)
    }
}

// --------------------------------------------------------------------------
// dstat histogram
// --------------------------------------------------------------------------

void dstat_hist(`Data' D)
{
    `Int' i, j, a, b
    `RC'  AT, B, h, c
    `RS'  min
    `Grp' G
    
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j)
            AT = *D.AT[D.k]
            h = mm_diff(AT) // bin width
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a,1 \ b,.|] = AT, AT + (h/2 \ .), (h \ .)
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "h":+strofreal(1::b-a)\"_ul" : strofreal(AT, D.vfmt)))
            if (G.N==0) {
                ds_noobs(D, a, b)
                continue
            }
            B = mm_diff(mm_relrank(G.X(), G.w(), AT, 0, 1))
            min = min(G.X())
            if (min==AT[1]) {
                if (G.w()==1) B[1] = B[1] + sum(G.X():==AT[1])
                else B[1] = B[1] + quadsum(select(G.w(), (G.X():==AT[1])))
            }
            if (!D.freq) {
                if      (D.prop) c = 1 / G.W
                else if (D.pct)  c = 100 / G.W
                else {
                    c = 1 :/ (h * G.W)
                    _editmissing(c, 0) // zero width bins
                }
                B = B :* c
                if (D.uncond) B = B * (G.W/ds_Wj(D, G))
            }
            D.b[|a \ b|] = B \ B[rows(B)]
            if (D.noIF==0) ds_hist_IF(D, G, a, b, min)
        }
    }
}

void ds_hist_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `RS' min)
{
    `Int' i
    `RS'  W, c
    `RC'  z
    
    W = (D.uncond ? ds_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b-1|] = D.b[|a \ b-1|]
    for (i=a; i<=b; i++) {  // assuming b-a >= 1
        if (i==b) {
            D.IF[,i] = J(D.N, 1, 0)
            break
        }
        if (i==a) {
            if (D.at[i,1]<=min) z = G.X():<=D.at[i+1,1]
            else  z = (G.X():>D.at[i,1] :& G.X():<=D.at[i+1,1])
        }
        else z = (G.X():>D.at[i,1] :& G.X():<=D.at[i+1,1])
        if      (D.prop) z = (z :- D.b[i]    ) / W
        else if (D.pct)  z = (z :- D.b[i]/100) * (100/W)
        else if (D.freq) z = (z :- D.b[i]/W  )
        else {
            c = 1 / (D.at[i,3] * W)
            if (c>=.) c = 0 // zero width bin
            z = (z :- D.b[i]*D.at[i,3]) * c
        }
        ds_set_IF(D, G, i, z, -D.b[i]/W, D.b[i]/W)
    }
}

// --------------------------------------------------------------------------
// dstat cdf
// --------------------------------------------------------------------------

void dstat_cdf(`Data' D, `Bool' cc)
{
    `Int' i, j, a, b
    `RC'  AT
    `Grp' G
    
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j)
            if (D.AT[D.k]==NULL) AT = ds_cdf_AT(G.X(), D.n, D.ra)
            else                 AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "c":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0)      ds_noobs(D, a, b)
            else if (AT==.z) ds_noobs(D, a, b)
            else {
                D.b[|a \ b|] = _ds_cdf(G.X(), G.w(), AT, D.mid+D.floor*2,
                    D.freq, D.ipolate)
                if (cc) {
                    if (D.freq) D.b[|a \ b|] = G.W :- D.b[|a \ b|]
                    else        D.b[|a \ b|] = 1   :- D.b[|a \ b|]
                }
                if (D.pct)  D.b[|a \ b|] = D.b[|a \ b|] * 100
                if (D.uncond & !D.freq)
                    D.b[|a \ b|] = D.b[|a \ b|] * (G.W/ds_Wj(D, G))
                if (D.noIF==0) ds_cdf_IF(D, G, a, b, cc)
            }
        }
    }
}

`RC' ds_cdf_AT(`RC' X, `Int' n, `RR' ra)
{
    `RR' minmax
    
    if (rows(X)==0) return(J(n,1,.z))
    minmax = minmax(X)
    if (ra[1]<.) minmax[1] = ra[1]
    if (ra[2]<.) minmax[2] = ra[2]
    return(rangen(minmax[1], minmax[2], n))
}

`RC' _ds_cdf(`RC' X, `RC' w, `RC' at, `Int' t, `Bool' nonorm, `Bool' ipolate)
{   // t = 2 floor, t = 1 mid, t = 0 regular
    `Int'  r, W
    `RC'   c
    `IntC' p
    `RM'   cdf
    
    if (rows(X)==0 | rows(at)==0) return(J(rows(at),1,.))
    cdf = mm_ecdf2(X, w, 0, 1) // cumulative frequency at unique values of X
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
    if (mm_issorted(at)) return(__ds_cdf(cdf[,1], cdf[,2], at, t))
    p = order(at, 1)
    return(__ds_cdf(cdf[,1], cdf[,2], at[p], t)[invorder(p)])
}

`RC' __ds_cdf(`RC' x, `RC' cdf, `RC' at, `Int' t)
{
    `Int' i, j
    `RS'  ati
    `RC'  r, h
    
    i = rows(at)
    r = J(i, 1, 0)
    j = rows(x)
    if (t==2) {         // floor
        for (; i; i--) {
            ati = at[i]
            for (; j; j--) {
                if (x[j]<ati) break
            }
            if (j) r[i] = cdf[j]
            else break // at[i] <= min(x)
        }
    }
    else if (t==1) {    // mid
        h = mm_diff(0\cdf)/2
        for (; i; i--) {
            ati = at[i]
            for (; j; j--) {
                if (x[j]<=ati) break
            }
            if (j) {
                if (x[j]==ati) r[i] = cdf[j] - h[j]
                else           r[i] = cdf[j]
            }
            else break // at[i] is smaller than min(x)
        }
        return(r)
    }
    else {          // regular
        for (; i; i--) {
            ati = at[i]
            for (; j; j--) {
                if (x[j]<=ati) break
            }
            if (j) r[i] = cdf[j]
            else break // at[i] < min(x)
        }
    }
    return(r)
}

void ds_cdf_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Bool' cc)
{
    `Int' i, j, k, K
    `RS'  W
    `RR'  C
    `RC'  zP
    `RM'  z, AT
    
    W = (D.uncond ? ds_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b|] = D.b[|a \ b|]
    if (D.ipolate) {
        // compute IF as weighted average of the IFs of the next lower and
        // upper observed X-values
        AT = _ds_cdf_IF_AT(G.X(), D.at[|a\b|])
        j = 0
        for (i=a; i<=b; i++) {
            j++
            K = 1 + (AT[j,1]<AT[j,2])
            z = J(G.N, K, .)
            C = J(1, K, .)
            for (k=K; k; k--) {
                if (D.mid) {
                    zP = (G.X() :== AT[j,k]) / 2
                    if (cc) z[,k] = (G.X() :> AT[j,k])
                    else    z[,k] = (G.X() :< AT[j,k])
                    C[k]  = sum(quadcross(G.w(), (z[,k], zP))) / W
                    z[,k] = (z[,k] + zP) :- C[k]
                }
                else {
                    if (D.floor) {
                        if (cc) z[,k] = (G.X() :>= AT[j,k])
                        else    z[,k] = (G.X() :<  AT[j,k])
                    }
                    else {
                        if (cc) z[,k] = (G.X() :> AT[j,k])
                        else    z[,k] = (G.X() :<= AT[j,k])
                    }
                    C[k]  = quadcross(G.w(), z[,k]) / W
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
            ds_set_IF(D, G, i, z, -C/W, D.b[i]/W)
        }
        return
    }
    for (i=a; i<=b; i++) {
        if (D.pct)       C = D.b[i] / 100
        else if (D.freq) C = D.b[i] / W
        else             C = D.b[i]
        if (D.mid) {
            zP = (G.X() :== D.at[i]) / 2
            if (cc) z = (G.X() :> D.at[i])
            else    z = (G.X() :< D.at[i])
            z  = (z + zP) :- C
        }
        else {
            if (D.floor) {
                if (cc) z = (G.X() :>= D.at[i])
                else    z = (G.X() :<  D.at[i])
            }
            else {
                if (cc) z = (G.X() :>  D.at[i])
                else    z = (G.X() :<= D.at[i])
            }
            z = (z :- C)
        }
        if (D.pct)           z = z * (100/W)
        else if (!D.freq)    z = z / W
        ds_set_IF(D, G, i, z, -D.b[i]/W, D.b[i]/W)
    }
}

`RM' _ds_cdf_IF_AT(`RC' X, `RC' at) // at assumed fleeting
{
    `Int' i, j, n
    `RS'  ati
    `RC'  x, p
    `RM'  AT
    
    p  = order(at, 1)
    at = at[p]
    x  = mm_unique(X)
    n  = j = rows(x)
    i  = rows(at)
    AT = (at, at)
    for (; i; i--) {
        ati = at[i]
        for (; j; j--) {
            if (x[j]<=ati) break
        }
        if (j) {
            if (x[j]==ati) continue
            AT[i,1] = x[j]
            if (j<n) AT[i,2] = x[j+1]
        }
        else AT[i,2] = x[1]
    }
    return(AT[invorder(p),])
}

// --------------------------------------------------------------------------
// dstat proportion
// --------------------------------------------------------------------------

void dstat_prop(`Data' D)
{
    `Int' i, j, a, b
    `RC'  AT
    `Grp' G
    
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "p":+strofreal(1::b-a+1) : (D.cat ? strofreal(AT) :
                strofreal(AT, D.vfmt))))
            if (G.N==0)      ds_noobs(D, a, b)
            else if (AT==.z) ds_noobs(D, a, b)
            else {
                D.b[|a \ b|] = _ds_prop(D, G, AT)
                if (D.noIF==0) ds_prop_IF(D, G, a, b)
            }
        }
    }
}

`RC' _ds_prop(`Data' D, `Grp' G, `RC' AT)
{
    `Int' i
    `RS'  W
    `RM'  H
    `RC'  F
    `T'   A
    
    // obtain frequency table
    H = _ds_prop_freq(G.Xs(), G.ws(), G.N)
    W = (D.uncond ? ds_Wj(D, G) : G.W)
    // return if evaluation points are equal to observed levels
    if (AT==H[,1]) {
        if (D.pct)  return(H[,2] * (100/W))
        if (D.freq) return(H[,2])
                    return(H[,2] / W)
    }
    // post H as hash table
    A = asarray_create("real")
    asarray_notfound(A, 0)
    for (i=rows(H); i; i--) asarray(A, H[i,1], H[i,2])
    // copy frequencies
    i = rows(AT)
    F = J(i, 1, .)
    for (; i; i--) F[i] = asarray(A, AT[i])
    // return
    W = (D.uncond ? ds_Wj(D, G) : G.W)
    if (D.pct)  return(F * (100/W))
    if (D.freq) return(F)
                return(F / W)
}

`RM' _ds_prop_freq(`RC' X, `RC' w, `Int' n)
{   // return frequency table of X (single variable); X assumed sorted
    `Int' i, j, i0
    `RS'  x0
    `RM'  H
    
    H = J(n, 2, .)
    if (n==0) return(H)
    j = i0 = 1
    x0 = X[i0]
    if (rows(w)==1) {
        for (i=2;i<=n; i++) {
            if (X[i]!=x0) {
                H[j++,] = (x0, (i-i0)*w)
                i0 = i
                x0 = X[i0]
            }
        }
        H[j,] = (x0, (i-i0)*w)
    }
    else {
        for (i=2;i<=n; i++) {
            if (X[i]!=x0) {
                H[j++,] = (x0, quadsum(w[|i0\i-1|]))
                i0 = i
                x0 = X[i0]
            }
        }
        H[j,] = (x0, quadsum(w[|i0\i-1|]))
    }
    return(H[|1,1 \ j,2|])
}

void ds_prop_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i
    `RS'  W
    `RC'  h
    
    W = (D.uncond ? ds_Wj(D, G) : G.W)
    if (D.freq) D.IFtot[|a \ b|] = D.b[|a \ b|]
    for (i=a; i<=b; i++) {
        if (D.pct)       h = ((G.X() :== D.at[i]) :- D.b[i]/100) * (100/W)
        else if (D.freq) h = ((G.X() :== D.at[i]) :- D.b[i]/W  )
        else             h = ((G.X() :== D.at[i]) :- D.b[i]    ) / W
        ds_set_IF(D, G, i, h, -D.b[i]/W, D.b[i]/W)
    }
}

// --------------------------------------------------------------------------
// dstat quantile
// --------------------------------------------------------------------------

void dstat_quantile(`Data' D)
{
    `Int' i, j, a, b
    `RC'  AT
    `Grp' G
    
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "q":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) {
                ds_noobs(D, a, b)
                continue
            }
            D.b[|a \ b|] = 
                _mm_quantile(G.Xs(), G.ws(), AT, D.qdef, D.wtype==1, D.hdtrim)
            if (D.noIF) continue
            ds_quantile_IF(D, G, a, b)
        }
    }
}

void ds_quantile_IF(`Data' D, `Grp' G, `Int' a, `Int' b)
{
    `Int' i, l
    `RC'  h, fx
    
    if (D.qdef!=10 & D.qdef!=11) {
        fx = _ds_sum_d(D, G, D.b[|a \ b|])
        l = 0
    }
    for (i=a; i<=b; i++) {
        if      (D.qdef==10) h = _ds_sum_q_IF10(G, D.at[i], D.hdtrim)
        else if (D.qdef==11) h = _ds_sum_q_IF11(G, D.at[i], D.b[i])
        else                 h = _ds_sum_q_IFxx(G, D.at[i], D.b[i], fx[++l])
        ds_set_IF(D, G, i, h / G.W)
    }
}

// --------------------------------------------------------------------------
// dstat lorenz
// --------------------------------------------------------------------------

void dstat_lorenz(`Data' D)
{
    `Int' i, j, a, b, t, y
    `RC'  AT
    `Grp' G
    
    if (D.gap & D.pct) t = 6 // equality gap in percent
    else if (D.gap)    t = 5 // equality gap
    else if (D.abs)    t = 4 // absolute
    else if (D.gl)     t = 3 // generalized
    else if (D.sum)    t = 2 // total
    else if (D.pct)    t = 1 // ordinary in percent
    else               t = 0 // ordinary
    if (cols(D.Y)) y = 1     // by() has been specified
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j, y)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "l":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) ds_noobs(D, a, b)
            else {
                D.b[|a \ b|]  = _ds_lorenz(G, AT, t, y)
                if (D.noIF==0) ds_lorenz_IF(D, G, a, b, t, y)
            }
        }
    }
}

`RC' _ds_lorenz(`Grp' G, `RC' P, `Int' t, | `Int' y)
{   // determine Lorenz ordinates using interpolation
    `RS'  Xtot
    `PS'  Xc, Wc
    `RC'  p, L

    if (y<.) {
        Xc = &(G.XcY())
        Wc = &(G.WcY())
    }
    else {
        Xc = &(G.Xc())
        Wc = &(G.Wc())
    }
    Xtot = (*Xc)[rows(*Xc)]
    p = order(P, 1)
    L = P[p] * (*Wc)[rows(*Wc)]
        // using Wc[rows(Wc)] instead of G.W to avoid precision issues at P=1
    L[p] = mm_fastipolate(*Wc, *Xc, L)
        // (assuming that sum of weights>0 for each unique X-value)
    if (t==6) return((P - L/Xtot) * 100)   // equality gap in %
    if (t==5) return(P - L/Xtot)           // equality gap
    if (t==4) return((L - P:*Xtot) / G.W)  // absolute
    if (t==3) return(L / G.W)              // generalized
    if (t==2) return(L)                    // total
    if (t==1) return(L * (100/Xtot))       // ordinary in %
              return(L = L / Xtot)         // ordinary
}

void ds_lorenz_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Int' t, | `Int' y)
{
    if (t==2) D.IFtot[|a \ b|] = D.b[|a \ b|] // total
    D.IF[|1,a\.,b|] = _ds_lorenz_IF(D, G, D.at[|a \ b|], t, D.b[|a \ b|], y)
}

`RM' _ds_lorenz_IF(`Data' D, `Grp' G, `RC' at, `Int' t, `RC' B, | `Int' y)
{
    `Int' i, n
    `RC'  q, ex
    `RM'  IF
    
    n = rows(at)
    if (y<.) {
        q = _mm_quantile(G.Ys(), G.wsY(), at, 2)
        ex = _ds_lorenz_IF_EX(G, q)
    }
    else ex = q = _mm_quantile(G.Xs(), G.ws(), at, 2)
    IF = J(D.N, n, 0)
    for (i=1; i<=n; i++) {
        if (at[i]==0)               continue
        if (at[i]==1 & t!=2 & t!=3) continue
        IF[,i] = __ds_lorenz_IF(D, G, (y<. ? G.Y() : G.X()):<=q[i],
            ex[i], t, B[i], y)
    }
    return(IF)
}

`RC' _ds_lorenz_IF_EX(`Grp' G, `RC' at)
{
    `IntC' p
    `RC'   ex
    
    p  = order(at, 1)
    ex = J(rows(at), 1, .)
    ex[p] = mm_fastipolate(G.EXat(), G.EX(), at[p], 1)
    return(ex)
}

`RC' __ds_lorenz_IF(`Data' D, `Grp' G, `RC' Zq, `RS' ex, `Int' t, `RS' B,
    `Int' yvar)
{
    `RS' p, L, T, W
    `RC' z, h
    
    if (yvar<.) {
        T = (G.XcY())[rows(G.XcY())]
        W = (G.WcY())[rows(G.WcY())]
    }
    else {
        T = (G.Xc())[rows(G.Xc())]
        W = (G.Wc())[rows(G.Wc())]
    }
    z   = G.X() :* Zq
    p   = ds_mean(Zq, G.w(), G.W) // set p=mean(zq) to ensure mean(IF)=0
    L   = quadsum(z:*G.w()) // set L=sum(z) to ensure mean(IF)=0
    h   = ex * (p :- Zq)  // part of IF due to quantile
    if      (t==6) h = (z :- L/T:*G.X() :+ h) * (100/-T) // equality gap in %
    else if (t==5) h = (z :- L/T:*G.X() :+ h) / -T       // equality gap
    else if (t==4) h = (z :- L/W :+ p*(T/W :- G.X()) :+ h) / G.W // absolute
    else if (t==3) h = (z :- L/W :+ h) / G.W             // generalized
    else if (t==2) h =  z :- L/W :+ h                    // total
    else if (t==1) h = (z :- L/T:*G.X() :+ h) * (100/T)  // ordinary in %
    else           h = (z :- L/T:*G.X() :+ h) / T        // ordinary
    return(_ds_set_IF(D, G, h, 0, t==2 ? B/G.W : 0))
}

// --------------------------------------------------------------------------
// dstat pshare
// --------------------------------------------------------------------------

void dstat_pshare(`Data' D)
{
    `Int' i, j, a, b, d, t, y
    `RC'  L, AT, B, h
    `Grp' G
    
    if      (D.prop) {; d = 0; t = 0; } // proportion
    else if (D.ave)  {; d = 1; t = 3; } // average
    else if (D.gl)   {; d = 0; t = 3; } // generalized
    else if (D.sum)  {; d = 0; t = 2; } // total
    else if (D.pct)  {; d = 0; t = 1; } // percent
    else             {; d = 1; t = 0; } // density
    if (cols(D.Y)) y = 1 // by() has been specified
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j, y)
            AT = *D.AT[D.k]
            h = mm_diff(AT) // bin width
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a,1 \ b,.|] = AT, AT + (h/2 \ .), (h \ .)
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "s":+strofreal(1::b-a)\"_ul" : strofreal(AT, D.vfmt)))
            if (G.N==0) ds_noobs(D, a, b)
            else {
                L = _ds_lorenz(G, AT, t, y)
                B = mm_diff(L)
                if (d) B = B :* editmissing(1 :/ h, 0)
                D.b[|a \ b|]  = B \ B[rows(B)]
                if (D.noIF==0) ds_pshare_IF(D, G, a, b, t, d, L, y)
            }
        }
    }
}

void ds_pshare_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `Int' t, `Int' d, 
    `RC' L, | `Int' y)
{
    if (t==2) D.IFtot[|a \ b-1|] = D.b[|a \ b-1|] // total
    D.IF[|1,a \ .,b-1|] = _ds_pshare_IF(D, G, D.at[|a,1 \ b,1|], t, d, L, y)
    D.IF[,b] = J(D.N, 1, 0)
}

`RM' _ds_pshare_IF(`Data' D, `Grp' G, `RC' at, `Int' t, `Int' d, `RC' L, 
    | `Int' y)
{
    `Int' i, n
    `RM'  IF
    
    n = rows(at)
    IF = _ds_lorenz_IF(D, G, at, t, L, y)
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
    `Int' i, j, a, b, t
    `RS'  pl
    `RC'  AT
    `Grp' G
    
    // D.hcr = D.pgi = J(1, D.nvars*D.Nover, .)
    if   (D.abs) t = 1   // absolute
    else         t = 0   // relative
    if (cols(D.Y)) pl = -1      // pline is variable
    else           pl = D.pline
    D.k = b = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (j=1; j<=D.nvars; j++) {
            D.k = D.k + 1
            ds_init_G(G, D, D.L, j, ., pl)
            AT = *D.AT[D.k]
            a = b + 1
            b = a + rows(AT) - 1
            D.at[|a \ b|] = AT
            ds_set_nobs_sumw(D, G, a, b)
            ds_set_cstripe(D, i, j, (a,b), (D.novalues ? 
                "tip":+strofreal(1::b-a+1) : strofreal(AT, D.vfmt)))
            if (G.N==0) ds_noobs(D, a, b)
            else D.b[|a \ b|] = _ds_tip(D, G, AT, t, a, b)
        }
    }
}

`RC' _ds_tip(`Data' D, `Grp' G, `RC' P, `Int' t, `Int' a, `Int' b)
{   // determine tip ordinates
    `RC' pg, p, p2, L
    `RM' cdf
    
    if (t==1) pg =  (G.pl() :- G.X())            :* G.poor()
    else      pg = ((G.pl() :- G.X()) :/ G.pl()) :* G.poor()
    p = mm_order(-pg, 1, 1)
    cdf = _mm_ecdf2(pg[p], rows(G.w())==1 ? G.w() : G.w()[p], 0, 1)
    cdf = (0,0) \ (quadrunningsum(cdf[,1] :* mm_diff(0 \ cdf[,2])), cdf[,2])
    p2 = order(P, 1)
    L = P[p2] * cdf[rows(cdf),2]
        // using cdf[rows(cdf),2] instead of G.W to avoid precision issues at P=1
    L[p2] = mm_fastipolate(cdf[,2], cdf[,1], L)
    // D.hcr[D.k] = ds_mean(G.poor(), hc, G.w(), G.W)
    // D.pgi[D.k] = cdf[rows(cdf),1] / G.W
    if (D.noIF==0) ds_tip_IF(D, G, a, b, P, pg)
    return(L / G.W)
}

void ds_tip_IF(`Data' D, `Grp' G, `Int' a, `Int' b, `RC' P, `RC' pg)
{
    `Int' i, j
    `RS'  p, t
    `RC'  q, zq, z
    
    q = -mm_quantile(-pg, G.w(), P, 2)
    j = 0
    for (i=a; i<=b; i++) {
        j++
        if (P[j]==0) {
            ds_set_IF(D, G, i, J(G.N, 1, 0))
            continue
        }
        zq = (pg:>=q[j])
        p  = ds_mean(zq, G.w(), G.W)   // set p=mean(zq) to ensure mean(IF)=0
        z  = pg :* zq
        t  = ds_mean(z, G.w(), G.W)    // set t=mean(z) to ensure mean(IF)=0
        ds_set_IF(D, G, i, (z :- t :+ q[j] * (p :- zq)) / G.W)
    }
}

// --------------------------------------------------------------------------
// dstat summarize
// --------------------------------------------------------------------------

void dstat_sum(`Data' D)
{
    `Int'  i, l, j, ii, nn, k0, b0, b
    `IntC' p, q
    `SM'   AT
    `Grp'  G
    `T'    I
    
    I = _ds_sum_invalid_dict(("mld", "theil", "ge", "vlog"),
        ("gmean", "atkinson", "lvar", "sdlog", "chu", "watts"))
    k0 = b0 = 0
    for (i=1; i<=D.Nover; i++) {
        ds_init_L(D, i)
        if (i>1) G = `GRP'()
        for (l=1;l<=D.n;l++) { // at levels
            if (D.atname!="") {
                D.atval = D.atvals[l]
                q = selectindex(D.atvar[D.L.p]:==D.atval)
            }
            else q = .
            D.k = k0; b = b0
            for (j=1; j<=D.nvars; j++) {
                D.k = D.k + 1
                AT = *D.AT[D.k]
                nn = rows(AT)
                p = (b + l) :+ (0::nn-1) * D.n
                b = b + nn * D.n
                ds_set_cstripe(D, i, j, p, AT[,2]) // use stats as specified
                for (ii=1; ii<=nn; ii++) {
                    _dstat_sum(D, G, j, q, p[ii], AT[ii,1], I)
                    ds_set_nobs_sumw(D, G, p[ii])
                }
            }
        }
        k0 = D.k; b0 = b
    }
}

void _dstat_sum(`Data' D, `Grp' G, `Int' j, `IntC' q, `Int' i, `SS' s, `T' I)
{
    `SS'  stat, arg
    `SR'  args
    `RR'  o
    `Int' k, n, l, y
    `RS'  pl
    `PS'  f
    
    // parse arguments
    l = strpos(s,"(")
    if (l) {
        stat = substr(s,1,l-1)
        args = subinstr(substr(s,l+1,strlen(s)-l-1),","," ")
        if (strpos(args,"by") | strpos(args,"pl")) {
            args = tokens(args)
            n = length(args)
            o = J(1,n,.)
            l = 0
            for (k=1;k<=n;k++) {
                arg = args[k]
                if (substr(arg,1,2)=="by")      y  = strtoreal(substr(arg,3,.))
                else if (substr(arg,1,2)=="pl") pl = strtoreal(substr(arg,3,.))
                else                            o[++l] = strtoreal(arg)
            }
            if (l) o = o[|1\l|]
            else   o = J(1,0,.)
        }
        else {
            o = strtoreal(tokens(args))
            l = length(o)
        }
    }
    else {
        stat = s
        l = 0
    }
    // find statistic
    f = findexternal("ds_sum_"+stat+"()")
    if (f==NULL) {
        errprintf("function ds_sum_%s() not found\n", stat)
        exit(499)
    }
    // setup sample etc.
    ds_init_G(G, D, D.L, j, y, pl, q, _ds_sum_invalid_f(I, stat), o)
    if (G.N==0) {
        ds_noobs(D, i)
        return
    }
    // compute statistic
    if (l) (*f)(D, G, i, o)
    else   (*f)(D, G, i)
}

`BoolC' _ds_sum_invalid(`Data' D, `Int' j, `SS' s, `RC' z)
{
    `SS' t
    
    if (!hasmissing(z)) return(J(0,1,.)) // no invalid obs
    t = sprintf("%s", D.xvars[j])
    if (D.atname!="") t = sprintf("%s at %s=%g", t, D.atname, D.atval)
    if (D.ovar!="") {
        if (D.L.id<.) t = sprintf("%s in %s=%g", t, D.ovar, D.L.id)
        else          t = sprintf("%s in total", t)
    }
    t = sprintf("%s of %s: %g observations out of support", s, t, missing(z))
    if (D.relax) printf("{txt}(%s)\n", t)
    else {
        errprintf(t+"\n")
        errprintf("specify option {bf:relax} to use valid observations only\n")
        exit(459)
    }
    return(z :< .) // tag valid observations
}

`T' _ds_sum_invalid_dict(`SR' S1, `SR' S2)
{
    `Int' i
    `T'   A
    
    A = asarray_create()
    asarray_notfound(A, 0)
    for (i=length(S1);i;i--) {
        asarray(A,       S1[i], 1)
        asarray(A, "gw_"+S1[i], 1)
        asarray(A, "w_" +S1[i], 1)
        asarray(A, "b_" +S1[i], 1)
    }
    for (i=length(S2);i;i--) asarray(A, S2[i], 1)
    return(A)
}

`PS' _ds_sum_invalid_f(`T' A, `SS' s)
{
    if (asarray(A, s)) return(findexternal("_ds_sum_"+s+"_invalid()"))
    return(NULL)
}

`Bool' _ds_sum_omit(`Data' D, `Int' i)
{
    if (D.b[i]<.) return(0)
    D.omit[i] = 1
    D.b[i] = 0
    if (!D.noIF) D.IF[,i] = J(D.N, 1, 0)
    return(1)
}

void _ds_sum_fixed(`Data' D, `Int' i, `RS' b)
{
    D.b[i] = b
    if (D.noIF) return
    D.IF[,i] = J(D.N, 1, 0)
}

`RC' _ds_sum_ccdf(`RC' X, `RC' w, `RS' W)
{   // helper function to accumulate from above; X assumed sorted
    return((quadsum(w) :- _mm_ranks(X, w, 3, 1)) / W)
}

void _ds_sum_gw(`Data' D, `Grp' G, `Int' i, `PS' f, | `PR' o)
{   // helper function to take (weighted) average across Y-groups
    // p_k: relative size of group k
    // b_k: value of statistic in group k
    // b     = sum_k (p_k * b_k)
    // IF(b) = sum_k (p_k * IF(b_k) + b_k * IF(p_k))
    `Int'  k
    `RS'   b, bk, Wk
    `RC'   h, hk
    
    b = 0
    if (D.noIF==0) h = J(G.N, 1, 0)
    k = G.gw_K() // build group index and get number of groups
    for (;k;k--) {
        Wk = G.gw_W(k) // size of group
        bk = (*f)(G.gw_X(k), G.gw_w(k), Wk, D.noIF, hk=., o)
        b  = b + Wk/G.W * bk
        if (D.noIF) continue
        h[G.gw_p(k)] = h[G.gw_p(k)] :+ hk :+ bk
        h = h :- bk * Wk/G.W
    }
    D.b[i] = b
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, h / G.W)
}

void ds_sum_q(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_q(D, G, i, p/100, D.qdef)
}

void ds_sum_p(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_q(D, G, i, p/100, D.qdef)
}

void ds_sum_quantile(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_q(D, G, i, p/100, D.qdef)
}

void ds_sum_hdquantile(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_q(D, G, i, p/100, 10)
}

void ds_sum_mquantile(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_q(D, G, i, p/100, 11)
}

void _ds_sum_q(`Data' D, `Grp' G, `Int' i, `RS' p, `Int' qdef)
{
    
    if (p==0) {; _ds_sum_fixed(D, i, G.Xs()[1]);   return; }
    if (p==1) {; _ds_sum_fixed(D, i, G.Xs()[G.N]); return; }
    D.b[i] = _mm_quantile(G.Xs(), G.ws(), p, qdef, D.wtype==1, D.hdtrim)
    if (D.noIF) return
    ds_set_IF(D, G, i, _ds_sum_q_IF(D, G, p, D.b[i], qdef) / G.W)
}

`RC' _ds_sum_q_IF(`Data' D, `Grp' G, `RS' p, `RS' q, `Int' qdef)
{
    if (qdef==10) return(_ds_sum_q_IF10(G, p, D.hdtrim))
    if (qdef==11) return(_ds_sum_q_IF11(G, p, q))
    return(_ds_sum_q_IFxx(G, p, q, _ds_sum_d(D, G, q)))
}

`RC' _ds_sum_q_IFxx(`Grp' G, `RS' p, `RS' q, `RS' fx)
{
    `RC' z
    
    if (p==0) return(J(G.N, 1, 0))
    if (p==1) return(J(G.N, 1, 0))
    z = (G.X() :<= q)
    return((ds_mean(z, G.w(), G.W) :- z) / fx)
}

`RC' _ds_sum_q_IF10(`Grp' G, `RS' p, `RS' trim)
{ 
    if (p==0) return(J(G.N, 1, 0))
    if (p==1) return(J(G.N, 1, 0))
    return(__ds_sum_q_IF10(G.hdq_F(), G.hdq_dx(), p, G.Neff(), trim)[G.hdq_p()])
}

`RC' __ds_sum_q_IF10(`RC' F, `RC' dx, `RS' p, `RS' n, `RS' trim)
{   // (returns result in reverse order)
    `RS' a, b, wd, l, r
    `RC' h
    
    a = (n + 1) * p
    b = (n + 1) * (1 - p)
    h = betaden(a, b, F) :* dx
    if (trim<1) {
        wd = trim<=0 ? 1 / sqrt(n) : trim
        if      (a<=1 & b<=1) {; l=.   ; r=. ; } // can only happen if n<=1
        else if (a<=1 & b>1 ) {; l=0   ; r=wd; } // left border
        else if (a>1  & b<=1) {; l=1-wd; r=1 ; } // right border
        else {; l = __mm_quantile_hd_l(a, b, wd); r = l+wd; }
        if (l<.) h = (h :* (F:>=l :& F:<=r)) / mm_diff(ibeta(a, b, l\r))
    }
    return(quadrunningsum(h[rows(h)::1]) :- quadsum(h :* F))
}

`RC' _ds_sum_q_IF11(`Grp' G, `RS' p, `RS' q)
{
    `Int' r
    
    if (p==0) return(J(G.N, 1, 0))
    if (p==1) return(J(G.N, 1, 0))
    r = rows(G.mq_x())
    if (r<=1) return(J(G.N, 1, 0))
    mm_hunt(G.mq_F(), p, G.mq_j)
    if      (G.mq_j==0) return(J(G.N, 1, 0)) // below support
    else if (G.mq_j==r) return(J(G.N, 1, 0)) // above support
    return(- G.mq_s(G.mq_j, p, q) *
        __ds_sum_q_IF11(p, G.X(), G.mq_j, G.mq_x(), G.mq_F()))
}

`RC' __ds_sum_q_IF11(`RS' p, `RC' X, `Int' j, `RC' x, `RC' F)
{
    `RS' x1, x2, F1, F2
    
    x1 = x[j]; x2 = x[j+1]
    F1 = F[j]; F2 = F[j+1]
    return(((F2-p)/(F2-F1)) * (((X:<=x1) - (X:==x1)/2) :- F1) +
           ((p-F1)/(F2-F1)) * (((X:<=x2) - (X:==x2)/2) :- F2))
}

void ds_sum_d(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    ds_sum_density(D, G, i, at)
}

void ds_sum_density(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RC' z
    
    D.b[i] = _ds_sum_d(D, G, at)
    if (D.noIF) return
    z = ds_invp(G.N, G.pX(), D.S.K(D.S.X(), at, D.S.h()*D.S.l()))
    if (D.exact) ds_set_IF(D, G, i, (z :- D.b[i])/G.W)
    else         ds_set_IF(D, G, i, (z :- ds_mean(z, G.w(), G.W))/G.W)
}

`RC' _ds_sum_d(`Data' D, `Grp' G, `RC' at)
{
    ds_set_density(D, G) // prepare density object if needed
    return(D.S.d(at, D.exact))
}

void ds_sum_histogram(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' c, lo, up
    `RC' z
    
    lo = o[1]; up = o[2]
    if (lo>up) D.b[i] = .
    else {
        c = 1 / (up - lo)
        if (c>=.) c = 0 // zero width bin
        z = G.X():>lo :& G.X():<=up
        D.b[i] = ds_mean(z, G.w(), G.W) * c
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (z*c :- D.b[i])/G.W)
}

void ds_sum_cdf(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RC' z
    
    z = G.X():<=at
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void ds_sum_mcdf(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RS' C, P
    `RC' z, zp
    
    z  = G.X():<at
    zp = G.X():==at
    C  = ds_mean(z, G.w(), G.W)
    P  = ds_mean(zp, G.w(), G.W)
    D.b[i] = C + P/2
    if (D.noIF) return
    ds_set_IF(D, G, i, ((z :- C) + (zp :- P)/2) / G.W)
}

void ds_sum_fcdf(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RC' z
    
    z = G.X():<at
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void ds_sum_ccdf(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RC' z
    
    z = G.X():>at
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void ds_sum_mccdf(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RS' C, P
    `RC' z, zp
    
    z  = G.X():>at
    zp = G.X():==at
    C  = ds_mean(z, G.w(), G.W)
    P  = ds_mean(zp, G.w(), G.W)
    D.b[i] = C + P/2
    if (D.noIF) return
    ds_set_IF(D, G, i, ((z :- C) + (zp :- P)/2) / G.W)
}

void ds_sum_fccdf(`Data' D, `Grp' G, `Int' i, `RS' at)
{
    `RC' z
    
    z = G.X():>=at
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void ds_sum_proportion(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_freq(D, G, i, o[1], o[2], 0)
    if (D.omit[i]) return
    D.b[i] = D.b[i] / G.W
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / G.W
}

void ds_sum_pct(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_freq(D, G, i, o[1], o[2], 0)
    if (D.omit[i]) return
    D.b[i] = D.b[i] * (100 / G.W)
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * (100/G.W)
}

void ds_sum_f(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_freq(D, G, i, o[1], o[2], 1)
}

void ds_sum_frequency(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_freq(D, G, i, o[1], o[2], 1)
}

void ds_sum_count(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_freq(D, G, i, o[1], o[2], 1)
}

void _ds_sum_freq(`Data' D, `Grp' G, `Int' i, `RS' lo, `RS' up, `Bool' tot)
{
    `RC' z
    
    if (lo==.z) {   // overall count
        D.b[i] = G.W
        if (D.noIF) return
        if (tot) D.IFtot[i] = D.b[i]
        ds_set_IF(D, G, i, J(G.N,1,0), 0, 1)
        return
    }
    if (up==.z) {   // count x = #
        z = G.X():==lo
        D.b[i] = quadsum(G.w():*z)
    }
    else {          // count x in [lo,up]
        if (lo>up) D.b[i] = .
        else {
            z = G.X():>=lo :& G.X():<=up
            D.b[i] = quadsum(G.w():*z)
        }
        if (_ds_sum_omit(D, i)) return
    }
    if (D.noIF) return
    if (tot) D.IFtot[i] = D.b[i]
    ds_set_IF(D, G, i, (z :- (D.b[i]/G.W)), 0, D.b[i]/G.W)
}

void ds_sum_total(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' lo, up
    `RC' z
    
    lo = o[1]; up = o[2]
    if      (lo==.z) z = G.X()
    else if (up==.z) z = G.X() :* (G.X():==lo)
    else {
        if (lo>up) z = J(G.N, 1, .) // so that D.b[i]=.
        else       z = G.X() :* (G.X():>=lo :& G.X():<=up)
    }
    D.b[i] = quadsum(G.w():*z, 1)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    D.IFtot[i] = D.b[i]
    ds_set_IF(D, G, i, (z :- (D.b[i]/G.W)), 0, D.b[i]/G.W)
}

void ds_sum_min(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_fixed(D, i, min(G.X()))
}

void ds_sum_max(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_fixed(D, i, max(G.X()))
}

void ds_sum_range(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_fixed(D, i, mm_diff(minmax(G.X())))
}

void ds_sum_midrange(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_fixed(D, i, sum(minmax(G.X()))/2)
}

void ds_sum_mean(`Data' D, `Grp' G, `Int' i)
{
    D.b[i] = G.mean()
    if (D.noIF) return
    ds_set_IF(D, G, i, (G.X() :- D.b[i]) / G.W)
}

void ds_sum_gmean(`Data' D, `Grp' G, `Int' i)
{
    `RC' z
    
    z = ln(G.X())
    D.b[i] = exp(ds_mean(z, G.w(), G.W))
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- ln(D.b[i])) * (D.b[i] / G.W))
}

`BoolC' _ds_sum_gmean_invalid(`Data' D, `Int' j, `RC' X)
{   // registered in _ds_sum_invalid_dict()
    return(_ds_sum_invalid(D, j, "gmean", ln(X)))
}

void ds_sum_hmean(`Data' D, `Grp' G, `Int' i)
{
    `RC' z
    
    if (anyof(G.X(), 0)) {
        _ds_sum_fixed(D, i, 0)
        return
    }
    z = 1 :/ G.X()
    D.b[i] = 1 / ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    if (_ds_sum_omit(D, i)) return
    ds_set_IF(D, G, i, (z :- 1/D.b[i]) * (-D.b[i]^2 / G.W))
}

void ds_sum_trim(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS'   p1, p2, plo, pup
    `RC'   z, q
    `IntC' lo, up, mid
    
    p1 = o[1]/100
    if (o[2]==.z) p2 = 1 - p1
    else          p2 = 1 - o[2]/100
    q = _mm_quantile(G.Xs(), G.ws(), p1\p2, D.qdef, D.wtype==1, D.hdtrim)
    lo = (p1>0 ? G.X():<=q[1] : J(G.N, 1, 0)) // tag obs <= lower quantile
    up = (p2<1 ? G.X():>=q[2] : J(G.N, 1, 0)) // tag obs >= upper quantile
    mid = !lo :& !up  // tag obs in (lower quantile, upper quantile)
    D.b[i] = ds_mean(G.X(), G.w():*mid)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    plo = (p1>0 ? quadsum(G.w():*lo)/G.W : 0) // exact proportion excluded from below
    pup = (p2<1 ? quadsum(G.w():*up)/G.W : 0) // exact proportion excluded from above
    z = G.X():*mid
    if (p1>0) z = z + q[1]*(lo :- plo)
    if (p2<1) z = z + q[2]*(up :- pup)
    z = z / (1-plo-pup)
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
    // main part:      (X:*mid/(1-plo-pup) :- D.b[i]) / G.W
    // lower quantile: (q[1]*(lo :- plo)) / ((1-plo-pup)*G.W)
    // upper quantile: (q[2]*(up :- pup)) / ((1-plo-pup)*G.W)
}

void ds_sum_winsor(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS'   p1, p2, plo, pup
    `RC'   z, q
    `IntC' lo, up, mid
    
    p1 = o[1]/100
    if (o[2]==.z) p2 = 1 - p1
    else          p2 = 1 - o[2]/100
    q = _mm_quantile(G.Xs(), G.ws(), p1\p2, D.qdef, D.wtype==1, D.hdtrim)
    lo = (p1>0 ? G.X():<=q[1] : J(G.N, 1, 0)) // tag obs <= lower quantile
    up = (p2<1 ? G.X():>=q[2] : J(G.N, 1, 0)) // tag obs >= upper quantile
    mid = !lo :& !up  // tag obs in (lower quantile, upper quantile)
    plo = (p1>0 ? quadsum(G.w():*lo)/G.W : 0) // exact proportion excluded from below
    pup = (p2<1 ? quadsum(G.w():*up)/G.W : 0) // exact proportion excluded from above
    z = G.X():*mid
    if (p1>0) z = z + q[1]:*lo
    if (p2<1) z = z + q[2]:*up
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    if (p1>0) z = z :- q[1]*plo
    if (p2<1) z = z :- q[2]*pup
    z = z / (1-plo-pup)
    // note: winsorized mean = weighted sum of trimmed mean and quantiles
    z = (1-plo-pup) * (z :- ds_mean(z, G.w(), G.W))              // trimmed mean
    if (p1>0) z = z + plo * _ds_sum_q_IF(D, G, p1, q[1], D.qdef) // lower quantile
    if (p2<1) z = z + pup * _ds_sum_q_IF(D, G, p2, q[2], D.qdef) // upper quantile
    ds_set_IF(D, G, i, z / G.W)
}

void ds_sum_median(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_q(D, G, i, .5, D.qdef)
}

void ds_sum_huber(`Data' D, `Grp' G, `Int' i, `RS' eff)
{
    `RS' s, k, t
    `RC' z, phi, h
    `T'  S
    
    s = __ds_sum_mad_med_med(D, G, h=., t=.) / invnormal(0.75)
    S = mm_mloc(G.X(), G.w(), eff, "huber", t, s)
    D.b[i] = mm_mloc_b(S)
    if (D.noIF) return
    k = mm_mloc_k(S)
    z = (G.X() :- D.b[i]) / s
    phi = mm_huber_phi(z, k)
    ds_set_IF(D, G, i, (s * mm_huber_psi(z, k) 
        - (ds_mean(phi:*z, G.w(), G.W)/invnormal(0.75) * h))
        / (ds_mean(phi, G.w(), G.W) * G.W))
}

void ds_sum_biweight(`Data' D, `Grp' G, `Int' i, `RS' eff)
{
    `RS' s, k, t
    `RC' z, phi, h
    `T'  S
    
    s = __ds_sum_mad_med_med(D, G, h=., t=.) / invnormal(0.75)
    S = mm_mloc(G.X(), G.w(), eff, "biweight", t, s)
    D.b[i] = mm_mloc_b(S)
    if (D.noIF) return
    k = mm_mloc_k(S)
    z = (G.X() :- D.b[i]) / s
    phi = mm_biweight_phi(z, k)
    ds_set_IF(D, G, i, (s * mm_biweight_psi(z, k) 
        - (ds_mean(phi:*z, G.w(), G.W)/invnormal(0.75) * h))
        / (ds_mean(phi, G.w(), G.W) * G.W))
}

void ds_sum_hl(`Data' D, `Grp' G, `Int' i)
{
    `RC' z, F, d
    
    D.b[i] = mm_hl(G.Xs(), G.ws(), D.wtype==1)
    if (D.noIF) return
    z = 2*D.b[i] :- G.Xs()
    F = _mm_relrank(G.Xs(), G.ws(), z[G.N::1], 1)[G.N::1]
    d = _ds_sum_d(D, G, z)
    F[G.pX()] = F
    ds_set_IF(D, G, i, (ds_mean(F, G.w(), G.W) :- F) / 
                       (ds_mean(d, G.ws(), G.W) * G.W))
    // using mean(F, G.w) instead of 0.5 so that total of IF is exactly zero
}

void ds_sum_sd(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    ds_sum_variance(D, G, i, df)
    if (D.b[i]==0) return // too few observations
    D.b[i] = sqrt(D.b[i])
    if (D.noIF) return
    if (D.b[i]==0) return
    D.IF[,i] = D.IF[,i] / (2 * D.b[i])
}

void ds_sum_variance(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS' m, c
    pragma unset m
    
    D.b[i] = _ds_variance(G.X(), G.w(), G.W, D.wtype, df, m)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    ds_set_IF(D, G, i, (c*(G.X() :- m):^2 :- D.b[i]) / G.W)
}

`RS' _ds_variance(`RC' X, `RC' w, `RS' W, `Int' wtype, | `RS' df, `RS' m)
{    // replaces m
    `RC' v
    
    if (args()<5) df = 1
    m = ds_mean(X, w, W)
    v = quadcrossdev(X,m, w, X,m) / W
    if (df) {
        if (wtype==1) v = v * (W / (W - df))
        else          v = v * (rows(X) / (rows(X) - df))
    }
    return(v)
}

void ds_sum_mse(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' m, df, c
    
    m = o[1]; df = o[2]
    D.b[i] = quadcrossdev(G.X(),m, G.w(), G.X(),m) / G.W
    if (df!=0) {
        if (D.wtype==1)  D.b[i] = D.b[i] * (G.W / (G.W - df))
        else             D.b[i] = D.b[i] * (G.N / (G.N - df))
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    ds_set_IF(D, G, i, (c*(G.X() :- m):^2 :- D.b[i]) / G.W)
}

void ds_sum_rmse(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    ds_sum_mse(D, G, i, o)
    if (D.b[i]==0) return // too few observations
    D.b[i] = sqrt(D.b[i])
    if (D.noIF) return
    if (D.b[i]==0) return
    D.IF[,i] = D.IF[,i] / (2 * D.b[i])
}

void ds_sum_iqr(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RC' p, q
    
    p = o'/100
    q = _mm_quantile(G.Xs(), G.ws(), p, D.qdef, D.wtype==1, D.hdtrim)
    D.b[i] = q[2] - q[1]
    if (D.noIF) return
    ds_set_IF(D, G, i, (_ds_sum_q_IF(D, G, p[2], q[2], D.qdef)
                      - _ds_sum_q_IF(D, G, p[1], q[1], D.qdef)) / G.W)
}

void ds_sum_iqrn(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' c

    ds_sum_iqr(D, G, i, o)
    c = 1 / (invnormal(o[2]/100) - invnormal(o[1]/100))
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void ds_sum_mad(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    if (o[1] & o[2]) _ds_sum_mad_mean_mean(D, G, i)
    else if (o[1])   _ds_sum_mad_mean_med(D, G, i)
    else if (o[2])   _ds_sum_mad_med_mean(D, G, i)
    else             _ds_sum_mad_med_med(D, G, i)
}

void _ds_sum_mad_mean_mean(`Data' D, `Grp' G, `Int' i)
{
    `RS' t
    `RC' Z, U
    
    t = G.mean()
    U = G.X() :- t
    Z = abs(U)
    D.b[i] = ds_mean(Z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, ((Z :- D.b[i]) :- ds_mean(sign(U),G.w(),G.W) * U) / G.W)
}

void _ds_sum_mad_med_mean(`Data' D, `Grp' G, `Int' i)
{
    `RS'   t
    `IntC' p
    `RC'   Z, Zs, ws, U, h
    
    t = G.mean()
    U = G.X() :- t
    Z = abs(U)
    if (D.qdef!=10 & D.qdef!=11) {
        D.b[i] = mm_median(Z, G.w(), D.qdef, D.wtype==1) // Z not sorted!
        if (D.noIF) return
        ds_set_IF(D, G, i, _ds_sum_mad_med_IFxx(D, G, D.b[i], Z, t, U) / G.W)
        return
    }
    p  = mm_order(Z,1,1) // stable sort
    Zs = Z[p]
    ws = rows(G.w())==1 ? G.w() : G.w()[p]
    D.b[i] = _mm_median(Zs, ws, D.qdef, D.wtype==1, D.hdtrim)
    if (D.noIF) return
    if (D.qdef==10) h = _ds_sum_mad_med_IF10(G, D.b[i], t, U, Zs, ws, p, D.hdtrim)
    else            h = _ds_sum_mad_med_IF11(G, D.b[i], Z, t, U, Zs, ws)
    ds_set_IF(D, G, i, h / G.W)
}

void _ds_sum_mad_mean_med(`Data' D, `Grp' G, `Int' i)
{
    `RS' t
    `RC' Z, U

    t = _mm_median(G.Xs(), G.ws(), D.qdef, D.wtype==1, D.hdtrim)
    U = G.X() :- t
    Z = abs(U)
    D.b[i] = ds_mean(Z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, ((Z :- D.b[i]) :- ds_mean(sign(U),G.w(),G.W) *
        _ds_sum_q_IF(D, G, .5, t, D.qdef)) / G.W)
}

void _ds_sum_mad_med_med(`Data' D, `Grp' G, `Int' i)
{
    `RC' h
    
    D.b[i] = __ds_sum_mad_med_med(D, G, h=.)
    if (D.noIF) return
    ds_set_IF(D, G, i, h / G.W)
}

`RS' __ds_sum_mad_med_med(`Data' D, `Grp' G, | `RC' h, `RS' t)
{   // h an t will be filled in
    `RS'   b
    `IntC' p
    `RC'   Z, Zs, ws

    t = _mm_median(G.Xs(), G.ws(), D.qdef, D.wtype==1, D.hdtrim)
    Z  = abs(G.X() :- t)
    if (D.qdef!=10 & D.qdef!=11) {
        b = mm_median(Z, G.w(), D.qdef, D.wtype==1) // Z not sorted!
        if (D.noIF) return(b)
        h = _ds_sum_q_IF(D, G, .5, t, D.qdef) // IF of median
        h = _ds_sum_mad_med_IFxx(D, G, b, Z, t, h)
        return(b)
    }
    p  = mm_order(Z,1,1) // stable sort
    Zs = Z[p]
    ws = rows(G.w())==1 ? G.w() : G.w()[p]
    b  = _mm_median(Zs, ws, D.qdef, D.wtype==1, D.hdtrim)
    if (D.noIF) return(b)
    h = _ds_sum_q_IF(D, G, .5, t, D.qdef) // IF of median
    if (D.qdef==10) h = _ds_sum_mad_med_IF10(G, b, t, h, Zs, ws, p, D.hdtrim)
    else            h = _ds_sum_mad_med_IF11(G, b, Z, t, h, Zs, ws)
    return(b)
}

`RC' _ds_sum_mad_med_IFxx(`Data' D, `Grp' G, `RS' l, `RC' Z, `RS' t, `RC' U)
{
    `RC' z, d
    
    d = _ds_sum_d(D, G, (t - l) \ (t + l))
    z = (Z :<= l)
    return(((ds_mean(z, G.w(), G.W) :- z) - mm_diff(d) * U) / sum(d))
}

`RC' _ds_sum_mad_med_IF10(`Grp' G, `RS' l, `RS' t, `RC' U, `RC' Zs, `RC' ws,
    `IntC' p, `RS' trim)
{
    `RC' d
    `RM' cdf
    
    // obtain "density" of X at t-l and t+l; to be consistent with how the IF
    // is constructed for hdquantiles, the inverse of the total jump in the
    // influence function is used to recover the "density"
    cdf = _mm_ecdf2(G.Xs(), G.ws(), 1) // mid-cdf
    d = _ds_sum_mad_med_IF10_d(G.hdq_F(), G.hdq_dx(),
            mm_fastipolate(cdf[,1], cdf[,2], (t-l)\(t+l), 1), // p at t+/-l
            G.Neff(), trim)
    // influence function
    return(_ds_sum_mae_med_IF10(G, Zs, ws, p, trim) - mm_diff(d)/sum(d) * U)
}

`RC' _ds_sum_mad_med_IF10_d(`RC' F, `RC' dx, `RC' p, `RS' n, `RS' trim)
{   // X assumed sorted
    return(1/mm_diff(__ds_sum_q_IF10(F, dx, p[1], n, trim)[rows(F)\1]) \
           1/mm_diff(__ds_sum_q_IF10(F, dx, p[2], n, trim)[rows(F)\1]))
}

`RC' _ds_sum_mad_med_IF11(`Grp' G, `RS' l, `RC' Z, `RS' t, `RC' U,
    `RC' Zs, `RC' ws)
{
    `Int'  j, r
    `RS'   bw, s, c, d
    `RC'   x, F, sp, p
    `T'    S
    
    // sparsity function of Z = abs(X-t)
    F  = _mm_ecdf2(Zs, ws, 1)
    x  = F[,1]; F = F[,2]
    r = rows(x)
    if (r<=1) return(J(G.N, 1, 0))
    mm_hunt(F, .5, j=1)
    if      (j==0) return(J(G.N, 1, 0)) // below support
    else if (j==r) return(J(G.N, 1, 0)) // above support
    if (G.mqopt.cdf) {
        sp = _ds_mq_sp(x, F)
        if (G.mqopt.bw<.) bw = G.mqopt.bw / 2
        else              bw = .5 / ceil(2 * G.Neff()^(2/5))
        if (bw==0) s = sp[j]
        else       s = __ds_mq_sp(sp, F, .5, bw, j)
    }
    else {
        S = _ds_mq_d_init(c=., x, F, G.Neff(), G.mqopt.us)
        s = 1 / (_ds_mq_d(S, l) * c)
    }
    // difference of "density" of X at t-l and t+l 
    p = mm_fastipolate(G.mq_x(), G.mq_F(), (t-l)\(t+l), 1)
    mm_hunt(G.mq_F(), p[1], G.mq_j)
    d = 1/G.mq_s(G.mq_j, p[1], t-l)
    mm_hunt(G.mq_F(), p[2], G.mq_j)
    d = 1/G.mq_s(G.mq_j, p[2], t+l) - d
    // influence function
    return(- s * (__ds_sum_q_IF11(.5, Z, j, x, F) + d * U))
}

void ds_sum_madn(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' c
    
    ds_sum_mad(D, G, i, o)
    if (o[1]) c = sqrt(pi() / 2)
    else      c = 1 / invnormal(0.75)
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void ds_sum_mae(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    if (o[1]) _ds_sum_mae_mean(D, G, i, o[2])
    else      _ds_sum_mae_med(D, G, i, o[2])
}

void _ds_sum_mae_mean(`Data' D, `Grp' G, `Int' i, `RS' t)
{
    `RC' z
    
    z = abs(G.X() :- t)
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void _ds_sum_mae_med(`Data' D, `Grp' G, `Int' i, `RS' t)
{
    `IntC' p
    `RC'   Z, Zs, ws, h
    
    Z = abs(G.X() :- t)
    if (D.qdef!=10 & D.qdef!=11) {
        D.b[i] = mm_median(Z, G.w(), D.qdef, D.wtype==1) // Z not sorted!
        if (D.noIF) return
        ds_set_IF(D, G, i, _ds_sum_mae_med_IFxx(D, G, D.b[i], Z, t) / G.W)
        return
    }
    p  = mm_order(Z,1,1) // stable sort
    Zs = Z[p]
    ws = rows(G.w())==1 ? G.w() : G.w()[p]
    D.b[i] = _mm_median(Zs, ws, D.qdef, D.wtype==1, D.hdtrim)
    if (D.noIF) return
    if (D.qdef==10) h = _ds_sum_mae_med_IF10(G, Zs, ws, p, D.hdtrim)
    else            h = _ds_sum_mae_med_IF11(G, D.b[i], Z, Zs, ws)
    ds_set_IF(D, G, i, h / G.W)
}

`RC' _ds_sum_mae_med_IFxx(`Data' D, `Grp' G, `RS' l, `RC' Z, `RS' t)
{
    `RC' z
    
    z = (Z :<= l)
    return((ds_mean(z, G.w(), G.W) :- z) / sum(_ds_sum_d(D, G, (t-l)\(t+l))))
}

`RC' _ds_sum_mae_med_IF10(`Grp' G, `RC' Z, `RC' w, `IntC' p, `RS' trim)
{   // Z assumed sorted
    `Int'  N, k
    `IntC' pinv
    `RC'   F, dx
    
    _ds_hdq_cdf(Z, w, F=., dx=.)
    N = rows(Z); k = rows(F)
    if (k==N) pinv = invorder(revorder(p))
    else {
        pinv    = J(N, 1, .) 
        pinv[p] = runningsum(k \ -(Z[|2\N|]:!=Z[|1\N-1|]))
    }
    return(__ds_sum_q_IF10(F, dx, .5, G.Neff(), trim)[pinv])
}

`RC' _ds_sum_mae_med_IF11(`Grp' G, `RS' l, `RC' Z, `RC' Zs, `RC' ws)
{
    `Int'  j, r
    `RS'   bw, s, c
    `RC'   x, F, sp
    `T'    S
    
    F  = _mm_ecdf2(Zs, ws, 1)
    x  = F[,1]; F = F[,2]
    r = rows(x)
    if (r<=1) return(J(G.N, 1, 0))
    mm_hunt(F, .5, j=1)
    if      (j==0) return(J(G.N, 1, 0)) // below support
    else if (j==r) return(J(G.N, 1, 0)) // above support
    if (G.mqopt.cdf) {
        sp = _ds_mq_sp(x, F)
        if (G.mqopt.bw<.) bw = G.mqopt.bw / 2
        else              bw = .5 / ceil(2 * G.Neff()^(2/5))
        if (bw==0) s = sp[j]
        else       s = __ds_mq_sp(sp, F, .5, bw, j)
    }
    else {
        S = _ds_mq_d_init(c=., x, F, G.Neff(), G.mqopt.us)
        s = 1 / (_ds_mq_d(S, l) * c)
    }
    return(- s * __ds_sum_q_IF11(.5, Z, j, x, F))
}

void ds_sum_maen(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' c
    
    ds_sum_mae(D, G, i, o)
    if (o[1]) c = sqrt(pi() / 2)
    else      c = 1 / invnormal(0.75)
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void ds_sum_md(`Data' D, `Grp' G, `Int' i)
{
    `RS' m, cov
    `RC' F, B
    
    m   = G.mean()
    F   = _mm_ranks(G.Xs(), G.ws(), 3, 1, 1)
    cov = quadcross(G.Xs(), G.ws(), F) / G.W
    D.b[i] = 4*cov - 2*m // note: MD = 2 * mean * Gini
    if (D.noIF) return
    // In analogy to the Gini coefficient the main moment condition of the MD
    // can be written as h = 2*(2*F - 1)*X - MD
    B = ds_invp(G.N, G.pX(), _ds_sum_ccdf(G.Xs(), G.Xs():*G.ws(), G.W))
    F[G.pX()] = F
    ds_set_IF(D, G, i, ((4*F :- 2):*G.X() :- D.b[i] :+ 4*(B :- cov)) / G.W)
}

void ds_sum_mdn(`Data' D, `Grp' G, `Int' i)
{
    `RC' c
    
    ds_sum_md(D, G, i)
    c = sqrt(pi())/2
    D.b[i] = D.b[i] * c
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] * c
}

void ds_sum_mscale(`Data' D, `Grp' G, `Int' i, `RS' bp)
{
    `RS' k, med, delta
    `RC' z, psi
    `T'  S
    
    med = _mm_median(G.Xs(), G.ws(), D.qdef, D.wtype==1, D.hdtrim)
    S = mm_mscale(G.X(), G.w(), bp, ., med)
    D.b[i] = mm_mscale_b(S)
    if (D.noIF) return
    k = mm_mscale_k(S)
    delta = mm_mscale_delta(S)
    z = (G.X() :- med) / D.b[i]
    psi = mm_biweight_psi(z, k)
    ds_set_IF(D, G, i, (D.b[i] * (mm_biweight_rho(z, k) :- delta)
        - ds_mean(psi, G.w(), G.W) * _ds_sum_q_IF(D, G, .5, med, D.qdef))
        / (ds_mean(psi:*z, G.w(), G.W) * G.W))
}

void ds_sum_qn(`Data' D, `Grp' G, `Int' i)
{
    `RC' c
    `RC' z1, z2, F, d
    
    D.b[i] = mm_qn(G.Xs(), G.ws(), D.wtype==1)
    if (D.noIF) return
    c  = sqrt(2) * invnormal(5/8)
    z1 = G.Xs() :+ D.b[i]*c
    z2 = G.Xs() :- D.b[i]*c
    F  = _mm_relrank(G.Xs(), G.ws(), z1, 1) :- _mm_relrank(G.Xs(), G.ws(), z2, 1)
    d  = _ds_sum_d(D, G, z1)
    F[G.pX()] = F
    ds_set_IF(D, G, i, (ds_mean(F, G.w(), G.W) :- F) * 
        (1 / (c * ds_mean(d, G.ws(), G.W) * G.W)))
}

void ds_sum_skewness(`Data' D, `Grp' G, `Int' i)
{
    `RS' m, sd
    `RC' z
    pragma unset m
    pragma unset sd
    
    D.b[i] = _ds_sum_skewness(G.X(), G.w(), G.W, m, sd)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    z = (G.X() :- m) / sd 
    ds_set_IF(D, G, i, (z:^3 :- 3*z :- D.b[i]*(3/2)*(z:^2:-1) :- D.b[i]) / G.W)
}

`RS' _ds_sum_skewness(`RC' X, `RC' w, `RS' W, | `Rs' m, `Rs' sd) 
{   // replaces m and sd
    m  = ds_mean(X, w, W)
    sd = sqrt(quadcrossdev(X,m, w, X,m)/W) // sd without df correction
    return(ds_mean((X:-m):^3, w) / sd^3)
}

void ds_sum_qskew(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RC' p, q
    `RC' h2
    
    p = a/100 \ 0.5 \ 1 - a/100
    q = _mm_quantile(G.Xs(), G.ws(), p, D.qdef, D.wtype==1, D.hdtrim)
    D.b[i] = (q[1] + q[3] - 2*q[2]) / (q[3] - q[1])
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h2 = _ds_sum_q_IF(D, G, p[2], q[2], D.qdef)
    ds_set_IF(D, G, i,
         ( (q[3]-q[2]) * (_ds_sum_q_IF(D, G, p[1], q[1], D.qdef) - h2)
         - (q[2]-q[1]) * (h2 - _ds_sum_q_IF(D, G, p[3], q[3], D.qdef))
         ) * (2 / ((q[3] - q[1])^2 * G.W)))
}

void ds_sum_mc(`Data' D, `Grp' G, `Int' i)
{
    `RS' mc, q, dq, dH
    `RC' z, F1, F2, d, IF
    
    D.b[i] = mm_mc(G.Xs(), G.ws(), D.wtype==1)
    if (D.noIF) return
    mc = D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // IF not defined
    q  = _mm_median(G.Xs(), G.ws())
    dq = _ds_sum_d(D, G, q)
    F1 = _ds_sum_mc_F(G, z = (G.X() * (mc-1) :+ 2*q) / (mc+1))
    F2 = _ds_sum_mc_F(G,     (G.X() * (mc+1) :- 2*q) / (mc-1))
    d  = _ds_sum_d(D, G, z) :* (G.X() :>= q)
    dH = ds_mean(8 * d :* ((G.X() :- q) / (mc+1)^2), G.w(), G.W)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // IF not defined
    IF = (1 :- 4*F1:*(G.X():>q) :- 4*(F2:-.5):*(G.X():<q) 
        :+ sign(G.X():-q)*(1 - 4*ds_mean(d, G.w(), G.W)/(dq*(mc+1)))) / dH
    IF = IF :- ds_mean(IF, G.w(), G.W) // make sure that IF is centered at zero
    ds_set_IF(D, G, i, IF / G.W)
}

`RC' _ds_sum_mc_F(`Grp' G, `RC' z)
{
    `RC' F, p
    
    p = order(z, 1)
    F = J(rows(z), 1, .)
    F[p] = _mm_relrank(G.Xs(), G.ws(), z[p], 1)
    return(F)
}

void ds_sum_kurtosis(`Data' D, `Grp' G, `Int' i)
{
    `RS' sd, sk
    `RC' m, z
    
    m = G.mean()
    sd = sqrt(quadcrossdev(G.X(),m, G.w(), G.X(),m)/G.W) // sd without df correction
    z = G.X() :- m
    D.b[i] = ds_mean(z:^4, G.w(), G.W) / sd^4
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    sk = ds_mean(z:^3, G.w(), G.W) / sd^3
    z = z / sd
    ds_set_IF(D, G, i, ((z:^2 :- D.b[i]):^2 :- 4*sk*z :- 
        D.b[i]*(D.b[i]-1)) / G.W) 
}

void ds_sum_ekurtosis(`Data' D, `Grp' G, `Int' i)
{
    ds_sum_kurtosis(D, G, i)
    D.b[i] = D.b[i] - 3
}

void ds_sum_qw(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RC' p, q
    
    p = a/200 \ a/100 \ .5 - a/200 \ .5 + a/200 \ 1 - a/100 \ 1 - a/200
    q = _mm_quantile(G.Xs(), G.ws(), p, D.qdef, D.wtype==1, D.hdtrim)
    D.b[i] = (q[6] - q[4] + q[3] - q[1])/ (q[5] - q[2])
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (
        (q[5]-q[2]) * (_ds_sum_q_IF(D, G, p[6], q[6], D.qdef)
                     - _ds_sum_q_IF(D, G, p[4], q[4], D.qdef)
                     + _ds_sum_q_IF(D, G, p[3], q[3], D.qdef)
                     - _ds_sum_q_IF(D, G, p[1], q[1], D.qdef))
      - (q[6] - q[4] + q[3] - q[1]) * (_ds_sum_q_IF(D, G, p[5], q[5], D.qdef)
                                     - _ds_sum_q_IF(D, G, p[2], q[2], D.qdef))
      ) / ((q[5]-q[2])^2 * G.W))
}

void ds_sum_lqw(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RC' p, q, h2
    
    p = a/200 \ .25 \ .5 - a/200
    q = _mm_quantile(G.Xs(), G.ws(), p, D.qdef, D.wtype==1, D.hdtrim)
    D.b[i] = - (q[1] + q[3] - 2*q[2]) / (q[3] - q[1])
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h2 = _ds_sum_q_IF(D, G, p[2], q[2], D.qdef)
    ds_set_IF(D, G, i, (
        (q[2]-q[1]) * (h2 - _ds_sum_q_IF(D, G, p[3], q[3], D.qdef))
      - (q[3]-q[2]) * (_ds_sum_q_IF(D, G, p[1], q[1], D.qdef) - h2)
      ) * (2 / ((q[3] - q[1])^2 * G.W)))
}

void ds_sum_rqw(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RC' p, q, h2
    
    p = .5 + a/200 \ .75 \ 1 - a/200
    q = _mm_quantile(G.Xs(), G.ws(), p, D.qdef, D.wtype==1, D.hdtrim)
    D.b[i] = (q[1] + q[3] - 2*q[2]) / (q[3] - q[1])
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h2 = _ds_sum_q_IF(D, G, p[2], q[2], D.qdef)
    ds_set_IF(D, G, i, (
        (q[3]-q[2]) * (_ds_sum_q_IF(D, G, p[1], q[1], D.qdef) - h2)
      - (q[2]-q[1]) * (h2 - _ds_sum_q_IF(D, G, p[3], q[3], D.qdef))
      ) * (2 / ((q[3] - q[1])^2 * G.W)))
}

void ds_sum_lmc(`Data' D, `Grp' G, `Int' i)
{
    `RS' med, mc, q, dq, dH, gmed, Fmed
    `RC' p, z, F1, F2, d, IF
    
    med = _mm_median(G.Xs(), G.ws())
    p = selectindex(G.Xs():<med)
    if (rows(p)==0) D.b[i] = . // no obs
    else D.b[i] = -mm_mc(G.Xs()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()), D.wtype==1)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    mc = -D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // IF not defined
    q  = _mm_median(G.Xs()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()))
    dq = _ds_sum_d(D, G, q)
    F1 = _ds_sum_mc_F(G, z = (G.X() * (mc-1) :+ 2*q) / (mc+1))
    F2 = _ds_sum_mc_F(G,     (G.X() * (mc+1) :- 2*q) / (mc-1))
    d  = _ds_sum_d(D, G, z) :* (G.X():>=q :& G.X():<=med)
    dH = ds_mean(32 * d :* ((q :- G.X()) / (mc+1)^2), G.w(), G.W)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // IF not defined
    gmed = (med*(mc-1) + 2*q) / (mc+1)
    Fmed = _mm_relrank(G.Xs(), G.ws(), gmed, 1)
    IF = (1 :- 16*F1:*(G.X():>q :& G.X():<med) 
            :- 16*(F2:-.25):*(G.X():<q :& G.X():>gmed)
            :-  4*(G.X():<gmed)
            :-  8*sign(G.X():-med)*Fmed
            :+ (.25:-(G.X():<q))*(4 - 32*ds_mean(d, G.w(), G.W)/(dq*(mc+1)))) /
            dH
    IF = IF :- ds_mean(IF, G.w(), G.W) // make sure that IF is centered at zero
    ds_set_IF(D, G, i, IF / G.W)
}

void ds_sum_rmc(`Data' D, `Grp' G, `Int' i)
{
    `RS' med, mc, q, dq, dH, gmed, Fmed
    `RC' p, z, F1, F2, d, IF
    
    med = _mm_median(G.Xs(), G.ws())
    p = selectindex(G.Xs():>med)
    if (rows(p)==0) D.b[i] = . // no obs
    else D.b[i] = mm_mc(G.Xs()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()), D.wtype==1)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    mc = D.b[i]
    if (abs(mc)==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    q  = _mm_median(G.Xs()[p], (rows(G.ws())!=1 ? G.ws()[p]: G.ws()))
    dq = _ds_sum_d(D, G, q)
    F1 = 1 :- _ds_sum_mc_F(G, z = (G.X() * -(mc+1) :+ 2*q) /  (1-mc))
    F2 = 1 :- _ds_sum_mc_F(G,     (G.X() *  (1-mc) :- 2*q) / -(mc+1))
    d  = _ds_sum_d(D, G, z) :* (G.X():<=q :& G.X():>=med)
    dH = ds_mean(32 * d :* ((G.X() :- q) / (1-mc)^2), G.w(), G.W)
    if (dH==1) {; D.IF[,i] = J(D.N, 1, 0); return; } // SEs not defined
    gmed = (med*(mc+1) - 2*q) / (mc-1)
    Fmed = 1 - _mm_relrank(G.Xs(), G.ws(), gmed, 1)
    IF = (1 :- 16*F1:*(G.X():<q :& G.X():>med) 
            :- 16*(F2:-.25):*(G.X():>q :& G.X():<gmed)
            :-  4*(G.X():>gmed)
            :-  8*sign(med:-G.X())*Fmed
            :+ (.25:-(G.X():>q))*(4 - 32*ds_mean(d, G.w(), G.W)/(dq*(1-mc)))) /
            dH
    IF = IF :- ds_mean(IF, G.w(), G.W) // make sure that IF is centered at zero
    ds_set_IF(D, G, i, IF / G.W)
}

void ds_sum_hoover(`Data' D, `Grp' G, `Int' i)
{
    `RS' t
    `RC' z, z2
    
    t  = G.mean()
    z  = G.X() :- t
    z2 = abs(z)
    D.b[i] = ds_mean(z2, G.w(), G.W) / (2 * t)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, ((z2 / (2 * t) :- D.b[i])
        - ds_mean(sign(z) * t + z2, G.w(), G.W) / (2 * t^2) * z) / G.W)
}

void ds_sum_gini(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    __ds_sum_gini(0, D, G, i, df)
}

void ds_sum_agini(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    __ds_sum_gini(1, D, G, i, df)
}

void __ds_sum_gini(`Bool' abs, `Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RC' h
    
    D.b[i] = _ds_sum_gini(G.Xs(), G.ws(), G.W, D.noIF, h=., (&abs, &df, &D.wtype))
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, ds_invp(G.N, G.pX(), h) / G.W)
}

void ds_sum_gw_gini(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    _ds_sum_gw(D, G, i, &_ds_sum_gini(), (&0, &df, &D.wtype))
}

`RS' _ds_sum_gini(`RC' X, `RC' w, `RS' W, `Bool' noIF, `RC' h, `PR' o)
{   // input assumed sorted
    `Bool' abs
    `Int'  wtype
    `RS'   df, b, m, cp, c
    `RC'   F, B
    
    abs = *o[1]; df = *o[2]; wtype = *o[3]
    m   = ds_mean(X, w, W)
    F   = _mm_ranks(X, w, 3, 1, 1)
    cp  = quadcross(X, w, F) / W
    if (df!=0) { // small-sample adjustment
        if (wtype==1) c = W / (W - df)
        else          c = rows(X) / (rows(X) - df)
    }
    else c = 1
    if (abs) b = c * (cp * 2 - m)     // = c * (2 * cov)  with cov = cp - m/2
    else     b = c * (cp * 2 / m - 1) // = c * (2 * cov / m)
    if (b>=.) {
        if (!any(X)) { // X all zero
            b = 0
            if (!noIF) h = J(rows(X), 1, 0)
            return(b)
        }
    }
    if (noIF) return(b)
    // The main moment condition of the Gini coefficient can be written as
    // h = (2*F - 1)*X - G*X (see Binder & Kovacevic 1995)
    // (the IF of the Gini coefficient has the same structure as the IF of the
    // relative PDF; see -reldist-)
    B = _ds_sum_ccdf(X, X:*w, W)
    if (abs) h = ((2*F :- 1):*X :+ 2*(B :- cp))*c :- b
    else     h = ((F :- cp/m):*X :+ B :- cp) * ((2*c) / m)
            // = 1/m * (c*(2*F :- 1):*X :- b:*X :+ c*2*(B :- cp))
    return(b)
}

void ds_sum_b_gini(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS'   m, cp, c
    `IntC' p
    `RC'   w, mg, F, B, h
    
    m  = G.mean()
    mg = ds_invp(G.N, G.pY(), _mm_collapse2(G.XsY(), G.wsY(), G.Ys()))
    p  = mm_order(mg, 1, 1) // stable sort
    _collate(mg, p)
    w  = rows(G.w())==1 ? G.w() : G.w()[p] 
    F  = _mm_ranks(mg, w, 3, 1, 1)
    cp = quadcross(mg, w, F) / G.W
    if (df!=0) { // small-sample adjustment
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    D.b[i] = c * (cp * 2 / m - 1)
    if (D.b[i]>=.) {
        if (!any(G.X())) { // X all zero
            D.b[i] = 0
            if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
            return
        }
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    B  = ds_invp(G.N, p, _ds_sum_ccdf(mg, mg:*w, G.W))
    F  = ds_invp(G.N, p, F)
    mg = ds_invp(G.N, p, mg)
    h  = ((2*c) / m) * ((F :- cp/m):*G.X() :+ B :- cp)
        // not fully correct; ignores the channel trough ranks
    ds_set_IF(D, G, i, h / G.W)
}

void ds_sum_mld(`Data' D, `Grp' G, `Int' i)
{
    `RC' h
    
    D.b[i] = _ds_sum_mld(G.X(), G.w(), G.W, D.noIF, h=.)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_mld_invalid(`Data' D, `Int' j, `RC' X)
{   // registered in _ds_sum_invalid_dict()
    return(_ds_sum_invalid(D, j, "mld", ln(X)))
}

void ds_sum_gw_mld(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_gw(D, G, i, &_ds_sum_mld())
}

`BoolC' _ds_sum_gw_mld_invalid(`Data' D, `Int' j, `RC' X)
{   // registered in _ds_sum_invalid_dict()
    return(_ds_sum_invalid(D, j, "gw_mld", ln(X)))
}

`RS' _ds_sum_mld(`RC' X, `RC' w, `RS' W, `Bool' noIF, `RC' h, | `PR' o)
{
    `RS' b, m
    `RC' z
    pragma unused W
    pragma unused o
    
    m = ds_mean(X, w, W)
    z = ln(X)
    b = ln(m) - ds_mean(z, w, W)
    if (noIF==0) h = ((ln(m):-z) :- b) + (X :- m)/m
    return(b)
}

void ds_sum_w_mld(`Data' D, `Grp' G, `Int' i)
{
    `RC' mg, z, h
    
    z = ln(G.X())
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(G.XsY(), G.wsY(), G.Ys())
    z = ln(mg) - z
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (z :- D.b[i]) + (G.X() - mg):/mg
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_w_mld_invalid(`Data' D, `Int' j, `RC' X)
{   // registered in _ds_sum_invalid_dict()
    return(_ds_sum_invalid(D, j, "w_mld", ln(X)))
}

void ds_sum_b_mld(`Data' D, `Grp' G, `Int' i)
{
    `RS' m
    `RC' mg, h
    
    m = G.mean()
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(G.XsY(), G.wsY(), G.Ys())
    D.b[i] = ln(m) - ds_mean(ln(mg), G.w(), G.W)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = ((ln(m):-ln(mg)) :- D.b[i]) + (G.X() :- m)/m - (G.X() - mg):/mg
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_b_mld_invalid(`Data' D, `Int' j, `RC' X)
{   // registered in _ds_sum_invalid_dict()
    return(_ds_sum_invalid(D, j, "b_mld", ln(X)))
}

void ds_sum_theil(`Data' D, `Grp' G, `Int' i, `RS' nozero)
{
    `RC' h
    
    D.b[i] = _ds_sum_theil(G.X(), G.w(), G.W, D.noIF, h=., &nozero)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_theil_invalid(`Data' D, `Int' j, `RC' X, `RS' nozero)
{   // registered in _ds_sum_invalid_dict()
    return(__ds_sum_theil_invalid(D, j, "theil", X, nozero))
}

`BoolC' __ds_sum_theil_invalid(`Data' D, `Int' j, `SS' s, `RC' X, `RS' nozero)
{
    return(_ds_sum_invalid(D, j, s, ln(nozero ? X : editvalue(X, 0, 1))))
}

void ds_sum_gw_theil(`Data' D, `Grp' G, `Int' i, `RS' nozero)
{
    _ds_sum_gw(D, G, i, &_ds_sum_theil(), &nozero)
}

`BoolC' _ds_sum_gw_theil_invalid(`Data' D, `Int' j, `RC' X, `RS' nozero)
{   // registered in _ds_sum_invalid_dict()
    return(__ds_sum_theil_invalid(D, j, "gw_theil", X, nozero))
}

`RS' _ds_sum_theil(`RC' X, `RC' w, `RS' W, `Bool' noIF, `RC' h, `PR' o)
{
    `RS' b, m, delta, nozero
    `RC' z
    pragma unused W
    
    nozero = *o
    m = ds_mean(X, w, W)
    z = X :* ln(nozero ? X : editvalue(X, 0, 1))
    b = ds_mean(z, w, W)/m - ln(m)
    if (b>=. & !nozero) {
        if (!any(X)) { // X all zero
            b = 0
            if (!noIF) h = J(rows(X), 1, 0)
            return(b)
        }
    }
    if (noIF) return(b)
    delta = ds_mean(z, w, W)/m^2 + 1/m
    h = ((z/m:-ln(m)) :- b) - delta*(X :- m)
    return(b)
}

void ds_sum_w_theil(`Data' D, `Grp' G, `Int' i, `RS' nozero)
{
    `RS' m
    `RC' mg, lnmg, z, h
    
    m = G.mean()
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(G.XsY(), G.wsY(), G.Ys())
    lnmg = ln(nozero ? mg : editvalue(mg, 0, 1))
    z = G.X() :* ln(nozero ? G.X() : editvalue(G.X(), 0, 1)) - mg:*lnmg
    D.b[i] = ds_mean(z, G.w(), G.W) / m
    if (D.b[i]>=. & !nozero) {
        if (!any(G.X())) { // X all zero
            D.b[i] = 0
            if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
            return
        }
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (z/m :- D.b[i]) - (ds_mean(z, G.w(), G.W) / m^2) :* (G.X() :- m) -
        (lnmg:+1)/m :* (G.X() - mg)
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_w_theil_invalid(`Data' D, `Int' j, `RC' X, `RS' nozero)
{   // registered in _ds_sum_invalid_dict()
    return(__ds_sum_theil_invalid(D, j, "w_theil", X, nozero))
}

void ds_sum_b_theil(`Data' D, `Grp' G, `Int' i, `RS' nozero)
{
    `RS' m, mz
    `RC' mg, lnmg, z, h
    pragma unused nozero
    
    m = G.mean()
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(G.XsY(), G.wsY(), G.Ys())
    lnmg = ln(nozero ? mg : editvalue(mg, 0, 1))
    z = mg :* lnmg
    mz = ds_mean(z, G.w(), G.W)
    D.b[i] = mz/m - ln(m)
    if (D.b[i]>=. & !nozero) {
        if (!any(G.X())) { // X all zero
            D.b[i] = 0
            if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
            return
        }
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (z/m :- (ln(m) + D.b[i])) - ((mz/m + 1)/m) :* (G.X() :- m) +
        (lnmg:+1)/m :* (G.X() - mg)
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_b_theil_invalid(`Data' D, `Int' j, `RC' X, `RS' nozero)
{   // registered in _ds_sum_invalid_dict()
    return(__ds_sum_theil_invalid(D, j, "b_theil", X, nozero))
}

void ds_sum_ge(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RC' h
    
    if (a==0) {; ds_sum_mld(D, G, i);      return; }
    if (a==1) {; ds_sum_theil(D, G, i, 1); return; }
    D.b[i] = _ds_sum_ge(G.X(), G.w(), G.W, D.noIF, h=., &a)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_ge_invalid(`Data' D, `Int' j, `RC' X, `RS' a)
{   // registered in _ds_sum_invalid_dict()
    if (a==0) return(_ds_sum_invalid(D, j, "ge(0)", ln(X)))
    if (a==1) return(_ds_sum_invalid(D, j, "ge(1)", ln(X)))
    return(_ds_sum_invalid(D, j, "ge("+strofreal(a)+")", X:^a))
}

void ds_sum_gw_ge(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    if (a==0) {; ds_sum_gw_mld(D, G, i);      return; }
    if (a==1) {; ds_sum_gw_theil(D, G, i, 1); return; }
    _ds_sum_gw(D, G, i, &_ds_sum_ge(), &a)
}

`BoolC' _ds_sum_gw_ge_invalid(`Data' D, `Int' j, `RC' X, `RS' a)
{   // registered in _ds_sum_invalid_dict()
    if (a==0) return(_ds_sum_invalid(D, j, "gw_ge(0)", ln(X)))
    if (a==1) return(_ds_sum_invalid(D, j, "gw_ge(1)", ln(X)))
    return(_ds_sum_invalid(D, j, "gw_ge("+strofreal(a)+")", X:^a))
}

`RS' _ds_sum_ge(`RC' X, `RC' w, `RS' W, `Bool' noIF, `RC' h, `PR' o)
{
    `RS' a, b, c, m, mz, delta
    `RC' z
    pragma unused W
    
    a  = *o
    m  = ds_mean(X, w, W)
    z  = X:^a
    mz = ds_mean(z, w, W)
    c  = 1 / (a * (a - 1))
    b  = c * (mz/m^a - 1)
    if (b>=.) {
        if (!any(X)) { // X all zero
            b = 0
            if (!noIF) h = J(rows(X), 1, 0)
            return(b)
        }
    }
    if (noIF) return(b)
    delta = 1 / ((a - 1) * m^(a + 1)) * mz
    h = (c*(z/m^a:-1) :- b) - delta * (X :- m)
    return(b)
}

void ds_sum_w_ge(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RS' m
    `RC' z, mg, delta, h
    
    if (a==0) {; ds_sum_w_mld(D, G, i);      return; }
    if (a==1) {; ds_sum_w_theil(D, G, i, 1); return; }
    z = G.X():^a
    m = G.mean()
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(G.XsY(), G.wsY(), G.Ys())
    h = z :- mg:^a
    D.b[i] = (1/(a*(a-1)*m^a)) * ds_mean(h, G.w(), G.W)
    if (D.b[i]>=.) {
        if (!any(G.X())) { // X all zero
            D.b[i] = 0
            if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
            return
        }
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (1/(a*(a-1)*m^a)) * h :- D.b[i]
    h = h - mg:^(a-1)/((a-1)*m^a) :* (G.X() - mg)
    delta = ds_mean((1/((a-1)*m^(a+1))) * (z :- mg:^a), G.w(), G.W)
    h = h - delta * (G.X() :- m)
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_w_ge_invalid(`Data' D, `Int' j, `RC' X, `RS' a)
{   // registered in _ds_sum_invalid_dict()
    if (a==0) return(_ds_sum_invalid(D, j, "w_ge(0)", ln(X)))
    if (a==1) return(_ds_sum_invalid(D, j, "w_ge(1)", ln(X)))
    return(_ds_sum_invalid(D, j, "w_ge("+strofreal(a)+")", X:^a))
}

void ds_sum_b_ge(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RS' c, m
    `RC' mg, delta, h
    
    if (a==0) {; ds_sum_b_mld(D, G, i);      return; }
    if (a==1) {; ds_sum_b_theil(D, G, i, 1); return; }
    c = 1 / (a * (a - 1))
    m = G.mean()
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(G.XsY(), G.wsY(), G.Ys())
    D.b[i] = c * (ds_mean(mg:^a, G.w(), G.W) / m^a - 1)
    if (D.b[i]>=.) {
        if (!any(G.X())) { // X all zero
            D.b[i] = 0
            if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
            return
        }
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (c * (mg:^a/m^a :- 1) :- D.b[i]) + 
        (mg:^(a-1)/((a-1)*m^a)) :* (G.X() - mg)
    delta = ds_mean((1/((a-1)*m^(a+1))) * mg:^a, G.w(), G.W)
    h = h - delta * (G.X() :- m)
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_b_ge_invalid(`Data' D, `Int' j, `RC' X, `RS' a)
{   // registered in _ds_sum_invalid_dict()
    if (a==0) return(_ds_sum_invalid(D, j, "b_ge(0)", ln(X)))
    if (a==1) return(_ds_sum_invalid(D, j, "b_ge(1)", ln(X)))
    return(_ds_sum_invalid(D, j, "b_ge("+strofreal(a)+")", X:^a))
}

void ds_sum_atkinson(`Data' D, `Grp' G, `Int' i, `RS' e)
{
    if (e==1) _ds_sum_atkinson1(D, G, i)
    else      _ds_sum_atkinson(D, G, i, e)
}

`BoolC' _ds_sum_atkinson_invalid(`Data' D, `Int' j, `RC' X, `RS' e)
{   // registered in _ds_sum_invalid_dict()
    if (e==1) return(_ds_sum_invalid(D, j, "atkinson(1)", ln(X)))
    return(_ds_sum_invalid(D, j, "atkinson("+strofreal(e)+")", X:^(1-e)))
}

void _ds_sum_atkinson1(`Data' D, `Grp' G, `Int' i)
{
    `RS' m, lm
    `RC' z
    
    z = ln(G.X())
    m = G.mean()
    lm = ds_mean(z, G.w(), G.W)
    D.b[i] = 1 - exp(lm) / m
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, ((G.X():-m)/m :- (z :- lm)) * (exp(lm) / (m * G.W)))
}

void _ds_sum_atkinson(`Data' D, `Grp' G, `Int' i, `RS' e)
{
    `RS' m, lm
    `RC' z
    
    z = G.X():^(1-e)
    m = G.mean()
    lm = ds_mean(z, G.w(), G.W)
    D.b[i] = 1 - lm^(1/(1-e)) / m
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i,((G.X():-m) * (lm^(1/(1-e)) / m^2) :- (z :- lm) *
        (lm^(e/(1-e)) / ((1-e)*m))) / G.W)
}

void ds_sum_cv(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS' m, sd, c
    pragma unset m
    
    sd = sqrt(_ds_variance(G.X(), G.w(), G.W, D.wtype, df, m))
    D.b[i] = sd / m
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    ds_set_IF(D, G, i, 
        (c*(G.X() :- m):^2 :- sd^2) / (2 * sd * m * G.W)   // sd
      + (G.X() :- m) * (-sd / (m^2 * G.W))                 // mean
        )
}

void ds_sum_lvar(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS' m, c
    `RC' z
    
    z = ln(G.X())
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    m = G.mean()
    z = z :- ln(m)
    D.b[i] = quadcross(z, G.w(), z) * (c / G.W)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (c*z:^2 :- D.b[i] :- 
        2*ds_mean(z, G.w(), G.W) / m :* (G.X():-m)) / G.W)
}

`BoolC' _ds_sum_lvar_invalid(`Data' D, `Int' j, `RC' X, `RS' df)
{   // registered in _ds_sum_invalid_dict()
    pragma unused df
    return(_ds_sum_invalid(D, j, "lvar", ln(X)))
}

void ds_sum_vlog(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RC' h
    
    D.b[i] = _ds_sum_vlog(G.X(), G.w(), G.W, D.noIF, h=., (&df, &D.wtype))
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_vlog_invalid(`Data' D, `Int' j, `RC' X, `RS' df)
{   // registered in _ds_sum_invalid_dict()
    pragma unused df
    return(_ds_sum_invalid(D, j, "vlog", ln(X)))
}

void ds_sum_gw_vlog(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    _ds_sum_gw(D, G, i, &_ds_sum_vlog(), (&df, &D.wtype))
}

`BoolC' _ds_sum_gw_vlog_invalid(`Data' D, `Int' j, `RC' X, `RS' df)
{   // registered in _ds_sum_invalid_dict()
    pragma unused df
    return(_ds_sum_invalid(D, j, "gw_vlog", ln(X)))
}

`RC' _ds_sum_vlog(`RC' X, `RC' w, `RS' W, `Bool' noIF, `RC' h, `PR' o)
{
    `Int' wtype
    `RS'  df, b, m, c
    `RC'  z
    pragma unset m

    df = *o[1]; wtype = *o[2]
    c = 1
    if (df!=0) {
        if (wtype==1) c = W / (W - df)
        else          c = rows(X) / (rows(X) - df)
    }
    z = ln(X)
    m = ds_mean(z, w, W)
    b = quadcrossdev(z,m, w, z,m) * (c / W)
    if (noIF) return(b)
    h = (c*(z :- m):^2 :- b)
    return(b)
}

void ds_sum_w_vlog(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    __ds_sum_vlog(0, D, G, i, df)
}

`BoolC' _ds_sum_w_vlog_invalid(`Data' D, `Int' j, `RC' X, `RS' df)
{   // registered in _ds_sum_invalid_dict()
    pragma unused df
    return(_ds_sum_invalid(D, j, "w_vlog", ln(X)))
}

void ds_sum_b_vlog(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    __ds_sum_vlog(1, D, G, i, df)
}

`BoolC' _ds_sum_b_vlog_invalid(`Data' D, `Int' j, `RC' X, `RS' df)
{   // registered in _ds_sum_invalid_dict()
    pragma unused df
    return(_ds_sum_invalid(D, j, "b_vlog", ln(X)))
}

void __ds_sum_vlog(`Bool' btw, `Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS' m, c
    `RC' z, mg, zmg
    
    z = ln(G.X())
    c = 1
    if (df!=0) {
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    mg = J(G.N, 1, .)
    mg[G.pY()] = _mm_collapse2(z[G.pY()], G.wsY(), G.Ys())
    if (btw) {
        m = ds_mean(z, G.w(), G.W)
        D.b[i] = quadcrossdev(mg,m, G.w(), mg,m) * (c / G.W)
    }
    else {
        z = z - mg
        D.b[i] = quadcross(z, G.w(), z) * (c / G.W)
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    if (btw) {
        zmg = mg :- m
        zmg[G.pY()] = _mm_collapse2(zmg[G.pY()], G.wsY(), G.Ys())
        ds_set_IF(D, G, i, ((c*(mg :- m):^2 :- D.b[i]) + 
            (2*c*zmg):*(z - mg)) / G.W)
    }
    else {
        ds_set_IF(D, G, i, (c*z:^2 :- D.b[i]) / G.W)
    }
}

void ds_sum_sdlog(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    ds_sum_vlog(D, G, i, df)
    if (D.b[i]==0) return
    D.b[i] = sqrt(D.b[i])
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / (2 * D.b[i])
}

`BoolC' _ds_sum_sdlog_invalid(`Data' D, `Int' j, `RC' X, `RS' df)
{   // registered in _ds_sum_invalid_dict()
    pragma unused df
    return(_ds_sum_invalid(D, j, "sdlog", ln(X)))
}

void ds_sum_top(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_pshare(D, G, i, (1-p/100) \ 1)
}

void ds_sum_bottom(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_pshare(D, G, i, 0 \ p/100)
}

void ds_sum_mid(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100)
}

void ds_sum_palma(`Data' D, `Grp' G, `Int' i)
{
    `RS' b1, b2
    `RC' IF1
    
    _ds_sum_pshare(D, G, i, 0\.4) // bottom 40%
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _ds_sum_pshare(D, G, i, .9\1) // top 10%
    b2 = D.b[i]
    D.b[i] = b2 / b1
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / b1 - b2/b1^2 * IF1
}

void ds_sum_qratio(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RC' p, q
    
    p = o'/100
    q = _mm_quantile(G.Xs(), G.ws(), p, D.qdef, D.wtype==1, D.hdtrim)
    D.b[i] = q[2] / q[1]
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (_ds_sum_q_IF(D, G, p[2], q[2], D.qdef) / q[1]
        - q[2]/q[1]^2 * _ds_sum_q_IF(D, G, p[1], q[1], D.qdef)) / G.W)
}

void ds_sum_sratio(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    `RS' b1, b2
    `RC' p, IF1
    
    if      (o[1]==.z) p = (0,10,90,100)'     // zero arguments
    else if (o[3]==.z) p = (0,o[1],o[2],100)' // two arguments
    else               p = o'                 // four arguments
    p = p / 100
    _ds_sum_pshare(D, G, i, p[|1\2|]) // lower share
    b1 = D.b[i]
    if (D.noIF==0) IF1 = D.IF[,i]
    _ds_sum_pshare(D, G, i, p[|3\4|]) // upper share
    b2 = D.b[i]
    D.b[i] = b2 / b1
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    D.IF[,i] = D.IF[,i] / b1 - b2/b1^2 * IF1
}

void ds_sum_lorenz(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_lorenz(D, G, i, p/100, 0)
}

void _ds_sum_lorenz(`Data' D, `Grp' G, `Int' i, `RC' at, `Int' t)
{
    D.b[i] = _ds_lorenz(G, at, t)
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _ds_lorenz_IF(D, G, at, t, D.b[i])
}

void ds_sum_tlorenz(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_lorenz(D, G, i, p/100, 2)
}

void ds_sum_glorenz(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_lorenz(D, G, i, p/100, 3)
}

void ds_sum_alorenz(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_lorenz(D, G, i, p/100, 4)
}

void ds_sum_elorenz(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_lorenz(D, G, i, p/100, 5)
}

void ds_sum_share(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100)
}

void ds_sum_pshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100)
}

void _ds_sum_pshare(`Data' D, `Grp' G, `Int' i, `RC' at, | `Int' t, `Bool' d)
{
    `RC' L
    
    if (args()<5) t = 0
    if (args()<6) d = 0
    if (at[1]>at[2]) D.b[i] = .
    else {
        L = _ds_lorenz(G, at, t)
        D.b[i] = mm_diff(L)
        if (d) D.b[i] = D.b[i] * editmissing(1/(at[2]-at[1]), 0)
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _ds_pshare_IF(D, G, at, t, d, L)
}

void ds_sum_dshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100, 0, 1)
}

void ds_sum_tshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100, 2, 0)
}

void ds_sum_gshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100, 3, 0)
}

void ds_sum_ashare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_pshare(D, G, i, o'/100, 3, 1)
}

void ds_sum_gci(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    _ds_sum_gci(0, D, G, i, df)
}

void ds_sum_aci(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    _ds_sum_gci(1, D, G, i, df)
}

void _ds_sum_gci(`Bool' abs, `Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS' m, mF, cov, c
    `RM' mv
    `RC' F, B

    F   = _mm_ranks(G.Ys(), G.wsY(), 3, 1, 1)
    mv  = quadmeanvariance((G.XsY(), F), G.wsY())
    m   = mv[1,1]; mF  = mv[1,2]
    cov = mv[3,1] * ((G.W-1)/G.W)
    if (df!=0) { // small-sample adjustment
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    if (abs) D.b[i] = c * 2 * cov
    else     D.b[i] = c * 2 * cov / m
    if (_ds_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    B = ds_invp(G.N, G.pY(), _ds_sum_ccdf(G.Ys(), (G.XsY():-m):*G.wsY(), G.W))
    F[G.pY()] = F
    if (abs) {
        ds_set_IF(D, G, i, ((F:-mF):*(G.X():-m) :+ B :- 2*cov) * ((2*c)/G.W))
        // = (2*c*F:*z :- D.b[i] :+ 2*c*(B :- cov) :- 2*c*mF:*z) / G.W)
        // with z = G.X() :- m
    }
    else {
        ds_set_IF(D, G, i, ((F:-mF):*(G.X():-m) :- cov*(1:+G.X()/m) :+ B)
            * ((2*c)/(m*G.W)))
        // = (2*c*F:*z :- D.b[i]:*G.X() :+ 2*c*(B :- cov) :- 2*c*mF:*z) / (m*G.W))
        // with z = G.X() :- m
    }
}

void ds_sum_ccurve(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_ccurve(D, G, i, 0, p)
}

void _ds_sum_ccurve(`Data' D, `Grp' G, `Int' i, `Int' t, `RS' p)
{
    `RS' at
    
    at = p / 100
    D.b[i] = _ds_lorenz(G, at, t, 1)
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _ds_lorenz_IF(D, G, at, t, D.b[i], 1)
}

void ds_sum_tccurve(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_ccurve(D, G, i, 2, p)
}

void ds_sum_gccurve(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_ccurve(D, G, i, 3, p)
}

void ds_sum_accurve(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_ccurve(D, G, i, 4, p)
}

void ds_sum_eccurve(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_ccurve(D, G, i, 5, p)
}

void ds_sum_cshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_cshare(D, G, i, 0, 0, o[1], o[2])
}

void ds_sum_pcshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_cshare(D, G, i, 0, 0, o[1], o[2])
}

void _ds_sum_cshare(`Data' D, `Grp' G, `Int' i, `Int' t, `Bool' d,
    `RS' p1, `RS' p2)
{
    `RC' L, at
    
    at = (p1 \ p2) / 100
    if (at[1]>at[2]) D.b[i] = .
    else {
        L = _ds_lorenz(G, at, t, 1)
        D.b[i] = mm_diff(L)
        if (d) D.b[i] = D.b[i] * editmissing(1/(at[2]-at[1]), 0)
    }
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    if (t==2) D.IFtot[i] = D.b[i] // total
    D.IF[,i] = _ds_pshare_IF(D, G, at, t, d, L, 1)
}

void ds_sum_dcshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_cshare(D, G, i, 0, 1, o[1], o[2])
}

void ds_sum_tcshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_cshare(D, G, i, 2, 0, o[1], o[2])
}

void ds_sum_gcshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_cshare(D, G, i, 3, 0, o[1], o[2])
}

void ds_sum_acshare(`Data' D, `Grp' G, `Int' i, `RR' o)
{
    _ds_sum_cshare(D, G, i, 3, 1, o[1], o[2])
}

void ds_sum_hcr(`Data' D, `Grp' G, `Int' i)
{
    D.b[i] = ds_mean(G.poor(), G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (G.poor() :- D.b[i]) / G.W)
}

void ds_sum_pgap(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_pgap(0, D, G, i)
}

void ds_sum_apgap(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_pgap(1, D, G, i)
}

void _ds_sum_pgap(`Bool' abs, `Data' D, `Grp' G, `Int' i)
{
    `RS'   N, W
    `RC'   z, h, w
    `IntC' p
    
    p = selectindex(G.poor())
    N = length(p)
    if (N==0) { // no poor
        D.b[i] = 0
        if (D.noIF) return
        ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    if (rows(G.w())==1) {
        w = G.w()
        W = N * w
    }
    else {
        w = G.w()[p]
        W = quadsum(w)
    }
    if (abs==0) z = ((G.pl() :- G.X()) :/ G.pl())[p]
    else        z = (G.pl() :- G.X())[p]
    D.b[i] = ds_mean(z, w, W)
    if (D.noIF) return
    h = J(G.N, 1, 0)
    h[p] = (z :- D.b[i]) / W
    ds_set_IF(D, G, i, h)
}

void ds_sum_pgi(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_pgi(0, D, G, i)
}

void ds_sum_apgi(`Data' D, `Grp' G, `Int' i)
{
    _ds_sum_pgi(1, D, G, i)
}

void _ds_sum_pgi(`Bool' abs, `Data' D, `Grp' G, `Int' i)
{
    `RC' z
    
    z = (G.pl() :- G.X()) :* G.poor()
    if (abs==0) z = z :/ G.pl()
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void ds_sum_fgt(`Data' D, `Grp' G, `Int' i, `RS' a)
{
    `RC' z
    
    z = ((G.pl() :- G.X()) :* G.poor() :/ G.pl()):^a :* G.poor()
    if (hasmissing(z)) D.b[i] = .
    else               D.b[i] = ds_mean(z, G.w(), G.W)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

void ds_sum_chu(`Data' D, `Grp' G, `Int' i, `RS' a0)
{
    `RS' a, m
    `RC' z, h
    
    a  = a0/100
    if (a==0) z = (ln(G.X()) :- ln(G.pl())) :* G.poor()
    else      z = (1 :- (G.X():/G.pl()):^a) :* G.poor()
    m = ds_mean(z, G.w(), G.W)
    if (a==0) {
        D.b[i] = 1 - exp(m)
        if (_ds_sum_omit(D, i)) return
        if (D.noIF) return
        h = -exp(m)*(z :- m)
    }
    else {
        D.b[i] = m / a
        if (_ds_sum_omit(D, i)) return
        if (D.noIF) return
        h = z / a :- D.b[i]
    }
    ds_set_IF(D, G, i, h / G.W)
}

`BoolC' _ds_sum_chu_invalid(`Data' D, `Int' j, `RC' X, `RS' a)
{   // registered in _ds_sum_invalid_dict()
    if (a==0) return(_ds_sum_invalid(D, j, "chu(0)", ln(X)))
    return(_ds_sum_invalid(D, j, "chu("+strofreal(a)+")", X:^(a/100)))
}

void ds_sum_watts(`Data' D, `Grp' G, `Int' i)
{
    `RC' z
    
    z = (ln(G.pl()) :- ln(G.X())) :* G.poor()
    D.b[i] = ds_mean(z, G.w(), G.W)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    ds_set_IF(D, G, i, (z :- D.b[i]) / G.W)
}

`BoolC' _ds_sum_watts_invalid(`Data' D, `Int' j, `RC' X)
{   // registered in _ds_sum_invalid_dict()
    return(_ds_sum_invalid(D, j, "watts", ln(X)))
}

void ds_sum_sen(`Data' D, `Grp' G, `Int' i)
{
    `RS'   hcr, pgi, Gp
    `RC'   z_hcr, z_pgi, z_Gp
    `RS'   m, cp, N, W
    `RC'   Xs, ws, F, B
    `IntC' p
    
    // head count ratio
    z_hcr  = G.poor()
    hcr    = ds_mean(z_hcr, G.w(), G.W)
    // poverty gap index
    z_pgi  = ((G.pl() :- G.X()) :/ G.pl()) :* z_hcr
    pgi    = ds_mean(z_pgi, G.w(), G.W)
    // Gini among poor
    p = mm_order(G.X(), 1, rows(G.w())!=1)
    p = select(p, z_hcr[p])
    N = length(p)
    if (N==0) Gp = 0 // no poor
    else {
        Xs = G.X()[p]
        ws = (rows(G.w())!=1 ? G.w()[p] : G.w())
        W = (rows(ws)==1 ? N*ws : quadsum(ws))
        m  = ds_mean(Xs, ws, W)
        F  = _mm_ranks(Xs, ws, 3, 1, 1)
        cp = quadcross(Xs, ws, F) / W
        Gp = cp * 2 / m - 1
    }
    // sen poverty index
    D.b[i] = pgi + (hcr - pgi) * Gp
    if (D.noIF) return
    if (N==0) z_Gp = J(G.N, 1, 0) // no poor
    else {
        B = z_Gp = J(G.N, 1, 0)
        B[p] = _ds_sum_ccdf(Xs, Xs:*ws, W)
        z_Gp[p] = F
        z_Gp = z_hcr :* ((z_Gp :- cp/m):*G.X() :+ B :- cp) * (2 / m) * (G.W/W)
    }
    z_hcr = Gp             * (z_hcr :- hcr)
    z_pgi = (1+(hcr-1)*Gp) * (z_pgi :- pgi)
    ds_set_IF(D, G, i, (z_pgi + z_hcr + (hcr-pgi) * z_Gp) / G.W)
}

void ds_sum_sst(`Data' D, `Grp' G, `Int' i)
{
    `RS'   pgi, Gp, m, cp
    `RC'   pg, Xs, ws, F, B, h
    `IntC' p
    
    // poverty gap index
    pg  = ((G.pl() :- G.X()) :/ G.pl()) :* G.poor()
    pgi = ds_mean(pg, G.w(), G.W)
    // Gini of poverty gaps
    p  = mm_order(pg, 1, rows(G.w())!=1)
    Xs = pg[p]
    ws = (rows(G.w())!=1 ? G.w()[p] : G.w())
    m  = ds_mean(Xs, ws)
    F  = _mm_ranks(Xs, ws, 3, 1, 1)
    cp = quadcross(Xs, ws, F) / G.W
    Gp = cp * 2 / m - 1 
    // SST 
    D.b[i] = pgi * (1 + Gp) 
    if (D.noIF) return
    // part of IF related to Gini
    B = _ds_sum_ccdf(Xs, Xs:*ws, G.W)
    h = J(G.N, 1, 0)
    h[p] = ((F :- cp/m):*Xs :+ B :- cp) * (2 / m)
    // add part related to PGI
    ds_set_IF(D, G, i, ((1+Gp) * (pg :- pgi) + pgi * h) / G.W)
}

void ds_sum_takayama(`Data' D, `Grp' G, `Int' i)
{
    `RS'   m, cp
    `RC'   Xs, ws, F, B, h
    `IntC' p
    
    // generate censored outcomes
    Xs = (G.X() :* G.poor()) + (G.pl() :* !G.poor())
    // compute Gini from censored outcomes
    p  = mm_order(Xs, 1, rows(G.w())!=1)
    Xs = Xs[p]
    ws = (rows(G.w())!=1 ? G.w()[p] : G.w())
    m  = ds_mean(Xs, ws)
    F  = _mm_ranks(Xs, ws, 3, 1, 1)
    cp = quadcross(Xs, ws, F) / G.W
    D.b[i] = cp * 2 / m - 1 
    if (D.noIF) return
    B = _ds_sum_ccdf(Xs, Xs:*ws, G.W)
    h = J(G.N, 1, 0)
    h[p] = ((F :- cp/m):*Xs :+ B :- cp) * (2 / m)
    ds_set_IF(D, G, i, h / G.W)
}

void ds_sum_tip(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_tip(D, G, i, 0, p)
}

void ds_sum_atip(`Data' D, `Grp' G, `Int' i, `RS' p)
{
    _ds_sum_tip(D, G, i, 1, p)
}

void _ds_sum_tip(`Data' D, `Grp' G, `Int' i, `Int' t, `RS' p)
{
    D.b[i] = _ds_tip(D, G, p/100, t, i, i) // also computes IF
}

void ds_sum_correlation(`Data' D, `Grp' G, `Int' i)
{
    `RS'  mX, sdX, mY, sdY, cov
    `RM'  mv
    `RC'  zX, zY
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    mv = quadmeanvariance((G.X(), G.Y()), G.w())
    mv[(2,3),] = mv[(2,3),] * ((G.W-1)/G.W) // remove df correction
    sdX = sqrt(mv[2,1]); sdY = sqrt(mv[3,2]); cov = mv[3,1]
    D.b[i] = cov / (sdX * sdY)
    if (_ds_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    mX = mv[1,1];     mY = mv[1,2]
    zX = (G.X():-mX); zY = (G.Y():-mY)
    ds_set_IF(D, G, i, ((zX:*zY :- cov) - cov*((zX:^2:-sdX^2)/(2*sdX^2) 
        + (zY:^2:-sdY^2)/(2*sdY^2))) / (sdX*sdY*G.W))
}

void ds_sum_rsquared(`Data' D, `Grp' G, `Int' i)
{
    `RS'  mX, VX, mY, VY, cov
    `RM'  mv
    `RC'  zX, zY
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    mv = quadmeanvariance((G.X(), G.Y()), G.w())
    mv[(2,3),] = mv[(2,3),] * ((G.W-1)/G.W) // remove df correction
    VX = mv[2,1]; VY = mv[3,2]; cov = mv[3,1]
    D.b[i] = cov^2 / (VX * VY)
    if (_ds_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    mX = mv[1,1];     mY = mv[1,2]
    zX = (G.X():-mX); zY = (G.Y():-mY)
    ds_set_IF(D, G, i, (2*cov*(zX:*zY :- cov)
        - cov^2/VX*(zX:^2:-VX) - cov^2/VY*(zY:^2:-VY)) / (VX*VY*G.W))
}

void ds_sum_b(`Data' D, `Grp' G, `Int' i) ds_sum_slope(D, G, i)

void ds_sum_slope(`Data' D, `Grp' G, `Int' i)
{
    `RS'  cov, VY
    `RM'  mv
    `RC'  zX, zY
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    mv = quadmeanvariance((G.X(), G.Y()), G.w())
    mv[(2,3),] = mv[(2,3),] * ((G.W-1)/G.W) // remove df correction
    VY  = mv[3,2]
    cov = mv[3,1]
    D.b[i] = cov / VY
    if (_ds_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    zX = G.X() :- mv[1,1]
    zY = G.Y() :- mv[1,2]
    ds_set_IF(D, G, i, ((zX:*zY :- cov) - cov/VY*(zY:^2 :- VY)) / (VY*G.W))
}

void ds_sum_cohend(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS'    m1, m2, d, v, c, W1, W2
    `RC'    l, z
    `BoolC' g1
    `IntC'  p
    
    l = _mm_unique(G.Ys())       // levels of by
    if (length(l)!=2) D.b[i] = . // set missing if not two levels
    else {
        z  = J(G.N, 1, .)
        g1 = G.Y():==l[1]
        // level 1
        p = selectindex(g1)
        if (length(p)) {
            m1 = ds_mean(G.X()[p], rows(G.w())==1 ? G.w() : G.w()[p])
            z[p] = G.X()[p] :- m1
        }
        // level 2
        p = selectindex(!g1)
        if (length(p)) {
            m2 = ds_mean(G.X()[p], rows(G.w())==1 ? G.w() : G.w()[p])
            z[p] = G.X()[p] :- m2
        }
        // pooled variance
        if (df!=0) {
            if (D.wtype==1) c = G.W / (G.W - df)
            else            c = G.N / (G.N - df)
        }
        else c = 1
        v = c * quadcross(z, G.w(), z) / G.W
        // Cohen's d
        d = m2 - m1
        D.b[i] = d / sqrt(v)
    }
    if (_ds_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    W1 = rows(G.w())==1 ? G.w()*sum(g1) : quadsum(G.w():*g1)
    W2 = G.W - W1
    ds_set_IF(D, G, i, (
          ((z:*!g1) * (G.W/W2) - (z:*g1) * (G.W/W1)) / sqrt(v) // numerator
        - (c*z:^2 :- v) * ((d / (2*v*sqrt(v))))                // denominator
        ) / G.W)
}

void ds_sum_covariance(`Data' D, `Grp' G, `Int' i, `RS' df)
{
    `RS'  c, mX, mY
    `RM'  mv
    
    mv = quadmeanvariance((G.X(), G.Y()), G.w())
    mv[(2,3),] = mv[(2,3),] * ((G.W-1)/G.W) // remove df correction
    if (df!=0) { // small-sample adjustment
        if (D.wtype==1) c = G.W / (G.W - df)
        else            c = G.N / (G.N - df)
    }
    else c = 1
    D.b[i] = mv[3,1] * c
    if (_ds_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    mX = mv[1,1]; mY = mv[1,2]
    ds_set_IF(D, G, i, (c*((G.X():-mX) :* (G.Y():-mY)) :- D.b[i]) / G.W)
}

void ds_sum_spearman(`Data' D, `Grp' G, `Int' i)
{
    `RM' mv, cov, vx, vy
    `RC' FX, FY, hX, hY, hXY
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    FX = FY = J(G.N, 1, .)
    FX[G.pX()] = _mm_ranks(G.Xs(), G.ws(), 3, 1, 1)
    FY[G.pY()] = _mm_ranks(G.Ys(), G.wsY(), 3, 1, 1)
    mv = quadvariance((FX, FY), G.w()) * ((G.W-1) / G.W)
    cov = mv[2,1]; vx = mv[1,1]; vy = mv[2,2]
    D.b[i] = cov / (sqrt(vx) * sqrt(vy))
    if (_ds_sum_omit(D, i)) return // can happen if N=1
    // compute IF
    if (D.noIF) return
    hX = hY = J(G.N, 1, .)
    hX[G.pX()] = _ds_sum_ccdf(FX[G.pX()], (FY:*G.w())[G.pX()], G.W)
    hY[G.pY()] = _ds_sum_ccdf(FY[G.pY()], (FX:*G.w())[G.pY()], G.W)
    hXY        = (FX:*FY + hX + hY) :- 3*(cov + 0.25)
    hX[G.pX()] = _ds_sum_ccdf(FX[G.pX()], (2*FX:*G.w())[G.pX()], G.W)
    hX         = (FX:^2 + hX) :- 3*(vx + 0.25)
    hY[G.pY()] = _ds_sum_ccdf(FY[G.pY()], (2*FY:*G.w())[G.pY()], G.W)
    hY         = (FY:^2 + hY) :- 3*(vy + 0.25)
    ds_set_IF(D, G, i, (hXY - cov * (hX/(2*vx) + hY/(2*vy))) /
                          (sqrt(vx)*sqrt(vy) * G.W))
}

void ds_sum_taua(`Data' D, `Grp' G, `Int' i, `Bool' naive)
{
    `RS' K, S
    `RC' h
    
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    D.b[i] = S / K
    if (_ds_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = (1/K)*(G.cd_s() :- S) - (S/K^2)*(G.cd_k() :- K)
    ds_set_IF(D, G, i, h * (2/G.W))
}

void ds_sum_taub(`Data' D, `Grp' G, `Int' i, `Bool' naive)
{
    `RS' K, S, T, U, Q
    `RC' h
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    T = G.cd_T()
    U = G.cd_U()
    Q = sqrt((K - T) * (K - U))
    D.b[i] = S / Q
    if (_ds_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = (2*K-T-U)*(G.cd_k():-K) + (U-K)*(G.cd_t():-T) + (T-K)*(G.cd_u():-U)
    h = (1/Q)*(G.cd_s() :- S) - (S/(2*Q^3))*h 
    ds_set_IF(D, G, i, h * (2/G.W))
}

void ds_sum_somersd(`Data' D, `Grp' G, `Int' i, `Bool' naive)
{
    `RS' K, S, U, Q
    `RC' h
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    U = G.cd_U()
    Q = K - U
    D.b[i] = S / Q
    if (_ds_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = (1/Q)*(G.cd_s() :- S) - (S/Q^2)*((G.cd_k() - G.cd_u()) :- Q)
    ds_set_IF(D, G, i, h * (2/G.W))
}

void ds_sum_gamma(`Data' D, `Grp' G, `Int' i, `Bool' naive)
{
    `RS' K, S, T, U, V, Q
    `RC' h
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    G.cd_fast(naive)
    S = G.cd_S()
    K = G.cd_K()
    T = G.cd_T()
    U = G.cd_U()
    V = G.cd_V()
    Q = K - (T + U - V)
    D.b[i] = S / Q
    if (_ds_sum_omit(D, i)) return
    // compute IF
    if (D.noIF) return
    h = G.cd_k() - G.cd_t() - G.cd_u() + G.cd_v()
    h = (1/Q)*(G.cd_s() :- S) - (S/Q^2)*(h :- Q)
    ds_set_IF(D, G, i, 2*h / G.W)
}

void ds_sum_hhi(`Data' D, `Grp' G, `Int' i)
{
    `RC' h
    
    D.b[i] = ds_mean(G.prX(), G.w(), G.W)  // = sum_j pj^2
    if (D.noIF) return
    // IF: see v1.2.3 (27nov2021) for an easier to understand approach (slow)
    h = G.prX() :- quadsum(select(G.prX(), G.tagX()):^2)
    ds_set_IF(D, G, i, h * (2/G.W))
}

void ds_sum_hhin(`Data' D, `Grp' G, `Int' i)
{
    `Int'  K
    `RC'   h
    
    K = sum(G.tagX())
    D.b[i] = (ds_mean(G.prX(), G.w(), G.W) - 1/K) / (1 - 1/K)
    if (D.noIF) return
    // IF: see v1.2.3 (27nov2021) for an easier to understand approach (slow)
    h = G.prX() :- quadsum(select(G.prX(), G.tagX()):^2)
    ds_set_IF(D, G, i, h * (2/(G.W * (1 - 1/K))))
}

void ds_sum_gimp(`Data' D, `Grp' G, `Int' i)
{
    (void) ds_sum_hhi(D, G, i)  // = 1 - sum_j pj^2 = sum_j pj*(1-pj)
    if (D.omit[i]) return
    D.b[i] = 1 - D.b[i]
    if (D.noIF) return
    D.IF[,i] = -D.IF[,i]
}

void ds_sum_gimpn(`Data' D, `Grp' G, `Int' i)
{
    (void) ds_sum_hhin(D, G, i)
    if (D.omit[i]) return
    D.b[i] = 1 - D.b[i]
    if (D.noIF) return
    D.IF[,i] = -D.IF[,i]
}

void ds_sum_entropy(`Data' D, `Grp' G, `Int' i, `RS' base)
{   // base=0 => ln()
    `RC' pj, h
    
    D.b[i] = -ds_mean(ln(G.prX()), G.w(), G.W)  // = - sum_j pj * ln(pj)
    if (base!=0) D.b[i] = D.b[i] / ln(base)
    if (D.noIF) return
    // IF: see v1.2.3 (27nov2021) for an easier to understand approach (slow)
    pj = select(G.prX(), G.tagX())
    h = ln(G.prX()) :+ (1 - quadsum(pj :* (ln(pj) :+ 1)))
    if (base!=0) h = h / ln(base)
    ds_set_IF(D, G, i, -h/G.W)
}

void ds_sum_hill(`Data' D, `Grp' G, `Int' i, `RS' q)
{
    `RS' b0
    `RC' pj, h
    
    if (q==0) {
        _ds_sum_fixed(D, i, sum(G.tagX()))        // = sum_j pj^0 (n. of levels)
        return
    }
    if (q==1) {
        b0 = -ds_mean(ln(G.prX()), G.w(), G.W)    // = - sum_j pj * ln(pj)
        D.b[i] = exp(b0)
    }
    else if (q==2) {
        b0 = ds_mean(G.prX(), G.w(), G.W)         // = sum_j pj^2
        D.b[i] = b0^-1
    }
    else {
        b0 = ds_mean(G.prX():^(q-1), G.w(), G.W)  // = sum_j pj^q
        D.b[i] = b0^(1/(1-q))
    }
    if (D.noIF) return
    // IF: see v1.2.3 (27nov2021) for an easier to understand approach (slow)
    pj = select(G.prX(), G.tagX())
    if (q==1) {
        h = -exp(b0) :* (ln(G.prX()) :+ (1 - quadsum(pj :* (ln(pj) :+ 1))))
    }
    else {
        h = ((1/(1-q)) * b0^(1/(1-q)-1)) :* 
            (q * G.prX():^(q-1) :- quadsum(q * pj:^q))
    }
    ds_set_IF(D, G, i, h/G.W)
}

void ds_sum_renyi(`Data' D, `Grp' G, `Int' i, `RS' q)
{
    `RS' b0
    
    if (q==1) {
        ds_sum_entropy(D, G, i, 0)
        return
    }
    ds_sum_hill(D, G, i, q)
    b0 = D.b[i]
    D.b[i] = ln(b0)
    if (D.noIF) return
    D.IF[,i] = (1/b0) :* D.IF[,i]
}

void ds_sum_mindex(`Data' D, `Grp' G, `Int' i, `RS' base)
{
    _ds_sum_minfo(1, D, G, i, base)
}

void ds_sum_uc(`Data' D, `Grp' G, `Int' i)
{
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
    }
    else _ds_sum_minfo(2, D, G, i, 0)
}

void ds_sum_ucl(`Data' D, `Grp' G, `Int' i)
{
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
    }
    else _ds_sum_minfo(3, D, G, i, 0)
}

void ds_sum_ucr(`Data' D, `Grp' G, `Int' i)
{
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
    }
    else _ds_sum_minfo(4, D, G, i, 0)
}

void _ds_sum_minfo(`Int' stat, `Data' D, `Grp' G, `Int' i, `RS' base)
{   // stat: 0 = joint entropy [currently not used]
    //       1 = M index
    //       2 = uncertainty coefficient (symmetric)
    //       3 = uncertainty coefficient (left)
    //       4 = uncertainty coefficient (right)
    // could also add support for conditional entropy:
    //     H(X|Y) = H(X,Y) - H(Y)  and  H(Y|X) = H(X,Y) - H(X)
    `RS' b, bX, bY
    `RC' h, hX, hY, p
    
    b = -ds_mean(ln(G.prXY()), G.w(), G.W)      // = - sum_j sum_k pjk * ln(pjk)
    if (base!=0) b = b / ln(base)
    if (stat) {
        bX = -ds_mean(ln(G.prX()), G.w(), G.W)  // = - sum_j pj * ln(pj)
        if (base!=0) bX = bX / ln(base)
        bY = -ds_mean(ln(G.prY()), G.w(), G.W)  // = - sum_k pk * ln(pk)
        if (base!=0) bY = bY / ln(base)
    }
    if      (stat==1)  D.b[i] =    bX + bY - b              // M index
    else if (stat==2)  D.b[i] = 2*(bX + bY - b) / (bX + bY) // UC symmetric
    else if (stat==3)  D.b[i] =   (bX + bY - b) / bX        // UC left
    else if (stat==4)  D.b[i] =   (bX + bY - b) / bY        // UC right
    else               D.b[i] =              b              // joint entropy
    if (D.noIF) return
    // IF: see v1.2.3 (27nov2021) for an easier to understand approach (slow)
    p = select(G.prXY(), G.tagXY())
    h = ln(G.prXY()) :+ (1 - quadsum(p :* (ln(p) :+ 1)))
    if (base!=0) h = h / ln(base)
    if (stat) {
        p = select(G.prX(), G.tagX())
        hX = ln(G.prX()) :+ (1 - quadsum(p :* (ln(p) :+ 1)))
        if (base!=0) hX = hX / ln(base)
        p = select(G.prY(), G.tagY())
        hY = ln(G.prY()) :+ (1 - quadsum(p :* (ln(p) :+ 1)))
        if (base!=0) hY = hY / ln(base)
    }
    if      (stat==1)  h = hX + hY - h                           // M index
    else if (stat==2)  h = 2*(b/(bX+bY)^2*(hX + hY) - h/(bX+bY)) // UC symmetric
    else if (stat==3)  h = (b-bY)/bX^2*hX + hY/bX - h/bX         // UC left
    else if (stat==4)  h = hX/bY + (b-bX)/bY^2*hY - h/bY         // UC right
    ds_set_IF(D, G, i, -h/G.W)
}

void ds_sum_cramersv(`Data' D, `Grp' G, `Int' i, `Bool' bc)
{
    `Int' C, R, RC
    `RS'  b, pr0, q
    `RM'  prXY
    `RC'  h, z
    pragma unused bc // bias correction not implemented
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    // obtain probability table (long format): x, y, pxy, px, py, px*py
    prXY = select((G.X(), G.Y(), G.prXY()), G.tagXY())
    RC   = rows(prXY) // number of (nonempty) cells
    prXY = prXY, _ds_ifreq(prXY[,1], prXY[,3], RC),
                 _ds_ifreq(prXY[,2], prXY[,3], RC)
    prXY = prXY, prXY[,4]:*prXY[,5]
    R    = mm_nunique(prXY[,1])  // number of rows
    C    = mm_nunique(prXY[,2])  // number of columns
    if (R*C>RC) pr0 = 1 - quadsum(prXY[,6]) // expected mass in empty cells
    else        pr0 = 0
    // compute V
    b = quadsum((prXY[,3] - prXY[,6]):^2:/prXY[,6]) + pr0
    D.b[i] = sqrt(b / min((R-1, C-1)))
    if (D.noIF) return
    // IF: see v1.2.3 (27nov2021) for an easier to understand approach (slow)
    // - contribution of cells
    z = G.prX():*G.prY()
    h = 2 * (G.prXY():/z :- 1) :-
        2 * quadsum(prXY[,3]:^2:/prXY[,6] - prXY[,3])
    // - contributions of margins (somewhat complicated since aggregations by 
    //   levels of X and Y are needed and empty cells matter)
    q = quadsum((prXY[,3]:^2 - prXY[,6]:^2) :/ prXY[,6])
    z = (G.prXY():^2 - z:^2) :/ z
    if (pr0) { // has empty cells
        h = (h :+ 2 * (q + 1 - pr0)) -
          _ds_ifreq(G.X(), G.tagXY():*(z:/G.prX()+G.prY()), G.N, G.pX()) -
          _ds_ifreq(G.Y(), G.tagXY():*(z:/G.prY()+G.prX()), G.N, G.pY())
    }
    else {
        h = (h :+ 2 * q) -
          _ds_ifreq(G.X(), G.tagXY():*(z:/G.prX()), G.N, G.pX()) -
          _ds_ifreq(G.Y(), G.tagXY():*(z:/G.prY()), G.N, G.pY()) 
    }
    h = h / (2 * sqrt(b * min((R-1, C-1))))
    ds_set_IF(D, G, i, h/G.W)
}

void ds_sum_dissimilarity(`Data' D, `Grp' G, `Int' i)
{
    `Int' C, R, RC
    `RS'  b, d, pr0, q
    `RM'  prXY
    `RC'  prY, h, z, u, Z
    
    if (G.y_is_x==`TRUE') {
        D.b[i] = 1
        if (!D.noIF) ds_set_IF(D, G, i, J(G.N, 1, 0))
        return
    }
    // obtain probability table (long format): x, y, pxy, px, py
    prXY = select((G.X(), G.Y(), G.prXY()), G.tagXY())
    RC   = rows(prXY) // number of (nonempty) cells
    prXY = prXY, _ds_ifreq(prXY[,1], prXY[,3], RC),
                 _ds_ifreq(prXY[,2], prXY[,3], RC)
    R    = mm_nunique(prXY[,1])  // number of rows
    prY  = select(prXY[,5], mm_unique_tag(prXY[,2]))
    C    = rows(prY)            // number of rows
    if (R*C>RC) pr0 = 1 - quadsum(prXY[,4]:*prXY[,5]) // exp.mass in empty cells
    else        pr0 = 0
    // compute D
    b = quadsum(prXY[,4] :* abs(prXY[,3]:/prXY[,4] :- prXY[,5])) + pr0
    d = prY' * (1 :- prY)
    D.b[i] = b / (2 * d)
    // compute IF
    if (D.noIF) return
    // IF: see v1.2.4 (03dec2021) for an easier to understand approach (slow)
    // - contribution of cells
    Z = prXY[,3]:/prXY[,4] :- prXY[,5]
    z = G.prXY():/G.prX() - G.prY()
    h = sign(z) :- quadsum(prXY[,3]:*sign(Z))
    // - contributions of X margins
    q = quadsum(prXY[,4]:*(abs(Z) - prXY[,3]:/prXY[,4]:*sign(Z)))
    u = abs(z) - G.prXY():/G.prX():*sign(z)
    if (pr0) h = (h :- (q + pr0 - 1)) +
                 _ds_ifreq(G.X(), G.tagXY():*(u - G.prY()), G.N, G.pX())
    else     h = (h :- q) + _ds_ifreq(G.X(), G.tagXY():*u, G.N, G.pX())
    // - contributions of Y margins
    q = quadsum(prXY[,5]:*prXY[,4]:*sign(Z))
    u = G.prX():*sign(z)
    if (pr0) h = (h :+ (q + 1 - pr0)) -
                 _ds_ifreq(G.Y(), G.tagXY():*(u + G.prX()), G.N, G.pY())
    else h = (h :+ q) - _ds_ifreq(G.Y(), G.tagXY():*u, G.N, G.pY())
    // - denominator
    h = h / (2 * d)
    q = quadsum((-b / (2 * d^2)) :* prY :* (1 :- 2*prY))
    u = (-b / (2 * d^2)) * (1 :- 2*G.prY())
    h = (h :- q) + u
    ds_set_IF(D, G, i, h/G.W)
}

void ds_sum_or(`Data' D, `Grp' G, `Int' i)
{
    `RS'    p00, p01, p10, p11
    `BoolC' X, Y
    `RC'    h
    
    X = G.X():!=0; Y = G.Y():!=0
    if (D.wtype) {
        p11 = quadsum(( X:& Y):*G.w()) / G.W
        p10 = quadsum(( X:&!Y):*G.w()) / G.W
        p01 = quadsum((!X:& Y):*G.w()) / G.W
    }
    else {
        p11 = sum( X:& Y) / G.N
        p10 = sum( X:&!Y) / G.N
        p01 = sum(!X:& Y) / G.N
    }
    p00 = 1 - (p11 + p10 + p01)
    D.b[i] = (p11*p00) / (p10*p01)
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (p00/(p10*p01))         * (( X:& Y):-p11) +
        (p11/(p10*p01))         * ((!X:&!Y):-p00) -
        ((p11*p00)/(p10^2*p01)) * (( X:&!Y):-p10) -
        ((p11*p00)/(p10*p01^2)) * ((!X:& Y):-p01)
    ds_set_IF(D, G, i, h / G.W)
}

void ds_sum_rr(`Data' D, `Grp' G, `Int' i)
{
    `RS'    p00, p01, p10, p11, p1, p0
    `BoolC' X, Y
    `RC'    h
    
    X = G.X():!=0; Y = G.Y():!=0
    if (D.wtype) {
        p11 = quadsum(( X:& Y):*G.w()) / G.W
        p10 = quadsum(( X:&!Y):*G.w()) / G.W
        p01 = quadsum((!X:& Y):*G.w()) / G.W
    }
    else {
        p11 = sum( X:& Y) / G.N
        p10 = sum( X:&!Y) / G.N
        p01 = sum(!X:& Y) / G.N
    }
    p00 = 1 - (p11 + p10 + p01)
    p1 = p11 / (p11 + p01)
    p0 = p10 / (p10 + p00)
    D.b[i] = p1 / p0
    if (_ds_sum_omit(D, i)) return
    if (D.noIF) return
    h = (1/(p0*(p11+p01))) * (Y:*(X:-p1)) - (p1*(p10+p00)/p10^2) * (!Y:*(X:-p0))
    ds_set_IF(D, G, i, h / G.W)
}

end

exit




