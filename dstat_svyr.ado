*! version 1.0.5  15dec2022  Ben Jann
*! helper program for -dstat, vce(svy)-; do not use manually

program dstat_svyr, eclass properties(svylb svyb svyj)
    version 14
    _parse comma lhs 0 : 0
    syntax [, NOSE * ]
    dstat `lhs', nose `options'
    tempname b V
    mat `b' = e(b)
    mata: st_matrix("`V'", diag(1 :- st_matrix("e(omit)")))
    ereturn repost b=`b' V=`V', resize
    eret local cmd "prop" // trick to skip _check_omit
    eret local cmd0 "dstat_svyr"
end

