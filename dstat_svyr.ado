*! version 1.0.4  13dec2022  Ben Jann
*! helper program for -dstat, vce(svy)-; do not use manually

program dstat_svyr, eclass properties(svylb svyb svyj)
    version 14
    _parse comma lhs 0 : 0
    syntax [, NOSE * ]
    dstat `lhs', nose `options'
    tempname b V
    mat `b' = e(b)
    mat `V' = I(`=colsof(`b')')
    ereturn repost b=`b' V=`V', resize
    eret local cmd "dstat_svyr"
end

