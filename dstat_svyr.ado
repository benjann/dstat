*! version 1.0.2  27nov2020  Ben Jann

program dstat_svyr, eclass properties(svyr)
    version 14
    dstat `0'
    tempname b V
    mat `b' = e(b)
    mat `V' = I(`=colsof(`b')')
    mata: dstat_svyr_lbl_b()
    ereturn repost b=`b' V=`V', resize
    eret local cmd "dstat_svyr"
end

version 14
mata:
mata set matastrict on

void dstat_svyr_lbl_b()
{
    string matrix cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    cstripe[,1] = cstripe[,1] :+ "@" :+ cstripe[,2]
    cstripe[,2] = J(rows(cstripe), 1, "_cons")
    dstat_svyr_cstripe(st_local("b"), cstripe)
}

void dstat_svyr_cstripe(string scalar nm, string matrix S)
{
    real scalar br, uv

    br = setbreakintr(0)
    uv = st_numscalar("c(userversion)")
    if (uv>14.2) {
        (void) _stata("version 14.2, user", 1)
    }
    st_matrixcolstripe(nm, S)
    if (uv>14.2) {
        if (uv!=st_numscalar("c(userversion)")) {
            (void) _stata(sprintf("version %g, user", uv), 1)
        }
    }
    (void) setbreakintr(br)
}

end
exit


