*! version 1.0.3  27nov2020  Ben Jann

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
    if (uv>14.2) stata("version 14.2, user")
    st_matrixcolstripe(nm, S)
    if (uv>14.2) if (uv>14.2) stata("version "+strofreal(uv)+", user")
    (void) setbreakintr(br)
}

end
exit


