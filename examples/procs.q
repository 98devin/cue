

main {
    # doomsday clock
    get %0; put %-1;
    cue doom, %-1, %-2;
    
    put %2;
    
    get %0; put %1;
    cue func, %1, %2, %0;
}

func, a, b, out {
    cue dupr, %b;
    cue mulr, %a, %a, %b;
    cue {
        get %b; dec;      put %b;
        get %a; put %out; put %a;
    }
    cue func, %a, %b, %out;
}

dupr, a {
    get %a; put %a; put %a;
}

doom, n, zero {
    get %n;
    tst <= %zero { end; }
    dec; put %n;
    cue doom, %n, %zero;
}

mulr, dest, l, r {
    get %l;
    mul %r;
    put %dest;
}



