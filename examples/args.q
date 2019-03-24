

main {
    inc; inc; inc;
    mul %0;
    put %-3;
    cue spawn, %-3, %-2;
}

spawn, n, zero {
    get %n;
    tst <= %zero { die; }
    
    # n
    put %-1; put %-1; put %%-1;
    dec;
    
    # n - 1
    put %-1;
    dec;
    
    # n - 2
    put %-1;
    dec;
    
    # act(n, n - 1, n - 2, %0);
    cue act, %%-1, %%-1, %%-1, %0;
    
    # update n, recurse.
    put %n;
    cue spawn, %n, %zero;
}
 
act, inp, reg, sig, out {
    get %inp;
    put %out;
    
    cue sig, %sig;
}

sig, sig {
    inc; put %sig;
}



