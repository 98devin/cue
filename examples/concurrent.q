
main {
    get %0;  # take in number to count to
    put %-1; # store in alternate queue
    
    cue incr; # start incrementing thread
}

incr {
    get %-2; inc;
    put %-2; put %-2; # increment the counter
    
    get %-1;
    tst < %-2 { end; } # end program once we reach the number wanted.
    put %-1;
    
    cue puttr; # spawn new putter 'thread'
    cue incr;  # resume later
}

puttr {
    
    cue puttr; # resume later
    
    get %-2;
    put %-2;
    get %;
    
    
    tst = %-10 {
        get %-2;
        put %-2;
        put %;
        die;
    }
    
    put %-3;
    get %-2; put %-2; put %-2;
        
    mul %-3;
    put %%-2;
    
}
