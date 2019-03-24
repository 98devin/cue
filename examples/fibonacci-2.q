

main {

    # get number of times to count up    
    get %0; put %-1;
    
    # get first starting value
    get %0; put %1;
        
    # get second starting value
    get %0; put %2;
    
    # start fibonacci calculating thread
    cue fib, %1, %2, %3, %0;
    
    # setup constant for max values
    cue dupr_r,   %-1;
    
    # setup counter from 0
    cue count_r,  %-2, %-3;
    
    # continue calculations until
    # the counter reaches the constant.
    cue until_eq, %-1, %-2;
    
    # execution will end after,
    # leaving the results to be observed.
}


fib, a, b, apb, out {
    cue dupr, %a;
    cue putr, %a,   %out;           # output %a
    
    cue dupr, %b;
    cue addr, %a,   %b, %apb;       # %apb <- %a + %b
    
    cue putr, %b,   %a;             # %a <- %b
    cue putr, %apb, %b;             # %b <- %apb
    
    cue fib,  %a,   %b, %apb, %out; # repeat
}

until_eq, n, m {
    
    # if n != m then continue, and
    # requeue the check for later
    tst %n ! %m {
        cue until_eq, %n, %m;
        die;
    }
    
    # otherwise
    # end the program once everything
    # else already queued has finished.
    cue { end; }
    
}


# create an increasing counter
# in the queue %a, using the queue %c
# as a scratch register
count_r, a, c {
    get %c; inc;
    put %c; put %a;
    cue count_r, %a, %c;
}

# %apb <- %a + %b
addr, a, b, apb {
    get %a;
    add %b;
    put %apb;
}

# push the top value of %a
# back to itself, twice, to clone
# the value.
dupr, a {
    get %a;
    put %a; put %a;
}

# clone the top value of %a
# recursively, forever, e.g.
# to turn a queue into a constant value.
dupr_r, a {
    get %a;
    put %a; put %a;
    cue dupr_r, %a;
}

# %b <- %a
putr, a, b {
    get %a;
    put %b;
}