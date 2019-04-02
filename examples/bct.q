

# implementation of bitwise cyclic tag.
# this uses 
    # -1 -> 0
    #  1 -> 1
# as the encoding for the binary input, primarily
# for simplicity, because of 0-valued empty queues.

main {
    
    # -5 : doom signal
    # -4 : doom counter
    
    # -3 : signal
    # -2 : signal
    
    # -1 : zero
    
    #  0 : i/o
    
    #  1 : instructions
    #  2 : data
    #  3 : copy of data[0]
    
    inc; put %-2; dec; # initialize signal
    
    cue read_i_, %0, %1,     %-1, %-2, %-3; # read instruction sequence
    cue read_d_, %0, %2, %3, %-1, %-3, %-2; # read data        sequence
    
    inc; inc;         # 2
    put %-4; mul %-4; # 4
    put %-4; add %-4; # 8
    put %-4; mul %-4; # 64
    # put %-4; mul %-4; # 4096
    put %-4;          # iterations until death.
    
    # halt execution eventually.
    cue doom_, %-1, %-4, %-2, %-5;
    
    # do interpretation (once the doom counter starts)
    cue interp_, %0, %1, %2, %3, %-1, %-5; 
    
}


sig, sig {
    inc; put %sig;
}


doom_, zero, count, sig1, sig2 {
    
    tst %sig1 = %zero {
        cue doom_, %zero, %count, %sig1, %sig2;
        die;
    }
    
    #inc; put %sig2;
    cue sig, %sig2;
    
    cue { cue doom, %zero, %count; }
}
    
doom, zero, count {
    get %count;
    tst < %zero { end; }
    dec; put %count;
    cue doom, %zero, %count;
}


interp_, out, inst, dat, dat0, zero, sig_begin {
    tst %sig_begin = %zero {
        cue interp_, %out,  %inst, %dat,
                     %dat0, %zero, %sig_begin;
        die;
    }
    cue interp, %out, %inst, %dat, %dat0, %zero;
}


interp, out, inst, dat, dat0, zero {
    
    cue { get %zero; put %out; }
    
    get %inst; put %inst;
    cue { put %out; }
    
    tst < %zero {
        cue {
            get %dat; put %out;
            get %dat; pop %dat0; 
            
            tst = %zero { cue { end; } }
            tst ! %zero { put %dat0; }
        }
    } 
    
    cue { get %zero; inc; inc; put %out; }
    
    tst > %zero {
        get %dat0; put %dat0;
        
        tst > %zero {
            cue {
                get %inst; put %inst;
                put %out;
                put %dat; put %dat;
            }
        } tst < %zero {
            cue {
                get %inst; put %inst;
            }
        }
        
    } 
    
    cue { get %zero; inc; inc; inc; put %out; }
    
    cue interp, %out, %inst, %dat, %dat0, %zero;
    
}


read_i_, from, to, zero, sig_begin, sig_end {
    tst %sig_begin = %zero { 
        cue read_i_, %from, %to, %zero, %sig_begin, %sig_end;
        die;
    }
    cue read_i, %from, %to, %zero, %sig_end;
}

    
read_i, from, to, zero, sig_end {
    get %from;
    tst = %zero {
        cue sig, %sig_end; die;
    }    
    put %to;
    cue read_i, %from, %to, %zero, %sig_end;
}


read_d_, from, to, to0, zero, sig_begin, sig_end {
    tst %sig_begin = %zero { 
        cue read_d_, %from, %to, %to0, %zero, %sig_begin, %sig_end;
        die;
    }
    cue read_d, %from, %to, %to0, %zero, %sig_end;
}

   
read_d, from, to, to0, zero, sig_end {
    get %from;
    tst = %zero {
        cue sig, %sig_end; die;
    }    
    put %to0; put %to;
    cue read_d, %from, %to, %to, %zero, %sig_end;
}
