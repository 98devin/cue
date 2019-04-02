

# implementation of bitwise cyclic tag.
# this uses 
    # -1 -> 0
    #  1 -> 1
# as the encoding for the binary input, primarily
# for simplicity, because of 0-valued empty queues.

main {
    
    # -6 : end signal
    # -5 : doom signal
    # -4 : doom counter
    
    # -3 : signal
    # -2 : signal
    
    # -1 : zero
    
    #  0 : i/o
    
    #  1 : instructions
    #  2 : data
    #  3 : copy of data[0]
    
    #  4 : hold acc for `interp` closure
    
    cue sig_n2;
    
    cue read_i_; # read instruction sequence
    cue read_d_; # read data        sequence
    
    inc; inc;         # 2
    put %-4; mul %-4; # 4
    put %-4; add %-4; # 8
    put %-4; mul %-4; # 64
    # put %-4; mul %-4; # 4096
    put %-4;          # iterations until death.
    
    # halt execution eventually.
    cue doom_;
    
    # do interpretation (once the doom counter starts)
    cue interp_;
    
}

sig_n2 { inc; put %-2; }
sig_n3 { inc; put %-3; }
sig_n5 { inc; put %-5; }

doom_ {
    tst %-2 = %-1 {
        cue doom_; die;
    }
    cue doom;
    cue sig_n5;
}

doom {
    get %-4;
    tst < %-1 {
        get %-1; inc;
        put %-6; die;
    }
    dec; put %-4;
    cue doom;
}

interp_  {
    tst %-5 = %-1 {
        cue interp_; die;
    }
    cue interp;
}

mark0 {
    put %0;
}

mark2 {
    inc; inc;
    put %0;
}

mark3 {
    inc; inc; inc;
    put %0;
}

rot_inst {
    get %1; put %1;
}

rot_app_inst {
    get %1; put %1;
    put %0;
    put %2; put %2;
}

del_data {
    get %2; put %0;
    get %2; pop %3;
    
    tst = %-1 { inc; put %-6; die; }
    tst ! %-1 { put %3;   }
}

delay_put {
    get %4; put %0;
}

interp {
    
    tst %-6 ! %-1 { die; }
    
    cue mark0;
    
    get %1; put %1;
    
    put %4; cue delay_put;
    
    tst < %-1 {
        cue del_data;
    } 
    
    cue mark2;
    
    tst > %-1 {
        get %3; put %3;
        tst > %-1 {
            cue rot_app_inst;
        } tst < %-1 {
            cue rot_inst;
        }
    } 
    
    cue mark3;
    cue interp;    
}

read_i_ {
    tst %-2 = %-1 { 
        cue read_i_; die;
    }
    cue read_i;
}
    
read_i {
    get %0;
    tst = %-1 {
        cue sig_n3; die;
    }    
    put %1;
    cue read_i;
}

read_d_ {
    tst %-3 = %-1 { 
        cue read_d_; die;
    }
    cue read_d;
}
   
read_d {
    get %0;
    tst = %-1 {
        cue sig_n2; die;
    }    
    put %3; put %2;
    cue read_d1;
}

read_d1 {
    get %0;
    tst = %-1 {
        cue sig_n2; die;
    }    
    put %2; put %2;
    cue read_d1;  
}
