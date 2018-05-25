
main {  
  get %0;   # load number of values to print from input
  put %2;   # set as option for fib procedure
  cue fib1;
}


fib1 {
  
  put %1;   # 0
  
  inc;
  put %1;   # 1
  put %1;   # copy of 1, for convenience in fib2
  
  cue fib2; # begin fib2

}


fib2 {
  
  get %2;           # number of remaining values desired
  dec;              # reduce by 1
  tst < %3 { die; } # end program if it's less than 0
  put %2;           # update remaining values
  
  get %1; # a
  put %0; # output a
  
  add %1; # compute (a + b), where b is the next fibonacci number
  
  put %1; # append (a + b) to queue 1
  put %1; # twice
  
  # therefore we went from queue %1 looking like
    # [a, b, b]
  # to
    # [b, (a + b), (a + b)]
  
  # which means we can just recurse to compute again:
  cue fib2;
  
}