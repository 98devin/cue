
main {  
  get %0;   # load number of numbers to print
  put %2;   # set as option for fib procedure
  cue fib1;
  die;
}


fib1 {
  
  put %1;
  
  inc;
  put %1;
  put %1;
  
  cue fib2;

}


fib2 {
  
  get %2;
  dec;
  tst < %3 { die; }
  put %2;
  
  get %1;
  put %0;
  
  add %1;
  
  put %1;
  put %1;
  
  cue fib2;
  
}