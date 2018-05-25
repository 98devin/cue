
main {  

  get %2; # get input length so far (initially zero)
  
  # if the value from input isn't zero
  tst %0 ! %1 {

    # increment the counter    
    inc;
    put %2;  
    
    # recursively call without output yet
    cue main;
    die;
  
  }
  
  # otherwise output the length of (non-zero) arguments
  put %0;
  
}