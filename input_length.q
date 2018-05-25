
main {  

  add %2;
  
  tst %0 ! %1 {
    inc;
    put %2;
    cue main;
    die;
  }
  
  put %0;
  
}