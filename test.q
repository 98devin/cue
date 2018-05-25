


foo {
  
  inc; inc; inc;
  put %1;
  
}


bar {
  
  inc; inc;
  put %1;
  
}


bazEntry {
  
  cue bar;
  cue foo;
  cue baz;
  
  cue 
  
}

baz {
  
  inc;
  put %1;
  
  get %1;
  sub %1;
  
  tst = %1 {
    put %0;
    put %0;
    die;
  } 
  
  put %0;
  put %0;
  put %0;
  put %0;
  
}


main {
  
  inc;
  inc;
 
  put %0;
  mul %0;
 
  inc;
  
  put %0;
  
  cue bazEntry;
  
}