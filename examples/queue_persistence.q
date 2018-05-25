

# put 3 to queue %1
foo {
  inc; inc; inc;
  put %1;
}

# put 2 to queue %1
bar {  
  inc; inc;
  put %1;
}

# Call baz after first doing bar, and foo.
# switching the order of foo and bar here
# will change the behavior in baz's output.
baz_entry {
  cue bar;
  cue foo;
  cue baz;
}


# check if %1[0] - %1[1] = 1
# if so, print result two times
# otherwise four times
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
  cue baz_entry;
}