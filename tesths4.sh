#!/bin/bash

TIMEFORMAT="%R"
PATH=$PATH:./
cmd="ray4hs"
sz="512"
outp="pichs4.pgm"

echo "CPP"

for l in 9 12 13;
do
  echo "Level: $l"
  for i in {1..5};
  do
    opt="+RTS -qb0 -N1 -RTS"
    cmd2="$cmd $l $sz $opt";
    time  $cmd2 > $outp;
  done;
done;
