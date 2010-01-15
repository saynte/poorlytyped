#!/bin/bash

TIMEFORMAT="%R"
PATH=$PATH:./
cmd="rayhs"
sz="512"
outp="piccpp.pgm"

echo "CPP"

for l in 12 13;
do
  echo "Level: $l"

  for p in {1..8};
  do
    echo "Processors: $p"
    for i in {1..5};
    do
      opt="+RTS -qb0 -N$p -RTS"
      cmd2="$cmd $l $sz $opt";
      time  $cmd2 > $outp;
    done;
  done;
done;
