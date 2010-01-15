#!/bin/bash

TIMEFORMAT="%R"
PATH=$PATH:./
cmd="raycpp"
sz="512"
outp="piccpp.pgm"

echo "CPP"

for l in 9 12 13;
do
  echo "Level: $l"

  for p in {1..8};
  do
    echo "Processors: $p"
    OMP_NUM_THREADS=$p
    for i in {1..5};
    do
      cmd2="$cmd $l $sz";
      time  $cmd2 > $outp;
    done;
  done;
done;
