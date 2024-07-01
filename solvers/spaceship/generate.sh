#!/usr/bin/env bash

gfortran -O0 -g -fbounds-check stupid.f90 -o stupid
g++ ship.cpp -std=c++17 -g -Wextra -Wall -fsanitize=address -o ship

for i in ../../data/spaceship/*25*_sort10
do
  echo $i
  Nlines=`wc -l $i`
  printf "${i}\n${Nlines}" | ./stupid > ${i}.sol0
  #./ship $i > $i.sol1
done
