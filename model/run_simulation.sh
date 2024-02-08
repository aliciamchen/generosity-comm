#!/bin/bash#

mkdir -p output/vary_base_weights

parallel --bar --delay 1 --colsep ','  -d "\r\n" "sh ./simulation.sh {1} {2} {3} {4} {5} {6}" :::: input/grid_vary_base_weights.csv

head -n 1 output/vary_base_weights/beta2_a1b1_cw0.0.csv > output/combined.out && tail -n+2 -q output/vary_base_weights/*.csv >> output/combined.out
mv output/combined.out output/model_output.csv
