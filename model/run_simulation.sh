#!/bin/bash#

# vary beta
parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3}" :::: input/grid_vary_beta.csv

head -n 1 output/vary_beta/beta1_cw0.0.csv > output/combined.out && tail -n+2 -q output/vary_beta/*.csv >> output/combined.out
mv output/combined.out output/combined_beta.csv


# vary base weights
parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3}" :::: input/grid_vary_base_weights.csv

head -n 1 output/vary_base_weights/beta2_cw0.0.csv > output/combined.out && tail -n+2 -q output/vary_base_weights/*.csv >> output/combined.out
mv output/combined.out output/combined_base_weights.csv