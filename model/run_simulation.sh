#!/bin/bash#

# # vary beta
# parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3}" :::: input/grid_vary_beta.csv

# head -n 1 output/vary_beta/beta1_cw0.0.csv > output/combined.out && tail -n+2 -q output/vary_beta/*.csv >> output/combined.out
# mv output/combined.out output/combined_beta.csv


# vary base weights for comm
# parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3} {4}" :::: input/grid_vary_base_weights.csv

# head -n 1 output/vary_base_weights/beta2_cw0.0.csv > output/combined.out && tail -n+2 -q output/vary_base_weights/*.csv >> output/combined.out
# mv output/combined.out output/combined_base_weights.csv

# beta prior TODO: add argv for beta prior. right now its hard coded
parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3} {4}" :::: input/grid_vary_base_weights.csv

head -n 1 output/beta_prior/beta2_cw0.0.csv > output/combined.out && tail -n+2 -q output/vary_base_weights/*.csv >> output/combined.out
mv output/combined.out output/combined_beta_prior.csv

# # vary fairness and comm weights
# parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3} {4}" :::: input/grid_fairness_comm.csv

# head -n 1 output/fairness_comm/beta2_fw0.00_cw0.00.csv > output/combined.out && tail -n+2 -q output/fairness_comm/*.csv >> output/combined.out
# mv output/combined.out output/combined_fairness_comm.csv


# # vary base weights for fairness
# parallel --bar --colsep ',' "sh ./simulation.sh {1} {2} {3} {4}" :::: input/grid_vary_base_weights_fairness.csv

# head -n 1 output/vary_base_weights_fairness/beta2_fw0.00.csv > output/combined.out && tail -n+2 -q output/vary_base_weights_fairness/*.csv >> output/combined.out
# mv output/combined.out output/combined_vary_base_weights_fairness.csv