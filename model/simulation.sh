#!/bin/bash#
webppl model.wppl  --require webppl-csv -- --beta $1 --fairness_weight $2 --comm_weight $3 --condition $4

# webppl model.wppl --require webppl-csv -- --beta 1 --fairness_weight 1 --comm_weight 1 --condition sequential
## TODO: figure out how to handle args not passed in