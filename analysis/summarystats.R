library(here)
library(tidyverse)

d.demographics1 <-
  read.csv(here('data/exp1_demographics.csv')) %>% filter(pass_attention == T, understood == "yes")
d.demographics2 <-
  read.csv(here('data/exp2_demographics.csv')) %>% filter(pass_attention == T, understood == "yes")
d.demographics3 <-
  read.csv(here('data/exp3_demographics.csv')) %>% filter(pass_attention == T, understood == "yes")

combined <- rbind(d.demographics1, d.demographics2, d.demographics3)

combined %>% count(gender)
combined %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)
