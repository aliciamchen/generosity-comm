library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_few(base_size = 14.5))

palette <- c(
  "#cc494e",
  "#0096bf",
  "#a4d964",
  "#fcbe42",
  "#aa4fff",
  "#fc8e7c",
  "#fa9820",
  "#7593b6",
  "#f97a3b",
  "#ba3f5d",
  "#0063a2",
  "#4fa091"
)

show_col(palette)


d.model <- read.csv(here("model/output/combined_beta.csv"))

f <-
  ggplot(
    data = d.model,
    aes(
      x = comm_weight,
      y = p_alternation,
      col = beta, 
      group = beta
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1, alpha = 0.7) +
  scale_color_viridis_c(guide = "legend", breaks = c(1, 2, 4, 8)) +
  facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on communicative utility", y = "P(alternation)")

f


d.base.weights <- read.csv(here("model/output/combined_base_weights.csv"))

f <-
  ggplot(
    data = d.base.weights %>% filter(first_action == "other"),
    aes(
      x = w_other,
      y = p_alternation,
      col = comm_weight, 
      group = comm_weight
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1, alpha = 0.7) +
  scale_color_viridis_c( direction = -1) +
  # facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(own generous)", title = "first action other generous")

f


