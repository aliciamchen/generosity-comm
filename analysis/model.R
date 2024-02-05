library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_classic(base_size = 20))

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
    data = d.base.weights, #%>% filter(first_action == "other"),
    aes(
      x = w_other,
      y = p_own,
      col = comm_weight, 
      group = comm_weight
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1.7, alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  scale_color_viridis_c( direction = -1) +
  facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(own generous) second action", title = "p(own) faceted by first action")

f

ggsave(here("figures/outputs/model.pdf"), width = 9, height = 4)

# beta prior
d.beta.prior <- read.csv(here("model/output/combined_beta_prior.csv"))

f <-
  ggplot(
    data = d.beta.prior, #%>% filter(first_action == "other"),
    aes(
      x = w_other,
      y = p_own,
      col = comm_weight, 
      group = comm_weight
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1.7, alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  scale_color_viridis_c( direction = -1) +
  facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(own generous) second action", title = "p(own) faceted by first action")

f

ggsave(here("figures/outputs/model_beta12.pdf"), width = 9, height = 4)



f <-
  ggplot(
    data = d.base.weights, #%>% filter(first_action == "other"),
    aes(
      x = w_other,
      y = p_other,
      col = comm_weight, 
      group = comm_weight
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1, alpha = 0.7) +
  scale_color_viridis_c( direction = -1) +
  facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(other generous) second action", title = "p(other) faceted by first action")

f

# plot beta distributions

p = seq(0,1, length=100)
beta_outputs <- data.frame(
  p = p,
  uniform = dbeta(p, 1, 1), 
  skewed = dbeta(p, 1, 2)
)

ggplot(data = beta_outputs, aes(x = p, y = uniform)) + 
  geom_line(linewidth = 1.5) +
  ylim(c(0, 2))

ggsave(here("figures/outputs/beta_1_1.pdf"), width = 4, height = 4)

ggplot(data = beta_outputs, aes(x = p, y = skewed)) + 
  geom_line(linewidth = 1.5) + 
  ylim(c(0, 2))


ggsave(here("figures/outputs/beta_1_2.pdf"), width = 4, height = 4)

# What if you add fairness motivations? 
d.fairness <- read.csv(here("model/output/combined_fairness_comm.csv"))

ggplot(data = d.fairness %>% filter(first_action == "other"), 
       aes(x = comm_weight, y = fairness_weight, fill = p_alternation)) + 
  geom_tile() + 
  scale_fill_distiller(direction = 1) 


#Fairness base weights - vary base weights and fairness weight
d.fairness.base.weights <- read.csv(here("model/output/combined_vary_base_weights_fairness.csv"))


f <-
  ggplot(
    data = d.fairness.base.weights, #%>% filter(first_action == "other"),
    aes(
      x = w_other,
      y = p_own,
      col = fairness_weight, 
      group = fairness_weight
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(own generous) second action", title = "p(own) faceted by first action")

f

f <-
  ggplot(
    data = d.fairness.base.weights, #%>% filter(first_action == "other"),
    aes(
      x = w_other,
      y = p_other,
      col = fairness_weight, 
      group = fairness_weight
    )
  ) +
  geom_point(size = 2) +
  geom_path(size = 1, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  facet_grid(~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(other generous) second action", title = "p(other) faceted by first action")

f


# output sequential

d.seq <- read.csv(here("model/output_sequential.csv"))
f <- 
  ggplot()
