library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(ggpubr)
library(scales)

theme_set(theme_classic(base_size = 20))


# Beta(1, 1) prior
d <-
  read.csv(here("model/output/model_output.csv")) %>% filter(prior_a == 1, prior_b == 1)  #read.csv(here("model/output/combined_base_weights.csv"))

f <-
  ggplot(data = d,
         aes(
           x = w_other,
           y = p_own,
           col = comm_weight,
           group = comm_weight
         )) +
  geom_point(size = 2) +
  geom_path(size = 1.7, alpha = 0.7) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray") +
  scale_color_viridis_c(direction = -1) +
  facet_grid( ~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(own generous) second action", title = "p(own) faceted by first action")

f

ggsave(here("figures/outputs/model_beta11.pdf"),
       width = 9,
       height = 4)


# Beta(1, 2) prior
d <-
  read.csv(here("model/output/model_output.csv")) %>% filter(prior_a == 1, prior_b == 2)  # read.csv(here("model/output/combined_beta_prior.csv"))

f <-
  ggplot(data = d,
         aes(
           x = w_other,
           y = p_own,
           col = comm_weight,
           group = comm_weight
         )) +
  geom_point(size = 2) +
  geom_path(size = 1.7, alpha = 0.7) +
  geom_hline(yintercept = 0.5,
             linetype = "dashed",
             color = "gray") +
  scale_color_viridis_c(direction = -1) +
  facet_grid( ~ first_action, labeller = label_parsed) +
  labs(x = "weight on other", y = "P(own generous) second action", title = "p(own) faceted by first action")

f

ggsave(here("figures/outputs/model_beta12.pdf"),
       width = 9,
       height = 4)


# plot beta distributions

p = seq(0, 1, length = 100)
beta_outputs <- data.frame(p = p,
                           uniform = dbeta(p, 1, 1),
                           skewed = dbeta(p, 1, 2))

ggplot(data = beta_outputs, aes(x = p, y = uniform)) +
  geom_line(linewidth = 1.5) +
  ylim(c(0, 2))

ggsave(here("figures/outputs/beta_1_1.pdf"),
       width = 4,
       height = 4)

ggplot(data = beta_outputs, aes(x = p, y = skewed)) +
  geom_line(linewidth = 1.5) +
  ylim(c(0, 2))


ggsave(here("figures/outputs/beta_1_2.pdf"),
       width = 4,
       height = 4)
