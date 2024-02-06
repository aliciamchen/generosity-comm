library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(glue)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))


d <-
  read.csv(here('data/exp1_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("alternating", "repeating", "none"),
    names_to = "next_interaction",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    next_interaction = fct_relevel(next_interaction,
                                   "alternating", "repeating", "none"),
    relationship = fct_relevel(relationship,
                               "equal", "less", "more")
  ) %>%
  group_by(subject_id, story, relationship) %>%
  mutate(
    total_rating = sum(likert_rating, na.rm = T),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating)

d.demographics <-
  read.csv(here('data/exp1_demographics.csv')) %>% filter(pass_attention == T, understood == "yes")
d.demographics %>% count(gender)
d.demographics %>% summarize(
  mean_age = mean(age),
  sd_age = sd(age),
  min_age = min(age),
  max_age = max(age)
)

print(length(unique(d$subject_id)))


################## PLOTS

d.means.all <-
  d %>% drop_na() %>%
  group_by(relationship, next_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


# Aggregated results on one plot
f = ggplot(data = d,
           aes(x = relationship, y = likert_rating, fill = next_interaction)) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(
    values = wes_palette(n = 3, name = "FantasticFox1"),
    name = "next interaction",
    breaks = c("repeating", "alternating", "none")
  ) +
  scale_x_discrete(limits = c("more", "less", "equal")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f


## For paper (each condition one plot)

f = ggplot(
  data = d %>% filter(relationship == "more"),
  aes(x = relationship, y = likert_rating, fill = next_interaction)
) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "more"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "more"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    "repeating" = "#DD8D29",
    "alternating" = "#E2D200",
    "none" = "#D6D6D6"
  )) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/exp1_violin_higher_cont.pdf"),
  width = 6,
  height = 7.5
)


f = ggplot(
  data = d %>% filter(relationship == "equal"),
  aes(x = relationship, y = likert_rating, fill = next_interaction)
) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "equal"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "equal"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    "repeating" = "#DD8D29",
    "alternating" = "#E2D200",
    "none" = "#D6D6D6"
  )) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/exp1_violin_equal_cont.pdf"),
  width = 6,
  height = 7.5
)


f = ggplot(
  data = d %>% filter(relationship == "less"),
  aes(x = relationship, y = likert_rating, fill = next_interaction)
) +
  geom_violin(width = 1.16,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(relationship == "less"),
    mapping = aes(x = relationship, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(relationship == "less"),
    mapping = aes(x = relationship, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    "repeating" = "#DD8D29",
    "alternating" = "#E2D200",
    "none" = "#D6D6D6"
  )) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "status of generous person", y = "how likely?", fill = "next interaction") +
  theme(legend.position = "bottom")

f

ggsave(
  here("figures/outputs/exp1_violin_lower_cont.pdf"),
  width = 6,
  height = 7.5
)


################## STATS


# With all levels
mod <-
  lmer(likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                story) + (1 |
                                                                            subject_id),
       data = d)

summary(mod)

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# Pairwise contrasts
emm <- mod %>% emmeans(pairwise ~ relationship * next_interaction)
emm

# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# replicate asymmetric/symmetric results from 1a
emm_symmetry <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emm_symmetry
emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction)

contrast(
  emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction),
  interaction = c("pairwise", "pairwise")
)


##################################################

## Repeat all analyses with normalized values

# With all levels
mod <-
  lmer(
    normalized_likert_rating ~ 1 + next_interaction * relationship + (1 |
                                                                        story) + (1 |
                                                                                    subject_id),
    data = d
  )

summary(mod)

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# Pairwise contrasts
emm <- mod %>% emmeans(pairwise ~ relationship * next_interaction)
emm



# Interaction contrasts - compare `asymmetric` and `symmetric` to `no_info`
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# replicate asymmetric/symmetric results from 1a
emm_symmetry <-
  mod %>% emmeans(pairwise ~ relationship * next_interaction) %>%
  add_grouping("asymmetry_present", "relationship", c("yes", "no", "yes"))

emm_symmetry
emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction)

contrast(
  emmeans(emm_symmetry, pairwise ~ asymmetry_present * next_interaction),
  interaction = c("pairwise", "pairwise")
)
