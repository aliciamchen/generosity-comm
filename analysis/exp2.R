library(here)
library(tidyverse)
library(tidyboot)
library(ggthemes)
library(lme4)
library(lmerTest)
library(wesanderson)
library(forcats)
library(emmeans)

theme_set(theme_classic(base_size = 30))
options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))

d <-
  read.csv(here('data/exp2_data.csv')) %>% filter(pass_attention == T, understood == 'yes') %>%
  pivot_longer(
    cols = c("more", "equal", "less"),
    names_to = "relationship",
    values_to = "likert_rating"
  ) %>%
  mutate(likert_rating = likert_rating + 1) %>%
  select(-c("understood", "pass_attention")) %>%
  mutate(
    social_interaction = fct_relevel(
      social_interaction,
      "reciprocity",
      "precedent",
      "no_interaction"
    ),
    relationship = fct_relevel(relationship,
                               "equal", "less", "more")
  ) %>%
  group_by(subject_id, story, social_interaction) %>%
  mutate(
    total_rating = sum(likert_rating, na.rm = T),
    normalized_likert_rating = likert_rating / total_rating
  ) %>%
  select(-total_rating)


d.demographics <- read.csv(here('data/exp2_demographics.csv'))
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
  group_by(relationship, social_interaction) %>%
  tidyboot_mean(likert_rating, na.rm = TRUE) %>%
  rename(likert_rating = empirical_stat)


# Aggregated results
f = ggplot(data = d,
           aes(x = social_interaction, y = likert_rating, fill = relationship)) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all,
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all,
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    wes_palette(name = "Cavalcanti1")[1],
    wes_palette(name = "Cavalcanti1")[3],
    wes_palette(name = "Cavalcanti1")[2]
  ),
  name = "power/status of generous person") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of generous person") +
  theme(legend.position = "bottom")

f


# Individual plots

f = ggplot(
  data = d %>% filter(social_interaction == "precedent"),
  aes(x = social_interaction, y = likert_rating, fill = relationship)
) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "precedent"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "precedent"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    "equal" = "#00A08A",
    "less" = "#35ACFF",
    "more" = "#B87FFF"
  )) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of generous person") +
  theme(legend.position = "bottom")

f


ggsave(
  here("figures/outputs/exp2_violin_prec.pdf"),
  width = 6,
  height = 7.5
)



f = ggplot(
  data = d %>% filter(social_interaction == "reciprocity"),
  aes(x = social_interaction, y = likert_rating, fill = relationship)
) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "reciprocity"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    "equal" = "#00A08A",
    "less" = "#35ACFF",
    "more" = "#B87FFF"
  )) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of generous person") +
  theme(legend.position = "bottom")

f


ggsave(here("figures/outputs/exp2_violin_rec.pdf"),
       width = 6,
       height = 7.5)

# Control

f = ggplot(
  data = d %>% filter(social_interaction == "no_interaction"),
  aes(x = social_interaction, y = likert_rating, fill = relationship)
) +
  geom_violin(width = 1.4,
              bw = 0.43,
              position = position_dodge(width = 0.8)) +
  geom_point(
    d.means.all %>% filter(social_interaction == "no_interaction"),
    mapping = aes(x = social_interaction, y = likert_rating),
    size = 2.3,
    alpha = 1,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    d.means.all %>% filter(social_interaction == "no_interaction"),
    mapping = aes(x = social_interaction, ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.8),
    size = 1.5,
    width = 0.09
  ) +
  scale_fill_manual(values = c(
    "equal" = "#00A08A",
    "less" = "#35ACFF",
    "more" = "#B87FFF"
  )) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     limits = c(0.8, 7.2)) +
  labs(x = "interaction sequence", y = "how likely?", fill = "status of altruistic person") +
  theme(legend.position = "bottom")

f


ggsave(
  here("figures/outputs/exp2_violin_control.pdf"),
  width = 6,
  height = 7.5
)




##################### STATS

emm_options(lmerTest.limit = 3179)
emm_options(pbkrtest.limit = 3179)

# With all levels
mod <-
  lmer(likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                  story) + (1 |
                                                                              subject_id),
       data = d)

summary(mod)

# Pairwise contrasts (main preregistered analysis)
emm <- emmeans(mod, pairwise ~ relationship * social_interaction)
emm

# relationship social_interaction emmean     SE  df lower.CL upper.CL
# more         precedent            4.07 0.0894 310     3.89     4.24
# less         precedent            3.69 0.0893 309     3.51     3.86
# equal        precedent            3.53 0.0892 308     3.36     3.71

# contrast                                   estimate    SE   df t.ratio p.value
# more precedent - less precedent              0.3802 0.104 3041   3.664  0.0077
# more precedent - equal precedent             0.5363 0.104 3041   5.172  <.0001
# less precedent - equal precedent             0.1561 0.104 3041   1.506  0.8530


# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test

# relationship_pairwise social_interaction_pairwise  estimate    SE   df t.ratio p.value
# more - less           precedent - no_interaction      0.288 0.147 3041   1.966  0.0494


# Group by asymmetric/symmetric
emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("asymmetric", "relationship", c("yes", "yes", "no"))


emm <- emmeans(emm, pairwise ~ asymmetric * social_interaction)
emm


contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test



##################################################

## Repeat all analyses with normalized values

# With all levels
mod <-
  lmer(
    normalized_likert_rating ~ 1 + social_interaction * relationship + (1 |
                                                                          story) + (1 |
                                                                                      subject_id),
    data = d
  )

summary(mod)

# Pairwise contrasts (main preregistered analysis)
emm <- emmeans(mod, pairwise ~ relationship * social_interaction)
emm


# Interaction contrasts
contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test



# Group by asymmetric/symmetric
emm <-
  mod %>% emmeans(pairwise ~ relationship * social_interaction) %>%
  add_grouping("asymmetric", "relationship", c("yes", "yes", "no"))


emm <- emmeans(emm, pairwise ~ asymmetric * social_interaction)
emm



contrast_test <-
  contrast(emm, interaction = c("pairwise", "pairwise"))
contrast_test
