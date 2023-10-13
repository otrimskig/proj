
# Load required R packages
library(tidyverse)
library(rstatix)
library(ggpubr)


# Pairwise comparisons
pwc <- PlantGrowth %>%
  pairwise_t_test(weight ~ group, p.adjust.method = "bonferroni")
pwc

pwc <- pwc %>% add_xy_position(x = "group")
pwc

# Show adjusted p-values

ggboxplot(PlantGrowth, x = "group", y = "weight") +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )



# Show significance levels
# Hide non-significant tests
ggboxplot(PlantGrowth, x = "group", y = "weight") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )