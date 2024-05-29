library(tidyverse)

rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr24/main/data/rats.csv")

rats_df_42 <- filter(rats_df, batch == 42)

# binomial model, as glm
M_1 <- glm(cbind(m, n-m) ~ 1, data = rats_df_42, family = binomial())

# MLE estimate of probability of developing tumour in batch 42
plogis(coef(M_1))

# 95% confidence interval on that probability
plogis(confint.default(M_1))


# binomial model, of all sub-populations, assuming fixed prob of tumour
# across all sub-populations
M_2 <- glm(cbind(m, n-m) ~ 1, data = rats_df, family = binomial())

# MLE estimate of probability of tumour
plogis(coef(M_2))

# 95% confidence interval for this estimate
plogis(confint.default(M_2))

# look at the sample probabilities
print(
  mutate(rats_df, p = m/n), 
  n = Inf)


