library(tidyverse)
library(emmeans)
library(lme4)

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


# One probability for each sub-population (batch)
M_3 <- glm(cbind(m, n-m) ~ factor(batch), data = rats_df, family = binomial())

emmeans(M_3, specs = ~ factor(batch), type = 'response')


# Multilevel model of all the sub-populations
# A model of binomial models, one per each sub population

M_4 <- glmer(cbind(m, n-m) ~ 1 + (1|batch),
             data = rats_df,
             family = binomial())

summary(M_4)

plogis(fixef(M_4))

plogis(fixef(M_4) - 2 * .66)
plogis(fixef(M_4) + 2 * .66)

# estimated probabilities for each sub-population (batch)
plogis(coef(M_4)$batch[,1])

ranef(M_4)

confint(M_4) # 95% CI on the fixed effect, i.e. the mean of the normal distribution
as.data.frame(ranef(M_4))

# Random effects normal model ---------------------------------------------

alcohol_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr24/main/data/alcohol.csv")

alcohol_df_russia <- filter(alcohol_df, country == 'Russia')

# assumes alcohol values are samples from a normal distribution
M_5 <- lm(alcohol ~ 1, data = alcohol_df_russia)

coef(M_5)
sigma(M_5)

M_6 <- lmer(alcohol ~ 1 + (1|country), data = alcohol_df)

summary(M_6)

coef(M_6) # the "mu"s, in the diagram/equations
ranef(M_6) # the "zeta"s, in the diagram/equations

# total variance = ...
22.208 + 1.108 
4.713 ^ 2 + 1.053 ^ 2

# Intraclass correlation coefficient (ICC)
(4.713 ^ 2) / (4.713 ^ 2 + 1.053 ^ 2)



# Linear mixed effects models ---------------------------------------------

ggplot(sleepstudy, aes(x = Days, y = Reaction)) + geom_point() +
  stat_smooth(method='lm', se = F)

ggplot(sleepstudy, 
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point()

ggplot(sleepstudy, 
       aes(x = Days, y = Reaction, colour = Subject)
) + stat_smooth(method = 'lm', se = F) + geom_point()


ggplot(sleepstudy, 
       aes(x = Days, y = Reaction, colour = Subject)
) + stat_smooth(method = 'lm', se = F) + geom_point() + facet_wrap(~Subject)

# non multilevel linear model, i.e. simple linear regression
# lm(Reaction ~ Days, data = sleepstudy)

# multilevel linear model
M_7 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

lm(dist ~ 1 + speed, data = cars)
lm(dist ~ 0 + speed, data = cars)


# random slopes AND intercepts
M_7a <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), data = sleepstudy)

# what is this? random intercepts only model
#M_7b <- lmer(Reaction ~ 1 + Days + (1|Subject), data = sleepstudy)

# what is this? random slopes only model
#M_7c <- lmer(Reaction ~ 1 + Days + (0 + Days|Subject), data = sleepstudy)


summary(M_7)


