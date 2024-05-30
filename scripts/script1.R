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

# random intercepts only
M_8 <- lmer(Reaction ~ 1 + Days + (1|Subject), 
            data = sleepstudy)

summary(M_8)

# random slopes only
M_9 <- lmer(Reaction ~ Days + (0 + Days|Subject), 
            data = sleepstudy)
summary(M_9)


# random slopes AND random intercepts AND no correlation 
M_10 <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), 
            data = sleepstudy)

summary(M_10)

# random slopes AND random intercepts AND no correlation 
M_11 <- lmer(Reaction ~ Days + (Days||Subject), 
             data = sleepstudy)

anova(M_10, M_7)
anova(M_8, M_10)
anova(M_9, M_10)



# Prediction & visualization ----------------------------------------------

predict(M_7)

library(modelr)
add_predictions(sleepstudy, M_7) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('RI/RS/C')

add_predictions(sleepstudy, M_8) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('RI')

add_predictions(sleepstudy, M_9) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('RS')

add_predictions(sleepstudy, M_11) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('RI/RS')


# Model loglikelihood & deviances & LRT -----------------------------------

anova(M_11, M_7)

M_7_mle <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy, REML = FALSE)

M_11_mle <- lmer(Reaction ~ Days + (Days||Subject), 
                 REML = FALSE,
             data = sleepstudy)

logLik(M_7_mle)
deviance(M_7_mle)

logLik(M_11_mle)
deviance(M_11_mle)

deviance(M_11_mle) - deviance(M_7_mle)

anova(M_11_mle, M_7_mle)



# P-values etc ------------------------------------------------------------

M_12 <- lm(Reaction ~ Days, data = sleepstudy)
summary(M_12)
confint()

#library(lmerTest)

M_13 <- lmerTest::lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

# so don't forget about confidence intervals
confint(M_7)
confint(M_13)



# R^2 in mixed effects ----------------------------------------------------

m <- lm(dist ~ speed, data = cars)
summary(m)$r.sq

# Def 1: 1 - var of residual / var of outcome
1 - var(residuals(m)) / var(cars$dist)

# Def 2: var of predicted values/ var of outcome
var(predict(m)) / var(cars$dist)

# Def 3: var of predicted values / var of residual + var of predicted values
var(predict(m)) / (var(predict(m)) + var(residuals(m)))

# Def 4: 
m0 <- lm(dist ~ 1, data = cars)
1 - var(residuals(m))/var(residuals(m0))

# look at fixed effect predictions AND the fixed + random predictions
predict(M_7) # predictions based on fixed and random effects
predict(M_7, re.form = NA) # predictions based on fixed and random effects

add_predictions(sleepstudy, M_7) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('RI/RS/C')

mutate(sleepstudy, pred = predict(M_7, re.form = NA)) %>% 
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject) +
  ggtitle('RI/RS/C: fixed effects only prediction')


# Approx 1 of R^2 in mixed effects: Conditional R^2
var(predict(M_7)) / var(sleepstudy$Reaction)

# Approx 2 of R^2 in mixed effects: Marginal R^2
var(predict(M_7, re.form = NA)) / var(sleepstudy$Reaction)

performance::r2_nakagawa(M_7)

add_predictions(sleepstudy, M_7) |>
  ggplot(aes(x = Days, y = Reaction, colour = Subject)) +
  geom_point() +
  geom_line(aes(y = pred)) + # adds the best fit using fixed effects and random effects
  geom_line(data = mutate(sleepstudy, pred = predict(M_7, re.form = NA)), aes(y = pred)) + # add a best fit only using fixed effects
  facet_wrap(~Subject)


# Nested groups -----------------------------------------------------------


classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr24/main/data/classroom.csv")


ggplot(classroom_df, aes(x = ses, y = mathscore)) +
  geom_point() +
  stat_smooth(method = 'lm')


M_14 <- lmer(mathscore ~ ses + (ses|schoolid), data = classroom_df)
summary(M_14)
confint(M_14)


M_15 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|classid), data = classroom_df)
#M_16 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|schoolid/classid2), data = classroom_df)

summary(M_15)

# random effects model of mathscores
# 
M_16 <- lmer(mathscore ~ 1 + (1|schoolid) + (1|classid), data = classroom_df)

# remove correlation between random intercepts & slopes in classes
M_17 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses||classid), data = classroom_df)

# remove random slopes for classes

M_18 <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), 
             data = classroom_df)

M_19 <- lmer(mathscore ~ ses + (ses|schoolid) + (1|schoolid/classid2), 
             data = classroom_df)

M_20 <- lmer(mathscore ~ ses + (1|schoolid) + (1|classid), 
             data = classroom_df)
M_21 <- lmer(mathscore ~ ses + (1|schoolid/classid2), 
             data = classroom_df)
M_22 <- lmer(mathscore ~ ses + (1|schoolid) + (1|schoolid:classid2),
             data = classroom_df)

lm(len ~ supp * dose, data = ToothGrowth)
lm(len ~ supp + dose + supp:dose, data = ToothGrowth)

# crossed structures ------------------------------------------------------


blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr24/main/data/blp-short2.csv")


