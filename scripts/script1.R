library(tidyverse)

rats_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr24/main/data/rats.csv")

rats_df_42 <- filter(rats_df, batch == 42)

# binomial model, as glm
M_1 <- glm(cbind(m, n-m) ~ 1, data = rats_df_42, family = binomial())

plogis(coef(M_1))
