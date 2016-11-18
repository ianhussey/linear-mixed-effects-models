# Title: Process data from derivation experiment 2
# Author: Ian Hussey (ian.hussey@ugent.be)

# Dependencies ------------------------------------------------------------


library(ez)
library(dplyr)
library(lme4)
library(pim)
library(car)
library(effects)
library(schoRsch)
library(effsize)
library(pwr)
library(lsr)
library(simr)  # power calculations for lme4


# Data acquisition --------------------------------------------------------


setwd("/Users/Ian/git/mixed-effects-models/")

IAT_data <- 
  read.csv("long data.csv") %>%
  mutate(block = ifelse(block == 1, "B1C1_block", "B1C2_block"),
         condition = ifelse(condition == 1, "B1C1_condition", "B1C2_condition"))

IAT_data_outliers_removed <- 
  schoRsch::outlier(IAT_data, 
                    dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5)



# Plots -------------------------------------------------------------------


# raw vs trimmed 
plot(density(IAT_data$rt), col = "red")
lines(density(IAT_data_outliers_removed$rt), col = "blue")

# density plots
#first block consistent with history - red line - B1C1_condition, B1C1_block
plot(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "B1C1_condition" & IAT_data_outliers_removed$block == "B1C1_block" ]), col = "red")
#first block inconsistent with history - green line - B1C1_condition, B1C2_block
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "B1C1_condition" & IAT_data_outliers_removed$block == "B1C2_block"]), col = "green")
#first block inconcistent with history - blue line - B1C2_condition, B1C1_block
plot(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "B1C2_condition" & IAT_data_outliers_removed$block == "B1C1_block"]), col = "blue")
#first block consistent with history - magenta line - B1C2_condition, B1C2_block
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "B1C2_condition" & IAT_data_outliers_removed$block == "B1C2_block"]), col = "magenta")
# interactions
with(IAT_data_outliers_removed, interaction.plot(block, condition, rt))


# Analyses ----------------------------------------------------------------


# Traditional analysis when you calculate D1 scores and do a t test
# p = .002
# post hoc power = .95
# required sample size assuming desired power of .80 and the same effect size, required n = 46


# model 1 - linear effects model

# nb why no distinction between within and between factors?
# Case 2a here says there's no need but not sure why http://stats.stackexchange.com/questions/35590/linear-regression-with-repeated-measures-in-r

model_1 <- lm(rt ~ block * condition,
              data = IAT_data_outliers_removed)

summary(model_1)
Anova(model_1, type="II")

lsr::etaSquared(model_1, type = 2, anova = TRUE)
# interaction: p = 0.000009
# post hoc power = .99
# required sample size assuming same effect size = 46


# model 2 - mixed linear effects model - participant as random effect

model_2 <- lmer(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                data = IAT_data_outliers_removed)

summary(model_2)
Anova(model_2, type="II")
# interaction: p = 0.00000006

# power analysis for mixed linear effects models
# https://cran.r-project.org/web/packages/simr/vignettes/examples.html
powerSim(model_2, 
         seed = 9570,
         nsim = 100)


# model 3 - semi parametric effects model

model_3 <- pim(rt ~ block * condition,
               data = IAT_data)

summary(model_3)
#p value of interaction to exact decimal places
summary(model_3)[1][4]
#effec size
plogis(coef(model_3))
# interaction: p = 0.00000000000000000000000000000000000000000000002


# model 4 - semi parametric effects model

# PIM CAN'T DO RANDOM EFFECTS?


