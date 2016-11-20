# title: Process data from derivation experiment 2
# author: Ian Hussey (ian.hussey@ugent.be)

# dependencies ------------------------------------------------------------


library(ez)
library(dplyr)
#library(lme4)
library(car)
#library(effects)
library(schoRsch)
#library(effsize)
#library(pwr)
#library(simr)  # power calculations for lme4
library(afex)  # stacks on top of lmer for p values, eta2 etc
library(BayesFactor)


# data acquisition --------------------------------------------------------


setwd("/Users/Ian/git/linear mixed effects models/")

IAT_data <- 
  read.csv("long data.csv") %>%
  mutate(block = ifelse(block == 1, "B1C1_block", "B1C2_block"),
         condition = as.factor(ifelse(condition == 1, "IAT_first", "IAT_second")),
         participant = as.factor(participant))

IAT_data_outliers_removed <- 
  IAT_data %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5)


# plots -------------------------------------------------------------------


# raw vs trimmed 
plot(density(IAT_data$rt), col = "red")
lines(density(IAT_data_outliers_removed$rt), col = "blue")

# density plots
#first block consistent with history - red line - B1C1_condition, B1C1_block
plot(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_first" & 
                                            IAT_data_outliers_removed$block == "B1C1_block" ]), col = "red")

#first block inconsistent with history - green line - B1C1_condition, B1C2_block
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_first" & 
                                             IAT_data_outliers_removed$block == "B1C2_block"]), col = "green")
#first block inconcistent with history - blue line - B1C2_condition, B1C1_block
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_second" & 
                                             IAT_data_outliers_removed$block == "B1C1_block"]), col = "blue")
#first block consistent with history - magenta line - B1C2_condition, B1C2_block
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_second" & 
                                             IAT_data_outliers_removed$block == "B1C2_block"]), col = "magenta")
# interactions
with(IAT_data_outliers_removed, interaction.plot(block, condition, rt))


# model 1 ------------------------------------------------------------------


# frequentist mixed linear effects model with participant as a random effect

model_1 <- lmer(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                data = IAT_data_outliers_removed)

summary(model_1)
Anova(model_1, type="III")


# model 2 ------------------------------------------------------------------


# frequentist mixed linear effects model with participant as a random effect
# implemented using afex on top of lmer, to produce *p values*

model_2 <- afex::mixed(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       data = IAT_data_outliers_removed, 
                       type = 3,  # sum of squares
                       method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

summary(model_2)
print(model_2)  # same as using anova() here


# model 3 ------------------------------------------------------------------


# Bayes factors mixed linear effects model with participant as a random effect

model_3 <- generalTestBF(rt ~ block * condition + participant, 
                         whichRandom = "participant",  # random factors
                         data = IAT_data_outliers_removed,
                         rscaleFixed = "medium",  # default 
                         rscaleCont = "medium",  # default
                         rscaleRandom = "nuisance",  # default
                         multicore = TRUE) 

# all BF models
model_3

# BFs are transitive, so the contribution of the interaction is calculated by
# dividing the full model by the model without the interaction.
model_3[9] / model_3[8]


# power analysis for mixed linear effects models ---------------------------


# https://cran.r-project.org/web/packages/simr/vignettes/examples.html
# powerSim(model_2, 
#         seed = 9570,
#         nsim = 100)

# Error in getDefaultXname(fit) : 
#   Couldn't automatically determine a default fixed effect for this model.

