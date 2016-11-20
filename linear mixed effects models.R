# title: mixed linear effects models for RT data
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+

# dependencies ------------------------------------------------------------


library(ez)
library(dplyr)
library(car)
library(schoRsch)
library(afex)  # stacks on top of lmer for p values, eta2 etc
library(BayesFactor)
library(simr) # for power calculations via powerSim  


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
# implemented using afex on top of lmer, to produce *p values*

model_1 <- afex::mixed(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       data = IAT_data_outliers_removed, 
                       type = 3,  # sum of squares
                       method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

summary(model_1)
print(model_1)  # same as using anova() here


# write to disk
sink("1 frequentist linear mixed effects model.txt")
summary(model_1)
print(model_1)  # same as using anova() here
sink()



# power analysis for mixed linear effects models ---------------------------


# post_hoc_power <- powerSim(model_1, 
#                            test = (rt ~ block + condition + (1 | participant)),
#                            seed = 20560,  # generated via www.random.org
#                            nsim = 100)
# post_hoc_power

# I'm unsure as to what "test" compares, and included a non interaction model 
# doesn't produce sensible output.
# https://cran.r-project.org/web/packages/simr/vignettes/examples.html


# model 3 ------------------------------------------------------------------


# Bayes factors mixed linear effects model with participant as a random effect

model_2 <- generalTestBF(rt ~ block * condition + participant, 
                         whichRandom = "participant",  # random factors
                         data = IAT_data_outliers_removed,
                         rscaleFixed = "medium",  # default 
                         rscaleCont = "medium",  # default
                         rscaleRandom = "nuisance",  # default
                         multicore = TRUE) 

# all BF models
model_2

# BFs are transitive, so the contribution of the interaction is calculated by
# dividing the full model by the model without the interaction.
model_2[9] / model_2[8]


# write to disk
sink("2 BF linear mixed effects model.txt")
cat("FULL MODEL \n\n")
model_2
cat("\n\nINTERACTION ONLY (FULL MODEL DIVIDED BY NON-INTERACTIN MODEL) \n\n")
model_2[9] / model_2[8]
sink()

 

