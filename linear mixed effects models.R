# title: Process data from derivation experiment 2
# author: Ian Hussey (ian.hussey@ugent.be)

# dependencies ------------------------------------------------------------


library(ez)
library(dplyr)
library(lme4)
library(car)
library(effects)
library(schoRsch)
library(effsize)
library(pwr)
library(lsr)
library(simr)  # power calculations for lme4
library(afex)  # stacks on top of lmer for p values, eta2 etc
library(BayesFactor)


# data acquisition --------------------------------------------------------


setwd("/Users/Ian/git/linear mixed effects models/")

IAT_data <- 
  read.csv("long data.csv") %>%
  mutate(block = ifelse(block == 1, "B1C1_block", "B1C2_block"),
         condition = as.factor(ifelse(condition == 1, "IAT_first", "IAT_second")),
         participant = as.factor(participant)) %>%
  filter(participant)

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


# analyses

# model 1 - linear effects model ------------------------------------------


# nb why no distinction between within and between factors?
# Case 2a here says there's no need but not sure why http://stats.stackexchange.com/questions/35590/linear-regression-with-repeated-measures-in-r

model_1 <- lm(rt ~ block * condition,
              data = IAT_data_outliers_removed)

summary(model_1)
Anova(model_1, type="II")

lsr::etaSquared(model_1, type = 2, anova = TRUE)

# power analysis
pwr.f2.test(u = 3, 
            v = 7823, 
            f2 = 0.0025,  # f2 = f^2 = p-eta^2 / ( 1 - p-eta^2 ), i.e., 0.002504936 / 0.99749506
            sig.level = 0.05, 
            power = NULL)

pwr.f2.test(u = 3, 
            v = NULL, 
            f2 = 0.00125,  # f2 = f^2 = p-eta^2 / ( 1 - p-eta^2 ), i.e., assume it is half what study 2 is.
            sig.level = 0.05, 
            power = .8)


# model 2 - mixed linear effects model - participant as random eff --------


model_2 <- lmer(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                data = IAT_data_outliers_removed)

summary(model_2)
Anova(model_2, type="III")


# power analysis for mixed linear effects models --------------------------


# https://cran.r-project.org/web/packages/simr/vignettes/examples.html
powerSim(model_2, 
         seed = 9570,
         nsim = 100)

# Error in getDefaultXname(fit) : 
#   Couldn't automatically determine a default fixed effect for this model.


# model 3a - mixed linear effects model - participant as random eff --------


# afex::mixed is used on top of lmer to calculate p values
# results returned as F and p values, i.e., ANOVA

model3a <- afex::mixed(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                      data = IAT_data_outliers_removed, 
                      type = 3,  # sum of squares
                      method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                      progress = TRUE, 
                      return = "mixed")

summary(model3a)
print(model3a)  # same as using anova() here


# model 3b - mixed linear effects model - participant as random eff --------


# afex::mixed is used on top of lmer to calculate p values
# results returned as Liklihood Ratios

model3b <- afex::mixed(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       data = IAT_data_outliers_removed, 
                       type = 3,  # sum of squares
                       method = "LR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

summary(model3b)
print(model3b)  # same as using anova() here


# model 4 - semi parametric effects model ---------------------------------


# model_4 <- pim(rt ~ block * condition,
#                data = IAT_data)
# 
# summary(model_4)
# #p value of interaction to exact decimal places
# summary(model_4)[1][4]
# #effec size
# plogis(coef(model_4))

# significant runtime, not run yet.


# model 5 - semi parametric effects model ---------------------------------


# PIM CAN'T DO RANDOM EFFECTS?



# model 6 - Bayes factor A ------------------------------------------------


# using linear model BF: produces a single BF for that model
model6 <- lmBF(rt ~ block * condition + participant, 
               whichRandom = "participant",  # random factors
               data = IAT_data_outliers_removed,
               rscaleFixed = "medium",  # default 
               rscaleRandom = "nuisance",  # default
               rscaleCont = "medium",  # default
               progress = TRUE) 

model6


# model 7 - Bayes factor B ------------------------------------------------


# using general BF test, which includes all nested models
model7 <- generalTestBF(rt ~ block * condition + participant, 
                        whichRandom = "participant",  # random factors
                        data = IAT_data_outliers_removed,
                        rscaleFixed = "medium",  # default 
                        rscaleRandom = "nuisance",  # default
                        rscaleCont = "medium",  # default
                        multicore = TRUE) 

model7



# subsampling -------------------------------------------------------------


# # the significance/Bayes factors are so overwhelming in the above that it is
# worth considering what the data would look like if we had used a different
# sample size. The below creates subsamples of the dataset, using say only 20
# participants, and then reanalyses it.

subsample_participants <- 
  read.csv("long data.csv") %>%
  mutate(block = ifelse(block == 1, "B1C1_block", "B1C2_block"),
         condition = as.factor(ifelse(condition == 1, "IAT_first", "IAT_second"))) %>%
  distinct(participant) %>%
  slice(1:20)

subsample_IAT_data_outliers_removed <- 
  read.csv("long data.csv") %>%
  mutate(block = ifelse(block == 1, "B1C1_block", "B1C2_block"),
         condition = as.factor(ifelse(condition == 1, "IAT_first", "IAT_second"))) %>%
  inner_join(subsample_participants) %>%
  mutate(participant = as.factor(participant)) %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5)


# model 6 subsample - Bayes factor A --------------------------------------


# using linear model BF: produces a single BF for that model
model6_subsample <- lmBF(rt ~ block * condition + participant, 
                         whichRandom = "participant",  # random factors
                         data = subsample_IAT_data_outliers_removed,
                         rscaleFixed = "medium",  # default 
                         rscaleRandom = "nuisance",  # default
                         rscaleCont = "medium",  # default
                         progress = TRUE) 

model6_subsample


# model 7 subsample - Bayes factor B  --------------------------------------


# using general BF test, which includes all nested models
model7_subsample <- generalTestBF(rt ~ block * condition + participant, 
                                  whichRandom = "participant",  # random factors
                                  data = subsample_IAT_data_outliers_removed,
                                  rscaleFixed = "medium",  # default 
                                  rscaleRandom = "nuisance",  # default
                                  rscaleCont = "medium",  # default
                                  multicore = TRUE) 

model7_subsample

