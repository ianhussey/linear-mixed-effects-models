# title: mixed effects models - linear frequentist, linear bayes factors, and semi-parametric Probabalistic Index
# author: Ian Hussey (ian.hussey@ugent.be)
# license: 
#   code: GPLv3+        https://www.gnu.org/licenses/quick-guide-gplv3.en.html
#   data: CC BY 4.0     https://creativecommons.org/licenses/by/4.0/
#         please cite:  Hussey, I., & Hughes, S. (2016). Transitive relations and implicit attitudes. Dataset. osf.io/5nxby 


# dependencies ------------------------------------------------------------


library(ez)
library(dplyr)
library(car)
library(schoRsch)
library(afex)  # stacks on top of lmer for p values, eta2 etc
library(BayesFactor)
library(simr) # for power calculations via powerSim  
library(pim)


# data acquisition --------------------------------------------------------


setwd("/Users/Ian/git/linear mixed effects models/")

IAT_data <- 
  read.csv("sample data.csv") %>%
  filter(rt > 0) %>%  # log transformations require rts of 0 to be removed.
  mutate(block = ifelse(block == 1, "B1C1_block", "B1C2_block"),
         condition = as.factor(ifelse(condition == 1, "IAT_first", "IAT_second")),
         participant = as.factor(participant),
         block = as.factor(block),
         rt = log(rt))

# data transformations are done before trimming
IAT_data_outliers_removed <- 
  IAT_data %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5) %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5) 


# plots -------------------------------------------------------------------


# raw 
plot(density(IAT_data$rt), col = "red")
lines(density(IAT_data_outliers_removed$rt), col = "blue")
# rescaled
plot(density(IAT_data_outliers_removed$rt), col = "blue")

# by cell
# shorter history, history consistent IAT block - red line
plot(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_first" & 
                                            IAT_data_outliers_removed$block == "B1C1_block" ]), col = "red")

# shorter history, history inconsistent IAT block - green line
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_first" & 
                                             IAT_data_outliers_removed$block == "B1C2_block"]), col = "green")
# longer history, history consistent IAT block - blue line
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_second" & 
                                             IAT_data_outliers_removed$block == "B1C1_block"]), col = "blue")
# longer history, history inconsistent IAT block - magenta line
lines(density(IAT_data_outliers_removed$rt[IAT_data_outliers_removed$condition == "IAT_second" & 
                                             IAT_data_outliers_removed$block == "B1C2_block"]), col = "magenta")
# interactions
with(IAT_data_outliers_removed, interaction.plot(block, condition, rt))


# model 1 ------------------------------------------------------------------


# frequentist mixed linear effects model with participant as a random effect
# implemented using afex on top of lmer, to produce *p values*

# NB production of p values (over LRs etc) is contentious, but cite the
# following as recent evidence for the use of kenward roger method estiamtion: 
# http://link.springer.com/article/10.3758%2Fs13428-016-0809-y

# No effect sizes are produced due to contention over how to use the random
# factor error. See 
# http://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi

model_1 <- afex::mixed(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
                       contrasts = TRUE,  # set to true by default, but worth emphasising here that we're using effect coding not dummy coding.
                       data = IAT_data_outliers_removed,
                       type = 3,  # sum of squares
                       method = "KR",  # Kenward-Roger method of approximation of df for p values. Parametic bootstrapping ("PB") and liklihood ratio tests ("LR") also available.
                       progress = TRUE, 
                       return = "mixed")

save(model_1, file = "model_1_lmm_freq.RData")
#load(model_1, file = "model_1_lmm_freq.RData")

summary(model_1)
print(model_1)  # same as using anova() here

# intercepts for individual particiapnts in the random effect - requires a lmer object
#model_1b <- lmer(rt ~ block * condition + (1 | participant), # entering participant as a random effect acknowledges the non-independence of the multiple data points for each participant
#                data = IAT_data_outliers_removed,
#                contrasts = list(block="contr.sum", condition = "contr.sum"))  # effect coding rather than dummy coding
#lme4::ranef(model_1b)  

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


# model 2 ------------------------------------------------------------------


# Bayes factors mixed linear effects model with participant as a random effect

model_2 <- generalTestBF(rt ~ block * condition + participant, 
                         whichRandom = "participant",  # random factors
                         data = IAT_data_outliers_removed,
                         rscaleFixed = "medium",  # default 
                         rscaleCont = "medium",  # default
                         rscaleRandom = "nuisance",  # default
                         multicore = TRUE) 

save(model_2, file = "model_2_lmm_BF.RData")
#load(model_2, file = "model_2_lmm_BF.RData")

# all BF models
model_2

# BFs are transitive, so the contribution of the interaction is calculated by
# dividing the full model by the model without the interaction.
model_2["block + condition + block:condition + participant"] / 
  model_2["block + condition + participant"]

# write to disk
sink("2 BF linear mixed effects model.txt")
cat("FULL MODEL \n\n")
model_2
cat("\n\nINTERACTION ONLY (FULL MODEL DIVIDED BY NON-INTERACTIN MODEL) \n\n")
model_2["block + condition + block:condition + participant"] / 
  model_2["block + condition + participant"]
sink()


# model 3 -----------------------------------------------------------------


# NB semi parametric effects model - RUN TIME is c.3 HOURS!
# NB uncompressed model data is large: several gigabytes of memory, and saves to hundreds of megabytes on disk.
# NB does not include a random effect for the moment - not currently supported by PIM library.

model_3 <- pim(rt ~ block * condition,
               data = IAT_data)

#save(model_3, file = "model_3_pim.RData")
load(model_3, file = "model_3_pim.RData")

# summary
summary(model_3)

# p value to exact decimal places
summary(model_3)[,4]

# effect size
plogis(coef(model_3))

# 95% CIs on the PI effect size.
plogis(confint(model_3))

# write to disk
sink("3 semi-parametric fixed effects model.txt")
cat("SUMMARY \n\n")
summary(model_3)
cat("\n\nEXACT P VALUES \n\n")
summary(model_3)[,4]
cat("\n\nEFFECT SIZES \n\n")
plogis(coef(model_3))
cat("\n\n95% CIs ON THE ES \n\n")
plogis(confint(model_3))
sink()


