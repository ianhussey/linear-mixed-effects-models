########################################################################
# Random effects model for experiment 2 SCIAT data_df
# Author: Ian Hussey (ian.hussey@ugent.be) & Maarten De Schryver

########################################################################
# Dependencies
library(ez)
library(dplyr)
library(lme4)
library(car)
library(effects)
library(schoRsch)

########################################################################
# exp 2 SCIAT data
########################################################################

# data acquisition
setwd("/Users/Ian/Dropbox/Work/Manuscripts/Hussey & De Houwer, Learning via IAT CC/Random effects models/")
exp_2_sciat_df <- read.csv("processed data for analysis/exp 2 SCIAT data.csv")

exp_2_sciat_df$timepoint <- as.factor(exp_2_sciat_df$timepoint)

# For each participant Remove outliers: SD 2.5
#delete outliers from the data_dfset
exp_2_sciat_df <- outlier(exp_2_sciat_df, dv = "rt", todo="na", 
                          upper.limit = 10000, lower.limit = 100, 
                          upper.z = 2.5, lower.z = -2.5, 
                          factors = c("participant","condition","timepoint","block"),
                          z.keep = TRUE, z.name = "standard deviations",
                          print.summary = TRUE)

# returns:
# Function outlier() has screened 28000 trial(s) in total. 
# Of these were 877 trial(s) identified as outliers (3.13%).

# Analyses

# Fit a random effects models

# random intercept only for participant
model_3 <- lmer(rt ~ condition * timepoint * block + (1 | participant),
                contrasts = list(condition = "contr.sum",
                                 block = "contr.sum",
                                 timepoint = "contr.sum"),
                data = exp_2_sciat_df)

sink("random effects models/exp 2 SCIATs random effects model.txt")
summary(model_3)
cat("\n\n")
Anova(model_3, type="III")
sink()

# random intercept  for participant and stimulus
model <- lmer(rt ~ condition * timepoint * block + (1 | participant) + (1 | stimulus),
                contrasts = list(condition = "contr.sum",
                                 block = "contr.sum",
                                 timepoint = "contr.sum"),
                data = exp_2_sciat_df)
summary(model)
Anova(model, type="III")



# random intercept and random slope of block for participant and stimulus
model1 <- lmer(rt ~ condition * timepoint * block + (1 + block | participant) + (1|stimulus),
              contrasts = list(condition = "contr.sum",
                               block = "contr.sum",
                               timepoint = "contr.sum"),
              data = exp_2_sciat_df)
anova(model, model1)
summary(model1)
Anova(model1, type="III")

# random intercept slope of block + timepoint for participant and stimulus
model2 <- lmer(rt ~ condition * timepoint * block + (1 + block + timepoint| participant) + (1|stimulus),
               contrasts = list(condition = "contr.sum",
                                block = "contr.sum",
                                timepoint = "contr.sum"),
               data = exp_2_sciat_df)
anova(model1, model2)
summary(model2)
Anova(model1, type="III")

# random intercept slope of block + timepoint for participant and stimulus
# no random slopes needed for stimulus
model3 <- lmer(rt ~ condition * timepoint * block + (1 + block + timepoint| participant) + (1 + timepoint|stimulus),
               contrasts = list(condition = "contr.sum",
                                block = "contr.sum",
                                timepoint = "contr.sum"),
               data = exp_2_sciat_df)
anova(model2, model3)

# --> final model = model2

model2 <- lmer(rt ~   block*timepoint*condition + (1 + block + timepoint| participant) + (1|stimulus),
               contrasts = list(condition = "contr.sum",
                                block = "contr.sum",
                                timepoint = "contr.sum"),
               data = exp_2_sciat_df)
summary(model2)
Anova(model2, type="III")
plot(effect("block:timepoint:condition",model2))

ranef(model2)


# Check effect stimulus as fixd factor: chinese vs attribute


exp_2_sciat_df$type <- ifelse(exp_2_sciat_df$stimulus %in% c("pic1.jpg","pic2.jpg","pic3.jpg","pic4.jpg","pic5.jpg"),
                              1,0)
exp_2_sciat_df$type <- as.factor(exp_2_sciat_df$type)
levels(exp_2_sciat_df$type) <- c("att", "chi")

model3 <- lmer(rt ~ condition * timepoint * block*type + (1 + block + timepoint| participant),
               contrasts = list(condition = "contr.sum",
                                block = "contr.sum",
                                timepoint = "contr.sum",
                                type = "contr.sum"),
               data = exp_2_sciat_df)

summary(model3)
Anova(model3, type="III")
plot(effect("condition:type ",model3))


