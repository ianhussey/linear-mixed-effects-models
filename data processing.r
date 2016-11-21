# title: process IAT data
# author: Ian Hussey (ian.hussey@ugent.be)
# license: 
#   code: GPLv3+        https://www.gnu.org/licenses/quick-guide-gplv3.en.html
#   data: CC BY 4.0     https://creativecommons.org/licenses/by/4.0/
#         please cite:  Hussey, I., & Hughes, S. (2016). Transitive relations and implicit attitudes. Dataset. osf.io/5nxby 


# Clean the workspace
rm(list=ls())


# Dependencies ------------------------------------------------------------


library(plyr)
library(tidyverse)
library(data.table)


# Data acquisition and cleaning -------------------------------------------


## Set the working directory
setwd("/Users/Ian/Dropbox/Work/Projects/Alphabet soup/Hussey & Hughes - Derivation study/OSF - Transitive relations and implicit attitudes/Experiment 3/Raw Data")

# Read all files with the .iqdat extension
files <- list.files(pattern = "\\.csv$")  

# Read these files sequentially into a single data frame
input_df <- dplyr::tbl_df(plyr::rbind.fill(lapply(files, data.table::fread, header = TRUE)))  # tbl_df() requires dplyr, rbind.fill() requires plyr, fread requires data.table

# Make some variable names more transparent
cleaned_df <- 
  input_df %>%
  dplyr::select(subject,
                date,
                time,
                blockcode,  # name of block
                blocknum,
                trialnum,
                response,  # for string responses
                correct,
                latency,
                trialcode) %>%
  dplyr::rename(participant = subject,
                block_name = blockcode,
                block_n = blocknum,
                trial_n = trialnum,
                string_response = response,
                accuracy = correct,
                rt = latency) %>%
  dplyr::mutate(participant = as.numeric(participant),
                block_n = as.numeric(block_n),
                trial_n = as.numeric(trial_n),
                accuracy = as.numeric(accuracy),
                rt = as.numeric(rt),
                condition = ifelse(participant %% 2 == 1, 1, ifelse(participant %% 2 == 0, 2, NA)))  # condition was assigned based on odd/even participant code

# MANUAL INCLUSIONS HERE - ONLY THOSE WITH COMPLETE DATA
inclusion_df <- read.csv("/Users/Ian/Dropbox/Work/Projects/Alphabet soup/Hussey & Hughes - Derivation study/OSF - Transitive relations and implicit attitudes/Experiment 3/Processed data/inclusion list.csv")
cleaned_df <- dplyr::inner_join(cleaned_df, inclusion_df, by = "participant")


# subsample data ----------------------------------------------------------


# full dataset is not needed to demo the analyses

subsample_participants <- 
  cleaned_df %>% 
  distinct(participant) %>% 
  slice(1:50)  # first 50 participants

cleaned_df <- 
  cleaned_df %>% 
  inner_join(subsample_participants, by = "participant")  # include only participant codes from the above slice


# extract IAT rts ---------------------------------------------------------


IAT_data <-  
  cleaned_df %>%
  dplyr::filter(block_name == "IAT_test_compatible_block" | block_name == "IAT_test_incompatible_block") %>%
  dplyr::mutate(block = ifelse(block_name == "IAT_test_compatible_block", 1, 2)) %>%
  dplyr::select(participant, 
                condition,
                block,
                trial_n,
                rt,
                accuracy)


# Write to disk -----------------------------------------------------------


IAT_data %>% write.csv(file = "/Users/Ian/git/linear mixed effects models/example data.csv", row.names = FALSE)

