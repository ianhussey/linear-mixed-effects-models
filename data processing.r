# Title: Process data from derivation experiment 2
# Author: Ian Hussey (ian.hussey@ugent.be)

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
                condition = ifelse(participant%%2 == 1, 1, ifelse(participant%%2 == 0, 2, NA)))

# MANUAL INCLUSIONS HERE - ONLY THOSE WITH COMPLETE DATA
inclusion_df <- read.csv("/Users/Ian/Dropbox/Work/Projects/Alphabet soup/Hussey & Hughes - Derivation study/OSF - Transitive relations and implicit attitudes/Experiment 3/Processed data/inclusion list.csv")

cleaned_df <- dplyr::inner_join(cleaned_df, inclusion_df, by = "participant")

# IAT rts -----------------------------------------------------------------


# select relevant data
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

write.csv(IAT_data, file = "/Users/Ian/git/linear mixed effects models/long data.csv", row.names = FALSE)

