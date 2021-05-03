library(tidyverse)
library(bayesm)
library(dummies)

# Reading in data files and then checking the first couple rows of each
respondent_df <- read.csv('stc-dc-task-cbc -v3(1).csv', sep = '\t')
head(respondent_df)

load('stc-cbc-respondents-v3(1).RData')
head(resp.data.v3)

extra_df <- read.csv('extra-scenarios-v3.csv', sep = ',')
head(extra_df)


# This loads the two efcode functions
load('efCode.RData')

# Prints the functions to the console to see what they do
efcode.att.f
efcode.attmat.f










rhierMnlDP()

