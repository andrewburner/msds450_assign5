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


task.mat <- respondent_df %>%
  select(screen, RAM, processor, price, brand) %>%
  as.matrix()

X.mat <- efcode.attmat.f(task.mat)

dim(task.mat)
dim(X.mat)

head(task.mat)

# In X.mat, cols 1&2 are for the screen variable
# cols 3&4 = RAM, cols 5&6 = processor
# cols 7&8 = price, cols 9-11 = brand
# A value of 0 for an attribute in task.mat = -1's for all corresponding
# columns in X.mat
head(X.mat)

pricevec <- (respondent_df %>% pull(price)) - mean(respondent_df$price)

X.brands <- X.mat[,9:11]
X.BrandByPrice <- X.brands*pricevec

X.matrix <- cbind(X.mat, X.BrandByPrice)

ydata <- resp.data.v3 %>%
  select(starts_with("DCM1_"), -dcm1_timer) %>% 
  as.matrix()

sum(is.na(ydata))

zowner <- 1 * (!is.na(resp.data.v3$vList3))

#####################################
# This is the end of step 2
#####################################











