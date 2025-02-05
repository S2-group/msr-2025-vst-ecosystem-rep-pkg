setwd(".")
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(effectsize)

options(max.print=50)
font_size = 10

############################
# Read the data
############################

vst = read.csv("./CSVs Used/final_data_curated - final_data_curated_categorized.csv", stringsAsFactors = TRUE)
baseline = read.csv("../Repositories data/repo_final_mined_data/curated_csv/baseline_repos.csv", stringsAsFactors = TRUE)

for(i in 1:nrow(baseline)) {
  row <- baseline[i,]
  if(row['contributors_number'] == 0) {
    row['contributors_number'] <- 1
    baseline[i,] <- row
  }
}

summary(baseline$contributors_number)

# Repository size              
# Number of Commits 
# Number of Forks 
# Number of Stars 
# Number of Watchers 
# Number of Open PRs 
# Number of Closed PRs 
# Number of Contributors 
# Contributors’ experience in years 
# Repository owner type (i.e., organiz. or user) 
# License type (e.g., GPL, MIT)

features <- c('size', 
              'commits_number', 
              'forks_count', 
              'stargazers_count', 
              'subscribers_count', 
              'open_pull_requests', 
              'closed_pull_requests', 
              'open_issues_only', 
              'closed_issues_only', 
              'contributors_number')

for (i in seq(1, length(features))) {
  feat <- features[i]
  print(feat)
  print(summary(vst[[feat]]))
  print(summary(baseline[[feat]]))
  print(wilcox.test(vst[[feat]], baseline[[feat]]))
  boxplot(vst[[feat]], baseline[[feat]], outline=FALSE)
  title(feat)
}

summary(vst$owner.type)
summary(baseline$owner.type)

summary(vst$license.key)
summary(baseline$license.key)
