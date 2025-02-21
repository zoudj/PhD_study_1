
# this script is run for executing result_analysis_study_1_p1.R across all folders that store OpenBUGS results 

library(dplyr)
library(coda)
library(ggplot2)
library(rlist)
library(purrr)
library(R2OpenBUGS)
library(tidyverse)
library(patchwork)

# get directories in the target directory
folders <- list.dirs(path = './', recursive = F, full.names = F)
folder_idx <- grepl('Mixture', folders)
folder_t <- folders[folder_idx]
folder_t


for(j in 1:length(folder_t)){
  source('./util/util_OpenBUGS.R')
  source('./util/util_result_1.R')
  dir.create(paste0('./results_new/', folder_t[j]))
  # set working directory
  setwd(folder_t[j])
  print(paste0('Working on ', folder_t[j]))
  # load result analysis file
  source('../result_analysis_study_1_p1.R')
  # reset working directory
  setwd('../')
}
  

