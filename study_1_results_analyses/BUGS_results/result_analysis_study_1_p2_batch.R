
# this script should be run in 'BUGS_results' folder

library(dplyr)
library(coda)
library(ggplot2)
library(rlist)
library(purrr)
library(R2OpenBUGS)
library(tidyverse)

# get directories in the target directory
folders <- list.dirs(path = './', recursive = F, full.names = F)
folder_idx <- grepl('Mixture', folders)
folder_t <- folders[folder_idx]
folder_t


for(j in seq_along(folder_t)){
  # loading R scripts 
  source('../util/util_OpenBUGS.R')
  source('../util/util_result_1.R')
  # set working directory
  setwd(folder_t[j])
  print(paste0('Working on ', folder_t[j]))
  rdata <- list.files(pattern = 'results.RData')
  # load data
  load(file = rdata)
  # load result analysis file
  source('../result_analysis_study_1_p2.R')
  # reset working directory
  setwd('../')
}


