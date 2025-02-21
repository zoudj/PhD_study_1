
# this script is run to save data for further analyses
# retrieve indexes of experimental conditions
condition_idx <- folder_t[j] %>% 
  strsplit(split = '_', fixed = T) %>% 
  unlist() %>% 
  `[`(2)

###################################################
# save data to the current working directory
write.csv(cmem_2cco, file = './class_est.csv', row.names = F)
write.csv(theta_1cwr_mean, './theta_est_1cwr.csv', row.names = F)
write.csv(theta_1cwr_sd, './theta_sd_1cwr.csv', row.names = F)
write.csv(theta_2cco_mean, './theta_est_2cco.csv', row.names = F)
write.csv(theta_2cco_sd, './theta_sd_2cco.csv', row.names = F)

###################################################
# aggregate data and save it
data_save_1 <- data.frame('cdn_idx' = condition_idx, 'Ability' = ability$x, 'Class_idx' = class_idx$x, 'Bias_1cwr' = bias_theta_1cwr, 'Bias_2cco' = bias_theta_2cco, 'SE_1cwr' = se_theta_1cwr, 'SE_2cco' = se_theta_2cco, 'RMSE_1cwr' = rmse_theta_1cwr, 'RMSE_2cco' = rmse_theta_2cco, 'Covr_len_1cwr' = covr_len_theta_1cwr, 'Covr_len_2cco' = covr_len_theta_2cco)
write.csv(data_save_1, file = './data_save_1.csv', row.names = F)

data_save_2 <- data.frame('cdn_idx' = condition_idx, 'Accuracy_2cco' = cmem_accuracy, 'Cvor_1cwr' = covr_theta_1cwr, 'Covr_2cco' = covr_theta_2cco)
write.csv(data_save_2, file = './data_save_2.csv', row.names = F)

# save all results as a image
save.image(file = paste0('Mixture_', condition_idx, "_results_V2.RData"))
# remove all objects in the workspace
rm(list = setdiff(ls(), c("folder_t", "j")))


###################################################

