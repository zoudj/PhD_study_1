
# get directories in the target directory
dirs <- list.dirs(path = './', recursive = F)
# identify 'chain' directories where results are stored, ignore seed directories
target_idx <- grepl('chain', dirs, fixed = TRUE)
dirs_t <- dirs[target_idx]


# count the number of files in each target directory
file_num <- vector()
# identify directory seed number
dir_seed <- vector()
seed_ignore <- vector()

for(t in seq_along(dirs_t)){
  # parse directory name to get directory seed number
  temp_str1 <- dirs_t[t] %>%
    strsplit(split = '/', fixed = T) %>% 
    unlist()
  temp_str2 <- temp_str1[length(temp_str1)] %>% 
    strsplit(split = '_', fixed = T) %>% 
    unlist() %>% 
    `[`(1)
  dir_seed[t] <- temp_str2
  
  # check if the chain is finished normally or early
  if(file.exists(paste0(dirs_t[t], '/log.txt'))){
    temp_log <- bugs.log(paste0(dirs_t[t], '/log.txt')) %>% 
      `[[`(1)
    if(is.matrix(temp_log)){
      temp_sample_size <- temp_log[, dim(temp_log)[2]] %>% 
        unique()
      # check the length of chains
      if(!(temp_sample_size %in% c(2500, 5000))){
        print(dirs_t[t])
        seed_ignore <- append(seed_ignore, temp_str2)
      }
    } else{
      print(dirs_t[t])
      seed_ignore <- append(seed_ignore, temp_str2)
    }
  }
  # count the number of files in each target directory
  # if the chain is not finished normally, the number of files would not equal 9 (the initial number of files is 3)
  temp_files <- list.files(path = dirs_t[t])
  file_num[t] <- length(temp_files)
  if((file_num[t] != 9) & (file_num[t] != 3)){
    print(dirs_t[t])
  }
  if(file_num[t] != 9){
    seed_ignore <- append(seed_ignore, temp_str2)
  }
}


dir_seed <- unique(dir_seed)
seed_ignore <- unique(seed_ignore)
seed_keep <- setdiff(dir_seed, seed_ignore)
print(paste0('Valid replications: ', length(seed_keep)))

# checking the number of valid replications, just in case there are abnormal results
if(length(seed_keep) > 50){
  set.seed(123)
  seed_final <- sample(seed_keep, size = 50)
} else if(length(seed_keep) <= 50){
  seed_final <- seed_keep
} 
print(paste0('Final selected replications: ', length(seed_final)))
if(length(seed_keep) < 50){
  warning('Final selected replications are less than 50!')
}

# get the index of experimental conditions from the directory name
condition_idx <- getwd() %>% 
  strsplit(split = '/', fixed = T) %>% 
  unlist() %>% 
  `[`(length(.)) %>% 
  strsplit(split = '_', fixed = T) %>% 
  unlist() %>% 
  `[`(2)

# import data in the target directory
class_idx <- read.csv('./true_class_idx.csv', header = T)
ability <- read.csv('./true_ability.csv', header = T)
difficulty <- read.csv('./item_diff.csv', header = T)
discrimination <- read.csv('./item_disc.csv', header = T)
# OpenBUGS results
data_all <- map(seed_final, ~{get_data(seed_idx = .x)})
# ability estimates derived from 1-calss regular IRT 
theta_1cwr <- map(data_all, ~{.x[[1]] %>% 
    select(starts_with('theta'))})
# ability estimates derived from 2-calss mixture IRT 
theta_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('theta'))})

# calculate mean and standard deviation of abilities
theta_1cwr_mean <- map_df(theta_1cwr, ~{.x['mean_1cwr',]}) 
theta_2cco_mean <- map_df(theta_2cco, ~{.x['mean_2cco',]}) 
theta_1cwr_sd <- map_df(theta_1cwr, ~{.x['sd_1cwr',]}) 
theta_2cco_sd <- map_df(theta_2cco, ~{.x['sd_2cco',]}) 

# item discrimination estimates
a_1cwr <- map(data_all, ~{.x[[1]] %>% 
    select(starts_with('a'))})
a_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('a'))})
# item difficulty estimates
b_1cwr <- map(data_all, ~{.x[[1]] %>% 
    select(starts_with('b'))})
b_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('b'))})

# class membership estimates derived from mixtue IRT
cmem_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('c_mem')) %>% 
    filter(row.names(.) %in% c('median_2cco')) %>% 
    as.vector() %>% 
    unlist()}) %>% 
  list.rbind() %>% 
  as.data.frame()

# calculate class membership accuracy
cmem_accuracy <- vector()
for(i in 1:nrow(cmem_2cco)){
  temp <- cmem_2cco[i,] %>%
    as.vector() %>%
    unlist()
  crosstab <- table(temp, class_idx$x, deparse.level = 2)
  if(dim(crosstab)[1] == 2){
    accuracy <- (crosstab[1,1] + crosstab[2,2])/sum(crosstab)
  } else if(dim(crosstab)[1] == 1){
    if(rownames(crosstab) == '1'){
      accuracy <- crosstab[1,1]/sum(crosstab)
    } else {
      accuracy <- crosstab[1,2]/sum(crosstab)
    }
  } else {
    print("Something is wrong in the crosstab.")
    accuracy <- NA
  }
  cmem_accuracy[i] <- accuracy
}

# class proportion estimates
pi_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('pi'))})
# class proportion ratio
pi_ratio <- map_dbl(pi_2cco, ~{.x[1,1]/.x[1,2]})
#######################################################
# calculate performance measures
# bias, standard error, coverage, etc.

# calculate theta bias
bias_theta_1cwr <- calc_bias(df = theta_1cwr_mean, true = ability)
bias_theta_2cco <- calc_bias(df = theta_2cco_mean, true = ability)

# range(bias_theta_1cwr)
# mean(bias_theta_1cwr)
# range(bias_theta_2cco)
# mean(bias_theta_2cco)

# visualize bias
p_bias <- ggplot(mapping = aes(x=ability$x, y= bias_theta_1cwr))+
  geom_point(color = 'cyan', alpha = 0.5) + 
  geom_point(aes(x=ability$x, y=bias_theta_2cco), inherit.aes = F, color = 'brown', alpha = 0.5) + 
  geom_smooth(mapping = aes(x=ability$x, y= bias_theta_1cwr), method = 'loess', color = 'navyblue') + 
  geom_smooth(mapping = aes(x=ability$x, y= bias_theta_2cco), method = 'loess', color = 'brown1') 
# p_bias

# ggsave('bias_scatter_plot.jpeg', p_bias, height = , width = , units = '', dpi = 300)

# calculate standard error
se_theta_1cwr <- calc_se(df = theta_1cwr_mean)
se_theta_2cco <- calc_se(df = theta_2cco_mean)
# range(se_theta_1cwr)
# mean(se_theta_1cwr)
# range(se_theta_2cco)
# mean(se_theta_2cco)

# visualize standard error
p_se <- ggplot(mapping = aes(x = ability$x, y = se_theta_1cwr)) +
  geom_point(color = 'seagreen') + 
  geom_point(aes(x=ability$x, y=se_theta_2cco), inherit.aes = F, color = 'brown')
# p_se

# calculate RMSE (root mean squared error)
rmse_theta_1cwr <- calc_rmse(df = theta_1cwr_mean, true = ability)
rmse_theta_2cco <- calc_rmse(df = theta_2cco_mean, true = ability)
# range(rmse_theta_1cwr)
# mean(rmse_theta_1cwr)
# range(rmse_theta_2cco)
# mean(rmse_theta_2cco)

# calculate coverage
covr_theta_1cwr <- calc_covr(df_mean = theta_1cwr_mean, df_sd = theta_1cwr_sd, true = ability)
covr_theta_2cco <- calc_covr(df_mean = theta_2cco_mean, df_sd = theta_2cco_sd, true = ability)
# covr_theta_1cwr
# covr_theta_2cco
# range(covr_theta_1cwr)
# range(covr_theta_2cco)
# mean(covr_theta_1cwr)
# mean(covr_theta_2cco)

# calculate coverage length
covr_len_theta_1cwr <- calc_covr_len(df = theta_1cwr_mean)
covr_len_theta_2cco <- calc_covr_len(df = theta_2cco_mean)
# range(covr_len_theta_1cwr)
# mean(covr_len_theta_1cwr)
# range(covr_len_theta_2cco)
# mean(covr_len_theta_2cco)



# save all results as a image
save.image(file = paste0('../results_new/', folder_t[j], '/Mixture_', condition_idx, "_results.RData"))
# remove all objects in the workspace, except for target folder and counter variable j.
rm(list = setdiff(ls(), c("folder_t", "j")))


#######################################################
