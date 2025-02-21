
# this script primarily aggregates measures of parameter estimates and draws associated plots

library(dplyr)
library(tidyr)
library(stringr)
library(coda)
library(ggplot2)
library(ggpattern)
library(purrr)
source('../util/util_result_1.R')

# get directories in the target directory
folders <- list.dirs(path = './', recursive = F)
folder_idx <- grepl('Mixture', folders)
folder_t <- folders[folder_idx]
folder_t

# get indexes of experimental conditions
cdn_idx <- map_chr(folder_t, ~{.x %>% 
    strsplit(split = '_', fixed = T) %>% 
    unlist() %>% 
    `[`(2)})


# creating lists to store aggregated data
data_agg_1 <- list()
data_agg_2 <- list()
class_est <- list()
theta_est_1cwr <- list()
theta_est_2cco <- list()
theta_sd_1cwr <- list()
theta_sd_2cco <- list()

for(j in 1:length(folder_t)){
  data_agg_1[[j]] <- read.csv(file = paste0(folder_t[j], '/data_save_1.csv'))
  data_agg_2[[j]] <- read.csv(file = paste0(folder_t[j], '/data_save_2.csv'))
  class_est[[j]] <- read.csv(file = paste0(folder_t[j], '/class_est.csv'))
  theta_est_1cwr[[j]] <- read.csv(file = paste0(folder_t[j], '/theta_est_1cwr.csv'))
  theta_est_2cco[[j]] <- read.csv(file = paste0(folder_t[j], '/theta_est_2cco.csv'))
  theta_sd_1cwr[[j]] <- read.csv(file = paste0(folder_t[j], '/theta_sd_1cwr.csv'))
  theta_sd_2cco[[j]] <- read.csv(file = paste0(folder_t[j], '/theta_sd_2cco.csv'))
}

# condition-index vector
cdn_v <- map_chr(data_agg_1, ~{.x$cdn_idx %>% 
    unique()})
# condition names
cdn_name <- map_chr(cdn_v, ~{folder_t[cdn_idx == .x]})

# estimated class indexes (memberships)
class_est_agg <- map(class_est, ~{.x %>% 
    summarise_all(median)})
# frequency tables of estimated class indexes
class_est_agg2 <- map(class_est_agg, ~{.x %>% 
    unlist() %>% 
    table()})

# the average accuracy of classifications in 16 conditions
map_dbl(data_agg_2, ~{mean(.x$Accuracy_2cco, na.rm = T)})
map_dbl(data_agg_2, ~{median(.x$Accuracy_2cco, na.rm = T)})
#####################################################
# performance measures: overall
# bias aggregation
bias_1cwr_agg <- map_dbl(data_agg_1, ~{mean(.x$Bias_1cwr)})
bias_2cco_agg <- map_dbl(data_agg_1, ~{mean(.x$Bias_2cco)})
# print mean of individual biases (sample-level bias)
cbind(bias_1cwr_agg, bias_2cco_agg) %>% round(digits = 3)

# percentage bias
# this measure is not used in the dissertation
pbias <- map(data_agg_1, ~{
  ab_range <- range(.x$Ability)
  data.frame('cdn_idx' = .x$cdn_idx, 'pbias_1cwr' = (.x$Bias_1cwr / ab_range)*100, 'pbias_2cco' = (.x$Bias_2cco / ab_range)*100)})

# map(pbias, ~{mean(.x$pbias_1cwr)})
# map(pbias, ~{median(.x$pbias_1cwr)})
# map(pbias, ~{range(.x$pbias_1cwr)})
# 
# map(data_agg_1, ~{range(.x$Bias_1cwr)})

# SE (standard error) aggregation
# this measure is not used in the dissertation
se_1cwr_agg <- map_dbl(data_agg_1, ~{mean(.x$SE_1cwr)})
se_2cco_agg <- map_dbl(data_agg_1, ~{mean(.x$SE_2cco)})
cbind(se_1cwr_agg, se_2cco_agg) %>% round(digits = 3)

# RMSE (root mean squared error) aggregation
# this measure is not used in the dissertation
rmse_1cwr_agg <- map_dbl(data_agg_1, ~{mean(.x$RMSE_1cwr)})
rmse_2cco_agg <- map_dbl(data_agg_1, ~{mean(.x$RMSE_2cco)})
cbind(rmse_1cwr_agg, rmse_2cco_agg) %>% round(digits = 3)

# coverage aggregation
# typo in the column name: Cvor_1cwr should be Covr_1cwr
# this measure is not used in the dissertation
covr_1cwr_agg <- map_dbl(data_agg_2, ~{mean(.x$Cvor_1cwr)})
covr_2cco_agg <- map_dbl(data_agg_2, ~{mean(.x$Covr_2cco)})
cbind(covr_1cwr_agg, covr_2cco_agg) %>% round(digits = 3)

# accuracy of classifications aggregation
accu_2cco_agg <- map_dbl(data_agg_2, ~{mean(.x$Accuracy_2cco, na.rm = T)})

# put all aggregated data in a data frame
table_agg <- data.frame('condition'= cdn_name, 'bias_1cwr' = bias_1cwr_agg, 'bias_2cco' = bias_2cco_agg, 'se_1cwr' = se_1cwr_agg, 'se_2cco' = se_2cco_agg, 'rmse_1cwr' = rmse_1cwr_agg, 'rmse_2cco' = rmse_2cco_agg, 'covr_1cwr' = covr_1cwr_agg, 'covr_2cco' = covr_2cco_agg, 'accu_2cco' = accu_2cco_agg)

# save aggregated data
dir.create('results_agg')
write.csv(table_agg, './results_agg/table_agg.csv', row.names = F)

####################################################
# performance measures: based on true classes, i.e., the data are divided by true class membership, then measures are calculated

# bias aggregation
bias_by_cls <- map_df(data_agg_1, ~{.x %>% 
    group_by(Class_idx) %>% 
    summarise(bias1 = mean(Bias_1cwr), bias2 = mean(Bias_2cco))})
bias_by_cls$cdn <- rep(cdn_v, each = 2)
# SE aggregation
se_by_cls <- map_df(data_agg_1, ~{.x %>% 
    group_by(Class_idx) %>% 
    summarise(se1 = mean(SE_1cwr), se2 = mean(SE_2cco))})
se_by_cls$cdn <- rep(cdn_v, each = 2)
# RMSE aggregation
rmse_by_cls <- map_df(data_agg_1, ~{.x %>% 
    group_by(Class_idx) %>% 
    summarise(rmse1 = mean(RMSE_1cwr), rmse2 = mean(RMSE_2cco))})
rmse_by_cls$cdn <- rep(cdn_v, each = 2)
# coverage aggregation
covr_by_class <- list()
for(k in 1:length(theta_est_1cwr)){
  true_ab <- data_agg_1[[k]]$Ability
  true_class <- data_agg_1[[k]]$Class_idx
  ab_est_1cwr <- theta_est_1cwr[[k]]
  ab_est_2cco <- theta_est_2cco[[k]]
  ab_sd_1cwr <- theta_sd_1cwr[[k]]
  ab_sd_2cco <- theta_sd_2cco[[k]]
  
  true_ab_c1 <- true_ab[true_class == 1]
  true_ab_c2 <- true_ab[true_class == 2]
  ab_est_1cwr_c1 <- ab_est_1cwr[, true_class == 1]
  ab_est_1cwr_c2 <- ab_est_1cwr[, true_class == 2]
  ab_sd_1cwr_c1 <- ab_sd_1cwr[, true_class == 1]
  ab_sd_1cwr_c2 <- ab_sd_1cwr[, true_class == 2]
  ab_est_2cco_c1 <- ab_est_2cco[, true_class == 1]
  ab_est_2cco_c2 <- ab_est_2cco[, true_class == 2]
  ab_sd_2cco_c1 <- ab_sd_2cco[, true_class == 1]
  ab_sd_2cco_c2 <- ab_sd_2cco[, true_class == 2]
  
  covr_1cwr_c1 <- calc_covr(df_mean = ab_est_1cwr_c1, df_sd = ab_sd_1cwr_c1, true = true_ab_c1)
  covr_1cwr_c2 <- calc_covr(df_mean = ab_est_1cwr_c2, df_sd = ab_sd_1cwr_c2, true = true_ab_c2)
  
  covr_2cco_c1 <- calc_covr(df_mean = ab_est_2cco_c1, df_sd = ab_sd_2cco_c1, true = true_ab_c1)
  covr_2cco_c2 <- calc_covr(df_mean = ab_est_2cco_c2, df_sd = ab_sd_2cco_c2, true = true_ab_c2)
  temp_df <- data.frame('covr_1cwr_c1' = covr_1cwr_c1, 'covr_1cwr_c2' = covr_1cwr_c2, 'covr_2cco_c1' = covr_2cco_c1, 'covr_2cco_c2' = covr_2cco_c2)
  covr_by_class[[k]] <- temp_df
}

# aggregated table of coverage by true class membership 
table_covr_by_cls <- map_df(covr_by_class, ~{.x %>% summarise_all(mean)}) %>% 
  bind_cols(data.frame('cdn'= cdn_idx)) %>% 
  pivot_longer(cols = c('covr_1cwr_c1', 'covr_1cwr_c2', 'covr_2cco_c1', 'covr_2cco_c2'), names_to = 'model_by_cls', values_to = 'covr') %>% 
  rowwise() %>% 
  mutate('model' = strsplit(x = model_by_cls, split = '_')[[1]][2], 'class' = strsplit(x = model_by_cls, split = '_')[[1]][3]) %>% 
  pivot_wider(id_cols = c(cdn, class), names_from = model, values_from = covr)
names(table_covr_by_cls)[3:4] <- c('covr_1cwr', 'covr_2cco') 

# put all measures in an aggregated table
table_agg_by_cls <- cbind(bias_by_cls, se_by_cls[, c('se1', 'se2')], table_covr_by_cls[, c('covr_1cwr', 'covr_2cco')], rmse_by_cls[, c('rmse1', 'rmse2')])
table_agg_by_cls$cdn_name <- rep(cdn_name, each = 2)

# save the aggregated table
write.csv(table_agg_by_cls, './results_agg/table_agg_by_cls.csv', row.names = F)

####################################################
# calculate bias etc. based on mixture IRT's classification (not true class membership)
# not used in the dissertation
class_est_agg3 <- map(class_est_agg, ~{.x %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(Class_est = V1)
})
table_by_clsi <- map2(data_agg_1, class_est_agg3, ~{
  .x %>% 
    select(cdn_idx, Class_idx, Bias_2cco, SE_2cco, RMSE_2cco) %>% 
    bind_cols(.y)
})


table_by_clsi[[1]] %>% 
  group_by(Class_idx, Class_est) %>%
  summarise('Count' = n(), 'Bias_2cco' = mean(Bias_2cco), 'SE_2cco' = mean(SE_2cco), 'RMSE_2cco' = mean(RMSE_2cco))

table_by_clsi_agg <- map_df(table_by_clsi, ~{.x %>% 
    group_by(Class_idx, Class_est) %>%
    summarise('cdn_idx' = unique(cdn_idx), 'Count' = n(), 'Bias_2cco' = mean(Bias_2cco), 'SE_2cco' = mean(SE_2cco), 'RMSE_2cco' = mean(RMSE_2cco))})

write.csv(table_by_clsi_agg, './results_agg/table_agg_by_classification.csv', row.names = F)

####################################################
# plot density based on true class memberships
# prepare data used in ggplot()
theta_1cwr_agg <- map(theta_est_1cwr, ~{.x %>% 
    summarise_all(mean) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename('ab_1cwr' = V1)})

theta_2cco_agg <- map(theta_est_2cco, ~{.x %>% 
    summarise_all(mean) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename('ab_2cco' = V1)})

df_dens <- pmap(list(data_agg_1, theta_1cwr_agg, theta_2cco_agg), ~{..1 %>% 
    select(cdn_idx, Ability, Class_idx) %>% 
    cbind(..2) %>% 
    cbind(..3) %>% 
    mutate(Class_idx = as.character(Class_idx))})

# density plot function
p_density <- function(df = NULL){
  temp_cdn <- unique(df$cdn_idx)
  p_dens <- ggplot(data = df) + 
    geom_density(aes(x = Ability, color = Class_idx, linetype = '1'), adjust = 1) + 
    geom_density(aes(x = ab_1cwr, color = Class_idx, linetype = '2'), adjust = 1, alpha = 0.7) + 
    geom_density(aes(x= ab_2cco, color = Class_idx, linetype = '3'), adjust = 1, alpha = 0.7) + 
    scale_y_continuous(limits = c(NA, 1.2), n.breaks = 6) +
    scale_x_continuous(limits = c(-4, 4), n.breaks = 6)+ labs(x = expression(Ability(theta)), y = 'Density') + 
    scale_linetype_manual(name = 'Line Types', values = c(1, 3, 2), labels = c("True Value", "Regular IRT", "Mixture IRT")) +
    scale_color_manual(name = 'Classes', values = c("darkorange", "turquoise4"), labels = c("Class 1", "Class 2"))
  # p_dens
  ggsave(filename = paste0('./results_agg/Density_plot_by_true_classes_Mix_', temp_cdn, '.jpeg'), plot = p_dens, width = 2000, height = 1200, units = 'px', dpi = 300)
}

# draw density plots in batch
map(df_dens, p_density)


# plot density for mixture IRT only
p_dens_mix <- function(df = NULL){
  temp_idx <- unique(df$cdn_idx)
  p_dens <- ggplot(data = df) + 
    geom_density(aes(x = Ability, color = Class_idx, linetype = '1'), adjust = 1) + 
    geom_density(aes(x= ab_2cco, color = Class_idx, linetype = '3'), adjust = 1, alpha = 0.7) + 
    scale_y_continuous(limits = c(NA, 1.2), n.breaks = 6) +
    scale_x_continuous(limits = c(-4, 4), n.breaks = 6)+ labs(x = expression(Ability(theta)), y = 'Density') + 
    scale_linetype_manual(name = 'Line Types', values = c(1, 3, 2), labels = c("True Value", "Mixture IRT")) + 
    scale_color_manual(name = 'Classes', values = c("darkorange", "turquoise4"), labels = c("Class 1", "Class 2"))
  # p_dens
  ggsave(filename = paste0('./results_agg/Density_plot_by_true_classes_mix_only_cdn_', temp_idx, '.jpeg'), plot = p_dens, width = 2000, height = 1200, units = 'px', dpi = 300)
}
# draw density plots in batch
map(df_dens, p_dens_mix)

#######################################################
# plot overall bias (person-level biases of regular IRT and mixture IRT)
plot_bias <- function(df = NULL, xlimt = NULL, ylimt = NULL){
  temp_idx <- df$cdn_idx %>% unique()
  df1 <- data.frame('true_ab' = df$Ability, 'class_idx' = df$Class_idx, 'bias' = df$Bias_1cwr, 'Model' = 'A(regular IRT)')
  df2 <- data.frame('true_ab' = df$Ability, 'class_idx' = df$Class_idx, 'bias' = df$Bias_2cco, 'Model' = 'B(mixture IRT)')
  temp_df <- rbind(df1, df2)
  p_bias <- ggplot(data = temp_df) + 
    geom_point(mapping = aes(x = true_ab, y = bias, color = Model), alpha = 0.7, size = 1) + 
    geom_smooth(mapping = aes(x = true_ab, y = bias, color = Model), method = 'loess') + 
    labs(x = expression(True~Ability~(theta))) + 
    scale_y_continuous(expression(Bias~of~hat(theta)), limits = ylimt) + 
    theme(legend.position="bottom") + 
    lims(x = xlimt)
  p_bias
  ggsave(filename = paste0('./results_agg/Bias_plot_cdn_', temp_idx, '.jpeg'), plot = p_bias, width = 2000, height = 1000, units = 'px', dpi = 300)
}


# plot overall bias: person-level biases of regular IRT only
plot_bias_reg <- function(df = NULL, xlimt = NULL, ylimt = NULL){
  temp_idx <- df$cdn_idx %>% unique()
  df1 <- data.frame('true_ab' = df$Ability, 'class_idx' = df$Class_idx, 'bias' = df$Bias_1cwr, 'Model' = 'Regular IRT')
  p_bias <- ggplot(data = df1) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(mapping = aes(x = true_ab, y = bias), alpha = 0.7, size = 1) + 
    geom_smooth(mapping = aes(x = true_ab, y = bias), method = 'loess', color = 'black') + 
    scale_y_continuous(limits = ylimt) +
    theme(legend.position="none", 
          axis.text=element_text(size=20),
          axis.title.x=element_blank(), 
          axis.title.y=element_blank()) + 
    lims(x = xlimt)
  p_bias
  ggsave(filename = paste0('./results_agg/New3_bias_plot_regular_IRT_cdn_', temp_idx, '.jpeg'), plot = p_bias, width = 2000, height = 1000, units = 'px', dpi = 300)
}

# find minimum and maximum of abilities
# to be used as the limit of x axis
ab_min <- map_dbl(data_agg_1, ~{min(.x$Ability)}) %>% min()
ab_max <- map_dbl(data_agg_1, ~{max(.x$Ability)}) %>% max()
# find minimum and maximum of biases
# to be used as the limit of y axis
bias_min <- map_dbl(data_agg_1, ~{c(min(.x$Bias_1cwr), min(.x$Bias_2cco)) %>% min()}) %>% min()
bias_max <- map_dbl(data_agg_1, ~{c(max(.x$Bias_1cwr), max(.x$Bias_2cco)) %>% max()}) %>% max()

# plot both regular IRT and mixture IRT
map(data_agg_1, ~{plot_bias(df = .x, xlimt = c(ab_min, ab_max), ylimt = c(bias_min, bias_max))})

# plot regular IRT only
map(data_agg_1, ~{plot_bias_reg(df = .x, xlimt = c(ab_min, ab_max), ylimt = c(bias_min, bias_max))})
###################################################
# plot person-level biases: mixture IRT by true class membership

dot_bias_by_cls <- function(df = NULL, xlimt = NULL, ylimt = NULL){
  temp_idx <- df$cdn_idx %>% unique()
  df1 <- data.frame('true_ab' = df$Ability, 'class_idx' = factor(df$Class_idx, levels = c(1,2), labels = c('Major', 'Minor')), 'bias' = df$Bias_2cco, 'Model' = 'Mixture IRT')
  p_bias <- ggplot(data = df1) + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(mapping = aes(x = true_ab, y = bias, color = class_idx, group = class_idx, shape = class_idx), alpha = 1, size = 0.5) + 
    geom_smooth(mapping = aes(x = true_ab, y = bias, group = class_idx, linetype = class_idx), method = 'loess', color = 'black') + 
    scale_color_manual(values = c('grey35', 'black')) +
    scale_shape_manual(values = c(1, 16)) +
    scale_linetype_manual(values = c('dashed', 'solid')) +
    scale_y_continuous(limits = ylimt) + 
    guides(color = guide_legend(title = 'Class:')) +
    theme(legend.position = "none",
          legend.text = element_text(size=20), 
          axis.text=element_text(size=20),
          axis.title.x=element_blank(), 
          axis.title.y=element_blank()) +
    lims(x = xlimt)
  p_bias
  ggsave(filename = paste0('./results_agg/New4_individual_bias_by_cls_cdn_', temp_idx, '.jpeg'), plot = p_bias, width = 2000, height = 1000, units = 'px', dpi = 300)
}

# revise scatter plot and smoothing line
# black and white, change background to dark mode, dots to white
# the generated plots would be used in the paper for publication
dot_bias_by_cls <- function(df = NULL, xlimt = NULL, ylimt = NULL){
  temp_idx <- df$cdn_idx %>% unique()
  df1 <- data.frame('true_ab' = df$Ability, 'class_idx' = factor(df$Class_idx, levels = c(1,2), labels = c('Major', 'Minor')), 'bias' = df$Bias_2cco, 'Model' = 'Mixture IRT')
  p_bias <- ggplot(data = df1) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = 'white') +
    geom_point(mapping = aes(x = true_ab, y = bias, color = class_idx, group = class_idx, shape = class_idx), alpha = 1, size = 0.5) + 
    geom_smooth(mapping = aes(x = true_ab, y = bias, group = class_idx, linetype = class_idx), method = 'loess', color = 'white') + 
    scale_color_manual(values = c('#717171', 'white')) +
    scale_shape_manual(values = c(4, 16)) +
    scale_linetype_manual(values = c('dashed', 'solid')) +
    scale_y_continuous(limits = ylimt) + 
    guides(color = guide_legend(title = 'Class:')) +
    theme(legend.position = "none",
          legend.text = element_text(size=20), 
          axis.text=element_text(size=20),
          axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'black')) +
    lims(x = xlimt)
  p_bias
  ggsave(filename = paste0('./results_agg/New5_individual_bias_by_cls_cdn_', temp_idx, '.jpeg'), plot = p_bias, width = 2000, height = 1000, units = 'px', dpi = 300)
}

# find minimum and maximum of abilities
# to be used as the limit of x axis
ab_min <- map_dbl(data_agg_1, ~{min(.x$Ability)}) %>% min()
ab_max <- map_dbl(data_agg_1, ~{max(.x$Ability)}) %>% max()

# bias_min <- map_dbl(data_agg_1, ~{c(min(.x$Bias_1cwr), min(.x$Bias_2cco)) %>% min()}) %>% min()
# bias_max <- map_dbl(data_agg_1, ~{c(max(.x$Bias_1cwr), max(.x$Bias_2cco)) %>% max()}) %>% max()

# find minimum and maximum of biases
# to be used as the limit of y axis
bias_min <- map_dbl(data_agg_1, ~{min(.x$Bias_2cco) %>% min()}) %>% min()
bias_max <- map_dbl(data_agg_1, ~{max(.x$Bias_2cco) %>% max()}) %>% max()

# drawing plots in batch
map(data_agg_1, ~{dot_bias_by_cls(df = .x, xlimt = c(ab_min, ab_max), ylimt = c(bias_min, bias_max))})

###################################################

# drawing bar plots (sample-level biases)
# bias, SE, and coverage bar plots, comparison between two models 
# note the order of conditions is changed

# get the order of conditions to match that in the dissertation
cdn_order <- factor(paste0('Condition ', c(1, 10:16, 2:9)), levels = paste0('Condition ', 1:16))

# cdn_order <- factor(paste0('Condition ', 1:8), levels = paste0('Condition ', 1:8))
model_type <- factor(c(rep('Regular IRT', 16), rep('Mixture IRT', 16)), levels = c('Regular IRT', 'Mixture IRT'))
# aggregate data table for use in the bar plot
bias_agg <- data.frame('cdn' = rep(cdn_order, times = 2), 'bias' = c(bias_1cwr_agg, bias_2cco_agg), 'model' = model_type, 'idx' = rep(c(1, 10:16, 2:9), 2))

# draw all conditions in one plot
p_bar_bias <- ggplot(data = bias_agg) + 
  geom_bar(mapping = aes(x = cdn, y= bias, group = model, color = model, fill = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) + 
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = "bottom")
# display the plot
p_bar_bias
# save the plot
ggsave(filename = './results_agg/Bar_plot_bias_agg.jpeg', plot = p_bar_bias, width = 2500, height = 1000, units = 'px', dpi = 300)

# draw conditions 1-8 in one plot
# due to the limit of space, draw only the first 8 conditions
bias_agg_p1 <- bias_agg %>% 
  filter(idx <= 8)

p_bar_bias1 <- ggplot(data = bias_agg_p1) + 
  geom_bar(mapping = aes(x = cdn, y= bias, group = model, color = model, fill = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) + 
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  guides(fill = guide_legend(title = 'Model:'), color = guide_legend(title = 'Model:')) +
  theme(legend.position = c(0.9, 0.2))

# black and white bar plot, no patterns (for use in the paper)
# only change fills of bars
p_bar_bias1 <- ggplot(data = bias_agg_p1) + 
  geom_bar(mapping = aes(x = cdn, y= bias, group = model, fill = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) + 
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model, vjust = ifelse(bias >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  scale_fill_manual(values = c('grey5', 'grey')) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  guides(fill = guide_legend(title = 'Model:'), color = guide_legend(title = 'Model:')) +
  theme(legend.position = c(0.9, 0.2))

# balck and white plot for journal paper
p_bar_bias1 <- ggplot(data = bias_agg_p1) + 
  geom_bar_pattern(mapping = aes(x = cdn, y= bias, group = model, pattern = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8, fill = 'white', colour = 'grey', pattern_density = 0.5, pattern_fill = 'black', pattern_colour = 'white') +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model), position = position_dodge(width = 0.9), vjust = 1.5, size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  guides(
    group = guide_legend(title = 'Model:'),
    pattern = guide_legend(title = 'Model:')) +
  theme(legend.position = c(0.9, 0.2))                            
p_bar_bias1
ggsave(filename = './results_agg/Bar_plot_bias_agg1_new2.jpeg', plot = p_bar_bias1, width = 2000, height = 800, units = 'px', dpi = 300)

# draw conditions 9-16 in one plot
bias_agg_p2 <- bias_agg %>% 
  filter(idx >= 9)

p_bar_bias2 <- ggplot(data = bias_agg_p2) + 
  geom_bar(mapping = aes(x = cdn, y= bias, group = model, color = model, fill = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) + 
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  guides(fill = guide_legend(title = 'Model:'), color = guide_legend(title = 'Model:')) +
  theme(legend.position = c(0.9, 0.2))

# black and white bar plot, no patterns
# only change fills of bars
p_bar_bias2 <- ggplot(data = bias_agg_p2) + 
  geom_bar(mapping = aes(x = cdn, y= bias, group = model, fill = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) + 
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_fill_manual(values = c('grey5', 'grey')) +
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model, vjust = ifelse(bias >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  guides(fill = guide_legend(title = 'Model:'), color = guide_legend(title = 'Model:')) +
  theme(legend.position = c(0.9, 0.2))

# balck and white plot for journal paper
p_bar_bias2 <- ggplot(data = bias_agg_p2) + 
  geom_bar_pattern(mapping = aes(x = cdn, y= bias, group = model, pattern = model), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8, fill = 'white', colour = 'grey', pattern_density = 0.5, pattern_fill = 'black', pattern_colour = 'white') +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(aes(x = cdn, y= bias,label = format(round(bias,3), nsamll = 3), group = model, vjust = ifelse(bias >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + labs(x = 'Condition', y = 'Bias') + 
  guides(
    group = guide_legend(title = 'Model:'),
    pattern = guide_legend(title = 'Model:')) +
  theme(legend.position = c(0.9, 0.2))   

p_bar_bias2
ggsave(filename = './results_agg/Bar_plot_bias_agg2_new2.jpeg', plot = p_bar_bias2, width = 2000, height = 800, units = 'px', dpi = 300)

##################################################
# bar plots comparing bias based on true classes
# mixtrue IRT only

# prepare data
table_agg_by_cls$cdn <- rep(cdn_order, each = 2)
table_agg_by_cls$cdn_idx <- rep(c(1, 10:16, 2:9), each = 2)
table_agg_by_cls$Class_idx <- as.factor(table_agg_by_cls$Class_idx)
# draw plots
p_bar_bias_cls <- ggplot(data = table_agg_by_cls) + 
  geom_bar(mapping = aes(x = cdn, y= bias2, group = Class_idx, color = Class_idx, fill = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) + 
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = "bottom") + 
  guides(color = 'none') + scale_fill_manual(name = "Class Index", values = c('#F8766D', '#00BFC4'), labels = c('Major', 'Minor'))

p_bar_bias_cls

ggsave(filename = './results_agg/Bar_plot_bias_by_cls.jpeg', plot = p_bar_bias_cls, width = 2500, height = 1000, units = 'px', dpi = 300)

# data of first 8 conditions
table_agg_by_cls1 <- table_agg_by_cls %>% 
  filter(cdn_idx <= 8)
# data of last 8 conditions
table_agg_by_cls2 <- table_agg_by_cls %>% 
  filter(cdn_idx >= 9)

# bar plot of the first 8 conditions grouped by true class membership
p_bar_bias_cls1 <- ggplot(data = table_agg_by_cls1) + 
  geom_bar(mapping = aes(x = cdn, y= bias2, group = Class_idx, color = Class_idx, fill = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = c(0.9, 0.2)) + 
  guides(color = 'none') + 
  scale_fill_manual(name = "Class:", values = c('#F8766D', '#00BFC4'), labels = c('Major', 'Minor'))


# black and white bar plot, no patterns
# only change fills of bars
p_bar_bias_cls1 <- ggplot(data = table_agg_by_cls1) + 
  geom_bar(mapping = aes(x = cdn, y= bias2, group = Class_idx, fill = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx, vjust = ifelse(bias2 >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = c(0.9, 0.2)) + 
  guides(color = 'none') + 
  scale_fill_manual(name = "Class:", values = c('grey5', 'grey'), labels = c('Major', 'Minor'))

# black and white plot for journal paper
p_bar_bias_cls1 <- ggplot(data = table_agg_by_cls1) + 
  geom_bar_pattern(mapping = aes(x = cdn, y= bias2, group = Class_idx, pattern = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8, fill = 'white', colour = 'grey', pattern_density = 0.5, pattern_fill = 'black', pattern_colour = 'white') +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx, vjust = ifelse(bias2 >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = c(0.9, 0.2)) + 
  guides(group = guide_legend(title = 'Model:'),
         pattern = guide_legend(title = 'Model:')
         ) 



p_bar_bias_cls1
ggsave(filename = './results_agg/Bar_plot_bias_by_cls1_new2.jpeg', plot = p_bar_bias_cls1, width = 2000, height = 800, units = 'px', dpi = 300)

# bar plot of the last 8 conditions grouped by true class membership
p_bar_bias_cls2 <- ggplot(data = table_agg_by_cls2) + 
  geom_bar(mapping = aes(x = cdn, y= bias2, group = Class_idx, color = Class_idx, fill = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = c(0.9, 0.2)) +
  guides(color = 'none') + 
  scale_fill_manual(name = "Class:", values = c('#F8766D', '#00BFC4'), labels = c('Major', 'Minor'))

# black and white bar plot, no patterns
# only change fills of bars
p_bar_bias_cls2 <- ggplot(data = table_agg_by_cls2) + 
  geom_bar(mapping = aes(x = cdn, y= bias2, group = Class_idx, fill = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx, vjust = ifelse(bias2 >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = c(0.9, 0.2)) +
  guides(color = 'none') + 
  scale_fill_manual(name = "Class:", values = c('grey5', 'grey'), labels = c('Major', 'Minor'))

# black and white plot for journal paper
p_bar_bias_cls2 <- ggplot(data = table_agg_by_cls2) + 
  # geom_bar(mapping = aes(x = cdn, y= bias2, group = Class_idx, color = Class_idx, fill = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8) +
  geom_bar_pattern(mapping = aes(x = cdn, y= bias2, group = Class_idx, pattern = Class_idx), stat = 'identity', position = position_dodge(width = 0.9), width = 0.8, fill = 'white', colour = 'grey', pattern_density = 0.5, pattern_fill = 'black', pattern_colour = 'white') +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  geom_text(aes(x = cdn, y= bias2,label = format(round(bias2,3), nsamll = 3), group = Class_idx, vjust = ifelse(bias2 >= 0, -0.5, 1.5)), position = position_dodge(width = 0.9), size = 3) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = 'Condition', y = 'Bias') + 
  theme(legend.position = c(0.9, 0.2)) + 
  guides(group = guide_legend(title = 'Model:'),
         pattern = guide_legend(title = 'Model:')) 

p_bar_bias_cls2
ggsave(filename = './results_agg/Bar_plot_bias_by_cls2_new2.jpeg', plot = p_bar_bias_cls2, width = 2000, height = 800, units = 'px', dpi = 300)

#################################################


