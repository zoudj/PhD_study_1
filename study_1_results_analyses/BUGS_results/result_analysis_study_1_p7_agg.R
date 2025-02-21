library(dplyr)
library(ggplot2)
library(purrr)
library(rlist)
library(lm.beta)

#ANOVA

data_wide <- data %>% 
  select(ID, grp_id, cdn_idx, size, prop, shift, coverage, Bias_1cwr, Bias_2cco, SE_1cwr, SE_2cco, RMSE_1cwr, RMSE_2cco, Covr_len_1cwr, Covr_len_2cco) %>% 
  mutate(size = factor(size, levels = c('600', '1200')),
         prop = factor(prop, levels = c('3to1', '5to1'), labels = c('3:1', '5:1')),
         shift = factor(shift, levels = c('merged', 'separated'), labels = c('N(1, 0.5)', 'N(2.5, 0.5)')),
         range = factor(coverage, c('C1only', 'full'), labels = c('Major Only', 'Entire')))

# sample_n(data_wide, size = 10)

# independent variables
iv <- c('size', 'prop', 'shift', 'range')
# dependent variables
dv <- colnames(data_wide)[8:15]

# fit model to data, return model results
# mod_result <- function(data = NULL, formula = NULL){
#   mod <- lm(formula = formula, data = data)
#   paste0('Adjusted R^2 is: ', summary(mod)$adj.r.squared) %>% 
#     print()
#   return(mod)
# }

mod_result <- function(data = NULL, formula = NULL){
  mod <- lm(formula = formula, data = data)
  paste0('R^2 is: ', summary(mod)$r.squared) %>% 
    print()
  return(mod)
}

# full model formula
form_full <- map(dv, ~{
  paste0(.x, '~(', paste0(iv, collapse = ' + '), ')^4') %>% as.formula()
})

# full model fitting
# predictors as continuous
# mod_full_list <- map(form_full, ~{mod_result(data = data_wide2, formula = .x)})

# predictors as binary instead
mod_full_list <- map(form_full, ~{mod_result(data = data_wide, formula = .x)})

##################################################
# 2-way interaction elements
two_way <- combn(iv, m = 2) %>% 
  data.frame() %>% 
  as.list()

# 3-way interaction elements
three_way <- combn(iv, m = 3) %>% 
  data.frame() %>% 
  as.list()
##################################################

# ggeffects - Estimated Marginal Means and Adjusted Predictions from Regression Models
# https://strengejacke.github.io/ggeffects/

library(ggeffects)
library(sjPlot)
library(patchwork)

# calculate predicted value for regular IRT data
data_pred_1cwr <- map(two_way, ~{
  df1 <- ggpredict(mod_full_list[[1]], terms = .x)
  df2 <- data.frame('X' = df1$x, 'G' = df1$group, 'predict' = df1$predicted)
  return(df2)
})
# calculate predicted value for mixture IRT data
data_pred_2cco <- map(two_way, ~{
  df1 <- ggpredict(mod_full_list[[2]], terms = .x)
  df2 <- data.frame('X' = df1$x, 'G' = df1$group, 'predict' = df1$predicted)
  return(df2)
})

#######################################
# creat a named list, the mapped names will be shown in the plot
var_name <- list('size' = 'Sample Size',
                 'prop' = 'Class Proportion',
                 'shift' = 'Class Ability Distribution',
                 'range' = 'Difficulty Range',
                 'Bias_1cwr' = 'Bias (Regular IRT)',
                 'Bias_2cco' = 'Bias (Mixture IRT)',
                 'SE_1cwr' = 'SE (Regular IRT)',
                 'SE_2cco' = 'SE (Mixture IRT)',
                 'RMSE_1cwr' = 'RMSE (Regular IRT)',
                 'RMSE_2cco' = 'RMSE (Mixture IRT)',
                 'Covr_len_1cwr' = 'Coverage Length (Regular IRT)',
                 'Covr_len_2cco' = 'Coverage Length (Mixture IRT)'
)


#######################################
# 2-way interaction plot function (2D)
plot_2way <- function(df = NULL, ivs = NULL, dv = NULL, pred_min = NULL, pred_max = NULL){
  p <- ggplot(data = df, aes(x = X, y = predict, linetype = G, group = G, label = round(predict, 3))) + 
    geom_line() + 
    geom_point() +
    geom_text(vjust = 0.5, hjust = 1, nudge_x = -0.02, size = 6) +
    ylim(pred_min, pred_max) +
    labs(x = var_name[[ivs[1]]], y = 'Predicted Bias')+
    guides(linetype = guide_legend(title = paste0(var_name[[ivs[2]]], ':'))) +
    scale_linetype_manual(values = c('dashed', 'dotted')) +
    scale_x_discrete(expand = expansion(mult = c(0.3, 0.7))) +
    theme(legend.position = c(0.85, 0.8),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14),
          legend.title=element_text(size=14), 
          legend.text=element_text(size=14))
  ggsave(filename = paste('Two_way_new5', dv, ivs[1], ivs[2], '.jpg', sep = '_'), plot = p, width = 1800, height = 900, units = 'px')
}

# 2-way interaction plot function (2D), but switching X vairable  and grouping variable
plot_2way <- function(df = NULL, ivs = NULL, dv = NULL, pred_min = NULL, pred_max = NULL){
  p <- ggplot(data = df, aes(x = G, y = predict, linetype = X, group = X, label = round(predict, 3))) + 
    geom_line() + 
    geom_point() +
    geom_text(vjust = 0.5, hjust = 1, nudge_x = -0.02, size = 6) +
    ylim(pred_min, pred_max) +
    labs(x = var_name[[ivs[2]]], y = 'Predicted Bias') +
    guides(linetype = guide_legend(title = paste0(var_name[[ivs[1]]], ':'))) +
    scale_linetype_manual(values = c('dashed', 'dotted')) +
    scale_x_discrete(expand = expansion(mult = c(0.3, 0.7))) +
    theme(legend.position = c(0.85, 0.8),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=14),
          legend.title=element_text(size=14), 
          legend.text=element_text(size=14))
  ggsave(filename = paste('Two_way_new5', dv, ivs[2], ivs[1], '.jpg', sep = '_'), plot = p, width = 1800, height = 900, units = 'px')
}

# calculate minimum and maximum of predicted values
# for use in y axis
min_1cwr <- map_dbl(data_pred_1cwr, ~{.x$predict %>% min()})
max_1cwr <- map_dbl(data_pred_1cwr, ~{.x$predict %>% max()})

min_2cco <- map_dbl(data_pred_2cco, ~{.x$predict %>% min()})
max_2cco <- map_dbl(data_pred_2cco, ~{.x$predict %>% max()})

min_all <- map2_dbl(min_1cwr, min_2cco, ~{
  c(.x, .y) %>% min()
})
max_all <- map2_dbl(max_1cwr, max_2cco, ~{
  c(.x, .y) %>% max()
})

# draw 2-way interaction plots and save each plot
pwalk(list(data_pred_1cwr, two_way, min_all, max_all), 
      function(dt, term, ymin, ymax){
  plot_2way(df = dt, ivs = term, 
            dv = 'Bias_Regular',
            pred_min = ymin, pred_max = ymax)
})

pwalk(list(data_pred_2cco, two_way, min_all, max_all), 
      function(dt, term, ymin, ymax){
        plot_2way(df = dt, ivs = term, 
                  dv = 'Bias_Mixture',
                  pred_min = ymin, pred_max = ymax)
      })



# walk2(data_pred_1cwr, two_way, ~{
#   plot_2way(df = .x, ivs = .y, dv = 'Bias_Regular')
# })

# walk2(data_pred_2cco, two_way, ~{
#   plot_2way(df = .x, ivs = .y, dv = 'Bias_Mixture')
# })

# draw 2-way interaction plots in batch
two_way_plots <- pmap(list(data_pred_2cco, two_way, min_all, max_all), 
      function(dt, term, ymin, ymax){
        plot_2way(df = dt, ivs = term, 
                  dv = 'Bias_Mixture',
                  pred_min = ymin, pred_max = ymax)
      })

# two_way_plots <- map2(data_pred_1cwr, two_way, ~{
#   plot_2way(df = .x, ivs = .y, dv = 'Bias_Regular')
# })


# two_way_plots <- map2(data_pred_2cco, two_way, ~{
#   plot_2way(df = .x, ivs = .y, dv = 'Bias_Mixture')
# })

# aggregate multiple plots in one large plot
# two_way_agg <- (two_way_plots[[1]]+two_way_plots[[2]])/(two_way_plots[[3]]+two_way_plots[[4]])/(two_way_plots[[5]]+two_way_plots[[6]])
# save aggregated plots
# ggsave(filename = 'Two_way_agg_Bias_regular.jpg', plot = two_way_agg, width = 2000, height = 2500, units = 'px')


####################################################