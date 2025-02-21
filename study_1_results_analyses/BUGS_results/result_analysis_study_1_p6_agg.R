# this script generates plots of 3-way interaction terms

library(dplyr)
library(ggplot2)
library(purrr)
library(rlist)
library(QuantPsyc)
library(lm.beta)
library(plotly)
library(htmlwidgets)

# define color palette
ggcolor <- c('#F8766D', '#00BFC4', '#00BA38', '#FF61C3', '#619CFF')
# independent variables
iv <- c('size', 'prop', 'shift', 'range')
# dependent variables
dv <- colnames(data_wide)[8:15]
# creat a named list
var_name <- list('size' = 'SampleSize',
                 'prop' = 'ClassProp',
                 'shift' = 'ClsAbDist',
                 'range' = 'DifficulRange',
                 'Bias_1cwr' = 'Bias (Regular IRT)',
                 'Bias_2cco' = 'Bias (Mixture IRT)',
                 'SE_1cwr' = 'SE (Regular IRT)',
                 'SE_2cco' = 'SE (Mixture IRT)',
                 'RMSE_1cwr' = 'RMSE (Regular IRT)',
                 'RMSE_2cco' = 'RMSE (Mixture IRT)',
                 'Covr_len_1cwr' = 'Coverage Length (Regular IRT)',
                 'Covr_len_2cco' = 'Coverage Length (Mixture IRT)'
                 )

# retrieve a list of datasets
df <- map(dv, ~{data_wide %>% 
    select_at(c(.x, iv))})
map(df, colnames)

# 3-way interaction terms
inter_3way <- labels(terms(~.^3, data = data_wide[, iv]))
inter_2way <- labels(terms(~.^2, data = data_wide[, iv]))
term_3way <- setdiff(inter_3way, inter_2way) %>% 
  map(~{.x %>% 
      strsplit(split = ':', fixed = T) %>% 
      unlist()})

# create model formula containing 3-way interactions
set_3way_form <- function(DV = NULL, IV = NULL, data = data_wide){
  form_full <- paste0(DV, '~(', paste0(IV, collapse = ' + '), ')^4') %>% as.formula()
  mod_full <- lm(form_full, data = data)
  terms_all <- mod_full$terms %>% attr(which = 'term.labels')
  terms3 <- terms_all[c(1,2,3,4,11,12,13,14)]
  form3 <- paste0(DV, '~', paste0(terms3, collapse = ' + ')) %>% as.formula()
  return(form3)
}

set_2way_form <- function(DV = NULL, IV = NULL, data = data_wide){
  form_full <- paste0(DV, '~(', paste0(IV, collapse = ' + '), ')^4') %>% as.formula()
  mod_full <- lm(form_full, data = data)
  terms_all <- mod_full$terms %>% attr(which = 'term.labels')
  terms2 <- terms_all[1:10]
  form2 <- paste0(DV, '~', paste0(terms2, collapse = ' + ')) %>% as.formula()
  return(form2)
}


# formula list
# 3-way
form_list <- map(dv, ~{set_3way_form(DV = .x, IV = iv, data = data_wide)})
# 2-way
# form_2way_all <- map(dv, ~{set_2way_form(DV = .x, IV = iv, data = data_wide)})

# fit model to data, return estimated values
mod_data <- function(data = NULL, formula = NULL){
  mod <- lm(formula = formula, data = data)
  paste0('Adjusted R^2 is: ', summary(mod)$adj.r.squared) %>% 
    print()
  # estimated DV
  mod_fit <- mod$fitted.values
  mod_raw <- mod$model
  mod_raw$fit <- mod_fit
  return(mod_raw)
}

# model data list contains raw data and fitted values
mod_data_list <- map(form_list, ~{mod_data(data = data_wide, formula = .x)})

# fit model to data, return model results
mod_result <- function(data = NULL, formula = NULL){
  mod <- lm(formula = formula, data = data)
  paste0('Adjusted R^2 is: ', summary(mod)$adj.r.squared) %>% 
    print()
  return(mod)
}

############################################
# data summary, compute mean of the target metric
# C stack usage issue
# What turned out to be the key was my use of the group_by() and summarise() functions beforehand. I grouped my data according to months, and wanted the plot to display time on the x axis. However, plotly has special considerations for grouped data - from what I understand, it tries to make a separate trace for each group level. The exact combination of grouping and x-axis variable made this task apparently crash R.


# version 0.1, input data use estimated values
sumy_mean <- function(data = NULL, inter = NULL){
  unused <- setdiff(iv, inter)
  temp_df <- data %>% 
    select(!all_of(unused)) %>% 
    dplyr::group_by_at(inter) %>% 
    # to avoid C stack usage issue, ungroup in the end of operation
    # summarise_all(list('gmean' = mean)) %>% 
    summarise_at(c("fit"), list('gmean' = mean), na.rm = TRUE) %>% 
    dplyr::ungroup()
  if('size' %in% colnames(temp_df)){
    temp_df <- temp_df %>% 
      mutate(size = paste0('s', size),
             size = factor(size, levels = c('s600', 's1200')))
  }
  if('shift' %in% colnames(temp_df)){
    temp_df <- temp_df %>% 
      mutate(shift = paste0('m', shift),
             shift = factor(shift, levels = c('m1', 'm2.5')))
  }
  if('range' %in% colnames(temp_df)){
    temp_df <- temp_df %>%
      mutate(range = case_when(
        range == 'both' ~ 'entire',
        range == 'major only' ~ 'Major only'
      ))
  }
  attr(temp_df, "metric") <- colnames(data)[1]
  return(temp_df)
  # head(temp_df)
}

# use original metric
# for(i in seq_along(df)){
#   temp_df <- map(term_3way, ~{sumy_mean(data = df[[i]], inter = .x)})
#   walk(temp_df, inter_3d)
# }

# a function to generate 3D plots
inter_3d <- function(data = NULL){
  if(ncol(data) != 4){
    warning('The number of columns is not equal to 4. Something is wrong.')
  } else if(colnames(data)[4] != 'gmean'){
    warning('The last column is not the metric. Something is wrong.')}
  else {
      # get column names
      col_names <- colnames(data)
      # get the metric name
      metric_name <- attributes(data)$metric
      # prepare data subsets
      data_sub1 <- data %>% 
        filter_at(1, all_vars(. == levels(data[[1]])[1]))
      data_sub2 <- data %>% 
        filter_at(1, all_vars(. == levels(data[[1]])[2]))
      data_sub3 <- data %>% 
        filter_at(2, all_vars(. == levels(data[[2]])[1]))
      data_sub4 <- data %>% 
        filter_at(2, all_vars(. == levels(data[[2]])[2]))
      # draw 3D plot
      p <- plot_ly() %>% 
        add_trace(data = data, x = ~data[[1]], y = ~data[[2]], z = ~data[[4]], color = ~data[[3]], colors = ggcolor, type = 'scatter3d', mode = 'markers', showlegend = T) %>% 
        add_trace(data = data_sub1, x = ~data_sub1[[1]], y = ~data_sub1[[2]], z = ~data_sub1[[4]], color = ~data_sub1[[3]], colors = ggcolor, type = 'scatter3d', mode = 'lines', line = list(dash = "solid", width = 5), showlegend = F)  %>%
        add_trace(data = data_sub2, x = ~data_sub2[[1]], y = ~data_sub2[[2]], z = ~data_sub2[[4]], color = ~data_sub2[[3]], colors = ggcolor, type = 'scatter3d', mode = 'lines', line = list(dash = "solid", width = 5), showlegend = F)  %>%
        add_trace(data = data_sub3, x = ~data_sub3[[1]], y = ~data_sub3[[2]], z = ~data_sub3[[4]], color = ~data_sub3[[3]], colors = ggcolor, type = 'scatter3d', mode = 'lines', line = list(dash = "longdash", width = 5), showlegend = F)  %>%
        add_trace(data = data_sub4, x = ~data_sub4[[1]], y = ~data_sub4[[2]], z = ~data_sub4[[4]], color = ~data_sub4[[3]], colors = ggcolor, type = 'scatter3d', mode = 'lines', line = list(dash = "longdash", width = 5), showlegend = F)  %>%
        layout(
          scene = list(xaxis = list(title = var_name[[col_names[1]]]),
                       yaxis = list(title = var_name[[col_names[2]]]),
                       zaxis = list(title = paste0('Estimated ', var_name[[metric_name]]))
          ),
          legend = list(
            title = list(
              text = var_name[[col_names[3]]],
              x = 1,
              y = 1
            )
          ),
          showlegend = T)
      # p
      saveWidget(p, file = paste('./interaction_3way_new/Interaction_3way', metric_name, var_name[[col_names[1]]], var_name[[col_names[2]]], var_name[[col_names[3]]], '.html', sep = '_'), selfcontained = FALSE)
    }
}

# use estimated metric
for(i in seq_along(mod_data_list)){
  temp_df <- map(term_3way, ~{sumy_mean(data = mod_data_list[[i]], inter = .x)})
  walk(temp_df, inter_3d)
}
##########################################################

