# created by DZ, Sept. 20, 2021
# Functions in this script are used for generating simulated mixture data.

library(dplyr)
library(tidyr)
library(ggplot2)
library(extraDistr)

# set sub-population proportion, e.g., "1:1", "1:9", "1:1:1"
# if there's no sub-populations, then input "1"
# currently it can only handle two classes
get_prop <- function(prop_str){
  # input is a string indicating proportions of sub-populations
  if(!is.character(prop_str)){
    warning("The input should be a string indicating proportions, numbers separated by colons, such as '1:1'.")
  }
  if(grepl(':', prop_str, fixed = T)){
    temp_v <- strsplit(prop_str, ":", fixed = T) %>%
      unlist() %>%
      as.numeric()
    
  } else if(grepl('to', prop_str, fixed = T)){
    temp_v <- strsplit(prop_str, "to", fixed = T) %>%
      unlist() %>%
      as.numeric()
  } else {
    warning("Something is wrong.")
  }
  # get first proportion
  num1 <- temp_v[1]
  # get second proportion
  num2 <- temp_v[2]
  # calculate pi1 & pi2
  pi1 <- num1 / (num1 + num2)
  pi2 <- num2 / (num1 + num2)
  return(c(pi1, pi2))
}

# get class index for a group of simulees
# parameter 1: number of simulees
# parameter 2: a proportion vector returned from get_prop() 
get_class_idx <- function(N = NA, prop = NULL){
  c1 <- rep(1, floor(N * prop[1]))
  c2 <- rep(2, N - length(c1))
  class_idx <- sample(c(c1, c2), N, replace = F)
  # use rcat() from extraDistr package
  # class_idx <- rcat(n = N, prob = prop)
  paste("Class 1 got", table(class_idx)[1], "simulees, class 2 got", table(class_idx)[2], "simulees.", sep = " ") %>% print()
  return(class_idx)
}

# create an ability vector for the mixture sample
# parameter 1: number of simulees
# parameter 2: a vector indicating simulee's class
# parameter 3: mean of class one 
# parameter 4: variance of class one
# parameter 5: mean of class two
# parameter 6: variance of class two
# parameter 7: distribution for class one
# parameter 8: distribution for class two
get_ability <- function(N = NULL, class_idx = NULL, c1_m = NA, c1_v = NA, c2_m = NA, c2_v = NA, c1_distri = "normal", c2_distri = "normal", c1_min = NA, c1_max = NA, c2_min = NA, c2_max = NA){
  ability <- numeric(N)
  for(i in seq_along(class_idx)){
    # sample a theta value from a normal distribution according to the class index for each simulee
    if(class_idx[i] == 1){
      # when the simulee belongs to class 1
      if(c1_distri == "normal"){
        ability[i] <- rnorm(1, mean = c1_m, sd = sqrt(c1_v))
      } else if (c1_distri == "uniform"){
        # this part can be ignored because we don't use uniform distribution to generate abilities
        ability[i] <- runif(1, min = c1_min, max = c1_max)
      }
    } else if (class_idx[i] == 2) {
      # when the simulee belongs to class 2
      if(c2_distri == "normal"){
        ability[i] <- rnorm(1, mean = c2_m, sd = sqrt(c2_v))
      } else if (c2_distri == "uniform"){
        # this part can be ignored because we don't use uniform distribution to generate abilities
        ability[i] <- runif(1, min = c2_min, max = c2_max)
      }
    }}
  paste("Class one was sampled from a", c1_distri, "distribution, class two was sampled from a", c2_distri, "distribution.", sep = " ") %>% print()
  return(ability)
}

# combine person related vectors: class_idx, ability, person_id
# parameter 1: class_idx
# parameter 2: each simulee's ability
# parameter 3: N, number of simulees
get_person_df <- function(N = NA, class_idx = NULL, ability = NULL){
  data.frame(person_id = 1:N, class_idx = class_idx, ability = ability) %>%
    return()
}


### draw plots of simulees' abilities
# draw the density plot
get_plot_dens <- function(person_data = NULL, difficulty = NULL){
  diff <- data.frame(x = difficulty)
  p <- ggplot(data = person_data, aes(x = ability)) + geom_density() + geom_density(aes(group = class_idx, color = as.factor(class_idx)), linetype = "dashed") + geom_point(data = diff, aes(x = x, y = -0.03), size = 0.2, color = "brown") + labs(color = "Class") + ggtitle("Density Line of Mixture Simulees' Abilities and Item Difficulty Distribution") + theme(plot.title = element_text(size = 10))
  p
  ggsave("Simulee_ability_density_and_item_difficulty_distribution.pdf", dpi = 200,width = 6, height = 4, units = "in")
}


# combine item related vectors: item_id, difficulty1, discrimination1, difficulty2, discrimination2, 

get_item_df <- function(n = NULL, n_set = 1, diffi1 = NULL, discri1 = NULL, diffi2 = NULL, discri2 = NULL){
  if(n_set == 1){
    # only one set of difficulty and discrimination
    item_df <- data.frame(item_id = 1:n, difficulty = diffi1, discrimination = discri1) 
  } else if(n_set ==2){
    # two sets of difficulty and discrimination
    item_df <- data.frame(item_id = 1:n, difficulty1 = diffi1, discrimination1 = discri1, difficulty2 = diffi2, discrimination2 = discri2)
  }
  return(item_df)
}

# merge item data frame and person data frame
# no self-defined functions for this part
# example:
# df_long <- merge(df_item, df_person)

# calculate model scores and simulated score (adding randomness)
# parameter 1: prop: proportion
# parameter 2: a1: discrimination1
# parameter 3: b1: difficulty1
# parameter 4: a2: discrimination2
# parameter 5: b2: difficulty2
# parameter 6: theta: ability
get_model_score <- function(prop = NULL, a1 = NULL, b1 = NULL, a2 = NULL, b2 = NULL, theta = NULL){
  if (is.null(a2) & is.null(b2)){
    a2 <- a1
    b2 <- b1
  }
  z1 <- a1 * (theta - b1)
  z2 <- a2 * (theta - b2)
  p1 <- prop[1] * (exp(z1)/(1 + exp(z1)))
  p2 <- prop[2] * (exp(z2)/(1 + exp(z2)))
  return(p1 + p2)
}

### important
# don't draw a random sample in this script, because seed is not set, so every time it will get different results.


# select columns and convert the long format data frame into a wide format data frame
long2wide <- function(long_data){
  long_data %>%
    dplyr::select(item_id, person_id, class_idx, sim_score) %>%
    spread(item_id, sim_score) %>%
    return()
}

# save the wide format data as a data file for further analysis in Mplus
#the following code should be revised, to add prop in the file name
save_data <- function(data = NULL, path = getwd(), filename = ""){
  write.table(data, file = paste(path, "/", filename, sep = ""), row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  print(paste("Data saved as", filename, "at", path, sep = " "))
}
