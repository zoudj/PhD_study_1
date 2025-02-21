

# Study 1: assuming measurement invariance

# import data generation script
# data generation functions
source("./util/util_data_generation.R")
# functions that generate models used in OpenBUGS
source("./util/util_mixture_model_MI.R")
# functions that generate initial values for Bayesian analyses
source("./util/util_initials_model_MI.R")
# purrr enhances R’s functional programming (FP) toolkit by providing a complete and consistent set of tools for working with functions and vectors. 
library(purrr)


###########################################################
### factors to be manipulated
## two-class proportion
# 3:1, 5:1
## model class
# 1-class(regular IRT model), 2-class(mixture IRT model)
## test difficulty range, covering the entire range of abilities of both classes, or only covering the Major class (C1)
# full, C1only
# sample size
# 600, 1200

# sample size specification
# sim: simulees
sim_n <- c(600L, 1200L)
# test length (number of items) specification
# suppose all items are dichotomous and in one dimension
item_n <- 50
# class proportion specification
prop_v <- c("3:1", "5:1")
# test difficulty range specification
# full: covering entire range of both classes; C1only: covering only Major Class
range_v <- c("full", "C1only")
# class ability distribution specification
# in this R script, two levels of class ability distribution is descraibed as separated and merged
# separated: Major Class: N(0,1); Minor Class: N(2.5, 0.5)
# merged: Major Class: N(0,1); Minor Class: N(1, 0.5)
mean_dist <- c("separated", "merged")

# setting number of latent classes
C <- 1:2
# number of Markov chains for each model in each replication
N_chain <- 3
# number of replications for each experimental condition
N_rep <- 50
#############################################################
# aggregating  experimental conditions, i.e., all combinations of the four experimental factors, 2*2*2*2 = 16.
conditions <- vector()
# cd: abbr. of condition
cd <- 1
for(i in seq_along(sim_n)){
  for(j in seq_along(prop_v)){
    for(m in seq_along(mean_dist)){
      for(n in seq_along(range_v)){
        prop <- paste0(substr(prop_v[j],1,1), 'to', substr(prop_v[j],3,3))
        conditions <- append(conditions, paste('Mixture', cd, sim_n[i], prop, mean_dist[m], range_v[n], sep = '_'))
        cd <- cd + 1
      }
    }
  }
}

# experiment conditions and levels in each condition
conditions_split <- strsplit(conditions, split = '_', fixed = T)
# creating a data frame to store condition names and their level details
df_conditions <- data.frame(Name = conditions) %>% 
  mutate(S_size = map_chr(conditions_split, ~{.x[3]}),
         Proportion = map_chr(conditions_split, ~{.x[4]}),
         M_distance = map_chr(conditions_split, ~{.x[5]}),
         Coverage = map_chr(conditions_split, ~{.x[6]}))

# combinations of sample size and class proportion
# 4 levels
cd_sspp <- df_conditions %>% 
  select(S_size, Proportion) %>%  
  distinct(S_size, Proportion, .keep_all = TRUE)
# combinations of sample size, class proportion and mean distance (class ability distribution)
# 8 levels
cd_ssppmd <- df_conditions %>% 
  select(S_size, Proportion, M_distance) %>%  
  distinct(S_size, Proportion, M_distance, .keep_all = TRUE)
# combinations of mean distance (class ability distribution) and item difficulty coverage (test difficulty range)
cd_md_cover <- df_conditions %>% 
  select(M_distance, Coverage) %>%  
  distinct(M_distance, Coverage, .keep_all = TRUE)
#############################################################
# sampling random seeds, 5 digits
# random seeds will be used where random values are required
# seed_vector_xxxx <- sample(10000:99999, 50000)
# write.csv(seed_vector_xxxx, "seed_vector_xxxx_2023Mar11.csv", row.names = F)
seed_vector <- read.csv("seed_vector_xxxx_2023Mar11.csv") %>%
  `$`(x)
# seed index, starting from 1
s <- 1L
set.seed(seed_vector[s])
s <- s + 1


# generating a latent class index (class membership) for each simulee
# using sample size and class proportion to generate class indexes
gen_class_idx <- function(sspp = NULL){
  # sample size
  ss <- sspp$S_size %>% 
    as.integer()
  # class proportion
  pp <- sspp$Proportion %>% 
    map(get_prop)
  # generate class index
  # get_class_idx() is a self_defined function from util_data_generation.R
  class_idx <- map2(ss, pp, ~{get_class_idx(.x, .y)})
  # save data
  pwalk(list(class_idx, sspp$S_size, sspp$Proportion, 1:length(class_idx)), ~{write.csv(..1, file = paste('class_idx',..4, ..2, ..3, '.csv', sep = '_'), row.names = F)})
  return(class_idx)
}

# save true class indices
true_class_idx <- gen_class_idx(sspp = cd_sspp)
# check true class indices
map(true_class_idx, table)


# generating person abilities based on sample size, class proportion (class proportion is reflected in class indices), and mean distance (class ability distribution)
gen_true_ab <- function(ssppmd = NULL, class_idx = NULL){
  if( nrow(ssppmd) != length(class_idx)){
    warning('Length of true class idx list should be equal to rows of ssppmd.')
    break
  }
  # sample size
  ss <- ssppmd$S_size %>% 
    as.integer()
  # class proportion
  pp <- ssppmd$Proportion %>% 
    map(get_prop)
  # mean distance types
  md <- ssppmd$M_distance
  
  # generate true ability sets
  ability_list <- list()
  for(j in 1:nrow(ssppmd)){
    if(md[j] == 'separated'){
      repeat{
        # class ability distributions are specified here
        temp_ab  <- get_ability(N = ss[j], class_idx = class_idx[[j]], c1_m = 0, c1_v = 1, c2_m = 2.5, c2_v = 0.25)
        temp_ab_c1 <- temp_ab[class_idx[[j]] == 1]
        temp_ab_c2 <- temp_ab[class_idx[[j]] == 2]
        temp_c1_range <- range(temp_ab_c1)
        temp_c2_range <- range(temp_ab_c2)
        # make sure sampled abilities in the 3 SD range, for class 1, -3 ~ +3; for class 2, 1 ~ 4
        if(temp_c1_range[1] >= -3 & temp_c1_range[2] <= 3 & temp_c2_range[1] >= 1 &  temp_c2_range[2] <= 4){
          ability_list[[j]] <- temp_ab
          break
        }
      }
      
    } else if (md[j] == 'merged'){
      repeat{
        # class ability distributions are specified here
        temp_ab <- get_ability(N = ss[j], class_idx = class_idx[[j]], c1_m = 0, c1_v = 1, c2_m = 1, c2_v = 0.25)
        temp_ab_c1 <- temp_ab[class_idx[[j]] == 1]
        temp_ab_c2 <- temp_ab[class_idx[[j]] == 2]
        temp_c1_range <- range(temp_ab_c1)
        temp_c2_range <- range(temp_ab_c2)
        # make sure sampled abilities in the 3 SD range, for class 1, -3 ~ +3; for class 2, -0.5 ~ 2.5
        if(temp_c1_range[1] >= -3 & temp_c1_range[2] <= 3 & temp_c2_range[1] >= -0.5 &  temp_c2_range[2] <= 2.5){
          ability_list[[j]] <- temp_ab
          break
        }
      }
      
    } else {
      print('The specification of mean distance is wrong.')
    }
  }
  # save data
  pwalk(list(ability_list, ssppmd$S_size, ssppmd$Proportion, ssppmd$M_distance, 1:length(ability_list)), ~{write.csv(..1, file = paste('ability',..5, ..2, ..3, ..4, '.csv', sep = '_'), row.names = F)})
  return(ability_list)
}

# using the above function to generate simulees' abilities
true_ability <- gen_true_ab(cd_ssppmd, class_idx = rep(true_class_idx, each =2))

# checking true ability range
true_ab_range <- map(true_ability, range)
true_ab_range
# check maximum ability in class 1
map2_dbl(true_ability, rep(true_class_idx, each =2), ~{
  .x[.y == 1] %>%
    max()})
# check empirical ability SD
map_dbl(true_ability, sd)
map_dbl(true_ability, mean)
map_dbl(true_ability, median)


# generating item difficulties constrained by true ability range
gen_item_diff <- function(cd = NULL, ab = NULL, c_idx = NULL, n_item = 50){
  diff_list <- list()
  for(j in 1:nrow(cd)){
    md <- cd$M_distance[j]
    cover <- cd$Coverage[j]
    ab_min <- min(ab[[j]])
    ab_max <- max(ab[[j]])
    ab_mean <- mean(ab[[j]])
    ab_sd <- sd(ab[[j]])
    ab_c1_max <- ab[[j]][c_idx[[j]] == 1] %>% max()
    
    if(cover == "full"){
      repeat{
        temp_diff <- rnorm(n = n_item, mean = ab_mean, sd = ab_sd)
        if(min(temp_diff) < ab_min & min(temp_diff) > (ab_min - 0.1) & max(temp_diff) > ab_max & max(temp_diff) < (ab_max + 0.1)){
          diff_list[[j]] <- temp_diff
          break
        } 
      }
    } else if(cover == "C1only"){
      repeat{
        temp_diff <- rnorm(n = n_item, mean = 0, sd = 1.1)
        if(min(temp_diff) < ab_min & min(temp_diff) > (ab_min - 0.1) & max(temp_diff) > ab_c1_max & max(temp_diff) < (ab_c1_max + 0.1)){
          diff_list[[j]] <- temp_diff
          break
        } 
      }
    } 
  }
  return(diff_list)
}
# generate item difficulties
item_diff <- gen_item_diff(cd = df_conditions, ab = rep(true_ability, each =2), c_idx = rep(true_class_idx, each =4))

# checking item difficulty range
map(item_diff, range)

# generate item discriminations
item_disc <- list()
for(j in 1:nrow(df_conditions)){
  # using a log-normal distribution
  item_disc[[j]] <- rlnorm(n = 50, meanlog = 0.15, sdlog = 0.2)
}
# checking item discrimination range
map(item_disc, range)


########################################################
# create a new condition folder
# map(df_conditions$Name, dir.create)

# create condition folders and copy files
for(j in 1:nrow(df_conditions)){
  dir.create(df_conditions$Name[j])
  setwd(df_conditions$Name[j])
  # true ability
  write.csv(rep(true_ability, each =2)[[j]], 'true_ability.csv', row.names = F)
  # true class index
  write.csv(rep(true_class_idx, each =4)[[j]], 'true_class_idx.csv', row.names = F)
  # item difficulty
  write.csv(item_diff[[j]], 'item_diff.csv', row.names = F)
  # item discrimination
  write.csv(item_disc[[j]], 'item_disc.csv', row.names = F)
  setwd('../')
}

# a utility function
copy_data <- function(destination = dirname(getwd()), from_folder = NULL, N_chain = 3){
  # create a new directory and save data, model and itnitials
  current_folder <- strsplit(getwd(), split = '/', fixed = T)%>% 
    unlist() %>% 
    `[`(length(.))
  bugs_files <- list.files(from_folder)
  data_file <- bugs_files[startsWith(bugs_files, 'data')]
  model_file <- bugs_files[startsWith(bugs_files, 'model')]
  for(n in 1:N_chain){
    new_folder <- paste0(destination, '/', current_folder, '_', from_folder, '_chain', n)
    dir.create(new_folder)
    setwd(from_folder)
    file.copy(c(data_file, model_file, paste('inits', n, '.txt', sep = '')), new_folder)
    setwd('../')
  }
}

# a utility function, for generating response data
rep_data_gen <- function(myseed = NULL, N_sim = NULL, N_item = NULL, myprop = NULL, mean_dist = NULL, N_chain = 3, rep_idx = NULL){
  # set random seed
  set.seed(myseed[s])
  root_folder <- getwd()
  # creat a new seed folder
  folder_temp <- paste0('seed', myseed[s])
  s <<- s + 1
  dir.create(folder_temp)
  setwd(folder_temp)
  # creat two new sub-folders
  dir.create("BUGS_2C_correct")
  dir.create("BUGS_1C_wrong")
  
  # get sub-population proportion
  prop <- get_prop(myprop)
  # get class index
  class_idx <- read.csv('../true_class_idx.csv') %>%
    `$`(x)
  # get individual ability
  ability <- read.csv(paste0('../true_ability.csv')) %>%
    `$`(x)
  # get item difficulty
  difficulty <- read.csv('../item_diff.csv') %>%
    `$`(x)
  # get item discrimination
  discrimination <- read.csv('../item_disc.csv') %>%
    `$`(x)
  # generate response data
  df_person <- get_person_df(N_sim, class_idx, ability)
  df_item <- get_item_df(N_item, 1, difficulty, discrimination)
  df_long <- merge(df_item, df_person) %>%
    mutate(model_score = get_model_score(prop = prop, a1 = discrimination, b1 = difficulty, theta = ability)) 
  # convert probability to response scores, i.e., 0 or 1
  sim_score <- vector()
  temp_p <- runif(n = nrow(df_long), min = 0, max = 1)
  for(q in 1:nrow(df_long)){
    if(df_long$model_score[q] >= temp_p[q]){
      sim_score[q] <- 1
    } else {
      sim_score[q] <- 0
    }
  }
  df_long$sim_score <- sim_score
  write.csv(df_long$model_score, 'model_score.csv', row.names = F)
  write.csv(df_long$sim_score, 'simulated_score.csv', row.names = F)
  
  ### convert long data format to wide data format
  df_wide <- long2wide(df_long)
  df_res <- df_wide[, 3:ncol(df_wide)] %>%
    as.matrix()
  # coverting response data to data files used in OpenBUGS
  # for C = 1, 1-class model
  data_BUGS_1c <- list("NS" = N_sim, "NI" = N_item, "resp" = df_res)
  bugs.data(data_BUGS_1c, dir = "./BUGS_1C_wrong/", digits = 5, data.file = "data_bugs_1c_wr.txt")
  # for C = 2, 2-class model
  data_BUGS_2c <- list("NS" = N_sim, "NI" = N_item, "C" = 2, "prop" = prop, "resp" = df_res)
  bugs.data(data_BUGS_2c, dir = "./BUGS_2C_correct/", digits = 5, data.file = "data_bugs_2c_co.txt")
  
  # generate initial values and models used in OpenBUGS
  # one class 2PL wrong model
  inits <- gen_inits(NI = N_item, NS = N_sim, C = 1, mod = "2PL", nchains = N_chain, RNG = RNG, seed = myseed[s : (s + N_chain - 1)])
  s <<- s + 3
  inits_c1_bugs <- inits[[2]]
  bugs.inits(inits_c1_bugs, n.chains = N_chain, digits = 5, inits.files = paste("./BUGS_1C_wrong/inits", 1:N_chain, ".txt", sep = ""))
  write.model(model_1C_2PL_BUGS, "./BUGS_1C_wrong/model_1c_2PL_wr.txt")
  copy_data(from_folder = 'BUGS_1C_wrong')
    
  # two class 2PL correct model
  inits <- gen_inits(NI = N_item, NS = N_sim, C = 2, mod = "2PL", nchains = N_chain, RNG = RNG, seed = myseed[s : (s + N_chain - 1)])
  s <<- s + 3
  inits_c2_bugs <- inits[[2]]
  bugs.inits(inits_c2_bugs, n.chains = N_chain, digits = 5, inits.files = paste("./BUGS_2C_correct/inits", 1:N_chain, ".txt", sep = ""))
  if(mean_dist == 'separated'){
    write.model(model_2C_2PL_BUGS_separated, "./BUGS_2C_correct/model_2c_2PL_co_separated.txt")
  } else if(mean_dist == 'merged'){
    write.model(model_2C_2PL_BUGS_merged, "./BUGS_2C_correct/model_2c_2PL_co_merged.txt")
  } else{
    warning('Something is wrong with your 2-class model.')
  }
  
  copy_data(from_folder = 'BUGS_2C_correct')
  
  setwd("../")
}


# finding generated directories that will store data, models, initial values for use in OpenBUGS
dirs <- list.dirs(full.names = F, recursive = F)
target_idx <- grepl('to', dirs, fixed = TRUE)
dirs_target <- dirs[target_idx]

######################################################

# generating and copying data to the taget directories
# 50 replications in each experimental condition
for(d in seq_along(dirs_target)){
  dir_str <- strsplit(dirs_target[d], split = '_', fixed = T)%>% unlist()
  sim <- as.integer(dir_str[3])  
  prop <- dir_str[4]
  m_dist <- dir_str[5]
  setwd(dirs_target[d])
  for(r in 1:N_rep){
    rep_data_gen(myseed = seed_vector, N_sim = sim, N_item = item_n, myprop = prop, mean_dist = m_dist, N_chain = 3, rep_idx = r)
  }
  setwd('../')
}
