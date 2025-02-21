# created by DZ, Mar. 25, 2022
# this file stores functions that generate initial values for Bayesian analyses

### for 2PL models
# 1-class
# 2-class
library(R2OpenBUGS)

# specify random number generators
# three different random number generator (RNG) algorithms from R's base package
RNG <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", "base::Super-Duper")

# initial proportion values for the two classes
pi_list <- list(c(0.7,0.3), c(0.5, 0.5), c(0.3, 0.7))

gen_inits <- function(NI = NULL, NS = NULL, C = NULL, mod = "2PL", nchains = 3, RNG = NULL, seed = NULL) {
  if(length(RNG) != nchains){
    print("The length of RNG should be the same as the number of chains.")
    break
  }
  if(length(seed) != nchains){
    print("The length of seed should be the same as the number of chains.")
    break
  }
  print(paste("Use the following RNG: ", RNG, sep = ""))
  print(paste("Use the following seeds: ", seed, sep = ""))
  inits <- list()
  if (mod == "2PL" | mod == "2pl"){
    if(C == 1){
      for(i in 1:nchains){
        # initial values for the use in OpenBUGS, correesponding to parameters in the IRT models
        inits[[i]] <- list(bb = rnorm(NI, 0, 1), a = rlnorm(NI, 0.15, 0.2), t_mu = rnorm(1, 0, 1), t_sd = abs(rnorm(1, 0, 1)), theta = runif(NS, -4, 4), .RNG.name = RNG[i], .RNG.seed = seed[i])
      }
    }else if(C == 2){
      for (i in 1:nchains){
        # initial values for the use in OpenBUGS, correesponding to parameters in the IRT models
        inits[[i]] <- list(bb = rnorm(NI, 0, 1), a = rlnorm(NI, 0.15, 0.2), t_mu = rnorm(C, 0, 1), t_sd = abs(rnorm(C, 0, 1)), pi = pi_list[[i]], c_mem = rcat(NS, prob = c(0.5, 0.5)), theta = runif(NS, -4, 4), .RNG.name = RNG[i], .RNG.seed = seed[i])
      }
    }
  } else{
    print("Something is worng with the model type.")
    break
  }
  # initial values for JAGS
  # ignore this part because we don't use JAGS
  inits_jags <- list()
  for(i in seq_along(inits)){
    inits_jags[[i]] <- dump.format(inits[[i]])
  }
  # initial values for OpenBUGS
  inits_bugs <- list()
  for(i in seq_along(inits)){
    inits_bugs[[i]] <- inits[[i]][1:(length(inits[[i]])-2)]
  }
  return(list(inits_jags, inits_bugs))
  
}

