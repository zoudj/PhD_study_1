# created by DZ, Mar. 25, 2022
# this file stores mixture models I use in the study

#########################################################

#########################################################
### 1-class 2PL model

# FOR openBUGS
model_1C_2PL_BUGS <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  t_mu ~ dnorm(0.0, 4.0)
  t_sd ~ dnorm(1.0, 1.0E+2)%_%T(0,)
  t_tau <- 1 / (t_sd * t_sd)
  # simulee's ability
  for (s in 1:NS) {
    theta[s] ~ dnorm(t_mu, t_tau)
  }
  theta_mean <- mean(theta[1:NS])
  # item parameters
  # a: discrimination; b: difficulty
  # in this case a in c1 = a in c2, b in c1 = b in c2 correspondingly
  for(i in 1:NI){
    # discrimination parameter a
    # set equal a in the two classes for each item
    # apply item centering constraint
    # b parameter true value: mu == 0, sd == 2
    bb[i] ~ dnorm(0.0, 0.25)
    b[i] <- bb[i] - mean(bb[1:NI])
    # log-normal distribution 
    a[i] ~ dlnorm(0.15, 25)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:NI){
      # Rasch model, a == 1
      logit(p[s,i]) <- a[i] * (theta[s] - b[i])
      r[s,i] ~ dbern(p[s,i])
      l[s,i] <- log(p[s,i]) * r[s,i] + log(1 - p[s,i]) * (1 - r[s,i])
    }
  }
}
#########################################################
### 2-class 2PL model

# FOR openBUGS
model_2C_2PL_BUGS_separated <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  # class proportion
  pi[1:C] ~ ddirich(prop[])
  # ability parameters
  # theta mu for C classes
  # standard deviation of theta for C classess
  # T(0,) truncate the positive half
  # %_% is the dummy operator to prevent from throwing an error
  # calculate tau (precision parameter) from sd
  t_mu[1] ~ dnorm(0.0, 1.0E+2)
  t_sd[1] ~ dnorm(1.0, 1.0E+2)%_%T(0,)
  t_tau[1] <- 1 / (t_sd[1] * t_sd[1])
  t_mu[2] ~ dnorm(2.5, 1.0E+2)
  t_sd[2] ~ dnorm(0.5, 1.0E+2)%_%T(0,)
  t_tau[2] <- 1 / (t_sd[2] * t_sd[2])
  # simulee's class membership and ability
  for (s in 1:NS) {
    c_mem[s] ~ dcat(pi[1:C])
    theta[s] ~ dnorm(t_mu[c_mem[s]], t_tau[c_mem[s]])
  }
  theta_mean <- mean(theta[1:NS])
  # item parameters
  # a: discrimination; b: difficulty
  # in this case a in c1 = a in c2, b in c1 = b in c2 correspondingly
  for(i in 1:NI){
    # discrimination parameter a
    # set equal a in the two classes for each item
    # apply item centering constraint
    # b parameter true value
    bb[i] ~ dnorm(0.0, 0.25)
    b[i] <- bb[i] - mean(bb[1:NI])
    # log-normal distribution 
    a[i] ~ dlnorm(0.15, 25)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:NI){
      # Rasch model, a == 1
      logit(p[s,i]) <- a[i] * (theta[s] - b[i])
      r[s,i] ~ dbern(p[s,i])
      l[s,i] <- log(p[s,i]) * r[s,i] + log(1 - p[s,i]) * (1 - r[s,i])
    }
  }
}

model_2C_2PL_BUGS_merged <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  # class proportion
  pi[1:C] ~ ddirich(prop[])
  # ability parameters
  # theta mu for C classes
  # standard deviation of theta for C classess
  # T(0,) truncate the positive half
  # %_% is the dummy operator to prevent from throwing an error
  # calculate tau (precision parameter) from sd
  t_mu[1] ~ dnorm(0.0, 1.0E+2)
  t_sd[1] ~ dnorm(1.0, 1.0E+2)%_%T(0,)
  t_tau[1] <- 1 / (t_sd[1] * t_sd[1])
  t_mu[2] ~ dnorm(1.0, 1.0E+2)
  t_sd[2] ~ dnorm(0.5, 1.0E+2)%_%T(0,)
  t_tau[2] <- 1 / (t_sd[2] * t_sd[2])
  # simulee's class membership and ability
  for (s in 1:NS) {
    c_mem[s] ~ dcat(pi[1:C])
    theta[s] ~ dnorm(t_mu[c_mem[s]], t_tau[c_mem[s]])
  }
  theta_mean <- mean(theta[1:NS])
  # item parameters
  # a: discrimination; b: difficulty
  # in this case a in c1 = a in c2, b in c1 = b in c2 correspondingly
  for(i in 1:NI){
    # discrimination parameter a
    # set equal a in the two classes for each item
    # apply item centering constraint
    # b parameter true value: mu == 0, sd == 2
    bb[i] ~ dnorm(0.0, 0.25)
    b[i] <- bb[i] - mean(bb[1:NI])
    # log-normal distribution 
    a[i] ~ dlnorm(0.15, 25)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:NI){
      # Rasch model, a == 1
      logit(p[s,i]) <- a[i] * (theta[s] - b[i])
      r[s,i] ~ dbern(p[s,i])
      l[s,i] <- log(p[s,i]) * r[s,i] + log(1 - p[s,i]) * (1 - r[s,i])
    }
  }
}