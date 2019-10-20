
# Load packages -----------------------------------------------------------
if (!require(ExpDE, quietly = TRUE)){
  install.packages("ExpDE")
}


if (!require(smoof, quietly = TRUE)){
  install.packages("smoof")
}


if (!require(CAISEr, quietly = TRUE)){
  install.packages("CAISEr")
}


# Constants ---------------------------------------------------------------
## Equipe D
## Config 1
recpars1 <- list(name = "recombination_blxAlphaBeta", alpha = 0.4, beta = 0.4)
mutpars1 <- list(name = "mutation_rand", f = 4)

## Config 2
recpars2 <- list(name = "recombination_eigen", othername = "recombination_bin", cr = 0.9)
mutpars2 <- list(name = "mutation_best", f = 2.8)

## Normalized effect size
d <- 0.5
## Significance
alpha <- 0.05
## Power
power <- 0.8
beta <- 1 - power

set.seed(15632) # set a random seed

# Parameter estimation functions ----------------------------------------------------
## Estimate number of instances
Ncalc.normaldata <- calc_instances(ncomparisons = 2, power = power,
                           d = d,
                           sig.level = alpha,
                           alternative = "two.sided",
                           test = "t.test")

Ncalc.nonnormal <- calc_instances(ncomparisons = 2, power = power,
                        d = d,
                        sig.level = alpha,
                        alternative = "two.sided",
                        test = "wilcoxon")

Ncalc.normaldata$ninstances
Ncalc.nonnormaldata$ninstances

N <- Ncalc.normaldata$ninstances

## Estimate the number of blocks
a <- 2 # number of levels
tau <- c(-d, d, rep(0, a - 2)) # define tau vector
b <- 5
while (qf(1 - alpha, a - 1, (a - 1)*(b - 1)) 
       >
       qf(beta, a - 1, (a - 1)*(b - 1), b*sum(tau^2)/a)){
  b <- b + 1
}

## Estimate number of repetitions per instance
my.ExpDE <- function(mutp, recp, dim, instance){
  
  selpars <- list(name = "selection_standard")
  stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
  probpars <- list(name = instance$FUN, xmin = rep(-5, dim), xmax = rep(10, dim))
  popsize = 5 * dim
  
  out <- ExpDE(mutpars = mutp,
               recpars = recp,
               popsize = popsize,
               selpars = selpars,
               stopcrit = stopcrit,
               probpars = probpars,
               showpars = list(show.iters = "none"))
  
  print(out$Fbest)
  return(list(value = out$Fbest))
}

instances <- ceiling(seq(2, 150, length.out = N))
instances.repetitions.calc <- data.frame("instance" = instances, 
                                         "N.config.1" = rep(0,N),
                                         "N.config.2" = rep(0,N)) 

for (row in 1:nrow(instances.repetitions.calc)){
  
  dim <- instances.repetitions.calc[row, "instance"]
  print(paste("Processing Instance:", dim))
  
  # define a rosenbrock function for a given dimension
  fn <- function(X){
    if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as Z
    
    Y <- apply(X, MARGIN = 1, FUN = smoof::makeRosenbrockFunction(dimensions = dim))
    return(Y)
  }
  
  instance <- list(FUN = "fn")
  
  algorithm1 <- list(FUN = "my.ExpDE", alias = "algo1", mutp = mutpars1, recp = recpars1, dim = dim, instance = instance)
  algorithm2 <- list(FUN = "my.ExpDE", alias = "algo2", mutp = mutpars2, recp = recpars2, dim = dim, instance = instance)
  algorithms <- list(alg1 = algorithm1, alg2 = algorithm2)
  
  myreps <- calc_nreps(instance   = instance,
                       algorithms = algorithms,
                       se.max     = 1,          # desired (max) standard error
                       dif        = "perc",        # type of difference
                       comparisons = "all.vs.all", # differences to consider
                       method     = "param",       # method ("param", "boot")
                       nstart     = 30,            # initial number of samples
                       nmax       = 60,          # maximum allowed sample size
                       seed       = 15632,          # seed for PRNG
                       boot.R     = 499,           # number of bootstrap resamples (unused)
                       ncpus      = 1,             # number of cores to use
                       force.balanced = FALSE,     # force balanced sampling?
                       load.folder   = NA,         # file to load results from
                       save.folder = NA)         # folder to save results
  
  instances.repetitions.calc[row, "N.config.1"] <-  myreps$Nk[1]
  instances.repetitions.calc[row, "N.config.2"] <-  myreps$Nk[2]
  
  print(paste("Finished. Instance:", dim))
  print(myreps$Nk)
  
}


# RCBD functions ----------------------------------------------------
instances <- ceiling(seq(2, 150, length.out = Ncalc.nonnormal$ninstances)) # number of instances
N <- 30 # number of replicates per instance

# Define a class to store the levels configuration
level.config <- function(mp, rp, id) {
  value <- list(mutparsX = mp, recparsX = rp, id = id)
  class(value) <- append(class(value),"level.config")
  return(value)
}

config.1 <- level.config(mutpars1, recpars1, 1)
config.2 <- level.config(mutpars2, recpars2, 2)

#config.list <- c(config1, config2)

rcbd.configuration.generator <- function(level, b, instances, N){
  nrows <- length(instances) * N
  n.instances <- length(instances)
  instance <- sort(rep(instances, N))
  groups <- ceiling(instance / b)
    
  X <- data.frame("algorithm" = rep(level$id, nrows), 
                  "replicate" = rep(seq(1,N), n.instances),
                  "instance" = instance,
                  "group" =  groups,
                  "result" = rep(-1, nrows))
  
  return(X)
  
}


x.config.1 <- rcbd.data.generator(config.1, b, instances, N)
x.config.2 <- rcbd.data.generator(config.2, b, instances, N)
x.config.all <- rbind(x.config.1, x.config.2)
x.config.all.shuffled <- x.config.all[sample(nrow(x.config.all)), ]

split.size <- (nrow(x.config.all.shuffled)/3)
x.config.all.shuffled$member <- ceiling((1:nrow(x.config.all.shuffled))/split.size)

x.config.all.shuffled.victor <- x.config.all.shuffled[x.config.all.shuffled$member == 1,1:5]
x.config.all.shuffled.gilmar <- x.config.all.shuffled[x.config.all.shuffled$member == 2,1:5]
x.config.all.shuffled.maressa <- x.config.all.shuffled[x.config.all.shuffled$member == 3,1:5]

write.csv(x.config.all.shuffled.victor, 'rcbd.config.victor.csv', row.names=FALSE)
write.csv(x.config.all.shuffled.gilmar, 'rcbd.config.gilmar.csv', row.names=FALSE)
write.csv(x.config.all.shuffled.maressa, 'rcbd.config.maressa.csv', row.names=FALSE)




