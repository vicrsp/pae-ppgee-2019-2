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

if (!require(dplyr, quietly = TRUE)){
  install.packages("dplyr")
}

if (!require(ggplot2, quietly = TRUE)){
  install.packages("ggplot2")
}

if (!require(ggridges, quietly = TRUE)){
  install.packages("ggridges")
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


# Execute a RCBD test configuration ------------------------------------------
my.ExpDE <- function(mutp, recp, dim, instance){
  
  #fname <- paste0('instances.list[[', dim, ']]$FUN')
  fn.current <- function(X){
    if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as Z
    
    Y <- apply(X, MARGIN = 1, FUN = smoof::makeRosenbrockFunction(dimensions = dim))
    return(Y)
  }
  
  
  assign("fn", fn.current, envir = .GlobalEnv)
  
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

instances.list <- lapply(2:150, function(i){ 
  list(FUN = 'fn', alias = paste0("Inst.", i))
})


algorithm1 <- list(FUN = "my.ExpDE", alias = "algo1", mutp = mutpars1, recp = recpars1, dim = dim)
algorithm2 <- list(FUN = "my.ExpDE", alias = "algo2", mutp = mutpars2, recp = recpars2, dim = dim)
algorithms <- list(alg1 = algorithm1, alg2 = algorithm2)

if(!file.exists('experiment_data/CAISEr_results_20191020142945.rds')){
  my.results <- run_experiment(instances.list, algorithms,
                               d = d, se.max = .1,
                               power = power, sig.level = alpha,
                               alternative = "two.sided",
                               test = "wilcoxon", method = "param",
                               nstart = 20, nmax = 60,
                               power.target = "mean",
                               dif = "perc", comparisons = "all.vs.all",
                               ncpus = 1, seed = 1234,
                               save.partial.results = 'experiment_data/', 
                               load.partial.results= 'experiment_data/', 
                               save.final.result = 'experiment_data/')

} else {
  my.results <- readRDS(file = 'experiment_data/CAISEr_results_20191020142945.rds', refhook = NULL)
}

summary(my.results)
plot(my.results)

write.table(x = my.results$data.raw, file='experiment_raw_results.txt')
