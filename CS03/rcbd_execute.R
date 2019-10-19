# Load packages -----------------------------------------------------------
if (!require(ExpDE, quietly = TRUE)){
  install.packages("ExpDE")
}


if (!require(smoof, quietly = TRUE)){
  install.packages("smoof")
}

# Execute a RCBD test configuration ------------------------------------------
Z <- read.csv('rcbd.config.victor.csv')
set.seed(15632) # set a random seed

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
  
  return(list(value = out$Fbest))
}

for (row in 1:nrow(Z)){
  
  if(Z[row, "result"] == 0){ # start from the last execution
    dim <- Z[row, "instance"]
    algo <- Z[row, "algorithm"]
    replicate <- Z[row, "replicate"]
    
    if(algo == 1)
      algo.config <- config.1
    else
      algo.config <- config.2
    
    fn <- function(X){
      if(!is.matrix(X)) X <- matrix(X, nrow = 1) # <- if a single vector is passed as Z
      
      Y <- apply(X, MARGIN = 1, FUN = smoof::makeRosenbrockFunction(dimensions = dim))
      return(Y)
    }
    
    instance <- list(FUN = "fn")
    out <- my.ExpDE(algo.config$mutparsX, algo.config$recparsX, dim, instance)
    
    Z[row, "result"] <- out$value
    print(paste("Finished. Instance:", dim, "; Algo:", algo, "; Repetition:", replicate, "; Result=", out$value))
  }
}
