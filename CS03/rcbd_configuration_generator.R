
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


# RCBD functions ----------------------------------------------------
b <- 5 # number of elements per block
a <- 2 # number of levels
instances <- seq(2,150) # number of instances
N <- 5 # number of replicates per instance

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




