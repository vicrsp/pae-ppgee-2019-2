
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
# Ncalc.normaldata <- calc_instances(ncomparisons = 2, power = power,
#                            d = d,
#                            sig.level = alpha,
#                            alternative = "two.sided",
#                            test = "t.test")
# 
# Ncalc.nonnormal <- calc_instances(ncomparisons = 2, power = power,
#                         d = d,
#                         sig.level = alpha,
#                         alternative = "two.sided",
#                         test = "wilcoxon")
# 
# Ncalc.normaldata$ninstances
# Ncalc.nonnormaldata$ninstances
# 
# N <- Ncalc.normaldata$ninstances

## Estimate the number of blocks
tau <- c(-d, d, rep(0, a - 2)) # define tau vector
b <- 5
while (qf(1 - alpha, a - 1, (a - 1)*(b - 1))
       >
       qf(beta, a - 1, (a - 1)*(b - 1), b*sum(tau^2)/a)){
  b <- b + 1
}

# RCBD functions ----------------------------------------------------
instances <- seq(2, 150) # number of instances
N <- 30 # number of replicates per instance

rcbd.configuration.generator <- function(level, b, instances, N){
  nrows <- length(instances) * N
  n.instances <- length(instances)
  instance <- sort(rep(instances, N))
  groups <- sapply(instance, function(i){ ceiling(i/(n.instances/b))  })
    
  X <- data.frame("algorithm" = rep(level, nrows), 
                  "replicate" = rep(seq(1,N), n.instances),
                  "instance" = instance,
                  "group" =  groups,
                  "result" = rep(-1, nrows))
  return(X)
}


x.config.1 <- rcbd.configuration.generator(1, b, instances, N)
x.config.2 <- rcbd.configuration.generator(2, b, instances, N)
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




