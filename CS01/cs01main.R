#Maressa Nunes
if (!require(ExpDE, quietly = TRUE)){
  install.packages("ExpDE")
}

if (!require(ggplot2, quietly = TRUE)){
    install.packages("ggplot2")
}

if (!require(car, quietly = TRUE)){
  install.packages("car")
}

if (!require(boot, quietly = TRUE)){
  install.packages("boot")
}

# define current constants
current_mu <- 50
current_var <- 100

# Set-up the data generating procedure
mre <- list(name = "recombination_bin", cr = 0.9)
mmu <- list(name = "mutation_rand", f = 2)
mpo <- 100
mse <- list(name = "selection_standard")
mst <- list(names = "stop_maxeval", maxevals = 10000)
mpr <- list(name = "sphere", xmin = -seq(1, 20), xmax = 20 + 5 * seq(5, 24))

set.seed(1234) # to generate always the same results

# define functions for data generation
get.single.observation <- function(mpo, mmu, mre, mse, mst, mpr){
  generator <- ExpDE(mpo, mmu, mre, mse, mst, mpr, showpars = list(show.iters = "none"))
  return(generator$Fbest)
}

get.n.samples <- function(mpo, mmu, mre, mse, mst, mpr, N){
  my.sample <- numeric(N)
  for (i in seq(N)){
    my.sample[i] <- get.single.observation(mpo, mmu, mre, mse, mst, mpr)
  }
  return(my.sample)
}

#print(get.single.observation(mpo, mmu, mre, mse, mst, mpr))
#print(get.n.samples(mpo, mmu, mre, mse, mst, mpr, 10))

# 1: mean cost test analysis
## define mean cost test constants
sig_level_mean <- 0.01
delta <- 4
beta <- 0.2
pi <- 1 - beta
ci_mean <- 1 - sig_level_mean

## 1.1 null hypothesis H0: the mean execution cost is smaller than the curent value (50)
## 1.2 calculate sample size
(sample_size_calc <- power.t.test(delta = delta,
                           sd = sqrt(current_var),
                           sig.level = sig_level_mean,
                           power = pi,
                           alternative = "one.sided",
                           type = "one.sample"))

N <- ceiling(sample_size_calc$n)
print(c("Calculated number of samples: ", N), quote = FALSE)
## 1.3 hypothesis testing
## Generate the data based on the previous calculations
data_mean_test <- get.n.samples(mpo, mmu, mre, mse, mst, mpr, N)

## Calculate some statistics
summary(data_mean_test)
var(data_mean_test)

# Plot a boxplot (basic)
boxplot(data_mean_test, horizontal = TRUE)
# Plot a boxplot (ggplot2)
p1 <- ggplot(as.data.frame(data_mean_test), aes(y = data_mean_test))
p1 + geom_boxplot() + coord_flip() + theme(axis.text.y = element_blank())

# Plot a histogram
hist(data_mean_test, las = 1, breaks = 10)
# plot a histogram (ggplot2)
p2 <- ggplot(as.data.frame(data_mean_test), aes(x = data_mean_test))
p2 + geom_histogram(bins = 10)

# Plot a Normal qq-plot (basic)
qqnorm(data_mean_test, las = 1)
qqline(data_mean_test)
# Plot a Normal qq-plot (a little better)
qqPlot(data_mean_test, las = 1, pch = 16)

# Plot a Normal qq-plot (ggplot2)
p3 <- ggplot(as.data.frame(data_mean_test), aes(sample = data_mean_test))
p3 + geom_qq() + geom_qq_line()

# Hipothesis testing
mean_test_hyp <- t.test(data_mean_test, mu = current_mu,
                  alternative = "less",
                  conf.level = ci_mean)

## 1.4 calculate confidence interval of the mean
ci_mean_test_hyp <- t.test(data_mean_test, mu = current_mu, conf.level = ci_mean)$conf.int
print(ci_mean_test_hyp)

## 1.5 Assumptions validation
K <- 999
boot.means <- numeric(K)
for (i in seq(K)){
  boot.sample <- sample(data_mean_test, replace = TRUE) # sample with replacement
  boot.means[i] <- mean(boot.sample)
}

qqnorm(boot.means, las = 1, pch = 16)
qqline(boot.means)

## 1.6 Test power discussion
(CI_u <- (N - 1) * var(data_mean_test) / qchisq(p = sig_level_mean, df = N - 1))
(my.pwr <- power.t.test(n = N,
                        delta = delta,
                        sd = sqrt(CI_u),
                        sig.level = sig_level_mean,
                        type = "one.sample",
                        alternative = "one.sided"))

# 2: variance test analysis
# define variance test constants
sig_level_sd <- 0.05
ci_sd <- 1 - sig_level_sd

## 2.1 null hypothesis H0: the mean execution cost variance is smaller than the curent value (100)
# A transformation should be applied to the data, because the CLT is not applicable to the sample variance estimator.
qqPlot(log(data_mean_test), pch = 16, las = 1)
qqPlot(sqrt(data_mean_test), pch = 16, las = 1)
## None of the cases lead to normality!

boot.out <- boot(my.sample, statistic = function(x, i){var(x[i])}, R = 999)
(my.boot.var <- boot.ci(boot.out, conf = ci_sd, type = "bca"))

## From this test, we can assure with a 95% confidence interval that the new version will have much less variance
## than the current one (100 -> 46.72).
