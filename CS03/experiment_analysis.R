if (!require(ggplot2, quietly = TRUE)){
  install.packages("ggplot2")
}

if (!require(ggridges, quietly = TRUE)){
  install.packages("ggridges")
}

if (!require(dplyr, quietly = TRUE)){
  install.packages("dplyr")
}

# Analysis ----------------------------------------------------------------
data.experiment <- read.table('experiment_raw_results.txt')
data.experiment$ObservationLogScaled <- log(data.experiment$Observation)

data.all.instances <- read.csv('data.all.instances.csv')
data.all.instances.normalized <- data.all.instances %>% group_by(instance) %>% mutate(Max = max(result), Min = min(result), Std = sd(result), Avg = mean(result)) %>% mutate(resultMinMaxNorm = (result - Min)/(Max - Min), resultZnorm = (result - Avg)/Std)

b <- 34
random.blocks <- sample.int(150, b)
data.block.instances <- data.all.instances.normalized %>%  filter(instance %in% random.blocks)

res.aov <- aov(resultMinMaxNorm  ~ algorithm + instance, data = data.block.instances)

#data.experiment.grouped <- data.experiment %>% group_by(Algorithm,Instance) %>% summarise(avg = mean(Observation))

ggplot(data.block.instances, aes(x = resultMinMaxNorm, y = as.factor(instance), fill = as.factor(algorithm))) + geom_density_ridges() +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03)

#ggplot(data.experiment %>% filter(Algorithm == 'algo2'), aes(x = ObservationLogScaled,  fill = Instance)) +
#  geom_density() 

ggplot(data.block.instances, aes(x = instance, 
                            y = resultMinMaxNorm, 
                            group = as.factor(algorithm), 
                            colour = as.factor(algorithm))) + geom_line(linetype=2) + geom_point(size=2)
