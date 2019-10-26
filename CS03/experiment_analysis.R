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

data.maressa <- read.csv('rcbd.config.maressa.csv')
data.victor <- read.csv('rcbd.config.victor.csv')
data.gilmar <- read.csv('rcbd.config.gilmar.csv')

data.all.instances <- data.maressa %>% bind_rows(data.victor) %>% bind_rows(data.gilmar) %>% select(algorithm, replicate, instance, group, result)
write.csv(data.all.instances, 'data.all.instances.csv')

res.aov <- aov(result ~ algorithm + instance, data = data.all.instances)

#data.experiment.grouped <- data.experiment %>% group_by(Algorithm,Instance) %>% summarise(avg = mean(Observation))

ggplot(data.experiment, aes(x = ObservationLogScaled, y = Instance, fill = Algorithm)) + geom_density_ridges() +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03)

#ggplot(data.experiment %>% filter(Algorithm == 'algo2'), aes(x = ObservationLogScaled,  fill = Instance)) +
#  geom_density() 

ggplot(data.experiment, aes(x = Instance, 
                            y = ObservationLogScaled, 
                            group = Algorithm, 
                            colour = Algorithm)) + geom_line(linetype=2) + geom_point(size=2)
