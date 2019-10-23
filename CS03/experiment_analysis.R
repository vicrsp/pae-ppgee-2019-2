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
my.results <- readRDS(file = 'experiment_data_test/CAISEr_results_20191022122433.rds', refhook = NULL)
summary(my.results)

data.experiment <- read.table('experiment_raw_results.txt')

data.experiment.instance.stats <- data.experiment %>% group_by(Instance) %>% summarise(Avg = mean(Observation), Std = sd(Observation), Min = min(Observation), Max = max(Observation))
data.experiment <- data.experiment %>% inner_join(data.experiment.instance.stats, by = "Instance") 

data.experiment <- data.experiment %>% mutate(ObservationZNormalized = (Observation - Avg)/Std)
data.experiment <- data.experiment %>% mutate(ObservationMinMaxNormalized = (Observation - Min)/(Max - Min))

data.experiment$ObservationLogScaled <- log(data.experiment$Observation)

data.experiment <- data.experiment %>% mutate(InstanceNumber = as.integer(gsub("Inst.","", Instance))) %>% arrange(InstanceNumber)
data.experiment$InstanceNumber <- as.factor(data.experiment$InstanceNumber)

ggplot(data.experiment, aes(x = ObservationMinMaxNormalized, y = InstanceNumber, fill = Algorithm)) + geom_density_ridges() +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03)

ggplot(data.experiment, aes(x = InstanceNumber, 
                            y = ObservationMinMaxNormalized, 
                            group = Algorithm, 
                            colour = Algorithm)) + geom_line(linetype=2) + geom_point(size=2)

