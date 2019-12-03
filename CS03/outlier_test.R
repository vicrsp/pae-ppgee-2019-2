df1 <- read.csv('rcbd.config.maressa.csv')[,c("algorithm","instance","result","replicate")]
df2 <- read.csv('rcbd.config.victor.csv')[,c("algorithm","instance","result","replicate")]
df3 <- read.csv('rcbd.config.gilmar.csv')[,c("algorithm","instance","result","replicate")]

df <- rbind(df1,df2,df3) %>% filter(instance > 4)

df <- df %>% mutate(resultLog = log(result)) 

df$out <- rep(0,length(df))

for(i in seq(5,150)){
  
  data.instance <- df %>% filter(instance == i)
  outliers <- boxplot(data.instance$result, plot = FALSE)$out
  #print(outliers)
  
  df[which(df$instance == i & df$result %in% outliers),"out"] <- 1
  
  #df.filt <- rbind(df.filt, data.instance[!data.instance %in% outliers])
}

df.filt <- df %>% filter(out == 0)


df.filt$instance = as.factor(df.filt$instance)
df.filt$algorithm = as.factor(df.filt$algorithm)


ggplot(df.filt, aes(x = instance, 
               y = resultLog, 
               group = (algorithm), 
               colour = (algorithm))) + geom_line(linetype=2) + geom_point(size=2)


ggplot(df.filt, aes(x = resultLog, y = instance, fill = algorithm)) + geom_density_ridges() +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) + theme(legend.position="bottom")

set.seed(1234)
random.blocks <- sample(5:150, 34)
data.block.instances <- df.filt %>%  filter(instance %in% random.blocks)

res.aov <- aov(resultLog  ~ algorithm + instance, data = data.block.instances)
summary(res.aov)

invisible(qqPlot(res.aov$residuals, dist='norm',envelope=.95, las = 1, pch = 16))
shapiro.test(res.aov$residuals)
