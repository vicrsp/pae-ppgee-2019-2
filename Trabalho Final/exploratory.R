if (!require(ggplot2, quietly = TRUE)){
  install.packages("ggplot2")
}

if (!require(car, quietly = TRUE)){
  install.packages("car")
}

if (!require(boot, quietly = TRUE)){
  install.packages("boot")
}

if (!require(dplyr, quietly = TRUE)){
  install.packages("boot")
}

if (!require(ggridges, quietly = TRUE)){
  install.packages("ggridges")
}

if (!require(multcomp, quietly = TRUE)){
  install.packages("multcomp")
}


data <- read.csv('data_pre_processed_raw.csv')
data.materials <- read.csv('data_pre_processed_filt_materials.csv')

data$turma = as.factor(data$Turma)
data$material = as.factor(data$Material)
data$mes = as.factor(data$Mês)
data$turno = as.factor(data$Turno)

data.materials$turma = as.factor(data.materials$Turma)
data.materials$material = as.factor(data.materials$Material)
data.materials$mes = as.factor(data.materials$Mês)
data.materials$turno = as.factor(data.materials$Turno)

#boxplot
ggplot(data, aes(x=mes, y=VarConsumo_Eletrica)) + 
  geom_boxplot() +  facet_grid(cols = vars(turma), scales = "free_y")

ggplot(data, aes(x=mes, y=VarConsumo_Quimica)) + 
  geom_boxplot() +  facet_grid(cols = vars(turma), scales = "free_y")

ggplot(data, aes(x=mes, y=VarTempoProcesso)) + 
  geom_boxplot() +  facet_grid(cols = vars(turma), scales = "free_y")


#boxplot2
ggplot(data, aes(x=mes, y=VarConsumo_Eletrica, fill = turma)) + 
  geom_boxplot() 

ggplot(data, aes(x=mes, y=VarConsumo_Quimica, fill = turma)) + 
  geom_boxplot() 

ggplot(data, aes(x=mes, y=VarTempoProcesso, fill = turma)) + 
  geom_boxplot() 


#boxplot3
ggplot(data.materials, aes(x=mes, y=VarConsumo_Eletrica, fill = material)) + 
  geom_boxplot() + facet_grid(cols = vars(turma), scales = "free_y")

ggplot(data.materials, aes(x=mes, y=VarConsumo_Quimica, fill = material)) + 
  geom_boxplot() + facet_grid(cols = vars(turma), rows=vars(material), scales = "free_y")

ggplot(data.materials, aes(x=mes, y=VarTempoProcesso)) + 
  geom_boxplot() + facet_grid(cols = vars(turma), rows=vars(material), scales = "free_y")


#ridge
#ggplot(data, aes(x = VarConsumo_Eletrica, y = mes, fill = turma)) + geom_density_ridges() +
#  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) + theme(legend.position="bottom")

#ggplot(data, aes(x = VarConsumo_Quimica, y = mes, fill = turma)) + geom_density_ridges() +
#  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) + theme(legend.position="bottom")

#ggplot(data, aes(x = VarTempoProcesso, y = mes, fill = turma)) + geom_density_ridges() +
#  geom_density_ridges() + theme(legend.position="bottom")

#scatter
ggplot(data, aes(x=VarConsumo_Quimica, y=VarConsumo_Eletrica, color=material)) + geom_point() +  facet_grid(cols = vars(turma), scales = "free_y")


## Fatorial
anv <- aov(log(VarConsumo_Quimica) ~  turno * material, data = data.materials)
par(mfrow=c(2,2))
plot(anv)
summary(anv)

# Multiple comparison
tukey.test <- glht(anv, linfct = mcp(material = "Tukey"))
tukey.test.CI <- confint(tukey.test, level = 0.95)
par(mar = c(5, 8, 4, 2), las = 1)
par(mfrow=c(1,1))
plot(tukey.test.CI, xlab = "Quimica")

## Quimica
# Log transf
anv <- aov(log(VarConsumo_Quimica) ~ turma + mes, data = data)
par(mfrow=c(2,2))
plot(anv)
summary(anv)

# Multiple comparison
tukey.test <- glht(anv, linfct = mcp(turma = "Tukey"))
tukey.test.CI <- confint(tukey.test, level = 0.95)
par(mar = c(5, 8, 4, 2), las = 1)
par(mfrow=c(1,1))
plot(tukey.test.CI, xlab = "Quimica")

## Eletrica
# Log transf
anv <- aov(log(VarConsumo_Eletrica) ~ turma + mes, data = data)
par(mfrow=c(2,2))
plot(anv)
summary(anv)

# Multiple comparison
tukey.test <- glht(anv, linfct = mcp(turma = "Tukey"))
tukey.test.CI <- confint(tukey.test, level = 0.95)
par(mar = c(5, 8, 4, 2), las = 1)
par(mfrow=c(1,1))
plot(tukey.test.CI, xlab = "Eletrica")


## Tempo
# Log transf
anv <- aov(log(VarTempoProcesso) ~ turma + mes, data = data)
par(mfrow=c(2,2))
plot(anv)
summary(anv)

# Multiple comparison
tukey.test <- glht(anv, linfct = mcp(turma = "Tukey"))
tukey.test.CI <- confint(tukey.test, level = 0.95)
par(mar = c(5, 8, 4, 2), las = 1)
par(mfrow=c(1,1))
plot(tukey.test.CI, xlab = "Tempo de processo")

## Sample sizes
a <- 4
b <- 9
d <- 1
alpha <- 0.05
beta <- 0.2

tau <- c(-d, d, rep(0, a - 2)) # define tau vector
n <- 2

tb <- data.frame(n = rep(-1, 50), ratio = rep(-1,50), phi = rep(-1,50))

for(i in seq(1,40,by=2)){
  
  n <- i + 2
  f1 <- qf(1 - alpha, a - 1, a*b*(n - 1))
  f2 <- qf(beta, a - 1, a*b*(n - 1), (a*n*sum(tau^2)/b))
  phi <- a*n*sum(tau^2)/b
  
  tb[i, ] = c(n, f1/f2, phi)
}

n_b <- min((tb %>% filter(ratio <= 1 & ratio > 0))$n)
tb %>% filter(ratio > 0)
