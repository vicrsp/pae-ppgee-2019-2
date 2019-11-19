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
data <- data %>% filter(Mês < 11)

data$turma = as.factor(data$Turma..to.number.)
data$material = as.factor(data$Material..to.number.)
data$mes = as.factor(data$Mês)

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

#ridge
#ggplot(data, aes(x = VarConsumo_Eletrica, y = mes, fill = turma)) + geom_density_ridges() +
#  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) + theme(legend.position="bottom")

#ggplot(data, aes(x = VarConsumo_Quimica, y = mes, fill = turma)) + geom_density_ridges() +
#  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) + theme(legend.position="bottom")

#ggplot(data, aes(x = VarTempoProcesso, y = mes, fill = turma)) + geom_density_ridges() +
#  geom_density_ridges() + theme(legend.position="bottom")


# Raw
anv <- aov((VarTempoProcesso) ~ turma + mes, data = data)
par(mfrow=c(2,2))
plot(anv)
summary(anv)


par(mfrow=c(1,1))
qqPlot(anv$residuals)

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
