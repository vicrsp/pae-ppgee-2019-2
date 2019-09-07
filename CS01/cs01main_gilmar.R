#----------TRABALHO DE PLANEJAMENTO E ANÁLISE DE EXPERIMENTOS-------------------
#Estudo de Caso 1:
#Autor: Gilmar Pereira
#Grupo: D
#Data da utlima modificação:
#-------------------------------------------------------------------------------

#Verificação e instalação de pacotes:
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------

#Conjunto de dados para o experimento:
#-------------------------------------------------------------------------------
mre <- list(name = "recombination_bin", cr = 0.9)
mmu <- list(name = "mutation_rand", f = 2)
mpo <- 100
mse <- list(name = "selection_standard")
mst <- list(names = "stop_maxeval", maxevals = 10000)
mpr <- list(name = "sphere", xmin = -seq(1, 20), xmax = 20 + 5 * seq(5, 24))
#-------------------------------------------------------------------------------

#Funções criadas:
#-------------------------------------------------------------------------------
#função para observação simples (geração de um dado)
get.single.observation <- function(mpo, mmu, mre, mse, mst, mpr){
  generator <- ExpDE(mpo, mmu, mre, mse, mst, mpr, showpars = list(show.iters = "none"))
  return(generator$Fbest)
}
#Função para observaçõa multipla (geração de n dados)
get.n.samples <- function(mpo, mmu, mre, mse, mst, mpr, N){
  my.sample <- numeric(N)
  for (i in seq(N)){
    my.sample[i] <- get.single.observation(mpo, mmu, mre, mse, mst, mpr)
  }
  return(my.sample)
}
#-------------------------------------------------------------------------------

#Teste do CUSTO MÉDIO:
#-------------------------------------------------------------------------------

#Dados para o trabalho:
alpha<-0.01 #nivel de significancia
delta<-4    #tamanho minimo do efeito relevante
pi<-0.8     #potencia desejada
beta<-1-pi 

#constantes atuais:
media_a<-50
variancia_a<-100

#Atividade 1: Definição da Hipotese nula
#Objetivo: custo médio menor que software anterior (u=50)
#custo_medio<50

#Atividade 2: Tamanho amostral
#Calculo do tamanho amostral usando a função power.t.test:
sample_size <- power.t.test(delta = delta,
                             sd = sqrt(variancia_a),
                             sig.level = alpha,
                             power = pi,
                             alternative = "one.sided",
                             type = "one.sample")

N <- ceiling(sample_size$n)
print(c("Tamanho amostral calculado: ", N), quote = FALSE)

#Atividade 3: Teste da hipotese
#Gerando os dados:
my_samples<-get.n.samples(mpo,mmu,mre,mse,mst,mpr,N)

#Analise exploratória dos dados:
#Dados estatisticos (minimo, 1ºquartil, mediana, media, 3ºquartil, maximo)
summary(my_samples)

#Variancia:
var(my_samples)

#boxplot (quartis)
boxplot(my_samples, horizontal = TRUE)

#histograma
hist(my_samples)

#normal
qqnorm(my_samples, las = 1)
qqline(my_samples)

#teste para custo médio
my_teste<-t.test(my_samples, 
                  mu=50,                 #hipotese nula
                  alternative = "greater",  #hipotese alternativa
                  conf.level = 0.99)
my_teste
#Atividade 4: Calculo do intervalo de confiança
my_teste$conf.int

#Atividade 5: Validação e discussão
#reamostragem:
N2<-999
my_means<-numeric(N2)
for(i in seq(N2)){
  my_means_sample<-sample(my_samples, replace = TRUE)
  my_means[i]<-mean(my_means_sample)
}

#normal
qqnorm(my_means, las = 1, pch = 16)
qqline(my_means)

#Atividade 6: Discussão sobre potencia do teste
#intervalo superior unilateral de confiança
intervalo_conf_max<-(N-1)*var(my_samples)/qchisq(p = 0.01, df = N-1)
intervalo_conf_max

#calculo da potencia
(pontencia<-power.t.test(n = N, delta = delta, sd = sqrt(intervalo_conf_max), sig.level = alpha, type = "one.sample", alternative = "one.sided"))

#-------------------------------------------------------------------------------


#Teste da VARIANCIA:
#-------------------------------------------------------------------------------

#Atividade 1: Definição da hipotese nula


#Atividade 2: Teste da hipotese

#Atividade 3: Intervalo de confiança para variancia

#Atividade 4: Validação e discussão

#-------------------------------------------------------------------------------


