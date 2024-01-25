

# Realizado em 02/11/2023

# Utilizando o data set "predictive_maintenance.csv"

library(ISLR)
library(neuralnet)

#install.packages("MASS")
library("MASS")
library("rpart")
library("dplyr")

set.seed(2)

#Baixa os dados

data <- read.csv(file = "predictive_maintenance.csv")

head(data)
# Reduzindo o número de observaçoes para reduzir o tempo de processamento
# Calcula o número de observações a serem mantidas (20%)
n_data_1 <- 0.4*nrow(data)
n_data_1


# Realiza amostragem aleatória
amostra <- sample(1:nrow(data), n_data_1)

# Cria um novo dataset com base na amostra
data <- data[amostra,]


#private = as.numeric(College$Private)-1, foi o exemplo
#No caso a variável EQUIPMENT_FAILURE já está no formato 1 ou 0
#private <- ifelse(data$Private == 'Yes', 1, 0)

#Eliminando colunas
data <- data[,-c(1,2,3,10)]

n=names(data)
n

#Alterando o nome das variáveis

data <- rename(data,ar_temp="Air.temperature..K.",
               process_temp="Process.temperature..K.",
               rotac="Rotational.speed..rpm.",torque="Torque..Nm." ,
               desgaste="Tool.wear..min.",target= "Target"  )



#Criando um objeto para a variável categórica

target <- data$target

#Padronizar dados para melhor performance
data <- data[,1:5]
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- data.frame(scale(data,center = min_data, scale = max_data - min_data))

#Inclui variável explicada (target)
scaled$target <- target


set.seed(0)
#train test split
index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

#Utiliza o neuralnet
set.seed(0)

#Forma de não digitar os nomes das variáveis na neuralnet
n = names(train_data) #Objeto com o nome das variáveis

#Objeto com a variável target e variáveis features
f <- as.formula(paste("target ~", paste(n[!n %in% "target"], collapse = " + ")))
f
nn <- neuralnet(f,data=test_data,hidden=c(5,4),linear.output=F)
plot(nn)

pr.nn <- compute(nn,test_data[,1:5]) # Não inclue a variável target

#Explica sapply que arredonda, no caso sem casas decimais

pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
pr.nn$net.result

table(test_data$target,pr.nn$net.result)


Acc <- (24+1163) / (24+1163+13)
Acc
#CART comparação

set.seed(0)

# árvore
fit_tree <- rpart(f,method="class", data=train_data)#Árvores construída com 
#train_data e testada com test_data
tree_predict <- predict(fit_tree,test_data,type = "class")
table(test_data$target,tree_predict)


Acc_tree <- (1150+20) / (1150+20+17+13)

Acc_tree


#Acc= 0,9892; Acc_tree=0,975 O que representa que o modelo em rede neural 
# é mais preciso

#Plot

plot(test_data$target,type = 'l',col="red",xlab = "x", ylab = "Falha")
lines(pr.nn$net.result,col = "blue")
tree_predict



#Fazendo previsões pontuais

novos_dados <- data.frame(ar_temp=300, process_temp=350,rotac=3000,
                          torque=3000,desgaste=200  )


previsoes <- predict(nn,novos_dados[,1:5])
previsoes <- sapply(previsoes,round,digits=0)

print(previsoes)

