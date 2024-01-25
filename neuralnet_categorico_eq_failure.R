

# Realizado em 02/11/2023

# Utilizando o data set equipment_failure_data_1.csv

library(ISLR)
library(neuralnet)

#install.packages("MASS")
library("MASS")
library("rpart")


set.seed(0)

#Baixa os dados

data <- read.csv(file = "equipment_failure_data_1.csv")

head(data)



#private = as.numeric(College$Private)-1, foi o exemplo
#No caso a variável EQUIPMENT_FAILURE já está no formato 1 ou 0
#private <- ifelse(data$Private == 'Yes', 1, 0)

#Eliminando colunas
data <- data[,-c(1,2,3,4,5,6)]

n=names(data)
n

#Alterando a posição das coluna de forma que a variável "EQUIPMENT_FAILURE" seja o target

data <- data[c("S15","S17","S13","S16","S19","S18","S8","S5","AGE_OF_EQUIPMENT","EQUIPMENT_FAILURE")]

#Criando um objeto para a variável categórica

EQUIPMENT_FAILURE <- data$EQUIPMENT_FAILURE

#Padronizar dados para melhor performance
data <- data[,1:9]
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- data.frame(scale(data,center = min_data, scale = max_data - min_data))

#Inclui variável explicada (target)
scaled$EQUIPMENT_FAILURE <- EQUIPMENT_FAILURE


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
f <- as.formula(paste("EQUIPMENT_FAILURE ~", paste(n[!n %in% "EQUIPMENT_FAILURE"], collapse = " + ")))
f
nn <- neuralnet(f,data=test_data,hidden=c(5,4),linear.output=F)
plot(nn)

pr.nn <- compute(nn,test_data[,1:9]) # Não inclue a variável target

#Explica sapply que arredonda, no caso sem casas decimais

pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
pr.nn$net.result

table(test_data$EQUIPMENT_FAILURE,pr.nn$net.result)


Acc <- (44901) / (44901+56)
Acc
#CART comparação

set.seed(0)

# árvore
fit_tree <- rpart(f,method="class", data=train_data)#Árvores construída com 
#train_data e testada com test_data
tree_predict <- predict(fit_tree,test_data,type = "class")
table(test_data$EQUIPMENT_FAILURE,tree_predict)

Acc_tree <- (44901) / (44901+56)

Acc_tree

compare <- rbind(tree_predict,scaled$EQUIPMENT_FAILURE)
                 
compare

#Mesmíssimo resultado neste data set

#Plot

plot(test_data$EQUIPMENT_FAILURE,type = 'l',col="red",xlab = "x", ylab = "Falha")
lines(pr.nn$net.result,col = "blue")
tree_predict

# Acontece que todos os valores preditos são "0", não prevê falhas a partir
# do data set, Talvez porque o número de falhas é muito pequeno.
