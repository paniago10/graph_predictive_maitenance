

# Realizado em 06/11/2023
#Refeito em 27/11/2023

# Utilizando  machine_data_samples_1.csv
library(dplyr)
library(ISLR)
library(neuralnet)

#install.packages("MASS")
library("MASS")
library("rpart")


set.seed(0)

#Baixa os dados

data <- read.csv(file = "machine_data_samples_1.csv")

head(data)

n=names(data)
n


#Criando um objeto para a variável categórica

EQUIPMENT_FAILURE <- data$Failure

#Padronizar dados para melhor performance
data <- data[,1:6]
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- data.frame(scale(data,center = min_data, scale = max_data - min_data))

#Inclui variável explicada (target)
scaled$Failure <- EQUIPMENT_FAILURE


set.seed(0)
#train test split
index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(scaled[index,])
test_data <- as.data.frame(scaled[-index,])

#Utiliza o neuralnet
set.seed(0)

#Forma de não digitar os nomes das variáveis na neuralnet
n = names(train_data) #Objeto com o nome das variáveis
n
#Objeto com a variável target e variáveis features
f <- as.formula(paste("Failure ~", paste(n[!n %in% "Failure"], collapse = " + ")))
f
nn <- neuralnet(f,data=test_data,hidden=c(6,6,5,4),linear.output=F)

plot(nn)

pr.nn <-compute(nn,test_data[,1:6]) # Não inclue a variável target

#Explica sapply que arredonda, no caso sem casas decimais

pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
pr.nn$net.result

contagem <-table(pr.nn$net.result)
contagem

table(test_data$Failure,pr.nn$net.result)


Acc <- (1344+407) / (1344+407+49)
Acc
#97,27%

library(neuralnet)
library(openxlsx)


# Gerar previsões de falha no tempo com a base de treino
predictions <- predict(nn, train_data)
predictions_df <- data.frame(Failure = round(predictions[, 1]),
                             Time_Used_h = train_data$Time_Used_h)

# Filtrar apenas as previsões onde "Failure" é igual a 1
predictions_failure_1 <- predictions_df[predictions_df$Failure == 1, , drop = FALSE]

# Desescala dos dados
descaled_predictions <- predictions_failure_1
descaled_predictions$Time_Used_h <- descaled_predictions$Time_Used_h *(max_data["Time_Used_h"] - min_data["Time_Used_h"]) + min_data["Time_Used_h"]


# Salvar os resultados em uma tabela Excel
write.xlsx(descaled_predictions, file = "resultados.xlsx", rowNames = FALSE)


#CART comparação

set.seed(0)

# árvore
fit_tree <- rpart(f,method="class", data=train_data)#Árvores construída com 
#train_data e testada com test_data
tree_predict <- predict(fit_tree,test_data,type = "class")
table(test_data$Failure,tree_predict)



Acc <- (1349+451) / (1349+451)
Acc
#98,72%



#Fazendo outra abordagem na construção da árvores de decisão
#Usando as Variáveis não escaladas

#Inclui variável explicada (target)
data$Failure <- EQUIPMENT_FAILURE

#Transfornado os dados de Failure para categóricos, pois assim é possível
#Fazer o "caret::twoClassSummary"

data$Failure <- as.factor(data$Failure)




#train test split

train_data_c <- as.data.frame(data[index,])
test_data_c <- as.data.frame(data[-index,])



arvore <- rpart::rpart(f,
                       data=test_data_c,
                       method='class',
                       xval=5,
                       control = rpart.control(cp = 0, #Custo de complexidade
                                               minsplit = 1, #valor mínimo de observações
                                               maxdepth = 7) #Profundidade máxima da árvores
)
print(arvore)
# Visualizando a árvore #

# Definindo uma paleta de cores

paleta = scales::viridis_pal(begin=.75, end=1)(20)

# Plotando a árvore

rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

tree_predict <- predict(arvore,test_data_c,type = "class")
table(test_data$Failure,tree_predict)

#tree_predict
#     0    1
#0 1349    0
#1   0   471
Acc_tree <- (1349+471) / (1349+471)

Acc_tree

#100%

# Verificando a complexidade da árvore
arvore$frame

############################################
# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, train_data_c)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "1", "0"))

head(p_treino)

p_teste = stats::predict(arvore, test_data_c)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "1", "0"))

head(p_teste)
c_treino


tab <- table(c_treino, train_data_c$Failure) #Tabela 
acc <- (tab[1,1]+tab[2,2])/nrow(train_data_c)
acc

#99,959%

tab

#c_treino    0    1
#0         3921   82
#1           15  182


tab <- table(c_teste, test_data_c$Failure)
acc <- (tab[1,1]+tab[2,2])/nrow(test_data_c)
acc
#100%

tab
#c_teste    0    1
#0        1349  0
#1         0    451

###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (1 no caso): contém a probabilidade da classe 1
# <classe 2> (0 no caso): contém a probabilidade da classe 2
aval_treino <- data.frame(obs=train_data_c$Failure, 
                          pred=c_treino,
                          "1" = p_treino[,2],
                          "0" = 1-p_treino[,2]
)
head(aval_treino)




caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))
aval_treino <- data.frame(obs=train_data_c$Failure, 
                          pred=c_treino,
                          "1" = p_treino[,2],
                          "0" = 1-p_treino[,2]
)
head(aval_treino)



caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

#Avaliando com a base de teste

aval_teste <- data.frame(obs=test_data_c$Failure, 
                          pred=c_teste,
                          "1" = p_teste[,2],
                          "0" = 1-p_teste[,2]
)
head(aval_teste)

caret::twoClassSummary(aval_teste, lev=levels(aval_teste$obs))


#Plot

plot(test_data$Failure,type = 'l',col="red",xlab = "x", ylab = "Falha")
lines(pr.nn$net.result,col = "blue")


# Acontece que todos os valores preditos são "0", não prevê falhas a partir
# do data set, Talvez porque o número de falhas é muito pequeno.

