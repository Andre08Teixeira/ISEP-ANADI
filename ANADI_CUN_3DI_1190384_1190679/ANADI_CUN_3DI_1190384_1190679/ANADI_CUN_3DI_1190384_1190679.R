### Trabalho Prático 2
### 3DI 
### André Teixeira - 1190384@isep.ipp.pt
### Ivo Oliveira - 1190679@isep.ipp.pt

#Packages
#install.packages("dplyr")
library(dplyr)
#install.packages("e1071")
library(e1071)
#install.packages("corrplot")
library(corrplot)
#install.packages("readr")
library(readr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("neuralnet")
library(neuralnet)
#install.packages("NeuralNetTools")
library(NeuralNetTools)
#install.packages("FNN")
library(FNN)
#install.packages("class")
library(class)
#install.packages("car")
library(car)
#install.packages("nortest")
library(nortest)
#install.packages("caret")
library(caret)


#Significado dos atributos
#altitude_results -> Altitude
#vo2_results -> Oxigénio no sangue
#hr_results -> Batimento do coração
#gender -> Género
#Team -> Equipa
#Background -> Perfil do ciclista
#Pro.level -> Nível de competição do ciclista
#Winter.Training.Camp - > Campo de treino de Inverno
#Continent -> Continente

##Funcoes utilizadas
# Função de normalização min-max
normalise <- function(y) {
  (y - min(y)) / (max(y) - min(y))
}
# Função de denormalização dos dados
minmaxdesnorm <- function(x, goal.attrib) {
  return (x * (max(goal.attrib) - min(goal.attrib)) + min(goal.attrib))
}

# Função para calcular o MAE
MAE <- function(test, predicted) {
  mean(abs(test - predicted))
}

# Função para calcular o RMSE
RMSE <- function(test, predicted) {
  sqrt(mean((test - predicted) ^ 2))
}

#minmax function y'= (y-miny)/(maxy-miny)
minmax <- function(x){
  return (x-min(x)/(max(x)-min(x)))
}

#Tópico 4.1
#Exercicio 1
#Import dos dados
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ciclistas <- read.csv("Dados Excel/ciclismo.csv") #ler os ciclistas do ficheiro
dim(ciclistas) ##dimensão dos dados
summary(ciclistas) ##sumário dos dados

#Exercicio 2
#Criação do atributo Age
ciclistas$dob <- as.Date(ciclistas$dob, format = "%Y-%m-%d")
ciclistas <- cbind(ciclistas, Age =  as.integer((Sys.Date() - ciclistas$dob) / 365.25))


#Exercicio 3
#Analise dos atributos significativos

#Medidas de forma
#Boxplot
boxplot(ciclistas$altitude_results)
boxplot(ciclistas$vo2_results)
boxplot(ciclistas$hr_results)
boxplot(ciclistas$Age)

#Histograma
hist(ciclistas$altitude_results)
hist(ciclistas$vo2_results)
hist(ciclistas$hr_results)
hist(ciclistas$Age)

#Assimetria
skewness(ciclistas$altitude_results) #-0.1151302
skewness(ciclistas$vo2_results) #-0.2323301
skewness(ciclistas$hr_results) #-0.2069204
skewness(ciclistas$Age) #-0.004814619

#Curtose
kurtosis(ciclistas$altitude_results) #-0.3417119
kurtosis(ciclistas$vo2_results) #-0.2408601
kurtosis(ciclistas$hr_results) #-0.2256084
kurtosis(ciclistas$Age) #-1.150905

#Medidas de variabilidade/dispersão
#Amplitude
diff(range(ciclistas$altitude_results)) # 76
diff(range(ciclistas$vo2_results)) # 79
diff(range(ciclistas$hr_results)) # 83
diff(range(ciclistas$Age)) # 21

#Amplitude Inter-Quartil
IQR(ciclistas$altitude_results) # 20
IQR(ciclistas$vo2_results) # 20
IQR(ciclistas$hr_results) # 21
IQR(ciclistas$Age) # 10

#Vâriancia
var(ciclistas$altitude_results) # 203.6797
var(ciclistas$vo2_results) # 197.024
var(ciclistas$hr_results) # 216.4331
var(ciclistas$Age) # 34.46224

#Desvio-padrão
sd(ciclistas$altitude_results) # 14.27164
sd(ciclistas$vo2_results) # 14.03652
sd(ciclistas$hr_results) # 14.71167
sd(ciclistas$Age) # 5.870455

#Medidas de forma
#Boxplot
boxplot(ciclistas$altitude_results)
boxplot(ciclistas$vo2_results)
boxplot(ciclistas$hr_results)
boxplot(ciclistas$Age)

#Histograma
hist(ciclistas$altitude_results)
hist(ciclistas$vo2_results)
hist(ciclistas$hr_results)
hist(ciclistas$Age)

#Assimetria
skewness(ciclistas$altitude_results) #-0.1151302
skewness(ciclistas$vo2_results) #-0.2323301
skewness(ciclistas$hr_results) #-0.2069204
skewness(ciclistas$Age) #-0.004814619

#Curtose
kurtosis(ciclistas$altitude_results) #-0.3417119
kurtosis(ciclistas$vo2_results) #-0.2408601
kurtosis(ciclistas$hr_results) #-0.2256084
kurtosis(ciclistas$Age) #-1.150905

#Gráfico circular
###Género
contGender <- table(ciclistas$gender) #Calcular a contagem géneros
percentGender <- round(100 * contGender / sum(contGender), 1) #Calcular as percentagens
pie(contGender, labels = paste(names(contGender), percentGender, "%"), col = c("pink", "blue"), main = "Gender Distribution")

###Equipa
contTeam <- table(ciclistas$Team) #Calcular a contagem equipas
percentTeam <- round(100 * contTeam / sum(contTeam), 1) #Calcular as percentagens
pie(contTeam, labels = paste(names(contTeam), percentTeam, "%"), col = c("red", "green","blue", "yellow","pink"), main = "Team Distribution")

###Perfil do ciclista
contBackground <- table(ciclistas$Background) #Calcular a contagem de perfis de ciclistas
percentBackground <- round(100 * contBackground / sum(contBackground), 1) #Calcular as percentagens
pie(contBackground, labels = paste(names(contBackground), percentBackground, "%"), col = c("red", "green","blue", "yellow","pink","gray"), main = "Background Distribution")

###Nível de competição do ciclista
contProLevel <- table(ciclistas$Pro.level) #Calcular a contagem de elementos de cada equipa
percentProLevel <- round(100 * contProLevel / sum(contProLevel), 1) #Calcular as percentagens
pie(contProLevel, labels = paste(names(contProLevel), percentProLevel, "%"), col = c("red", "green"), main = "Pro level Distribution")

###Campo de treino de Inverno 
contWinterTraining <- table(ciclistas$Winter.Training.Camp) #Calcular a contagem de elementos que completaram o Campo de treino de Inverno 
percentWinterTraining <- round(100 * contWinterTraining / sum(contWinterTraining), 1) #Calcular as percentagens
pie(contWinterTraining, labels = paste(names(contWinterTraining), percentWinterTraining, "%"), col = c("green", "red"), main = "Winter Training Camp Completion Distribution")

###Continente 
contContinent <- table(ciclistas$Continent) #Calcular a contagem de coninentes 
percentContinent <- round(100 * contContinent / sum(contContinent), 1) #Calcular as percentagens
pie(contContinent, labels = paste(names(contContinent), percentContinent, "%"), col = c("red", "green","blue", "yellow","pink","gray"), main = "Origin Continent Distribution")


#Execicio 4
#a) Remover NA
if (anyNA(ciclistas)) {
  print(sum(is.na(ciclistas))) -> 0 
  ciclistas <- na.omit(ciclistas)
}

#b) Identificar dados inconsistentes e outliers (outl)
#Usando IQR
#Altitude
outlAltitudeResults <- c(ciclistas$altitude_results[ciclistas$altitude_results < (quantile(ciclistas$altitude_results, 0.25) - 1.5 * IQR(ciclistas$altitude_results))], 
                         ciclistas$altitude_results[ciclistas$altitude_results > (quantile(ciclistas$altitude_results, 0.75) + 1.5 * IQR(ciclistas$altitude_results))])
#R:. 24


#Oxigénio no sangue
outl_vo2Results <- c(ciclistas$vo2_results[ciclistas$vo2_results < (quantile(ciclistas$vo2_results, 0.25) - 1.5 * IQR(ciclistas$vo2_results))], 
                     ciclistas$vo2_results[ciclistas$vo2_results > (quantile(ciclistas$vo2_results, 0.75) + 1.5 * IQR(ciclistas$vo2_results))])
#R:. 21, 24, 27, 28 

#Batimento cardiaco
outl_hrResults <- c(ciclistas$hr_results[ciclistas$hr_results < (quantile(ciclistas$hr_results, 0.25) - 1.5 * IQR(ciclistas$hr_results))], 
                    ciclistas$hr_results[ciclistas$hr_results > (quantile(ciclistas$hr_results, 0.75) + 1.5 * IQR(ciclistas$hr_results))])
#R:. 17, 19 


#Idade
outlIdade <- c(ciclistas$Age[ciclistas$Age < (quantile(ciclistas$Age, 0.25) - 1.5 * IQR(ciclistas$Age))], 
               ciclistas$Age[ciclistas$Age > (quantile(ciclistas$Age, 0.75) + 1.5 * IQR(ciclistas$Age))])
#R:. -

#c) Implementar seleção de atributos
# Remover colunas irrelevantes
ciclistas <- subset(ciclistas, select = -c(ID, dob))

#d)
# Converter valores do género para número
ciclistas$gender <- ifelse(ciclistas$gender == "male", 1, 0)

# Converter valores de Prolevel para número
ciclistas$Pro.level <- ifelse(ciclistas$Pro.level == "Continental", 0, 1)

# Converter valores de Winter Training Camp para número
ciclistas$Winter.Training.Camp <- ifelse(ciclistas$Winter.Training.Camp == "none", 0, 1)

# Converter valores de Background para número
ciclistas$Background <- ifelse(ciclistas$Background == "None", 0, 
                           ifelse(ciclistas$Background == "Time Trial", 1, 
                                  ifelse(ciclistas$Background == "Sprinter", 2, 
                                         ifelse(ciclistas$Background == "Cobblestones", 3, 
                                                ifelse(ciclistas$Background == "Hill", 4, 
                                                       ifelse(ciclistas$Background == "Mountain", 5, -1))))))

# Converter valores das equipas para número
ciclistas$Team <- ifelse(ciclistas$Team == "group A", 0, 
                     ifelse(ciclistas$Team == "group B", 1, 
                            ifelse(ciclistas$Team == "group C", 2, 
                                   ifelse(ciclistas$Team == "group D", 3, 
                                          ifelse(ciclistas$Team == "group E", 4, -1)))))

# Converter valores do continente para número
ciclistas$Continent <- ifelse(ciclistas$Continent == "Africa", 0, 
                          ifelse(ciclistas$Continent == "Asia", 1, 
                                 ifelse(ciclistas$Continent == "Australia", 2, 
                                        ifelse(ciclistas$Continent == "Europe", 3, 
                                               ifelse(ciclistas$Continent == "North America", 4,
                                                      ifelse(ciclistas$Continent == "South America", 4,-1))))))


#Exercicio 5
#Diagrama de correlação entre todos os atributos
corrDiagram <- cor(ciclistas)
corrplot(corrDiagram, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45)

#Exercicio 6
# Calcula o número de linhas dos dados
nrows <- nrow(ciclistas)
# Define o tamanho do conjunto de treinamento (70% dos dados)
train_size <- floor(0.7 * nrows)
# Gera uma sequência de índices aleatórios
random_indices <- sample(nrows)
# Seleciona os índices para o conjunto de treinamento e teste
train_indices <- random_indices[1:train_size]
test_indices <- random_indices[(train_size + 1):nrows]
# Divide os dados em conjunto de treinamento e teste
data.train <- ciclistas[train_indices, ]
data.test <- ciclistas[test_indices, ]
# Resumo dos resultados de altitude nos conjuntos de treinamento e teste
summary(data.train$altitude_results)
summary(data.test$altitude_results)

#a)Função linear resultante

#Ajuste ao modelo de regressão linear
formula <- altitude_results ~ hr_results
model <- lm(formula, data = data.train)
model
#R:. Desta forma obtemos a seguinte função linear: altitude_results = 14.556 + 0.767 * hr_results

#b) Visualização a reta correspondente ao modelo de regressão linear simples e o respetivo diagrama de dispersão
ggplot(data.train, aes(x = hr_results, y = altitude_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
#R:. A reta apresentada indica que quanto maiores sao os valores do teste de frequencia cardiaca (hr_results),
# maiores sao os valores do treino de altitude (altitude_results). 


#c) Calculo do erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) do
# modelo sobre os 30% casos de teste.
diff <- data.test$altitude_results-predict(model, data.test)

# MAE
mae <- mean(abs(diff))
cat("mae",mae) # 6.772438

# RMSE
rmse <- sqrt(mean(diff^2))
cat("rmse: ",rmse) # 8.28004


#d) Utilizando um modelo diferente para tentar atingir melhores resultados
# Ajusta o modelo de regressão linear com múltiplas variáveis preditoras
formula <- altitude_results ~ hr_results + vo2_results + gender + Background + Pro.level + Age
model_2 <- lm(formula, data = data.train)
# Calcular coeficientes do modelo
coefficients <- coef(summary(model_2))
# Calcular previsões de acordo com o modelo ajustado
pred_2 <- predict(model_2, newdata = data.test)
# Calcular diferenças entre valores observados e previstos
dif_2 <- data.test$altitude_results - pred_2
#MAE
mae <- mean(abs(dif_2))
cat("mae:", mae)
#RMSE
rmse <- sqrt(mean(dif_2^2))
cat("rmse:", rmse)


#Exercicio 7
#a) Regressão linear múltipla
summary(ciclistas$vo2_results)

# Normalizar os dados
ciclistas.norm <- as.data.frame(lapply(ciclistas, normalise))

# Verificar dados após a normalização (agora entre 0 e 1)
summary(ciclistas$vo2_results)
summary(ciclistas)

# Aplicar o critério Holdout
index <- sample(1:nrow(ciclistas), 0.7 * nrow(ciclistas))
ciclistas.train <- ciclistas.norm[index, ]
ciclistas.test <- ciclistas.norm[-index, ]
dim(ciclistas.test)

# Verificar resumo dos resultados de vo2_results nos conjuntos de treinamento e teste
summary(ciclistas.train$vo2_results)
summary(ciclistas.test$vo2_results)

# Realizar regressão linear múltipla
mlr.model <- lm(vo2_results ~ ., data = ciclistas.train)

# Examinar os resultados da regressão linear
summary(mlr.model)

# Produzir gráficos adicionais de variáveis
avPlots(mlr.model)


#b) Obter árvore de regressão usando a função "rpart"
# Ajustar o modelo de árvore de decisão
formula <- vo2_results ~ .
tree.model <- rpart(formula, method = "anova", data = ciclistas.train)
# Gerar o gráfico da árvore de decisão para visualização
rpart.plot(tree.model, digits = 3)


#c) Obter rede neuronal usando a função "neuralnet"
# Ajustar o modelo de rede neuronal
model <- vo2_results ~ gender + Team + Background + Pro.level + 
  Winter.Training.Camp + altitude_results + hr_results + Continent + Age

# 1 Node interno
numnodes <- 1

nn.model <- neuralnet(model,
                      data = ciclistas.train,
                      hidden = numnodes
)

# Visualizar a estrutura da rede neuronal
plot(nn.model)

# Obter os nomes das camadas e pesos da rede neuronal
names(nn.model$weights)
nn.model$weights

# Obter a matriz de resultados do modelo
nn.model$result.matrix

# Obter os resultados da rede neuronal
nn.model$net.result

#Exercicio 8
#Comparar os resultados obtidos pelos modelos referidos na questão 7, usando o erro 
#médio absoluto (MAE) e a raiz quadrada do erro médio (RMSE)
# Previsões do modelo de regressão linear múltipla
mlr.pred <- predict(mlr.model, newdata = ciclistas.test)
mlr.pred <- minmaxdesnorm(mlr.pred, ciclistas.norm$vo2_results)

# Cálculo do MAE e RMSE para o modelo de regressão linear múltipla
mlr.mae <- MAE(ciclistas.test$vo2_results, mlr.pred)
cat("mae:", mlr.mae)
mlr.rmse <- RMSE(ciclistas.test$vo2_results, mlr.pred)
cat("rmse:", mlr.rmse)

# Previsões do modelo de árvore de decisão
tree.pred <- predict(tree.model, newdata = ciclistas.test)
tree.pred <- minmaxdesnorm(tree.pred, ciclistas.norm$vo2_results)

# Cálculo do MAE e RMSE para o modelo de árvore de decisão
tree.mae <- MAE(ciclistas.test$vo2_results, tree.pred)
cat("mae:", tree.mae)
tree.rmse <- RMSE(ciclistas.test$vo2_results, tree.pred)
cat("rmse:", tree.rmse)

# Previsões do modelo de rede neuronal
nn.pred <- compute(nn.model, ciclistas.test)$net.result
nn.pred <- minmaxdesnorm(nn.pred, ciclistas.norm$vo2_results)

# Cálculo do MAE e RMSE para o modelo de rede neuronal
nn.mae <- MAE(ciclistas.test$vo2_results, nn.pred)
cat("mae:", nn.mae)
nn.rmse <- RMSE(ciclistas.test$vo2_results, nn.pred)
cat("rmse:", nn.rmse)

#Exercicio 9
#Para se saber qual teste realizar, é necessário fazer um teste de shapiro
#para verificar se segue uma distribuição normal ou não

#Verificar a normalidade
#H0: Segue uma distribuição normal
#H1: Não segue uma distribuição normal
#α=0.05
res.nn<- nn.pred - ciclistas.test$vo2_results

lillie.mlr <- lillie.test(residuals(mlr.model))
lillie.nn <- lillie.test(res.nn)

lillie.mlr
lillie.nn

#Como ambos o p-value de ambos os testes Shapiro são superiores ao
#nivel de significância, aceita-se a hipóste nula, e conclui-se que ambos os 
#modelos seguem uma distribuição normal.

#Com isto, irá ser preciso recorrer a testes paramétricos, utilizando-se 
#o t test 

#H0:regressão linear multipla = Rede Neuronal
#H1:regressão linear multipla != Rede Neuronal
#α=0.05

v.mlr <- c(mlr.mae, mlr.rmse)
v.nn <- c(nn.mae, nn.rmse)

t.test(v.mlr, v.nn)

#Como o p-value = 0.9027 é superior a 0.05, não podemos rejeitar a hipótese nula,
#não existem evidencias estatistica com o nivel de significancia de 5% que provem que 
#existe diferenças significativas entre a precisão dos dois modelos


#Tópico 4.2
#Exercicio 1
###Árvore de decisão
sample = sample(1:nrow(ciclistas), 0.7 * nrow(ciclistas))
ciclistas.train = ciclistas[sample,]
ciclistas.test = ciclistas[-sample,]

tree.model = rpart(Pro.level ~ . , data = ciclistas.train)
rpart.plot(
  tree.model,
  digits = 3,
  fallen.leaves = TRUE,
  type = 3
)


tree.pred = predict(tree.model, ciclistas.test, type = 'matrix')


m_conf_arvore_dec <- table(ciclistas.test$Pro.level, tree.pred)
acc_arvore_dec <- sum(diag(m_conf_arvore_dec))/sum(m_conf_arvore_dec)
cat("Accuracy: ", acc_arvore_dec * 100, "%") #Accuracy:  1.33333 %



#Rede neuronal

ciclistas_norm = as.data.frame(lapply (ciclistas, minmax))
treino_norm = as.data.frame(lapply (ciclistas.train, minmax))
teste_norm = as.data.frame(lapply (ciclistas.test, minmax))

#criacao da rede com 2 niveis internos de 1 no cada um
nn.model <- neuralnet(Pro.level ~ ., data = treino_norm, hidden = c(1,1))
plot(nn.model)

#Ver qual a accuracy
prevs_modelo_rede <- predict(nn.model, ciclistas.test, type = "class")

m_conf_rede <- table(ciclistas.test$Pro.level,prevs_modelo_rede)

accuracy_rede <- sum(diag(m_conf_rede))/sum(m_conf_rede)
cat("Accuracy: ", accuracy_rede * 100, "%") #Accuracy:  6.3333 %



#K-VIZINHOS MAIS PRÓXIMOS

ncols <- dim(ciclistas.norm)[2] #numero de colunas -> 10

treino_norm <- ciclistas.norm[index, -ncols]
teste_norm <- ciclistas.norm[-index, -ncols]

treino_norm.Pro.Level <- ciclistas.norm[index, ncols]
teste_norm.Pro.Level <- ciclistas.norm[-index, ncols]

k <- c()
accuracy <- c()
for(i in seq (1,50,2)){
  knn.pred <- knn(train = treino_norm , test = teste_norm , cl = treino_norm.Pro.Level , k= i)
  m.conf <- table(teste_norm.Pro.Level, knn.pred)
  accuracy <- c(accuracy, sum(diag(m.conf))/sum(m.conf))
  k <- c(k,i)
}

k_max <- k[which.max(accuracy)]
k_max # 11
plot(k, accuracy)
knn.pred <- knn(train = treino_norm , test = teste_norm , cl = treino_norm.Pro.Level , k_max)

m_conf_k_viz <- table(ciclistas.test$Pro.level,knn.pred)

acc_k_viz <- sum(diag(m_conf_k_viz))/sum(m_conf_k_viz)
cat("Accuracy: ", acc_k_viz * 100, "%") #Accuracy:4.666667  %

##OS MELHORES MÈTODOS SÃO A ÁRVORE DE DECISÃO E O  K-VIZINHOS MAIS PRÓXIMOS

##a
kf <- 10
folds <- sample(1: kf, nrow (ciclistas), replace = TRUE)
cv.error <- matrix(nrow = kf , ncol = 2)


k <- k_max #39
for (i in 1: kf){
  train.cv <- ciclistas.norm[folds != i,]
  test.cv <- ciclistas.norm[folds == i,]
  
  ncols <- dim(train.cv)[2]
  
  train.Ativo <- ciclistas.norm[folds != i, ncols ]
  tst.Ativo <- ciclistas.norm[folds == i, ncols]
  
  knn.pred <- knn (train = train.cv[, -ncols], test = test.cv[,-ncols], cl= train.Ativo , k= k_max)
  m.conf <- table(tst.Ativo,knn.pred)
  
  nn.model <- neuralnet(Pro.level ~ ., data = train.cv)
  prevs_modelo_rede <- predict(nn.model, test.cv, type = "class")
  
  m_conf_rede <- table(tst.Ativo,prevs_modelo_rede)
  
  cv.error [i, ] <- c (sum (diag (m.conf))/ sum (m.conf), sum(diag(m_conf_rede))/sum(m_conf_rede))
}
apply(cv.error, 2, mean) #0.043645815(rede) e 0.009311413(knn)
apply(cv.error, 2, sd) #0.022955728 (rede) e 0.008410984(knn)


#c.

#H0:  nao há diferenças significativas vs H1: há diferenças significativas 

teste <- t.test(cv.error[1,], cv.error[2,])
teste$p.value # 0.6847019
#Como o p = 0.6847019 e , portanto, é superior a 0.05,  não podemos rejeitar a hipotese nula.
#Não existem evidências estatísticas, ao nível de significância de 5%, que provem que existem 
#diferencas significativas entre a accuracy dos dois modelos


#d.

#Rede neuronal
print(m_conf_rede)

#Accuracy
accuracy_rede #0.16

#Precision = Sensitivity = TP/(TP+FP)
TP_rede <- m_conf_rede[1,1] #0
FP_rede <- m_conf_rede[2,1] #0

precision_rede <- (TP_rede/(TP_rede+FP_rede))
precision_rede #NaN

#Recall 0 Specificity = TP/(TP+FN)
FN_rede <- m_conf_rede[1,2]
recall_rede <-(TP_rede/(TP_rede+FP_rede))
recall_rede #NaN

#F1 = ((2*precision*recall)/(precision+recall))
F1_rede <- ((2*precision_rede*recall_rede)/(precision_rede+recall_rede)) 
F1_rede #NaN

#K-vizinhos-mais-proximos

print(m_conf_k_viz)



#Accuracy
acc_k_viz #0.04

#Precision = Sensitivity = TP/(TP+FP)
TP_k_viz <- m_conf_k_viz[1,1] #2
FP_k_viz <- m_conf_k_viz[2,1] #0

precision_k_viz <- (TP_k_viz/(TP_k_viz+FP_k_viz))
precision_k_viz #0.2222222

#Recall = Specificity = TP/(TP+FN)
FN_k_viz <- m_conf_k_viz[1,2]
recall_k_viz <-(TP_k_viz/(TP_k_viz+FN_k_viz))
recall_k_viz #0.2857143

#F1 = ((2*precision*recall)/(precision+recall))
F1_k_viz <- ((2*precision_k_viz*recall_k_viz)/(precision_k_viz+recall_k_viz)) 
F1_k_viz #0.25

#RESUMO
accuracy_rede #0.16
acc_k_viz #0.04

precision_rede #NaN
precision_k_viz #0.2222222

recall_rede #NaN
recall_k_viz #0.2857143

F1_rede #NaN
F1_k_viz #0.25

#O modelo que apresentou melhor desempenho de acordo com os criterios foi a Rede Neuronal


#Exercicio 2 
#Árvore de Decisão

set.seed(123)
index2 <- sample(1:nrow(ciclistas.norm), 0.7 * nrow(ciclistas.norm))
ciclistas.train <- ciclistas.norm[index2, -11]
ciclistas.test <- ciclistas.norm[-index2, -11]
summary(ciclistas.train)
train_labels <- ciclistas.norm[index2, "Winter.Training.Camp"]
tst_labels <- ciclistas.norm[-index2, "Winter.Training.Camp"]
tree.model2 <- rpart(Winter.Training.Camp ~ ., method = "class", data = ciclistas.train)
# Visualizar a árvore de classificação
rpart.plot(tree.model2, digits = 3)
# Previsões discretas
rpart.pred <- predict(tree.model2, ciclistas.test, type = "class")
rpart.pred
# Matriz de confusão da árvore de decisão
m_conf_arvore <- table(tst_labels, rpart.pred)
print(m_conf_arvore)
# Cálculo da precisão (accuracy) da árvore de decisão
accuracy_arvore <- sum(diag(m_conf_arvore)) / sum(m_conf_arvore)
accuracy_arvore # 0.6966667


# Rede Neural
model2 <- as.formula("Winter.Training.Camp ~ gender + Team + Background + Pro.level + altitude_results + vo2_results + hr_results + Continent + Age")
numnodes2 <- 1
nn.model2 <- neuralnet(model2, data = ciclistas.train, hidden = numnodes2)
# Visualizar a estrutura da rede neural
plot(nn.model2)
# Previsões da rede neural
prevs_modelo_rede <- compute(nn.model2, ciclistas.test[, -which(names(ciclistas.test) == "Winter.Training.Camp")])$net.result
# Arredondar as previsões para valores binários
rede.pred <- ifelse(prevs_modelo_rede > 0.5, 1, 0)
# Matriz de confusão da rede neural
m_conf_rede <- table(tst_labels, rede.pred)
# Cálculo da precisão (accuracy) da rede neural
accuracy_rede <- mean(rede.pred == ciclistas.test$Winter.Training.Camp)
accuracy_rede # 0.7066667



#a) Calcular média e o desvio padrão usando o método k-fold cross validation
data <- ciclistas.norm
k <- 10
folds <- sample(1:k, nrow(data), replace = TRUE)
numnodes <- 1 

# Matriz para armazenar as precisões
accuracy <- matrix(nrow = k, ncol = 2)

# Loop k-fold cross validation
for (i in 1:k) {
  train.cv <- data[folds != i, ]
  test.cv <- data[folds == i, ]
  
  train.Camp <- data[folds != i, "Winter.Training.Camp"]
  tst.Camp <- data[folds == i, "Winter.Training.Camp"]
  
  # Modelo de rede neural
  nn.model <- neuralnet(model2, data = train.cv, hidden = numnodes)
  prevs_modelo_rede <- compute(nn.model, test.cv[, -which(names(test.cv) == "Winter.Training.Camp")])$net.result
  nn.pred <- ifelse(prevs_modelo_rede > 0.5, 1, 0)
  
  # Matriz de confusão da rede neural
  cfmatnn <- table(tst.Camp, nn.pred)
  
  # Modelo de árvore de decisão
  tree.model <- rpart(Winter.Training.Camp ~ ., method = "class", data = train.cv)
  rpart.pred <- predict(tree.model, test.cv, type = "class")
  
  # Matriz de confusão da árvore de decisão
  cfmatrpart <- table(tst.Camp, rpart.pred)
  
  # Armazenar as precisões na matriz accuracy
  accuracy[i, ] <- c(sum(diag(cfmatnn)) / sum(cfmatnn),
                     sum(diag(cfmatrpart)) / sum(cfmatrpart))
}

# Precisão média
mean_accuracy <- apply(accuracy, 2, mean)
cat("Precisão média (nn e árvore):", mean_accuracy, "\n") # rede neuronal:0.7323471  arvore:0.7015413

# Desvio padrão
sd_accuracy <- apply(accuracy, 2, sd)
cat("Desvio padrão (nn e árvore):", sd_accuracy, "\n") # rede neuronal:0.0398576  arvore:0.04000683


#b) Verificar se existe diferença significativa no desempenho dos dois melhores modelos obtidos

ciclistas.nn<- rede.pred - ciclistas.test$Winter.Training.Camp
ciclistas.tree <- rpart.pred 

summary(rpart.pred)

# Verificar normalidade dos resíduos do modelo de rede neural
lillie.nn <- lillie.test(ciclistas.nn)
lillie.nn
#Lilliefors (Kolmogorov-Smirnov) normality test
#D = 0.38306, p-value < 2.2e-16

# Teste de hipótese para comparar a diferença nas precisões
test <- wilcox.test(accuracy[, 1], accuracy[, 2])
test
#Wilcoxon rank sum test with continuity correction
#W = 67, p-value = 0.2176


#c) Comparar resultados dos modelos

# Cálculo das métricas de avaliação para o modelo de árvore de decisão
TP_arvore <- m_conf_arvore[1, 1]
FP_arvore <- m_conf_arvore[2, 1]
FN_arvore <- m_conf_arvore[1, 2]
accuracy_arvore <- sum(diag(m_conf_arvore)) / sum(m_conf_arvore)
precision_arvore <- TP_arvore / (TP_arvore + FP_arvore)
recall_arvore <- TP_arvore / (TP_arvore + FN_arvore)
F1_arvore <- (2 * precision_arvore * recall_arvore) / (precision_arvore + recall_arvore)

# Cálculo das métricas de avaliação para o modelo de rede neural
TP_rede <- m_conf_rede[1, 1]
FP_rede <- m_conf_rede[2, 1]
FN_rede <- m_conf_rede[1, 2]
accuracy_rede <- sum(diag(m_conf_rede)) / sum(m_conf_rede)
precision_rede <- TP_rede / (TP_rede + FP_rede)
recall_rede <- TP_rede / (TP_rede + FN_rede)
F1_rede <- (2 * precision_rede * recall_rede) / (precision_rede + recall_rede)

# Resultados Finais
accuracy_arvore # 0.7066667
accuracy_rede # 0.7233333

precision_arvore # 0.7
precision_rede # 0.7268722

recall_arvore # 0.9162304
recall_rede # 0.8638743

F1_arvore # 0.7936508
F1_rede # 0.7894737



#Exercicio 3
#a)


# Definir o número de folds para o k-fold cross-validation
k <- 10

# Criar os folds para o cross-validation
folds <- createFolds(ciclistas.norm$gender, k = k)

# Definir as métricas de avaliação
metric <- "Accuracy"

# Vetor para armazenar as taxas de acerto (accuracy)
accuracy_rede <- numeric(k)
accuracy_knn <- numeric(k)

# Loop para realizar o k-fold cross-validation
for (i in 1:k) {
  # Dividir os dados em conjunto de treinamento e teste para o fold atual
  train_data <- ciclistas.norm[-folds[[i]], ]
  test_data <- ciclistas.norm[folds[[i]], ]
  
  # Treinar o modelo de rede neuronal
  nn_model <- neuralnet(model, data = train_data, hidden = numnodes)
  
  # Fazer previsões usando o modelo de rede neuronal
  nn_pred <- compute(nn_model, test_data)$net.result
  nn_pred <- minmaxdesnorm(nn_pred, ciclistas.norm$gender)
  
  # Calcular a taxa de acerto para o modelo de rede neuronal
  accuracy_rede[i] <- sum(nn_pred == test_data$gender) / length(test_data$gender)
  
  # Converter a variável de destino em um fator de dois níveis para o modelo de KNN
  train_data$gender <- as.factor(train_data$gender)
  
  # Treinar o modelo de KNN
  knn_model <- train(gender ~ ., data = train_data, method = "knn")
  
  # Fazer previsões usando o modelo de KNN
  knn_pred <- predict(knn_model, newdata = test_data)
  
  # Calcular a taxa de acerto para o modelo de KNN
  accuracy_knn[i] <- sum(knn_pred == test_data$gender) / length(test_data$gender)
}

# Calcular a média e o desvio padrão da taxa de acerto para ambos os modelos
mean_accuracy_rede <- mean(accuracy_rede)
sd_accuracy_rede <- sd(accuracy_rede)

mean_accuracy_knn <- mean(accuracy_knn)
sd_accuracy_knn <- sd(accuracy_knn)

# Exibir os resultados
cat("Rede Neuronal - Taxa de Acerto Média:", mean_accuracy_rede, "\n") # 0
cat("Rede Neuronal - Desvio Padrão da Taxa de Acerto:", sd_accuracy_rede, "\n") # 0

cat("KNN - Taxa de Acerto Média:", mean_accuracy_knn, "\n") # 0.634 
cat("KNN - Desvio Padrão da Taxa de Acerto:", sd_accuracy_knn, "\n") # 0.04087923 



#b) Verificar se existe diferença significativa no desempenho dos dois melhores modelos obtidos
# Vetor para armazenar as diferenças nas taxas de acerto
diff_accuracy <- accuracy_rede - accuracy_knn

# Verificar se há empates nas diferenças
if (any(diff_accuracy == 0)) {
  # Adicionar pequeno ruído às diferenças para evitar empates
  diff_accuracy <- diff_accuracy + runif(length(diff_accuracy), min = 1e-8, max = 1e-7)
}
# Realizar o teste de Wilcoxon pareado
test <- wilcox.test(accuracy_rede, accuracy_knn, paired = TRUE)

# Verificar o resultado do teste
p_value <- test$p.value
p_value # 0.005825024
# Exibir o resultado
if (p_value < 0.05) {
  cat("Existe diferença significativa no desempenho dos modelos (p < 0.05)")
} else {
  cat("Não existe diferença significativa no desempenho dos modelos (p >= 0.05)")
}


#c) Comparar os resultados dos modelos de acordo com os critérios: Accuracy, Sensitivity, Specificity e F1

#Organizar informação para criar matrix de confusão
model2 <- as.formula("Winter.Training.Camp ~ gender + Team + Background + Pro.level + altitude_results + vo2_results + hr_results + Continent + Age")
numnodes2 <- 1
nn.model2 <- neuralnet(model2, data = ciclistas.train, hidden = numnodes2)
# Visualizar a estrutura da rede neuronal
plot(nn.model2)
# Previsões da rede neuronal
prevs_modelo_rede <- compute(nn.model2, ciclistas.test[, -which(names(ciclistas.test) == "gender")])$net.result
# Arredondar as previsões para valores binários
rede.pred <- ifelse(prevs_modelo_rede > 0.5, 1, 0)
# Matriz de confusão da rede neuronal
m_conf_rede <- table(tst_labels, rede.pred)


accuracy_rede <- (TP_rede + TN_rede) / (TP_rede + FP_rede + TN_rede + FN_rede)
sensitivity_rede <- TP_rede / (TP_rede + FN_rede)
specificity_rede <- TN_rede / (TN_rede + FP_rede)
F1_rede <- (2 * TP_rede) / (2 * TP_rede + FP_rede + FN_rede)

# Calcular as métricas para o modelo de KNN
TP_knn <- sum(m_conf_rede[1, 1])
FP_knn <- sum(m_conf_rede[2, 1])
TN_knn <- sum(m_conf_rede[2, 2])
FN_knn <- sum(m_conf_rede[1, 2])

accuracy_knn <- (TP_knn + TN_knn) / (TP_knn + FP_knn + TN_knn + FN_knn)
sensitivity_knn <- TP_knn / (TP_knn + FN_knn)
specificity_knn <- TN_knn / (TN_knn + FP_knn)
F1_knn <- (2 * TP_knn) / (2 * TP_knn + FP_knn + FN_knn)

# Comparar os resultados dos modelos
results <- data.frame(Modelo = c("Rede Neuronal", "KNN"),
                      Accuracy = c(accuracy_rede, accuracy_knn),
                      Sensitivity = c(sensitivity_rede, sensitivity_knn),
                      Specificity = c(specificity_rede, specificity_knn),
                      F1 = c(F1_rede, F1_knn))

# Exibir os resultados
results
#       Modelo  Accuracy Sensitivity Specificity        F1
# 1 Rede Neuronal 0.7066667   0.8638743   0.4311927 0.7894737
# 2         KNN 0.6566667   0.7068063   0.5688073 0.7238606
