#Análise Exploratória de Dados

#Medidas descritivas
#Medidas de localização

#Média

#crio um vetor de dados com o nome amostra1, pode ser <- ou =
amostra1 <- c(4,6,7,8,9,10)
mean(amostra1)

#Outro vetor de dados com o nome amostra2 com valores não definidos (NA)
amostra2 <- c(-2,NA,1,6,8,NA,-4)

#Faz a média mas não considera os NA
mean(amostra2,na.rm = TRUE)


#Mediana
#Se o número de elementos(n=13) for impar então pego o do meio(7)
#Se n for par vou pegar nos do meio e faço a média
#

#Registo outro vetor de dados
dados <- c(3,5,7,15,16,1,17,24,0,-1,5,2,9)
median(dados)

#Quantis ou Quartis

############################################################
#Dá todos
quantile(dados)

###########################################################
#Dá os que queremos
quantile(dados,c(0.25,0.5))

############################################################
#Dá só um
quantile(dados,0.25)

#Medidas de Dispersão/variabilidade
#Amplitude (total)

max(dados) - min(dados)
##      ou
diff(range(dados))

#Intervalo inter quartil
IQR(dados)

#Variância
dados=c(0.9,0.8,0.3,1.1,1.2,1.3,0.7,0.5,-0.1,0,-0.7)
var(dados)

#Desvio padrão
sd(dados)
# ou
#sqrt(var(dados)) na console


#Medidas de Forma
install.packages("e1071")
library(e1071)

#100 números random, com média 15 e desvio padrão 5
set.seed(33)
dados <- rnorm(100,mean=15,sd=5)
mean(dados)

##Assimetria
skewness(dados)
hist(dados)

set.seed(55)
dados <- rchisq(5500,0.9,df=5)
skewness(dados)
hist(dados)


set.seed(66)
data <- rchisq(88,0,3)
kurtosis(data)

amostra <- c(rep('L',4),rep('C',23),rep('D',16),rep('O',7))
data <- factor(amostra ,c('L','C','D','O'),
               labels=c("Leitura", "Cinema","Desporto", "Outros"))
tabela <- table(data); tabela

#Frequências relativas
tabela.rel <- prop.table(tabela);tabela.rel



#Gráfico de barras
barplot(tabela)

#Gráfico Circular
pie(tabela)


#Caixa de bigodes
dados1=rchisq(100,.2, df=7)
dados2=rchisq(100,5, df=7)
classes=c("dados1","dados2")
boxplot(dados1,dados2,names=classes,col=c(4,2))





