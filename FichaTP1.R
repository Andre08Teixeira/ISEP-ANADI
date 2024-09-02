#TP3
install.packages("readxl")
library(readxl)

getwd()

setwd("C:/Users/ASUS/Desktop/Universidade/3º Ano/2º Semestre/ANADI/TP/TP1")

dias_sem_chuva <- read_excel("PORDATA_N.º-de-dias-sem-chuva.xlsx",range="Quadro!A8:J70")
colnames(dias_sem_chuva)[1] <- "Anos"
colnames(dias_sem_chuva)

boxplot(dias_sem_chuva[2:8],main="Dias sem chuva",ylab="Nº de dias")

dias_sem_chuva$`Castelo Branco`

#1.f)
quantis.CB <- quantile(dias_sem_chuva$`Castelo Branco`)
IQR(dias_sem_chuva$`Castelo Branco`)
quantis.CB

quantis.P <- quantile(dias_sem_chuva$Porto,na.rm=TRUE)
IQR(dias_sem_chuva$Porto,na.rm=TRUE)
quantis.P

#1.g)
tab.P <- table(dias_sem_chuva$Porto);tab.P

#tabela com dados em classes definidas empiricamente (ex5:classes)

tab.P <- table(cut(dias_sem_chuva$Porto,breaks=5))

#tabela com os limites das classes especificados classes definidas empiricamente (ex4:classes)
tab.P_clas <- table(cut(dias_sem_chuva$Porto,breaks=c(150,176,201,226,251)));tab.P_clas #freq. absolutas 


prop.table(tab.P_clas) #fre. relativas

#alternatuiva
tab.P_frel <- tab.P_clas/sum(tab.P_clas);tab.P_frel

#tabela com dados em calsses regra de Struges
nc <- nclass.Sturges(dias_sem_chuva$Porto)
1+3.3*log10(length(dias_sem_chuva$Porto));nc

tab_P_St <- table(cut(dias_sem_chuva$Porto,breaks = nc));tab_P_St


#1.h)
hist.P <- hist(dias_sem_chuva$Porto)

summary(hist.P)
hist.P$breaks
hist.P$counts                       #freq.absolutas
hist.P$counts/sum(hist.P$counts)    #freq. relativas

#1.i) Evolução temporal
#Dados sao series temporais -> usamos funcao ts para colocar dados em objeto de series teporais
dsc.P <- ts(dias_sem_chuva$Porto, start =1960);dsc.P

ts.plot(dsc.P, main="Dias sem chuva", lty=3,ylab="Numero de dias",xlab="Ano")
legend("bottomleft",legend= "Porto",lty=3)
dsc.F <- ts(dias_sem_chuva$Faro, start= 1960)
ts.plot(dsc.P,dsc.F,lty=c(1:2),main= "Dias sem chuva",ylab="Numero de dias",xlab="Ano")
legend("bottomleft",legend = c("Porto","Faro"),lty= c(1:2))



#TP2
install.packages("dplyr")
library(dplyr)


#extrair os dados dos anos de 2010a 2015
dias_sem_chuva_2010_15 <-filter(dias_sem_chuva,Anos>=2010 & Anos<=2015)
View(dias_sem_chuva_2010_15)
