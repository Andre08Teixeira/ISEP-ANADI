# TP5 - Correlação e Regressão Linear

#| Ex1)           
anunc <- c(8,12,16,8,8,8,27,12,8,16)
medid <- c(4.8, 5, 17.5, 6.4, 6.7, 4.3, 20.3, 8, 3.5, 6.3)

#H0:?
cor.test(anunc,medid, method ="kendall")



# Conclusão: O coefeciente de Kendall foi de 0.597. Como p=0.026 <0.05= alfa, rejeita-se H0.
#Há evidências de uma relação de ordem significativa entre o tempo anunciado e o medido.

#| Ex2)   Spearman (rho)
View(Notas)
colnames(Notas) <- c("Estat","Calc")

#Converter notas Calc e Estat em numérico (Calc_n e Estat_n)
niveis_Estat <- c("Excelente","Muito Bom","Bom","Suficiente","Insuficiente","Mau")
codigos_Estat<-1:6

niveis_Calc<-c("A","B","C","D","E","F")
codigos_Calc<-1:6

Notas$Estat_n <- codigos_Estat[match(Notas$Estat,niveis_Estat)]
Notas$Calc_n  <- codigos_Calc[match(Notas$Calc,niveis_Calc)]

# H0:? rho <= 0 vs H1: rho > 0 || H0: rho = 0 vs H1: rho >0
cor.test(Notas$Estat_n,Notas$Calc_n, alternative = "greater", method = "spearman")

# Conclusão: O coeficiente de Spearman foi de 0.455, como p=0.011<0.05=alfa, rejeita-se H0.
#Há evidências de uma relação de ordem significativa e positiva entre as notas de cálculo e as
# de estatística.


#| Ex 3)  TPC


#| Ex 4)
library(readr)
View(fang_data)

library(Hmisc)
rcorr(as.matrix(fang_data[2:5]),type="pearson")    

# Os dados são continuos, mas serão normais?
library (nortest)
lillie.test(fang_data$Facebook)

hist(fang_data$Facebook)
hist(fang_data$Amazon)
hist(fang_data$Netflix)
hist(fang_data$Google)

#Por isso o mais adequado será o coeficiente de Spearman
rcorr(as.matrix(fang_data[2:5]), type="spearman")

#Diferenças entre os coefs? 
#Conclusão
#Apenas a correlação de ordem entre a Amazon e o FB é positiva = 0.92 (p<0.001)
#A correlação mais fraca é de -0.01(não sign: p =0.7121) entre Google e Amazon.
#As restantes são todas negativas e significativas (p<0.001), variando entre -0.27
#(FB com Google) e -0.46 (Netflix com FB e Amazon).

# Ex 5) 
xi <- c(21,24,32,47,50,59,68,74,62,50,41,30)
yi <- c(185.79, 214.47, 288.03, 424.84, 454.58, 539.03, 621.55, 675.06, 562.03, 452.93, 369.95, 273.98)
# 5.a)
plot(xi,yi)

# a dispersão dos pontos indica a existência de uma relação linear,pois eles estão distribuídos
#sobre uma reta. A relação é positiva, pois a reta tem declive positivo


# 5.b) 
reg1 <- lm (yi ~ xi)

summary (reg1)

reg1$coefficients[1]
reg1$coefficients[2]

#ou:
coef(reg1)
# Equacão do Modelo: 

#Estimar y(40) - Podemos usar o nosso modelo? (RVM)

x0 <- data.frame (xi=40)
predict(reg1,x0)
#ou:



# 5.c)

 

# 5.d) Pressupostos RL:  
# - Homocedasticidade; 
# - Normalidade dos Residuos e média zero, 
# - Independência dos resíduos

#####1? Pressuposto: Homocedasticidade:
yi
fitted(reg1)   
xi_df<- data.frame(xi)
predict(reg1,xi_df)

residuals(reg1)         # = yi-fitted(reg1)


par(mfrow=c(2,1))
plot (fitted(reg1), residuals(reg1), xlab="Val. Ajustados", ylab="Residuos")
abline(h=0)

plot (xi, residuals(reg1), xlab="xi", ylab="Residuos")
abline(h=0)


mx <- median(xi);   mx
var.test(residuals(reg1)[xi>mx], residuals(reg1)[xi<mx])


#NOTA: Para amostras grandes (n>30), assumindo res?duos independentes 
# e com distribui??o normal,pode-se efectuar um teste de Breusch-Pagan
# (fun??o bptest do package lmtest). 
install.packages("lmtest")
library(lmtest)
bptest(formula = yi ~ xi)

#####2? Pressuposto: Normalidade dos Residuos de media zero
dev.off()
qqnorm(residuals(reg1), xlab="Residuos", ylab="Quantis teoricos")
qqline(residuals(reg1))
hist(residuals(reg1))
#H0:
shapiro.test(residuals(reg1))
#Conclusao?

#H0:
t.test(residuals(reg1),mu=0,alternative="two.sided")
#Conclusao?

###### 3? Pressuposto: independencia dos residuos
#H0
library (car)
durbinWatsonTest(reg1)

