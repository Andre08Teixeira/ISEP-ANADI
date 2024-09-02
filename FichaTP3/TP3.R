#TP3

#1
#Tn variável aleatória que representa o nº de oficiais em 25 que pontua mais que 50 no teste
#Assumindo H0 verdadeira, ou seja, p=0,45, temos:

#Tn ~ Bi (n=25,p=0,45)

#A pergunta colocada indica as hipoteses(alfa=0.05): 
#                    H0: p=0.45     vs Hi:p> 0.45

#Teste unilateral à direita, logo queremos P(Tn>13 |p0=0.45)
sum(dbinom(13:25,25,0.45))

#ou
pbinom(12,25,prob=0.45,lower.tail = F)

#ou ainda:
test1<-binom.test(13,25,p=0.45,alternative = "greater",conf.level = 0.95)
test1$p.value

#Ou seja, como p=0.306>alfa, não se rejeita H0. Não há evidências de que os oficiais do exército
#reajam melhor sobre pressão do que a população em geral


#2
#Foram selecionadas 19 crianças
#15 crianças escolheram o balde vermelho
test2<-binom.test(15,19,alternative = "greater",conf.level = 0.95)
test2$p.value

#Ou seja, como p=0.0096, rejeita-se H0.
´

#3

#Partido P >30%?
#2000 eleitores no total
#700 escolheram partido P

p_est <- 700/2000
2000*p_est*(1-p_est) #np(1-p) > 9 

#Optamos por usar o teste binomial aproxiamado("prop.test")
test3<-prop.test(700,2000,p=0.3,alternative = "greater") #95% é o default
test3$p.value

#Como p=6e -07 < alfa rejeita-se H0. Assume-se que  o partido irá ter uma votação de 30%
#com um nível de confiança de 5%


#Ex04 Importar Dataset
