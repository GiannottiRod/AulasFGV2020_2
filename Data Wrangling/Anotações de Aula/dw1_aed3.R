
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                            VARIAVEIS QUANTITATIVAS                                %
#                          Prof. Abraham Laredo Sicsu                               %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         DISTRIBUIÇÃO DE FREQUENCIAS E MEDIDAS                     %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#geral
summary(dd)


# vamos analisar IDADE
summary(dd$IDADE)  
which(is.na(dd$IDADE))                   #linhas com MV
# se quisermos eliminar esses 3 MV --> dd=dd[-c(2235,2424,2713),]
mean(dd$IDADE, na.rm = T)
quantile(dd$IDADE, probs =seq(0,1,.05), na.rm=T) 

#install.packages("psych")
library(psych)
 describe(dd$IDADE) #mais detalhado que summary
#mad: median absolute deviation (from the median).
 
head(sort(dd$IDADE, decreasing = T),20)  #mostra os 20 maiores
head(sort(dd$IDADE, decreasing = F),20)  #mostra os 20 menores; podiamos usar tail depois de sort


hh=hist(dd$IDADE, col="lightblue", bor="red");grid(col=2)
hh$mids
hh$counts
cbind(hh$mids, hh$counts)
hh2=hist(dd$IDADE, col="yellow", bor="blue",  breaks = 10)  #R faz aproximadamente 10 quebras

#Ajuste dos eixos, caso desejado
hist(dd$IDADE, axes=F, col = "salmon")
axis(1, at = seq(0, 200, 10))
axis(2, at = seq(0, 2000, 100))
grid (lty = 2, col = 4)

bb=boxplot(dd$IDADE, col=4, main="idade"); grid(col = 1)
bb$stats
bb$out
which(dd$IDADE>=87) 

summary(dd$TMPRSD)
hh=hist(dd$TMPRSD, col="blue", bor="red", breaks = 20)
cbind(hh$mids, hh$counts)
boxplot(dd$TMPRSD)

#vamos tentar corrigir assimetria
dd$lntmp=log(dd$TMPRSD+1) #em geral, orrige assimetria substancial aa direita
dd$rztmp=sqrt(dd$TMPRSD)  #em geral, corrige assimetria moderada aa direita

par(mfrow=c(1,3)) #opção graficos; alternativa layout(matrix(1:3, ncol=3)) 
boxplot(dd$TMPRSD, main="tmprsd", col=2)
boxplot(dd$lntmp, main="log de tmprsd", col=3)
boxplot(dd$rztmp, main="raiz de tmprsd", col=4)
hist(dd$TMPRSD, main="tmprsd", col=2)
hist(dd$lntmp, main="log de tmprsd", col=3)
hist(dd$rztmp, main="raiz de tmprsd", col=4)


#==================================================================================
#EXERCICIO C1
#exercicio: analise a var IALFAB do arquivo MOBILE: 
            #summary, decis, histograma, boxplot, 
            # quinze maiores e quinze menores valores
            #se houver, identifique paises outliers
            #redução de assimetria sem eliminar os outliers  
#EXERCICIO C2
#exercicio: analise a var GROSSINC do arquivo MOBILE 
            #summary, decis, histograma, boxplot, 
            #se houver, identifique paises outliers
            #redução de assimetria sem eliminar os outliers  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mob=MOBILE
quantile(mob$IALFAB, probs = seq(0, 1, length = 11))

hist(mob$IALFAB)
boxplot(mob$IALFAB)

library(tidyverse)

mob %>%
  arrange(IALFAB) %>%
  head(15)


mob %>%
  arrange(desc(IALFAB)) %>%
  head(15)

which(mob$IALFAB<0.8)

subset(mob,mob$IALFAB<0.8)

mob[mob$IALFAB<0.8,]

boxplot((mob$IALFAB)^20)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          REMOVENDO OUTLIERS                                       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bb=boxplot(dd$RNDTOT, col=3, main="rentot"); grid(col=1)
which(dd$RNDTOT>15000)
dd2=dd[-1740,]  
bb=boxplot(dd2$RNDTOT, col=3, main="rentot"); grid(col=1)

#removendo o novo db dd2 criado apenas para analise 
rm(dd2)

#===================================================================================
#Exercicio:  considere a var POP do arquivo MOBILE
      #identifique eventuais outliers e remova-os gerando um novo arquivo
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boxplot(mob$POP)
mob2=mob[mob$POP<1000000,]


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#          ANALISANDO UMA VARIAVEL QUANTI by CATEGORIA DE UMA QUALI                 %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boxplot(dd$IDADE~dd$nuresid, col=rainbow(4), main="IDADE por tipo de RESID")
boxplot(log(dd$IDADE)~dd$nuresid, col=rainbow(11), main="logIDADE por tipo de RESID"); grid(col=1)

# o WARNING  está informando que temos algum log(0)
boxplot(log(dd$IDADE+18)~dd$nuresid, col=rainbow(11), main="logIDADE por tipo de RESID"); grid(col=1)
which(log(dd$IDADE+18)<3) 
dd$IDADE[2365]
boxplot(log(dd$IDADE[dd$IDADE>0])~dd$nuresid[dd$IDADE>0], col=rainbow(11), main="logIDADE por tipo de RESID"); grid(col=1)

#grafico mostrando IDADE por STATUS ("preferimos o boxplot")
cores=ifelse(dd$STATUS=="mau",1,2) #define uma cor para cada categoria
plot(dd$IDADE,type="n")
text(dd$IDADE, labels=c("*"),  col = cores)
# adiante veremos forma mais elegante com ggplot2

library(psych)
describeBy(dd$IDADE,dd$STATUS) #medidas por categoria

aggregate(dd$IDADE, list(dd$STATUS), median , na.rm = T)
# poderiamos utilizar mean, max, min, .....

#===================================================================================
#exercicio C3
#exercício; considere o db MOBILE
  # analise a var TMPRSD  pela var nuredsid
  # boxplot, medidas descritivas
  # comente os resaultados
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ds=DirtyShop
ds$nuresid=ds$RESID      
ds$nuresid[ds$RESID =="pROP"]="PROP"
ds$nuresid[is.na(ds$nuresid)] =  "MV"
boxplot(ds$TMPRSD~ds$nuresid)


library(psych)
describeBy(ds$TMPRSD,ds$nuresid)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                     QUANTITATIVA x QUANTITATIVA                                   %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(dd$IDADE, dd$RNDTOT, pch=19, col=4)

plot(dd$IDADE, dd$RNDTOT[dd$RNDTOT<15000], pch=19, col=4)
plot(dd$IDADE[dd$RNDTOT<15000], dd$RNDTOT[dd$RNDTOT<15000], pch=19, col=4)

plot(dd$IDADE , dd$TMPRSD, pch=19, col=4)
cores=ifelse(dd$STATUS=="mau",1,2) #define uma cor para cada categoria
plot(dd$IDADE , dd$TMPRSD, type = "n")
plot(dd$IDADE , dd$TMPRSD, pch=19, col=cores)

reg=lm(data=dd, TMPRSD~IDADE ) # calcula a reta de regressao
abline(reg, col=4, lwd=8, lty=2) # marca a reta de regressao no grafico


cor(dd$IDADE, dd$RNDTOT)
ddx=na.omit(dd) # novo arquivo sem MV
cor(ddx$IDADE, ddx$RNDTOT)
cor(ddx$IDADE[ddx$RNDTOT<15000], ddx$RNDTOT[ddx$RNDTOT<15000])
#note a influencia de um outlier!!!
rm(ddx) #removendo arquivo que nao será utilizado
#===================================================================================
#exercício; considere o db MOBILE
    # plote IDH (x) x IALFAB (y) , analise e comente.
    #calcule a correlação entre essas vars 
    # façao grafico diferenciando a cor pelo NIVDES

    # plote POP (x) x IDADEMED (y) , analise e comente.
    #calcule a correlação entre essas vars, elimine posteriormente eventuais ouliers e comente
    # façao grafico diferenciando a cor pelo NIVDES

library(ggplot2)
ggplot(data = mob) + 
  geom_point(mapping = aes(x = IDH, y = IALFAB,color=NIVDES))

cor(mob$IDH,mob$IALFAB)

sum(is.na(mob$POP))
mob3=mob[!is.na(mob$POP),]
sum(is.na(mob3$POP))
cor(mob3$POP,mob3$IDADEMED)


library(ggplot2)
ggplot(data = mob3) + 
  geom_point(mapping = aes(x = POP, y = IDADEMED, color=NIVDES))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot(mob$IDH)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                               FUNÇÕES aplly                                       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# vamos gerar um db apenas 6 colunas para simplificar a saida para a aula
dd6=dd[,1:6]


apply(dd6, 2 , median )
# problema: pode ser devido aa existencia de MV ?
ddx=na.omit(dd6)
apply(ddx, 2 , median ) 
#problema : existencia de vars quali para a função median
apply(ddx[,c(3,6)], 2 , median ) 

vet=apply(dd6, 2 , max  );vet   
vet=apply(dd6, 2 , max , na.rm=T);vet   

# função tapply --> mesmo objetivo, porem permite agrupar por categoria
#                    uma variavel de cada vez
colnames(ddx)
tapply(ddx$IDADE, ddx$RESID, median )  
tapply(ddx$TMPRSD, ddx$RESID, range) 

# extra:   sapply(dd6, max, na.rm=T) ; lapply ; não precisamos por enquanto

#===================================================================================
#exercício: considere o db MOBILE
      # #verifique se há MV e remova-os
      # calule usando apply  : média, mediana e as ampliturdes de POP
      # calcule as medianas de IDH e de IALFAB por NIVDES utilizando tapply
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mob=MOBILE[,1:11]


sum(is.na(mob))

mob4 <- mob %>%
  drop_na()

pop=as.matrix(mob4$POP)
apply(pop,2,mean)
apply(pop,2,median)
apply(pop,2,range)

tapply(mob4$IDH,mob4$NIVDES,median)
tapply(mob4$IALFAB,mob4$NIVDES,median)
