
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                           ANALISE VARIAVEIS QUALITATIVAS                             %
#                            Prof. Abraham Laredo Sicsu                                %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                     DISTRIBUICOES DE FREQUENCIAS                                    %%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# variavel RESID  
tt=table(dd$RESID); tt  #nao apresenta frequencia de missing values (MV)
sum(is.na(dd$RESID))

tt=table(dd$RESID, useNA = "ifany"); tt  #  apresenta frequencia de missing values (MV)

 
#criando uma nova variável (corrigida), para que pertença ao arquivo dd
  dd$nuresid=dd$RESID      #sugestão: manter sempre a variavel original original
  dd$nuresid[dd$RESID=="pROP"]="PROP"
  table(dd$nuresid, useNA = "ifany")
  
# CUIDADO: se uma variavel estiver no formato factor, e quisermos um label novo, 
#  transformar as.character antes

# criando a categoria MV para preencher as Missings
  dd$nuresid[is.na(dd$nuresid)] =  "MV"
  tt=table(dd$nuresid, useNA = "ifany");tt

pp=prop.table(tt); pp

ppp= round(pp,3)  ;ppp

barplot(tt, col="yellow", 
        main = "nuresid"); grid(col=1)  #freqs absolutas
barplot(pp, col=11); grid(col=2)   # freqs relativas (proporçoes)
barplot(pp, col=11, ylim=c(0,1), main="nuresid"); grid(col=1)

#importante: aas vezes apesar de quali a variavel vem codificada como 1, 2, 3 
  #Se quisermos transforma em texto:  1=primario,   2=secundario , 3=superior
#                   dados$v1 <- as.factor(dados$v1,
#                   levels = c(1,2,3),
#                   labels = c("primario", "secundario", "superior"))


#===================================================================================
# EXERCICIO B1
#Exericio 1.2.1  considerar o db MOBILE--> analisar e plotar NIVDES

#Exericio 1.2.1  considerar a db MOBILE--> analisar e plotar SISGOV
#                entre na base de dados, veja qual o país que está em branco e corrija
#                COMANDO -->   which(is.na(dd$SISGOV))
#                apresente o gráfico final após a correção

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                              CRUZANDO VARIAVEIS QUALITATIVAS                         %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tt=table(dd$STATUS,dd$nuresid, dnn = c("status","nuresid") ); tt   # neste caso a virgula representa o "by" no barplot
prop.table(tt)
round(prop.table(tt,1),3) # %linha
barplot(prop.table(tt,1),legend =rownames(tt), beside=T,  col = c(11,18), ylim=c(0,1)  ); grid(col=1)

prop.table(tt,2)  # %coluna
barplot(prop.table(tt,1),legend =rownames(tt), beside=T,  col =rainbow(2), ylim=c(0,1)  ); grid(col=1)
barplot(prop.table(tt,2),legend =rownames(tt), beside=FALSE,  col = rainbow(2), ylim=c(0,1)  ); grid(col=1)

tt=table(dd$nuresid,dd$STATUS ); tt   # neste caso a virgula representa o "by" no barplot
barplot(prop.table(tt,2),legend =rownames(tt), beside=F,  col =rainbow(4), ylim=c(0,1)  ); grid(col=1)

#lembrando: no eixo horizontal vai a variável que está na coluna; as % daslinhas


vv=table(dd$nuresid,dd$STATUS ); vv

#nomeando linhas e colunas
vvv=table(dd$nuresid,dd$STATUS, dnn = c("nuresid","STATUS") ); vvv

prop.table(vvv)
prop.table(vvv,1)
barplot(prop.table(vvv,1),legend =rownames(vvv), beside=T,  col = rainbow(4), ylim=c(0,1)  ); grid(col=1)
prop.table(vvv,2)
barplot(prop.table(vvv,2),legend =rownames(vvv), beside=T,  col = rainbow(4), ylim=c(0,1)  ); grid(col=1)
barplot(prop.table(vvv,2),legend =rownames(vvv), beside=FALSE,  col = rainbow(4), ylim=c(0,1)  ); grid(col=1)

#===========================================================================
# Exercicio B2
#Exercicio: considere os dados MOBILE
      # cruze as vars NIVDES E SISGOV, 
      # analise os graficos NIVDES por SISGOV(eixo X)
      # analise os graficos SISGOV por NIVDES (eixo X)
      #analisando os graficos, qual a relação entre as vars?
      #qual seria seu sistema de governo preferido, com base apenas nesses dados?
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          FORMATANDO A TABELA COM gmodels                             %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install.packages("gmodels")
library(gmodels)

CrossTable(dd$nuresid, dd$STATUS)  #excesso de informação
CrossTable(dd$nuresid, dd$STATUS, prop.chisq = F, prop.t = F)  #deixando apenas %linha e %coluna

#===========================================================================

#Exercicio:    Tabule SISGOV vs NIVDES apresentando apenas a % do total

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                        representação gráfica e análise das tabelas                                        %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tt=table(dd$STATUS,dd$nuresid ); tt   # neste caso a virgula representa o "by" no barplot
A=chisq.test(tt)
A$observed
A$expected
A$ residuals
A
assocplot(t(tt), col=c(11,2) ) #col=2 (red) acima do expected; indica Pearson residuals

