
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                 Transformação / criação de variáveis                  %
#                   Prof. Abraham Laredo Sicsu                          %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                      geração de dummies                               %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#gerando apenas uma variavel
library(dummies)
newband=dummy(CAMBRIDGE$BANDEIRA)  #CUIDADO: k categorias --> k dummies
head(newband)
head(newband[,-1])

#ou---------------------
band_beta=ifelse(CAMBRIDGE$BANDEIRA=="beta", 1 , 0)
band_gama=ifelse(CAMBRIDGE$BANDEIRA=="gama", 1 , 0)
CAMBRIDGE$BANDEIRA=as.character(CAMBRIDGE$BANDEIRA) #caso contrario sai o número 1,2 ou 3
head(cbind(CAMBRIDGE$BANDEIRA,  band_beta,band_gama))
CAMBRIDGE$BANDEIR=NULL


#gerando todas as vars dummies de uma base de dados
newcamb=model.matrix(data=CAMBRIDGE, ~.)
colnames(newcamb)
newcamb=newcamb[,-1] 

#=========================================================================
#   Exercicio: abrir o arquivo MOBILE[,3:11] fornecido pelo Professor
#        gerar as dummies de NIVDES utlizando o comando ifelse
#        gerar as dummies de SISGOV utlizando o comando ifelse (cuidado!!!)
#        gerar um novo df ("mob-dum") com  as dummies de todas as quali
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            DISCRETIZAÇÃO DE VARIAVEIS QUANTITATIVAS                   %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install.packages("arules")
library(arules)
#discretização em 5 classes de mesma frequencia
dd$kidade=discretize(dd$IDADE, method = "frequency", breaks =5 )
CrossTable(dd$kidade, dd$STATUS, prop.c = F,prop.chisq = F,prop.t = F)

#discretização em categorias com lilites predeterminados
limites=c(0,40,50,70,90) #importante:colocar limites inferior e superior
dd$kkidade=discretize(dd$IDADE, method = "fixed", breaks =limites )
CrossTable(dd$kkidade, dd$STATUS, prop.c = F,prop.chisq = F,prop.t = F)
#===========================================================================

#exercicio: no db MOBILE gere a var MOB_POP= MOBPHONE / POP
#   discretize essa var em 4 categorias de mesma frequencia --> gere KMOB_POP
#   analise a tabela KMOB_POP (linha) po SISGOV (coluna)
#   faça o barplot correspondente

#exercicio: no db MOBILE gere a var POP
#   discretize essa var em 4 categorias deforma que a 1a tenha 30% dos paises, a segunda 40%, e a terceira 30%
#   sugestão: para determinar os limites utilize quantile(dd$IDADE, probs = c(.30,.70,1), na.rm = T)
#   analise a tabela dessa nova var por NIVDES
#   faça o barplot correspondente
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            PADRONIZAÇÃO DE VARIAVEIS QUANTITATIVAS                   %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Vamos considerar a variável RENDA de camb.completo
#vamos padronizar subtraido a média e dividindo pelo desvio padrão

summary(camb.completo$RENDA)
z_renda=scale(camb.completo$RENDA)
summary(z_renda); sd(z_renda)

#Agora vamos padroinizar subtraindo a mediana e dividindo pela amplitude
mediana=median(camb.completo$RENDA); mediana
minimo=min(camb.completo$RENDA)
maximo=max(camb.completo$RENDA)

z_renda=scale(camb.completo$RENDA, center = mediana, scale=maximo-minimo)
summary(z_renda); sd(z_renda)

#===========================================================================

#exercicio: padronize a variavel RENDA de forma que os valores variem entre 0 e 1

#exercicio: no db MOBILE gere a var POP
#           padronize com média =0 e desvio padraõ =1
#           padronize de forma que os valores variem entre -1 e 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

