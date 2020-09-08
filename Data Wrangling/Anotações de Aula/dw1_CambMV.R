
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                           MISSING VALUES                                 %
#                      Prof. Abraham Laredo Sicsu                          %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# planilha CAMB 
camb.completo=CAMBRIDGE[,-1]      # todas as colunas completas para comparação futura
sum(is.na(camb.completo))
camb.miss=CAMBRIDGEcomMV[,-1]     # eliminei as vars complete.cases()
str(camb.miss)
summary(camb.miss) #chr nao mostra NA!

#   miss RANDOM --> super importante: transformar chr em factor  e data.frame
#***********************************************************************
#camb.miss$FINANCIA=as.factor(camb.miss$FINANCIA)
#camb.miss$SEXO=as.factor(camb.miss$SEXO)
#camb.miss$BANDEIRA=as.factor(camb.miss$BANDEIRA)
#camb.miss=as.data.frame(camb.miss)
#str(camb.miss)

summary(camb.miss)
sum(is.na(camb.miss))  # numero de celulas vazias
table(camb.miss$SEXO, useNA = 'ifany')
table(camb.miss$BANDEIRA, useNA = 'ifany')

#quem quiser salvar no Excel (costuma dar problema)
library(xlsx)
write.xlsx(camb.miss, "camb_miss.xlsx")
#se desejarmos gerar arquivo csv (e depois transformar em excel)
# write.csv(cab.miss, "xixi.csv")
#***********************************************************************
#Nao fazer
#exercicio:  considere a planilha MOBILE
#     utilizando set.seed(111)  gere a df mobi.miss com 3% de MV
#     gere um arquivo em csv correspondente à planilha mobo.miss
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                           análise descritiva                             %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(VIM)
names(camb.miss)
resumo=summary(aggr(camb.miss, sortVar=T,number=T, cex.axis=.7, prop=F))$combinations 
# prop=T daria em proporção
class(resumo)
resumo
head(resumo,10) 
colnames(camb.miss)
resumo[order(resumo$Percent,decreasing = T),] #ordenação de dataframe

#***********************************************************************
#exercicio D1:  considere a db  mobi.miss
      #         aplique o package VIM para descrever a estrutura de MV
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#           substituindo NA por um valor ou por 'MV'                       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
#só para exemplificar vamos criar a var flag
flag=camb.miss$BANDEIRA
table(flag, useNA = "ifany")
class(flag)
#levels(flag)=c(levels(flag), 'MV') # se for "factor"  precisa este comando
flag[is.na(flag)] <- 'NI'
table(flag)


#***********************************************************************
#exercicio D2:  considere a db  mobi.miss
#            considere uma variável qualitativa que tenha  MV  (se houver)
#            gere a variavel xxx=a essa variavel
#            rode table (xxx)
#            (cuidado, antes verifique se essa variável é factor ou chr)
#            impute o "valor" MV na variavel xxx no lugar do MV
#            verifique o que ocorre com table(xxx)
#            remova a variavel xxx
#
#            considere a variável NIVDES e verifique se tem MV
#            gere a variavek yyy= nivdes e tranforme-a em factor
#            selecione arbitrariamente um país e substitua yyy pela categoria DESCONHECIDO
#            verifique o que ocorre com table(yyy)
#            remova a variavel yyy
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#           Imputando valores com kNN     pacote VIM                       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

library(VIM)
camb.vim=VIM::kNN(camb.miss, k=25,  addRandom = T, weightDist = T)  
#cuidado, kNN  tem que pertencer ao VIM (há outros pacotes com esse nome)
 

original=camb.completo$TEMPO[is.na(camb.miss$TEMPO)]
imputado=camb.vim$TEMPO[is.na(camb.miss$TEMPO)]
cbind(original,imputado,(original-imputado)/original*100)
mean(abs((original-imputado)/original*100))

#***********************************************************************
#exercicio:  considere a db  mobi.miss
#            impute valores com kNN gerando mobi.knn
#            compare os valores originais e os imputados para todas as variáveis
#            comente; compare com os resultados obtidos com missForest
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#           Imputando valores com irmi     pacote VIM                       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
# o  pacote  depende  fortemente de haver relação entre as variaveis. cada
# uma é estimada em função das outras. Se a relação entre as vars nao for satisfatória
# os valores imputados nao serão muito satisfatórios

library(VIM)
camb.irmi=irmi(camb.miss, robust = T)  #cuidado, irmi tem que pertencer ao VIM


original=camb.completo$TEMPO[is.na(camb.miss$TEMPO)]
imputado=camb.irmi$TEMPO[is.na(camb.miss$TEMPO)]
cbind(original,imputado,(original-imputado)/original*100)
mean(abs((original-imputado)/original*100))
#***********************************************************************
#exercicio:  considere a db  mobi.miss
#            impute valores com kNN gerando mobi.irmi
#            compare os valores originais e os imputados para todas as variáveis
#            comente; compare com os resultados obtidos com missForest e knn
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#       Imputando valores com missForest --> depois da proxima eletiva     %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
# o  pacote  depende  fortemente de haver relação entre as variaveis. cada
# uma é estimada em função das outras. Se a relação entre as vars nao for satisfatória
# os valores imputados nao serão muito satisfatórios

library(missForest)
set.seed(200)
str(camb.miss) #tem que ser data.frame e nao pode ser chr
camb.miss$SEXO=as.factor(camb.miss$SEXO)
camb.miss$BANDEIRA=as.factor(camb.miss$BANDEIRA)
camb.miss=as.data.frame(camb.miss)
summary(camb.miss)

camb.RF=missForest(camb.miss, xtrue = camb.completo)
ycamb.randomforest=camb.RF$ximp

original=camb.completo$BANDEIRA[is.na(camb.miss$BANDEIRA)];original
imputado=ycamb.randomforest$BANDEIRA[is.na(camb.miss$BANDEIRA)]; imputado
cbind(original, imputado)

original=camb.completo$RENDA[is.na(camb.miss$RENDA)];original
imputado=ycamb.randomforest$RENDA[is.na(camb.miss$RENDA)]; imputado
round(cbind(original, imputado, (original-imputado)/original*100),2)
#***********************************************************************
#exercicio:  considere a db  mobi.miss
#            impute valores com missForest; gere o arquivo mobi.forest
#            compare os valores originais e os imputados para todas as variáveis
#            comente
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
