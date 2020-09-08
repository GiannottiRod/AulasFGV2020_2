


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          ANALISE EXPLORATORIA DE DADOS                            %
#                           Prof. Abraham Laredo Sicsu                              %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          IMPORTANDO UM ARQUIVO DO EXCEL                           %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Files -->  Upload --> escolher arquivo --> importardataset (..YES)


dd  <- DIRTYSHOP     # abreviando para simplificar digitacao 
str(dd)             # estrutura do arquivo de dados
colnames(dd)
head(dd, 10) #apresenta as 10 primeiras linhas (default é 6) e mostra tipo de variavel

#===================================================================================
#   Exercicio: abrir o arquivo MOBILE fornecido pelo Professor

#   Verificar estrutura, tipo de dados e listar os primeiros 8 paises
head(MOBILE)
head(mob)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                          GERANDO  SUB ARQUIVOS                                    %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dd1 <-  dd[, 2:4]       #arquivo com as colunas 2 3 e 4
head(dd1)

dd2 <- dd[,c(3,5:7)]   #arquivo com as colunas 3,5,6,7
head(dd2)

dd3=dd[, -c(1,4)]   #arquivo sem as colunas 1 e 4 (NOTE QUE UTILIZEI = EM VEZ DE <- )
head(dd3)

dd4=dd[1:20, 2:3]  #arquivo com as linhas 1 a 20 e colunas 2 e 3
dd4                # mostra todo o arquivo (interropendo se tiver muitas colunas)


dd5=dd[dd$UNIFED=="RJ",]   #contem apenas as linhas com RJ
dd5

dd6=dd[dd$UNIFED != "RJ",  ] #exclui RJ
dd6

dd7=subset(dd, dd$IDADE>50)  #considera apenas os clientes com MAIS de 50 anos
dim(dd7)


sum(is.na(dd))       #conta número de missing values (nao é o numero de linhas incompletas)
dd8 <- na.omit(dd)   #elimina todos as linhas com os dados em branco
sum(is.na(dd8))

dim(dd[complete.cases(dd),]) #dim: dimensao
dim(dd[!complete.cases(dd),]) 

rm(dd5,dd6,dd7,dd8)    #remove os arquivos criados acima

#======================================================================================
# Exercicio A1 : Utilizar MOBILE fornecido pelo Professor
# gerar arquivo contendo apenas paises com PRESIDENCIALISMO. 
     # qual a dimensão desse data.frame
     # verificar se esse novo data.rame contem missing values
     # reduzir esse arquivo considerando apenas os paises que tiverem IDH maior que 0.90
     # remover os arquivos criados
# voltando ao arquivo original, quantos Paises tem todas as obervações completas?
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            SALVANDO UM ARQUIVO DE DADOS PARA USO EM EXCEL                           %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write.csv(dd, "dd.csv", row.names = FALSE)

#para salvar um script no Rstudo cloud-> clicar no script na caixa de FILES (ao lado) --> More --> export
#ou entrar em File na barra de ferramentes e dar save as

#para salvar um script no Rstudo instalado em sua máquina --> FILe --> save as...


#======================================================================================
#Exercicio: Salve o script atual em sua área de trabalho
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       ALGUNS COMANDOS SIMPLES                                       %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dd$nova=log(dd$IDADE)   #variável 'nova' será inserida no arquivo dd
head(dd)
dd$nova <- NULL   #eliminamos pois nao vamos utilizá-la

dd$nova=dd$IDADE^2
dd$nova <- NULL   #eliminamos pois nao vamos utilizá-la

summary(dd$IDADE)

min(dd$IDADE)

sum(is.na(dd$IDADE))   #conta quantos missing values há em IDADE

min(dd$IDADE, na.rm = T)

sd(dd$IDADE, na.rm = T)
var(dd$IDADE, na.rm = T)

haha=log(dd$IDADE+1)
sum(is.na(haha)) #veja o que acontece quando idade=0 ou MV

#======================================================================================
#  Exercicio A2 : Arquivo MOBILE fornecido pelo Professor
      #calcular  mean, sd, median, max, sqrt, log10 da var IDADEMED

      #calcular o número de celulares por habitante e.... 
      #adicionar essa variavel ao arquivo original
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                       GERANDO UMA BASE DE DADOS EM R                                 %
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nomes=c("ROMUALDO", "PAULO", "MARIA","ROMILDA")
class(nomes)
idades=c(25,36,72,18)
pesos=c(50 ,80,130,194)
class(idades);class(pesos)
nw=cbind(idades,pesos); nw; class(nw)  #as duas vars sao do mesmo formato --> matriz

newdata=data.frame(nomes, idades);newdata;class(newdata);str(newdata)
# gera data.frame com as  vars de tipos diferentes

#======================================================================================
#  Exercicio A3
    #gere um vetor X com os labels 'peso', 'altura', 'idade', 'numero_de_irmãos' 
    #gere um vetor Y com os valores correspondentes Invente!
    #gere um data frame onde a primeira coluna sao os labels e a segunda coluna o vetor Y
    #dê o print desse db
    #analise a estrutura desse df

    #agregue ao vetor X o label 'nome"
    #agregue ao vetor Y um nome (invente)
    #gere um data frame onde a primeira coluna sao os labels e a segunda coluna os valores
    # #analise a estrutura desse df

    # procure no google> como alterar o nome de uma única variavel no r
    # procure no google> como alterar de todas as variáveis ao mesmo tempo no r

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

