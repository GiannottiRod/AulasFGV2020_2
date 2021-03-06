###############################################################################
# Quiz 1                                                                      #
# Rodrigo Gregolin Giannotti                                                  #
###############################################################################

## Init

library(tidyverse)
library(magrittr)
library(readxl)

## Questão 1

readxl::excel_sheets("DADOSPAX.xlsx")
cam <- readxl::read_xlsx("DADOSPAX.xlsx", sheet = "CAMBRIDGE")

head(cam)

boxplot(cam$TEMPO ~ cam$BANDEIRA)
grid(col = "red")
par(new=TRUE)
boxplot(cam$TEMPO ~ cam$BANDEIRA,
        main = "Boxplot TEMPO x BANDEIRA")

cam %>%
  group_by(BANDEIRA) %>%
  summarise(median(TEMPO))

## Questão 2

table(cam$SEXO, cam$BANDEIRA) %>%
  prop.table(margin = 1) %>%
  round(digits = 3)

## Questão 3

tabela <- table(cam$BANDEIRA, cam$SEXO) %>%
  prop.table(margin = 2) %>%
  barplot(col = rainbow(3))
legend('bottomleft', legend = c('alfa','beta','gama'), col=rainbow(3), fill=rainbow(3) )

## Questão 4

betacard <- cam %>%
  filter(BANDEIRA == "beta")

betacard %>%
  group_by(SEXO) %>%
  summarise(sd(TEMPO))
