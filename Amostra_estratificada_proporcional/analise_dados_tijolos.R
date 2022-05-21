library(readr)
library(dplyr)

tijolo = read_csv("/home/larissa/R/relatorio2/dados_tijolos.csv")
View(tijolo)

tabela_mistura = select(tijolo,Mistura, Índice)
View(tabela_mistura)

#selecionando manualmente somente os tijolos de mistura = "0"
mistura0 = tijolo[1:12,]
View(mistura0)

#selecionando manualmente somente os tijolos de mistura = "1"
mistura1 = tijolo[13:15,]
View(mistura1)

#amostra estratificada em relação a mistura

#tamanho da amostra a ser tirada
qtd_retirar = 3

#descobrindo o tamanho de cada amostra que deve ser tirada de cada tipo de mistura
N = nrow(tabela_mistura)
print(N)

#A que percentual da população corresponte o tamanho da amostra
perc_total = qtd_retirar / N
print(perc_total)

#Quantidade de elementos a ser retirada da mistura="0"
perc1 = perc_total*12
print(perc1)

#Quantidade de elementos a ser retirada da mistura="1"
perc2 = perc_total*3
print(perc2)

#Escolhendo os elementos aleatoriamente da mistura="0"
tam_amostar1 = sample(nrow(mistura0), 2, replace = FALSE, prob = NULL )
print(tam_amostar1)

#Escolhendo os elementos aleatoriamente da mistura="1"
tam_amostar2 = sample(nrow(mistura1), 1, replace = FALSE, prob = NULL )
print(tam_amostar2)

if (tam_amostar2==1){
ind_amostra <- c(tam_amostar1, 13)
}else if (tam_amostar2==2){
  ind_amostra <- c(tam_amostar1, 14)
}else{
  ind_amostra <- c(tam_amostar1, 15)
}

#Índices dos elementos da amostra estratificada proporcional
print(ind_amostra)

#amostra estratificada
amostra = (tijolo[ind_amostra,])
View(amostra)

#exportando tabela da amostra como csv
write.table(amostra, file="Quadro2 - amostra.csv", sep=",")
