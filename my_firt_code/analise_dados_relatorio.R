#Importando a biblioteca
library(readr)

#lendo os dados do csv
dados_relatorio = read_csv("dados.csv")
View(dados_relatorio)



#tabela que do sexo relacionada saude mental
usar = table(dados_relatorio$`Qual o seu sexo?`,dados_relatorio$`Em uma escala de 0 a 10 quanto a  pandemia afetou a sua saúde mental de forma negativa?`)
View(usar)
plot(usar)



#numero total de participantes
n_populacao = 23

 

#grafico do sexo
sexo = table(dados_relatorio$`Qual o seu sexo?`)
barplot(sexo, col = c("#FF0000","#006778"), ylab = "Quantidade")



#Criando tabela dos dados referente ao sexo
#criando a matriz da tabela
sexo_exportar = matrix(NA,3, 3)
colnames(sexo_exportar) = c("Total","Freq. absoluta ", "Percentual")
rownames(sexo_exportar)= c(rownames(sexo), "TOTAL")

#preenchendo a matriz
sexo_exportar[1,1]= 11
sexo_exportar[2,1]= 12
sexo_exportar[3,1]= 23
sexo_exportar[1,2]= 0.4782609
sexo_exportar[2,2]= 0.5217391
sexo_exportar[3,2]= 1
sexo_exportar[1,3]= "47.82%"
sexo_exportar[2,3]= "52.17%"
sexo_exportar[3,3]= "100%"

#exportar a tabela
write.table(sexo_exportar, file="Quadro1 -sexo.csv", sep="\t")



#Retorna a frequência de cada dado(opção) da variável sexo
freq_sexo = prop.table(sexo)
print(freq_sexo)
barplot(freq_sexo, col = c("#FF0000","#006778"), ylab = "Freq. absoluta", 
        main = "Percentual de cada sexo")



#Carregando dados da coluna "afetou fisicamente"
afetou_fisicamente = table(dados_relatorio$`A pandemia afetou a sua saúde física de forma negativa?`)
freq_afetou_fisicamente = prop.table(afetou_fisicamente)
print(freq_afetou_fisicamente)
barplot(afetou_fisicamente*100 / n_populacao,col =c("#CD1818","#FFB72B","#6BCB77"), 
        ylab = "Percentual")



#Relação sexo vs afetou fisicamente
sexo_afetouFisicamente = table(dados_relatorio$`Qual o seu sexo?`,
                    dados_relatorio$`A pandemia afetou a sua saúde física de forma negativa?`)
write.table(sexo_afetouFisicamente)
prop.table(sexo_afetouFisicamente)
barplot(sexo_afetouFisicamente, beside = TRUE,legend.text = rownames(sexo),
          args.legend = list(x= "topleft"),col = c("#FF0000","#006778"))

#grafico mosaic uso quando quero relacionar duas ou + variáveis qualitativas.
plot(sexo_afetouFisicamente)



#Carregando dados da coluna quanto investe
investimento_saude = table(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)

#Frequencia dos dado(opções) de invetimentos
freq_investimentos = prop.table(investimento_saude)
write.table(freq_investimentos)
barplot(freq_investimentos)


#Grafico relaciona sexo vs investimento
sexo_investimento = table(dados_relatorio$`Qual o seu sexo?`,
                     dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)
barplot(sexo_investimento*100/n_populacao, beside = TRUE,legend.text = rownames(sexo),
        args.legend = list(x= "top"),col = c("#FF0000","#006778"), 
        xlab = "Investimento mensal ($)", ylab = "Percentual")

#uso boxplot quando for uma variável qualitativa relaconada a uma quantitativa
boxplot(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`~dados_relatorio$`Qual o seu sexo?`)



#Ordenando elementos da coluna investimentos
gastos_ordenado = sort(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)

#menor e maior valor da variável
range(gastos_ordenado) 

#Definindo a amplitude total 
amplitude_total = ceiling((max(gastos_ordenado)- min(gastos_ordenado)))
print(amplitude_total)

#Encontrar número de classes(intervalos)
length(gastos_ordenado)#quantos elementos tem
n_classes = ceiling((sqrt(length(gastos_ordenado))))
print(n_classes)

#Definindo a amplitude do intervalo
amplitude_intervalo = ceiling(amplitude_total/n_classes)
print(amplitude_intervalo)

#limtes inferior(1ª classe) e limite superior(ultima classe)
lim_infclass = min(gastos_ordenado)
lim_supclass = lim_infclass+(n_classes*amplitude_intervalo)
print(lim_infclass)
print(lim_supclass)

#contruindo tabela de distribuição de frequência
#informar ao R quais serão as quebras da nossa distribuição usaremos a função seq()
quebras = seq(lim_infclass,lim_supclass,amplitude_intervalo)
print(quebras)

#gráfico sexo vs gastos
DF_gastos = table(cut(gastos_ordenado,breaks = quebras, right = TRUE))
View(DF_gastos)

hist(gastos_ordenado,  
     main = "Investimentos mensais", 
     xlab = "Valor mensal (R$)", ylab = "Freq. Absoluta", 
     col = c("#E02401", "#FF5403","#FF8E00", "#FFC300","#FFE61B"), 
     border = c("black"),
     breaks = quebras,
     right = TRUE,
     axes = FALSE
)
axis(1, at=seq(lim_infclass,lim_supclass,amplitude_intervalo))
axis(2,at=seq(0,max(gastos_ordenado),5))
mean(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)
sum(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)


#construção do gráfigo dos gasto do sexo feminino
feminino_gastos = matrix(NA,11,1)
feminino_gastos[,1]=c(0,0,0,50,100,200,300,300,300,300,500)
sum(feminino_gastos)
mean(feminino_gastos)  #média
median(feminino_gastos) #mediana
media_F_total = sum(feminino_gastos) /sum(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)
print(media_F_total*100)
View(feminino_gastos) 
amplitude_total_F = ceiling((max(feminino_gastos)- min(feminino_gastos)))
n_classes_F = ceiling((sqrt(length(feminino_gastos))))
print(n_classes_F)
amplitude_intervalo_F = ceiling(amplitude_total_F/n_classes_F)
print(amplitude_intervalo_F)
lim_infclass_F = min(feminino_gastos)
lim_supclass_F = lim_infclass_F+(n_classes_F*amplitude_intervalo_F)
print(lim_infclass_F)
print(lim_supclass_F)
quebras_F = seq(lim_infclass_F,lim_supclass_F,amplitude_intervalo_F)
print(quebras_F)
DF_gastos = table(cut(gastos_ordenado,breaks = quebras, right = TRUE))
View(DF_gastos)

feminino_exportar = matrix(NA,4, 3)
colnames(feminino_exportar)=c("Intervalos","Frequencia","Percentual em relação ao total feminino")
feminino_exportar[,1] = c("[0,125[","[125,250[", "[250,375[", "[375,500]") 
feminino_exportar[,2] = c(5,1,4,1) 
feminino_exportar[,3] = c("7.31%","9.75%", "58.53%", "24.39%") 
View(feminino_exportar)
write.table(feminino_exportar, file="tabela1 -investimentos feminino.csv", sep="\t")


#construção do gráfigo dos gasto do sexo feminino
masculino_gastos = matrix(NA,12, 1)
masculino_gastos[,1]= c(0,0,0,0,20,30,65,100,100,100,200,500)
sum(masculino_gastos)
mean(masculino_gastos)  #média
median(masculino_gastos) #mediana
amplitude_total_M = ceiling((max(masculino_gastos)- min(masculino_gastos)))
n_classes_M = ceiling((sqrt(length(masculino_gastos))))
print(n_classes_M)
amplitude_intervalo_M = ceiling(amplitude_total_M/n_classes_M)
print(amplitude_intervalo_M)
lim_infclass_M = min(masculino_gastos)
lim_supclass_M = lim_infclass_M+(n_classes_M*amplitude_intervalo_M)
print(lim_infclass_M)
print(lim_supclass_M)
quebras_M = seq(lim_infclass_M,lim_supclass_M,amplitude_intervalo_M)
print(quebras_M)
masculino_exportar = matrix(NA,4, 3)
colnames(masculino_exportar)=c("Intervalos","Frequencia","Percentual em relação ao total masculino")
masculino_exportar[,1] = c("[0,125[","[125,250[", "[250,375[", "[375,500]") 
masculino_exportar[,2] = c(10,1,0,1) 
masculino_exportar[,3] = c("37.21%","17.93%", "0.00%", "44.84%") 
View(masculino_exportar)
write.table(feminino_exportar, file="Tabela2 -investimentos masculino.csv", sep="\t")
media_M_total = sum(masculino_gastos) /sum(dados_relatorio$`Em média quanto você gasta por mês com sua saúde?`)
print(media_M_total*100)
View(masculino_gastos) 


#Gráfico pandemia afetou quanto pode investir
sexo_afetou_investir = table(dados_relatorio$`Qual o seu sexo?`,
                   dados_relatorio$`Você concorda que a pandemia afetou o quanto você poderia dedicar financeiramente (dinheiro) à sua saúde?`)
View(sexo_afetou_investir)
afetou_investir = table(dados_relatorio$`Você concorda que a pandemia afetou o quanto você poderia dedicar financeiramente (dinheiro) à sua saúde?`)
range(sexo_afetou_investir)
barplot(sexo_afetou_investir, beside = TRUE,
        col = c("#FF0000","#006778"), ylab = "Freq. absoluta")
#custumizando legenda
legend("topleft",inset = 0.0,
       c("F", "M"), lty=c(0,0), pch= c(15,15),
       col =c("#FF0000","#006778"))



#Gráfico pandemia afetou pisicologicamente
sexo_afetouPisicologicamente = table(dados_relatorio$`Qual o seu sexo?`,
                                     dados_relatorio$`Em uma escala de 0 a 10 quanto a  pandemia afetou a sua saúde mental de forma negativa?`)
View(sexo_afetouPisicologicamente)
barplot(sexo_afetouPisicologicamente,
        col = c("#FF0000","#006778"), ylab = "Freq. absoluta", xlab = "0 - Não afetou    10 - Totalmente")
legend("topright",inset = 0.0,
       c("F", "M"), lty=c(0,0), pch= c(15,15),
       col =c("#FF0000","#006778"))


#Gráfico praticou esportes durante a pandemia VS sexo
pratica_esporte = table(dados_relatorio$`Você praticou algum esporte durante esse período de pandemia?`,dados_relatorio$`Qual o seu sexo?`)
View(pratica_esporte)
barplot(pratica_esporte,col =c("#CD1818","#FFB72B"), 
        ylab = "Freq. absoluta", axes = FALSE)
axis(2, at=seq(0,18,2))
plot(pratica_esporte)


#Gráfico ida ao psicologo durante a pandemia VS já frequentava
ida_psicologo = table(dados_relatorio$`Você alguma vez foi a um psicólogo(a), durante a pandemia?`)
View(ida_psicologo)
barplot(ida_psicologo,col =c("#CD1818","#FFB72B"), 
        ylab = "Freq. absoluta", axes = FALSE)
axis(2, at=seq(0,20,3))
plot(ida_psicologo)

ja_frequentava_psico = table(dados_relatorio$`Você já frequentava o psicologo(a) antes da pandemia?`)
View(ja_frequentava_psico)
barplot(ja_frequentava_psico,col =c("#CD1818","#FFB72B"), 
        ylab = "Freq. absoluta", axes = FALSE)
axis(2, at=seq(0,5,1))