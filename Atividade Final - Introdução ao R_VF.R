#A TABELA ESCOLHIDA PELO GRUPO FOI RETIRADA NO SITE "http://www.imp.seade.gov.br/frontend/#/tabelas"
#SÃO INFORMAÇÕES REFERENTE A EMPREGO ENTRE OS GENEROS E FORMAÇÃO


#1.INSTALANDO OS PACOTES DO R
require(dplyr)
require(data.table)
require(tidyr)


#2.ABRINDO A TABELA E FAZENDO A VISUALIZAÇÃO
instrucao <- read.csv2("emprego instrucao.csv", header = TRUE)
instrucao <- as.data.frame(instrucao)

sexo <- read.csv2("emprego sexo.csv", header = TRUE)
sexo <- as.data.frame(sexo)


#3.FAZENDO A VISUALIZAÇÃO
#View(instrucao)
#View(sexo)


#4.TRANSFORMANDO OS CAMPOS VAZIOS "NA"
instrucao[is.na(instrucao)] <- 0
sexo[is.na(sexo)] <- 0


#5.CRIANDO UMA TABELA SECUNDÁRIA COM OS DADOS QUE INTERESSAM E FAZENDO A VISUALIZAÇÃO
instrucao <- instrucao[,c(1:7)]
#View(instrucao)


sexo <- sexo[,c(1:5)]
#View(sexo)


#6.JUNTANDO AS TABELAS
emprego <- inner_join(instrucao, sexo, by=c("Localidades", "Periodos"))
#View(emprego)
ncol(emprego)

#7.SIMPLIFICANDO OS DADOS DA TABELA
emprego <- emprego[,c(1:3,9:10,4:7)]
names(emprego)<-c("Localidades","Periodo","Empregos_formais","Homens", "Mulheres",
                  "Ensino_fundamental_incompleto","Ensino_fundamental_completo",
                  "Ensino_médio_completo","Ensino_superior_completo")

#7.CRIANDO SUB-TABELAS
  #TABELA DE EMPREGO POR ANO E SEXO
  emprego_ano_sexo <-emprego 
  emprego_ano_sexo$Localidades = NULL

  emprego_ano_sexo <- emprego %>% 
               group_by(Periodo) %>% 
               dplyr::summarise(Amostra = n(), 
                          Total_empregos_ano = sum(Empregos_formais),
                          Total_emprego_homem = sum(Homens),
                          Total_emprego_mulher = sum(Mulheres))
  View(emprego_ano_sexo)
   
#8.PERCENTUAL DO NÍVEL DE INSTRUÇÃO SOBRE O NÚMERO DE EMPREGOS FORMAIS POR Periodo
  emprego_ano_instrucao <- emprego %>% group_by(Periodo) %>% 
    summarise(superior=sum(as.numeric(Ensino_superior_completo))/sum(as.numeric(Empregos_formais))*100,
              Ens_Medio=sum(as.numeric(Ensino_médio_completo))/sum(as.numeric(Empregos_formais))*100,
              Ens_Fund_comp=sum(as.numeric(Ensino_fundamental_completo))/sum(as.numeric(Empregos_formais))*100,
              Ens_Fund_incomp=sum(as.numeric(Ensino_fundamental_incompleto))/sum(as.numeric(Empregos_formais))*100)
  
  #View(emprego_ano_instrucao)
  
  str(emprego)
  str(instrucao)
  str(sexo)

#9.ANÁLISE1 A SER ESCOLHIDA COM GRÁFICO

  #rm(NOTA_REDAÇAO_FEM)
  
  EmpregosFormais <- sexo %>% 
    dplyr::filter(Localidades=="Adamantina") %>% 
    dplyr::group_by(Localidades,Periodos,EmpregosFormais) %>% 
    dplyr::summarise(Numero=n())
  
  EmpregosFormais_fem <- sexo %>% 
    dplyr::filter(Localidades=="Adamantina") %>% 
    dplyr::group_by(Localidades,Periodos,EmpregosFormaisdeMulheres) %>% 
    dplyr::summarise(Numero=n())

  

  ####    FREQUENCIA DE EMPREGOS DO SEXO FEMININO 
  plot.default(EmpregosFormais$Periodos,EmpregosFormais$EmpregosFormais,
               main = "Empregos Formais por periodo",type = "b",
               new = T, pch = 20, ylab = "Empregos",xlab = "Periodo",ylim=c(4e3,12e3), xlim=c(2013,2017),las=1,
               cex = 1,ann = T,xaxt='n')
  par(new = TRUE)
  plot.default(EmpregosFormais_fem$Periodos,EmpregosFormais_fem$EmpregosFormaisdeMulheres,
               main = "",type = "b", col = "deeppink1",
               new = T, pch = 20, ylab = "",xlab = "",ylim=c(4e3,12e3), xlim=c(2013,2017),las=1,
               cex = 1,ann = T,xaxt='n')
  
  axis(1, at=EmpregosFormais$Periodos,labels=EmpregosFormais$Periodos, col.axis=1, las=2, cex.axis=0.7)
  abline(v = 2015, col = "red", lty = 1, lwd = 1)
  text(EmpregosFormais$Periodos,EmpregosFormais$EmpregosFormais,labels = EmpregosFormais$EmpregosFormais, cex=0.7, col="red",adj = 1, pos = 3)
  text(EmpregosFormais_fem$Periodos,EmpregosFormais_fem$EmpregosFormaisdeMulheres,labels = EmpregosFormais_fem$EmpregosFormaisdeMulheres, cex=0.7, col="deeppink1",adj = 1, pos = 3)
  



