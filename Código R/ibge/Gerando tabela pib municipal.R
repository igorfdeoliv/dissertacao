#Pacotes a serem utilizados----

  if(!require(dplyr))
    install.packages("dplyr")

  if(!require(stringr))
    install.packages("stringr")

  if(!require(readxl))
    install.packages("readxl")

#Limpando bases da mem�ria----

  rm(list=ls())

#Diret�rio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/ibge/pib municipal")

#Carregando e organizando tabelas----

  df1 <- read_excel("~/GitHub/dissertacao/dataset/ibge/pib municipal/pib 2002 2009.xls")
  df2 <- read_excel("~/GitHub/dissertacao/dataset/ibge/pib municipal/pib 2010 2017.xls")

  df1 <- df1 %>% 
    select("Ano","C�digo da Unidade da Federa��o","Sigla da Unidade da Federa��o",
         "Nome da Unidade da Federa��o","C�digo do Munic�pio","Nome do Munic�pio",
         "Semi�rido","Valor adicionado bruto da Agropecu�ria, \na pre�os correntes\n(R$ 1.000)",
         "Valor adicionado bruto da Ind�stria,\na pre�os correntes\n(R$ 1.000)",
         "Valor adicionado bruto dos Servi�os,\na pre�os correntes \n- exceto Administra��o, defesa, educa��o e sa�de p�blicas e seguridade social\n(R$ 1.000)",
         "Valor adicionado bruto da Administra��o, defesa, educa��o e sa�de p�blicas e seguridade social, \na pre�os correntes\n(R$ 1.000)",
         "Valor adicionado bruto total, \na pre�os correntes\n(R$ 1.000)","Impostos, l�quidos de subs�dios, sobre produtos, \na pre�os correntes\n(R$ 1.000)",
         "Produto Interno Bruto, \na pre�os correntes\n(R$ 1.000)","Produto Interno Bruto per capita, \na pre�os correntes\n(R$ 1,00)")

  df2 <- df2 %>% 
    select("Ano","C�digo da Unidade da Federa��o","Sigla da Unidade da Federa��o", 
         "Nome da Unidade da Federa��o","C�digo do Munic�pio","Nome do Munic�pio", 
         "Semi�rido","Valor adicionado bruto da Agropecu�ria, \na pre�os correntes\n(R$ 1.000)", 
         "Valor adicionado bruto da Ind�stria,\na pre�os correntes\n(R$ 1.000)", 
         "Valor adicionado bruto dos Servi�os,\na pre�os correntes \n- exceto Administra��o, defesa, educa��o e sa�de p�blicas e seguridade social\n(R$ 1.000)", 
         "Valor adicionado bruto da Administra��o, defesa, educa��o e sa�de p�blicas e seguridade social, \na pre�os correntes\n(R$ 1.000)", 
         "Valor adicionado bruto total, \na pre�os correntes\n(R$ 1.000)","Impostos, l�quidos de subs�dios, sobre produtos, \na pre�os correntes\n(R$ 1.000)", 
         "Produto Interno Bruto, \na pre�os correntes\n(R$ 1.000)","Produto Interno Bruto per capita, \na pre�os correntes\n(R$ 1,00)")

  pibmun <- rbind(df1,df2)

  rm(df1,df2)

  names(pibmun) <- c("ano","cod_uf","uf","estado","cod_mun","municipio",
                   "semiarido","vaba","vabi","vabs","vabadm","vabt",
                   "t","pib","pib_percapta")

  pibmun <- pibmun %>% 
    mutate("estado"=toupper(estado)) %>%
    mutate("municipio"=toupper(municipio)) %>% 
    mutate("municipio"=chartr("�����������'-", "AEIOUAOAEOC  ",municipio)) %>% 
    mutate("chave"=str_c(municipio," ","(", uf, ")")) %>% 
    filter(ano!=2002)

  pibmun <- pibmun[,c(1,16,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

#Destino tabela----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base")

#Exportando tabela----

  write.table(pibmun,file='pibmun.csv',sep=';',dec=".",na="",quote=TRUE, row.names=FALSE)