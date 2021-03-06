#Carregando pacotes a setem utilizados----

  if(!require(dplyr))
    install.packages("dplyr")

  if(!require(stringr))
    install.packages("stringr")

#Limpando bases da mem�ria----

  rm(list=ls())

#Organizando e manipulando a base de dados----

#Informa��es sobre os sal�rios m�dios recebidos por culturas

  ano1 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2003%20consulta84077740.csv"
  ano2 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2004%20consulta93907528.csv"
  ano3 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2005%20consulta599322.csv"
  ano4 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2006%20consulta88878957.csv"
  ano5 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2007%20consulta20674703.csv"
  ano6 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2008%20consulta93264368.csv"
  ano7 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2009%20consulta21922120.csv"
  ano8 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2010%20consulta59993853.csv"
  ano9 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2011%20consulta18517870.csv"
  ano10 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2012%20consulta50313615.csv"
  ano11 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2013%20consulta91523342.csv"
  ano12 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2014%20consulta95908753.csv"
  ano13 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2015%20consulta58737713.csv"
  ano14 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2016%20consulta34072961.csv"
  ano15 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/rais/2017%20consulta90972656.csv"

#Tabela 2003----

#Importando tabela

  w2003 <- read.table(ano1, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2003 <- w2003 %>% 
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>% 
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>% 
    mutate("chave"=str_c(Mun," ","(", Estado,")") ) %>% 
    filter(Mun!="IGNORADO") %>% 
    mutate("ano"=2003)

#Reorganizando colunas:

  w2003$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2003$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2003$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2003$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2003$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2003$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2003$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2003$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2003$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2003$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2003$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2003$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2003$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2003$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2003$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2003$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2003$Total <- as.numeric(w2003$Total)

  w2003 <- w2003[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2003")

  write.table(w2003,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano1,w2003)

#Tabela 2004----

#Importando tabela

  w2004 <- read.table(ano2, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2004 <- w2004 %>% 
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>% 
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado,")") ) %>% 
    filter(Mun!="IGNORADO") %>% 
    mutate("ano"=2004)

#Reorganizando colunas

  w2004$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2004$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2004$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2004$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2004$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2004$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2004$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2004$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2004$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2004$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2004$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2004$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2004$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2004$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2004$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2004$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2004$Total <- as.numeric(w2004$Total)

  w2004 <- w2004[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2004")

  write.table(w2004,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano2,w2004)

#tabela 2005----

#Importando tabela

  w2005 <- read.table(ano3, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2005 <- w2005 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2005)

#Reorganizando colunas

  w2005$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2005$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2005$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2005$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2005$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2005$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2005$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2005$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2005$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2005$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2005$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2005$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2005$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2005$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2005$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2005$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2005$Total <- as.numeric(w2005$Total)

  w2005 <- w2005[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2005")

  write.table(w2005,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano3,w2005)

#tabela 2006----

#Importando tabela

  w2006 <- read.table(ano4, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2006 <- w2006 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2006)

#Reorganizando colunas

  w2006$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2006$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2006$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2006$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2006$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2006$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2006$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2006$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2006$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2006$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2006$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2006$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2006$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2006$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2006$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2006$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2006$Total <- as.numeric(w2006$Total)


  w2006 <- w2006[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2006")

  write.table(w2006,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano4,w2006)

#tabela 2007----

#Importando tabela

  w2007 <- read.table(ano5, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2007 <- w2007 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2007)

#Reorganizando colunas

  w2007$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2007$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2007$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2007$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2007$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2007$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2007$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2007$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2007$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2007$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2007$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2007$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2007$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2007$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2007$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2007$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2007$Total <- as.numeric(w2007$Total)

  w2007 <- w2007[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2007")

  write.table(w2007,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano5,w2007)

#tabela 2008----

#Importando tabela

  w2008 <- read.table(ano6, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2008 <- w2008 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2008)

#Reorganizando colunas

  w2008$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2008$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2008$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2008$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2008$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2008$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2008$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2008$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2008$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2008$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2008$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2008$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2008$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2008$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2008$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2008$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2008$Total <- as.numeric(w2008$Total)

  w2008 <- w2008[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela


  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2008")

  write.table(w2008,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano6,w2008)

#tabela 2009----

#Importando tabela

  w2009 <- read.table(ano7, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2009 <- w2009 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2009)

#Reorganizando colunas

  w2009$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2009$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2009$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2009$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2009$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2009$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2009$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2009$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2009$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2009$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2009$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2009$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2009$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2009$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2009$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2009$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2009$Total <- as.numeric(w2009$Total)

  w2009 <- w2009[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2009")

  write.table(w2009,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano7,w2009)

#tabela 2010----

#Importando tabela

  w2010 <- read.table(ano8, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2010 <- w2010 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2010)

#Reorganizando colunas

  w2010$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2010$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2010$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2010$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2010$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2010$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2010$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2010$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2010$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2010$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2010$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2010$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2010$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2010$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2010$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2010$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2010$Total <- as.numeric(w2010$Total)

  w2010 <- w2010[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2010")

  write.table(w2010,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano8,w2010)

#tabela 2011----

#Importando tabela

  w2011 <- read.table(ano9, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2011 <- w2011 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2011)

#Reorganizando colunas

  w2011$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2011$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2011$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2011$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2011$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2011$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2011$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2011$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2011$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2011$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2011$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2011$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2011$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2011$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2011$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2011$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2011$Total <- as.numeric(w2011$Total)

  w2011 <- w2011[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

  write.table(w2011,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano9,w2011)

#tabela 2012----

#Importando tabela

  w2012 <- read.table(ano10, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2012 <- w2012 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2012)

#Reorganizando colunas

  w2012$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2012$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2012$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2012$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2012$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2012$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2012$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2012$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2012$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2012$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2012$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2012$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2012$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2012$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2012$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2012$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2012$Total <- as.numeric(w2012$Total)

  w2012 <- w2012[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2012")

  write.table(w2012,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano10,w2012)

#tabela 2013----

#Importando tabela

  w2013 <- read.table(ano11, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2013 <- w2013 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2013)

#Reorganizando colunas

  w2013$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2013$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2013$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2013$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2013$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2013$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2013$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2013$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2013$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2013$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2013$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2013$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2013$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2013$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2013$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2013$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2013$Total <- as.numeric(w2013$Total)

  w2013 <- w2013[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2013")

  write.table(w2013,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano11,w2013)

#tabela 2014----

#Importando tabela

  w2014 <- read.table(ano12, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2014 <- w2014 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2014)

#Reorganizando colunas

  w2014$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2014$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2014$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2014$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2014$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2014$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2014$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2014$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2014$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2014$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2014$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2014$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2014$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2014$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2014$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2014$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2014$Total <- as.numeric(w2014$Total)

  w2014 <- w2014[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2014")

  write.table(w2014,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano12,w2014)

#tabela 2015----

#Importando tabela

  w2015 <- read.table(ano13, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2015 <- w2015 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2015)

#Reorganizando colunas

  w2015$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2015$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2015$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2015$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2015$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2015$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2015$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2015$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2015$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2015$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2015$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2015$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2015$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2015$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2015$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2015$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2015$Total <- as.numeric(w2015$Total)

  w2015 <- w2015[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela
  
  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2015")

  write.table(w2015,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano13,w2015)

#tabela 2016----

#Importando tabela

  w2016 <- read.table(ano14, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2016 <- w2016 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2016)

#Reorganizando colunas

  w2016$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2016$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2016$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2016$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2016$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2016$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2016$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2016$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2016$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2016$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2016$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2016$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2016$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2016$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2016$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2016$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2016$Total <- as.numeric(w2016$Total)

  w2016 <- w2016[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2016")

  write.table(w2016,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano14,w2016)

#tabela 2017----

#Importando tabela

  w2017 <- read.table(ano15, header=T, sep=";", dec = ".", nrows = 5658, skip = 1)

  w2017 <- w2017 %>%
    mutate("Estado"=str_sub(Munic�pio, end = 2)) %>%
    mutate("Mun"=str_sub(Munic�pio, start = 4)) %>%
    mutate(Mun=chartr("�����������'-", "AEIOUAOAEOC  ", Mun)) %>%
    mutate("chave"=str_c(Mun," ","(", Estado, ")")) %>%
    filter(Mun!="IGNORADO") %>%
    mutate("ano"=2017)

#Reorganizando colunas

  w2017$PRODUTOR.DA.CULTURA.DE.DENDE <- as.numeric(w2017$PRODUTOR.DA.CULTURA.DE.DENDE)
  w2017$TRABALHADOR.NA.CULTURA.DE.DENDE <- as.numeric(w2017$TRABALHADOR.NA.CULTURA.DE.DENDE)
  w2017$PRODUTOR.DA.CULTURA.DE.GIRASSOL <- as.numeric(w2017$PRODUTOR.DA.CULTURA.DE.GIRASSOL)
  w2017$TRABALHADOR.NA.CULTURA.DO.GIRASSOL <- as.numeric(w2017$TRABALHADOR.NA.CULTURA.DO.GIRASSOL)
  w2017$PRODUTOR.DA.CULTURA.DE.MAMONA <- as.numeric(w2017$PRODUTOR.DA.CULTURA.DE.MAMONA)
  w2017$TRABALHADOR.NA.CULTURA.DE.MAMONA <- as.numeric(w2017$TRABALHADOR.NA.CULTURA.DE.MAMONA)
  w2017$PRODUTOR.DA.CULTURA.DE.SOJA <- as.numeric(w2017$PRODUTOR.DA.CULTURA.DE.SOJA)
  w2017$TRABALHADOR.NA.CULTURA.DE.SOJA <- as.numeric(w2017$TRABALHADOR.NA.CULTURA.DE.SOJA)
  w2017$Total <- as.numeric(w2017$Total)

  w2017 <- w2017[,c(14,13,11,12,1,2,6,3,9,4,7,5,8,10)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

  write.table(w2017,file='rais.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano15,w2017)