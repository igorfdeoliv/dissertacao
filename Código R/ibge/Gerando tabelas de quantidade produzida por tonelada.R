#Carregando pacotes a serem utilizados----

  if(!require(dplyr))
    install.packages("dplyr")

  if(!require(stringr))
    install.packages("stringr")

  if(!require(tidyverse))
    install.packages("tidyverse")

#Limpando bases da mem�ria----

  rm(list=ls())

#Organizando tabelas----

#Quantidade produzida por cultura em toneladas

  ano1 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2003%20tabela5457.csv"
  ano2 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2004%20tabela5457.csv"
  ano3 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2005%20tabela5457.csv"
  ano4 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2006%20tabela5457.csv"
  ano5 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2007%20tabela5457.csv"
  ano6 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2008%20tabela5457.csv"
  ano7 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2009%20tabela5457.csv"
  ano8 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2010%20tabela5457.csv"
  ano9 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2011%20tabela5457.csv"
  ano10 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2012%20tabela5457.csv"
  ano11 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2013%20tabela5457.csv"
  ano12 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2014%20tabela5457.csv"
  ano13 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2015%20tabela5457.csv"
  ano14 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2016%20tabela5457.csv"
  ano15 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2017%20tabela5457.csv"
  ano16 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2018%20tabela5457.csv"
  ano17 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/quantidade%20produzida/2019%20tabela5457.csv"

#Tabela 2003----

#Importando tabela

  q2003 <- read.csv(ano1, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2003 <- q2003[-1,]

  q2003 <- q2003 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2003)

#Reorganizando as colunas

  q2003 <- q2003[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2003")

  write.table(q2003,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano1,q2003)

#Tabela 2004----

#Importando tabela

  q2004 <- read.csv(ano2, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2004 <- q2004[-1,]

  q2004 <- q2004 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2004)

#Reorganizando as colunas

  q2004 <- q2004[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2004")

  write.table(q2004,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano2,q2004)

#Tabela 2005----

#Importando tabela

  q2005 <- read.csv(ano3, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2005 <- q2005[-1,]

  q2005 <- q2005 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2005)

#Reorganizando as colunas

  q2005 <- q2005[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2005")

  write.table(q2005,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano3,q2005)

#Tabela 2006----

#Importando tabela

  q2006 <- read.csv(ano4, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2006 <- q2006[-1,]

  q2006 <- q2006 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2006)

#Reorganizando as colunas

  q2006 <- q2006[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2006")

  write.table(q2006,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano4,q2006)

#Tabela 2007----

#Importando tabela

  q2007 <- read.csv(ano5, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2007 <- q2007[-1,]

  q2007 <- q2007 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2007)

#Reorganizando as colunas

  q2007 <- q2007[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2007")

  write.table(q2007,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano5,q2007)

#Tabela 2008----

#Importando tabela

  q2008 <- read.csv(ano6, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2008 <- q2008[-1,]

  q2008 <- q2008 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2008)

#Reorganizando as colunas

  q2008 <- q2008[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2008")

  write.table(q2008,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano6,q2008)

#Tabela 2009----

#Importando tabela

  q2009 <- read.csv(ano7, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2009 <- q2009[-1,]

  q2009 <- q2009 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2009)

#Reorganizando as colunas

  q2009 <- q2009[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2009")

  write.table(q2009,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano7,q2009)

#Tabela 2010----

#Importando tabela

  q2010 <- read.csv(ano8, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2010 <- q2010[-1,]

  q2010 <- q2010 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2010)

#Reorganizando as colunas

  q2010 <- q2010[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2010")

  write.table(q2010,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano8,q2010)

#Tabela 2011----

#Importando tabela

  q2011 <- read.csv(ano9, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2011 <- q2011[-1,]

  q2011 <- q2011 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2011)

#Reorganizando as colunas

  q2011 <- q2011[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

  write.table(q2011,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano9,q2011)

#Tabela 2012----

#Importando tabela

  q2012 <- read.csv(ano10, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2012 <- q2012[-1,]

  q2012 <- q2012 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2012)

#Reorganizando as colunas

  q2012 <- q2012[,c(7,1,2,3,4,5,6)]

#Exportando tabela
  
  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2012")

  write.table(q2012,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano10,q2012)

#Tabela 2013----

#Importando tabela

  q2013 <- read.csv(ano11, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2013 <- q2013[-1,]

  q2013 <- q2013 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2013)

#Reorganizando as colunas

  q2013 <- q2013[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2013")

  write.table(q2013,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano11,q2013)

#Tabela 2014----

#Importando tabela

  q2014 <- read.csv(ano12, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2014 <- q2014[-1,]

  q2014 <- q2014 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2014)

#Reorganizando as colunas

  q2014 <- q2014[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2014")

  write.table(q2014,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano12,q2014)

#Tabela 2015----

#Importando tabela

  q2015 <- read.csv(ano13, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2015 <- q2015[-1,]

  q2015 <- q2015 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2015)

#Reorganizando as colunas

  q2015 <- q2015[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2015")

  write.table(q2015,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano13,q2015)

#Tabela 2016----

#Importanto tabela

  q2016 <- read.csv(ano14, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2016 <- q2016[-1,]

  q2016 <- q2016 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2016)

#Reorganizando as colunas

  q2016 <- q2016[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2016")

  write.table(q2016,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano14,q2016)

#Tabela 2017----

#Importando tabela

  q2017 <- read.csv(ano15, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  q2017 <- q2017[-1,]

  q2017 <- q2017 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2017)

#Reorganizando as colunas

  q2017 <- q2017[,c(7,1,2,3,4,5,6)]

#Esportando tabela

  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

  write.table(q2017,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano15,q2017)

#Tabela 2018----
  
#Importando tabela
  
  q2018 <- read.csv(ano16, header=T, sep=";",dec = ",", skip = 4,nrows = 5563,
                    na.strings = c("-","..."), encoding = "UTF-8")
  
  q2018 <- q2018 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2018)
  
#Reorganizando as colunas
  
  q2018 <- q2018[,c(7,1,2,3,4,5,6)]
  
#Esportando tabela
  
  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2018")
  
  write.table(q2018,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)
  
  rm(ano16,q2018)
  
#Tabela 2019----
  
#Importando tabela
  
  q2019 <- read.csv(ano17, header=T, sep=";",dec = ",", skip = 4,nrows = 5563,
                    na.strings = c("-","..."), encoding = "UTF-8")
  
  q2019 <- q2019 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2019)
  
#Reorganizando as colunas
  
  q2019 <- q2019[,c(7,1,2,3,4,5,6)]
  
#Esportando tabela
  
  setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2019")
  
  write.table(q2019,file='quantidade_produzida.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)
  
  rm(ano17,q2019)