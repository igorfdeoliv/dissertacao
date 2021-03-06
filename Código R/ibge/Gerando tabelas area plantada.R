#Carregando pacotes a serem utilizados----

  if(!require(dplyr))
    install.packages("dplyr")

  if(!require(stringr))
    install.packages("stringr")

#Limpando bases da mem�ria----

  rm(list=ls())

#Organizando tabelas----

#�rea plantada por cultura em hectares

  ano1 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2003%20tabela5457.csv"
  ano2 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2004%20tabela5457.csv"
  ano3 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2005%20tabela5457.csv"
  ano4 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2006%20tabela5457.csv"
  ano5 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2007%20tabela5457.csv"
  ano6 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2008%20tabela5457.csv"
  ano7 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2009%20tabela5457.csv"
  ano8 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2010%20tabela5457.csv"
  ano9 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2011%20tabela5457.csv"
  ano10 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2012%20tabela5457.csv"
  ano11 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2013%20tabela5457.csv"
  ano12 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2014%20tabela5457.csv"
  ano13 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2015%20tabela5457.csv"
  ano14 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2016%20tabela5457.csv"
  ano15 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/area%20plantada/2017%20tabela5457.csv"

#Tabela 2003----

#Importando tabela

  h2003<-read.csv(ano1, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2003<-h2003[-1,]

  h2003<-h2003 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2003)

#Reorganizando as colunas:

  h2003 <- h2003[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2003")

  write.table(h2003,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano1,h2003)

#Tabela 2004----

#Importando tabela

  h2004<-read.csv(ano2, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2004<-h2004[-1,]

  h2004<-h2004 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2004)

#Reorganizando as colunas:

  h2004 <- h2004[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2004")

  write.table(h2004,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano2,h2004)

#Tabela 2005----

#Importando tabela

  h2005<-read.csv(ano3, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2005<-h2005[-1,]

  h2005<-h2005 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2005)

#Reorganizando as colunas:

  h2005 <- h2005[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2005")

  write.table(h2005,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano3,h2005)

#Tabela 2006----

#Importando tabela

  h2006<-read.csv(ano4, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2006<-h2006[-1,]

  h2006<-h2006 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2006)

#Reorganizando as colunas:

  h2006 <- h2006[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2006")

  write.table(h2006,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano4,h2006)

#Tabela 2007----

#Importando tabela

  h2007<-read.csv(ano5, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2007<-h2007[-1,]

  h2007<-h2007 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2007)

#Reorganizando as colunas:

  h2007 <- h2007[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2007")

  write.table(h2007,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano5,h2007)

#Tabela 2008----

#Importando tabela

  h2008<-read.csv(ano6, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2008<-h2008[-1,]

  h2008<-h2008 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2008)

#Reorganizando as colunas:

  h2008 <- h2008[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2008")

  write.table(h2008,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano6,h2008)

#Tabela 2009----

#Importando tabela

  h2009<-read.csv(ano7, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2009<-h2009[-1,]

  h2009<-h2009 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2009)

#Reorganizando as colunas:

  h2009 <- h2009[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2009")

  write.table(h2009,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano7,h2009)

#Tabela 2010----

#Importando tabela

  h2010<-read.csv(ano8, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2010<-h2010[-1,]

  h2010<-h2010 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2010)

#Reorganizando as colunas:

  h2010 <- h2010[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2010")

  write.table(h2010,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano8,h2010)

#Tabela 2011----

#Importando tabela

  h2011<-read.csv(ano9, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2011<-h2011[-1,]

  h2011<-h2011 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2011)

#Reorganizando as colunas:

  h2011 <- h2011[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

  write.table(h2011,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano9,h2011)

#Tabela 2012----

#Importando tabela

  h2012<-read.csv(ano10, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2012<-h2012[-1,]

  h2012<-h2012 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2012)

#Reorganizando as colunas:

  h2012 <- h2012[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2012")

  write.table(h2012,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano10,h2012)

#Tabela 2013----

#Importando tabela

  h2013<-read.csv(ano11, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2013<-h2013[-1,]

  h2013<-h2013 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2013)

#Reorganizando as colunas:

  h2013 <- h2013[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2013")

  write.table(h2013,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano11,h2013)

#Tabela 2014----

#Importando tabela

  h2014<-read.csv(ano12, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2014<-h2014[-1,]

  h2014<-h2014 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2014)

#Reorganizando as colunas:

  h2014 <- h2014[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2014")

  write.table(h2014,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano12,h2014)

#Tabela 2015----

#Importando tabela

  h2015<-read.csv(ano13, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2015<-h2015[-1,]

  h2015<-h2015 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2015)

#Reorganizando as colunas:

  h2015 <- h2015[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2015")

  write.table(h2015,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano13,h2015)

#Tabela 2016----

#Importando tabela

  h2016<-read.csv(ano14, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2016<-h2016[-1,]
  
  h2016<-h2016 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2016)

#Reorganizando as colunas:

  h2016 <- h2016[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2016")

  write.table(h2016,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano14,h2016)

#Tabela 2017----

#Importando tabela

  h2017<-read.csv(ano15, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

  h2017<-h2017[-1,]

  h2017<-h2017 %>% 
    mutate(Munic�pio=toupper(Munic�pio)) %>% 
    mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
    mutate("ano"=2017)

#Reorganizando as colunas

  h2017 <- h2017[,c(7,1,2,3,4,5,6)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

  write.table(h2017,file='area_plantada.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(ano15,h2017)