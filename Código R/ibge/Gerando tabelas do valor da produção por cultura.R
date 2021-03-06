#Carregando pacotes a serem utilizados----

if(!require(dplyr))
  install.packages("dplyr")

if(!require(stringr))
  install.packages("stringr")

#Limpando bases da mem�ria----

rm(list=ls())

#Organizando tabelas----

#Valor da produ��o por cultura em mil reais

ano1 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2003%20tabela5457.csv"
ano2 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2004%20tabela5457.csv"
ano3 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2005%20tabela5457.csv"
ano4 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2006%20tabela5457.csv"
ano5 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2007%20tabela5457.csv"
ano6 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2008%20tabela5457.csv"
ano7 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2009%20tabela5457.csv"
ano8 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2010%20tabela5457.csv"
ano9 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2011%20tabela5457.csv"
ano10 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2012%20tabela5457.csv"
ano11 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2013%20tabela5457.csv"
ano12 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2014%20tabela5457.csv"
ano13 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2015%20tabela5457.csv"
ano14 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2016%20tabela5457.csv"
ano15 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/ibge/valor%20da%20producao/2017%20tabela5457.csv"

#Tabela 2003----

#Importando tabela

v2003<-read.csv(ano1, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2003<-v2003[-1,]

v2003<-v2003 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2003)

#Reorganizando as colunas

v2003$Dend�..cacho.de.coco. <- as.numeric(v2003$Dend�..cacho.de.coco.)
v2003$Girassol..em.gr�o. <- as.numeric(v2003$Girassol..em.gr�o.)
v2003$Mamona..baga. <- as.numeric(v2003$Mamona..baga.)
v2003$Soja..em.gr�o. <- as.numeric(v2003$Soja..em.gr�o.)

v2003 <- v2003[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2003")

write.table(v2003,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano1,v2003)

#Tabela 2004----

#Importando tabela

v2004<-read.csv(ano2, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2004<-v2004[-1,]

v2004<-v2004 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2004)

#Reorganizando as colunas

v2004$Dend�..cacho.de.coco. <- as.numeric(v2004$Dend�..cacho.de.coco.)
v2004$Girassol..em.gr�o. <- as.numeric(v2004$Girassol..em.gr�o.)
v2004$Mamona..baga. <- as.numeric(v2004$Mamona..baga.)
v2004$Soja..em.gr�o. <- as.numeric(v2004$Soja..em.gr�o.)

v2004 <- v2004[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2004")

write.table(v2004,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano2,v2004)

#Tabela 2005----

#Importando tabela

v2005<-read.csv(ano3, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2005<-v2005[-1,]

v2005<-v2005 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2005)

#Reorganizando as colunas

v2005$Dend�..cacho.de.coco. <- as.numeric(v2005$Dend�..cacho.de.coco.)
v2005$Girassol..em.gr�o. <- as.numeric(v2005$Girassol..em.gr�o.)
v2005$Mamona..baga. <- as.numeric(v2005$Mamona..baga.)
v2005$Soja..em.gr�o. <- as.numeric(v2005$Soja..em.gr�o.)

v2005 <- v2005[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2005")

write.table(v2005,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano3,v2005)

#Tabela 2006----

#Importando tabela

v2006<-read.csv(ano4, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2006<-v2006[-1,]

v2006<-v2006 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2006)

#Reorganizando as colunas

v2006$Dend�..cacho.de.coco. <- as.numeric(v2006$Dend�..cacho.de.coco.)
v2006$Girassol..em.gr�o. <- as.numeric(v2006$Girassol..em.gr�o.)
v2006$Mamona..baga. <- as.numeric(v2006$Mamona..baga.)
v2006$Soja..em.gr�o. <- as.numeric(v2006$Soja..em.gr�o.)

v2006 <- v2006[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2006")

write.table(v2006,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano4,v2006)

#Tabela 2007----

#Importando tabela

v2007<-read.csv(ano5, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2007<-v2007[-1,]

v2007<-v2007 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2007)

#Reorganizando as colunas

v2007$Dend�..cacho.de.coco. <- as.numeric(v2007$Dend�..cacho.de.coco.)
v2007$Girassol..em.gr�o. <- as.numeric(v2007$Girassol..em.gr�o.)
v2007$Mamona..baga. <- as.numeric(v2007$Mamona..baga.)
v2007$Soja..em.gr�o. <- as.numeric(v2007$Soja..em.gr�o.)

v2007 <- v2007[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2007")

write.table(v2007,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano5,v2007)

#Tabela 2008----

#Importando tabela

v2008<-read.csv(ano6, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2008<-v2008[-1,]

v2008<-v2008 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2008)

#Reorganizando as colunas

v2008$Dend�..cacho.de.coco. <- as.numeric(v2008$Dend�..cacho.de.coco.)
v2008$Girassol..em.gr�o. <- as.numeric(v2008$Girassol..em.gr�o.)
v2008$Mamona..baga. <- as.numeric(v2008$Mamona..baga.)
v2008$Soja..em.gr�o. <- as.numeric(v2008$Soja..em.gr�o.)

v2008 <- v2008[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2008")

write.table(v2008,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano6,v2008)

#Tabela 2009----

#Importando tabela

v2009<-read.csv(ano7, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2009<-v2009[-1,]

v2009<-v2009 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2009)

#Reorganizando as colunas

v2009$Dend�..cacho.de.coco. <- as.numeric(v2009$Dend�..cacho.de.coco.)
v2009$Girassol..em.gr�o. <- as.numeric(v2009$Girassol..em.gr�o.)
v2009$Mamona..baga. <- as.numeric(v2009$Mamona..baga.)
v2009$Soja..em.gr�o. <- as.numeric(v2009$Soja..em.gr�o.)

v2009 <- v2009[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2009")

write.table(v2009,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano7,v2009)

#Tabela 2010----

#Importando tabela

v2010<-read.csv(ano8, header=T, sep=";",dec = ",", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2010<-v2010[-1,]

v2010<-v2010 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2010)

#Reorganizando as colunas

v2010$Dend�..cacho.de.coco. <- as.numeric(v2010$Dend�..cacho.de.coco.)
v2010$Girassol..em.gr�o. <- as.numeric(v2010$Girassol..em.gr�o.)
v2010$Mamona..baga. <- as.numeric(v2010$Mamona..baga.)
v2010$Soja..em.gr�o. <- as.numeric(v2010$Soja..em.gr�o.)

v2010 <- v2010[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2010")

write.table(v2010,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano8,v2010)

#Tabela 2011----

#Importando tabela

v2011<-read.csv(ano9, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2011<-v2011[-1,]

v2011<-v2011 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2011)

#Reorganizando as colunas

v2011$Dend�..cacho.de.coco. <- as.numeric(v2011$Dend�..cacho.de.coco.)
v2011$Girassol..em.gr�o. <- as.numeric(v2011$Girassol..em.gr�o.)
v2011$Mamona..baga. <- as.numeric(v2011$Mamona..baga.)
v2011$Soja..em.gr�o. <- as.numeric(v2011$Soja..em.gr�o.)

v2011 <- v2011[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

write.table(v2011,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano9,v2011)

#Tabela 2012----

#Importando tabela

v2012<-read.csv(ano10, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2012<-v2012[-1,]

v2012<-v2012 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2012)

#Reorganizando as colunas

v2012$Dend�..cacho.de.coco. <- as.numeric(v2012$Dend�..cacho.de.coco.)
v2012$Girassol..em.gr�o. <- as.numeric(v2012$Girassol..em.gr�o.)
v2012$Mamona..baga. <- as.numeric(v2012$Mamona..baga.)
v2012$Soja..em.gr�o. <- as.numeric(v2012$Soja..em.gr�o.)

v2012 <- v2012[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2012")

write.table(v2012,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano10,v2012)

#Tabela 2013----

#Importando tabela

v2013<-read.csv(ano11, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2013<-v2013[-1,]

v2013<-v2013 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2013)

#Reorganizando as colunas

v2013$Dend�..cacho.de.coco. <- as.numeric(v2013$Dend�..cacho.de.coco.)
v2013$Girassol..em.gr�o. <- as.numeric(v2013$Girassol..em.gr�o.)
v2013$Mamona..baga. <- as.numeric(v2013$Mamona..baga.)
v2013$Soja..em.gr�o. <- as.numeric(v2013$Soja..em.gr�o.)

v2013 <- v2013[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2013")

write.table(v2013,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano11,v2013)

#Tabela 2014----

#Importando tabela

v2014<-read.csv(ano12, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2014<-v2014[-1,]

v2014<-v2014 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2014)

#Reorganizando as colunas

v2014$Dend�..cacho.de.coco. <- as.numeric(v2014$Dend�..cacho.de.coco.)
v2014$Girassol..em.gr�o. <- as.numeric(v2014$Girassol..em.gr�o.)
v2014$Mamona..baga. <- as.numeric(v2014$Mamona..baga.)
v2014$Soja..em.gr�o. <- as.numeric(v2014$Soja..em.gr�o.)

v2014 <- v2014[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2014")

write.table(v2014,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano12,v2014)

#Tabela 2015----

#Importando tabela

v2015<-read.csv(ano13, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2015<-v2015[-1,]

v2015<-v2015 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2015)

#Reorganizando as colunas

v2015$Dend�..cacho.de.coco. <- as.numeric(v2015$Dend�..cacho.de.coco.)
v2015$Girassol..em.gr�o. <- as.numeric(v2015$Girassol..em.gr�o.)
v2015$Mamona..baga. <- as.numeric(v2015$Mamona..baga.)
v2015$Soja..em.gr�o. <- as.numeric(v2015$Soja..em.gr�o.)

v2015 <- v2015[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2015")

write.table(v2015,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano13,v2015)

#Tabela 2016----

#Importando tabela

v2016<-read.csv(ano14, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2016<-v2016[-1,]

v2016<-v2016 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2016)

#Reorganizando as colunas

v2016$Dend�..cacho.de.coco. <- as.numeric(v2016$Dend�..cacho.de.coco.)
v2016$Girassol..em.gr�o. <- as.numeric(v2016$Girassol..em.gr�o.)
v2016$Mamona..baga. <- as.numeric(v2016$Mamona..baga.)
v2016$Soja..em.gr�o. <- as.numeric(v2016$Soja..em.gr�o.)

v2016 <- v2016[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2016")

write.table(v2016,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano14,v2016)

#Tabela 2017----

#Importando tabela

v2017<-read.csv(ano15, header=T, sep=";",dec = ".", skip = 3,nrows = 5564,
                na.strings = c("-","..."), encoding = "UTF-8")

v2017<-v2017[-1,]

v2017<-v2017 %>% 
  mutate(Munic�pio=toupper(Munic�pio)) %>% 
  mutate(Munic�pio=chartr("�����������'-", "AEIOUAOAEOC  ", Munic�pio)) %>%
  mutate("ano"=2017)

#Reorganizando as colunas

v2017$Dend�..cacho.de.coco. <- as.numeric(v2017$Dend�..cacho.de.coco.)
v2017$Girassol..em.gr�o. <- as.numeric(v2017$Girassol..em.gr�o.)
v2017$Mamona..baga. <- as.numeric(v2017$Mamona..baga.)
v2017$Soja..em.gr�o. <- as.numeric(v2017$Soja..em.gr�o.)

v2017 <- v2017[,c(7,1,2,3,4,5,6)]

#Exportando tabela

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

write.table(v2017,file='valor_producao.csv',sep=";",dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(ano15,v2017)