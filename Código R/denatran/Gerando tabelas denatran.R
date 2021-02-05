#Carregando pacotes a setem utilizados----

  if(!require(dplyr))
    install.packages('dplyr')

  if(!require(readxl))
    install.packages('readxl')

  if(!require(stringr))
    install.packages('stringr')

#Limpando bases da memória----

  rm(list=ls())

#Tabela 2003----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2003 <- read_excel("Frota_Mun_Dez_03t.xls")

  d2003 <- d2003 %>%
    mutate(MUNICÍPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICÍPIO)) %>%
    mutate("chave"=str_c(MUNICÍPIO," ","(", UF,")")) %>% 
    mutate("ano"=2003)

  d2003 <- d2003[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2003) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2003 <- d2003 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2003$CAMINHAO+d2003$MICRO_ONIBUS+d2003$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2003")

  write.table(d2003,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2003)

#Tabela 2004----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2004 <- read_excel("Frota Munic 122004 Internet.xls",skip=2)

  d2004 <- d2004 %>% 
    mutate(MUNICÍPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICÍPIO)) %>%
    mutate("chave"=str_c(MUNICÍPIO," ","(", UF,")")) %>% 
    mutate("ano"=2004)

  d2004 <- d2004[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2004) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2004 <- d2004 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2004$CAMINHAO+d2004$MICRO_ONIBUS+d2004$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2004")

  write.table(d2004,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2004)

#Tabela 2005----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2005 <- read_excel("Frota Munic 122005 Internet.xls",skip=2)

  d2005 <- d2005 %>% 
    mutate(MUNICÍPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICÍPIO)) %>%
    mutate("chave"=str_c(MUNICÍPIO," ","(", UF,")")) %>% 
    mutate("ano"=2005)

  d2005 <- d2005[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2005) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2005 <- d2005 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2005$CAMINHAO+d2005$MICRO_ONIBUS+d2005$ONIBUS))

#Exportando tabela
  
  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2005")

  write.table(d2005,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2005)

#Tabela 2006----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2006 <- read_excel("Frota Munic DEZ2006 Internet.xls", 
                    sheet = "DEZ_2006", skip = 2)

  d2006 <- d2006 %>% 
    mutate(MUNICÍPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICÍPIO)) %>%
    mutate("chave"=str_c(MUNICÍPIO," ","(", UF,")")) %>% 
    mutate("ano"=2006)

  d2006 <- d2006[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2006) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2006 <- d2006 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2006$CAMINHAO+d2006$MICRO_ONIBUS+d2006$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2006")

  write.table(d2006,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2006)

#Tabela 2007----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2007 <- read_excel("Frota Munic Dez2007 Internet.xls", 
                    sheet = "DEZ_2007", skip = 2)

  d2007 <- d2007 %>% 
    mutate(MUNICÍPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICÍPIO)) %>%
    mutate("chave"=str_c(MUNICÍPIO," ","(", UF,")")) %>% 
    mutate("ano"=2007)

  d2007 <- d2007[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2007) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2007 <- d2007 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2007$CAMINHAO+d2007$MICRO_ONIBUS+d2007$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2007")

  write.table(d2007,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2007)

#Tabela 2008----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2008 <- read_excel("Frota Munic Dez2008.xls", 
                    sheet = "DEZ_2008", skip = 2)

  d2008 <- d2008 %>%
    mutate(MUNICÍPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICÍPIO)) %>%
    mutate("chave"=str_c(MUNICÍPIO," ","(", UF,")")) %>% 
    mutate("ano"=2008)

  d2008 <- d2008[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2008) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2008 <- d2008 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2008$CAMINHAO+d2008$MICRO_ONIBUS+d2008$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2008")

  write.table(d2008,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2008)

#Tabela 2009----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2009 <- read_excel("Frota Munic Dez2009.xls", 
                    sheet = "DEZ_2009", skip = 2)

  d2009 <- d2009 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2009)

  d2009 <- d2009[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2009) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2009 <- d2009 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2009$CAMINHAO+d2009$MICRO_ONIBUS+d2009$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2009")

  write.table(d2009,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2009)

#Tabela 2010----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2010 <- read_excel("Frota Munic DEZ2010.xls", 
                    sheet = "DEZ_2010", skip = 2)

  d2010 <- d2010 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2010)

  d2010 <- d2010[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2010) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2010 <- d2010 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2010$CAMINHAO+d2010$MICRO_ONIBUS+d2010$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2010")

  write.table(d2010,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2010)

#Tabela 2011----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2011 <- read_excel("Frota Munic. DEZ.2011.xls", 
                    sheet = "DEZ_2011", skip = 3)

  d2011 <- d2011 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2011)

  d2011 <- d2011[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2011) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2011 <- d2011 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>%
    mutate("total"=(d2011$CAMINHAO+d2011$MICRO_ONIBUS+d2011$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

  write.table(d2011,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2011)

#Tabela 2012----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2012 <- read_excel("Frota Munic.DEZ.2012.xls", 
                    sheet = "DEZ_2012", skip = 3)

  d2012 <- d2012 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2012)

  d2012 <- d2012[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2012) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2012 <- d2012 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2012$CAMINHAO+d2012$MICRO_ONIBUS+d2012$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2012")

  write.table(d2012,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2012)

#Tabela 2013----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2013 <- read_excel("Frota Munic.DEZ.2013.xls", 
                    sheet = "DEZ_2013", skip = 3)
  
  d2013 <- d2013 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2013)

  d2013 <- d2013[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2013) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2013 <- d2013 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2013$CAMINHAO+d2013$MICRO_ONIBUS+d2013$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2013")

  write.table(d2013,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2013)

#Tabela 2014---

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2014 <- read_excel("frota_por_municipio_e_tipo_dez_2014.xlsx", 
                    sheet = "DEZ_2014", skip = 3)

  d2014 <- d2014 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2014)

  d2014 <- d2014[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2014) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2014 <- d2014 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2014$CAMINHAO+d2014$MICRO_ONIBUS+d2014$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2014")

  write.table(d2014,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2014)

#Tabela 2015----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2015 <- read_excel("Frota_por_Municipio_e_Tipo-DEZ_15.xls", 
                    sheet = "JUL_2015", skip = 3)

  d2015 <- d2015 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2015)

  d2015 <- d2015[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2015) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2015 <- d2015 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2015$CAMINHAO+d2015$MICRO_ONIBUS+d2015$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2015")

  write.table(d2015,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2015)

#Tabela 2016----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2016 <- read_excel("frota_por_municipio_e_tipo-dez_16.xlsx", 
                    sheet = "DEZ_2016", skip = 3)

  d2016 <- d2016 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2016)

  d2016 <- d2016[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2016) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2016 <- d2016 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2016$CAMINHAO+d2016$MICRO_ONIBUS+d2016$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2016")

  write.table(d2016,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2016)

#Tabela 2017----

#Diretório local de trabalho

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/denatran")

#Importando tabela

  d2017 <- read_excel("frota_munic_dezembro_2017.xls", 
                    sheet = "DEZ_2017", skip = 2)

  d2017 <- d2017 %>% 
    mutate(MUNICIPIO=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>%
    mutate("chave"=str_c(MUNICIPIO," ","(", UF,")")) %>% 
    mutate("ano"=2017)

  d2017 <- d2017[,c(26,25,1,2,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,3)]

  names(d2017) <- c("ano","chave","UF","MUNICIPIO","AUTOMOVEL","BONDE","CAMINHAO",
                  "CAMINHAO TRATOR","CAMINHONETE","CAMIONETA","CHASSI PLATAFORMA",
                  "CICLOMOTOR","MICRO_ONIBUS","MOTOCICLETA","MOTONETA","ONIBUS",
                  "QUADRICICLO","REBOQUE","SEMI-REBOQUE","SIDE-CAR","OUTROS",
                  "TRATOR ESTEIRA", "TRATOR RODAS","TRICICLO","UTILITARIOS",
                  "TOTAL")

  d2017 <- d2017 %>% 
    select("ano","chave","UF","MUNICIPIO","CAMINHAO","MICRO_ONIBUS","ONIBUS") %>% 
    mutate("total"=(d2017$CAMINHAO+d2017$MICRO_ONIBUS+d2017$ONIBUS))

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

  write.table(d2017,file='demanda.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(d2017)