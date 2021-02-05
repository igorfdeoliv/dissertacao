#Carregando pacotes a serem utilizados----

  if(!require(dplyr))
    install.packages('dplyr')

  if(!require(readxl))
    install.packages('readxl')

  if(!require(stringr))
    install.packages('stringr')

#Limpando bases da memÛria----

  rm(list=ls())

#Organizando tabelas

  ano11 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/bacen/2013%20RelatorioQuantidade%20e%20Valor%20dos%20Contratos%20por%20Munic%C3%ADpio%2009-01-2021.csv"
  ano12 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/bacen/2014%20RelatorioQuantidade%20e%20Valor%20dos%20Contratos%20por%20Munic%C3%ADpio%2007-01-2021.csv"
  ano13 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/bacen/2015%20RelatorioQuantidade%20e%20Valor%20dos%20Contratos%20por%20Munic%C3%ADpio%2007-01-2021.csv"
  ano14 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/bacen/2016%20RelatorioQuantidade%20e%20Valor%20dos%20Contratos%20por%20Munic%C3%ADpio%2007-01-2021.csv"
  ano15 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/bacen/2017%20RelatorioQuantidade%20e%20Valor%20dos%20Contratos%20por%20Munic%C3%ADpio%2008-01-2021.csv"

#Tabela 2003----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2003 <- read_excel("~/GitHub/dissertacao/Dataset/bacen/2003 rel517.xlsx", 
                    sheet = "rel517", skip = 8) %>% 
    filter(ATIVIDADE!="NA") %>% 
    filter(ATIVIDADE!="Total") %>%
    mutate(MUNICÕPIO=toupper(MUNICÕPIO)) %>% 
    mutate(MUNICÕPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICÕPIO)) %>% 
    mutate("chave"=str_c(UF," ",MUNICÕPIO))

  names(c2003) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total","chave")

  x <- c2003 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2003 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2003 <- full_join(x,y) %>% 
    mutate("ano"=2003)

  c2003 <- c2003 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2003 <- c2003[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2003")

  write.table(c2003,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2003)

#Tabela 2004----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2004 <- read_excel("~/GitHub/dissertacao/Dataset/bacen/2004 rel517.xlsx", 
                    skip = 5) %>% 
    filter(ATIVIDADE!="NA") %>% 
    filter(ATIVIDADE!="Total") %>%
    mutate(MUNICIPIO=toupper(MUNICIPIO)) %>% 
    mutate(MUNICIPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICIPIO)) %>% 
    mutate("chave"=str_c(UF," ",MUNICIPIO))

  names(c2004) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total","chave")

  x <- c2004 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2004 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2004 <- full_join(x,y) %>% 
    mutate("ano"=2004)

  c2004 <- c2004 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2004 <- c2004[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2004")

  write.table(c2004,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2004)

#Tabela 2005----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2005 <- read_excel("~/GitHub/dissertacao/Dataset/bacen/2005 rel517.xlsx", 
                    skip = 6) %>% 
    filter(ATIVIDADE!="NA") %>% 
    filter(ATIVIDADE!="Total") %>% 
    mutate(MUNICIPIO=toupper(MUNICIPIO)) %>% 
    mutate(MUNICIPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICIPIO))

  names(c2005) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total")

#Recodificando as linhas dos estados para criaÁ„o de vari·vel chave----

  c2005[c2005 == "ACRE"] <- "AC"
  c2005[c2005 == "ALAGOAS"] <- "AL"
  c2005[c2005 == "AMAP¡"] <- "AP"
  c2005[c2005 == "AMAZONAS"] <- "AM"
  c2005[c2005 == "BAHIA"] <- "BA"
  c2005[c2005 == "CEAR¡"] <- "CE"
  c2005[c2005 == "DISTRITO FEDERAL"] <- "DF"
  c2005[c2005 == "ESPÕRITO SANTO"] <- "ES"
  c2005[c2005 == "GOI¡S"] <- "GO"
  c2005[c2005 == "MARANH√O"] <- "MA"
  c2005[c2005 == "MATO GROSSO"] <- "MT"
  c2005[c2005 == "MATO GROSSO DO SUL"] <- "MS"
  c2005[c2005 == "MINAS GERAIS"] <- "MG"
  c2005[c2005 == "PAR¡"] <- "PA"
  c2005[c2005 == "PARAÕBA"] <- "PB"
  c2005[c2005 == "PARAN¡"] <- "PR"
  c2005[c2005 == "PERNAMBUCO"] <- "PE"
  c2005[c2005 == "PIAUÕ"] <- "PI"
  c2005[c2005 == "RIO DE JANEIRO"] <- "RJ"
  c2005[c2005 == "RIO GRANDE DO NORTE"] <- "RN"
  c2005[c2005 == "RIO GRANDE DO SUL"] <- "RS"
  c2005[c2005 == "ROND‘NIA"] <- "RO"
  c2005[c2005 == "RORAIMA"] <- "RR"
  c2005[c2005 == "SANTA CATARINA"] <- "SC"
  c2005[c2005 == "S√O PAULO"] <- "SP"
  c2005[c2005 == "SERGIPE"] <- "SE"
  c2005[c2005 == "TOCANTINS"] <- "TO"

  c2005 <- c2005 %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2005 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2005 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2005 <- full_join(x,y) %>% 
    mutate("ano"=2005)

  c2005 <- c2005 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2005 <- c2005[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2005")

  write.table(c2005,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2005)

#Tabela 2006----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2006 <- read_excel("~/GitHub/dissertacao/Dataset/bacen/2006 rel517.xlsx", 
                    sheet = "rel517", skip = 7) %>% 
    filter(ATIVIADE!="NA") %>% 
    filter(ATIVIADE!="Total") %>% 
    mutate(MUNICÕPIO=toupper(MUNICÕPIO)) %>% 
    mutate(MUNICÕPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICÕPIO))

  names(c2006) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total")

  c2006[c2006 == "ACRE"] <- "AC"
  c2006[c2006 == "ALAGOAS"] <- "AL"
  c2006[c2006 == "AMAP¡"] <- "AP"
  c2006[c2006 == "AMAZONAS"] <- "AM"
  c2006[c2006 == "BAHIA"] <- "BA"
  c2006[c2006 == "CEAR¡"] <- "CE"
  c2006[c2006 == "DISTRITO FEDERAL"] <- "DF"
  c2006[c2006 == "ESPÕRITO SANTO"] <- "ES"
  c2006[c2006 == "GOI¡S"] <- "GO"
  c2006[c2006 == "MARANH√O"] <- "MA"
  c2006[c2006 == "MATO GROSSO"] <- "MT"
  c2006[c2006 == "MATO GROSSO DO SUL"] <- "MS"
  c2006[c2006 == "MINAS GERAIS"] <- "MG"
  c2006[c2006 == "PAR¡"] <- "PA"
  c2006[c2006 == "PARAÕBA"] <- "PB"
  c2006[c2006 == "PARAN¡"] <- "PR"
  c2006[c2006 == "PERNAMBUCO"] <- "PE"
  c2006[c2006 == "PIAUÕ"] <- "PI"
  c2006[c2006 == "RIO DE JANEIRO"] <- "RJ"
  c2006[c2006 == "RIO GRANDE DO NORTE"] <- "RN"
  c2006[c2006 == "RIO GRANDE DO SUL"] <- "RS"
  c2006[c2006 == "ROND‘NIA"] <- "RO"
  c2006[c2006 == "RORAIMA"] <- "RR"
  c2006[c2006 == "SANTA CATARINA"] <- "SC"
  c2006[c2006 == "S√O PAULO"] <- "SP"
  c2006[c2006 == "SERGIPE"] <- "SE"
  c2006[c2006 == "TOCANTINS"] <- "TO"

  c2006 <- c2006 %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2006 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2006 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2006 <- full_join(x,y) %>% 
    mutate("ano"=2006)

  c2006 <- c2006 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2006 <- c2006[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2006")

  write.table(c2006,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2006)

#Tabela 2007----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2007 <- read_excel("~/GitHub/dissertacao/Dataset/bacen/2007 rel517.xlsx", 
                    sheet = "rel517", skip = 5) %>% 
    filter(ATIVIDADE!="NA") %>% 
    filter(ATIVIDADE!="Total") %>% 
    mutate(MUNICIPIO=toupper(MUNICIPIO)) %>% 
    mutate(MUNICIPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICIPIO))

  names(c2007) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total")

  c2007[c2007 == "ACRE"] <- "AC"
  c2007[c2007 == "ALAGOAS"] <- "AL"
  c2007[c2007 == "AMAP¡"] <- "AP"
  c2007[c2007 == "AMAZONAS"] <- "AM"
  c2007[c2007 == "BAHIA"] <- "BA"
  c2007[c2007 == "CEAR¡"] <- "CE"
  c2007[c2007 == "DISTRITO FEDERAL"] <- "DF"
  c2007[c2007 == "ESPÕRITO SANTO"] <- "ES"
  c2007[c2007 == "GOI¡S"] <- "GO"
  c2007[c2007 == "MARANH√O"] <- "MA"
  c2007[c2007 == "MATO GROSSO"] <- "MT"
  c2007[c2007 == "MATO GROSSO DO SUL"] <- "MS"
  c2007[c2007 == "MINAS GERAIS"] <- "MG"
  c2007[c2007 == "PAR¡"] <- "PA"
  c2007[c2007 == "PARAÕBA"] <- "PB"
  c2007[c2007 == "PARAN¡"] <- "PR"
  c2007[c2007 == "PERNAMBUCO"] <- "PE"
  c2007[c2007 == "PIAUÕ"] <- "PI"
  c2007[c2007 == "RIO DE JANEIRO"] <- "RJ"
  c2007[c2007 == "RIO GRANDE DO NORTE"] <- "RN"
  c2007[c2007 == "RIO GRANDE DO SUL"] <- "RS"
  c2007[c2007 == "ROND‘NIA"] <- "RO"
  c2007[c2007 == "RORAIMA"] <- "RR"
  c2007[c2007 == "SANTA CATARINA"] <- "SC"
  c2007[c2007 == "S√O PAULO"] <- "SP"
  c2007[c2007 == "SERGIPE"] <- "SE"
  c2007[c2007 == "TOCANTINS"] <- "TO"

  c2007 <- c2007 %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2007 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2007 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))
  
  c2007 <- full_join(x,y) %>% 
    mutate("ano"=2007)

  c2007 <- c2007 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2007 <- c2007[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2007")

  write.table(c2007,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2007)

#Tabela 2008----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  df<- read_xlsx("2008 rel517.xlsx", skip = 7,n_max = 22071,
               col_names = c("mun","custeio_cont", "custeio_val", "inv_cont", "inv_val",
                             "com_cont", "com_val","tot_cont", "tot_val"))

  df<-df %>% 
    mutate(i=seq(1,22071,1)) %>% 
    mutate(custeio_cont=ifelse(mun[i+2]=="Pecu·ria",custeio_cont[i+3],NA)) %>% 
    mutate(custeio_val=ifelse(mun[i+2]=="Pecu·ria",custeio_val[i+3],NA)) %>%    
    mutate(inv_cont=ifelse(mun[i+2]=="Pecu·ria",inv_cont[i+3],NA)) %>% 
    mutate(inv_val=ifelse(mun[i+2]=="Pecu·ria",inv_val[i+3],NA)) %>%
    mutate(com_cont=ifelse(mun[i+2]=="Pecu·ria",com_cont[i+3],NA)) %>% 
    mutate(com_val=ifelse(mun[i+2]=="Pecu·ria",com_val[i+3],NA)) %>%    
    mutate(tot_cont=ifelse(mun[i+2]=="Pecu·ria",tot_cont[i+3],NA)) %>% 
    mutate(tot_val=ifelse(mun[i+2]=="Pecu·ria",tot_val[i+3],NA)) %>% 
    mutate(estado=ifelse(mun[i+2]=="AgrÌcola"& mun!="Total",1,0)) %>% 
    filter(mun!="AgrÌcola" & mun!="Pecu·ria" & mun!="Total") %>% 
    mutate(i=seq(1,5534,1)) %>% 
    mutate(estado=ifelse(estado==1,i,0))

  est<-df %>% 
    select("mun","estado") %>% 
    filter(estado>=1)

  obs<-data.frame(mun=c("Total"),
                estado=c(5534))

  est<-rbind(est,obs)

  rm(obs)
  
  est<-est %>% 
    mutate(i=seq(1,28,1)) %>% 
    mutate(num=estado[i+1]-estado[i]-1)

  est<-est[1:27,]
  attach(est)

  estado<-rep(mun[i],num[i])

  df<-df %>%
    filter(estado==0) %>% 
    select(-"i",-"estado")

  df<-cbind(estado,df)

  rm(est,estado)

  df_estado<-data.frame(sigla=c("AC","AL","AP","AM","BA","CE","DF","ES","GO",
                              "MA","MT","MS","MG","PA","PB","PR","PE","PI",
                              "RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
                      estado=c("ACRE","ALAGOAS","AMAP¡","AMAZONAS","BAHIA","CEAR¡",
                               "DISTRITO FEDERAL", "ESPÕRITO SANTO", "GOI¡S", "MARANH√O",
                               "MATO GROSSO", "MATO GROSSO DO SUL", "MINAS GERAIS", "PAR¡",
                               "PARAÕBA", "PARAN¡", "PERNAMBUCO", "PIAUÕ", "RIO DE JANEIRO",
                               "RIO GRANDE DO NORTE", "RIO GRANDE DO SUL", "ROND‘NIA", "RORAIMA",
                               "SANTA CATARINA", "S√O PAULO", "SERGIPE", "TOCANTINS"))

  df<-right_join(df_estado, df, c("estado"))

  names(df) <- c("estado","uf","municipio","c.custeio","v.custeio",
               "c.investimento","v.investimento","c.comercializacao",
               "v.comercializacao","total.contratos","valores.totais")

  c2008 <- df %>% 
    select("estado","municipio","total.contratos","valores.totais") %>% 
    mutate(municipio=toupper(municipio)) %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",municipio)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")")) %>% 
    mutate("ano"=2008)

  rm(df,df_estado)

  c2008 <- c2008[,c(6,5,1,2,3,4)]  

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2008")

  write.table(c2008,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(c2008)

#Tabela 2009----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  df<- read_xlsx("2009 rel517.xlsx", skip = 6,n_max = 22083,  
               col_names = c("mun","custeio_cont", "custeio_val", "inv_cont", "inv_val",
                             "com_cont", "com_val","tot_cont", "tot_val"))

  df<-df %>% 
    mutate(i=seq(1,22083,1)) %>% 
    mutate(custeio_cont=ifelse(mun[i+2]=="Pecu·ria",custeio_cont[i+3],NA)) %>% 
    mutate(custeio_val=ifelse(mun[i+2]=="Pecu·ria",custeio_val[i+3],NA)) %>%    
    mutate(inv_cont=ifelse(mun[i+2]=="Pecu·ria",inv_cont[i+3],NA)) %>% 
    mutate(inv_val=ifelse(mun[i+2]=="Pecu·ria",inv_val[i+3],NA)) %>%
    mutate(com_cont=ifelse(mun[i+2]=="Pecu·ria",com_cont[i+3],NA)) %>% 
    mutate(com_val=ifelse(mun[i+2]=="Pecu·ria",com_val[i+3],NA)) %>%    
    mutate(tot_cont=ifelse(mun[i+2]=="Pecu·ria",tot_cont[i+3],NA)) %>% 
    mutate(tot_val=ifelse(mun[i+2]=="Pecu·ria",tot_val[i+3],NA)) %>% 
    mutate(estado=ifelse(mun[i+2]=="AgrÌcola"& mun!="Total",1,0)) %>% 
    mutate(regra=ifelse(estado[i+1]==1,1,0)) %>% 
    filter(mun!="AgrÌcola" & mun!="Pecu·ria" & mun!="Total" & regra==0) %>% 
    select(-"regra") %>% 
    mutate(i=seq(1,5534,1)) %>% 
    mutate(estado=ifelse(estado==1,i,0))

  est<-df %>% 
    select("mun","estado") %>% 
    filter(estado>=1)
  
  obs<-data.frame(mun=c("Total"),
                estado=c(5535))

  est<-rbind(est,obs)

  rm(obs)

  est<-est %>% 
    mutate(i=seq(1,28,1)) %>% 
    mutate(num=estado[i+1]-estado[i]-1)

  est<-est[1:27,]
  attach(est)
  estado<-rep(mun[i],num[i])

  df<-df %>% 
    filter(estado==0) %>% 
    select(-"i",-"estado")

  df<-cbind(estado,df)

  rm(est,estado)

  df_estado<-data.frame(sigla=c("AC","AL","AP","AM","BA","CE","DF","ES","GO",
                              "MA","MT","MS","MG","PA","PB","PR","PE","PI",
                              "RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
                      estado=c("ACRE","ALAGOAS","AMAP¡","AMAZONAS","BAHIA","CEAR¡",
                               "DISTRITO FEDERAL", "ESPÕRITO SANTO", "GOI¡S", "MARANH√O",
                               "MATO GROSSO", "MATO GROSSO DO SUL", "MINAS GERAIS", "PAR¡",
                               "PARAÕBA", "PARAN¡", "PERNAMBUCO", "PIAUÕ", "RIO DE JANEIRO",
                               "RIO GRANDE DO NORTE", "RIO GRANDE DO SUL", "ROND‘NIA", "RORAIMA",
                               "SANTA CATARINA", "S√O PAULO", "SERGIPE", "TOCANTINS"))

  c2009 <- right_join(df_estado, df, c("estado"))

  names(c2009) <- c("estado","uf","municipio","c.custeio","v.custeio",
                  "c.investimento","v.investimento","c.comercializacao",
                  "v.comercializacao","total.contratos","valores.totais")

  c2009 <- c2009 %>% 
    select("estado","municipio","total.contratos","valores.totais") %>% 
    mutate(municipio=toupper(municipio)) %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",municipio)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")")) %>% 
    mutate("ano"=2009)

  rm(df,df_estado)

  c2009 <- c2009[,c(6,5,1,2,3,4)]
  
#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2009")

  write.table(c2009,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(c2009)

#Tabela 2010----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2010 <- read_excel("2010 rel517.xlsx", 
                    sheet = "rel517", skip = 5) %>% 
    filter(ATIVIDADE!="NA") %>% 
    filter(ATIVIDADE!="Total") %>% 
    mutate(MUNICÕPIO=toupper(MUNICÕPIO)) %>% 
    mutate(MUNICÕPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICÕPIO))

  names(c2010) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total")

  c2010[c2010 == "ACRE"] <- "AC"
  c2010[c2010 == "ALAGOAS"] <- "AL"
  c2010[c2010 == "AMAP¡"] <- "AP"
  c2010[c2010 == "AMAZONAS"] <- "AM"
  c2010[c2010 == "BAHIA"] <- "BA"
  c2010[c2010 == "CEAR¡"] <- "CE"
  c2010[c2010 == "DISTRITO FEDERAL"] <- "DF"
  c2010[c2010 == "ESPÕRITO SANTO"] <- "ES"
  c2010[c2010 == "GOI¡S"] <- "GO"
  c2010[c2010 == "MARANH√O"] <- "MA"
  c2010[c2010 == "MATO GROSSO"] <- "MT"
  c2010[c2010 == "MATO GROSSO DO SUL"] <- "MS"
  c2010[c2010 == "MINAS GERAIS"] <- "MG"
  c2010[c2010 == "PAR¡"] <- "PA"
  c2010[c2010 == "PARAÕBA"] <- "PB"
  c2010[c2010 == "PARAN¡"] <- "PR"
  c2010[c2010 == "PERNAMBUCO"] <- "PE"
  c2010[c2010 == "PIAUÕ"] <- "PI"
  c2010[c2010 == "RIO DE JANEIRO"] <- "RJ"
  c2010[c2010 == "RIO GRANDE DO NORTE"] <- "RN"
  c2010[c2010 == "RIO GRANDE DO SUL"] <- "RS"
  c2010[c2010 == "ROND‘NIA"] <- "RN"
  c2010[c2010 == "RORAIMA"] <- "RR"
  c2010[c2010 == "SANTA CATARINA"] <- "SC"
  c2010[c2010 == "S√O PAULO"] <- "SP"
  c2010[c2010 == "SERGIPE"] <- "SE"
  c2010[c2010 == "TOCANTINS"] <- "TO"

  c2010 <- c2010 %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2010 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2010 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2010 <- full_join(x,y) %>% 
    mutate("ano"=2010)

  c2010 <- c2010 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2010 <- c2010[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2010")

  write.table(c2010,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2010)

#Tabela 2011----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2011 <- read_excel("2011 rel517.xlsx", 
                    sheet = "rel517", skip = 6) %>% 
    filter(ATIVIDADE!="NA") %>% 
    filter(ATIVIDADE!="Total") %>% 
    mutate(MUNICÕPIO=toupper(MUNICÕPIO)) %>% 
    mutate(MUNICÕPIO=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",MUNICÕPIO)) %>% 
    mutate("chave"=str_c(UF," ",MUNICÕPIO))

  names(c2011) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total","chave")

  x <- c2011 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2011 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2011 <- full_join(x,y) %>% 
    mutate("ano"=2011)
  
  c2011 <- c2011 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2011 <- c2011[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

  write.table(c2011,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2011)

#Tabela 2012----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2012 <- read_excel("2012 rel517.xlsx", 
                    sheet = "rel5111", skip = 4) %>% 
    filter(...2!="NA") %>% 
    filter(...2!="Total") %>%
    mutate(...2=toupper(...2)) %>% 
    mutate(...2=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ",...2)) %>% 
    mutate("chave"=str_c(...1," ",...2))

  names(c2012) <- c("estado","municipio","atv","c.custeio","v.custeio",
                  "c.investimento","v.investimento",
                  "c.comercializacao","v.comercializacao","c.total",
                  "v.total","chave")

  x <- c2012 %>%
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=TRUE))

  y <- c2012 %>%  group_by(chave) %>% 
    summarise(valores.totais=sum(v.total,na.rm=TRUE))

  c2012 <- full_join(x,y) %>% 
    mutate("ano"=2012)

  c2012 <- c2012 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2012 <- c2012[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2012")

  write.table(c2012,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,c2012)

#Tabela 2013----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2013 <- read.csv(ano11,encoding="UTF-8")

  names(c2013) <- c("estado","municipio","cod_mun","atv","drop1",
                  "c.custeio","v.custeio","drop2","c.investimento",
                  "v.investimento","c.comercializacao","v.comercializacao",
                  "drop3","drop4","drop5","c.total","v.total")

  c2013 <- c2013 %>% 
    select("estado","municipio","cod_mun","atv","c.custeio","v.custeio",
         "c.investimento","v.investimento","c.comercializacao",
         "v.comercializacao","c.total","v.total") %>% 
    filter(atv!="NA") %>% 
    filter(atv!="Total") %>% 
    filter(atv!="Total Geral") %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ", municipio)) %>%
    mutate(v.total=str_replace_all(v.total,"[.]","")) %>%
    mutate(v.total=str_replace_all(v.total,"[,]",".")) %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2013 %>% 
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=T))

  y <- c2013 %>% 
    group_by(chave) %>% 
    summarise(valores.totais=sum(as.numeric(v.total),na.rm=T))

  c2013 <- full_join(x,y) %>% 
    mutate("ano"=2013)

  c2013 <- c2013 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2013 <- c2013[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2013")

  write.table(c2013,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,ano11,c2013)

#Tabela 2014----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2014 <- read.csv(ano12,encoding="UTF-8")

  names(c2014) <- c("estado","municipio","cod_mun","atv","drop1",
                  "c.custeio","v.custeio","drop2","c.investimento",
                  "v.investimento","c.comercializacao","v.comercializacao",
                  "drop3","drop4","drop5","c.total","v.total")

  c2014 <- c2014 %>% 
    select("estado","municipio","cod_mun","atv","c.custeio","v.custeio",
         "c.investimento","v.investimento","c.comercializacao",
         "v.comercializacao","c.total","v.total") %>% 
    filter(atv!="NA") %>% 
    filter(atv!="Total") %>% 
    filter(atv!="Total Geral") %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ", municipio)) %>%
    mutate(v.total=str_replace_all(v.total,"[.]","")) %>%
    mutate(v.total=str_replace_all(v.total,"[,]",".")) %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2014 %>% 
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=T))

  y <- c2014 %>% 
    group_by(chave) %>% 
    summarise(valores.totais=sum(as.numeric(v.total),na.rm=T))

  c2014 <- full_join(x,y) %>% 
    mutate("ano"=2014)

  c2014 <- c2014 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2014 <- c2014[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2014")

  write.table(c2014,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,ano12,c2014)

#Tabela 2015----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2015 <- read.csv(ano13,encoding="UTF-8")

  names(c2015) <- c("estado","municipio","cod_mun","atv","drop1",
                  "c.custeio","v.custeio","drop2","c.investimento",
                  "v.investimento","c.comercializacao","v.comercializacao",
                  "drop3","drop4","drop5","c.total","v.total")

  c2015 <- c2015 %>% 
    select("estado","municipio","cod_mun","atv","c.custeio","v.custeio",
         "c.investimento","v.investimento","c.comercializacao",
         "v.comercializacao","c.total","v.total") %>% 
    filter(atv!="NA") %>% 
    filter(atv!="Total") %>% 
    filter(atv!="Total Geral") %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ", municipio)) %>%
    mutate(v.total=str_replace_all(v.total,"[.]","")) %>%
    mutate(v.total=str_replace_all(v.total,"[,]",".")) %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2015 %>% 
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=T))

  y <- c2015 %>% 
    group_by(chave) %>% 
    summarise(valores.totais=sum(as.numeric(v.total),na.rm=T))

  c2015 <- full_join(x,y) %>% 
    mutate("ano"=2015)

  c2015 <- c2015 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2015 <- c2015[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2015")

  write.table(c2015,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,ano13,c2015)

#Tabela 2016----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2016 <- read.csv(ano14,encoding="UTF-8")

  names(c2016) <- c("estado","municipio","cod_mun","atv","drop1",
                  "c.custeio","v.custeio","drop2","c.investimento",
                  "v.investimento","c.comercializacao","v.comercializacao",
                  "drop3","drop4","drop5","c.total","v.total")

  c2016 <- c2016 %>% 
    select("estado","municipio","cod_mun","atv","c.custeio","v.custeio",
         "c.investimento","v.investimento","c.comercializacao",
         "v.comercializacao","c.total","v.total") %>% 
    filter(atv!="NA") %>% 
    filter(atv!="Total") %>% 
    filter(atv!="Total Geral") %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ", municipio)) %>%
    mutate(v.total=str_replace_all(v.total,"[.]","")) %>%
    mutate(v.total=str_replace_all(v.total,"[,]",".")) %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2016 %>% 
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=T))

  y <- c2016 %>% 
    group_by(chave) %>% 
    summarise(valores.totais=sum(as.numeric(v.total),na.rm=T))

  c2016 <- full_join(x,y) %>% 
    mutate("ano"=2016)

  c2016 <- c2016 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2016 <- c2016[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2016")

  write.table(c2016,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,ano14,c2016)

#Tabela 2017----

#DiretÛrio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/bacen")

#Importando tabela

  c2017 <- read.csv(ano15,encoding="UTF-8")

  names(c2017) <- c("estado","municipio","cod_mun","atv","drop1",
                  "c.custeio","v.custeio","drop2","c.investimento",
                  "v.investimento","c.comercializacao","v.comercializacao",
                  "drop3","drop4","drop5","c.total","v.total")

  c2017 <- c2017 %>% 
    select("estado","municipio","cod_mun","atv","c.custeio","v.custeio",
         "c.investimento","v.investimento","c.comercializacao",
         "v.comercializacao","c.total","v.total") %>% 
    filter(atv!="NA") %>% 
    filter(atv!="Total") %>% 
    filter(atv!="Total Geral") %>% 
    mutate(municipio=chartr("¡…Õ”⁄√’¬ ‘«'-", "AEIOUAOAEOC  ", municipio)) %>%
    mutate(v.total=str_replace_all(v.total,"[.]","")) %>%
    mutate(v.total=str_replace_all(v.total,"[,]",".")) %>%
    mutate("chave"=str_c(estado," ",municipio))

  x <- c2017 %>% 
    group_by(chave) %>% 
    summarise(total.contratos=sum(c.total,na.rm=T))

  y <- c2017 %>% 
    group_by(chave) %>% 
    summarise(valores.totais=sum(as.numeric(v.total),na.rm=T))

  c2017 <- full_join(x,y) %>% 
    mutate("ano"=2017)

  c2017 <- c2017 %>% 
    mutate("estado"=str_sub(chave,start=1,end=2)) %>% 
    mutate("municipio"=str_sub(chave,start=4)) %>% 
    mutate("chave"=str_c(municipio," (", estado,")"))

  c2017 <- c2017[,c(4,1,5,6,2,3)]

#Exportando tabela

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

  write.table(c2017,file='bacen.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(x,y,ano15,c2017)