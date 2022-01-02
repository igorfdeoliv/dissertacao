#Carregando pacotes----

if(!require(dplyr))
  install.packages("dplyr")

if(!require(stringr))
  install.packages("stringr")

if(!require(readxl))
  install.packages("readxl")

if(!require(tidyverse))
  install.packages("tidyverse")

#Limpando memória----

rm(list=ls())

#Diretório local de trabalho----

setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

#Importando tabelas----

qprod <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/quantidade_produzida.csv"
hprod <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/area_plantada.csv"
vprod <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/valor_producao.csv"
demanda <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/demanda.csv"
contratos <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/bacen.csv"
salarios <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/rais.csv"

#Gerando e juntando tabelas de quantidade e area

qprod <- read.csv(qprod,sep=";",dec=".")
hprod <- read.csv(hprod,sep=";",dec=".")

df1 <- right_join(qprod,hprod,by='Cód.')

names(df1) <- c("ano","cod_mun","Município","q_dende","q_girassol",
                "q_mamona","q_soja","drop1","drop2","h_dende",
                "h_girassol","h_mamona","h_soja")

df1 <- df1 %>% 
  select(-"drop1",-"drop2")

rm(qprod,hprod)

vprod <- read.csv(vprod,sep=";",dec=".")

df2 <- right_join(df1,vprod,by="Município")

names(df2) <- c("ano","cod_mun","chave","q_dende","q_girassol",
                "q_mamonoa","q_soja","h_dende","h_girassol",
                "h_mamona","h_soja","drop1","drop2","v_dende",
                "v_girassol","v_mamona","v_soja")

df2 <- df2 %>% 
  select(-"drop1",-"drop2")

rm(vprod)

#Gerando e juntando a tabela de salarios ao df anterior

salarios <- read.csv(salarios,sep=";",dec=".")

df3 <- right_join(df2,salarios,by="chave")

df3 <- df3 %>% 
  filter(cod_mun!="NA")

names(df3) <- c("ano","cod_mun","chave","q_dende","q_girassol",
                "q_mamona","q_soja","h_dende","h_girassol",
                "h_mamona","h_soja","v_dende","v_girassol",
                "v_mamona","v_soja","drop1","drop2","drop3",
                "drop4","dende1","dende2","girassol1","girassol2",
                "mamona1","mamona2","soja1","soja2","drop5")

df3 <- df3 %>% 
  mutate("s_dende"=(df3$dende1+df3$dende2)) %>% 
  mutate("s_girassol"=(df3$girassol1+df3$girassol2)) %>% 
  mutate("s_mamona"=(df3$mamona1+df3$mamona2)) %>% 
  mutate("s_soja"=(df3$soja1+df3$soja2)) %>%
  select(-"drop1",-"drop2",-"drop3",-"drop4",-"drop5",
         -"dende1",-"dende2",-"girassol1",-"girassol2",
         -"mamona1",-"mamona2",-"soja1",-"soja2")

rm(salarios)

#Gerando e juntando tabela de demanda ao df anterior

demanda <- read.csv(demanda,sep=";",dec=".")

df4 <- right_join(df3,demanda,by="chave")

df4 <- df4 %>% 
  filter(cod_mun!="NA")

names(df4) <- c("ano","cod_mun","chave","q_dende","q_girassol",
                "q_mamona","q_soja","h_dende","h_girassol","h_mamona",
                "h_soja","v_dende","v_girassol","v_mamona","v_soja",
                "s_dende","s_girassol","s_mamona","s_soja","drop1",
                "drop2","drop3","drop4","drop5","drop6","demanda_bio")

df4 <- df4 %>% 
  select(-"drop1",-"drop2",-"drop3",-"drop4",-"drop5",-"drop6")

rm(demanda)

#Gerando e juntando tabela de contratos ao df anterior

pronaf <- read.csv(contratos,sep=";",dec=".")

df <- pronaf %>% 
  filter(estado=="DF")

x <- df %>% 
  group_by(estado) %>% 
  summarise(t.contratos=sum(as.numeric(total.contratos),na.rm=TRUE))

y <- df %>% 
  group_by(estado) %>% 
  summarise(v.totais=sum(as.numeric(valores.totais),na.rm=TRUE))

df <- right_join(x,y,by="estado")

df<- df %>% 
  mutate("ano"=2011) %>% 
  mutate("chave"='BRASILIA (DF)') %>% 
  mutate("municipio"='BRASILIA')

df<- df[,c(4,5,1,6,2,3)]

names(df) <- c("ano","chave","estado","municipio","total.contratos",
               "valores.totais")

rm(x,y)

pronaf <- pronaf %>% 
  filter(estado!="DF")

pronaf <- rbind(pronaf,df)

df5 <- right_join(df4,pronaf,by="chave")

names(df5) <- c("ano","cod_mun","chave","q.dende","q.girassol","q.mamona",
                "q.soja","h.dende","h.girassol","h.mamona","h.soja",
                "v.dende","v.girassol","v.mamona","v.soja","s.dende",
                "s.girassol","s.mamona","s.soja","d.bio","ano.y",
                "estado","municipio","total.contratos","valores.totais")

b2011 <- df5 %>% 
  filter(cod_mun!="NA") %>% 
  select(-"ano.y",-"estado",-"municipio")

rm(df,df1,df2,df3,df4,df5,contratos,pronaf)

#Incluindo informações municipais

setwd ("E:/igorf/Documents/GitHub/dissertacao/dataset/base")

pibmun <- read.csv('pibmun.csv',sep=";",dec=",")

pibmun <- pibmun %>% 
  filter(ano==2011)

b2011 <- right_join(b2011,pibmun,by="cod_mun")

b2011 <- b2011 %>% 
  filter(chave.x!="NA") %>% 
  select(-"ano.y",-"chave.y",-"estado",-"municipio")

names(b2011) <- c("ano","cod_mun","chave","q.dende","q.girassol",
                  "q.mamona","q.soja","h.dende","h.girassol","h.mamona",
                  "h.soja","v.dende","v.girassol","v.mamona","v.soja",
                  "s.dende","s.girassol","s.mamona","s.soja","d.bio",
                  "total.contratos","valores.totais","regiao","cod_uf","uf",
                  "semiarido","vaba","vabi","vabs","vabadm","vabt",
                  "t","pib","pib.per.capta")

b2011 <- b2011[,c(1,2,23,24,25,26,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                  19,20,21,22,27,28,29,30,31,32,33,34)]

rm(pibmun)

#Inlcuindo amc----

amc <- read_excel("E:/igorf/Documents/GitHub/dissertacao/dataset/ibge/amc/AMC_1980_2010.xlsx",
                  col_name =c("municipio","cod_mun","amc"))

b2011 <- b2011 %>% 
  mutate(cod_mun=as.character(cod_mun))

b2011 <- right_join(b2011,amc,by="cod_mun")

b2011 <- b2011 %>% 
  filter(ano!="NA") %>% 
  select(-"municipio")

rm(amc)

#Deflacionando valores----  

deflator <- read_excel("E:/igorf/Documents/GitHub/dissertacao/dataset/ipea/ipeadata[18-01-2021-09-33].xls")

x <- filter(deflator,ano==2011)

b2011 <- b2011 %>% 
  mutate(v.dende=(v.dende*(x$deflator))) %>% 
  mutate(v.girassol=(v.girassol*(x$deflator))) %>% 
  mutate(v.mamona=(v.mamona*(x$deflator))) %>% 
  mutate(v.soja=(v.soja*(x$deflator))) %>% 
  mutate(s.dende=(s.dende*(x$deflator))) %>%
  mutate(s.girassol=(s.girassol*(x$deflator))) %>%
  mutate(s.mamona=(s.mamona*(x$deflator))) %>%
  mutate(s.soja=(s.soja*(x$deflator))) %>%
  mutate(valores.totais=(valores.totais*(x$deflator))) %>% 
  mutate(vaba=(as.numeric(vaba)*(x$deflator))) %>% 
  mutate(vabi=(as.numeric(vabi)*(x$deflator))) %>% 
  mutate(vabs=(as.numeric(vabs)*(x$deflator))) %>% 
  mutate(vabadm=(as.numeric(vabadm)*(x$deflator))) %>% 
  mutate(vabt=(as.numeric(vabt)*(x$deflator))) %>% 
  mutate(t=(as.numeric(t)*(x$deflator))) %>% 
  mutate(pib=(as.numeric(pib)*(x$deflator))) %>% 
  mutate(pib.per.capta=(as.numeric(pib.per.capta)*(x$deflator)))

rm(x,deflator)

#Categorizando a variavel semiarido

b2011 <- b2011 %>% 
  mutate(semiarido=if_else(semiarido=="Sim",1,0))

b2011 <- b2011[,c(1,2,35,4,5,3,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,25,26,27,28,
                  29,30,31,32,33,34)]

#Incluindo estimativa populacional----

est_pop <- read_excel("E:/igorf/Documents/GitHub/dissertacao/dataset/ibge/est_pop/POP2011_TCU.xls", 
                      skip = 3,col_name=c("uf","cod_uf","cod_mun","municipio","est_pop"))


est_pop <- est_pop %>% 
  mutate(municipio=toupper(municipio)) %>% 
  mutate(municipio=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'-", "AEIOUAOAEOC  ",municipio)) %>% 
  mutate("chave"=str_c(municipio," ","(",uf,")") ) %>% 
  select("chave","est_pop")

b2011 <- right_join(b2011,est_pop,by="chave")

b2011 <- b2011 %>% 
  filter(ano!="NA")

rm(est_pop)

#Criando dummy polos----

polos <- read_csv2('E:/igorf/Documents/GitHub/dissertacao/dataset/projeto_polos/polos.csv')

polos$cod_mun <- as.character(polos$cod_mun)

b2011 <- full_join(b2011, polos, by = 'cod_mun')

b2011 <- b2011 %>% 
  mutate(dummy1 = ifelse(is.na(polo), 0, 1)) %>% 
  filter(ano!="NA") %>% 
  select(-"amc.y",-"chave.y",-"estado",-"municipio",-"polo")

names(b2011) <- c("ano","cod_mun","amc","cod_uf","uf","regiao","semiarido","chave",
                  "q.dende","q.girassol","q.mamona","q.soja","h.dende",
                  "h.girassol","h.mamona","h.soja","v.dende","v.girassol",
                  "v.mamona","v.soja","s.dende","s.girassol","s.mamona",
                  "s.soja","d.bio","total.contratos","valores.totais",
                  "vaba","vabi","vabs","vabadm","vabt","t","pib",
                  "pib.per.capta","est_pop","polos")

b2011 <- b2011[,c(1,2,3,6,37,4,5,7,8,9,10,11,12,13,14,15,16,
                  17,18,19,20,21,22,23,24,25,26,27,28,29,30,
                  31,32,33,34,35,36)]

rm(polos)

#Exportando base----

setwd("E:/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2011")

write.table(b2011,file='b2011.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

rm(b2011)