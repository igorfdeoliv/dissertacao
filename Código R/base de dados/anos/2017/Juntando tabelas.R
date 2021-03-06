#Carregando pacotes----

if(!require(dplyr))
  install.packages("dplyr")

if(!require(stringr))
  install.packages("stringr")

if(!require(readxl))
  install.packages("readxl")

if(!require(tidyverse))
  install.packages("tidyverse")

#Limpando mem�ria----

rm(list=ls())

#Diret�rio local de trabalho----

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

#Importando tabelas----

qprod <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/quantidade_produzida.csv"
hprod <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/area_plantada.csv"
vprod <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/valor_producao.csv"
demanda <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/demanda.csv"
contratos <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/bacen.csv"
salarios <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/rais.csv"

#Gerando e juntando tabelas de quantidade e area

qprod <- read.csv(qprod,sep=";",dec=".")
hprod <- read.csv(hprod,sep=";",dec=".")

df1 <- right_join(qprod,hprod,by='C�d.')

names(df1) <- c("ano","cod_mun","Munic�pio","q_dende","q_girassol",
                "q_mamona","q_soja","drop1","drop2","h_dende",
                "h_girassol","h_mamona","h_soja")

df1 <- df1 %>% 
  select(-"drop1",-"drop2")

rm(qprod,hprod)

vprod <- read.csv(vprod,sep=";",dec=".")

df2 <- right_join(df1,vprod,by="Munic�pio")

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

df5 <- right_join(df4,pronaf,by="chave")

names(df5) <- c("ano","cod_mun","chave","q.dende","q.girassol","q.mamona",
                "q.soja","h.dende","h.girassol","h.mamona","h.soja",
                "v.dende","v.girassol","v.mamona","v.soja","s.dende",
                "s.girassol","s.mamona","s.soja","d.bio","ano.y",
                "estado","municipio","total.contratos","valores.totais")

b2017 <- df5 %>% 
  filter(cod_mun!="NA") %>% 
  select(-"ano.y",-"estado",-"municipio")

rm(df1,df2,df3,df4,df5,contratos,pronaf)

#Incluindo informa��es municipais----

setwd ("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base")

pibmun <- read.csv('pibmun.csv',sep=";",dec=",")

pibmun <- pibmun %>% 
  filter(ano==2017)

b2017 <- right_join(b2017,pibmun,by="cod_mun")

b2017 <- b2017 %>% 
  filter(chave.x!="NA") %>% 
  select(-"ano.y",-"chave.y",-"estado",-"municipio")

names(b2017) <- c("ano","cod_mun","chave","q.dende","q.girassol",
                  "q.mamona","q.soja","h.dende","h.girassol","h.mamona",
                  "h.soja","v.dende","v.girassol","v.mamona","v.soja",
                  "s.dende","s.girassol","s.mamona","s.soja","d.bio",
                  "total.contratos","valores.totais","cod_uf","uf",
                  "semiarido","vaba","vabi","vabs","vabadm","vabt",
                  "t","pib","pib.per.capta")

b2017 <- b2017[,c(1,2,23,24,25,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                  19,20,21,22,26,27,28,29,30,31,32,33)]

rm(pibmun)

#Inlcuindo amc----

amc <- read_excel("~/GitHub/dissertacao/dataset/ibge/amc/AMC_1980_2010.xlsx",
                  col_name =c("municipio","cod_mun","amc"))

b2017 <- b2017 %>% 
  mutate(cod_mun=as.character(cod_mun))

b2017 <- right_join(b2017,amc,by="cod_mun")

b2017 <- b2017 %>% 
  filter(ano!="NA") %>% 
  select(-"municipio")

rm(amc)

#Categorizando a variavel semiarido

b2017 <- b2017 %>% 
  mutate(semiarido=if_else(semiarido=="Sim",1,0))

b2017 <- b2017[,c(1,2,34,3,4,5,6,7,8,9,10,11,12,13,14,15,
                  16,17,18,19,20,21,22,23,24,25,26,27,28,
                  29,30,31,32,33)]

#Incluindo estimativa populacional----

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/ibge/est_pop")

est_pop <- read_excel('POP2017_TCU.xls',sheet="Munic�pios", 
                      skip = 2,col_name=c("uf","cod_uf","cod_mun","municipio","est_pop"))

est_pop <- est_pop %>% 
  mutate(municipio=toupper(municipio)) %>% 
  mutate(municipio=chartr("�����������'-", "AEIOUAOAEOC  ",municipio)) %>% 
  mutate("chave"=str_c(municipio," ","(",uf,")") ) %>% 
  select("chave","est_pop")

b2017 <- right_join(b2017,est_pop,by="chave")

b2017 <- b2017 %>% 
  filter(ano!="NA")

rm(est_pop)

#Criando dummy polos----

polos <- read_csv2('C:/Users/igorf/Documents/GitHub/dissertacao/dataset/projeto_polos/polos.csv')

polos$cod_mun <- as.character(polos$cod_mun)

b2017 <- full_join(b2017, polos, by = 'cod_mun')

b2017 <- b2017 %>% 
  mutate(polos=if_else(is.na(polo),0,1)) %>% 
  filter(ano!="NA") %>% 
  select(-"amc.y",-"chave.y",-"estado",-"municipio",-"polo")

names(b2017) <- c("ano","cod_mun","amc","cod_uf","uf","semiarido","chave",
                  "q.dende","q.girassol","q.mamona","q.soja","h.dende",
                  "h.girassol","h.mamona","h.soja","v.dende","v.girassol",
                  "v.mamona","v.soja","s.dende","s.girassol","s.mamona",
                  "s.soja","d.bio","total.contratos","valores.totais",
                  "vaba","vabi","vabs","vabadm","vabt","t","pib",
                  "pib.per.capta","est_pop","polos")

b2017 <- b2017[,c(1,2,3,6,36,4,5,7,8,9,10,11,12,13,14,15,16,
                  17,18,19,20,21,22,23,24,25,26,27,28,29,30,
                  31,32,33,34,35)]

rm(polos)

#Exportando base----

setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base/anos/2017")

write.table(b2017,file='b2017.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)
