#carregando pacotes----

if(!require(dplyr))
  install.packages("dplyr")

if(!require(stringr))
  install.packages("stringr")

#Limpando diretório----

rm(list=ls())

#Diretório local de trabalho----

setwd('C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base')

#Importando tabelas----

ano1 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2003/b2003.csv"
ano2 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2004/b2004.csv"
ano3 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2005/b2005.csv"
ano4 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2006/b2006.csv"
ano5 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2007/b2007.csv"
ano6 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2008/b2008.csv"
ano7 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2009/b2009.csv"
ano8 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2010/b2010.csv"
ano9 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2011/b2011.csv"
ano10 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2012/b2012.csv"
ano11 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2013/b2013.csv"
ano12 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2014/b2014.csv"
ano13 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2015/b2015.csv"
ano14 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2016/b2016.csv"
ano15 <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/anos/2017/b2017.csv"

b2003 <- read.csv(ano1,sep=";",dec=".")

b2004 <- read.csv(ano2,sep=";",dec=".")

b2005 <- read.csv(ano3,sep=";",dec=".")

b2006 <- read.csv(ano4,sep=";",dec=".")

b2007 <- read.csv(ano5,sep=";",dec=".")

b2008 <- read.csv(ano6,sep=";",dec=".")

b2009 <- read.csv(ano7,sep=";",dec=".")

b2010 <- read.csv(ano8,sep=";",dec=".")

b2011 <- read.csv(ano9,sep=";",dec=".")

b2012 <- read.csv(ano10,sep=";",dec=".")

b2013 <- read.csv(ano11,sep=";",dec=".")

b2014 <- read.csv(ano12,sep=";",dec=".")

b2015 <- read.csv(ano13,sep=";",dec=".")

b2016 <- read.csv(ano14,sep=";",dec=".")

b2017 <- read.csv(ano15,sep=";",dec=".")

#Juntando todas as tabelas----

base <- rbind(b2003,b2004,b2005,b2006,b2007,b2008,b2009,b2010,b2011,
              b2012,b2013,b2014,b2015,b2016,b2017)

rm(ano1,ano2,ano3,ano4,ano5,ano6,ano7,ano8,ano9,ano10,ano11,
   ano12,ano13,ano14,ano15,b2003,b2004,b2005,b2006,b2007,b2008,
   b2009,b2010,b2011,b2012,b2013,b2014,b2015,b2016,b2017)

#Criando variáveis dependentes----

base <- base %>% 
  mutate("prod_dende"=(base$q.dende/base$h.dende)) %>% 
  mutate("prod_girassol"=(base$q.girassol/base$h.girassol)) %>% 
  mutate("prod_mamona"=(base$q.mamona/base$h.mamona)) %>% 
  mutate("prod_soja"=(base$q.soja/base$h.soja)) %>% 
  mutate("rm_dende"=(base$v.dende/base$q.dende)) %>% 
  mutate("rm_girassol"=(base$v.girassol/base$h.girassol)) %>% 
  mutate("rm_mamona"=(base$v.mamona/base$h.mamona)) %>% 
  mutate("rm_soja"=(base$v.soja/base$h.soja))

base <- base %>% 
  mutate('prod_dende'=str_replace_all(prod_dende,'NaN','0')) %>%
  mutate('prod_dende'=str_replace_all(prod_dende,'Inf','0')) %>% 
  mutate('prod_girassol'=str_replace_all(prod_girassol,'NaN','0')) %>% 
  mutate('prod_girassol'=str_replace_all(prod_girassol,'Inf','0')) %>%
  mutate('prod_mamona'=str_replace_all(prod_mamona,'NaN','0')) %>% 
  mutate('prod_mamona'=str_replace_all(prod_mamona,'Inf','0')) %>% 
  mutate('prod_soja'=str_replace_all(prod_soja,'NaN','0')) %>% 
  mutate('prod_soja'=str_replace_all(prod_soja,'Inf','0')) %>% 
  mutate('rm_dende'=str_replace_all(rm_dende,'NaN','0')) %>%
  mutate('rm_dende'=str_replace_all(rm_dende,'Inf','0')) %>% 
  mutate('rm_girassol'=str_replace_all(rm_girassol,'NaN','0')) %>% 
  mutate('rm_girassol'=str_replace_all(rm_girassol,'Inf','0')) %>%
  mutate('rm_mamona'=str_replace_all(rm_mamona,'NaN','0')) %>% 
  mutate('rm_mamona'=str_replace_all(rm_mamona,'Inf','0')) %>% 
  mutate('rm_soja'=str_replace_all(rm_soja,'NaN','0')) %>% 
  mutate('rm_soja'=str_replace_all(rm_soja,'Inf','0')) 

base$prod_dende <- as.numeric(base$prod_dende)
base$prod_girassol <- as.numeric(base$prod_girassol)
base$prod_mamona <- as.numeric(base$prod_mamona)
base$prod_soja <- as.numeric(base$prod_soja)
base$rm_dende <- as.numeric(base$rm_dende)
base$rm_girassol <- as.numeric(base$rm_girassol)
base$rm_mamona <- as.numeric(base$rm_mamona)
base$rm_soja <- as.numeric(base$rm_soja)
base$est_pop <- as.numeric(base$est_pop)

base <- base %>% 
  filter(est_pop!="NA")

#Limpando variáveis----

base <- base %>% 
  select(-"cod_uf",-"vabi",-"vabs",-"vabadm",-"vabt")

#Exportando tabela----

write.table(base,file='nbase.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)