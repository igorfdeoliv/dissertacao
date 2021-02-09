#Carregando pacotes----

  if(!require(dplyr))
    install.packages('dplyr')

  if(!require(readxl))
    install.packages('readxl')

#Limpando memória----

  rm(list=ls())

#Diretório local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/projeto_polos")

#Gerando tabela de atributos Projeto Polos de Biodiesel----

  polos <- read_excel("~/GitHub/dissertacao/dataset/projeto_polos/tabela_de_atributos_projeto_polos.xlsx")

  polos <- polos %>% 
    mutate(NomMunic=toupper(NomMunic)) %>% 
    mutate(NomMunic=chartr("ÁÉÍÓÚÃÕÂÊÔÇ'´-", "AEIOUAOAEOC    ",NomMunic)) %>% 
    mutate(NomUF=toupper(NomUF))

  polos[polos == "ACRE"] <- "AC"
  polos[polos == "ALAGOAS"] <- "AL"
  polos[polos == "AMAPÁ"] <- "AP"
  polos[polos == "AMAZONAS"] <- "AM"
  polos[polos == "BAHIA"] <- "BA"
  polos[polos == "CEARÁ"] <- "CE"
  polos[polos == "DISTRITO FEDERAL"] <- "DF"
  polos[polos == "ESPÍRITO SANTO"] <- "ES"
  polos[polos == "GOIÁS"] <- "GO"
  polos[polos == "MARANHÃO"] <- "MA"
  polos[polos == "MATO GROSSO"] <- "MT"
  polos[polos == "MATO GROSSO DO SUL"] <- "MS"
  polos[polos == "MINAS GERAIS"] <- "MG"
  polos[polos == "PARÁ"] <- "PA"
  polos[polos == "PARAÍBA"] <- "PB"
  polos[polos == "PARANÁ"] <- "PR"
  polos[polos == "PERNAMBUCO"] <- "PE"
  polos[polos == "PIAUÍ"] <- "PI"
  polos[polos == "RIO DE JANEIRO"] <- "RJ"
  polos[polos == "RIO GRANDE DO NORTE"] <- "RN"
  polos[polos == "RIO GRANDE DO SUL"] <- "RS"
  polos[polos == "RONDÔNIA"] <- "RO"
  polos[polos == "RORAIMA"] <- "RR"
  polos[polos == "SANTA CATARINA"] <- "SC"
  polos[polos == "SÃO PAULO"] <- "SP"
  polos[polos == "SERGIPE"] <- "SE"
  polos[polos == "TOCANTINS"] <- "TO"

  polos <- polos %>% 
    mutate("chave"=str_c(NomMunic," (", NomUF,")"))

  polos <- polos[,c(3,8,2,4,6,1,5,7)]

#Importando tabela de areas minimas comparaveis para criar id----

  amc <- read_excel("~/GitHub/dissertacao/dataset/ibge/amc/AMC_1980_2010.xlsx")

  names(amc) <- c("municpio","CodMunic","amc")

  df <- right_join(polos,amc,by="CodMunic")

  df <- df %>% 
    filter(NomUF!="NA") %>% 
    select(-"NomMunic",-"gid",-"ufmunic",-"Região")

  polos <- df[,c(1,6,2,3,5,4)]

  names(polos) <- c("cod_mun","amc","chave","estado","municipio","polo")

#Exportando tabela----  

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/projeto_polos")

  write.table(polos,file='polos.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(amc,df)