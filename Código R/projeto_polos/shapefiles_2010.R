#Carregando pacotes----

  if(!require(dplyr))
    install.packages('dplyr')

  if(!require(readxl))
    install.packages('readxl')

#Limpando mem�ria----

  rm(list=ls())

#Diret�rio local de trabalho----

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/projeto_polos")

#Gerando tabela de atributos Projeto Polos de Biodiesel----

  polos <- read_excel("~/GitHub/dissertacao/dataset/projeto_polos/tabela_de_atributos_projeto_polos.xlsx")

  polos <- polos %>% 
    mutate(NomMunic=toupper(NomMunic)) %>% 
    mutate(NomMunic=chartr("�����������'�-", "AEIOUAOAEOC    ",NomMunic)) %>% 
    mutate(NomUF=toupper(NomUF))

  polos[polos == "ACRE"] <- "AC"
  polos[polos == "ALAGOAS"] <- "AL"
  polos[polos == "AMAP�"] <- "AP"
  polos[polos == "AMAZONAS"] <- "AM"
  polos[polos == "BAHIA"] <- "BA"
  polos[polos == "CEAR�"] <- "CE"
  polos[polos == "DISTRITO FEDERAL"] <- "DF"
  polos[polos == "ESP�RITO SANTO"] <- "ES"
  polos[polos == "GOI�S"] <- "GO"
  polos[polos == "MARANH�O"] <- "MA"
  polos[polos == "MATO GROSSO"] <- "MT"
  polos[polos == "MATO GROSSO DO SUL"] <- "MS"
  polos[polos == "MINAS GERAIS"] <- "MG"
  polos[polos == "PAR�"] <- "PA"
  polos[polos == "PARA�BA"] <- "PB"
  polos[polos == "PARAN�"] <- "PR"
  polos[polos == "PERNAMBUCO"] <- "PE"
  polos[polos == "PIAU�"] <- "PI"
  polos[polos == "RIO DE JANEIRO"] <- "RJ"
  polos[polos == "RIO GRANDE DO NORTE"] <- "RN"
  polos[polos == "RIO GRANDE DO SUL"] <- "RS"
  polos[polos == "ROND�NIA"] <- "RO"
  polos[polos == "RORAIMA"] <- "RR"
  polos[polos == "SANTA CATARINA"] <- "SC"
  polos[polos == "S�O PAULO"] <- "SP"
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
    select(-"NomMunic",-"gid",-"ufmunic",-"Regi�o")

  polos <- df[,c(1,6,2,3,5,4)]

  names(polos) <- c("cod_mun","amc","chave","estado","municipio","polo")

#Exportando tabela----  

  setwd("C:/Users/igorf/Documents/GitHub/dissertacao/dataset/projeto_polos")

  write.table(polos,file='polos.csv',sep=';',dec=".",na="0",quote=TRUE, row.names=FALSE)

  rm(amc,df)