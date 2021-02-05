#Qual o efeito de estar alocado no projeto polos de biodiesel e produzir
#oleaginosas sobre a produtividade, rendimento médio e salários médios pagos nas
#culturas relacionadas?

#Podemos considerar estes municípios, na média, iguais?

#Não. Então temos de usar propensity score matching para obter estimativas
#causais mais criveis do efeito observado do projeto polos de biodiesel.

#Passo a passo

#1. Estimar os propensity scores (a probabilidade de ser tratado dado uma série
#de covariáveis do efeito observado do projeto polos de biodiesel)

#2. Examinar qual a região de suporte comum.

#3. Escolher e executar o algoritmo de matching. Neste caso, vamos usar um tipo
#específico de algorítmo chamado: nearest neighbor propensity score matching

#4. Examinar o balanceamento das covariaveis depois do matching

#Qual o efeito de estar alocado no projeto polos de biodiesel e produzir
#oleaginosas sobre a produtividade, rendimento médio e salários médios pagos nas
#culturas relacionadas?

#Podemos considerar estes municípios, na média, iguais?

#Não. Então temos de usar propensity score matching para obter estimativas
#causais mais criveis do efeito observado do projeto polos de biodiesel.

#Passo a passo

#1. Estimar os propensity scores (a probabilidade de ser tratado dado uma série
#de covariáveis do efeito observado do projeto polos de biodiesel)

#2. Examinar qual a região de suporte comum.

#3. Escolher e executar o algoritmo de matching. Neste caso, vamos usar um tipo
#específico de algorítmo chamado: nearest neighbor propensity score matching

#4. Examinar o balanceamento das covariaveis depois do matching

#Importando pacotes para trabalho----

  if(!require(dplyr))
    install.packages("dplyr")

  if(!require(MatchIt))
    install.packages("MatchIt")

  if(!require(plm))
    install.packages("plm")

  if(!require(psych))
    install.packages("psych")

  if(!require(stargazer))
    install.packages("stargazer")

  if(!require(ggplot2))
    install.packages("ggplot2")

  if(!require(cowplot))
    install.packages("cowplot")

  if(!require(tidyverse))
    install.packages("tidyverse")

  if(!require(broom))
    install.packages("broom")

  if(!require(modelsummary))
    install.packages("modelsummary")

  if(!require(scales))
    install.packages("scales")

#Limpando memória----

  rm(list=ls())

#Importando base de dados----

  base <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/base.csv"
  
  pnpb <- read.csv(base,sep=";",dec=".")

  rm(base)

#Análise descritiva das variáveis de interesse antes do pareamento----

#Produtividade da soja (quantidade(t)/área plantada(hec))

  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios = n(),
            mean_match = mean(prod_soja),
            std_error = sd(prod_soja) / sqrt(n_municipios))

#Rendimento médio da soja (R$/quantidade(t))

  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios = n(),
            mean_match = mean(rm_soja),
            std_error = sd(rm_soja) / sqrt(n_municipios))

#Salários médios pagos a produtores e trabalhadores da cultura de soja

  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios = n(),
            mean_match = mean(s.soja),
            std_error = sd(s.soja) / sqrt(n_municipios))

################################################################################  

#A diferença de média é estatisticamente significativa a 95% de 
#confidencialidade:

#Produtividade da soja (quantidade(t)/área plantada(hec))

  with(pnpb, t.test(prod_soja ~ polos))

#Rendimento médio da soja (R$/quantidade(t))

  with(pnpb, t.test(rm_soja ~ polos))

#Salários médios pagos a produtores e trabalhadores da cultura de soja  

  with(pnpb, t.test(s.soja ~ polos))

#p-valor < 0.05, rejeita H0, existem evidências de que as médias são diferentes.

################################################################################

#Diferença de médias: variáveis de pré-tratamento----

#total.contratos: número de contratos via PRONAF
#valores.totais: valores totais recebidos por município via PRONAF, deflacionado pelo IPCA (base=2017)
#d.bio: proxy de demanda de biodiesel, número de ônibus e caminhões por município
#pib.per.capta: valor do pib per capta municipal, deflacionado pelo IPCA (base=2017)

  pnpb_cov <- c('total.contratos','valores.totais','d.bio','vaba','t')

  pnpb %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean(.,na.rm=TRUE)))

  lapply(pnpb_cov,function(v) {
    t.test(pnpb[,v] ~ pnpb[,'polos'])
})

################################################################################

  m_ps <- glm(polos ~ total.contratos + valores.totais + d.bio +
              pib.per.capta,family=binomial(),data=pnpb)
  summary(m_ps)

#Calculando as probabilidades de participar do programa:

  prs_df <- data.frame(pr_score=predict(m_ps,type="response"),
                     polos=m_ps$model$polos)
  head(prs_df)

################################################################################
  
  labs <- paste("Município participa ou não do projeto:",c("Sim","Não"))

  prs_df %>% 
    mutate(polos=ifelse(polos==1, labs[1],labs[2]
  )) %>% 
    ggplot(aes(x=pr_score)) + geom_histogram(color="white") + 
    facet_wrap(~polos) + xlab("Probabilidade de participar do projeto polos") +
    theme_bw()

################################################################################

#Esse comando é necessário quando existem variáveis missing na amostra.

#  pnpb_nomiss <- pnpb %>% 
#    select(prod_soja,polos,one_of(pnpb_cov)) %>% 
#    na.omit()

#Verifica a dimensão do data frame após a exclusão das missings

#  dim(pnpb)
#  dim(pnpb_nomiss)

#Iniciando matching----

  matching <- matchit(polos ~ total.contratos + valores.totais + d.bio +
                      vaba + t,data=pnpb,link="probit",method="nearest",
                    ratio=1)

  summary(matching)

  plot(matching)

#Declarando base após matching----

  psm <- match.data(matching)
  head(psm)
  dim(psm)

#Verificando as médias das covariadas após o pareamento----

  psm %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean))

  lapply(pnpb_cov,function(v) {
    t.test(psm[,v] ~ psm$polos)
})

  rm(pnpb_nomiss,prs_df,m_ps,matching,labs,pnpb_cov)

#Análise descritiva variáveis dependentes----

  adprod <- psm %>% group_by(ano,polos) %>% 
    summarise(média=mean(prod_soja),
            DP=sd(prod_soja),
            mediana=median(prod_soja))

  adrm <- psm %>% group_by(ano,polos) %>% 
    summarise(média=mean(rm_soja),
            DP=sd(rm_soja),
            mediana=median(rm_soja))

  adw <- psm %>% group_by(ano,polos) %>% 
    summarise(média=mean(s.soja),
            DP=sd(s.soja),
            mediana=median(s.soja))

  rm(adprod,adrm,adw)

#Dummy para identificação do período de tratamento----

  psm <- psm %>%
    mutate(data_treat=if_else(ano>2006,1,0))

#Criando a variável de diff in diff, interagindo polos e data_treat----

  psm <- psm %>% 
    mutate(estimador_dd=polos*data_treat)

  table(psm$estimador_dd)

#Declarando que a base de dados é um painel----

  painel <- pdata.frame(psm,index=c("chave","ano"))

#Estimado os modelo e analisando os resultados----

#Produtividade após o pareamento

  reg1 <- plm(prod_soja ~ total.contratos + valores.totais + 
              d.bio + vaba + t + estimador_dd,data=painel,model="pooling")

  reg2 <- plm(prod_soja ~ total.contratos + valores.totais + 
              d.bio + vaba + t + estimador_dd,data=painel,model="within")

#Rendimento médio após o pareamento

  reg3 <- plm(rm_soja ~ total.contratos + valores.totais +
              d.bio + vaba + t + estimador_dd,data=painel,model="pooling")

  reg4 <- plm(rm_soja ~ total.contratos + valores.totais + 
              d.bio + vaba + t + estimador_dd,data=painel,model="within")

#Salários relacionados às atividades após o pareamento

  reg5 <- plm(s.soja ~ total.contratos + valores.totais +
              d.bio + vaba + t + estimador_dd,data=painel,model="pooling")

  reg6 <- plm(s.soja ~ total.contratos + valores.totais + 
              d.bio + vaba + t + estimador_dd,data=painel,model="within")

#Tabela de resultados

  stargazer(reg1,reg2,reg3,reg4,reg5,reg6,type="text",omit.stat=c("LL","ser","f"),
            dep.var.labels=c("Produtividade","Rendimento Médio","Salários"),
            out="Modelo1.html")

  rm(reg1,reg2,reg3,reg4,reg5,reg6)
  
#Estimado os modelo e analisando os resultados + semiarido----
  
#Produtividade após o pareamento
  
  reg1 <- plm(prod_soja ~ semiarido + total.contratos + valores.totais + 
                d.bio + vaba + t + estimador_dd,data=painel,model="pooling")
  
  reg2 <- plm(prod_soja ~ semiarido + total.contratos + valores.totais + 
                d.bio + vaba + t + estimador_dd,data=painel,model="within")
  
#Rendimento médio após o pareamento
  
  reg3 <- plm(rm_soja ~ semiarido + total.contratos + valores.totais +
                d.bio + vaba + t + estimador_dd,data=painel,model="pooling")
  
  reg4 <- plm(rm_soja ~ semiarido + total.contratos + valores.totais + 
                d.bio + vaba + t + estimador_dd,data=painel,model="within")
  
#Salários relacionados às atividades após o pareamento
  
  reg5 <- plm(s.soja ~ semiarido + total.contratos + valores.totais +
                d.bio + vaba + t + estimador_dd,data=painel,model="pooling")
  
  reg6 <- plm(s.soja ~ semiarido + total.contratos + valores.totais + 
                d.bio + vaba + t + estimador_dd,data=painel,model="within")
  
#Tabela de resultados
  
  stargazer(reg1,reg2,reg3,reg4,reg5,reg6,type="text",omit.stat=c("LL","ser","f"),
            dep.var.labels=c("Produtividade","Rendimento Médio","Salários"),
            out="Modelo2.html")
  
  rm(reg1,reg2,reg3,reg4,reg5,reg6)