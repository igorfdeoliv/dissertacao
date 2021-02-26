#Qual o efeito de estar alocado no projeto polos de biodiesel e produzir
#oleaginosas sobre a produtividade, rendimento m�dio e sal�rios m�dios pagos nas
#culturas relacionadas?

#Podemos considerar estes munic�pios, na m�dia, iguais?

#N�o. Ent�o temos de usar propensity score matching para obter estimativas
#causais mais criveis do efeito observado do projeto polos de biodiesel.

#Passo a passo

#1. Estimar os propensity scores (a probabilidade de ser tratado dado uma s�rie
#de covari�veis do efeito observado do projeto polos de biodiesel)

#2. Examinar qual a regi�o de suporte comum.

#3. Escolher e executar o algoritmo de matching. Neste caso, vamos usar um tipo
#espec�fico de algor�tmo chamado: nearest neighbor propensity score matching

#4. Examinar o balanceamento das covariaveis depois do matching

#Qual o efeito de estar alocado no projeto polos de biodiesel e produzir
#oleaginosas sobre a produtividade, rendimento m�dio e sal�rios m�dios pagos nas
#culturas relacionadas?

#Podemos considerar estes munic�pios, na m�dia, iguais?

#N�o. Ent�o temos de usar propensity score matching para obter estimativas
#causais mais criveis do efeito observado do projeto polos de biodiesel.

#Passo a passo

#1. Estimar os propensity scores (a probabilidade de ser tratado dado uma s�rie
#de covari�veis do efeito observado do projeto polos de biodiesel)

#2. Examinar qual a regi�o de suporte comum.

#3. Escolher e executar o algoritmo de matching. Neste caso, vamos usar um tipo
#espec�fico de algor�tmo chamado: nearest neighbor propensity score matching

#4. Examinar o balanceamento das covariaveis depois do matching

#Importando pacotes para trabalho----

  if(!require(cobalt))
    install.packages('cobalt')

  if(!require(dplyr))
    install.packages("dplyr")

  if(!require(ggplot2))
    install.packages("ggplot2")

  if(!require(lmtest))
    install.packages('lmtest') 

  if(!require(MatchIt))
    install.packages("MatchIt")

  if(!require(plm))
    install.packages("plm")

  if(!require(sandwich))
    install.packages('sandwich')

  if(!require(stargazer))
    install.packages("stargazer")

#Limpando mem�ria----

  rm(list=ls())

#Importando base de dados----
  
  base <- "https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/base.csv"

  pnpb <- read.csv(base,sep=";",dec=".")

  rm(base)

#An�lise descritiva das vari�veis de interesse antes do pareamento----

#Produtividade da soja (quantidade(t)/�rea plantada(hec))

  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios = n(),
            mean_match = mean(prod_soja),
            std_error = sd(prod_soja) / sqrt(n_municipios))

#Rendimento m�dio da soja (R$/quantidade(t))

  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios = n(),
            mean_match = mean(rm_soja),
            std_error = sd(rm_soja) / sqrt(n_municipios))

#Sal�rios m�dios pagos a produtores e trabalhadores da cultura de soja

  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios = n(),
            mean_match = mean(s.soja),
            std_error = sd(s.soja) / sqrt(n_municipios))

################################################################################  

#A diferen�a de m�dia � estatisticamente significativa a 95% de 
#confidencialidade:

#Produtividade da soja (quantidade(t)/�rea plantada(hec))

  with(pnpb, t.test(prod_soja ~ polos))

#Rendimento m�dio da soja (R$/quantidade(t))

  with(pnpb, t.test(rm_soja ~ polos))

#Sal�rios m�dios pagos a produtores e trabalhadores da cultura de soja  

  with(pnpb, t.test(s.soja ~ polos))

#p-valor < 0.05, rejeita H0, existem evid�ncias de que as m�dias s�o diferentes.

################################################################################

#Diferen�a de m�dias: vari�veis de pr�-tratamento----

#total.contratos: n�mero de contratos via PRONAF
#valores.totais: valores totais recebidos por munic�pio via PRONAF, deflacionado pelo IPCA (base=2017)
#d.bio: proxy de demanda de biodiesel, n�mero de �nibus e caminh�es por munic�pio
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
              vaba + t,family=binomial(),data=pnpb)
  summary(m_ps)

  stargazer(m_ps,type='text',dep.var.labels=c('Tratamento'), out='Modelo0.html')

#Calculando as probabilidades de participar do programa:

  prs_df <- data.frame(pr_score=predict(m_ps,type="response"),
                     polos=m_ps$model$polos)
  head(prs_df)

################################################################################

  labs <- paste('Participa do Projeto Polos de Biodiesel:',c("Sim","N�o"))

  prs_df %>% 
    mutate(polos=ifelse(polos==1, labs[1],labs[2]
  )) %>% 
    ggplot(aes(x=pr_score)) + geom_histogram(color="white") + 
    facet_wrap(~polos) + xlab('Probabilidade de participar do projeto polos') +
    ylab('N� Munic�pios') +
    theme_bw()

################################################################################

#Esse comando � necess�rio quando existem vari�veis missing na amostra.

#  pnpb_nomiss <- pnpb %>% 
#    select(prod_soja,polos,one_of(pnpb_cov)) %>% 
#    na.omit()

#Verifica a dimens�o do data frame ap�s a exclus�o das missings

#  dim(pnpb)
#  dim(pnpb_nomiss)

#Iniciando matching----

  matching <- matchit(polos ~ total.contratos + valores.totais + d.bio +
                      vaba + t,data=pnpb,link="probit",method="nearest",
                    ratio=1)

#Resumo estat�stico ap�s o pareamento----

  summary(matching)

#Gr�fico de ajustamento das covariadas antes e depois do pareamento----

  plot(matching)

#Sobreposi��o dos grupos tratamento e controle ap�s o pareamento----  

  bal.plot(matching,var.name='distance')

#Declarando base ap�s matching----

  psm <- match.data(matching)
  head(psm)
  dim(psm)

#Verificando as m�dias das covariadas ap�s o pareamento----

  psm %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean))

  lapply(pnpb_cov,function(v) {
    t.test(psm[,v] ~ psm$polos)
})

  rm(pnpb_nomiss,prs_df,m_ps,matching,labs,pnpb_cov)

#An�lise descritiva vari�veis dependentes----

  adprod <- psm %>% group_by(ano,polos) %>% 
    summarise(m�dia=mean(prod_soja),
            DP=sd(prod_soja),
            mediana=median(prod_soja))

  adrm <- psm %>% group_by(ano,polos) %>% 
    summarise(m�dia=mean(rm_soja),
            DP=sd(rm_soja),
            mediana=median(rm_soja))

  adw <- psm %>% group_by(ano,polos) %>% 
    summarise(m�dia=mean(s.soja),
            DP=sd(s.soja),
            mediana=median(s.soja))

  rm(adprod,adrm,adw)

#Dummy para identifica��o do per�odo de tratamento----

  psm <- psm %>%
    mutate(data_treat=if_else(ano>2006,1,0))

#Criando a vari�vel de diff in diff, interagindo polos e data_treat----

  psm <- psm %>% 
    mutate(estimador_dd=polos*data_treat)

  table(psm$estimador_dd)

#Declarando que a base de dados � um painel----

  painel <- pdata.frame(psm,index=c("chave","ano"))

#Estimado os modelo e analisando os resultados----

#Produtividade ap�s o pareamento

  reg1 <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos +
              h.soja + d.bio + vaba + est_pop,data=painel,model="pooling")

  reg2 <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos +
              h.soja + d.bio + vaba + est_pop,data=painel,model="within")

#Rendimento m�dio ap�s o pareamento

  reg3 <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
              h.soja + d.bio + vaba + est_pop,data=painel,model="pooling")

  reg4 <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
              h.soja + d.bio + vaba + est_pop,data=painel,model="within")

#Sal�rios relacionados �s atividades ap�s o pareamento

  reg5 <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
              h.soja + d.bio + vaba + est_pop,data=painel,model="pooling")

  reg6 <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
              h.soja + d.bio + vaba + est_pop,data=painel,model="within")

#Tabela de resultados

  stargazer(reg1,reg2,reg3,reg4,reg5,reg6,type="text",omit.stat=c("LL","ser","f"),
          dep.var.labels=c("Produtividade","Renda M�dia","Sal�rios"),
          out="Modelo1.txt")

  rm(reg1,reg2,reg3,reg4,reg5,reg6)

#Estimado os modelo e analisando os resultados + semiarido----

#Produtividade ap�s o pareamento

  reg1 <- plm(prod_soja ~ polos + semiarido + data_treat + estimador_dd +
              total.contratos + h.soja + d.bio + vaba + est_pop,data=painel,
            model="pooling")

  reg2 <- plm(prod_soja ~ polos + semiarido + data_treat + estimador_dd + 
              total.contratos + h.soja + d.bio + vaba + est_pop,data=painel,
            model="within")

#Rendimento m�dio ap�s o pareamento

  reg3 <- plm(rm_soja ~ polos + semiarido + data_treat + estimador_dd + 
              total.contratos + h.soja + d.bio + vaba + est_pop,data=painel,
            model="pooling")

  reg4 <- plm(rm_soja ~ polos + semiarido + data_treat + estimador_dd +
              total.contratos + h.soja + d.bio + vaba + est_pop,data=painel,
            model="within")

#Sal�rios relacionados �s atividades ap�s o pareamento

  reg5 <- plm(s.soja ~ polos + semiarido + data_treat + estimador_dd + 
              total.contratos + h.soja + d.bio + vaba + est_pop,data=painel,
            model="pooling")

  reg6 <- plm(s.soja ~ polos + semiarido + data_treat + estimador_dd + 
              total.contratos + h.soja + d.bio + vaba + est_pop,data=painel,
            model="within")

#Tabela de resultados

  stargazer(reg1,reg2,reg3,reg4,reg5,reg6,type="text",omit.stat=c("LL","ser","f"),
          dep.var.labels=c("Produtividade","Renda M�dia","Sal�rios"),
          out="Modelo2.txt")

  rm(reg1,reg2,reg3,reg4,reg5,reg6)