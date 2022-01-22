# Importando pacotes para trabalho----

  if(!require(cowplot))
    install.packages('cowplot')

  if(!require(dplyr))
    install.packages('dplyr')

  if(!require(ggplot2))
    install.packages('ggplot2')

  if(!require(lmtest))
    install.packages('lmtest')

  if(!require(plm))
    install.packages('plm')

  if(!require(stargazer))
    install.packages('stargazer')

  if(!require(tidyverse))
    install.packages('tidyverse')

# Limpando memória----

  rm(list=ls())

# Importando base de dados----

 base <- 'https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/psm.csv'
 
 psm <- read.csv(base,sep=';',dec='.')
 
 rm(base)
 
# Crição de dummy para identificação do período de tratamento----
 
 psm <- psm %>% 
   mutate(data_treat=if_else(ano>2005,1,0))

# Criando a variável de diff in diff, interagindo polos e data do tratamento----
 
 psm <- psm %>% 
   mutate(estimador_dd=polos*data_treat)
 
# Verificando as tendências dos grupos de controle e tratamento das variáveis
# dependentes:
 
 df <- psm %>% 
   mutate(data_treat = ano >= 2006,
          tratamento = polos >= 1) %>% 
   mutate('tratamento'=str_replace_all(tratamento,'FALSE','Não Polos')) %>% 
   mutate('tratamento'=str_replace_all(tratamento,'TRUE','Polos'))
 
# Produtividade----

 # Dendê
 
 p1 <- ggplot(df,aes(ano,prod_dende,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Produtividade \n Dendê',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 # Girassol
 
 p2 <- ggplot(df,aes(ano,prod_girassol,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Produtividade \n Girassol',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 # Mamona
 
 p3 <-  ggplot(df,aes(ano,prod_mamona,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Produtividade \n Mamona',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 # Soja
 
 p4 <-  ggplot(df,aes(ano,prod_soja,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Produtividade \n Soja',
        linetype=NULL) +
   theme(legend.position = 'bottom')

 plot_grid(p1,p2,p3,p4)
 rm(p1,p2,p3,p4)

# Renda média das oleaginosas----
 
 # Renda média do dendê
 
 r1 <- ggplot(df,aes(ano,rm_dende,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Renda Média \n Dendê',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 # Renda média do girassol
 
 r2 <- ggplot(df,aes(ano,rm_girassol,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Renda Média \n Girassol',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 # Renda média mamona
 
 r3 <- ggplot(df,aes(ano,rm_mamona,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Renda Média \n Mamona',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 # Renda média Soja
 
 r4 <- ggplot(df,aes(ano,rm_soja,linetype=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   theme_classic() +
   labs(x='Período',
        y='Renda Média \n Soja',
        linetype=NULL) +
   theme(legend.position = 'bottom')
 
 plot_grid(r1,r2,r3,r4)
 rm(r1,r2,r3,r4)
 
# Declarando que a base de dados é um painel----
 
 painel <- pdata.frame(psm,index=c('chave','ano'))
 
 setwd('E:/igorf/Documents/GitHub/dissertacao/dataset/base')
 
 write.table(painel,file='painel.csv',sep=';',dec='.',na='',quote=TRUE, row.names=FALSE)
 
 rm(psm,df)
 
# Estimação dos modelos----
 
 setwd('E:/igorf/Documents/GitHub/dissertacao/Equações')
 
# Produtividade Dendê
 
  dp_p <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')

  def_p <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  dea_p <- plm(prod_dende ~ estimador_dd + data_treat + polos + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')
  
# Teste LM para EA x MQO

  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(dp_p)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(def_p,dp_p)

# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(dea_p,def_p)
  
# Renda média Dendê
  
  dp_r <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')
  
  def_r <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + sudeste + coeste + 
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  dea_r <- plm(rm_dende ~ estimador_dd + data_treat + polos + sudeste + coeste + 
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(dp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(def_r,dp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(dea_r,def_r)
  
# Produtividade Girassol
  
  gp_p <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')
  
  gef_p <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')

  gea_p <- plm(prod_girassol ~ estimador_dd + data_treat + polos + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')
  
# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(gp_p)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(gef_p,gp_p)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(gea_p,gef_p)
  
# Renda média Girassol
  
  gp_r <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')

  gef_r <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  gea_r <- plm(rm_girassol ~ estimador_dd + data_treat + polos + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(gp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(gef_r,gp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(gea_r,gef_r)
  
# Produtividade Mamona
  
  mp_p <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')
  
  mef_p <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  mea_p <- plm(prod_mamona ~ estimador_dd + data_treat + polos + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(mp_p)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(mef_p,mp_p)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(mea_p,mef_p)
  
# Renda média Mamona
  
  mp_r <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')
  
  mef_r <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  mea_r <- plm(rm_mamona ~ estimador_dd + data_treat + polos + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')
  
# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(mp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(mef_r,mp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(mea_r,mef_r)
  
# Produtividade Soja
  
  sp_p <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + sudeste + coeste+
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')
  
  sef_p <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + sudeste + coeste+
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  sea_p <- plm(prod_soja ~ estimador_dd + data_treat + polos + sudeste + coeste+
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(sp_p)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(sef_p,sp_p)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(sea_p,sef_p)
  
# Renda média Soja
  
  sp_r <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + sudeste + coeste+
                sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
              data=painel,model='pooling')
  
  sef_r <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='within')
  
  sea_r <- plm(rm_soja ~ estimador_dd + data_treat + polos + sudeste + coeste +
                 sul + norte + semiarido + total.contratos + d.bio + vaba + pib.per.capta,
               data=painel,model='random')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(sp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(sef_r,sp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(sea_r,sef_r)
  
# Equações Produtividade----
  
  stargazer(dp_p,def_p,gp_p,gef_p,mp_p,mef_p,sp_p,sef_p,type='text',
            omit.stat=c('LL','ser','f'),out='produtividade_culturas_r.html')
  
# Equações renda media----
  
  stargazer(dp_r,def_r,gp_r,gef_r,mp_r,mef_r,sp_r,sef_r,type='text',
            omit.stat=c('LL','ser','f'),out='renda_media_culturas_r.html')
  
# Equações Dendê----
  
  stargazer(dp_p,def_p,dp_r,def_r,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média'),out='dende.html')
  
# Equações Girassol----

  stargazer(gp_p,gef_p,gp_r,gef_r,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média'),out='girassol.html')  
  
# Equações Mamona----

  stargazer(mp_p,mef_p,mp_r,mef_r,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média'),out='mamona.html')  
  
# Equações Soja----
  
  stargazer(sp_p,sef_p,sp_r,sef_r,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média'),out='soja.html')

# Limpando equações da memória
  
  rm(list=ls())