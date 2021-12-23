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
  
  rm(dp_p,dp_r,dp_s,dea_p,dea_r,dea_s,def_p,def_r,def_s,gp_p,gp_r,gp_s,
     gea_p,gea_r,gea_s,gef_p,gef_r,gef_s,mp_p,mp_r,mp_s,mea_p,mea_r,mea_s,
     mef_p,mef_r,mef_s,sp_p,sp_r,sp_s,sea_p,sea_r,sea_s,sef_p,sef_r,sef_s)
  
# Verificando o efeito por região + semiárido----
  
  # Separando a amosrta por região
  
  nordeste <- painel %>% 
    filter(regiao=='Nordeste')
  
  norte <- painel %>% 
    filter(regiao=='Norte')
  
  sudeste <- painel %>% 
    filter(regiao=='Sudeste')
  
  sul <- painel %>% 
    filter(regiao=='Sul')
  
  coeste <- painel %>% 
    filter(regiao=='Centro-oeste')
  
  semiarido <- painel %>% 
    filter(semiarido==1)

# Estimação para regiões----
  
  # Produtividade Dendê----
  
  dp_p_ne <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  def_p_ne <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=nordeste,model='within')

  dp_p_s <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.dende + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  def_p_s <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  dp_p_co <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  def_p_co <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  dp_p_sd <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  def_p_sd <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=sudeste,model='within')

  dp_p_n <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.dende + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  def_p_n <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=norte,model='within')
  
  dp_p_sa <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  def_p_sa <- plm(prod_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Renda média Dendê----
  
  dp_r_ne <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  def_r_ne <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  dp_r_s <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.dende + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  def_r_s <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  dp_r_co <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  def_r_co <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  dp_r_sd <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  def_r_sd <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.dende + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  dp_r_n <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.dende + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  def_r_n <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=norte,model='within')

  dp_r_sa <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.dende + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  def_r_sa <- plm(rm_dende ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.dende + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Equações Dendê----
  
  stargazer(dp_p_sa,def_p_sa,dp_p_ne,def_p_ne,dp_p_s,def_p_s,dp_p_co,def_p_co,
            dp_p_sd,def_p_sd,dp_p_n,def_p_n,type='text',omit.stat=c('LL','ser','f'),
            out='produtividade_dende_regioes.html')
  
  rm(dp_p_sa,def_p_sa,dp_p_ne,def_p_ne,dp_p_s,def_p_s,dp_p_co,def_p_co,
     dp_p_sd,def_p_sd,dp_p_n,def_p_n)
  
  stargazer(dp_r_sa,def_r_sa,dp_r_ne,def_r_ne,dp_r_s,def_r_s,dp_r_co,def_r_co,
            dp_r_sd,def_r_sd,dp_r_n,def_r_n,type='text',omit.stat=c('LL','ser','f'),
            out='renda_media_dende_regioes.html')
  
  rm(dp_r_sa,def_r_sa,dp_r_ne,def_r_ne,dp_r_s,def_r_s,dp_r_co,def_r_co,
     dp_r_sd,def_r_sd,dp_r_n,def_r_n)
  
# Produtividade Girassol----
  
  gp_p_ne <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  gef_p_ne <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.girassol + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  gp_p_s <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.girassol + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  gef_p_s <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  gp_p_co <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  gef_p_co <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.girassol + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  gp_p_sd <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  gef_p_sd <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.girassol + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  gp_p_n <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.girassol + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  gef_p_n <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=norte,model='within')

  gp_p_sa <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.girassol + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  gef_p_sa <- plm(prod_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Renda média Girassol----
  
  gp_r_ne <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  gef_r_ne <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.girassol + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  gp_r_s <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.girassol + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  gef_r_s <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  gp_r_co <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  gef_r_co <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.girassol + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  gp_r_sd <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  gef_r_sd <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.girassol + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  gp_r_n <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.girassol + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  gef_r_n <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=norte,model='within')

  gp_r_sa <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.girassol + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  gef_r_sa <- plm(rm_girassol ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.girassol + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Equações Girassol----
  
  stargazer(gp_p_sa,gef_p_sa,gp_p_ne,gef_p_ne,gp_p_s,gef_p_s,gp_p_co,gef_p_co,
            gp_p_sd,gef_p_sd,gp_p_n,gef_p_n,type='text',omit.stat=c('LL','ser','f'),
            out='produtividade_girassol_regioes.html')
  
  rm(gp_p_sa,gef_p_sa,gp_p_ne,gef_p_ne,gp_p_s,gef_p_s,gp_p_co,gef_p_co,
     gp_p_sd,gef_p_sd,gp_p_n,gef_p_n)
  
  stargazer(gp_r_sa,gef_r_sa,gp_r_ne,gef_r_ne,gp_r_s,gef_r_s,gp_r_co,gef_r_co,
            gp_r_sd,gef_r_sd,gp_r_n,gef_r_n,type='text',omit.stat=c('LL','ser','f'),
            out='renda_media_girassol_regioes.html')
  
  rm(gp_r_sa,gef_r_sa,gp_r_ne,gef_r_ne,gp_r_s,gef_r_s,gp_r_co,gef_r_co,
     gp_r_sd,gef_r_sd,gp_r_n,gef_r_n)
  
# Produtividade Mamona----
  
  mp_p_ne <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  mef_p_ne <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.mamona + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  mp_p_s <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.mamona + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  mef_p_s <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  mp_p_co <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  mef_p_co <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.mamona + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  mp_p_sd <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  mef_p_sd <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.mamona + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  mp_p_n <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.mamona + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  mef_p_n <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=norte,model='within')

  mp_p_sa <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.mamona + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  mef_p_sa <- plm(prod_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Renda média Mamona----
  
  mp_r_ne <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  mef_r_ne <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.mamona + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  mp_r_s <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.mamona + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  mef_r_s <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  mp_r_co <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  mef_r_co <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.mamona + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  mp_r_sd <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  mef_r_sd <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.mamona + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  mp_r_n <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.mamona + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  mef_r_n <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=norte,model='within')

  mp_r_sa <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.mamona + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  mef_r_sa <- plm(rm_mamona ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.mamona + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Equações Mamona----
  
  stargazer(mp_p_sa,mef_p_sa,mp_p_ne,mef_p_ne,mp_p_s,mef_p_s,mp_p_co,mef_p_co,
            mp_p_sd,mef_p_sd,mp_p_n,mef_p_n,type='text',omit.stat=c('LL','ser','f'),
            out='prod_mamona_regioes.html')
  
  rm(mp_p_sa,mef_p_sa,mp_p_ne,mef_p_ne,mp_p_s,mef_p_s,mp_p_co,mef_p_co,
     mp_p_sd,mef_p_sd,mp_p_n,mef_p_n)
  
  stargazer(mp_r_sa,mef_r_sa,mp_r_ne,mef_r_ne,mp_r_s,mef_r_s,mp_r_co,mef_r_co,
            mp_r_sd,mef_r_sd,mp_r_n,mef_r_n,type='text',omit.stat=c('LL','ser','f'),
            out='renda_media_mamona_regioes.html')
  
  rm(mp_r_sa,mef_r_sa,mp_r_ne,mef_r_ne,mp_r_s,mef_r_s,mp_r_co,mef_r_co,
     mp_r_sd,mef_r_sd,mp_r_n,mef_r_n)
  
# Produtividade Soja----
  
  sp_p_ne <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  sef_p_ne <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.soja + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  sp_p_s <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.soja + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  sef_p_s <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  sp_p_co <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  sef_p_co <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.soja + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  sp_p_sd <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  sef_p_sd <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.soja + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  sp_p_n <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.soja + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  sef_p_n <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=norte,model='within')

  sp_p_sa <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.soja + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  sef_p_sa <- plm(prod_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=semiarido,model='within')
  
# Renda média Soja----
  
  sp_r_ne <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=nordeste,model='pooling')
  
  sef_r_ne <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.soja + d.bio + vaba + pib.per.capta,data=nordeste,model='within')
  
  sp_r_s <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.soja + d.bio + vaba + pib.per.capta,data=sul,model='pooling')
  
  sef_r_s <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=sul,model='within')
  
  sp_r_co <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=coeste,model='pooling')
  
  sef_r_co <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.soja + d.bio + vaba + pib.per.capta,data=coeste,model='within')
  
  sp_r_sd <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=sudeste,model='pooling')
  
  sef_r_sd <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                    h.soja + d.bio + vaba + pib.per.capta,data=sudeste,model='within')
  
  sp_r_n <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.soja + d.bio + vaba + pib.per.capta,data=norte,model='pooling')
  
  sef_r_n <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=norte,model='within')

  sp_r_sa <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                  h.soja + d.bio + vaba + pib.per.capta,data=semiarido,model='pooling')
  
  sef_r_sa <- plm(rm_soja ~ estimador_dd + data_treat + polos + ano + total.contratos +
                   h.soja + d.bio + vaba + pib.per.capta,data=semiarido,model='within')

# Equações Soja----
  
  stargazer(sp_p_sa,sef_p_sa,sp_p_ne,sef_p_ne,sp_p_s,sef_p_s,sp_p_co,sef_p_co,
            sp_p_sd,sef_p_sd,sp_p_n,sef_p_n,type='text',omit.stat=c('LL','ser','f'),
            out='produtividade_soja_regioes.html')

  rm(sp_p_sa,sef_p_sa,sp_p_ne,sef_p_ne,sp_p_s,sef_p_s,sp_p_co,sef_p_co,
     sp_p_sd,sef_p_sd,sp_p_n,sef_p_n)
  
  stargazer(sp_r_sa,sef_r_sa,sp_r_ne,sef_r_ne,sp_r_s,sef_r_s,sp_r_co,sef_r_co,
            sp_r_sd,sef_r_sd,sp_r_n,sef_r_n,type='text',omit.stat=c('LL','ser','f'),
            out='renda_media_soja_regioes.html')
  
  rm(sp_r_sa,sef_r_sa,sp_r_ne,sef_r_ne,sp_r_s,sef_r_s,sp_r_co,sef_r_co,
     sp_r_sd,sef_r_sd,sp_r_n,sef_r_n)
  
  rm(coeste,nordeste,norte,semiarido,sudeste,sul)