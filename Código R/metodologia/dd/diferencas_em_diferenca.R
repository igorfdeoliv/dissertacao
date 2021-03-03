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
          tratamento = polos >= 1)
 
# Quantidade produzida----

 # Dendê
 
 q1 <- ggplot(df,aes(ano,q.dende,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produção Dendê (t)') +
   theme_light()
 
 # Girassol
 
 q2 <- ggplot(df,aes(ano,q.girassol,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produção Girassol (t)') +
   theme_light()
 
 # Mamona
 
 q3 <- ggplot(df,aes(ano,q.mamona,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produção Mamona (t)') +
   theme_light()
 
 # Soja
 
 q4 <- ggplot(df,aes(ano,q.soja,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produção Soja (t)') +
   theme_light()
 
 plot_grid(q1,q2,q3,q4)
 rm(q1,q2,q3,q4)
  
# Produtividade das oleaginosas----
 
  # Produtividade do dendê
 
 p1 <- ggplot(df,aes(ano,prod_dende,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
          y='Produtividade Dendê') +
   theme_light()
 
 # Produtividade do girassol
 
 p2 <- ggplot(df,aes(ano,prod_girassol,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produtividade Girassol') +
   theme_light()
 
 # Produtividade mamona
 
 p3 <- ggplot(df,aes(ano,prod_mamona,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produtividade Mamona') +
   theme_light()
 
 # Produtividade Soja
 
 p4 <- ggplot(df,aes(ano,prod_soja,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Produtividade Soja') +
   theme_light()
 
 plot_grid(p1,p2,p3,p4)
 rm(p1,p2,p3,p4)
 
# Renda média das oleaginosas----
 
 # Renda média do dendê
 
 r1 <- ggplot(df,aes(ano,rm_dende,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média \n Dendê') +
   theme_light()
 
 # Renda média do girassol
 
 r2 <- ggplot(df,aes(ano,rm_girassol,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média \n Girassol') +
   theme_light()
 
 # Renda média mamona
 
 r3 <- ggplot(df,aes(ano,rm_mamona,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média \n Mamona') +
   theme_light()
 
 # Renda média Soja
 
 r4 <- ggplot(df,aes(ano,rm_soja,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média \n Soja') +
   theme_light()
 
 plot_grid(r1,r2,r3,r4)
 rm(r1,r2,r3,r4)
 
# Salários médios relacionados às oleaginosas----
 
 # Salários médios dendê
 
 s1 <- ggplot(df,aes(ano,s.dende,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Salários médios \n relacionados \n ao Dendê') +
   theme_light()
 
 # Salários médios girassol
 
 s2 <- ggplot(df,aes(ano,s.girassol,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Salários médios \n realacionados \n ao Girassol') +
   theme_light()
 
 # Salários médios mamona
 
 s3 <- ggplot(df,aes(ano,s.mamona,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Salários médios \n relacionados à Mamona') +
   theme_light()
 
 # Salários médios Soja
 
 s4 <- ggplot(df,aes(ano,s.soja,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Salários médios \n relacionados à Soja') +
   theme_light()
 
 plot_grid(s1,s2,s3,s4)
 rm(s1,s2,s3,s4,df)
 
# Declarando que a base de dados é um painel----
 
 painel <- pdata.frame(psm,index=c('chave','ano'))
 
 rm(psm)
 
# Estimação dos modelos----
 
# Produtividade Dendê
 
  dp_p <- plm(prod_dende ~ polos + data_treat + estimador_dd + total.contratos + 
                h.dende + d.bio + vaba + est_pop,data=painel,model='pooling')

  dea_p <- plm(prod_dende ~ polos + data_treat + estimador_dd + total.contratos +
               h.dende + d.bio + vaba + est_pop,data=painel,model='random')
  
  def_p <- plm(prod_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='within')
  
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
  
  dp_r <- plm(rm_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  dea_r <- plm(rm_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='random')
  
  def_r <- plm(rm_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='within')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(dp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(def_r,dp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(dea_r,def_r)
  
# Salários médios Dendê
  
  dp_s <- plm(s.dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  dea_s <- plm(s.dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='random')
  
  def_s <- plm(s.dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='within')
  
# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(dp_s)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(def_s,dp_s)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(dea_s,def_s)
  
# Produtividade Girassol
  
  gp_p <- plm(prod_girassol ~ polos + data_treat + estimador_dd + total.contratos + 
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  gea_p <- plm(prod_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='random')
  
  gef_p <- plm(prod_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='within')
  
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
  
  gp_r <- plm(rm_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  gea_r <- plm(rm_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='random')
  
  gef_r <- plm(rm_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='within')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(gp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(gef_r,gp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(gea_r,gef_r)
  
# Salários médios Girassol
  
  gp_s <- plm(s.girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  gea_s <- plm(s.girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='random')
  
  gef_s <- plm(s.girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='within')
  
# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(gp_s)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(gef_s,gp_s)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(gea_s,gef_s)

# Produtividade Mamona
  
  mp_p <- plm(prod_mamona ~ polos + data_treat + estimador_dd + total.contratos + 
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  mea_p <- plm(prod_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='random')
  
  mef_p <- plm(prod_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='within')

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
  
  mp_r <- plm(rm_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  mea_r <- plm(rm_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='random')
  
  mef_r <- plm(rm_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='within')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(mp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(mef_r,mp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(mea_r,mef_r)
  
# Salários médios Mamona
  
  mp_s <- plm(s.mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  mea_s <- plm(s.mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='random')
  
  mef_s <- plm(s.mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='within')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(mp_s)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(mef_s,mp_s)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(mea_s,mef_s)
  
# Produtividade Soja
  
  sp_p <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos + 
                 h.soja + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  sea_p <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='random')
  
  sef_p <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='within')

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
  
  sp_r <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  sea_r <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='random')
  
  sef_r <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='within')

# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(sp_r)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(sef_r,sp_r)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(sea_r,sef_r)
  
# Salários médios Soja
  
  sp_s <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  sea_s <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='random')
  
  sef_s <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='within')
  
# Teste LM para EA x MQO
  
  # H0: modelo pooled ; H1: modelo EA
  
  plmtest(sp_s)
  
# Teste F para EF x MQO
  
  # H0: modelo pooled ; H1: EF
  
  pFtest(sef_s,sp_s)
  
# Teste de Hausman para EA x EF
  
  # H0: EA ; H1: EF
  
  phtest(sea_s,sef_s)
  
# Equações Dendê----
  
  stargazer(dp_p,def_p,dp_r,def_r,dp_s,def_s,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média','Salários'),out='dende.html')
  
# Equações Girassol----

  stargazer(gp_p,gef_p,gp_r,gef_r,gp_s,gef_s,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média','Salários'),out='girassol.html')  
  
# Equações Mamona----

  stargazer(mp_p,mef_p,mp_r,mef_r,mp_s,mef_s,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média','Salários'),out='mamona.html')  
  
# Equações Soja----
  
  stargazer(sp_p,sef_p,sp_r,sef_r,sp_s,sef_s,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Produtividade','Renda Média','Salários'),out='soja.html')

# Limpando equações da memória
  
  rm(dp_p,dp_r,dp_s,dea_p,dea_r,dea_s,def_p,def_r,def_s,gp_p,gp_r,gp_s,
     gea_p,gea_r,gea_s,gef_p,gef_r,gef_s,mp_p,mp_r,mp_s,mea_p,mea_r,mea_s,
     mef_p,mef_r,mef_s,sp_p,sp_r,sp_s,sea_p,sea_r,sea_s,sef_p,sef_r,sef_s)