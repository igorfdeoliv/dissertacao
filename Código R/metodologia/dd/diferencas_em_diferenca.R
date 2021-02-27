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
        y='Renda Média Dendê') +
   theme_light()
 
 # Renda média do girassol
 
 r2 <- ggplot(df,aes(ano,rm_girassol,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média Girassol') +
   theme_light()
 
 # Renda média mamona
 
 r3 <- ggplot(df,aes(ano,rm_mamona,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média Mamona') +
   theme_light()
 
 # Renda média Soja
 
 r4 <- ggplot(df,aes(ano,rm_soja,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Renda Média Soja') +
   theme_light()
 
 plot_grid(r1,r2,r3,r4)
 rm(r1,r2,r3,r4)
 
# Salários médios relacionados às oleaginosas----
 
 # Salários médios dendê
 
 s1 <- ggplot(df,aes(ano,s.dende,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Salários médios \n relacionados ao Dendê') +
   theme_light()
 
 # Salários médios girassol
 
 s2 <- ggplot(df,aes(ano,s.girassol,color=tratamento)) +
   stat_summary(geom='line') +
   geom_vline(xintercept=2006) +
   labs(x='Período',
        y='Salários médios \n realacionados ao Girassol') +
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
 
  reg1 <- plm(prod_dende ~ polos + data_treat + estimador_dd + total.contratos + 
                h.dende + d.bio + vaba + est_pop,data=painel,model='pooling')

  reg2 <- plm(prod_dende ~ polos + data_treat + estimador_dd + total.contratos +
               h.dende + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg3 <- plm(prod_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='within')

  stargazer(reg1,reg2,reg3,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Produtividade')
  
 # Renda média Dendê
  
  reg4 <- plm(rm_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg5 <- plm(rm_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg6 <- plm(rm_dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='within')

  stargazer(reg4,reg5,reg6,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Renda Média')
  
  # Salários médios Dendê
  
  reg7 <- plm(s.dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg8 <- plm(s.dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg9 <- plm(s.dende ~ polos + data_treat + estimador_dd + total.contratos +
                h.dende + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg7,reg8,reg9,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Salários')
  
  rm(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9)
  
  # Produtividade Girassol
  
  reg1 <- plm(prod_girassol ~ polos + data_treat + estimador_dd + total.contratos + 
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg2 <- plm(prod_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg3 <- plm(prod_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg1,reg2,reg3,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Produtividade')
  
  # Renda média Girassol
  
  reg4 <- plm(rm_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg5 <- plm(rm_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg6 <- plm(rm_girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg4,reg5,reg6,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Renda Média')
  
  # Salários médios Girassol
  
  reg7 <- plm(s.girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg8 <- plm(s.girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg9 <- plm(s.girassol ~ polos + data_treat + estimador_dd + total.contratos +
                 h.girassol + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg7,reg8,reg9,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Salários')
  
  rm(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9)
  
  # Produtividade Mamona
  
  reg1 <- plm(prod_mamona ~ polos + data_treat + estimador_dd + total.contratos + 
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg2 <- plm(prod_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg3 <- plm(prod_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg1,reg2,reg3,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Produtividade')
  
  # Renda média Mamona
  
  reg4 <- plm(rm_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg5 <- plm(rm_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg6 <- plm(rm_mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg4,reg5,reg6,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Renda Média')
  
  # Salários médios Mamona
  
  reg7 <- plm(s.mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg8 <- plm(s.mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg9 <- plm(s.mamona ~ polos + data_treat + estimador_dd + total.contratos +
                 h.mamona + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg7,reg8,reg9,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Salários')
  
  rm(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9)
  
  # Produtividade Soja
  
  reg1 <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos + 
                 h.soja + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg2 <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg3 <- plm(prod_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg1,reg2,reg3,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Produtividade')
  
  # Renda média Soja
  
  reg4 <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg5 <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg6 <- plm(rm_soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg4,reg5,reg6,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Renda Média')
  
  # Salários médios Soja
  
  reg7 <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='pooling')
  
  reg8 <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='random')
  
  reg9 <- plm(s.soja ~ polos + data_treat + estimador_dd + total.contratos +
                 h.soja + d.bio + vaba + est_pop,data=painel,model='within')
  
  stargazer(reg7,reg8,reg9,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels='Salários')
  
  rm(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9)