#Notas SIDRA IBGE (PAM)

  #5-Os produtos girassol e triticale só apresentam informação a partir de 2005.

  #12-Valor da produção: variável derivada calculada pela média ponderada das
#informações de quantidade e preço médio corrente pago ao produtor, de acordo
#com os períodos de colheita e comercialização de cada produto. As despesas de
#frete e impostos não são incluídas no preço.

# Importando pacotes para trabalho----

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

  if(!require(rstatix))
    install.packages('rstatix')

  if(!require(stargazer))
    install.packages("stargazer")

# Limpando memória----

  rm(list=ls())

# Importando base de dados----

  base <- 'https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/base.csv'

  pnpb <- read.csv(base,sep=';',dec='.')
  
  rm(base)
  
# Criando dummy para regiao----
  

# Análise descritiva das variáveis de interesse antes do pareamento----
  
# Produtividade (quantidade (ton) / área plantada (hec))
  
  # Dendê (Cacho de coco)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_dende),
              std_error = sd(prod_dende) / sqrt(n_municipios))

  # Girassol(em grão)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_girassol),
              std_error = sd(prod_girassol) / sqrt(n_municipios))
  
  # Mamona (baga)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_mamona),
              std_error = sd(prod_mamona) / sqrt(n_municipios))
  
  # Soja (em grão)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_soja),
              std_error = sd(prod_soja) / sqrt(n_municipios))
  
# Renda média (Valor da produção (R$) / area plantada (ha))
  
  # Dendê (Cacho de coco)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(rm_dende),
              std_error = sd(rm_dende) / sqrt(n_municipios))
  
  # Girassol(em grão)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(rm_girassol),
              std_error = sd(rm_girassol) / sqrt(n_municipios))
  
  # Mamona (baga)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(rm_mamona),
              std_error = sd(rm_mamona) / sqrt(n_municipios))
  
  # Soja (em grão)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(rm_soja),
              std_error = sd(rm_soja) / sqrt(n_municipios))
  
# Salários médios recebidos relacionados às culturas (R$)
  
  # Dendê (Cacho de coco)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(s.dende),
              std_error = sd(s.dende) / sqrt(n_municipios))
  
  # Girassol(em grão)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(s.girassol),
              std_error = sd(s.girassol) / sqrt(n_municipios))
  
  # Mamona (baga)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(s.mamona),
              std_error = sd(s.mamona) / sqrt(n_municipios))
  
  # Soja (em grão)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_soja),
              std_error = sd(prod_soja) / sqrt(n_municipios))
  
# Análise descritiva dados----
  
  tb_prod_soja <- pnpb %>% group_by(polos, norte) %>% 
    summarise(média = mean(v.mamona),
              desvio_padrao = sd(v.mamona),
              mediana = median(v.mamona))
  
  table(pnpb$uf)
  table(pnpb$norte)
  
  describeBy(pnpb$prod_soja, group=pnpb$d.ne:pnpb$polos)
  
# Teste t para verificar se as médias entre os grupos são iguais----
  
  # H0: p-valor > 0.05 médias são estatíticamente iguais
  
  #Produtividade
  
  with(pnpb,t.test(prod_dende ~ polos)) # significativo
  
  with(pnpb,t.test(prod_girassol ~ polos)) # significativo
  
  with(pnpb,t.test(prod_mamona ~ polos)) # significativo
  
  with(pnpb,t.test(prod_soja ~ polos)) # significativo
  
  #Renda média
  
  with(pnpb,t.test(rm_dende ~ polos)) # significativo
  
  with(pnpb,t.test(rm_girassol ~ polos)) # significativo
  
  with(pnpb,t.test(rm_mamona ~ polos)) # significativo
  
  with(pnpb,t.test(rm_soja ~ polos)) # significativo
  
  #Salários
  
  with(pnpb,t.test(s.dende ~ polos)) # significativo
  
  with(pnpb,t.test(s.girassol ~ polos)) # não significativo
  
  with(pnpb,t.test(s.mamona ~ polos)) # significativo a 10%
  
  with(pnpb,t.test(s.soja ~ polos)) # significativo 
  
# Teste de MANN-WHITNEY----
  
  # H0: p-valor > 0.05 médias são estatisticamente iguais
  
  # Dendê
  
  wilcox.test(prod_dende ~ polos, data=pnpb)
  
  wilcox.test(rm_dende ~ polos, data=pnpb)
  
  wilcox.test(s.dende ~ polos, data=pnpb)
  
  # Girassol

  wilcox.test(prod_girassol ~ polos, data=pnpb)
  
  wilcox.test(rm_girassol ~ polos, data=pnpb)
  
  wilcox.test(s.girassol ~ polos, data=pnpb)
    
  # Mamona
  
  wilcox.test(prod_mamona ~ polos, data=pnpb)
  
  wilcox.test(rm_mamona ~ polos, data=pnpb)
  
  wilcox.test(s.mamona ~ polos, data=pnpb)
  
  # Soja
    
  wilcox.test(prod_soja ~ polos, data=pnpb)
  
  wilcox.test(rm_soja ~ polos, data=pnpb)
  
  wilcox.test(s.soja ~ polos, data=pnpb)
  
# Resumo estatístico das variáveis de interesse----
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(prod_dende,prod_girassol,prod_mamona,prod_soja,
                      type='mean_sd')
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(rm_dende,rm_girassol,rm_mamona,rm_soja,
                      type='mean_sd')
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(s.dende,s.girassol,s.mamona,s.soja,
                      type='mean_sd')
  
# Diferença entre médias: variáveis de pré-tratamento----
  
  pnpb_cov <- c('total.contratos','valores.totais','d.bio','vaba','t')
  
  pnpb %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean(.,na.rm=TRUE)))
  
  lapply(pnpb_cov,function(v) {
    t.test(pnpb[,v] ~ pnpb[,'polos'])
  })
  
  m_ps <- glm(polos ~ total.contratos + valores.totais + d.bio +
                vaba + t,family=binomial(),data=pnpb)
  summary(m_ps)
  
  stargazer(m_ps,type='text',omit.stat=c('LL','ser','f'),
            dep.var.labels=c('Tratamento'),out='modbin.html')
  
# Calculando as probabilidades preditas de participar do projeto polos----
  
  prs_df <- data.frame(pr_score=predict(m_ps, type='response'),
                       polos=m_ps$model$polos)
  head(prs_df)
  
# Representação gráfica das probabilidades preditas de participar do projeto polos----
  
  labs <- paste(c('Municípios Polos','Municípios Não Polos'))
  
  prs_df %>% 
    mutate(polos=ifelse(polos==1,labs[1],labs[2]
                        )) %>% 
    ggplot(aes(x=pr_score)) + geom_histogram(color='white') +
    facet_wrap(~polos) + xlab('Probabilidade escore') +
    ylab('Número de Municípios') + 
    theme_bw()

# Iniciando matching----
  
  matching <- matchit(polos ~ total.contratos + valores.totais + d.bio +
                        vaba + t,data=pnpb,link='probit',method='nearest',
                      ratio=1)
  
  summary(matching)
  
# Sobreposição da densidade da amostra antes e após o pareamento----
  
  bal.plot(matching,var.name='distance',which='both',position='bottom',
           sample.names=c('(a) antes','(b) depois'),
           colors =c('white','black')) +
    labs(title=NULL,x='Distância', 
         y='Densidade',fill=NULL)
  
# Balanceamento da amostra antes e após o pareamento----
  
  bal.plot(matching,type='histogram', mirror=TRUE,which = 'both',
           sample.names=c('(a) antes','(b) depois'),position='bottom',
           colors =c('white','black')) +
    labs(title=NULL,x='Distância', y='Proporção',fill=NULL)
  
# Declarando nova base após o pareamento----
  
  psm <- match.data(matching)
  head(psm)
  dim(psm)
  
# Verificando as médias das covariadas após o pareamento----
  
  psm %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean))
  
  lapply(pnpb_cov,function(v) {
    t.test(psm[,v] ~ psm$polos)
  })

  rm(m_ps,matching,pnpb,prs_df,labs,pnpb_cov)
    
# Salvando nova base----
  
  setwd('E:/igorf/Documents/GitHub/dissertacao/dataset/base')
  
  write.table(psm,file='psm.csv',sep=';',dec='.',na='',quote=TRUE, row.names=FALSE)