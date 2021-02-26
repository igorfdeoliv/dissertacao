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

  if(!require(sandwich))
    install.packages('sandwich')

  if(!require(stargazer))
    install.packages("stargazer")

# Limpando memória----

  rm(list=ls())

# Importando base de dados----

  base <- 'https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/base.csv'

  pnpb <- read.csv(base,sep=';',dec='.')
  
  rm(base)
  
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
  
# Renda média (Valor da produção (R$) / quantidade produzida (ton))
  
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
  
# A diferença entre as médias dos grupos de controle e tratamento são diferentes
# estatísticamente ao nível de 5% de significância ?----
  
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
  
# Calculando as probabilidades preditas de participar do projeto polos----
  
  prs_df <- data.frame(pr_score=predict(m_ps, type='response'),
                       polos=m_ps$model$polos)
  head(prs_df)
  
# Representação gráfica das probabilidades preditas de participar do projeto polos----
  
  labs <- paste('Participa do Projeto Polos de Biodiesel:',c('Sim','Não'))
  
  prs_df %>% 
    mutate(polos=ifelse(polos==1,labs[1],labs[2]
                        )) %>% 
    ggplot(aes(x=pr_score)) + geom_histogram(color='white') +
    facet_wrap(~polos) + xlab('Probabilidade de participar do Projeto Polos') +
    ylab('Nº Municípios') + 
    theme_bw()
    