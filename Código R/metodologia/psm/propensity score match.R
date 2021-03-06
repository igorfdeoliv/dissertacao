#Notas SIDRA IBGE (PAM)

  #5-Os produtos girassol e triticale s� apresentam informa��o a partir de 2005.

  #12-Valor da produ��o: vari�vel derivada calculada pela m�dia ponderada das
#informa��es de quantidade e pre�o m�dio corrente pago ao produtor, de acordo
#com os per�odos de colheita e comercializa��o de cada produto. As despesas de
#frete e impostos n�o s�o inclu�das no pre�o.

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

# Limpando mem�ria----

  rm(list=ls())

# Importando base de dados----

  base <- 'https://raw.githubusercontent.com/igorfdeoliv/dissertacao/main/dataset/base/base.csv'

  pnpb <- read.csv(base,sep=';',dec='.')
  
  rm(base)

# An�lise descritiva das vari�veis de interesse antes do pareamento----
  
# Produtividade (quantidade (ton) / �rea plantada (hec))
  
  # Dend� (Cacho de coco)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_dende),
              std_error = sd(prod_dende) / sqrt(n_municipios))

  # Girassol(em gr�o)
  
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
  
  # Soja (em gr�o)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_soja),
              std_error = sd(prod_soja) / sqrt(n_municipios))
  
# Renda m�dia (Valor da produ��o (R$) / quantidade produzida (ton))
  
  # Dend� (Cacho de coco)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(rm_dende),
              std_error = sd(rm_dende) / sqrt(n_municipios))
  
  # Girassol(em gr�o)
  
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
  
  # Soja (em gr�o)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(rm_soja),
              std_error = sd(rm_soja) / sqrt(n_municipios))
  
# Sal�rios m�dios recebidos relacionados �s culturas (R$)
  
  # Dend� (Cacho de coco)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(s.dende),
              std_error = sd(s.dende) / sqrt(n_municipios))
  
  # Girassol(em gr�o)
  
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
  
  # Soja (em gr�o)
  
  pnpb %>% 
    group_by(polos) %>% 
    summarise(n_municipios=n(),
              mean_match = mean(prod_soja),
              std_error = sd(prod_soja) / sqrt(n_municipios))
  
# Teste t para verificar se as m�dias entre os grupos s�o iguais----
  
  # H0: p-valor > 0.05 m�dias s�o estat�ticamente iguais
  
  #Produtividade
  
  with(pnpb,t.test(prod_dende ~ polos)) # significativo
  
  with(pnpb,t.test(prod_girassol ~ polos)) # significativo
  
  with(pnpb,t.test(prod_mamona ~ polos)) # significativo
  
  with(pnpb,t.test(prod_soja ~ polos)) # significativo
  
  #Renda m�dia
  
  with(pnpb,t.test(rm_dende ~ polos)) # significativo
  
  with(pnpb,t.test(rm_girassol ~ polos)) # significativo
  
  with(pnpb,t.test(rm_mamona ~ polos)) # significativo
  
  with(pnpb,t.test(rm_soja ~ polos)) # significativo
  
  #Sal�rios
  
  with(pnpb,t.test(s.dende ~ polos)) # significativo
  
  with(pnpb,t.test(s.girassol ~ polos)) # n�o significativo
  
  with(pnpb,t.test(s.mamona ~ polos)) # significativo a 10%
  
  with(pnpb,t.test(s.soja ~ polos)) # significativo 
  
# Teste de MANN-WHITNEY----
  
  # H0: p-valor > 0.05 m�dias s�o estatisticamente iguais
  
  # Dend�
  
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
  
# Resumo estat�stico das vari�veis de interesse----
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(prod_dende,prod_girassol,prod_mamona,prod_soja,
                      type='mean_sd')
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(rm_dende,rm_girassol,rm_mamona,rm_soja,
                      type='mean_sd')
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(s.dende,s.girassol,s.mamona,s.soja,
                      type='mean_sd')
  
# Diferen�a entre m�dias: vari�veis de pr�-tratamento----
  
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
  
# Representa��o gr�fica das probabilidades preditas de participar do projeto polos----
  
  labs <- paste('Participa do Projeto Polos de Biodiesel:',c('Sim','N�o'))
  
  prs_df %>% 
    mutate(polos=ifelse(polos==1,labs[1],labs[2]
                        )) %>% 
    ggplot(aes(x=pr_score)) + geom_histogram(color='white') +
    facet_wrap(~polos) + xlab('Probabilidade de participar do Projeto Polos') +
    ylab('N� Munic�pios') + 
    theme_bw()

# Iniciando matching----
  
  matching <- matchit(polos ~ total.contratos + valores.totais + d.bio +
                        vaba + t,data=pnpb,link='probit',method='nearest',
                      ratio=1)
  
  summary(matching)
  
# Gr�fico de ajustamento das covariadas antes e depois do pareamento----
  
  plot(matching)
  
# Sobreposi��o dos grupos de controle e tratamento ap�s o pareamento----
  
  bal.plot(matching,var.name='distance')
  
# Declarando nova base ap�s o pareamento----
  
  psm <- match.data(matching)
  head(psm)
  dim(psm)
  
# Verificando as m�dias das covariadas ap�s o pareamento----
  
  psm %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean))
  
  lapply(pnpb_cov,function(v) {
    t.test(psm[,v] ~ psm$polos)
  })

  rm(m_ps,matching,pnpb,prs_df,labs,pnpb_cov)
    
# Salvando nova base----
  
  setwd('C:/Users/igorf/Documents/GitHub/dissertacao/dataset/base')
  
  write.table(psm,file='psm.csv',sep=';',dec='.',na='',quote=TRUE, row.names=FALSE)