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
  
  pnpb <- pnpb %>% 
    mutate(norte=if_else(regiao=='Norte',1,0))
  
  pnpb <- pnpb %>% 
    mutate(nordeste=if_else(regiao=='Nordeste',1,0))

  pnpb <- pnpb %>% 
    mutate(sul=if_else(regiao=='Sul',1,0))
  
  pnpb <- pnpb %>% 
    mutate(sudeste=if_else(regiao=='Sudeste',1,0))
  
  pnpb <- pnpb %>% 
    mutate(coeste=if_else(regiao=='Centro-oeste',1,0))
  
# Comparação entre as médias dos polos antes do pareamento----
  
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
  
# Análise descritiva dados----
  
  # Média da Área plantada por cultura
  
  area.dende <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(h.dende),
              desvio_padrao = sd(h.dende),
              mediana = median(h.dende))

  area.girassol <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(h.girassol),
              desvio_padrao = sd(h.girassol),
              mediana = median(h.girassol))
  
  area.mamona <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(h.mamona),
              desvio_padrao = sd(h.mamona),
              mediana = median(h.mamona))
  
  area.soja <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(h.soja),
              desvio_padrao = sd(h.soja),
              mediana = median(h.soja))
    
  # Média da Quantidade produzida por cultura
  
  quant.dende <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(q.dende),
              desvio_padrao = sd(q.dende),
              mediana = median(q.dende))
  
  quant.girassol <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(q.girassol),
              desvio_padrao = sd(q.girassol),
              mediana = median(q.girassol))
  
  quant.mamona <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(q.mamona),
              desvio_padrao = sd(q.mamona),
              mediana = median(q.mamona))
  
  quant.soja <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(q.soja),
              desvio_padrao = sd(q.soja),
              mediana = median(q.soja))
  
  # Valor Médio da Produção por cultura
  
  valor.dende <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(v.dende),
              desvio_padrao = sd(v.dende),
              mediana = median(v.dende))
  
  valor.girassol <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(v.girassol),
              desvio_padrao = sd(v.girassol),
              mediana = median(v.girassol))
  
  valor.mamona <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(v.mamona),
              desvio_padrao = sd(v.mamona),
              mediana = median(v.mamona))
  
  valor.soja <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(v.soja),
              desvio_padrao = sd(v.soja),
              mediana = median(v.soja))
  
  rm(area.dende,area.girassol,area.mamona,area.soja,
     quant.dende,quant.girassol,quant.mamona,quant.soja,
     valor.dende,valor.girassol,valor.mamona,valor.soja)
  
# Análise descritiva variáveis dependentes----

  frota <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(d.bio),
              desvio_padrao = sd(d.bio),
              mediana = median(d.bio))
  
  contratos <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(total.contratos),
              desvio_padrao = sd(total.contratos),
              mediana = median(total.contratos))
  
  valor.contratos <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(valores.totais),
              desvio_padrao = sd(valores.totais),
              mediana = median(valores.totais))
  
  pibpcapta <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(pib.per.capta),
              desvio_padrao = sd(pib.per.capta),
              mediana = median(pib.per.capta))

  valoradc <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(vaba),
              desvio_padrao = sd(vaba),
              mediana = median(vaba))
  
  populacao <- pnpb %>% group_by(polos) %>% 
    summarise(média = mean(est_pop),
              desvio_padrao = sd(est_pop),
              mediana = median(est_pop))
    
  rm(pibpcapta,contratos,frota,populacao,valor.contratos,
     valoradc)
  
# Distribuição da produção total por região em toneladas----
  
    distr <- pnpb %>% group_by(regiao) %>% 
      summarise(total = sum(q.soja,q.dende,q.girassol,q.mamona))

  rm(distr)

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
  
# Teste de MANN-WHITNEY----
  
  # H0: p-valor > 0.05 médias são estatisticamente iguais
  
  # Dendê
  
  wilcox.test(prod_dende ~ polos, data=pnpb)
  
  wilcox.test(rm_dende ~ polos, data=pnpb)
  
  # Girassol

  wilcox.test(prod_girassol ~ polos, data=pnpb)
  
  wilcox.test(rm_girassol ~ polos, data=pnpb)
  
  # Mamona
  
  wilcox.test(prod_mamona ~ polos, data=pnpb)
  
  wilcox.test(rm_mamona ~ polos, data=pnpb)
  
  # Soja
    
  wilcox.test(prod_soja ~ polos, data=pnpb)
  
  wilcox.test(rm_soja ~ polos, data=pnpb)
  
# Resumo estatístico das variáveis de interesse----
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(prod_dende,prod_girassol,prod_mamona,prod_soja,
                      type='mean_sd')
  
  pnpb %>% group_by(polos) %>% 
    get_summary_stats(rm_dende,rm_girassol,rm_mamona,rm_soja,
                      type='mean_sd')
  
# Diferença entre médias: variáveis de pré-tratamento----
  
  pnpb_cov <- c('total.contratos','valores.totais','d.bio','vaba','pib.per.capta')
  
  pnpb %>% 
    group_by(polos) %>% 
    select(one_of(pnpb_cov)) %>% 
    summarise_all(funs(mean(.,na.rm=TRUE)))
  
  lapply(pnpb_cov,function(v) {
    t.test(pnpb[,v] ~ pnpb[,'polos'])
  })
  
  m_ps <- glm(polos ~ total.contratos + valores.totais + d.bio +
                vaba + pib.per.capta,family=binomial(),data=pnpb)
  summary(m_ps)
  
  setwd('E:/igorf/Documents/GitHub/dissertacao/Equações')
  
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
                        vaba + pib.per.capta,data=pnpb,method='nearest',
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

  rm(m_ps,matching,prs_df,labs,pnpb_cov)
    
# Salvando nova base----
  
  setwd('E:/igorf/Documents/GitHub/dissertacao/dataset/base')
  
  write.table(psm,file='psm.csv',sep=';',dec='.',na='',quote=TRUE, row.names=FALSE)
  
  rm(list=ls())