rm(list = ls())


#work_dir <- "~/cat-in-the-dat-ii"
work_dir <- "~/Dropbox/Projects/Kaggle_Categorical-Feature-Encoding-Challenge-II"
setwd(work_dir)


if (!require(caret)) install.packages('caret')
require(caret)

if (!require(data.table)) install.packages('data.table')
require(data.table)

if (!require(mice)) install.packages('mice')
require(mice)

if (!require(ggplot2)) install.packages('ggplot2')
require(ggplot2)


# ----- Carregando os dados de originais ------------------------------------
dados_originais <- fread('./train.csv', na.strings = '')
str(dados_originais)
nrow(dados_originais)
head(dados_originais)


# ----- Investigando as variáveis strings -----------------------------------
unique(dados_originais[,nom_0]) #Pode ser convertido a fator, poucos níveis = 3
unique(dados_originais[,nom_1]) #Pode ser convertido a fator, poucos níveis = 6
unique(dados_originais[,nom_2]) #Pode ser convertido a fator, poucos níveis = 6
unique(dados_originais[,nom_3]) #Pode ser convertido a fator, poucos níveis = 6
unique(dados_originais[,nom_4]) #Pode ser convertido a fator, poucos níveis = 4

length(unique(dados_originais[,nom_5])) #Muitos niveis = 1221
length(unique(dados_originais[,nom_5]))/nrow(dados_originais) * 100 #Os níveis representam 0,2%
dados_originais[nom_5 == '82dea7f55',.N] #Gerou 268 linhas para um nível específico
ggplot(dados_originais, aes(x = nom_5)) + 
  geom_bar() +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_5')) +
  theme(axis.text.x = element_text(angle = 90))

length(unique(dados_originais[,nom_6])) #Muitos niveis = 1520
length(unique(dados_originais[,nom_6]))/nrow(dados_originais) * 100 #Os níveis representam 0,25%
dados_originais[nom_6 == 'fa886b105',.N] #Gerou 268 linhas para um nível específico
ggplot(dados_originais, aes(x = nom_6)) + 
  geom_bar() +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_6')) +
  theme(axis.text.x = element_text(angle = 90))

length(unique(dados_originais[,nom_7])) #Muitos niveis = 223

length(unique(dados_originais[,nom_8])) #Muitos niveis = 223

length(unique(dados_originais[,nom_9])) #Muitos niveis = 2219
ggplot(dados_originais, aes(x = nom_9)) + 
  geom_bar() +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_9')) +
  theme(axis.text.x = element_text(angle = 90))

length(unique(dados_originais[,ord_0]))
length(unique(dados_originais[,ord_1]))
length(unique(dados_originais[,ord_2]))
length(unique(dados_originais[,ord_3]))
length(unique(dados_originais[,ord_4]))
length(unique(dados_originais[,ord_5]))


# ----- Verificando NA's ----------------------------------------------------
vet_na <- apply(dados_originais, c(1,2), is.na)
vet_na <- vet_na[,2:ncol(vet_na)]
str(vet_na)
head(vet_na)
class(vet_na)

# Verificando total de NA's por linha
na_linha <- apply(vet_na, c(1), sum)
sum(na_linha) #dados possuem pelo menos uma linha com NA.
length(na_linha) #total de linhas
sum(na_linha)/length(na_linha)*100 #percentual de NA's

# Verificando total de NA's por coluna
na_coluna <- apply(vet_na, c(2), sum)
na_coluna #Todas as colunas possuem valores de NA em alguma linha com exceção do target


# ----- Tratando as variáveis depois de carregada -----------------------------
# Retirados as variáveis nom_5, nom_6 e nom_9 porque possuem muitos niveis e podem gerar problemas no 
# modelo porque sempre pode existir um nível novo que não estejam nos dados.

# Tratando e retirando as variaveis: nom_5, nom_6 e nom_9
dados_tratados <- data.table()
dados_tratados[,':='(bin_0 = factor(dados_originais$bin_0),
                     bin_1 = factor(dados_originais$bin_1),
                     bin_2 = factor(dados_originais$bin_2),
                     bin_3 = factor(dados_originais$bin_3),
                     bin_4 = factor(dados_originais$bin_4),
                     nom_0 = factor(dados_originais$nom_0),
                     nom_1 = factor(dados_originais$nom_1),
                     nom_2 = factor(dados_originais$nom_2),
                     nom_3 = factor(dados_originais$nom_3),
                     nom_4 = factor(dados_originais$nom_4),
                     nom_7 = factor(dados_originais$nom_7),
                     nom_8 = factor(dados_originais$nom_8),
                     ord_0 = as.integer(factor(dados_originais$ord_0, ordered = TRUE)),
                     ord_1 = as.integer(factor(dados_originais$ord_1,
                                               levels = c("Novice","Contributor","Expert","Master","Grandmaster"),        
                                               ordered = TRUE)),
                     ord_2 = as.integer(factor(dados_originais$ord_2, 
                                               levels = c("Freezing","Cold","Warm","Hot","Boiling Hot","Lava Hot"),
                                               ordered = TRUE)),
                     ord_3 = as.integer(factor(dados_originais$ord_3, ordered = TRUE)),
                     ord_4 = as.integer(factor(dados_originais$ord_4, ordered = TRUE)),
                     ord_5 = as.integer(factor(dados_originais$ord_5, ordered = TRUE)),
                     day = dados_originais$day,
                     month = dados_originais$month,
                     target = factor(dados_originais$target))]

head(dados_tratados)
str(dados_tratados)
anyNA(dados_tratados)


# ----- Tratando os NA's com PMM para os dados de treino
arquivo <- 'dt-Treino_MissingValue_PMM.csv'
if (file.exists(paste('./',arquivo, sep = ''))) {
  dados_treino_1 <- fread(paste('./',arquivo, sep = ''))
  
  head(dados_treino_1, n = 10)
  anyNA(dados_treino_1)
  
  dados_treino_1[,':='(bin_0 = factor(dados_treino_1$bin_0),
                       bin_1 = factor(dados_treino_1$bin_1),
                       bin_2 = factor(dados_treino_1$bin_2),
                       bin_3 = factor(dados_treino_1$bin_3),
                       bin_4 = factor(dados_treino_1$bin_4),
                       nom_0 = factor(dados_treino_1$nom_0),
                       nom_1 = factor(dados_treino_1$nom_1),
                       nom_2 = factor(dados_treino_1$nom_2),
                       nom_3 = factor(dados_treino_1$nom_3),
                       nom_4 = factor(dados_treino_1$nom_4),
                       nom_7 = factor(dados_treino_1$nom_7),
                       nom_8 = factor(dados_treino_1$nom_8),
                       ord_0 = dados_treino_1$ord_0,
                       ord_1 = dados_treino_1$ord_1,
                       ord_2 = dados_treino_1$ord_2, 
                       ord_3 = dados_treino_1$ord_3,
                       ord_4 = dados_treino_1$ord_4,
                       ord_5 = dados_treino_1$ord_5,
                       day = dados_treino_1$day,
                       month = dados_treino_1$month)]
  
  dados_treino_1[, target := as.factor(dados_tratados$target)]
  str(dados_treino_1)
  
} else {
  
  dados_treino_1 <- copy(dados_tratados)
  
    (i <- Sys.time())
    system.time(miceModel_treino <- mice(dados_treino_1[,!names(dados_treino_1) %in% "target", with=F], 
                                         method = 'pmm'))
    dados_treino_1 <- complete(miceModel_treino)
    (f <- Sys.time())
    
    # OBS: O uso do mice com metodo PMM para o preenchimento dos NA's adiciona uma camada preditiva no tratatmento de dados.
  
    print(paste('Tempo de processamento: ',f - i))
    head(dados_treino_1, n = 10)
    anyNA(dados_treino_1)
    str(dados_treino_1)
    time <- c(paste('Inicio:',i), paste('Fim:', f), paste('Duracao: ',f - i))
    write.csv(as.data.frame(time), file = paste("./TIME_",arquivo, sep = ''))
    fwrite(x = dados_treino_1, file = paste('./',arquivo, sep = ''))
    
    dados_treino_1[, target := as.factor(dados_tratados$target)]
    str(dados_treino_1)
}


# ----- Tratando os NA's das variaveis: nom_5, nom_6 e nom_9 de TREINO com método locf (Last Observation Carried Forward)
dados_treino_1[,':='(nom_5 = as.integer(factor(dados_originais$nom_5)),
                     nom_6 = as.integer(factor(dados_originais$nom_6)),
                     nom_9 = as.integer(factor(dados_originais$nom_9)))]

# Avaliando o preenchimento pela Moda ----------------------------------------------------
# Esse tipo de preenchimento distorce a distribuição de frequência dos dados pelas classes
if (FALSE) {
  moda <- function(vetor) {
    z <- table(vetor)
    return(names(z)[z == max(z)][1])
  }
  
  head(sort(table(dados_treino_1$nom_5), decreasing = TRUE))
  dados_treino_1[which(vet_na[,11]),22] <- as.integer(moda(dados_treino_1$nom_5))
  head(sort(table(dados_treino_1$nom_6), decreasing = TRUE))
  dados_treino_1[which(vet_na[,11]),23] <- as.integer(moda(dados_treino_1$nom_6))
  head(sort(table(dados_treino_1$nom_9), decreasing = TRUE))
  dados_treino_1[which(vet_na[,11]),24] <- as.integer(moda(dados_treino_1$nom_9))
  
  # Vizualizando a distribuição das variáveis após a inserção
  ggplot() + 
    geom_bar(aes(dados_treino_1$nom_5)) +
    labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_5'))
  
  ggplot() + 
    geom_bar(aes(dados_treino_1$nom_6)) +
    labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_6'))
  
  ggplot() + 
    geom_bar(aes(dados_treino_1$nom_9)) +
    labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_9'))
  
  dados_treino_1 <- copy(dados_tratados)
  dados_treino_1[,':='(nom_5 = as.integer(factor(dados_originais$nom_5)),
                       nom_6 = as.integer(factor(dados_originais$nom_6)),
                       nom_9 = as.integer(factor(dados_originais$nom_9)))]
  str(dados_treino_1)
}

# Preenchendo os NA's nos dados de TREINO
print('Frequência (antes do preenchimento):')
head(sort(table(dados_treino_1$nom_5), decreasing = TRUE))
head(sort(table(dados_treino_1$nom_6), decreasing = TRUE))
head(sort(table(dados_treino_1$nom_9), decreasing = TRUE))

setnafill(dados_treino_1, type = c('locf'), cols = c('nom_5','nom_6','nom_9'))

print('Frequência (após do preenchimento):')
head(sort(table(dados_treino_1$nom_5), decreasing = TRUE))
head(sort(table(dados_treino_1$nom_6), decreasing = TRUE))
head(sort(table(dados_treino_1$nom_9), decreasing = TRUE))

sum(is.na(dados_treino_1$nom_5))
sum(is.na(dados_treino_1$nom_6))
sum(is.na(dados_treino_1$nom_9))

anyNA(dados_treino_1)
str(dados_treino_1)

# Vizualizando a distribuição das variáveis após a inserção no dados de TREINO
ggplot() + 
  geom_bar(aes(dados_treino_1$nom_5)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_5'))

ggplot() + 
  geom_bar(aes(dados_treino_1$nom_6)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_6'))

ggplot() + 
  geom_bar(aes(dados_treino_1$nom_9)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_9'))



# ----- Treinando e ajustando o modelo -------------------------
dt <- dados_treino_1
str(dt)
anyNA(dt)

rm(list = ls(pattern = 'dados'))
rm(list = ls(pattern = 'na'))

# Criando os data set's de treinamento e teste
set.seed(215)
i_treino <- createDataPartition(dt$target, p = 0.75, list = FALSE)
dt_treino <- dt[i_treino,]
dt_teste <- dt[-i_treino,]


# ----- Regressão Logística
system.time(fit_rl <- glm(target ~ ., data = dt_treino, family = binomial()))
summary(fit_rl)
ls(fit_rl)
fit_rl$aic

pred_rl <- predict(fit_rl, newdata = dt_teste, type = 'response')
pred_rl

cutoff <- 0.5
previsao <- ifelse(pred_rl >= cutoff, 1, 0)
previsao <- factor(previsao)
str(previsao)

(mc <- confusionMatrix(data = previsao, reference = dt_teste$target))
saveRDS(mc, file = 'matrizConfusao-PMM.RDS')

# ----- Carregando os dados de teste ------------------------------------------
dados_testes <- fread('./test.csv', na.strings = '')
str(dados_testes)
nrow(dados_testes)
head(dados_testes)


# ----- Verificando NA's ----------------------------------------------------
vet_na <- apply(dados_testes, c(1,2), is.na)
vet_na <- vet_na[,2:ncol(vet_na)]
str(vet_na)
head(vet_na)
class(vet_na)


# ----- Tratando as variáveis depois de carregada -----------------------------
# Tratando e retirando as variaveis: nom_5, nom_6 e nom_9
dados_tratados_teste <- data.table()
dados_tratados_teste[,':='(bin_0 = factor(dados_testes$bin_0),
                           bin_1 = factor(dados_testes$bin_1),
                           bin_2 = factor(dados_testes$bin_2),
                           bin_3 = factor(dados_testes$bin_3),
                           bin_4 = factor(dados_testes$bin_4),
                           nom_0 = factor(dados_testes$nom_0),
                           nom_1 = factor(dados_testes$nom_1),
                           nom_2 = factor(dados_testes$nom_2),
                           nom_3 = factor(dados_testes$nom_3),
                           nom_4 = factor(dados_testes$nom_4),
                           nom_7 = factor(dados_testes$nom_7),
                           nom_8 = factor(dados_testes$nom_8),
                           ord_0 = as.integer(factor(dados_testes$ord_0, ordered = TRUE)),
                           ord_1 = as.integer(factor(dados_testes$ord_1,
                                                     levels = c("Novice","Contributor","Expert","Master","Grandmaster"),        
                                                     ordered = TRUE)),
                           ord_2 = as.integer(factor(dados_testes$ord_2, 
                                                     levels = c("Freezing","Cold","Warm","Hot","Boiling Hot","Lava Hot"),
                                                     ordered = TRUE)),
                           ord_3 = as.integer(factor(dados_testes$ord_3, ordered = TRUE)),
                           ord_4 = as.integer(factor(dados_testes$ord_4, ordered = TRUE)),
                           ord_5 = as.integer(factor(dados_testes$ord_5, ordered = TRUE)),
                           day = dados_testes$day,
                           month = dados_testes$month)]

head(dados_tratados_teste)
str(dados_tratados_teste)
anyNA(dados_tratados_teste)


# ----- Tratando os NA's com PMM para os dados de teste
arquivo <- 'dt-Teste_MissingValue_PMM.csv'
if (file.exists(paste('./',arquivo, sep = ''))) {
  dados_teste_1 <- fread(paste('./',arquivo, sep = ''))
  
  head(dados_teste_1, n = 10)
  anyNA(dados_teste_1)
  
  dados_teste_1[,':='(bin_0 = factor(dados_teste_1$bin_0),
                      bin_1 = factor(dados_teste_1$bin_1),
                      bin_2 = factor(dados_teste_1$bin_2),
                      bin_3 = factor(dados_teste_1$bin_3),
                      bin_4 = factor(dados_teste_1$bin_4),
                      nom_0 = factor(dados_teste_1$nom_0),
                      nom_1 = factor(dados_teste_1$nom_1),
                      nom_2 = factor(dados_teste_1$nom_2),
                      nom_3 = factor(dados_teste_1$nom_3),
                      nom_4 = factor(dados_teste_1$nom_4),
                      nom_7 = factor(dados_teste_1$nom_7),
                      nom_8 = factor(dados_teste_1$nom_8),
                      ord_0 = dados_teste_1$ord_0,
                      ord_1 = dados_teste_1$ord_1,
                      ord_2 = dados_teste_1$ord_2, 
                      ord_3 = dados_teste_1$ord_3,
                      ord_4 = dados_teste_1$ord_4,
                      ord_5 = dados_teste_1$ord_5,
                      day = dados_teste_1$day,
                      month = dados_teste_1$month)]
  str(dados_teste_1)
  
} else {
  
  dados_teste_1 <- copy(dados_tratados_teste)
  
  (i <- Sys.time())
  system.time(miceModel_teste <- mice(dados_teste_1[,!names(dados_teste_1) %in% "target", with=F],
                                      method = 'pmm'))
  dados_teste_1 <- complete(miceModel_teste)
  (f <- Sys.time())
  
  # OBS: O uso do mice com metodo PMM para o preenchimento dos NA's adiciona uma camada preditiva no tratatmento de dados.
  
  print(paste('Tempo de processamento: ',f - i))
  head(dados_teste_1, n = 10)
  str(dados_teste_1)
  anyNA(dados_teste_1)
  time <- c(paste('Inicio:',i), paste('Fim:', f), paste('Duracao: ',f - i))
  write.csv(as.data.frame(time), file = paste("./TIME_",arquivo, sep = ''))
  fwrite(x = dados_teste_1, file = paste('./',arquivo, sep = ''))
}


# ----- Tratando os NA's das variaveis: nom_5, nom_6 e nom_9 de TESTE com método locf (Last Observation Carried Forward)
dados_teste_1[,':='(nom_5 = as.integer(factor(dados_testes$nom_5)),
                    nom_6 = as.integer(factor(dados_testes$nom_6)),
                    nom_9 = as.integer(factor(dados_testes$nom_9)))]

# Preenchendo os NA's
print('Frequência (antes do preenchimento):')
head(sort(table(dados_teste_1$nom_5), decreasing = TRUE))
head(sort(table(dados_teste_1$nom_6), decreasing = TRUE))
head(sort(table(dados_teste_1$nom_9), decreasing = TRUE))

setnafill(dados_teste_1, type = c('locf'), cols = c('nom_5','nom_6','nom_9'))

print('Frequência (após do preenchimento):')
head(sort(table(dados_teste_1$nom_5), decreasing = TRUE))
head(sort(table(dados_teste_1$nom_6), decreasing = TRUE))
head(sort(table(dados_teste_1$nom_9), decreasing = TRUE))

sum(is.na(dados_teste_1$nom_5))
sum(is.na(dados_teste_1$nom_6))
sum(is.na(dados_teste_1$nom_9))

anyNA(dados_teste_1)
str(dados_teste_1)

# Vizualizando a distribuição das variáveis após a inserção nos dados de TESTE
ggplot() + 
  geom_bar(aes(dados_teste_1$nom_5)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_5'))

ggplot() + 
  geom_bar(aes(dados_teste_1$nom_6)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_6'))

ggplot() + 
  geom_bar(aes(dados_teste_1$nom_9)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_9'))



# ----- Obtendo a predição do modelo
pred_rl_teste <- predict(fit_rl, newdata = dados_teste_1, type = 'response')
pred_rl_teste


previsao_final <- data.table()
previsao_final[, ':='(id = as.integer(600000:999999),
                      target = as.numeric(pred_rl_teste))]

str(previsao_final)
head(previsao_final, n = 10)
fwrite(previsao_final, file = './prevFinal-PMM.csv')

