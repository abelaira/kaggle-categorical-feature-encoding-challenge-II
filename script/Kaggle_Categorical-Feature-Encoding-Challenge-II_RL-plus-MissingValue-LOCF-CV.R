rm(list = ls())

work_dir <- "~/cat-in-the-dat-ii"
#work_dir <- "~/Dropbox/Projects/Kaggle_Categorical-Feature-Encoding-Challenge-II"
setwd(work_dir)


if (!require(caret)) install.packages('caret')
require(caret)

if (!require(data.table)) install.packages('data.table')
require(data.table)

if (!require(DMwR2)) install.packages('DMwR2')
require(DMwR2)

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
dados_tratados[,':='(bin_0 = as.integer(factor(dados_originais$bin_0)),
                     bin_1 = as.integer(factor(dados_originais$bin_1)),
                     bin_2 = as.integer(factor(dados_originais$bin_2)),
                     bin_3 = as.integer(factor(dados_originais$bin_3)),
                     bin_4 = as.integer(factor(dados_originais$bin_4)),
                     nom_0 = as.integer(factor(dados_originais$nom_0)),
                     nom_1 = as.integer(factor(dados_originais$nom_1)),
                     nom_2 = as.integer(factor(dados_originais$nom_2)),
                     nom_3 = as.integer(factor(dados_originais$nom_3)),
                     nom_4 = as.integer(factor(dados_originais$nom_4)),
                     nom_7 = as.integer(factor(dados_originais$nom_7)),
                     nom_8 = as.integer(factor(dados_originais$nom_8)),
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


# ----- Tratando os NA's com LOCF (Last Observation Carried Forward) para os dados de TREINO
dados_treino_1 <- copy(dados_tratados)

# ----- Tratando os NA's das variaveis: nom_5, nom_6 e nom_9 de TREINO com método locf (Last Observation Carried Forward)
dados_treino_1[,':='(nom_5 = as.integer(factor(dados_originais$nom_5)),
                     nom_6 = as.integer(factor(dados_originais$nom_6)),
                     nom_9 = as.integer(factor(dados_originais$nom_9)))]

# Preenchendo os NA's nos dados de TREINO
print('Frquência (antes do preenchimento):')
head(sort(table(dados_treino_1$bin_0), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_1), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_2), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_3), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_4), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_0), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_1), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_2), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_3), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_4), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_5), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_6), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_7), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_8), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_9), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_0), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_1), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_2), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_3), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_4), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_5), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$day), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$month), decreasing = TRUE), n = 10)

setnafill(dados_treino_1, type = c('locf'), cols = c('bin_0', 'bin_1', 'bin_2', 'bin_3', 'bin_4',
                                                     'nom_0', 'nom_1', 'nom_2', 'nom_3', 'nom_4', 'nom_5', 'nom_6', 'nom_7', 'nom_8', 'nom_9',
                                                     'ord_0', 'ord_1', 'ord_2', 'ord_3', 'ord_4', 'ord_5',
                                                     'day', 'month'))

print('Frequência (após do preenchimento):')
head(sort(table(dados_treino_1$bin_0), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_1), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_2), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_3), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$bin_4), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_0), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_1), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_2), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_3), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_4), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_5), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_6), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_7), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_8), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$nom_9), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_0), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_1), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_2), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_3), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_4), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$ord_5), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$day), decreasing = TRUE), n = 10)
head(sort(table(dados_treino_1$month), decreasing = TRUE), n = 10)


dados_treino_1[,':='(bin_0 = as.factor(dados_treino_1$bin_0),
                     bin_1 = as.factor(dados_treino_1$bin_1),
                     bin_2 = as.factor(dados_treino_1$bin_2),
                     bin_3 = as.factor(dados_treino_1$bin_3),
                     bin_4 = as.factor(dados_treino_1$bin_4),
                     nom_0 = as.factor(dados_treino_1$nom_0),
                     nom_1 = as.factor(dados_treino_1$nom_1),
                     nom_2 = as.factor(dados_treino_1$nom_2),
                     nom_3 = as.factor(dados_treino_1$nom_3),
                     nom_4 = as.factor(dados_treino_1$nom_4),
                     nom_7 = as.factor(dados_treino_1$nom_7),
                     nom_8 = as.factor(dados_treino_1$nom_8))]

anyNA(dados_treino_1)
str(dados_treino_1)


# Vizualizando a distribuição das variáveis após a inserção e redução no dados de TREINO
ggplot() + 
  geom_bar(aes(dados_treino_1$nom_5)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_5')) +
  theme(axis.text.x = element_text(angle = 90))

ggplot() + 
  geom_bar(aes(dados_treino_1$nom_6)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_6')) +
  theme(axis.text.x = element_text(angle = 90))

ggplot() + 
  geom_bar(aes(dados_treino_1$nom_9)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_9')) +
  theme(axis.text.x = element_text(angle = 90))



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
cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE,
                   summaryFunction = twoClassSummary, classProbs = TRUE)
str(cv)

str(dt_treino)
levels(dt_treino$target) <- c("Zero","Um")

system.time(model_rl <- train(target ~ .,
                              data = dt_treino, method = "glm", metric = "ROC", 
                              trControl = cv, control = list(maxit = 50)))
model_rl

pred_rl <- predict(model_rl, newdata = dt_teste)
str(pred_rl)

str(dt_teste)
levels(dt_teste$target) <- c("Zero", "Um")

(mc_cv <- confusionMatrix(data = pred_rl, dt_teste$target))
saveRDS(mc, file = 'matrizConfusao-LOCF-CV.RDS')


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
dados_tratados_teste[,':='(bin_0 = as.integer(factor(dados_testes$bin_0)),
                           bin_1 = as.integer(factor(dados_testes$bin_1)),
                           bin_2 = as.integer(factor(dados_testes$bin_2)),
                           bin_3 = as.integer(factor(dados_testes$bin_3)),
                           bin_4 = as.integer(factor(dados_testes$bin_4)),
                           nom_0 = as.integer(factor(dados_testes$nom_0)),
                           nom_1 = as.integer(factor(dados_testes$nom_1)),
                           nom_2 = as.integer(factor(dados_testes$nom_2)),
                           nom_3 = as.integer(factor(dados_testes$nom_3)),
                           nom_4 = as.integer(factor(dados_testes$nom_4)),
                           nom_7 = as.integer(factor(dados_testes$nom_7)),
                           nom_8 = as.integer(factor(dados_testes$nom_8)),
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


# ----- Tratando os NA's com LOCF (Last Observation Carried Forward) para os dados de TESTE
dados_teste_1 <- copy(dados_tratados_teste)

# ----- Tratando os NA's das variaveis: nom_5, nom_6 e nom_9 de TESTE com método locf (Last Observation Carried Forward)
dados_teste_1[,':='(nom_5 = as.integer(factor(dados_testes$nom_5)),
                    nom_6 = as.integer(factor(dados_testes$nom_6)),
                    nom_9 = as.integer(factor(dados_testes$nom_9)))]

# Preenchendo os NA's
print('Frquência (antes do preenchimento):')
head(sort(table(dados_teste_1$bin_0), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_1), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_2), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_3), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_4), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_0), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_1), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_2), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_3), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_4), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_5), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_6), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_7), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_8), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_9), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_0), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_1), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_2), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_3), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_4), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_5), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$day), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$month), decreasing = TRUE), n = 10)

setnafill(dados_teste_1, type = c('locf'), cols = c('bin_0', 'bin_1', 'bin_2', 'bin_3', 'bin_4',
                                                    'nom_0', 'nom_1', 'nom_2', 'nom_3', 'nom_4', 'nom_5', 'nom_6', 'nom_7', 'nom_8', 'nom_9',
                                                    'ord_0', 'ord_1', 'ord_2', 'ord_3', 'ord_4', 'ord_5',
                                                    'day', 'month'))

print('Frequência (após do preenchimento):')
head(sort(table(dados_teste_1$bin_0), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_1), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_2), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_3), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$bin_4), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_0), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_1), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_2), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_3), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_4), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_5), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_6), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_7), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_8), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$nom_9), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_0), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_1), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_2), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_3), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_4), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$ord_5), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$day), decreasing = TRUE), n = 10)
head(sort(table(dados_teste_1$month), decreasing = TRUE), n = 10)


dados_teste_1[,':='(bin_0 = as.factor(dados_teste_1$bin_0),
                    bin_1 = as.factor(dados_teste_1$bin_1),
                    bin_2 = as.factor(dados_teste_1$bin_2),
                    bin_3 = as.factor(dados_teste_1$bin_3),
                    bin_4 = as.factor(dados_teste_1$bin_4),
                    nom_0 = as.factor(dados_teste_1$nom_0),
                    nom_1 = as.factor(dados_teste_1$nom_1),
                    nom_2 = as.factor(dados_teste_1$nom_2),
                    nom_3 = as.factor(dados_teste_1$nom_3),
                    nom_4 = as.factor(dados_teste_1$nom_4),
                    nom_7 = as.factor(dados_teste_1$nom_7),
                    nom_8 = as.factor(dados_teste_1$nom_8))]

anyNA(dados_teste_1)
str(dados_teste_1)


# Vizualizando a distribuição das variáveis após a inserção nos dados de TESTE
ggplot() + 
  geom_bar(aes(dados_teste_1$nom_5)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_5')) +
  theme(axis.text.x = element_text(angle = 90))

ggplot() + 
  geom_bar(aes(dados_teste_1$nom_6)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_6')) +
  theme(axis.text.x = element_text(angle = 90))

ggplot() + 
  geom_bar(aes(dados_teste_1$nom_9)) +
  labs(y = 'Quantidade de Ocorrência', title = paste('Distribuição para NOM_9')) +
  theme(axis.text.x = element_text(angle = 90))



# ----- Obtendo a predição do modelo
pred_rl_prob <- predict(model_rl, newdata = dados_teste_1, type = 'prob')

head(pred_rl_prob, n = 20)
nrow(pred_rl_prob)

previsao_final <- data.table()
previsao_final[, ':='(id = as.integer(600000:999999),
                      target = pred_rl_prob$Um)]

str(previsao_final)
head(previsao_final, n = 20)
fwrite(previsao_final, file = './prevFinal-LOCF-CV.csv')

