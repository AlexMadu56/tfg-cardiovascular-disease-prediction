#Cargamos las librerías
library(pROC)
library(dplyr)
library(knitr)
library(kableExtra)
library(scales)
library(haven)
library(missForest)
library(ROSE)
library(UBL)
library(e1071)
library(caret)
library(randomForest)
library(ranger)
library(data.table)
#Cargamos los datos y los visualizamos
datos <- read_xpt("~/Desktop/LLCP2023.XPT ")
head( datos )
str( datos )
colnames( datos )
summary( datos )
colSums( is.na (datos) )
dim( datos )
#Elegimos y renombramos las variables utilizadas a lo largo del estudio
variables_finales <- c("DIABETE4", "BPHIGH6", "_RFCHOL3", "_BMI5CAT",
                       "
_SMOKER3","_MICHD","_TOTINDA", "SEXVAR", "_AGE_G","CVDSTRK3","_RFHLTH"
                       ,"_PHYS14D","_MENT14D","_EDUCAG","_INCOMG1","DRNKANY6")
categorias <- c( "DIABETES", "HIPERTENSION", "COLESTEROL", "IMC","FUMA
R", "INFARTO","ACTIVIDAD_FISICA","SEXO", "EDAD","DERRAME","SALUD_GENE
RAL","SALUD_F","SALUD_M","EDUCACIÓN","SALARIO","ALCOHOL")
datos_var<- datos[, variables_finales]
colSums( is.na (datos_var) )
dim(datos_var)
names(datos_var) <- categorias
summary(datos_var)
factores <- as.data.frame(lapply(datos_var, as.factor))
names(factores) <- categorias
summary(factores)
dim(factores)
#Creamos la variable objetivo
factores$DERRAME[factores$DERRAME %in% c(7, 9)] <- NA
factores$ECV <- NA # Inicializamos la variable
factores$ECV[factores$DERRAME == 1 | factores$INFARTO == 1] <- 0
factores$ECV[
  is.na(factores$ECV) &
    ((factores$DERRAME == 2 & factores$INFARTO != 1) |

     (factores$INFARTO == 2 & factores$DERRAME != 1))
] <- 1
table(factores$ECV, useNA = "always")
factores <- factores[!is.na(factores$ECV), ]
table(factores$ECV, useNA = "always")
factores <- factores[, !(names(factores) %in% c("INFARTO", "DERRAME"))
]
factores <- as.data.frame(lapply(factores, as.factor))
# Reemplazar las categorías no deseadas por NA y eliminar los niveles  no utilizados

factores$HIPERTENSION[factores$HIPERTENSION == 7 | factores$HIPERTENSION == 9] <- NA
factores$HIPERTENSION <- droplevels(factores$HIPERTENSION)
table(factores$HIPERTENSION)
factores$COLESTEROL[ factores$COLESTEROL == 9] <- NA
factores$COLESTEROL <- droplevels(factores$COLESTEROL)
table(factores$COLESTEROL)
factores$FUMAR[factores$FUMAR == 7 | factores$FUMAR == 9] <- NA
factores$FUMAR <- droplevels(factores$FUMAR)
factores$ACTIVIDAD_FISICA[ factores$ACTIVIDAD_FISICA == 9] <- NA
factores$ACTIVIDAD_FISICA <- droplevels(factores$ACTIVIDAD_FISICA)
factores$SALUD_GENERAL[ factores$SALUD_GENERAL == 9] <- NA
factores$SALUD_GENERAL <- droplevels(factores$SALUD_GENERAL)
summary(factores$SALUD_GENERAL)
factores$SALUD_F[ factores$SALUD_F == 9] <- NA
factores$SALUD_F <- droplevels(factores$SALUD_F)
summary(factores$SALUD_F)
factores$SALUD_M[ factores$SALUD_M == 9] <- NA
factores$SALUD_M <- droplevels(factores$SALUD_M)
summary(factores$SALUD_M)
factores$EDUCACIÓN[ factores$EDUCACIÓN == 9] <- NA
factores$EDUCACIÓN <- droplevels(factores$EDUCACIÓN)
summary(factores$EDUCACIÓN)
factores$SALARIO[ factores$SALARIO == 9] <- NA
factores$SALARIO <- droplevels(factores$SALARIO)
summary(factores$SALARIO)
factores$ALCOHOL[factores$ALCOHOL == 7 | factores$ALCOHOL == 9] <- NA
factores$ALCOHOL <- droplevels(factores$ALCOHOL)
factores$DIABETES[factores$DIABETES == 7 | factores$DIABETES == 9] <-
  NA
factores$DIABETES <- droplevels(factores$DIABETES)
summary(factores)
#Renombramos categorías
# DIABETES
factores$DIABETES <- factor(factores$DIABETES,
                            levels = c(1, 2, 3, 4),
                            labels = c("Sí", "Sí, solo durante el emba
razo", "No", "PREDIABETES"))
# HIPERTENSION
factores$HIPERTENSION <- factor(factores$HIPERTENSION,
                                levels = c(1, 2, 3, 4),
                         
                                 
                                labels = c("Sí", "Sí, solo durante el
embarazo", "No", "Pre-hipertensión"))
# COLESTEROL
factores$COLESTEROL <- factor(factores$COLESTEROL,
                              levels = c(1, 2),
                              labels = c("No", "Sí"))
# IMC (clasificación estándar)
factores$IMC <- factor(factores$IMC,
                       levels = c(1, 2, 3, 4),
                       labels = c("Bajo peso", "Peso normal", "Sobrepe
so", "Obesidad"))
# FUMAR
factores$FUMAR <- factor(factores$FUMAR,
                         levels = c(1, 2, 3, 4),
                         labels = c("Fumador diario", "Fumador ocasion
al", "Exfumador", "Nunca ha fumado"))
# ACTIVIDAD FISICA
factores$ACTIVIDAD_FISICA <- factor(factores$ACTIVIDAD_FISICA,
                                    levels = c(1, 2),
                                    labels = c("Sí", "No"))
# SEXO
factores$SEXO <- factor(factores$SEXO,
                        levels = c(1, 2),
                        labels = c("Hombre", "Mujer"))
# SALUD_GENERAL
factores$SALUD_GENERAL <- factor(factores$SALUD_GENERAL,
                                 levels = c(1, 2),
                                 labels = c( "Buena", "Mala"))
# EDUCACIÓN
factores$EDUCACIÓN <- factor(factores$EDUCACIÓN,
                             levels = c(1, 2, 3, 4),
                             labels = c("No terminó secundaria",
                                        "Graduado de secundaria",
                                        "Asistió a universidad o escu
ela técnica",
                                        "Graduado universitario o téc
nico"))
# SALARIO (rango de ingresos)
factores$SALARIO <- factor(factores$SALARIO,
                           levels = c(1, 2, 3, 4, 5, 6,7),
                           labels = c("< $15,000",
                                      "$15,000 a < $25,000",
                                      "$25,000 a < $35,000",
                                      "$35,000 a < $50,000",
                                      "$50,000 a < $100,000",
                               
                                      
                                      "$100,000 a < $200,000",
                                      "200,000 o más"))
# ALCOHOL
factores$ALCOHOL <- factor(factores$ALCOHOL,
                           levels = c(1, 2),
                           labels = c("Sí", "No"))
# ECV (enfermedad cardiovascular)
factores$ECV <- factor(factores$ECV,
                       levels = c(0, 1),
                       labels = c("Sí", "No"))
factores$EDAD <- factor(factores$EDAD,
                        levels = c(1, 2, 3, 4, 5, 6),
                        labels = c("18-24",
                                   "25-34",
                                   "35-44",
                                   "45-54",
                                   "55-64",
                                   "65+"))
factores$SALUD_F <- factor(factores$SALUD_F,
                           levels = c(1, 2, 3),
                           labels = c("0 días con salud física mala",
                                      "1-13 días con salud física mala
"
                                      ,
                                      "14 o más días con salud física
mala"))
factores$SALUD_M <- factor(factores$SALUD_M,
                           levels = c(1, 2, 3),
                           labels = c("0 días con salud mental mala",
                                      "1-13 días con salud mental mala
"
                                      ,
                                      "14 o más días con salud mental
mala"))
#Gráficos de barras
# Ruta donde se guardarán las imágenes
path_escritorio <- "/Users/alejandromaduenogalvez/Desktop"
# Lista de variables
variables <- colnames(factores)
# Loop para generar un gráfico por variable
for (var in variables) {
  counts <- table(factores[[var]])
  # Guardar gráfico
  png(filename = file.path(path_escritorio, paste0(var, ".png")),
      width = 1000, height = 800)
  par(mar = c(10, 4, 4, 2) + 0.1)
  bar_midpoints <- barplot(
    counts,
    main = var,
    col = "lightblue",
    border = "black",
    names.arg = rep(""
                    , length(counts)),
    ylab = ""
    ,
    yaxt = "n",
    cex.main = 2
  )
  # Agregar etiquetas rotadas para que se puedan leer
  text(x = bar_midpoints,
       y = par("usr")[3] - 0.02 * max(counts),
       labels = names(counts),
       srt = 45,
       adj = 1,
       xpd = TRUE,
       cex = 1.2)
  dev.off()
}
#Separamos en train y test
set.seed(4983) 
train_index <- sample(1:nrow(factores), 0.8 * nrow(factores))
train_data <- factores[train_index, ]
table(train_data$INFARTO)
summary(train_data)
str(train_data)
test_data <- factores[-train_index, ]
table(test_data$INFARTO)
summary(test_data)
str(test_data)
factores_imputado <-missForest(train_data,
                               maxiter = 5,
                               ntree = 100,
                               verbose = TRUE)
factores_imputado2 <- missForest(test_data,
                                 maxiter = 5,
                                 ntree = 100,
                                 verbose = TRUE)
factores_imputado<-factores_imputado$ximp
summary(factores_imputado)
factores_imputado2<-factores_imputado2$ximp
summary(factores_imputado2)
write_sav(factores_imputado, "train.sav")
write_sav(factores_imputado2, "test.sav")
train <- read_sav("Desktop/train.sav")
test <- read_sav("Desktop/test.sav")
 
#Undersampling
set.seed(4983)
under<- ovun.sample(ECV ~ ., data = (train), method = "under", N = sum
                    (train$ECV == 0) * 2)$data
#Oversampling
set.seed(4983)
N_target <- sum(train$ECV == 1) * 2
over <- ovun.sample(ECV ~ ., data = train, method = "over", N = N_target, seed = 4983)$data
#SMOTE
set.seed(4983)
smote <- SmoteClassif(ECV ~ ., dat = train, C.perc = list("0" = 7, "1"
                                                          = 1), k = 5, dist = "Overlap")
#Modelo glm con la muestra normal
model <- glm( ECV~ ., data = train, family = binomial)
summary(model)
set.seed(4983)
predicciones_prob_glm_normal <- predict(model, newdata = test, type =
                                          "response")
#Modelo glm con undersampling
model <- glm( ECV~ ., data = under, family = binomial)
set.seed(4983)
predicciones_prob_glm_under <- predict(model, newdata = test, type = "
response")
#Modelo glm con oversampling
model <- glm( ECV~ ., data = over, family = binomial)
set.seed(4983)
pred_glm_over <- predict(model, newdata = test, type = "response")
#Modelo glm con SMOTE
model <- glm( ECV~ ., data = smote, family = binomial)
set.seed(4983)
pred_glm_smote <- predict(model, newdata = test, type = "response")
#Modelo NB con muestra normal
model_nb <- naiveBayes(ECV ~ ., data = train)
set.seed(4983)
pred_nb_normal <- predict(model_nb, newdata = test, type = "raw")[,2]
#Gráfico de importancia de las variables de NB
set.seed(4983)
modelo_nb <- train(
  ECV ~ .,
  data = train, #
  method = "naive_bayes",
  trControl = trainControl(method = "cv", number = 5)
)
# Extraer importancia
imp_nb <- varImp(modelo_nb)$importance

imp_nb
# Crear data frame con la columna X0
imp_df <- data.frame(
  Variable = nombres_corregidos,
  Importance = imp_nb$X0
)
ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance))
+
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de Variables - Naive Bayes",
       x = "Variable",
       y = "Importancia (ROC curve)") +
  theme_minimal()
#Modelo NB con undersampling
model_nb_u <- naiveBayes(ECV ~ ., data = under)
set.seed(4983)
pred_nb_under <- predict(model_nb_u, newdata = test, type = "raw")[,2]
#Modelo NB con oversampling
model_nb_o <- naiveBayes(ECV ~ ., data = over)
set.seed(4983) # Para reproducibilidad
pred_nb_over <- predict(model_nb_o, newdata = test, type = "raw")[,2]
#Modelo NB con smote
model_nb_s <- naiveBayes(ECV ~ ., data = smote )
set.seed(4983)
pred_nb_smote <- predict(model_nb_s, newdata = test, type = "raw")[,2
] # Probabilidad de ECV = 1
#AJUSTE DE HIPERPARÁMETROS
# Establecer semilla para reproducibilidad
# Fijar tamaño de muestra
sample_size <- 30000
# Crear muestras de tamaño 30,000
set.seed(4983)
train_sample <- train[sample(nrow(train), size = sample_size), ]
set.seed(4983)
under_sample <- under[sample(nrow(under), size = sample_size), ]
set.seed(4983)
over_sample <- over[sample(nrow(over), size = sample_size), ]
set.seed(4983)
smote_sample <- smote[sample(nrow(smote), size = sample_size), ]
# Comprobación de tamaños y codificación de variable respuesta
cat("train_sample:", dim(train_sample), "\n")
train_sample$ECV <- factor(train_sample$ECV, levels = c(0, 1), labels
                           = c("Si", "No"))
cat("under_sample:", dim(under_sample), "\n")
under_sample$ECV <- factor(under_sample$ECV, levels = c(0, 1), labels
                           = c("Si", "No"))

cat("over_sample:", dim(over_sample), "\n")
over_sample$ECV <- factor(over_sample$ECV, levels = c(0, 1), labels =
                            c("Si", "No"))
cat("smote_sample:", dim(smote_sample), "\n")
smote_sample$ECV <- factor(smote_sample$ECV, levels = c(0, 1), labels
                           = c("Si", "No"))
#Para Random Forest
ctrl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)
grid_rf <- expand.grid(
  mtry = c(1:5),
  splitrule = "gini",
  min.node.size = c(2:10)
)
set.seed(4983)
rf_ranger_n <- train(
  ECV ~ .,
  data = train_sample,
  method = "ranger",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = grid_rf,
  num.trees = 100,
  importance = "impurity"
)
set.seed(4983)
rf_ranger_u <- train(
  ECV ~ .,
  data = under_sample,
  method = "ranger",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = grid_rf,
  num.trees = 100,
  importance = "impurity"
)
set.seed(4983)
rf_ranger_o <- train(
  ECV ~ .,
  data = over_sample,
  method = "ranger",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = grid_rf,
  num.trees = 100,
  importance = "impurity"
)
set.seed(4983)
rf_ranger_s <- train(
  ECV ~ .,
  data = smote_sample,
  method = "ranger",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = grid_rf,
  num.trees = 100,
  importance = "impurity"
)
rf_ranger_n$bestTune
rf_ranger_u$bestTune
rf_ranger_o$bestTune
rf_ranger_s$bestTune
#Para XGBoost
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = TRUE)
set.seed(4983)
xgb_model <- train(ECV ~ ., data = train_sample,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneLength = 5)
print(xgb_model$bestTune)
set.seed(4983)
xgb_model_u <- train(ECV ~ ., data = under_sample,
                     method = "xgbTree",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneLength = 5)
print(xgb_model_u$bestTune)
set.seed(4983)
xgb_model_o <- train(ECV ~ ., data = over_sample,
                     method = "xgbTree",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneLength = 5)
print(xgb_model_o$bestTune)
set.seed(4983)
xgb_model_s <- train(ECV ~ ., data = smote_sample,
                     method = "xgbTree",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneLength = 5)
print(xgb_model_s$bestTune)
#Modelo RF muestra normal
rf_model_n <- randomForest(ECV ~ ., data = train, ntree = 200, mtry =
                             2,nodesize = 4, importance=TRUE)
set.seed(4983)
pred_rf_normal <- predict(rf_model_n, newdata = test, type = "prob")[,
                                                                     2]
#Gráfico de importancia de variables
imp <- varImp(rf_model_n)$importance
imp$Variable <- rownames(imp)
# Extraer importancia (MeanDecreaseGini)
imp_rf <- importance(rf_model_n, type = 2) # type = 2 es MeanDecrease
Gini
# Convertir a data.frame y agregar nombres de variables
imp_df <- data.frame(Variable = rownames(imp_rf), Gini = imp_rf[,1])
dt_imp_rf <- data.table(imp_df)
dt_imp_rf[, VariableBase := sub("[0-9]+$",
                                "", Variable)] # agrupa variable sin sufijo
# Sumar importancia por VariableBase
imp_agrupada_rf <- dt_imp_rf[, .(Gini = sum(Gini)), by = VariableBase]
imp_agrupada_rf <- imp_agrupada_rf[order(-Gini)]
# Gráfico
ggplot(imp_agrupada_rf, aes(x = reorder(VariableBase, Gini), y = Gini)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = ""
       ,
       x = "Variable",
       y = "Importancia (MeanDecreaseGini)") +
  theme_minimal()
#Modelo Under
rf_model_u <- randomForest(ECV ~ ., data = under, ntree = 150, mtry =
                             3,nodesize = 9, importance=TRUE)
set.seed(4983)
pred_rf_under <- predict(rf_model_u , newdata = test, type = "prob")[,
                                                                     2]
#Modelo over

rf_model_o <- randomForest(ECV ~ ., data = over, ntree = 100, mtry = 4
                           ,nodesize = 2, importance=TRUE)
set.seed(4983)
pred_rf_over <- predict(rf_model_o, newdata = test, type = "prob")[,2]
#Modelo Smote
rf_model_s <- randomForest(ECV ~ ., data = smote, ntree = 200, mtry =
                             5,nodesize = 3, importance=TRUE)
set.seed(4983)
pred_rf_smote <- predict(rf_model_s, newdata = test, type = "prob")[,2
]
#Modelo XGB para muestra normal
# Convertimos a matriz numérica
train_matrix <- model.matrix(ECV ~
                               .
                             -1, data = train)
test_matrix <- model.matrix(ECV ~
                              .
                            -1, data = test)
# XGBoost requiere la variable respuesta como vector numérico
train_label <- as.numeric(train$ECV) - 1
test_label <- as.numeric(test$ECV) - 1
set.seed(4983)
xgb_model<- xgboost(data = train_matrix,
                    label = train_label,
                    nrounds = 150,
                    max_depth = 2,
                    eta = 0.3,
                    gamma = 0,
                    colsample_bytree = 0.6,
                    min_child_weight = 1,
                    subsample = 0.75,
                    objective = "binary:logistic",
                    verbose = 0)
set.seed(4983)
pred_xgb_n <- predict(xgb_model, newdata = test_matrix)
#Gráfico de importancia de variables
imp <- varImp(xgb_model)$importance
imp$Variable <- rownames(imp)
# Cambia nombres aquí
imp$Variable <- nombres_correctos
ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Importancia de variables en XGBoost (caret)",
       x = "Variable",
       y = "Importancia relativa") +
  theme_minimal()
importance_matrix <- xgboost::xgb.importance(model = xgb_model)
print(importance_matrix)
# Convertir a data.table
dt_imp <- data.table(importance_matrix)
# Crear columna con nombre base (variable sin sufijo numérico)
dt_imp[, VariableBase := sub("[0-9]+$",
                             "", Feature)]

# Sumar importancia (Gain) por VariableBase
imp_agrupada <- dt_imp[, .(Gain = sum(Gain)), by = VariableBase]
# Ordenar de mayor a menor importancia
imp_agrupada <- imp_agrupada[order(-Gain)]
ggplot(imp_agrupada, aes(x = reorder(VariableBase, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # para barras horizontales
  labs(title = ""
       ,
       x = "Variable",
       y = "Importancia (Gain)") +
  theme_minimal()
#Modelo para Undersampling
train_matrix <- model.matrix(ECV ~
                               .
                             -1, data = under)
test_matrix <- model.matrix(ECV ~
                              .
                            -1, data = test)
train_label <- as.numeric(under$ECV) - 1
test_label <- as.numeric(test$ECV) - 1
xgb_model_u<- xgboost(data = train_matrix,
                      label = train_label,
                      nrounds = 150,
                      max_depth = 2,
                      eta = 0.4,
                      gamma = 0,
                      colsample_bytree = 0.8,
                      min_child_weight = 1,
                      subsample = 0.875,
                      objective = "binary:logistic",
                      verbose = 0)
set.seed(4983)
pred_xgb_u <- predict(xgb_model_u, newdata = test_matrix)
#Modelo para Oversampling
train_matrix <- model.matrix(ECV ~
                               .
                             -1, data = over)
test_matrix <- model.matrix(ECV ~
                              .
                            -1, data = test)
train_label <- as.numeric(over$ECV) - 1
test_label <- as.numeric(test$ECV) - 1
xgb_model_o<- xgboost(data = train_matrix,
                      label = train_label,
                      nrounds = 50,
                      max_depth = 4,
                      eta = 0.3,
                      gamma = 0,
                      colsample_bytree = 0.6,
                      min_child_weight = 1,
                      subsample = 0.875,
                      objective = "binary:logistic",
                      verbose = 0)
set.seed(4983)
pred_xgb_o <- predict(xgb_model_o, newdata = test_matrix)
#Modelo para SMOTE
train_matrix <- model.matrix(ECV ~
                               .
                             -1, data = smote)

test_matrix <- model.matrix(ECV ~
                              .
                            -1, data = test)
train_label <- as.numeric(smote$ECV) - 1
test_label <- as.numeric(test$ECV) - 1
xgb_model_s<- xgboost(data = train_matrix,
                      label = train_label,
                      nrounds = 150,
                      max_depth = 2,
                      eta = 0.3,
                      gamma = 0,
                      colsample_bytree = 0.8,
                      min_child_weight = 1,
                      subsample = 0.5,
                      objective = "binary:logistic",
                      verbose = 0)
set.seed(4983)
pred_xgb_s <- predict(xgb_model_s, newdata = test_matrix)
#Figura 1 Curvas ROC
par(mfrow = c(2, 2))
# -------- Muestra original --------
roc_glm_normal <- roc(test$ECV, pred_glm_normal)
roc_nb_normal <- roc(test$ECV, pred_nb_normal)
roc_rf_normal <- roc(test$ECV, pred_rf_normal)
roc_xgb_normal <- roc(test$ECV, pred_xgb_n)
plot(roc_glm_normal, col = "blue", lwd = 2, main = "ROC - Muestra Orig
inal")
lines(roc_nb_normal, col = "orange", lwd = 2)
lines(roc_rf_normal, col = "forestgreen", lwd = 2)
lines(roc_xgb_normal, col = "red", lwd = 2)
legend("bottomright", legend = c(
  paste0("GLM (AUC = ", round(auc(roc_glm_normal), 4), ")"),
  paste0("NB (AUC = ", round(auc(roc_nb_normal), 4), ")"),
  paste0("RF (AUC = ", round(auc(roc_rf_normal), 4), ")"),
  paste0("XGB (AUC = ", round(auc(roc_xgb_normal), 4), ")")
), col = c("blue", "orange", "forestgreen", "red"), lwd = 2, cex = 0.4
)
# -------- Undersampling --------
roc_glm_under <- roc(test$ECV, pred_glm_under)
roc_nb_under <- roc(test$ECV, pred_nb_under)
roc_rf_under <- roc(test$ECV, pred_rf_under)
roc_xgb_under <- roc(test$ECV, pred_xgb_u)
plot(roc_glm_under, col = "blue", lwd = 2, main = "ROC - Undersampling
")
lines(roc_nb_under, col = "orange", lwd = 2)
lines(roc_rf_under, col = "forestgreen", lwd = 2)
lines(roc_xgb_under, col = "red", lwd = 2)
legend("bottomright", legend = c(
  paste0("GLM (AUC = ", round(auc(roc_glm_under), 4), ")"),
  paste0("NB (AUC = ", round(auc(roc_nb_under), 4), ")"),
  paste0("RF (AUC = ", round(auc(roc_rf_under), 4), ")"),
  paste0("XGB (AUC = ", round(auc(roc_xgb_under), 4), ")")
), col = c("blue", "orange", "forestgreen", "red"), lwd = 2, cex = 0.4
)
# -------- Oversampling --------
roc_glm_over <- roc(test$ECV, pred_glm_over)
roc_nb_over <- roc(test$ECV, pred_nb_over)
roc_rf_over <- roc(test$ECV, pred_rf_over)
roc_xgb_over <- roc(test$ECV, pred_xgb_o)
plot(roc_glm_over, col = "blue", lwd = 2, main = "ROC - Oversampling")
lines(roc_nb_over, col = "orange", lwd = 2)
lines(roc_rf_over, col = "forestgreen", lwd = 2)
lines(roc_xgb_over, col = "red", lwd = 2)
legend("bottomright", legend = c(
  paste0("GLM (AUC = ", round(auc(roc_glm_over), 4), ")"),
  paste0("NB (AUC = ", round(auc(roc_nb_over), 4), ")"),
  paste0("RF (AUC = ", round(auc(roc_rf_over), 4), ")"),
  paste0("XGB (AUC = ", round(auc(roc_xgb_over), 4), ")")
), col = c("blue", "orange", "forestgreen", "red"), lwd = 1, cex = 0.4
)
# -------- SMOTE --------
roc_glm_smote <- roc(test$ECV, pred_glm_smote)
roc_nb_smote <- roc(test$ECV, pred_nb_smote)
roc_rf_smote <- roc(test$ECV, pred_rf_smote)
roc_xgb_smote <- roc(test$ECV, pred_xgb_s)
plot(roc_glm_smote, col = "blue", lwd = 2, main = "ROC - SMOTE")
lines(roc_nb_smote, col = "orange", lwd = 2)
lines(roc_rf_smote, col = "forestgreen", lwd = 2)
lines(roc_xgb_smote, col = "red", lwd = 2)
legend("bottomright", legend = c(
  paste0("GLM (AUC = ", round(auc(roc_glm_smote), 4), ")"),
  paste0("NB (AUC = ", round(auc(roc_nb_smote), 4), ")"),
  paste0("RF (AUC = ", round(auc(roc_rf_smote), 4), ")"),
  paste0("XGB (AUC = ", round(auc(roc_xgb_smote), 4), ")")
), col = c("blue", "orange", "forestgreen", "red"), lwd = 2, cex = 0.4
)
comparar_modelos <- function(prob_modelo1, prob_modelo2, prob_modelo3,
                             prob_modelo4, real, nombres = c("GLM", "NB", "RF", "XGB")) {
  calcular_metricas <- function(real, probs, nombre_modelo) {
    roc_obj <- roc(real, probs, quiet = TRUE, levels = c(0, 1), direction = "<")
    auc_val <- round(as.numeric(auc(roc_obj)), 4)
    umbral_youden <- coords(roc_obj, "best", ret = "threshold", transpose = TRUE)
    umbral_topleft <- coords(roc_obj, "best", ret = "threshold", best.method = "closest.topleft", transpose = TRUE)
    pred_youden <- ifelse(probs >= umbral_youden, 1, 0)
    cm_youden <- table(Real = real, Predicho = pred_youden)
    tp_youden <- cm_youden["0", "0"]
    fn_youden <- cm_youden["0", "1"]
    fp_youden <- cm_youden["1", "0"]
    tn_youden <- cm_youden["1", "1"]
    sens_youden <- tp_youden / (tp_youden + fn_youden)
    spec_youden <- tn_youden / (tn_youden + fp_youden)
    acc_youden <- (tp_youden + tn_youden) / sum(cm_youden)
    bal_acc_youden <- (sens_youden + spec_youden) / 2
    pred_topleft <- ifelse(probs >= umbral_topleft, 1, 0)
    cm_topleft <- table(Real = real, Predicho = pred_topleft)
    tp_topleft <- cm_topleft["0", "0"]
    fn_topleft <- cm_topleft["0", "1"]
    fp_topleft <- cm_topleft["1", "0"]
    tn_topleft <- cm_topleft["1", "1"]
    sens_topleft <- tp_topleft / (tp_topleft + fn_topleft)
    spec_topleft <- tn_topleft / (tn_topleft + fp_topleft)
    acc_topleft <- (tp_topleft + tn_topleft) / sum(cm_topleft)
    bal_acc_topleft <- (sens_topleft + spec_topleft) / 2
    data.frame(Modelo = c(paste(nombre_modelo, "- Youden"), paste(nombre_modelo",- Closest Topleft")),AUC = rep(auc_val, 2), Accuracy = c(acc_youden, acc_topleft), Sensibilidad = c(sens_youden, sens_topleft),Especificidad = c(spec_youden, spec_topleft), Balanced_Accuracy = c(bal_acc_youden, bal_acc_topleft), Umbral = c(umbral_youden, umbral_topleft))}
  resultados <- rbind(
    calcular_metricas(real, prob_modelo1, nombres[1]),
    calcular_metricas(real, prob_modelo2, nombres[2]),
    calcular_metricas(real, prob_modelo3, nombres[3]),
    calcular_metricas(real, prob_modelo4, nombres[4])
  )
  resultados[, 2:6] <- lapply(resultados[, 2:6], function(x) round(x,
                                                                   5))
  colorear_top2 <- function(col) {
    mejor <- max(col, na.rm = TRUE)
    segundo_mejor <- sort(unique(col), decreasing = TRUE)[2]
    sapply(col, function(x) {
      if (is.na(x)) return(cell_spec(x, format = "html", color = "black"))
      else if (x == mejor) return(cell_spec(x, format = "html", color
                                            = "#ff0000"))
      else if (x == segundo_mejor) return(cell_spec(x, format = "html"
                                                    , color = "#007bff"))
      else return(as.character(x))
    })
  }
  resultados_coloreados <- resultados %>%
    mutate(
      AUC = colorear_top2(AUC),
      Accuracy = colorear_top2(Accuracy),
      Sensibilidad = colorear_top2(Sensibilidad),
      Especificidad = colorear_top2(Especificidad),
      Balanced_Accuracy = colorear_top2(Balanced_Accuracy),
      Umbral = round(Umbral, 4)
    )
  kable(resultados_coloreados, escape = FALSE, format = "html", align
        = "c") %>%
    kable_styling(full_width = FALSE, position = "center",
                  bootstrap_options = c("striped", "hover"))
}
resultados_finales_n <- comparar_modelos(
  prob_modelo1 = pred_glm_normal,
  prob_modelo2 = pred_nb_normal,
  prob_modelo3 = pred_rf_normal,
  prob_modelo4 = pred_xgb_n,
  real = test$ECV,
  nombres = c("GLM", "NB", "RF", "XGB")
)
resultados_finales_n
resultados_finales_u <- comparar_modelos(
  prob_modelo1 = pred_glm_under,
  prob_modelo2 = pred_nb_under,
  prob_modelo3 = pred_rf_under,
  prob_modelo4 = pred_xgb_u,
  real = test$ECV,
  nombres = c("GLM", "NB", "RF", "XGB")
)
resultados_finales_u
resultados_finales_o <- comparar_modelos(
  prob_modelo1 = pred_glm_over,
  prob_modelo2 = pred_nb_over,
  prob_modelo3 = pred_rf_over,
  prob_modelo4 = pred_xgb_o,
  real = test$ECV,
  nombres = c("GLM", "NB", "RF", "XGB")
)
resultados_finales_o
resultados_finales_s <- comparar_modelos(
  prob_modelo1 = pred_glm_smote,
  prob_modelo2 = pred_nb_smote,
  prob_modelo3 = pred_rf_smote,
  prob_modelo4 = pred_xgb_s,
  real = test$ECV,
  nombres = c("GLM", "NB", "RF", "XGB")
)
resultados_finales_s