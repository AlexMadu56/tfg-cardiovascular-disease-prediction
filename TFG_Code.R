
######################## 1.CARGAMOS DATOS ############################################################################################################################################################################################################################################################################################################
library(haven)

datos <- read_xpt("-----/LLCP2023.XPT  2")
#Los visualizamos 

head( datos )

#Vemosdel tipo que son
str( datos )

colnames( datos ) 

#Resumen
summary( datos )

#Datos faltantes
colSums( is.na (datos)  )

#Dimensión de los datos
dim( datos )

######################## 2.ELECCIÓN VARIABLES ############################################################################################################################################################################################################

#Elegimos y renombramos las variables utilizadas a lo largo del estudio

variables_finales <- c("DIABETE4", "BPHIGH6", "_RFCHOL3", "_BMI5CAT","_SMOKER3"
                       , "_MICHD",
                       "_TOTINDA", "SEXVAR", "_AGE_G","CVDSTRK3","_RFHLTH","_PHYS14D","_MENT14D","_EDUCAG","_INCOMG1","DRNKANY6")


categorias <- c( "DIABETES", "HIPERTENSION", "COLESTEROL", "IMC","FUMAR",  "INFARTO", 
                 "ACTIVIDAD_FISICA","SEXO", "EDAD","DERRAME","SALUD_GENERAL","SALUD_F","SALUD_M","EDUCACIÓN","SALARIO","ALCOHOL")



datos_var<- datos[, variables_finales]

#Repetimos el proceso anterior
colSums( is.na (datos_var)  )

dim(datos_var)

names(datos_var) <- categorias

summary(datos_var)
nrow(unique(datos_var))

factores <- as.data.frame(lapply(datos_var, as.factor))

names(factores) <- categorias
summary(factores)
dim(factores)

########################3.PREPROCESAMIENTO NAs################################################################################################################################################################################################

#Eliminamos los Na y no sabe no contesta de la variable infarto y a partir de ahí hacemos una imputación múltiple

factores$DERRAME[factores$DERRAME %in% c(7, 9)] <- NA
factores$ECV <- NA  # Inicializamos la variable

factores$ECV[factores$DERRAME == 1 | factores$INFARTO == 1] <- 0
factores$ECV[
  is.na(factores$ECV) & 
    ((factores$DERRAME == 2 & factores$INFARTO != 1) | 
       (factores$INFARTO == 2 & factores$DERRAME != 1))
] <- 1
table(factores$ECV, useNA = "always")
factores <- factores[!is.na(factores$ECV), ]
table(factores$ECV, useNA = "always")


factores <- factores[, !(names(factores) %in% c("INFARTO", "DERRAME"))]

factores <- as.data.frame(lapply(factores, as.factor))


dim(factores)
summary(factores)

library(naniar)
library(dplyr)

# Ver resumen de los valores faltantes
gg_miss_var(factores)  # Muestra el porcentaje de valores faltantes por variable
factores_sample <- factores %>% slice_sample(n = 10000)  # Tomar 10,000 filas aleatorias
vis_miss(factores_sample)


######################## 3.1.1 Factorizamos las variables################################################################################################################################################################################################


summary(factores)

# Reemplazar las categorías no deseadas por NA y eliminar los niveles no utilizados
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

# Verifica que se eliminaron
summary(factores$SALUD_GENERAL)


factores$SALUD_F[ factores$SALUD_F == 9] <- NA
factores$SALUD_F <- droplevels(factores$SALUD_F)

# Verifica que se eliminaron
summary(factores$SALUD_F)

factores$SALUD_M[ factores$SALUD_M == 9] <- NA
factores$SALUD_M <- droplevels(factores$SALUD_M)

# Verifica que se eliminaron
summary(factores$SALUD_M)


factores$EDUCACIÓN[ factores$EDUCACIÓN == 9] <- NA
factores$EDUCACIÓN <- droplevels(factores$EDUCACIÓN)

# Verifica que se eliminaron
summary(factores$EDUCACIÓN)


factores$SALARIO[ factores$SALARIO == 9] <- NA
factores$SALARIO <- droplevels(factores$SALARIO)

# Verifica que se eliminaron
summary(factores$SALARIO)

factores$ALCOHOL[factores$ALCOHOL == 7 | factores$ALCOHOL == 9] <- NA
factores$ALCOHOL <- droplevels(factores$ALCOHOL)

factores$DIABETES[factores$DIABETES == 7 | factores$DIABETES == 9] <- NA
factores$DIABETES <- droplevels(factores$DIABETES)

summary(factores)

#Frecuencias

porcentaje_validos <- lapply(factores, function(x) {
  x_validos <- x[!(x %in% c( NA))]
  
  validos <- length(x_validos)      # Contar registros válidos
  total <- length(x)                # Total de registros (sin excluir valores específicos)
  
  validos / total * 100             # Porcentaje de datos válidos
})

porcentaje_validos 

## Renombrar

# DIABETES
factores$DIABETES <- factor(factores$DIABETES,
                            levels = c(1, 2, 3, 4),
                            labels = c("Sí", "Sí, solo durante el embarazo", "No", "PREDIABETES"))

# HIPERTENSION
factores$HIPERTENSION <- factor(factores$HIPERTENSION,
                                levels = c(1, 2, 3, 4),
                                labels = c("Sí", "Sí, solo durante el embarazo", "No", "Pre-hipertensión"))

# COLESTEROL
factores$COLESTEROL <- factor(factores$COLESTEROL,
                              levels = c(1, 2),
                              labels = c("No", "Sí"))

# IMC (clasificación estándar)
factores$IMC <- factor(factores$IMC,
                       levels = c(1, 2, 3, 4),
                       labels = c("Bajo peso", "Peso normal", "Sobrepeso", "Obesidad"))

# FUMAR
factores$FUMAR <- factor(factores$FUMAR,
                         levels = c(1, 2, 3, 4),
                         labels = c("Fumador diario", "Fumador ocasional", "Exfumador", "Nunca ha fumado"))

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
                                 labels = c( "Buena",  "Mala"))



# EDUCACIÓN
factores$EDUCACIÓN <- factor(factores$EDUCACIÓN,
                              levels = c(1, 2, 3, 4),
                              labels = c("No terminó secundaria",
                                         "Graduado de secundaria",
                                         "Asistió a universidad o escuela técnica",
                                         "Graduado universitario o técnico"))
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
                                      "1-13 días con salud física mala",
                                      "14 o más días con salud física mala"))

factores$SALUD_M <- factor(factores$SALUD_M,
                           levels = c(1, 2, 3),
                           labels = c("0 días con salud mental mala",
                                      "1-13 días con salud mental mala",
                                      "14 o más días con salud mental mala"))

summary(factores)
## Gráfico
path_escritorio <- file.path(Sys.getenv("HOME"), "Desktop", "graficoss")
dir.create(path_escritorio, showWarnings = FALSE)



counts <- table(factores$EDAD)

bar_midpoints <- barplot(
  counts,
  main = "EDAD",
  col = "lightblue",
  border = "black",
  names.arg = rep("", length(counts)),  # Oculta etiquetas por defecto
  ylab = "",
  yaxt = "n",              
  cex.main = 2
)

# Añadir etiquetas con rotación de 45 grados
text(x = bar_midpoints,
     y = par("usr")[3] - 0.02 * max(counts),  # posición vertical
     labels = names(counts),
     srt = 45,       # rotación
     adj = 1,        # alineación derecha
     xpd = TRUE,     # permitir que se dibuje fuera del área del plot
     cex = 1.2)



# Ruta donde se guardarán las imágenes
path_escritorio <- "/Users/alejandromaduenogalvez/Desktop" 
# Crear el directorio si no existe
if (!dir.exists(path_escritorio)) {
  dir.create(path_escritorio)
}

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
    names.arg = rep("", length(counts)),  # Oculta etiquetas por defecto
    ylab = "",
    yaxt = "n",
    cex.main = 2
  )
  
  # Agregar etiquetas rotadas
  text(x = bar_midpoints,
       y = par("usr")[3] - 0.02 * max(counts),
       labels = names(counts),
       srt = 45,
       adj = 1,
       xpd = TRUE,
       cex = 1.2)
  
  dev.off()
}

# Lista de variables a graficar
variables <- c("SALUD_M", "SALUD_F", "EDUCACIÓN")

# Generar gráficos
for (var in variables) {
  counts <- table(factores[[var]])
  
  png(filename = file.path(path_escritorio, paste0(var, ".png")),
      width = 1000, height = 800)
  
  # Aumentar margen inferior (mar[1])
  par(mar = c(14, 4, 4, 2) + 0.1)
  
  bar_midpoints <- barplot(
    counts,
    main = var,
    col = "lightblue",
    border = "black",
    names.arg = rep("", length(counts)),
    ylab = "",
    yaxt = "n",
    cex.main = 2
  )
  
  text(x = bar_midpoints,
       y = par("usr")[3] - 0.02 * max(counts),
       labels = names(counts),
       srt = 45,
       adj = 1,
       xpd = TRUE,
       cex = 1.2)
  
  dev.off()
}

#Observamos que en la mayoría de variables los datos faltantes son escasos y en porcentaje no son realmente relevantes, menor de un 5%.
#Aún así haremos imputación múltiple para que los datos faltantes nos permitan el uso de todos los modelos del estudio.
# Las variables con más de un 5% son CHEQUEO_COLESTEROL: 93.95875%, IMC: 90.71305% y FUMAR: 94.72856%.



######################## DIVISION DE DATASET ################################################################################################################################################################################################

set.seed(4983)  # Para reproducibilidad
train_index <- sample(1:nrow(factores), 0.8 * nrow(factores))  # 80% de los datos para entrenamiento


train_data <- factores[train_index, ]
table(train_data$INFARTO)
summary(train_data)
str(train_data)

test_data <- factores[-train_index, ]
table(test_data$INFARTO)
summary(test_data)
str(test_data)


######################## IMPURACIÓN MULTIPLE ################################################################################################################################################################################################
install.packages("mice")
library(mice)

library(missForest)

factores_imputado <-missForest(train_data, 
                               maxiter = 5,       # Menos iteraciones
                               ntree = 100,       # Menos árboles
                               verbose = TRUE)    # Ver progreso
factores_imputado2 <- missForest(test_data, 
                                 maxiter = 5,       # Menos iteraciones
                                 ntree = 100,       # Menos árboles
                                 verbose = TRUE)    # Ver progreso

factores_imputado<-factores_imputado$ximp
summary(factores_imputado)

factores_imputado2<-factores_imputado2$ximp
summary(factores_imputado2)

######################## Guardar train test ################################################################################################################################################################################################

write_sav(factores_imputado, "train.sav")
write_sav(factores_imputado2, "test.sav")

library(haven)
train <- read_sav("Desktop/train.sav")
train <- as.data.frame(lapply(train, as.factor))
train$ECV <- ifelse(train$ECV == 2, 1, 0)
train$ECV <- as.factor(train$ECV)

table(train$ECV)
summary(train)
test <- read_sav("Desktop/test.sav")
test <- as.data.frame(lapply(test, as.factor))
test$ECV <- ifelse(test$ECV == 2, 1, 0)
test$ECV <- as.factor(test$ECV)

table(test$ECV)
summary(test)
######################## BALANCEO ################################################################################################################################################################################################
########################  UNDERSAMPLING ################################################################################################################################################################################################
library(ROSE)  

set.seed(4983)
under<- ovun.sample(ECV ~ ., data = (train), method = "under", N = sum(train$ECV == 0) * 2)$data


table(train$ECV)

table(under$ECV)
######################## OVERSAMPLING ################################################################################################################################################################################################

library(ROSE)  
set.seed(4983)
N_target <- sum(train$ECV == 1) * 2

# Sobremuestreo
over <- ovun.sample(ECV ~ ., data = train, method = "over", N = N_target, seed = 4983)$data

# Verifica
table(over$ECV)





set.seed(4983)

# Número de positivos
n_pos <- sum(train$ECV == 1)

# Queremos que estos sean el 30% del total
# Entonces, total = n_pos / 0.3
N_target <- round(n_pos / 0.3)

# Oversampling con ROSE
over <- ovun.sample(ECV ~ ., data = train, method = "over", N = N_target, seed = 4983)$data

# Verifica proporciones
prop.table(table(over$ECV))
######################## 4.4 SMOTE-NC ################################################################################################################################################################################################
library(smotefamily)
train_encoded <- model.matrix(ECV ~ . -1, data = train)
train_label <- as.factor(train$ECV)

smote_out <- SMOTE(train_encoded, train_label, K = 5)

summary(smote_out)

balanced_data <- smote_out$data
balanced_data$.class <- as.factor(balanced_data$.class)


train$ECV <- as.factor(train$ECV)

# Aplica SMOTE solo a la clase minoritaria
smote_50_50 <- SmoteClassif(ECV~.,
  dat = train, 
  C.perc = list("0" = 2),  # Solo la clase minoritaria, la multiplicamos por 2
  k = 5,
  dist = "Overlap"  # Ideal para variables categóricas
)

smote_50_50 <- SmoteClassif(
  form = ECV ~ ., 
  dat = train, 
  C.perc = list("1" = 2),  # Aquí "1" debe coincidir con el valor de la clase minoritaria
  k = 5,
  dist = "Overlap"  # Recomendado para variables categóricas
)

library(UBL)

train_data_df <- as.data.frame(unique(train_data))

smote_50_50 <- SmoteClassif(ECV ~ ., 
                            dat = (train), 
                            C.perc = "balance",  
                            k = 2,  
                            dist = "Overlap")

# Calcular el nuevo C.perc en formato de lista
prop_deseada <- 0.3  # Queremos que la clase minoritaria represente el 30%
N0 <- sum(train$ECV == 0)  # Cantidad de No Infarto
N1 <- sum(train$ECV == 1)  # Cantidad de Infarto

# Calcular la cantidad de infartos que necesitamos generar
N1_nuevo <- (prop_deseada * N0) / (1 - prop_deseada)
C.perc_valor <- N1_nuevo / N1  # Factor de aumento de la clase minoritaria

# Crear la lista de proporciones para cada clase
C.perc_lista <- list("0" = C.perc_valor,  # No modificar la clase mayoritaria
                     "1" = 1)  # Sobremuestrear la clase minoritaria


# Aplicar SMOTE con el nuevo C.perc
smote_30_70 <- SmoteClassif(ECV ~ ., 
                            dat = train,  
                            C.perc = list("0"=2,"1"=0.8),  
                            k = 3,  
                            dist = "Overlap")

# Verificar la distribución de clases en el SMOTE 50-50

# Verificar la distribución de clases en el SMOTE 70-30
table(smote_50_50$ECV)


library(caret)
set.seed(4983)

# Índices para la primera mitad (50%)
indices_parte1 <- createDataPartition(train$ECV, p = 0.5, list = FALSE)

parte1 <- train[indices_parte1, ]
parte2 <- train[-indices_parte1, ]

library(UBL)  # o la librería que uses que tenga SmoteClassif
set.seed(4983)
smote1 <- SmoteClassif(ECV ~ ., dat = parte1, C.perc = list("0" = 7, "1" = 1), k = 5, dist = "Overlap")
set.seed(4983)
smote2 <- SmoteClassif(ECV ~ ., dat = parte2, C.perc = list("0" = 7, "1" = 1), k = 5, dist = "Overlap")

smote <- rbind(smote1, smote2)

table(smote$ECV)

###################### 5 MODELO GLM    ######################################################################################################################################################################################################################################
# Renombrar columnas en train
names(train)[names(train) == "EDUCACIÖN"] <- "EDUCACIÓN"
names(train)[names(train) == "SaALRIO"] <- "SALARIO"

# Renombrar columnas en test
names(test)[names(test) == "EDUCACIÖN"] <- "EDUCACIÓN"
names(test)[names(test) == "SaALRIO"] <- "SALARIO"

names(train)[names(train) == "Alcohol"] <- "ALCOHOL"
names(test)[names(test) == "Alcohol"] <- "ALCOHOL"

summary(train)
# DIABETES
train$DIABETES <- factor(train$DIABETES,
                         levels = c(1, 2, 3, 4),
                         labels = c("Sí", "Sí, pero solo durante el embarazo", "No", "PREDIABETES"))

test$DIABETES <- factor(test$DIABETES,
                        levels = c(1, 2, 3, 4),
                        labels = c("Sí", "Sí, pero solo durante el embarazo", "No", "PREDIABETES"))

# HIPERTENSION
train$HIPERTENSION <- factor(train$HIPERTENSION,
                             levels = c(1, 2, 3, 4),
                             labels = c("Sí", "Sí, pero solo durante el embarazo", "No", "Pre-hipertensión"))

test$HIPERTENSION <- factor(test$HIPERTENSION,
                            levels = c(1, 2, 3, 4),
                            labels = c("Sí", "Sí, pero solo durante el embarazo", "No", "Pre-hipertensión"))

# COLESTEROL
train$COLESTEROL <- factor(train$COLESTEROL,
                           levels = c(1, 2),
                           labels = c("No", "Sí"))

test$COLESTEROL <- factor(test$COLESTEROL,
                          levels = c(1, 2),
                          labels = c("No", "Sí"))

# IMC
train$IMC <- factor(train$IMC,
                    levels = c(1, 2, 3, 4),
                    labels = c("Bajo peso", "Peso normal", "Sobrepeso", "Obesidad"))

test$IMC <- factor(test$IMC,
                   levels = c(1, 2, 3, 4),
                   labels = c("Bajo peso", "Peso normal", "Sobrepeso", "Obesidad"))

# FUMAR
train$FUMAR <- factor(train$FUMAR,
                      levels = c(1, 2, 3, 4),
                      labels = c("Fumador diario", "Fumador ocasional", "Exfumador", "Nunca ha fumado"))

test$FUMAR <- factor(test$FUMAR,
                     levels = c(1, 2, 3, 4),
                     labels = c("Fumador diario", "Fumador ocasional", "Exfumador", "Nunca ha fumado"))

# ACTIVIDAD_FISICA
train$ACTIVIDAD_FISICA <- factor(train$ACTIVIDAD_FISICA,
                                 levels = c(1, 2),
                                 labels = c("Sí", "No"))

test$ACTIVIDAD_FISICA <- factor(test$ACTIVIDAD_FISICA,
                                levels = c(1, 2),
                                labels = c("Sí", "No"))

# SEXO
train$SEXO <- factor(train$SEXO,
                     levels = c(1, 2),
                     labels = c("Hombre", "Mujer"))

test$SEXO <- factor(test$SEXO,
                    levels = c(1, 2),
                    labels = c("Hombre", "Mujer"))

# EDAD (agrupada en 6 grupos)
train$EDAD <- factor(train$EDAD,
                     levels = 1:6,
                     labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 o más"))

test$EDAD <- factor(test$EDAD,
                    levels = 1:6,
                    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65 o más"))

# SALUD_GENERAL
train$SALUD_GENERAL <- factor(train$SALUD_GENERAL,
                              levels = c(1,2),
                              labels = c( "Buena",  "Mala"))

test$SALUD_GENERAL <- factor(test$SALUD_GENERAL,
                             levels = c(1,2),
                             labels = c( "Buena", "Mala"))

# SALUD_F (física)
train$SALUD_F <- factor(train$SALUD_F,
                        levels = 1:3,
                        labels = c("0 días", "1-13 días", "14+ días"))

test$SALUD_F <- factor(test$SALUD_F,
                       levels = 1:3,
                       labels = c("0 días", "1-13 días", "14+ días"))

# SALUD_M (mental)
train$SALUD_M <- factor(train$SALUD_M,
                        levels = 1:3,
                        labels = c("0 días", "1-13 días", "14+ días"))

test$SALUD_M <- factor(test$SALUD_M,
                       levels = 1:3,
                       labels = c("0 días", "1-13 días", "14+ días"))

# EDUCACIÓN
train$EDUCACIÓN <- factor(train$EDUCACIÓN,
                          levels = 1:4,
                          labels = c("No terminó secundaria", "Secundaria completa", 
                                     "Asistió a técnico/universidad", "Graduado de técnico/universidad"))

test$EDUCACIÓN <- factor(test$EDUCACIÓN,
                         levels = 1:4,
                         labels = c("No terminó secundaria", "Secundaria completa", 
                                    "Asistió a técnico/universidad", "Graduado de técnico/universidad"))

# SALARIO
train$SALARIO <- factor(train$SALARIO,
                        levels = 1:7,
                        labels = c("Menos de $15,000", "$15,000 a <$25,000", "$25,000 a <$35,000",
                                   "$35,000 a <$50,000", "$50,000 a <$100,000", "$100,000 a <$200,000",
                                   "$200,000 o más"))

test$SALARIO <- factor(test$SALARIO,
                       levels = 1:7,
                       labels = c("Menos de $15,000", "$15,000 a <$25,000", "$25,000 a <$35,000",
                                  "$35,000 a <$50,000", "$50,000 a <$100,000", "$100,000 a <$200,000",
                                  "$200,000 o más"))

# ECV (evento cerebrovascular)
train$ECV <- factor(train$ECV,
                    levels = c(0, 1),
                    labels = c("Sí", "No"))

test$ECV <- factor(test$ECV,
                   levels = c(0, 1),
                   labels = c("Sí", "No"))



train$ALCOHOL <- factor(train$ALCOHOL,
                    levels = c(1, 2),
                    labels = c("Sí", "No"))

test$ALCOHOL <- factor(test$ALCOHOL,
                   levels = c(1, 2),
                   labels = c("Sí", "No"))
summary(train)

summary(test)


# DIABETES → referencia: No
train$DIABETES <- relevel(train$DIABETES, ref = "No")

# HIPERTENSION → referencia: No
train$HIPERTENSION <- relevel(train$HIPERTENSION, ref = "No")

# COLESTEROL → referencia: No
train$COLESTEROL <- relevel(train$COLESTEROL, ref = "No")

# IMC → referencia: Peso normal
train$IMC <- relevel(train$IMC, ref = "Peso normal")

# FUMAR → referencia: Nunca ha fumado
train$FUMAR <- relevel(train$FUMAR, ref = "Nunca ha fumado")

# ACTIVIDAD_FISICA → referencia: Sí
train$ACTIVIDAD_FISICA <- relevel(train$ACTIVIDAD_FISICA, ref = "Sí")

# SEXO → referencia: Mujer
train$SEXO <- relevel(train$SEXO, ref = "Mujer")

# EDAD → referencia: 25-34 (por ejemplo)
train$EDAD <- relevel(train$EDAD, ref = "18-24")

# SALUD_GENERAL → referencia: Buena
train$SALUD_GENERAL <- relevel(train$SALUD_GENERAL, ref = "Mala")

# SALUD_FÍSICA → referencia: 0 días
train$SALUD_F <- relevel(train$SALUD_F, ref = "0 días")

train$ALCOHOL <- relevel(train$ALCOHOL, ref = "No")

# SALUD_MENTAL → referencia: 0 días
train$SALUD_M <- relevel(train$SALUD_M, ref = "0 días")

train$ECV<- relevel(train$ECV, ref = "No")
table(train$ECV)


set.seed(4983)  # Para reproducibilidad


model <- glm( ECV~ ., data = train, family = binomial)

summary(model)
library(knitr)
library(dplyr)

# Extraer coeficientes en data.frame
library(dplyr)
library(tibble)

# Extraer coeficientes y calcular OR
coef_df <- summary(model)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  rename(
    Estimate = Estimate,
    Std_Error = `Std. Error`,
    Z_value = `z value`,
    P_value = `Pr(>|z|)`
  ) %>%
  mutate(
    Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      P_value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    Odds_Ratio = exp(Estimate)
  )

library(knitr)
library(kableExtra)

coef_df %>%
  kable(
    format = "html",
    digits = 3,
    caption = "Resumen de coeficientes del modelo logístico",
    col.names = c("Variable", "Estimación", "Error Estándar", "Valor z", "Valor p", "Significancia", "Odds Ratio")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

library(pROC)

set.seed(4983) 
predicciones_prob_glm_normal <- predict(model, newdata = test, type = "response")

# Calcular la curva ROC
roc_curve <- roc(test$ECV, predicciones_prob)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Modelo")
auc(roc_curve) 

#Youden
coords(roc_curve, "best", ret = "threshold")

predicciones_clase <- ifelse(predicciones_prob >  0.9072469, "1", "0")  # 1 = Infarto, 0 = No infarto

confusionMatrix(factor(predicciones_clase, levels = c(1, 0)), factor(test$ECV, levels = c(1, 0)))

library(caret)
#Balanced
balanced_acc <- sapply(umbrales, function(t) {
  pred <- ifelse(predicciones_prob > t, 1, 0)
  cm <- confusionMatrix(factor(pred, levels = c(1, 0)),
                        factor(test$ECV, levels = c(1, 0)))
  mean(c(cm$byClass["Sensitivity"], cm$byClass["Specificity"]))
})

umbral_balacc <- umbrales[which.max(balanced_acc)]
predicciones_clase <- ifelse(predicciones_prob >  0.9072469, "1", "0")  # 1 = Infarto, 0 = No infarto

confusionMatrix(factor(predicciones_clase, levels = c(1, 0)), factor(test$ECV, levels = c(1, 0)))

#F1

library(MLmetrics)

umbrales <- seq(0.01, 1, 0.01)
f1_scores <- sapply(umbrales, function(t) {
  pred <- ifelse(predicciones_prob > t, 1, 0)
  F1_Score(y_true = test$ECV, y_pred = pred, positive = "1")
})

umbral_f1 <- umbrales[which.max(f1_scores)]
predicciones_clase <- ifelse(predicciones_prob >  0.9072469, "1", "0")  # 1 = Infarto, 0 = No infarto

confusionMatrix(factor(predicciones_clase, levels = c(1, 0)), factor(test$ECV, levels = c(1, 0)))

# Umbral por punto más cercano al tope izquierdo
umbral_topleft <- coords(roc_curve, "closest.topleft", ret = "threshold")[[1]]
predicciones_clase <- ifelse(predicciones_prob >  0.9072469, "1", "0")  # 1 = Infarto, 0 = No infarto
confusionMatrix(factor(predicciones_clase, levels = c(1, 0)), factor(test$ECV, levels = c(1, 0)))

GLMnormal
AUC 0.8208
Accuracy : 0.6788  
Sensitivity : 0.6584   1, sin infarto, clase positiva       
Specificity : 0.8359     
Balanced Accuracy : 0.7472          




######################  MODELO GLM NORMAL   ######################################################################################################################################################################################################################################
set.seed(4983)  # Para reproducibilidad
model <- glm( ECV~ ., data = under, family = binomial)

library(pROC)

set.seed(4983) 
predicciones_prob_glm_under <- predict(model, newdata = test, type = "response")

umbral <- 0.05
predicciones_clase <- ifelse(predicciones_prob >  0.3692242, "0", "1")  # 1 = Infarto, 0 = No infarto

#  Evaluar con la matriz de confusión
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)),positive = "1")

# Calcular la curva ROC
roc_curve <- roc(test$ECV, predicciones_prob)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Modelo")
auc(roc_curve) 
coords(roc_curve, "best", ret = "threshold")

confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)))

AUC 0.8209
Accuracy : 0.7097 mejora  
Sensitivity : 0.79620   0,  infarto, clase positiva       
Specificity : 0.69850     
Balanced Accuracy : 0.74735   

######################  MODELO GLM UNDER   ######################################################################################################################################################################################################################################
set.seed(4983)  # Para reproducibilidad
model <- glm( ECV~ ., data = under, family = binomial)

library(pROC)
model <- glm( ECV~ ., data = under9, family = binomial)

set.seed(4983) 
predicciones_prob <- predict(model, newdata = test, type = "response")

umbral <- 0.05
predicciones_clase <- ifelse(predicciones_prob >  0.6, "0", "1")  # 1 = Infarto, 0 = No infarto

#  Evaluar con la matriz de confusión
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)))

# Calcular la curva ROC
roc_curve <- roc(test$ECV, predicciones_prob)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Modelo")
auc(roc_curve) 

confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test_data$INFARTO, levels = c(0, 1)))


umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Random Forest con SMOTE")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

######################  MODELO GLM OVER   ######################################################################################################################################################################################################################################

model <- glm( ECV~ ., data = over, family = binomial)

library(pROC)

set.seed(4983)
pred_glm_over <- predict(model, newdata = test, type = "response")

model <- glm( ECV~ .-SALUD_M, data = over, family = binomial)

library(pROC)

set.seed(4983)
pred_glm_over <- predict(model, newdata = test, type = "response")

#  Convertir probabilidades a clases usando el umbral elegido
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > umbral, 1, 0)  # 1 = Infarto, 0 = No infarto

# Evaluar con la matriz de confusión
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)))

#  Calcular la curva ROC
roc_curve <- roc(test$ECV, pred_glm_over)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Modelo")
auc(roc_curve)  # Área bajo la curva

best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold #0.4848145

#6 Mejor umbral
predicciones_clase <- ifelse(predicciones_prob > 0.5004444, 0, 1)  # 1 = Infarto, 0 = No infarto

confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)))

umbrales <- seq(0.01, 1, by = 0.01)

# Paso 2: crear vectores para guardar sensibilidad y especificidad
sensibilidades <- numeric(length(umbrales))
especificidades <- numeric(length(umbrales))

# Paso 3: recorrer cada umbral
for (i in seq_along(umbrales)) {
  umbral <- umbrales[i]
  
  # Predicciones en clase binaria
  pred_clase <- ifelse(predicciones_prob > umbral, "0", "1")
  
  # Matriz de confusión
  cm <- confusionMatrix(factor(pred_clase, levels = c("0", "1")), 
                        factor(test$ECV, levels = c("0", "1")))
  
  # Extraer sensibilidad y especificidad
  sensibilidades[i] <- cm$byClass["Sensitivity"]
  especificidades[i] <- cm$byClass["Specificity"]
}

# Paso 4: graficar
plot(especificidades, sensibilidades, type = "l", col = "blue", lwd = 2,
     xlab = "Especificidad", ylab = "Sensibilidad",
     main = "Curva Sensibilidad vs Especificidad")


AUC: 0.8209
Accuracy : 0.6916          
Sensitivity : 0.81887         
Specificity : 0.67511     
Balanced Accuracy : 0.74699         

#####################GLM CON SMOTE#####################


model <- glm( ECV~ ., data = smote, family = binomial)

library(pROC)

set.seed(4983)
pred_glm_smote <- predict(model, newdata = test, type = "response")


#  Convertir probabilidades a clases usando el umbral elegido
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > umbral, 0, 1)  # 1 = Infarto, 0 = No infarto

# Evaluar con la matriz de confusión
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)))

#  Calcular la curva ROC
roc_curve <- roc(test$ECV, pred_xgb_smote)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Modelo")
auc(roc_curve)  # Área bajo la curva

best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold #0.579282

#6 Mejor umbral
predicciones_clase <- ifelse(predicciones_prob >0.579282, 1, 0)  # 1 = Infarto, 0 = No infarto

confusionMatrix(factor(predicciones_clase, levels = c(0, 1)), factor(test$ECV, levels = c(0, 1)))


# Secuencia de umbrales a evaluar
umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "1", "0")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Random Forest con SMOTE")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)




AUC: 0.8195
Accuracy : 0.6877          
Sensitivity : 0.82425         
Specificity : 0.67000    
Balanced Accuracy : 0.74713         

######################  MODELO rf normal  ######################################################################################################################################################################################################################################

library(randomForest)
set.seed(4983)  # Para reproducibilidad
train$ECV<-as.factor(train$ECV)
# Entrenar el modelo de Random Forest
rf_model <- randomForest(ECV ~ ., data = unique(train), ntree = 100, mtry = sqrt(ncol(train)-1))

# Ver el resumen del modelo# Ver el resumunique()en del modelo
print(rf_model)

set.seed(4983) 
predicciones_prob <- predict(rf_model, newdata = test, type = "prob")[,2]  # Probabilidad de INFARTO = 1
umbral <- 0.5

predicciones_clase <- ifelse(pred_rf_normal > .9775, "1", "0")  # 1 = Infarto, 0 = No infarto
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
library(pROC)

roc_curve <- roc(test$ECV, predicciones_prob)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Random Forest")
auc(roc_curve)  # Área bajo la curva

set.seed(4983)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # 0.05
predicciones_clase <- ifelse(predicciones_prob > 0.965, "1", "0")  # Nuevo umbral óptimo

# Evaluar con la Matriz de Confusión
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))


umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Random Forest con SMOTE")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)



AUC: Area under the curve: 0.7402
Accuracy : 0.709          
Sensitivity : 0.68052        
Specificity : 0.71272  
Balanced Accuracy : 0.69662        

######################  MODELO rf normal  ######################################################################################################################################################################################################################################

library(randomForest)
set.seed(4983)  # Para reproducibilidad
train$ECV<-as.factor(train$ECV)
# Entrenar el modelo de Random Forest
rf_model <- randomForest(ECV ~ ., data = train, ntree = 300, mtry = 3,nodesize = 10, importance=TRUE)

# Ver el resumen del modelo
print(rf_model)

set.seed(4983) 
predicciones_prob <- predict(rf_model, newdata = test, type = "prob")[,2]  # Probabilidad de INFARTO = 1
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > .985, "1", "0")  # 1 = Infarto, 0 = No infarto
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
library(pROC)

roc_curve <- roc(test$ECV, predicciones_prob)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Random Forest Normal")
auc(roc_curve)  # Área bajo la curva

set.seed(4983)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # 0.05
predicciones_clase <- ifelse(predicciones_prob > 0.955, "1", "0")  # Nuevo umbral óptimo

# Evaluar con la Matriz de Confusión
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))



######################  MODELO rf under  ######################################################################################################################################################################################################################################
Area under the curve: 0.7994
Accuracy : 0.7043          
Sensitivity : 0.78258         
Specificity : 0.69415 
Balanced Accuracy : 0.73836         


library(randomForest)
set.seed(4983)  # Para reproducibilidad
# Entrenar el modelo de Random Forest
rf_model <- randomForest(ECV ~ ., data = under9, ntree = 100, mtry = 6,nodesize = 10, importance=TRUE)

# Ver el resumen del modelo
print(rf_model)

set.seed(4983) 
pred_rf_under <- predict(rf_model, newdata = test, type = "prob")[,2]  # Probabilidad de INFARTO = 1
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > .5, "0", "1")  # 1 = Infarto, 0 = No infarto
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
library(pROC)

roc_curve <- roc(test$ECV, predicciones_prob)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Random Forest")
auc(roc_curve)  # Área bajo la curva

set.seed(4983)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # 0.05
predicciones_clase <- ifelse(predicciones_prob > 0.485, "0", "1")  # Nuevo umbral óptimo

# Evaluar con la Matriz de Confusión
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))



# Secuencia de umbrales a evaluar
umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Random Forest con SMOTE")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

######################  MODELO rf over  ######################################################################################################################################################################################################################################

library(randomForest)
set.seed(4983)  # Para reproducibilidad
# Entrenar el modelo de Random Forest

rf_model <- randomForest(ECV ~ .-FUMAR-SALUD_GENERAL-EDUCACIÖN-Alcohol, data = over, ntree = 100, mtry = 6,nodesize = 2, importance=TRUE)
print(rf_model$importance)
# Ver el resumen del modelo
print(rf_model)

set.seed(4983) 
pred_rf_over <- predict(rf_model, newdata = test, type = "prob")[,2]  # Probabilidad de INFARTO = 1
umbral <- 0.5
roc_curve <- roc(test$ECV, pred_rf_over)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Random Forest")
auc(roc_curve)  


predicciones_clase <- ifelse(predicciones_prob > .2, "0", "1")  # 1 = Infarto, 0 = No infarto
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
library(pROC)

roc_curve <- roc(test$ECV, pred_rf_over)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Random Forest")
auc(roc_curve)  # Área bajo la curva

set.seed(4983)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # 0.05
predicciones_clase <- ifelse(predicciones_prob > 0.275, "0", "1")  # Nuevo umbral óptimo

# Evaluar con la Matriz de Confusión
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Random Forest con SMOTE")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)




Area under the curve: 0.789
Accuracy : 0.6763          
Sensitivity : 0.81460         
Specificity : 0.65838   
Balanced Accuracy : 0.73649         

######################  MODELO rf smote  ######################################################################################################################################################################################################################################

library(randomForest)
set.seed(4983)  # Para reproducibilidad
# Entrenar el modelo de Random Forest
rf_model <- randomForest(ECV ~ ., data = smote, ntree = 100, mtry = 4,nodesize=5)

# Ver el resumen del modelo
print(rf_model)

set.seed(4983) 
pred_rf_smote<- predict(rf_model, newdata = test, type = "prob")[,2]  # Probabilidad de INFARTO = 1
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > .5, "1", "0")  # 1 = Infarto, 0 = No infarto
library(caret)
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
library(pROC)

roc_curve <- roc(test$ECV, pred_rf_smote)  # Aquí se usan probabilidades
plot(roc_curve, col = "blue", main = "Curva ROC - Random Forest")
auc(roc_curve)  # Área bajo la curva

set.seed(4983)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  
predicciones_clase <- ifelse(predicciones_prob > 0.655, "1", "0")  # Nuevo umbral óptimo

# Evaluar con la Matriz de Confusión
confusionMatrix(factor(predicciones_clase, levels = c("0", "1")), 
                factor(test$ECV, levels = c(0, 1)))
umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "1", "0")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Random Forest con SMOTE")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

Area under the curve: 0.7922
Accuracy : 0.6959         
Sensitivity : 0.78878        
Specificity : 0.68384       
Balanced Accuracy : 0.73631        


######################  MODELO nb normal  ######################################################################################################################################################################################################################################


install.packages("e1071")  # solo si no está instalado
library(e1071)
library(caret)
library(pROC)


library(e1071)
library(dplyr)
library(tidyr)

# Extraer tablas de la distribución condicional
tablas <- model_nb$tables

# Crear un resumen de la importancia basado en la diferencia máxima entre clases para cada variable categórica
importancia <- lapply(names(tablas), function(var) {
  tabla <- as.data.frame(tablas[[var]])
  if(ncol(tabla) == 2) { # solo para variables binarias
    diff <- abs(tabla[,1] - tabla[,2])
    data.frame(Variable = var, Importancia = max(diff))
  } else {
    data.frame(Variable = var, Importancia = NA) # para variables con más niveles podrías adaptar
  }
}) %>% bind_rows()

# Ordenar por importancia
importancia <- importancia %>% arrange(desc(Importancia))

print(importancia)







# Entrenar modelo Naive Bayes
model_nb <- naiveBayes(ECV ~ ., data = train)

# Predecir probabilidades
predicciones_prob <- predict(model_nb, newdata = test, type = "raw")[,2]  # Probabilidad de ECV = 1

# Elegir umbral (puedes empezar con 0.5 o el óptimo después)
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > umbral, "0", "1")

# Evaluar con matriz de confusión
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

# Curva ROC
roc_curve <- roc(test$ECV, predicciones_prob)
plot(roc_curve, col = "blue", main = "Curva ROC - Naive Bayes Normal")
auc(roc_curve)  # Área bajo la curva

# Obtener umbral óptimo (maximiza Sensibilidad + Especificidad)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # lo puedes usar para ajustar tu umbral

# Evaluar usando ese umbral óptimo
predicciones_clase <- ifelse(predicciones_prob > 0.9329214, "1", "0")
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)


best_threshold <- coords(roc_curve, "best", ret = "threshold", best.method = "closest.topleft")
best_threshold  # lo puedes usar para ajustar tu umbral

# Evaluar usando ese umbral óptimo
predicciones_clase <- ifelse(predicciones_prob > 0.9105446, "1", "0")
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)



umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}
# Graficar
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Naive Bayes")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)




######################  MODELO nb under  ######################################################################################################################################################################################################################################


install.packages("e1071")  # solo si no está instalado
library(e1071)
library(caret)
library(pROC)

# Entrenar modelo Naive Bayes
model_nb <- naiveBayes(ECV ~ ., data = under)

# Predecir probabilidades
set.seed(4983)
pred_nb_under <- predict(model_nb, newdata = test, type = "raw")[,2]  # Probabilidad de ECV = 1

# Elegir umbral (puedes empezar con 0.5 o el óptimo después)
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > umbral, "0", "1")

# Evaluar con matriz de confusión
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

# Curva ROC
roc_curve <- roc(test$ECV, predicciones_prob)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - Naive Bayes")
auc(roc_curve)  # Área bajo la curva

# Obtener umbral óptimo (maximiza Sensibilidad + Especificidad)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # lo puedes usar para ajustar tu umbral

# Evaluar usando ese umbral óptimo
predicciones_clase <- ifelse(predicciones_prob > 0.3692242, "0", "1")
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}
# Graficar
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Naive Bayes")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

Area under the curve: 0.8084
Accuracy : 0.687           
Sensitivity : 0.80098         
Specificity : 0.67221  
Balanced Accuracy : 0.73659         

######################  MODELO nb OVERr  ######################################################################################################################################################################################################################################


install.packages("e1071")  # solo si no está instalado
library(e1071)
library(caret)
library(pROC)

# Entrenar modelo Naive Bayes
model_nb <- naiveBayes(ECV ~ ., data = over)

# Predecir probabilidades# Predecir prooverbabilidades
set.seed(4983)
pred_nb_over <- predict(model_nb, newdata = test, type = "raw")[,2]  # Probabilidad de ECV = 1

# Elegir umbral (puedes empezar con 0.5 o el óptimo después)
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > umbral, "0", "1")

# Evaluar con matriz de confusión
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

# Curva ROC
roc_curve <- roc(test$ECV, pred_nb_over)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - Naive Bayes")
auc(roc_curve)  # Área bajo la curva

# Obtener umbral óptimo (maximiza Sensibilidad + Especificidad)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # lo puedes usar para ajustar tu umbral

# Evaluar usando ese umbral óptimo
predicciones_clase <- ifelse(predicciones_prob > 0.3502074, "0", "1")
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}
# Graficar
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Naive Bayes")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)


Area under the curve: 0.8086
Accuracy : 0.6793          
Sensitivity : 0.8119          
Specificity : 0.6621  
Balanced Accuracy : 0.7370          

######################  MODELO nb smote  ######################################################################################################################################################################################################################################


install.packages("e1071")  # solo si no está instalado
library(e1071)
library(caret)
library(pROC)

# Entrenar modelo Naive Bayes
model_nb <- naiveBayes(ECV ~ ., data = smote)

# Predecir probabilidades
pred_nb_smote<- predict(model_nb, newdata = test, type = "raw")[,2]  # Probabilidad de ECV = 1

# Elegir umbral (puedes empezar con 0.5 o el óptimo después)
umbral <- 0.5
predicciones_clase <- ifelse(predicciones_prob > umbral, "1", "0")

# Evaluar con matriz de confusión
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

# Curva ROC
roc_curve <- roc(test$ECV, predicciones_prob)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - Naive Bayes")
auc(roc_curve)  # Área bajo la curva

# Obtener umbral óptimo (maximiza Sensibilidad + Especificidad)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold  # lo puedes usar para ajustar tu umbral

# Evaluar usando ese umbral óptimo
predicciones_clase <- ifelse(predicciones_prob > 0.6753051, "1", "0")
confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test$ECV, levels = c(0, 1))
)

umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "1", "0")
  
  cm <- confusionMatrix(
    factor(pred_clase, levels = c("0", "1")),
    factor(test$ECV, levels = c(0, 1))
  )
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}
# Graficar
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - Naive Bayes")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

Area under the curve: 0.8099
Accuracy : 0.682           
Sensitivity : 0.81043         
Specificity : 0.66538         
Balanced Accuracy : 0.73791         


######################  MODELO xgboost  ######################################################################################################################################################################################################################################
library(xgboost)

# Convertimos a matriz numérica
train_matrix <- model.matrix(ECV ~ . -1, data = train)
test_matrix  <- model.matrix(ECV ~ . -1, data = test)

# XGBoost requiere la variable respuesta como vector numérico
train_label <- as.numeric(train$ECV) - 1  # Asume que ECV tiene niveles 1 y 2
test_label  <- as.numeric(test$ECV) - 1



set.seed(4983)
xgb_model <- xgboost(data = train_matrix, 
                     label = train_label, 
                     nrounds = 200, 
                     max_depth = 1,
                     eta = 0.4,
                     gamma = 0,
                     colsample_bytree = 0.8,
                     min_child_weight = 1,
                     subsample = 0.625,
                     objective = "binary:logistic", 
                     verbose = 0)

predicciones_prob <- predict(xgb_model, newdata = test_matrix)

library(pROC)
roc_curve <- roc(test_label, predicciones_prob)
plot(roc_curve, col = "blue", main = "Curva ROC - XGBoost Normal")
auc(roc_curve)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold
library(caret)
predicciones_clase <- ifelse(predicciones_prob > 0.8912951, "1", "0")

confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test_label, levels = c(0, 1))
)

Area under the curve: 0.8203
Accuracy : 0.7106          
Sensitivity : 0.79579         
Specificity : 0.69950  
Balanced Accuracy : 0.74765         

######################  MODELO xgboost under  ######################################################################################################################################################################################################################################
install.packages("xgboost")
install.packages("caret")
install.packages("pROC")


library(xgboost)

# Convertimos a matriz numérica
train_matrix <- model.matrix(ECV ~ . -1, data = under)
test_matrix  <- model.matrix(ECV ~ . -1, data = test)

# XGBoost requiere la variable respuesta como vector numérico
train_label <- as.numeric(under$ECV) - 1  # Asume que ECV tiene niveles 1 y 2
test_label  <- as.numeric(test$ECV) - 1


set.seed(4983)
xgb_model <- xgboost(
  data = train_matrix, 
  label = train_label, 
  nrounds = 250,
  max_depth = 2,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.75,
  objective = "binary:logistic",
  verbose = 0
)

pred_xgb_under <- predict(xgb_model, newdata = test_matrix)

library(pROC)
roc_curve <- roc(test_label, pred_xgb_under)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - XGBoost")
auc(roc_curve)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold
library(caret)
predicciones_clase <- ifelse(pred_xgb_under > 0.5092825, "1", "0")

confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test_label, levels = c(0, 1))
)

umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  cm <- confusionMatrix(factor(pred_clase, levels = c("0", "1")),
                        factor(test_label, levels = c(0, 1)))
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - XGBoost")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)


# Definir la matriz y la etiqueta
train_matrix <- model.matrix(ECV ~ . -1, data = under)
train_label <- as.numeric(under$ECV) - 1

# Crear objeto trainControl con búsqueda aleatoria
ctrl <- trainControl(
  method = "cv",
  number = 3,
  search = "random",       # <- aquí se activa la búsqueda aleatoria
  verboseIter = TRUE
)

set.seed(1234)
xgb_tune_random <- train(
  x = train_matrix,
  y = as.factor(under$ECV),
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 20,         # Número de combinaciones aleatorias que probará
  metric = "Accuracy"
)

# Ver la mejor combinación encontrada
xgb_tune_random$bestTune



Area under the curve: 0.8167
Accuracy : 0.7222          
Sensitivity : 0.77516         
Specificity : 0.71537  
Balanced Accuracy : 0.74526         

######################  MODELO xgboost Over  ######################################################################################################################################################################################################################################
Area under the curve: 0.8187
Accuracy : 0.7218     
Sensitivity : 0.77750   
Specificity : 0.71456    
Balanced Accuracy : 0.74603         
 
library(xgboost)

# Convertimos a matriz numérica
train_matrix <- model.matrix(ECV ~ . -1, data = over)
test_matrix  <- model.matrix(ECV ~ . -1, data = test)

# XGBoost requiere la variable respuesta como vector numérico
train_label <- as.numeric(over$ECV) - 1  # Asume que ECV tiene niveles 1 y 2
test_label  <- as.numeric(test$ECV) - 1


set.seed(4983)
xgb_model <- xgboost(
  data = train_matrix,
  label = train_label,
  nrounds = 50,
  max_depth = 2,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 1,
  objective = "binary:logistic",
  verbose = 0
)

pred_xgb_over<- predict(xgb_model, newdata = test_matrix)

library(pROC)
roc_curve <- roc(test_label, pred_xgb_over)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - XGBoost")
auc(roc_curve)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold
library(caret)
predicciones_clase <- ifelse(predicciones_prob > 0.5051543, "0", "1")

confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test_label, levels = c(0, 1))
)

umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  cm <- confusionMatrix(factor(pred_clase, levels = c("0", "1")),
                        factor(test_label, levels = c(0, 1)))
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - XGBoost")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)


#XG OVER HIPERPARAMETROS


# Convertimos a matriz y label
train_matrix <- model.matrix(ECV ~ . -1, data = over)
train_label <- as.numeric(over$ECV) - 1  # Asume factor con niveles 1 y 2

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)

# Definir los hiperparámetros que queremos probar
tune_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(1, 5),
  subsample = c(0.7, 1)
)

# Entrenamiento cruzado
set.seed(4983)
xgb_tune <- train(
  x = train_matrix,
  y = as.factor(over$ECV),
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 3, verboseIter = TRUE),
  tuneGrid = tune_grid,
  metric = "Accuracy"
)

# Ver los mejores hiperparámetros
xgb_tune$bestTune



# Entrenar modelo final XGBoost con los mejores hiperparámetros
xgb_model <- xgboost(
  data = train_matrix, 
  label = train_label, 
  nrounds = 250, 
  max_depth = 1,
  eta = 0.4,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 0.5,
  objective = "binary:logistic", 
  verbose = 0
)

set.seed(4983)
# Predicciones (probabilidades)
pred_xgb_over <- predict(xgb_model, newdata = test_matrix)

# Curva ROC y AUC
library(pROC)
roc_curve <- roc(test_label, predicciones_prob)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - XGBoost (ajustado)")
auc(roc_curve)

# Umbral óptimo (método de Youden: max(Sensibilidad + Especificidad - 1))
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold

# Clasificación con umbral óptimo
library(caret)
predicciones_clase <- ifelse(predicciones_prob > 0.5060737, "0", "1")

confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test_label, levels = c(0, 1))
)

# Evaluar sensibilidad y especificidad para múltiples umbrales
umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  cm <- confusionMatrix(factor(pred_clase, levels = c("0", "1")),
                        factor(test_label, levels = c(0, 1)))
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Gráfico de Sensibilidad y Especificidad vs Umbral
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - XGBoost (ajustado)")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

xgb_tune_random <- train(
  x = train_matrix,
  y = as.factor(over$ECV),
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 20,         # Número de combinaciones aleatorias que probará
  metric = "Accuracy"
)

# Ver la mejor combinación encontrada
xgb_tune_random$bestTune
######################  MODELO xgboost smote  ######################################################################################################################################################################################################################################
Area under the curve: 0.8175
Accuracy : 0.7049          
Sensitivity : 0.7953          
Specificity : 0.6931   
Balanced Accuracy : 0.7442          


library(xgboost)

# Convertimos a matriz numérica
train_matrix <- model.matrix(ECV ~ . -1, data = smote)
test_matrix  <- model.matrix(ECV ~ . -1, data = test)

# XGBoost requiere la variable respuesta como vector numérico
train_label <- as.numeric(smote$ECV) - 1  # Asume que ECV tiene niveles 1 y 2
test_label  <- as.numeric(test$ECV) - 1


set.seed(4983)




xgb_model <- xgboost(
  data = train_matrix, 
  label = train_label, 
  nrounds = 50, 
  max_depth =2,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 1,
  objective = "binary:logistic", 
  verbose = 0
)
set.seed(4983)

pred_xgb_smote<- predict(xgb_model, newdata = test_matrix)

library(pROC)
roc_curve <- roc(test_label, pred_xgb_smote)
plot(roc_curve, col = "darkgreen", main = "Curva ROC - XGBoost")
auc(roc_curve)
best_threshold <- coords(roc_curve, "best", ret = "threshold")
best_threshold
library(caret)
predicciones_clase <- ifelse(predicciones_prob > 0.55, "1", "0")

confusionMatrix(
  factor(predicciones_clase, levels = c("0", "1")),
  factor(test_label, levels = c(0, 1))
)

umbrales <- seq(0.01, 1, by = 0.01)
metricas <- data.frame()

for (t in umbrales) {
  pred_clase <- ifelse(predicciones_prob > t, "0", "1")
  cm <- confusionMatrix(factor(pred_clase, levels = c("0", "1")),
                        factor(test_label, levels = c(0, 1)))
  
  metricas <- rbind(metricas, data.frame(
    Umbral = t,
    Accuracy = cm$smoteall["Accuracy"],
    Kappa = cm$smoteall["Kappa"],
    Sensibilidad = cm$byClass["Sensitivity"],
    Especificidad = cm$byClass["Specificity"]
  ))
}

# Graficar Sensibilidad y Especificidad
plot(metricas$Umbral, metricas$Sensibilidad, type = "l", col = "red", lwd = 2,
     ylim = c(0, 1), ylab = "Sensibilidad / Especificidad", xlab = "Umbral", 
     main = "Sensibilidad vs Especificidad - XGBoost")
lines(metricas$Umbral, metricas$Especificidad, col = "blue", lwd = 2)
legend("bottomleft", legend = c("Sensibilidad", "Especificidad"), 
       col = c("red", "blue"), lty = 1, lwd = 2)

