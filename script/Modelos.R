## Limpiamos
rm(list=ls())
require(pacman) 

## Llama/instala-llama las librer?as listadas
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones din?micas
       tmaptools, # geocode_OSM()
       osmdata,
       spdep,
       secr,
       osmdata,
       here) # Get OSM's data
path = here('')
##Librerias requeridas
rm(list=ls())
require("pacman")
p_load("here")
p_load("readr")
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(rio) # Librer?a para importar datos 
p_load(tidyverse) # Librer?a para limpiar datos
p_load(e1071) # Tiene la funci?n para calcular skewness
p_load(EnvStats) # Transformaci?n Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(dplyr)
p_load(caret)
p_load(glmnet)
p_load(pls)
p_load(tidyr)
p_load(tibble)
p_load(gtsummary)


#Modelo 1 Tradicional****************************************

mod1 <- lm(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + property_type + balcon_terr + l3, data = train)
summary(mod1)

##CORREMOS OLS 
model1 <- train(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + property_type + balcon_terr + l3 ,
                # model to fit
                data = train,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

##Coeficientes model 1 OLS
df_coeficientes <- mod1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

##Gráfica importancia coeficientes model 1 OLS
df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

##Predicciones training
price_predict_train<-predict(model1,newdata=train)
price_predict_train

##MSE
training_mse_OLS <- mean((price_predict_train - train$price)^2)
paste("Error (mse) de entrenamiento:", training_mse_OLS)

# Predicciones de test
predicciones_test_ols <- predict(model1, newdata = test)
predicciones_test_ols

# MSE de test
test_mse_ols <- mean((predicciones_test_ols - test$price)^2)
paste("Error (mse) de test:", test_mse_ols)


#Modelo 2 Ridge Personas****************************************

x_train <- model.matrix(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + property_type + balcon_terr + l3, data = train)[, -1]
y_train <- train$price

#scale(x_train)
model2_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- model2_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model2_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci?n de la regularizaci?n") +
  theme_bw() +
  theme(legend.position = "none")

cv_error_ridge <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error_ridge)
paste("Mejor valor de lambda encontrado:", cv_error_ridge$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviaci?n est?ndar:", cv_error_ridge$lambda.1se)

modelo2_ridge_lambdamin <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error_ridge$lambda.min,
  standardize = TRUE
)

modelo2_ridge_lambdamin


df_coeficientes_ridge <- coef(modelo2_ridge_lambdamin) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

##Coeficientes modelo ridge
df_coeficientes_ridge

##Gráfica importancia coeficientes ridge
df_coeficientes_ridge %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Predicciones de entrenamiento
predict_train_ridge <- predict(modelo2_ridge_lambdamin, newx = x_train)
predict_train_ridge
# MSE de entrenamiento
training_mse_ridge <- mean((predict_train_ridge - train$price)^2)
paste("Error (mse) de entrenamiento:", training_mse_ridge)


#Modelo 3 Lasso****************************************

model_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- model_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci?n de la regularizaci?n") +
  theme_bw() +
  theme(legend.position = "none")


cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error_lasso)
paste("Mejor valor de lambda encontrado:", cv_error_lasso$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviaci?n est?ndar:", cv_error_lasso$lambda.1se)


model_lasso_min <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_lasso$lambda.min,
  standardize = TRUE
)

model_lasso_min

df_coeficientes_lasso <- coef(model_lasso_min) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

#Coeficientes modelo lasso
df_coeficientes_lasso

#Gráfica importancia coeficientes lasso
df_coeficientes_lasso %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Predicciones de entrenamiento
predict_train_lasso <- predict(model_lasso_min, newx = x_train)
predict_train_lasso
# MSE de entrenamiento
training_mse_lasso <- mean((predict_train_lasso - train$price)^2)
paste("Error (mse) de entrenamiento:", training_mse_lasso)

#Modelo 4--Elastic Net-----------------------------------------------------------
el <- train(price ~ bedrooms + new_surface + min_dist_bus + min_dist_market + property_type + balcon_terr + l3, data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10), preProcess = c("center", "scale"))

el ##The final values used for the model were alpha = 0.55 and lambda = 900321.
## RMSE= 548747488

# Model Prediction
price_predict_el <- predict(el, train)
price_predict_el


#Modelo 5 Superlearners------------------------------------------------------------
require("tidyverse")
require("ranger")
require("SuperLearner")
install.packages("VGAM", dependencies = FALSE)
require("VGAM")
# set the seed for reproducibility
set.seed(123)
XS <- data.frame(train$bedrooms, train$l3, train$new_surface, train$min_dist_bus, train$min_dist_market, train$property_type, train$balcon_terr )
YS <- train$price
folds = 5
index <- split(1:1000, 1:folds)
splt <- lapply(1:folds, function(ind) D[index[[ind]], ])
##Aquí hacemos un SuperLearnes por medio de OLS, RF y Glmnet
fitY <- SuperLearner(Y = YS, X = XS,
                     method = "method.NNLS", SL.library = c("SL.mean","SL.lm", "SL.ranger", "SL.glmnet"),
                     cvControl = list(V = folds))

fitY ## Nos dice que el mejor mode es ranger_All RMSE: 1.612468e+17

# Now predict the outcome for all possible x
yS <- predict(fitY, newdata = data.frame(XS),onlySL = T)$pred
# Create a dataframe of all x and predicted SL responses
Dl1 <- data.frame(XS, yS)

##RMSE de todos los modelos realizados

#OLS
training_mse_OLS ##3.010064e+17
#Ridge
training_mse_ridge ##3.020867e+17
#Lasso
training_mse_lasso ##3.010205e+17
#Elastic Net
##RMSE= 548747488
#Superlearner
#RMSE: 1.612468e+17


