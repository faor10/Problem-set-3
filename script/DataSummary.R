##En este código se obtienen las estadisticas descriptivas de los datos utilizados
##en el PS3

rm(list=ls())
require("pacman")
p_load("here")
p_load("readr")
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(rio) # Librería para importar datos 
p_load(tidyverse) # Librería para limpiar datos
p_load(e1071) # Tiene la función para calcular skewness
p_load(EnvStats) # Transformación Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(dplyr)
p_load(caret)
p_load(glmnet)
p_load(pls)
p_load(tidyr)
p_load(tibble)
p_load(gtsummary)

p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata,
       spdep,
       secr,
       osmdata,
       here) # Get OSM's data


train<- as.data.frame(train)
levels(train$balcon_terr) <- c("Sin Balcón/Terraza", "Con Balcón/Terraza")
summary <- train %>% select(price,l3,bedrooms,new_surface,property_type,min_dist_bus,min_dist_market,balcon_terr)
summary %>% tbl_summary(by=l3,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                          all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list(l3 ~ "Ciudad"
                        )
                         
)

test<- as.data.frame(test)
levels(test$balcon_terr) <- c("Sin Balcón/Terraza", "Con Balcón/Terraza")
summary <- test %>% select(l3,bedrooms,new_surface,property_type,min_dist_bus,min_dist_market,balcon_terr)
summary %>% tbl_summary(by=l3,statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list(l3 ~ "Ciudad")
                        
)
