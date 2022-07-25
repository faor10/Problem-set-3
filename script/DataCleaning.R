##En este script se presenta la limpieza y el tratamiento de datos realizado para el PS3.
##Se presenta el uso de datos espaciales para determinar la superficie de apartamentos que no cuentan con esta info.
##Se presenta el uso de datos espaciales para determinar la distancia minima a las estaciones de bus y supermercados.
##Se presenta el uso de texto para determinar la superficie de apartamentos que no cuentan con esta info.
## Llamar pacman (contiene la función p_load)
rm(list=ls())
require(pacman) 

## Llama/instala-llama las librerías listadas
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata,
       spdep,
       secr,
       osmdata,
       here) # Get OSM's data
path = here('')


train<-readRDS(here(path,"stores/train.Rds"))
test<-readRDS(here(path,"stores/test.Rds"))

#Limpieza de datos******************
#Conversion a factores

train <- train %>%
  mutate_at(.vars = c(
    "property_type","operation_type","l3"),
    .funs = factor)

test <- test %>%
  mutate_at(.vars = c(
    "property_type","operation_type","l3"),
    .funs = factor)

##Conversion a datos espaciales sf
train<-train %>% mutate(latp=lat,longp=lon)
train<-st_as_sf(train,coords=c('longp','latp'),crs="WGS84")
test<-test %>% mutate(latp=lat,longp=lon)
test<-st_as_sf(test,coords=c('longp','latp'),crs="WGS84")

train_bog<-subset(train,l3 %in% c("Bogotá D.C"))
train_med<-subset(train,l3 %in% c("Medellín"))
test_bog<-subset(test,l3 %in% c("Bogotá D.C"))
test_med<-subset(test,l3 %in% c("Medellín"))



##Tratamiento de la base de datos TRAIN
##Para Bogotá------------------------------------

point = geocode_OSM(paste(train$lat[1]," , ",train$lon[1]), as.sf=T) 
leaflet() %>% addTiles() %>% addCircles(data=point)
leaflet() %>% addTiles() %>% addCircles(data=train_bog)

##Se realizan las prediccion para encontrar la distancia minima a una estación de bus y supermercado
## objeto osm buses
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircleMarkers(data=train_bog, col="blue")

## objeto osm marketplace
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")

##distancia buses
dist_bus = st_distance(x=train_bog , y=bus_station)

min_dist_bus = apply(dist_bus , 1 , min)

train_bog$min_dist_bus<-min_dist_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=point , col="red" , weight=2) %>% # apartamentos
  addCircles(data=bus_station , col="black" , weight=2) # buses

##distancia marketplace
dist_market = st_distance(x=train_bog , y=marketplace)
dist_market
min_dist_market = apply(dist_market , 1 , min)
min_dist_market
train_bog$min_dist_market<-min_dist_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)%>% 
  addCircles(data=train_bog , col="black" , weight=2)# marketplace



##Para Medellín------------------------------------
##Se realizan las prediccion para encontrar la distancia minima a una estación de bus y supermercado
## objeto osm buses
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircleMarkers(data=train_med, col="blue")

## objeto osm marketplace
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")

##distancia buses
dist_bus = st_distance(x=train_med , y=bus_station)

min_dist_bus = apply(dist_bus , 1 , min)

train_med$min_dist_bus<-min_dist_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=train_med , col="red" , weight=2) %>% # apartamentos
  addCircles(data=bus_station , col="black" , weight=2) # buses

##distancia marketplace
dist_market = st_distance(x=train_med, y=marketplace)
dist_market
min_dist_market = apply(dist_market , 1 , min)
min_dist_market
train_med$min_dist_market<-min_dist_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)  # marketplace

train_final<-rbind(train_bog,train_med)  


##Tratamiento de la base de datos TEST
##Para Bogotá------------------------------------
##Se realizan las prediccion para encontrar la distancia minima a una estación de bus y supermercado
point = geocode_OSM(paste(test$lat[1]," , ",test$lon[1]), as.sf=T) 
leaflet() %>% addTiles() %>% addCircles(data=point)
leaflet() %>% addTiles() %>% addCircles(data=test_bog)

## objeto osm buses
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircleMarkers(data=test_bog, col="blue")

## objeto osm marketplace
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")

##distancia buses
dist_bus = st_distance(x=test_bog , y=bus_station)

min_dist_bus = apply(dist_bus , 1 , min)

test_bog$min_dist_bus<-min_dist_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=point , col="red" , weight=2) %>% # apartamentos
  addCircles(data=bus_station , col="black" , weight=2) # buses

##distancia marketplace
dist_market = st_distance(x=test_bog , y=marketplace)
dist_market
min_dist_market = apply(dist_market , 1 , min)
min_dist_market
test_bog$min_dist_market<-min_dist_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)  # marketplace
  
  
  
 ##Para Medellín------------------------------------
##Se realizan las prediccion para encontrar la distancia minima a una estación de bus y supermercado

## objeto osm buses
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

bus_station = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircleMarkers(data=test_med, col="blue")

## objeto osm marketplace
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="marketplace") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

marketplace = osm_sf$osm_points %>% select(osm_id,amenity) 
marketplace

leaflet() %>% addTiles() %>% addCircleMarkers(data=marketplace , col="blue")

##distancia buses
dist_bus = st_distance(x=test_med , y=bus_station)

min_dist_bus = apply(dist_bus , 1 , min)

test_med$min_dist_bus<-min_dist_bus


leaflet() %>% addTiles() %>% 
  addCircles(data=train_med , col="red" , weight=2) %>% # apartamentos
  addCircles(data=bus_station , col="black" , weight=2) # buses

##distancia marketplace
dist_market = st_distance(x=test_med, y=marketplace)
dist_market
min_dist_market = apply(dist_market , 1 , min)
min_dist_market
test_med$min_dist_market<-min_dist_market

leaflet() %>% addTiles() %>% 
  addCircles(data=marketplace , col="black" , weight=2)  # marketplace

test_final<-rbind(test_bog,test_med)  



#Solo para Chapinero 
#Se realizan las prediccion de área de los aptos en base de los vecinos cercanos por manzanas y  en la descripción

mz_bog<-st_read(here(path,"11_BOGOTA/URBANO/MGN_URB_MANZANA.shp"))


chapinero = getbb(place_name = "UPZ Chapinero, Bogota",
                  featuretype = "boundary:administrative",
                  format_out = "sf_polygon") %>% .$multipolygon

mz_chapi = mz_bog[chapinero,]
train_chapi = train_final[chapinero,]


leaflet() %>% addTiles()%>%addCircleMarkers(data=train_chapi, col="red") %>% 
  addPolygons(data=mz_chapi) 

## Prediccion de superficie a partir de la descripcion de cada vivienda

table(is.na(train_chapi$surface_total))
table(is.na(train_chapi$surface_covered))

train_chapi = train_chapi %>% mutate(new_surface=surface_total)
train_chapi = train_chapi %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))

table(is.na(train_chapi$new_surface))

train_chapi = train_chapi %>% mutate(new_desc=str_to_lower(description))


p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" ## pattern
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2" ## pattern

train_chapi$new_surface2<-NA

train_chapi = train_chapi %>% 
  mutate(new_surface2 = str_extract(string=train_chapi$new_desc , pattern= p1))
train_chapi = train_chapi %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=train_chapi$new_desc , pattern= p2),
                               new_surface2))
train_chapi$new_surface3<-NA
train_chapi = train_chapi %>% 
  mutate(new_surface3 = str_extract(string=train_chapi$new_surface2 , pattern= "[:digit:]{2,3}"))
train_chapi$new_surface3 <- as.numeric(train_chapi$new_surface3)
train_chapi = train_chapi %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

train_chapi = train_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(train_chapi$new_surface))

## Prediccion de superficie a partir de las manzanas

house_mnz = st_join(x = train_chapi,y = mz_chapi)
colnames(house_mnz)
house_mnz = house_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_mzn=median(new_surface,na.rm=T))

train_chapi = train_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,house_mnz$new_surface_mzn,new_surface))

table(is.na(house_mnz$new_surface_mzn))
table(is.na(train_chapi$new_surface))

## make buffer
house_buf = st_buffer(train_chapi,dist=20)
house_buf = st_join(house_buf,train_chapi[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
house_mnz = left_join(house_mnz,house_buf_mean,"property_id")

train_chapi = train_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,house_mnz$surface_new_3,new_surface))
table(is.na(train_chapi$new_surface))

train_chapi = train_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(train_chapi[is.na(train_chapi$new_surface)==F,]$new_surface),new_surface))
table(is.na(train_chapi$new_surface))
table(is.na(train_chapi$property_type))

##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

train_chapi$balcon<-NULL

train_chapi = train_chapi %>% 
  mutate(balcon = str_extract(string=train_chapi$new_desc , pattern= p1))
train_chapi = train_chapi %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_chapi$new_desc , pattern= p2),balcon))
train_chapi = train_chapi %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_chapi$new_desc , pattern= p3),balcon))

train_chapi = train_chapi %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))


train_chapi <- train_chapi %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)

#Solo Medellín-----------------------
#Se realizan las prediccion de área de los aptos en base de los vecinos cercanos por manzanas y  en la descripción
train_med<-subset(train_final,l3 %in% c("Medellín"))

mz_med<-st_read(here(path,"05_ANTIOQUIA/URBANO/MGN_URB_MANZANA.shp"))


poblado = getbb(place_name = "Comuna 14 - El Poblado, Medellín",
                featuretype = "boundary:administrative",
                format_out = "sf_polygon") 
sf_use_s2(FALSE)
mz_pob = mz_med[poblado,]



leaflet() %>% addTiles()%>%addCircleMarkers(data=train_med, col="red") %>% 
  addPolygons(data=poblado) 


##Train Poblado muy pocas observaciones, se realiza con todo Medellín


## Prediccion de superficie a partir de la descripcion

table(is.na(train_med$surface_total))
table(is.na(train_med$surface_covered))
train_med$new_surface<-NULL
train_med = train_med %>% mutate(new_surface=surface_total)
table(is.na(train_med$new_surface))
train_med = train_med %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))
table(is.na(train_med$new_surface))


train_med = train_med %>% mutate(new_desc=str_to_lower(description))


p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" ## pattern
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2" ## pattern

train_med$new_surface2<-NA

train_med = train_med %>% 
  mutate(new_surface2 = str_extract(string=train_med$new_desc , pattern= p1))
train_med = train_med %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=train_med$new_desc , pattern= p2),
                               new_surface2))
train_med$new_surface3<-NA

train_med = train_med %>% 
  mutate(new_surface3 = str_extract(string=train_med$new_surface2 , pattern= "[:digit:]{2,3}"))
train_med$new_surface3 <- as.numeric(train_med$new_surface3)
train_med = train_med %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

train_med = train_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(train_med$new_surface))


## make buffer
house_buf<-NULL
house_buf_mean<-NULL
house_buf = st_buffer(train_med,dist=100)
house_buf = st_join(house_buf,train_med[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
train_med = left_join(train_med,house_buf_mean,"property_id")

train_med = train_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,train_med$surface_new_3,new_surface))
table(is.na(train_med$new_surface))

train_med = train_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(train_med[is.na(train_med$new_surface)==F,]$new_surface),new_surface))
table(is.na(train_med$new_surface))
table(is.na(train_med$property_type))


##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

train_med$balcon<-NULL

train_med = train_med %>% 
  mutate(balcon = str_extract(string=train_med$new_desc , pattern= p1))
train_med = train_med %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_med$new_desc , pattern= p2),balcon))
train_med = train_med %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=train_med$new_desc , pattern= p3),balcon))

train_med = train_med %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))


train_med <- train_med %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)


##Predicciones para Test*************************

#Para Test Chapinero
#Se realizan las prediccion de área de los aptos en base de los vecinos cercanos por manzanas y  en la descripción
mz_bog<-st_read(here(path,"11_BOGOTA/URBANO/MGN_URB_MANZANA.shp"))


chapinero = getbb(place_name = "UPZ Chapinero, Bogota",
                  featuretype = "boundary:administrative",
                  format_out = "sf_polygon") %>% .$multipolygon

test_chapi<-test_bog

leaflet() %>% addTiles()%>%addCircleMarkers(data=test_chapi, col="red") %>% 
  addPolygons(data=mz_chapi) 

## Prediccion de superficie a partir de la descripcion

table(is.na(test_chapi$surface_total))
table(is.na(test_chapi$surface_covered))

test_chapi = test_chapi %>% mutate(new_surface=surface_total)
test_chapi = test_chapi %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))

table(is.na(test_chapi$new_surface))

test_chapi = test_chapi %>% mutate(new_desc=str_to_lower(description))


p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" ## pattern
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2" ## pattern

test_chapi$new_surface2<-NA

test_chapi = test_chapi %>% 
  mutate(new_surface2 = str_extract(string=test_chapi$new_desc , pattern= p1))
test_chapi = test_chapi %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=test_chapi$new_desc , pattern= p2),
                               new_surface2))
test_chapi$new_surface3<-NA
test_chapi = test_chapi %>% 
  mutate(new_surface3 = str_extract(string=test_chapi$new_surface2 , pattern= "[:digit:]{2,3}"))
test_chapi$new_surface3 <- as.numeric(test_chapi$new_surface3)
test_chapi = test_chapi %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

test_chapi = test_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(test_chapi$new_surface))

## Prediccion de superficie a partir de las manzanas

house_mnz = st_join(x = test_chapi,y = mz_chapi)
colnames(house_mnz)
house_mnz = house_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_mzn=median(new_surface,na.rm=T))

test_chapi = test_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,house_mnz$new_surface_mzn,new_surface))

table(is.na(house_mnz$new_surface_mzn))
table(is.na(test_chapi$new_surface))

## make buffer
house_buf = st_buffer(test_chapi,dist=20)
house_buf = st_join(house_buf,test_chapi[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
house_mnz = left_join(house_mnz,house_buf_mean,"property_id")

test_chapi = test_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,house_mnz$surface_new_3,new_surface))
table(is.na(test_chapi$new_surface))

test_chapi = test_chapi %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(test_chapi[is.na(test_chapi$new_surface)==F,]$new_surface),new_surface))
table(is.na(test_chapi$new_surface))
table(is.na(test_chapi$property_type))

##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

test_chapi$balcon<-NULL

test_chapi = test_chapi %>% 
  mutate(balcon = str_extract(string=test_chapi$new_desc , pattern= p1))
test_chapi = test_chapi %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=test_chapi$new_desc , pattern= p2),balcon))
test_chapi = test_chapi %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=test_chapi$new_desc , pattern= p3),balcon))

test_chapi = test_chapi %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))


test_chapi <- test_chapi %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)

#Para Test Medellin
#Se realizan las prediccion de área de los aptos en base de los vecinos cercanos por manzanas y  en la descripción

test_med<-subset(test_final,l3 %in% c("Medellín"))

mz_med<-st_read(here(path,"05_ANTIOQUIA/URBANO/MGN_URB_MANZANA.shp"))


poblado = getbb(place_name = "Comuna 14 - El Poblado, Medellín",
                featuretype = "boundary:administrative",
                format_out = "sf_polygon") 

mz_pob = mz_med[poblado,]

leaflet() %>% addTiles()%>%addCircleMarkers(data=test_med, col="red") %>% 
  addPolygons(data=poblado) 


## Prediccion de superficie a partir de la descripcion

table(is.na(test_med$surface_total))
table(is.na(test_med$surface_covered))
test_med$new_surface<-NULL
test_med = test_med %>% mutate(new_surface=surface_total)
table(is.na(test_med$new_surface))
test_med = test_med %>% mutate(new_surface=ifelse(is.na(new_surface)==T,surface_covered,new_surface))
table(is.na(test_med$new_surface))


test_med = test_med %>% mutate(new_desc=str_to_lower(description))


p1 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m" ## pattern
p2 = "[:space:]+[:digit:]{2,3}+[:punct:]+[:digit:]{1,2}+[:space:]+m2" ## pattern

test_med$new_surface2<-NA

test_med = test_med %>% 
  mutate(new_surface2 = str_extract(string=test_med$new_desc , pattern= p1))
test_med = test_med %>% 
  mutate(new_surface2 = ifelse(is.na(new_surface2)==T,
                               str_extract(string=test_med$new_desc , pattern= p2),
                               new_surface2))
test_med$new_surface3<-NA

test_med = test_med %>% 
  mutate(new_surface3 = str_extract(string=test_med$new_surface2 , pattern= "[:digit:]{2,3}"))
test_med$new_surface3 <- as.numeric(test_med$new_surface3)
test_med = test_med %>% mutate(new_surface3 = ifelse(new_surface3<=40,NA,new_surface3))

test_med = test_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,new_surface3,new_surface))
table(is.na(test_med$new_surface))


## make buffer
house_buf<-NULL
house_buf_mean<-NULL
house_buf = st_buffer(test_med,dist=100)
house_buf = st_join(house_buf,test_med[,"new_surface"])
st_geometry(house_buf) = NULL
house_buf_mean = house_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(new_surface.y,na.rm=T))
test_med = left_join(test_med,house_buf_mean,"property_id")

test_med = test_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,test_med$surface_new_3,new_surface))
table(is.na(test_med$new_surface))

test_med = test_med %>% mutate(new_surface = ifelse(is.na(new_surface)==T,mean(test_med[is.na(test_med$new_surface)==F,]$new_surface),new_surface))
table(is.na(test_med$new_surface))
table(is.na(test_med$property_type))


##Prediccion si tiene balcon / terraza
p1 = "balcon" ## pattern
p2 = "balcón" ## pattern
p3 = "terraza" ## pattern

test_med$balcon<-NULL

test_med = test_med %>% 
  mutate(balcon = str_extract(string=test_med$new_desc , pattern= p1))
test_med = test_med %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=test_med$new_desc , pattern= p2),balcon))
test_med = test_med %>% 
  mutate(balcon = ifelse(is.na(balcon)==T,
                         str_extract(string=test_med$new_desc , pattern= p3),balcon))

test_med = test_med %>% mutate(balcon_terr = ifelse(is.na(balcon)==T, 0 ,1))



##Tratamiento final de datos
test_med <- test_med %>%
  mutate_at(.vars = c(
    "balcon_terr"),
    .funs = factor)

table(is.na(test_chapi$new_surface))
table(is.na(test_chapi$bedrooms))
table(is.na(test_chapi$min_dist_bus))
table(is.na(test_chapi$min_dist_market))
table(is.na(test_chapi$property_type))
table(is.na(train_chapi$new_surface))
table(is.na(train_chapi$bedrooms))
table(is.na(train_chapi$min_dist_bus))
table(is.na(train_chapi$min_dist_market))
table(is.na(train_chapi$property_type))
table(is.na(train_chapi$price))
table(is.na(test_med$new_surface))
table(is.na(test_med$bedrooms))
table(is.na(test_med$min_dist_bus))
table(is.na(test_med$min_dist_market))
table(is.na(test_med$property_type))
table(is.na(train_med$new_surface))
table(is.na(train_med$bedrooms))
table(is.na(train_med$min_dist_bus))
table(is.na(train_med$min_dist_market))
table(is.na(train_med$property_type))
table(is.na(train_med$price))

save(test_chapi,train_chapi,test_med,train_med, file = "data_pred.RData")
load("stuff.RData")

train_chapi$chapi<-NULL
train_chapi$med<-NULL
train_med$chapi<-NULL
train_med$med<-NULL
train_med$surface_new_3<-NULL
train<-rbind(train_chapi,train_med)
test_med$surface_new_3<-NULL
test<-rbind(test_chapi,test_med)

