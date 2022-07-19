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
       here) # Get OSM's data
path = here('')

train<-readRDS(here(path,"/train.Rds"))
test<-readRDS(here(path,"/test.Rds"))

#Limpieza de datos

train <- train %>%
  mutate_at(.vars = c(
    "property_type","operation_type"),
    .funs = factor)

test <- test %>%
  mutate_at(.vars = c(
    "property_type","operation_type"),
    .funs = factor)


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


#Solo Chapinero

cha.poly<-st_read(here(path,"localidades.shp"))
class(cha.poly)
st_geometry(cha.poly[13,])

st_crs(cha.poly[13,])
cha.poly[13,]%>% head()
cha.poly<-st_transform(cha.poly,"WGS84") 
st_crs(cha.poly)
st_crs(cha.poly[13,])
cha.poly[13,]%>% head()
leaflet() %>% addTiles() %>% 
  addPolygons(data=cha.poly[13,])

lst = st_intersects (train_final,cha.poly[13,])
land = lengths(st_intersects(train_final,cha.poly[13,])) > 0
land=as.data.frame(land)
train_final$chapi<-NULL
train_final$chapi<-land[,1]

train_chap<-filter(train_final,chapi==TRUE)

leaflet() %>% addTiles()  %>% 
  addCircleMarkers(data=train_chap, col="blue")%>% 
  addPolygons(data=cha.poly[13,])

#Solo Medellín

med.poly<-st_read(here(path,"LimiteComunaCorregimiento_2014.shp"))
med.poly<-st_transform(med.poly,"WGS84") 
pol_poblad <-st_cast(med.poly[14,], "POLYGON")
class(pol_poblad)
st_geometry(pol_poblad)
st_crs(pol_poblad)
pol_poblad%>% head()

leaflet() %>% addTiles() %>% 
  addPolygons(data=pol_poblad)

lst = st_intersects (train_final,pol_poblad)
land = lengths(st_intersects(train_final,pol_poblad)) > 0
land=as.data.frame(land)
train_final$med<-NULL
train_final$med<-land[,1]

train_med<-filter(train_final,med==TRUE)

leaflet() %>% addTiles()  %>% 
  addCircleMarkers(data=train_final, col="blue")%>% 
  addPolygons(data=med.poly[14,])
