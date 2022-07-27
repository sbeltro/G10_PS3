################################################################################
# Problem Set 3: Making Money with ML?
# Autores: Natalia Capacho, Yurani Gonzalez, Sebastian Beltran
# Big Data - MECA 
################################################################################

# Limpiar el espacio de trabajo ----
rm(list=ls())

# Instalar Paquetes y cargar librerias ----
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse,
       skimr,
       leaps,
       caret,
       ranger,
       xgboost,
       nnls,
       SuperLearner,
       sf,
       leaflet,
       class,
       ggplot2,
       dplyr,
       spdep,
       stringr,
       rio,
       lwgeom,
       writexl,
       tmaptools,
       osmdata,
       randomForest,
       glmnet,
       ggspatial,
       ggsave,
       tableone)

# Datos ----
# * Train ---- 

# Cargar la base de datos
train <- readRDS("stores/train.rds")

# Convertir la base a clase sf con proyeccion 4326
train <- st_as_sf(train, 
                  coords = c("lon", "lat"),
                  crs = 4326)

# Verificar la clase
class(train)

# Limpieza de la base ----
# * Variable dummy de ciudad ----
train <- train %>% 
  mutate(bogota = ifelse(l3 == "Bogotá D.C", 
                         yes = 1,
                         no = 0))

# * Variable de superficie de vivienda en metros cuadrados ----
# 1. Imputar valores 
train <- train %>% 
  mutate(surface = ifelse(is.na(surface_total) == T, surface_covered, surface_total),
         surface = ifelse(is.na(surface) == T, pmax(surface_total, surface_covered), surface))

# 2. Extraer informacion de descripcion y titulo 
train <- train %>% 
  mutate(title = str_to_lower(string = title),
         description2 = str_to_lower(string = description),
         title = str_replace_all(string = title , pattern = paste0(":","|",",",".") , replacement = ""),
         description3 = str_replace_all(string = description2 , pattern = paste0(":","|",",",".") , replacement = ""),
         title_2 = str_replace_all(string = title , pattern = paste0("mt","|","metr","|","mts","|","m2","|","m²","|","mâ²","|","m ", "|", "metrs2", "|", "msts2", "|", "m^2") , replacement = "@"),
         description_2 = str_replace_all(string = description3, pattern = paste0("mt","|","metr","|","mts","|","m2","|","m²","mâ²","|","m ", "|", "metrs2", "|", "msts2", "|", "m^2") , replacement = "@"))

# Crear patrones
p1 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+@" 
p2 = "[:space:]+[:digit:]+[:space:]+@"
p3 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+@"
p4 = "[:space:]+[:digit:]+@"
p5 = "[:digit:]+[:punct:]+[:digit:]+[:space:]+@"
p6 = "[:digit:]+[:space:]+@"
p7 = "[:digit:]+[:punct:]+[:digit:]+@"
p8 = "[:digit:]+@"
p9 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m²" 
p10 = "[:space:]+[:digit:]+[:space:]+m²"
p11 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+m²"
p12 = "[:space:]+[:digit:]+m²"
p13 = "[:digit:]+[:punct:]+[:digit:]+[:space:]+m²"
p14 = "[:digit:]+[:space:]+m²"
p15 = "[:digit:]+[:punct:]+[:digit:]+m²"
p16 = "[:digit:]+m²"
p17 = "área+[:space]+[:digit:]"

# Crear variables
train <- train %>% 
  mutate(new_surface_desc = str_extract(string = train$description_2 , 
                                        pattern = paste0(p1,"|",p2,"|",p3,"|",p4,"|",
                                                         p5,"|",p6,"|",p7,"|",p8,"|",
                                                         p9,"|",p10,"|",p11,"|",p12,"|",
                                                         p13,"|",p14,"|",p15,"|",p16,"|",
                                                         p17)))
train <- train %>% 
  mutate(new_surface_tit = str_extract(string = train$title_2 , 
                                       pattern = paste0(p1,"|",p2,"|",p3,"|",p4,"|",
                                                        p5,"|",p6,"|",p7,"|",p8,"|",
                                                        p9,"|",p10,"|",p11,"|",p12,"|",
                                                        p13,"|",p14,"|",p15,"|",p16,"|",
                                                        p17)))

train <- train %>% 
  mutate(new_surface_desc = str_replace(new_surface_desc, "@|m²",""),
         new_surface_tit = str_replace(new_surface_tit, "@|m²",""))

train <- train %>% 
  mutate(new_surface_desc_e = sub("\\..*", "", new_surface_desc),
         new_surface_desc_d = ifelse(str_detect(string = new_surface_desc , 
                                                pattern = "[:punct:]"), sub(".*\\.", "", new_surface_desc), NA),
         new_surface_tit_e = sub("\\..*", "", new_surface_tit),
         new_surface_tit_d = ifelse(str_detect(string = new_surface_tit , 
                                               pattern = "[:punct:]"), sub(".*\\.", "", new_surface_tit), NA))

train <- train %>% 
  mutate(new_surface_desc_e = as.numeric(new_surface_desc_e),
         new_surface_tit_e = as.numeric(new_surface_tit_e),
         new_surface = pmax(new_surface_desc_e, new_surface_tit_e),
         new_surface = ifelse(is.na(new_surface_desc_e) == T, new_surface_tit_e, new_surface_desc_e),
         surface = ifelse(is.na(surface) == T, new_surface, surface),
         surface = as.numeric(surface))

train <- train %>% 
  mutate(surface = ifelse(surface < 10 | surface > 7000, yes = NA, no = surface))

# * Variable de baños ----
# 1. Extraer informacion de descripcion 
# Definir patrones
pi = "[:digit:]+[:space:]+baño" 
pii = "[:digit:]+baño" 
piii = "[:digit:]+[:space:]+bao" 
piv = "[:digit:]+bao" 
pv = "un+[:space:]+baño" 
pvi = "dos+[:space:]+baño" 
pvii = "tres+[:space:]+baño" 
pviii = "cuatro+[:space:]+baño" 
pix = "cinco+[:space:]+baño" 
px = "un+[:space:]+baño+[:space:]+y+[:space:]+un+[:space:]+baño" 

# Crear variables
train <- train %>% 
  mutate(bathrooms_des = str_extract(string = train$description2, 
                                     pattern = paste0(pi, "|", pii, "|", piii, "|", piv, "|",
                                                      pv, "|", pvi, "|", pvii, "|", pviii, "|", 
                                                      pix, "|", px)))
train <- train %>% 
  mutate(bathrooms_des = str_replace(string = train$bathrooms_des, 
                                     pattern = "un", 
                                     replacement = "1"))
train <- train %>% 
  mutate(bathrooms_des = str_replace(string = train$bathrooms_des, 
                                     pattern = "dos", 
                                     replacement = "2"))
train <- train %>% 
  mutate(bathrooms_des = str_replace(string = train$bathrooms_des, 
                                     pattern = "tres", 
                                     replacement = "3"))
train <- train %>% 
  mutate(bathrooms_des = str_replace(string = train$bathrooms_des, 
                                     pattern = "cuatro", 
                                     replacement = "4"))
train <- train %>% 
  mutate(bathrooms_des = str_replace(string = train$bathrooms_des, 
                                     pattern = "quinto", 
                                     replacement = "5"))

pxi = "[1-9]"

train <- train %>% 
  mutate(bathrooms_des2 = str_extract(string = train$bathrooms_des, 
                                      pattern = pxi))

train <- train %>% 
  mutate(bathrooms_f = ifelse(is.na(bathrooms) == T,
                              yes = bathrooms_des2,
                              no = bathrooms))

table(is.na(train$bathrooms)) # teniamos 30074 missings values
table(is.na(train$bathrooms_f)) # ahora tenemos 15123 missing values

train <- train %>% 
  mutate(bathrooms_f = as.numeric(bathrooms_f))

# Generar nuevas variables a partir de la descripcion ----
# * Variable de Parqueadero ---- 
# Definir patrones
pa = "[:space:]+parqueadero" 
pb = "[:space:]+gara" 
pc = "[:space:]+parqueo" 
pd = "[:punct:]+parqueadero" 
pe = "[:punct:]+gara" 
pf = "[:punct:]+parqueo" 

# Crear variable 
train <- train %>% 
  mutate(parqueadero = str_extract(string = train$description2, 
                                   pattern = paste0(pa, "|", pb, "|", pc, "|", pd, "|", 
                                                    pe, "|", pf)))

train <- train %>% 
  mutate(parqueadero = str_replace_all(string = train$parqueadero, 
                                       pattern = paste0(" ", "|", "[:space:]", "|", 
                                                        "[:punct:]", "|", "[:blank:]"), 
                                       repl=""))

table(is.na(train$parqueadero)) #tenemos 36893 missings values

# Imputar valores faltantes
train <- train %>% 
  mutate(parqueadero = ifelse(is.na(parqueadero) == T, 
                              yes = 0,
                              no = 1))

# * Variable de Terraza/balcon/patio/jardin ---- 
# Definir patrones
pa1 = "[:space:]+terraza" 
pb1 = "[:space:]+balc" 
pc1 = "[:space:]+patio(?![:space:]+bonito)" 
pd1 = "[:space:]+patio(?![:space:]+de+[:space:]+ropa)" 
pe1 = "[:space:]+jard" 
pf1 = "[:punct:]+terraza" 
pg1 = "[:punct:]+balc" 
ph1 = "[:punct:]+patio(?![:space:]+bonito)" 
pi1 = "[:punct:]+patio(?![:space:]+de+[:space:]+ropa)" 
pj1 = "[:punct:]+jard" 
pk1 = "terraza" 
pl1 = "balc" 
pm1 = "patio(?![:space:]+bonito)" 
pn1 = "patio(?![:space:]+de+[:space:]+ropa)" 
po1 = "jard" 

# Crear variable 
train <- train %>% 
  mutate(terrazaPatio = str_extract(string = train$description2, 
                                    pattern = paste0(pa1, "|", pb1, "|", pc1, "|", pd1, "|", 
                                                     pe1, "|", pf1, "|", pg1, "|", ph1, "|", 
                                                     pi1, "|", pj1, "|", pk1, "|", pl1, "|",
                                                     pm1, "|", pn1, "|", po1)))

train <- train %>% 
  mutate(terrazaPatio = str_replace_all(string = train$terrazaPatio, 
                                        pattern=paste0(" ", "|", "[:space:]", "|", "[:punct:]", "|", "[:blank:]"), 
                                        repl=""))

table(is.na(train$terrazaPatio)) #tenemos 48427 missing values

# Imputar valores faltantes
train <- train %>% 
  mutate(terrazaPatio = ifelse(is.na(terrazaPatio) == T, 
                               yes = 0,
                               no = 1))

# * Imputar valores faltantes con informacion de las manzanas del DANE ----
sf_use_s2(FALSE)

mnz_bog <- readRDS("stores/mnz_bog.rds")
mnz_bog_f <- subset(mnz_bog, select = c("MANZ_CCNCT","geometry"))
colnames(mnz_bog_f)[1] <- c("MANZ_CCNCT_b")

mnz_med <- readRDS("stores/mnz_med.rds")
mnz_med_f <- subset(mnz_med, select = c("MANZ_CCNCT","geometry"))
colnames(mnz_med_f)[1] <- c("MANZ_CCNCT_m")

train_bog = st_join(x = train, y = mnz_bog_f)
train_bog_med = st_join(x = train_bog, y = mnz_med_f)

train_bog_med <- train_bog_med %>% 
  mutate(MANZ_CCNCT = ifelse(l3 == "Bogotá D.C", 
                             yes = MANZ_CCNCT_b,
                             no = MANZ_CCNCT_m))

train_bog_med = train_bog_med %>%
  group_by(MANZ_CCNCT) %>%
  mutate(surface_2 = median(surface, na.rm = T),
         bathrooms_2 = median(bathrooms_f, na.rm = T))

base <- as_tibble(train_bog_med)

# Imputar valores faltantes
base <- base %>% 
  mutate(surface_f2 = ifelse(is.na(surface) == T, 
                             yes = surface_2,
                             no = surface),
         bathrooms_f2 = ifelse(is.na(bathrooms_f) == T, 
                               yes = bathrooms_2,
                               no = bathrooms_f))

# Base final (Version 1) 
table(is.na(base$surface_f2))
table(is.na(base$bathrooms_f2))

# Imputar valores faltantes y obtener base final preliminar 
train_final <- base %>% 
  mutate(surface_final = ifelse(is.na(surface_f2) == T, 
                                yes = median(surface_f2, na.rm = TRUE),
                                no = surface_f2),
         bathrooms_final = ifelse(is.na(bathrooms_f2) == T, 
                                  yes = median(bathrooms_f2, na.rm = TRUE),
                                  no = bathrooms_f2))

table(is.na(train_final$surface_final))
table(is.na(train_final$bathrooms_final))

train_final <- st_as_sf(train_final,
                        crs = 4326)

# Generar nuevas variables de Open Street Map ---- 
# * Variable de Universidades ----
#  1.1 Bogota 
# Crear un objeto de OSM que contega las universidades (incluyendo institutos tecnicos) dentro del poligono de Bogota 
osm_unibog = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "university")

# Extraer del objeto los features de universidades (id, nombre y amenity) y guardar los poligonos en nuevo objeto
osm_unibog_sf = osm_unibog %>% osmdata_sf()
uni_bog = osm_unibog_sf$osm_polygons  %>% select(osm_id, name, amenity)

#Visualizar un mapa que muestre todas las universidades
leaflet() %>% addTiles() %>% addPolygons(data = uni_bog, col = "blue")

# Verificar que las proyecciones sean iguales
st_crs(uni_bog) == st_crs(train_final)

# Medir la distancia entre hogares y  universidades 
dis_unibog <- st_distance(x = train_final, y = uni_bog)
head(dis_unibog)

# Encontrar la minima distancia entre cada hogar y una universidad (universidad mas cercana)
min_unibog = apply(dis_unibog, 1, min)

# Agregar la variable de distancia minima a la base de train
train_final <- train_final %>% 
  mutate(mind_unibog = min_unibog)

#  1.2 Medellin
# Crear un objeto de OSM que contega las universidades (incluyendo institutos tecnicos) dentro del poligono de Medellin 
osm_unimed = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity", value = "university")

# Extraer del objeto los features de universidades (id, nombre y amenity) y guardar los poligonos en nuevo objeto
osm_unimed_sf = osm_unimed %>% osmdata_sf()
uni_med = osm_unimed_sf$osm_polygons  %>% select(osm_id, name, amenity)

# Visualizar un mapa que muestre todas las universidades
leaflet() %>% addTiles() %>% addPolygons(data = uni_med, col="blue")

# Verificar que las proyecciones sean iguales
st_crs(uni_med) == st_crs(train_final)

# Medir la distancia entre hogares y universidades
dis_unimed <- st_distance(x = train_final,y = uni_med)
tail(dis_unimed)

# Encontrar la minima distancia entre cada hogar y una universidad (universidad mas cercana)
min_unimed = apply(dis_unimed, 1, min)

#Agregar la variable de distancia minima a la base de train
train_final <- train_final %>% 
  mutate(mind_unimed = min_unimed)

#  1.3 Bogota y Medellin 
# Analizar cuantos hogares pertenecen a Bogota y cuantos a Medellin
table(train_final$l3)

train_final <- train_final %>% 
  mutate(dismin_uni = ifelse(l3 == "Bogotá D.C", 
                             yes = mind_unibog,
                             no = mind_unimed))

# * Variable de Centros comerciales (CC) ---- 
#  2.1 Bogota
# Crear un objeto de OSM que contega los centros comerciales dentro del poligono de Bogota
osm_ccbog = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "shop", value = "mall")

# Extraer del objeto los features de centros comerciales (id, nombre y shop) y guardar los poligonos en nuevo objeto
osm_ccbog_sf = osm_ccbog %>% osmdata_sf()
cc_bog = osm_ccbog_sf$osm_polygons  %>% select(osm_id, name, shop)

# Visualizar un mapa que muestre todos los centros comerciales
leaflet() %>% addTiles() %>% addPolygons(data = cc_bog, col = "black")

# Verificar que las proyecciones sean iguales
st_crs(cc_bog) == st_crs(train_final)

# Medir la distancia entre hogares y centros comerciales
dis_ccbog <- st_distance(x = train_final, y = cc_bog)
head(dis_ccbog)

# Encontrar la minima distancia entre cada hogar y un centro comercial (CC mas cercano)
min_ccbog = apply(dis_ccbog, 1, min)

# Agregar la variable de distancia minima a la base de train
train_final <- train_final %>%
  mutate(mind_ccbog = min_ccbog)

#  2.2 Medellin 
# Crear un objeto de OSM que contega los centros comerciales dentro del polígono de Medellín 
osm_ccmed = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key = "shop", value = "mall")

# Extraer del objeto los features de centros comerciales (id, nombre y shop) y guardar los poligonos en nuevo objeto
osm_ccmed_sf = osm_ccmed %>% osmdata_sf()
cc_med = osm_ccmed_sf$osm_polygons  %>% select(osm_id, name, shop)

# Visualizar un mapa que muestre todos los centros comerciales
leaflet() %>% addTiles() %>% addPolygons(data = cc_med, col = "black")

# Verificar que las proyecciones sean iguales
st_crs(cc_med) == st_crs(train_final)

# Medir la distancia entre hogares y centros comerciales 
dis_ccmed <- st_distance(x = train_final, y = cc_med)
tail(dis_ccmed)

# Encontrar la minima distancia entre cada hogar y un centro comercial (CC mas cercano)
min_ccmed = apply(dis_ccmed, 1, min)

# Agregar la variable de distancia minima a la base de train
train_final <- train_final %>%
  mutate(mind_ccmed = min_ccmed)

#  2.3 Bogota y Medellin
train_final <- train_final %>% 
  mutate(dismin_cc = ifelse(l3 == "Bogotá D.C", 
                            yes = mind_ccbog,
                            no = mind_ccmed))

# Base final train ----
colnames(train_final)[which(colnames(train_final)=="price")] = "precio"
colnames(train_final)[which(colnames(train_final)=="bedrooms")] = "habitaciones"
colnames(train_final)[which(colnames(train_final)=="bathrooms_final")] = "baños"
colnames(train_final)[which(colnames(train_final)=="surface_final")] = "superficie"
colnames(train_final)[which(colnames(train_final)=="dismin_uni")] = "universidad"
colnames(train_final)[which(colnames(train_final)=="dismin_cc")] = "centroComercial"
colnames(train_final)[which(colnames(train_final)=="parqueadero")] = "parqueadero"
colnames(train_final)[which(colnames(train_final)=="terrazaPatio")] = "terrazaPatio"

variables_categoricas <- c("bogota", "parqueadero", "terrazaPatio")

train_final <- as.data.frame(train_final)
for (i in variables_categoricas){
  train_final[,i] = as.numeric(train_final[,i])
  train_final[,i] = as.logical(train_final[,i])
}

variables_numericas <- c("precio", "baños", "habitaciones", "superficie",
                         "universidad", "centroComercial")

train_final <- as.data.frame(train_final)
for (j in variables_numericas){
  train_final[,j] = as.numeric(train_final[,j])
}

saveRDS(train_final, "stores/train_final.rds")



# * Test ----

# Cargar la base de datos
test <- readRDS("stores/test.rds")

## Convertir la base a clase sf
test <- st_as_sf(test, 
                 coords = c("lon", "lat"),
                 crs = 4326)

#Verificar que la clase 
class(test)

# Limpieza de la base ----
# * Variable dummy de ciudad ----
test <- test %>% 
  mutate(bogota = ifelse(l3 == "Bogotá D.C", 
                         yes = 1,
                         no = 0))

# * Variable de superficie de vivienda en metros cuadrados ----
# 1. Imputar valores 
test <- test %>% 
  mutate(surface = ifelse(is.na(surface_total) == T, surface_covered, surface_total),
         surface = ifelse(is.na(surface) == T, pmax(surface_total, surface_covered), surface))

# 2. Extraer informacion de descripcion y titulo 
test <- test %>% 
  mutate(title = str_to_lower(string = title),
         description2 = str_to_lower(string = description),
         title = str_replace_all(string = title , pattern = paste0(":","|",",",".") , replacement = ""),
         description3 = str_replace_all(string = description2 , pattern = paste0(":","|",",",".") , replacement = ""),
         title_2 = str_replace_all(string = title , pattern = paste0("mt","|","metr","|","mts","|","m2","|","m²","|","mâ²","|","m ", "|", "metrs2", "|", "msts2", "|", "m^2") , replacement = "@"),
         description_2 = str_replace_all(string = description3, pattern = paste0("mt","|","metr","|","mts","|","m2","|","m²","mâ²","|","m ", "|", "metrs2", "|", "msts2", "|", "m^2") , replacement = "@"))

# Crear patrones
p1 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+@" 
p2 = "[:space:]+[:digit:]+[:space:]+@"
p3 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+@"
p4 = "[:space:]+[:digit:]+@"
p5 = "[:digit:]+[:punct:]+[:digit:]+[:space:]+@"
p6 = "[:digit:]+[:space:]+@"
p7 = "[:digit:]+[:punct:]+[:digit:]+@"
p8 = "[:digit:]+@"
p9 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m²" 
p10 = "[:space:]+[:digit:]+[:space:]+m²"
p11 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+m²"
p12 = "[:space:]+[:digit:]+m²"
p13 = "[:digit:]+[:punct:]+[:digit:]+[:space:]+m²"
p14 = "[:digit:]+[:space:]+m²"
p15 = "[:digit:]+[:punct:]+[:digit:]+m²"
p16 = "[:digit:]+m²"
p17 = "área+[:space]+[:digit:]"

# Crear variables
test <- test %>% 
  mutate(new_surface_desc = str_extract(string = test$description_2 , 
                                        pattern = paste0(p1,"|",p2,"|",p3,"|",p4,"|",
                                                         p5,"|",p6,"|",p7,"|",p8,"|",
                                                         p9,"|",p10,"|",p11,"|",p12,"|",
                                                         p13,"|",p14,"|",p15,"|",p16,"|",
                                                         p17)))
test <- test %>% 
  mutate(new_surface_tit = str_extract(string = test$title_2 , 
                                       pattern = paste0(p1,"|",p2,"|",p3,"|",p4,"|",
                                                        p5,"|",p6,"|",p7,"|",p8,"|",
                                                        p9,"|",p10,"|",p11,"|",p12,"|",
                                                        p13,"|",p14,"|",p15,"|",p16,"|",
                                                        p17)))

test <- test %>% 
  mutate(new_surface_desc = str_replace(new_surface_desc, "@|m²",""),
         new_surface_tit = str_replace(new_surface_tit, "@|m²",""))

test <- test %>% 
  mutate(new_surface_desc_e = sub("\\..*", "", new_surface_desc),
         new_surface_desc_d = ifelse(str_detect(string = new_surface_desc , 
                                                pattern = "[:punct:]"), sub(".*\\.", "", new_surface_desc), NA),
         new_surface_tit_e = sub("\\..*", "", new_surface_tit),
         new_surface_tit_d = ifelse(str_detect(string = new_surface_tit , 
                                               pattern = "[:punct:]"), sub(".*\\.", "", new_surface_tit), NA))

test <- test %>% 
  mutate(new_surface_desc_e = as.numeric(new_surface_desc_e),
         new_surface_tit_e = as.numeric(new_surface_tit_e),
         new_surface = pmax(new_surface_desc_e, new_surface_tit_e),
         new_surface = ifelse(is.na(new_surface_desc_e) == T, new_surface_tit_e, new_surface_desc_e),
         surface = ifelse(is.na(surface) == T, new_surface, surface),
         surface = as.numeric(surface))

test <- test %>% 
  mutate(surface = ifelse(surface < 10 | surface > 7000, yes = NA, no = surface))

# * Variable de baños ----
# 1. Extraer informacion de descripcion 
# Definir patrones
pi = "[:digit:]+[:space:]+baño" 
pii = "[:digit:]+baño" 
piii = "[:digit:]+[:space:]+bao" 
piv = "[:digit:]+bao" 
pv = "un+[:space:]+baño" 
pvi = "dos+[:space:]+baño" 
pvii = "tres+[:space:]+baño" 
pviii = "cuatro+[:space:]+baño" 
pix = "cinco+[:space:]+baño" 
px = "un+[:space:]+baño+[:space:]+y+[:space:]+un+[:space:]+baño" 

# Crear variables
test <- test %>% 
  mutate(bathrooms_des = str_extract(string = test$description2, 
                                     pattern = paste0(pi, "|", pii, "|", piii, "|", piv, "|",
                                                      pv, "|", pvi, "|", pvii, "|", pviii, "|", 
                                                      pix, "|", px)))
test <- test %>% 
  mutate(bathrooms_des = str_replace(string = test$bathrooms_des, 
                                     pattern = "un", 
                                     replacement = "1"))
test <- test %>% 
  mutate(bathrooms_des = str_replace(string = test$bathrooms_des, 
                                     pattern = "dos", 
                                     replacement = "2"))
test <- test %>% 
  mutate(bathrooms_des = str_replace(string = test$bathrooms_des, 
                                     pattern = "tres", 
                                     replacement = "3"))
test <- test %>% 
  mutate(bathrooms_des = str_replace(string = test$bathrooms_des, 
                                     pattern = "cuatro", 
                                     replacement = "4"))
test <- test %>% 
  mutate(bathrooms_des = str_replace(string = test$bathrooms_des, 
                                     pattern = "quinto", 
                                     replacement = "5"))

pxi = "[1-9]"

test <- test %>% 
  mutate(bathrooms_des2 = str_extract(string = test$bathrooms_des, 
                                      pattern = pxi))

test <- test %>% 
  mutate(bathrooms_f = ifelse(is.na(bathrooms) == T,
                              yes = bathrooms_des2,
                              no = bathrooms))

table(is.na(test$bathrooms)) # teniamos 4269 missings values
table(is.na(test$bathrooms_f)) # ahora tenemos 2557 missing values

test <- test %>% 
  mutate(bathrooms_f = as.numeric(bathrooms_f))

# Generar nuevas variables a partir de la descripcion ----
# * Variable de Parqueadero ---- 
# Definir patrones
pa = "[:space:]+parqueadero" 
pb = "[:space:]+gara" 
pc = "[:space:]+parqueo" 
pd = "[:punct:]+parqueadero" 
pe = "[:punct:]+gara" 
pf = "[:punct:]+parqueo" 

# Crear variable 
test <- test %>% 
  mutate(parqueadero = str_extract(string = test$description2, 
                                   pattern = paste0(pa, "|", pb, "|", pc, "|", pd, "|", pe, "|", pf)))

test <- test %>% 
  mutate(parqueadero = str_replace_all(string = test$parqueadero, 
                                       pattern=paste0(" ", "|", "[:space:]", "|", "[:punct:]", "|", "[:blank:]"), 
                                       repl=""))

table(is.na(test$parqueadero)) 

# Imputar valores faltantes
test <- test %>% 
  mutate(parqueadero = ifelse(is.na(parqueadero) == T, 
                              yes = 0,
                              no = 1))

# * Variable de Terraza/balcon/patio/jardin ---- 
# Definir patrones
pa1 = "[:space:]+terraza" 
pb1 = "[:space:]+balc" 
pc1 = "[:space:]+patio(?![:space:]+bonito)" 
pd1 = "[:space:]+patio(?![:space:]+de+[:space:]+ropa)" 
pe1 = "[:space:]+jard" 
pf1 = "[:punct:]+terraza" 
pg1 = "[:punct:]+balc" 
ph1 = "[:punct:]+patio(?![:space:]+bonito)" 
pi1 = "[:punct:]+patio(?![:space:]+de+[:space:]+ropa)" 
pj1 = "[:punct:]+jard" 
pk1 = "terraza" 
pl1 = "balc" 
pm1 = "patio(?![:space:]+bonito)" 
pn1 = "patio(?![:space:]+de+[:space:]+ropa)" 
po1 = "jard" 

# Crear variable 
test <- test %>% 
  mutate(terrazaPatio = str_extract(string = test$description2, 
                                    pattern = paste0(pa1, "|", pb1, "|", pc1, "|", pd1, "|", 
                                                     pe1, "|", pf1, "|", pg1, "|", ph1, "|", 
                                                     pi1, "|", pj1, "|", pk1, "|", pl1, "|",
                                                     pm1, "|", pn1, "|", po1)))

test <- test %>% 
  mutate(terrazaPatio = str_replace_all(string = test$terrazaPatio, 
                                        pattern=paste0(" ", "|", "[:space:]", "|", "[:punct:]", "|", "[:blank:]"), 
                                        repl=""))

table(is.na(test$terrazaPatio))

# Imputar valores faltantes
test <- test %>% 
  mutate(terrazaPatio = ifelse(is.na(terrazaPatio) == T, 
                               yes = 0,
                               no = 1))

# * Imputar valores faltantes con informacion de las manzanas del DANE ----
sf_use_s2(FALSE)

test_bog = st_join(x = test, y = mnz_bog_f)
test_bog_med = st_join(x = test_bog, y = mnz_med_f)

test_bog_med <- test_bog_med %>% 
  mutate(MANZ_CCNCT = ifelse(l3 == "Bogotá D.C", 
                             yes = MANZ_CCNCT_b,
                             no = MANZ_CCNCT_m))

test_bog_med = test_bog_med %>%
  group_by(MANZ_CCNCT) %>%
  mutate(surface_2 = median(surface, na.rm = T),
         bathrooms_2 = median(bathrooms_f, na.rm = T))

base_test <- as_tibble(test_bog_med)

# Imputar valores faltantes
base_test <- base_test %>% 
  mutate(surface_f2 = ifelse(is.na(surface) == T, 
                             yes = surface_2,
                             no = surface),
         bathrooms_f2 = ifelse(is.na(bathrooms_f) == T, 
                               yes = bathrooms_2,
                               no = bathrooms_f))

# Base final (Version 1) 
table(is.na(base_test$surface_f2))
table(is.na(base_test$bathrooms_f2))

# Imputar valores faltantes y obtener base final 
test_final <- base_test %>% 
  mutate(surface_final = ifelse(is.na(surface_f2) == T, 
                                yes = median(surface_f2, na.rm = TRUE),
                                no = surface_f2),
         bathrooms_final = ifelse(is.na(bathrooms_f2) == T, 
                                  yes = median(bathrooms_f2, na.rm = TRUE),
                                  no = bathrooms_f2))

table(is.na(test_final$surface_final))
table(is.na(test_final$bathrooms_final))

test_final <- st_as_sf(test_final,
                       crs = 4326)


# Generar nuevas variables de Open Street Map ---- 
# * Variable de Universidades ----
#  1.1 Bogota 
# Crear un objeto de OSM que contega las universidades (incluyendo institutos tecnicos) dentro del poligono de Chapinero 
osm_unibogt = opq(bbox = getbb("UPZs Localidad Chapinero")) %>%
  add_osm_feature(key = "amenity", value = "university")

# Extraer del objeto los features de universidades (id, nombre y amenity) y guardar los poligonos en nuevo objeto
osm_unibogt_sf = osm_unibogt %>% osmdata_sf()
uni_bogt = osm_unibogt_sf$osm_polygons %>% select(osm_id, name, amenity)

# Eliminar universidades fuera del poligono
uni_bogt = uni_bogt[-c(5,6,18,19,20,21,23,26,27,34),]

# Visualizar un mapa que muestre todas las universidades
leaflet() %>% addTiles() %>% addPolygons(data = uni_bogt, col = "blue")

# Verificar que las proyecciones sean iguales
st_crs(uni_bogt) == st_crs(test_final)

# Medir la distancia entre hogares y universidades
dis_unibogt <- st_distance(x = test_final, y = uni_bogt)
head(dis_unibogt)

# Encontrar la minima distancia entre cada hogar y una universidad (universidad mas cercana)
min_unibogt = apply(dis_unibogt, 1, min)

#Agregar la variable de distancia minima a la base de test
test_final <- test_final %>%
  mutate(mind_unibogt = min_unibogt)

#  1.2 Medellin 
# Crear un objeto de OSM que contega las universidades (incluyendo institutos tecnicos) dentro del poligono El Poblado 
osm_unimedt = opq(bbox = getbb(" Comuna 14 - El Poblado")) %>%
  add_osm_feature(key = "amenity", value = "university")

# Extraer del objeto los features de universidades (id, nombre y amenity) y guardar los poligonos en nuevo objeto
osm_unimedt_sf = osm_unimedt %>% osmdata_sf()
uni_medt = osm_unimedt_sf$osm_polygons %>% select(osm_id, name, amenity)

# Eliminar universidades fuera del poligono
uni_medt = uni_medt[-c(2),]

# Visualizar un mapa que muestre todas las universidades
leaflet() %>% addTiles() %>% addPolygons(data = uni_medt, col = "blue")

# Verificar que las proyecciones sean iguales
st_crs(uni_medt) == st_crs(test_final)

# Medir la distancia entre hogares y universidades
dis_unimedt <- st_distance(x = test_final, y = uni_medt)
tail(dis_unimedt) 

# Encontrar la minima distancia entre cada hogar y una universidad (universidad mas cercana)
min_unimedt = apply(dis_unimedt, 1, min)

#Agregar la variable de distancia minima a la base de test
test_final <- test_final %>%
  mutate(mind_unimedt = min_unimedt)

#  1.3 Bogota y Medellin
test_final <- test_final %>% 
  mutate(dismin_uni = ifelse(l3 == "Bogotá D.C", 
                             yes = mind_unibogt,
                             no = min_unimedt))

# * Variable de Centros comerciales (CC) ---- 
#  2.1 Bogota 
# Crear un objeto de OSM que contega los centros comerciales dentro del poligono de Chapinero 
osm_ccbogt = opq(bbox = getbb("UPZs Localidad Chapinero")) %>%
  add_osm_feature(key = "shop", value = "mall")

# Extraer del objeto los features de centros comerciales (id, nombre y shop) y guardar los poligonos en nuevo objeto
osm_ccbogt_sf = osm_ccbogt %>% osmdata_sf()
cc_bogt = osm_ccbogt_sf$osm_polygons %>% select(osm_id, name, shop)

# Visualizar un mapa que muestre todos los centros comerciales
leaflet() %>% addTiles() %>% addPolygons(data = cc_bogt, col = "black")

# Verificar que las proyecciones sean iguales
st_crs(cc_bogt) == st_crs(test_final)

# Medir la distancia entre los hogares y los centros comerciales y visualizar los primeros datos
dis_ccbogt <- st_distance(x = test_final, y = cc_bogt)
head(dis_ccbogt)

#Aplicar un loop que encuentre la minima distancia entre cada hogar y un centro comercial (CC más cercano)
min_ccbogt = apply(dis_ccbogt, 1, min)

#Agregar la variable de distancia minima a la base de test
test_final <- test_final %>%
  mutate(mind_ccbogt = min_ccbogt)

#  2.2 Medellin 
# Crear un objeto de OSM que contega los centros comerciales dentro del poligono El Poblado
osm_ccmedt = opq(bbox = getbb("Comuna 14 - El Poblado")) %>%
  add_osm_feature(key = "shop", value = "mall")

# Extraer del objeto los features de centros comerciales (id, nombre y shop) y guardar los poligonos en nuevo objeto
osm_ccmedt_sf = osm_ccmedt %>% osmdata_sf()
cc_medt = osm_ccmedt_sf$osm_polygons %>% select(osm_id, name, shop)

# Eliminar centros comerciales fuera del poligono
cc_medt = cc_medt[-c(2,7,9),]

# Visualizar un mapa que muestre todos los centros comerciales
leaflet() %>% addTiles() %>% addPolygons(data = cc_medt, col = "black")

#Verificar que las proyecciones sean iguales
st_crs(cc_medt) == st_crs(test_final)

#Medir la distancia entre hogares y centros comerciales
dis_ccmedt <- st_distance(x = test_final, y = cc_medt)
tail(dis_ccmedt)

# Encontrar la minima distancia entre cada hogar y un centro comercial (CC mas cercano)
min_ccmedt <- apply(dis_ccmedt, 1, min)

#Agregar la variable de distancia minima a la base de test
test_final <- test_final %>%
  mutate(mind_ccmedt = min_ccmedt)

#  2.3 Bogota y Medellin 
test_final <- test_final %>% 
  mutate(dismin_cc = ifelse(l3 == "Bogotá D.C", 
                            yes = mind_ccbogt,
                            no = mind_ccmedt))

# Base final test ----
colnames(test_final)[which(colnames(test_final)=="price")] = "precio"
colnames(test_final)[which(colnames(test_final)=="bedrooms")] = "habitaciones"
colnames(test_final)[which(colnames(test_final)=="bathrooms_final")] = "baños"
colnames(test_final)[which(colnames(test_final)=="surface_final")] = "superficie"
colnames(test_final)[which(colnames(test_final)=="dismin_uni")] = "universidad"
colnames(test_final)[which(colnames(test_final)=="dismin_cc")] = "centroComercial"
colnames(test_final)[which(colnames(test_final)=="parqueadero")] = "parqueadero"
colnames(test_final)[which(colnames(test_final)=="terrazaPatio")] = "terrazaPatio"

variables_categoricas <- c("bogota", "parqueadero", "terrazaPatio")

test_final <- as.data.frame(test_final)
for (i in variables_categoricas){
  test_final[,i] = as.numeric(test_final[,i])
  test_final[,i] = as.logical(test_final[,i])
}

variables_numericas <- c("baños", "habitaciones", "superficie",
                         "universidad", "centroComercial")

test_final <- as.data.frame(test_final)
for (i in variables_numericas){
  test_final[,i] = as.numeric(test_final[,i])
}

saveRDS(test_final, "stores/test_final.rds")




# Estadísticas Descriptivas ----
# Cargar bases de datos
train_final <- readRDS("stores/train_final.rds")
test_final <- readRDS("stores/test_final.rds")

# Train
train_final <- train_final %>% 
  mutate(parqueadero = as.logical(parqueadero))

ved_train <- c("precio", "baños", "habitaciones", "superficie", "parqueadero", "terrazaPatio", "universidad", "centroComercial")

tab_ved <- CreateTableOne(data = train_final,
                          strata = "l3",
                          vars = ved_train,
                          argsApprox = list(correct = TRUE))

ex_tab_ved <- print(tab_ved, quote = FALSE, noSpaces = TRUE, printToggle = FALSE) %>%
  as_tibble()
write_xlsx(ex_tab_ved, "views/ed_train.xlsx")

# Test 
test_final <- test_final %>% 
  mutate(parqueadero = as.logical(parqueadero))

ved_test <- c("baños", "habitaciones", "superficie", "parqueadero", "terrazaPatio", "universidad", "centroComercial")

tab_ved2 <- CreateTableOne(data = test_final,
                           strata = "l3",
                           vars = ved_test,
                           argsApprox = list(correct = TRUE))

ex_tab_ved2 <- print(tab_ved2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE) %>%
  as_tibble()
write_xlsx(ex_tab_ved2, "views/ed_test.xlsx")
