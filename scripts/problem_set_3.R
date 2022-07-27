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
