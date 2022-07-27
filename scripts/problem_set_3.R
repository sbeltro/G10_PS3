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
