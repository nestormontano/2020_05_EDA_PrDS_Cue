## Paquetes

library(dplyr) # manipulacion datos
# select - seleccionar columnas
# filter - filtrar filas por condición
# slice - filtrar filas por número de fila
# arrange - ordenar las filas
# mutate - modificar, crear o eliminar columnas
# summarise - resumir la info, obtener estad
# bind_* , *_join - Unir dataframes
library(tidyr) # manipulacion datos 2
# spread - filas a columnas (como tablas dinamicas)
# gather - columnas a filas (como tablas dinamicas)
library(magrittr) # Pipe %>%
# %>%, %$%, %<>%
library(readr) # Importar datos
library(ggplot2) # Gráficos
library(lubridate) # Manipular fechas
library(cowplot) # Grid de Gráficos
library(scales) # Escalas en los gráficos
library(purrr) # mapear funciones a las columnas 
# map_dfc, map_dbl, map_*

## Importacion de la data

# data_banco <- read_csv("Data/bank-additional-full.csv") ##ERROR
# str(data_banco)
# data_banco
# Se puede usar read.csv, el resultado es un data.frame
# Si usan read_csv o read_delim, el resultado es un tibble (data.frame)
# read_delim : Leer el archivo csv (prueben con read_csv también)
data_banco <- read_delim( "Data/bank-additional-full.csv", delim = ";")

# Ver las primeras filas 
head(data_banco)
# Ver las primeras filas dentro de RStudio
data_banco %>% 
  slice( 1:100 ) %>% 
  View

## ¿Para csv pesados??
## data.table::fread() ## Cargar .csv muy pesados
## ff.. # usar disco duro para guardar objetos de R.
## monetDB # Base de datos Columnar, interactuan con R.

## Entender la data
# str(data_banco) # Opcion de R-base
# glimpse permite ver tipos de variables
glimpse(data_banco)


## Variables a transformar

# unique( data_banco$y ) # obtener los valores únicos
data_banco %$% unique(y) # obtener los valores únicos (tidyverse style)


# %<>% : Pipe que permite calcular todo y guardarlo en la variable de la izquierda.
# data_banco %<>% ... ES EQUIVALENTE A data_banco <- data_banco %>% ...
# mutate - modificar, crear o eliminar columnas
data_banco %<>% 
  mutate(
    y = factor(y,
               levels= c("yes", "no"),
               labels = c("si", "no")
               )
  )

data_banco %$% unique(y) # obtener los valores únicos



# %$% : Pipe para enviar una columna al otro comando
# Unique : permite obtener valores únicos
data_banco %$% unique(education) # Unique de la columna "y"

## Para el ejercicio queremos visibilizar los NA
## esto nos sirve para luego poder imputar estos NA
## El proceso sería primero (mientras la columna aún es texto) 
## transformar el valor [unknown] a NA (todo como caracter)
## Luego pasar la columna a factor, con los niveles y ordered=TRUE
data_banco %<>%
  mutate(
    education = ifelse(education == "unknown",
                        yes= NA_character_,
                        no= as.character(education) )
  )

data_banco %$% unique(education)

data_banco %<>%
  mutate(
    education= factor( education,
                       levels = c("illiterate", "basic.4y", "basic.6y", 
                                  "basic.9y", "high.school", 
                                  "professional.course",
                                  "university.degree"), 
                       labels = c("No Educ.", "4A Bas.", "6A Bas.",
                                  "9A Bas.", "Bachill.", "Tecnico",
                                  "Univer."),
                       ordered = TRUE)
  )

data_banco %$% unique(education) # Unique de la columna "education"


# data_banco %>%  filter( education == NA) ## ERROR
data_banco %>%  filter( is.na(education) ) # correcto


# %$% : Pipe para enviar una columna al otro comando
# Unique : permite obtener valores únicos
data_banco %$% unique(month) 
data_banco %$% unique(day_of_week) 

# Convertir a factor
data_banco %<>% 
  mutate( 
    month = factor( month, 
                    levels= c("mar", "apr", "may", "jun", "jul",
                              "aug", "sep", "oct", "nov", "dec" ), 
                    labels= c("Mar", "Abr", "May", "Jun", "Jul", 
                              "Ago", "Sep", "Oct", "Nov", "Dec" ), 
                    ordered = TRUE),
    day_of_week = factor( day_of_week, 
                          levels= c("mon", "tue", "wed", "thu", "fri" ), 
                          labels= c("Lun", "Mar", "Mie", "Jue", "Vie" ), 
                          ordered = TRUE),
  ) 
# Verificar
data_banco %$% unique(month) 
data_banco %$% unique(day_of_week) 

# str : ver la estructura de la variable
data_banco %$% str(month)



#### EDA Análisis Exploratorio de Datos -------

data_banco %>% slice(1:100) %>%  View

## Contar los valores unicos que tiene cada columna

# data_banco %$% unique(education) %>% length()
# length( unique ( data_banco$education ))
# length( unique ( COL )) ## paquete purrr

data_banco %>% 
  map_df( function(x) length( unique( x )) ) %>%  ## paquete purr
  gather(key = "Variable", value = "Cant_Unicos") %>%  ## paquete tidyr
  arrange(-Cant_Unicos) %>% 
  View
  
## cantidad de datos en cada nivel de la variable job

# table( data_banco$job ) # Resulta un objeto tipo table
# quiero un objeto tipo data.frame

data_banco %>% 
  group_by( job ) %>% 
  summarise(
    Frecuencia= n()
  ) %>% 
  arrange( -Frecuencia)



## exploracion rapida

summary(data_banco)


## ¿Incíde el índice de precios al consumidor?

data_banco %>% 
  group_by( y ) %>% 
  summarise(
    Freq= n(), 
    Min= min(cons.price.idx, na.rm = T),
    Q1= quantile(cons.price.idx, probs = 0.25, na.rm = T),
    Mean= mean( cons.price.idx, na.rm = T),
    Media_Acotada= median(cons.price.idx, trim = 0.05, na.rm = T), 
    Mediana= median( cons.price.idx, na.rm = T),
    Q3= quantile(cons.price.idx, probs = 0.75, na.rm = T),
    Max= max( cons.price.idx, na.rm = T)
  )


ggplot(data_banco, aes(x= y, y= cons.price.idx )) +
  geom_boxplot( aes( fill= y)) +
  labs(title = "Boxplot Indice consumidor vs adquiere Dep.Plazo",
       x= "Adquiere Dep.Plazo",
       y= "Indice precio la consumidor")


## Graficar todos los indices en un sólo plot

plot_consprice <- ggplot(data_banco, aes(y, cons.price.idx)) + 
  geom_boxplot(aes(fill = y)) +
  labs(title = "Boxplot Indice consumidor \n vs adquiere dep.plazo",
       x= "Adquiere prestamo")
plot_consconf <- ggplot(data_banco, aes(y, cons.conf.idx)) + 
  geom_boxplot(aes(fill = y)) +
  labs(title = "Boxplot Confianza consumidor \n vs adquiere dep.plazo",
       x= "Adquiere prestamo")
plot_euribor3m <- ggplot(data_banco, aes(y, euribor3m)) + 
  geom_boxplot(aes(fill = y)) +
  labs(title = "Boxplot tasa euribor \n vs adquiere dep.plazo",
       x= "Adquiere prestamo")
plot_employed <- ggplot(data_banco, aes(y, nr.employed)) + 
  geom_boxplot(aes(fill = y)) +
  labs(title = "Boxplot Numero empleados \n vs adquiere dep.plazo",
       x= "Adquiere prestamo")

# Grafico compuesto
plot_grid(plot_consprice, plot_consconf, plot_euribor3m, plot_employed, 
          nrow = 2, ncol= 2)

plot_employed


## Incide la edad?



data_banco %>% 
  group_by( y ) %>% 
  summarise(
    Freq= n(), 
    Min= min(age, na.rm = T),
    Q1= quantile(age, probs = 0.25, na.rm = T),
    Mean= mean( age, na.rm = T),
    Media_Acotada= median(age, trim = 0.05, na.rm = T), 
    Mediana= median( age, na.rm = T),
    Q3= quantile(age, probs = 0.75, na.rm = T),
    Max= max( age, na.rm = T)
  ) %>% 
  View


## como frecuencia
ggplot( data_banco, aes(x= age)) +
  geom_histogram( aes(fill= y), binwidth= 10)  + 
  labs( x= 'Edad', y= 'Frecuencia', 
        title= 'Histograma de la cantidad de personas que \n aceptan el deposito según la edad',
        fill= 'Suscribe \n deposito')

## como participacion en el intervalo
ggplot( data_banco, aes(x= age)) +
  geom_histogram( aes(fill= y), binwidth= 10, position = 'fill')  + 
  labs( x= 'Edad', y= 'Frecuencia', 
        title= 'Histograma de la cantidad de personas que \n aceptan el deposito según la edad',
        fill= 'Suscribe \n deposito')



## Incide el tipo de trabajo en la aceptacion de deposito a plazo

# Tabla de contingencia entre job y y

data_banco %$% 
  table(job, y) %>% 
  prop.table() %>% 
  round(digits = 4) * 100

# Probabilidad condicional
data_banco %$% 
  table(job, y) %>% 
  prop.table(margin = 1) %>% 
  round(digits = 4) * 100


ggplot( data_banco, aes(x= job)) +
  geom_bar( aes(fill= y), position = "fill") +
  coord_flip() +
  labs( x= 'Trabajo', y= 'Porcentaje', 
        title= 'Proporcion aceptar deposito vs trabajo', 
        fill= 'Suscribe \n deposito')+
  scale_y_continuous(labels = percent)
