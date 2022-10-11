#ANÁLISIS DE DATOS DE UNA EMPRESA DE BICICLETAS COMPARTIDAS

#Comenzamos instalando el conjunto de paquetes tidyverse para ciencia de datos y cargando los paquetes lubridate (funciones de fecha) y ggplot2(visualización de datos).

install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
library('lubridate')
library('ggplot2')
library('dplyr')
library('magrittr')

#Ahora se comparan los nombres de las columnas de los tres archivos, si bien no tienen que estar en el mismo orden, si deben coincidir para poder unirlos en un sólo marco de datos

colnames(X22_06)
colnames(X22_07)
colnames(X22_08)

#Ahora inspeccionamos el conjunto de datos de cada archivo para buscar incongruencias

str(X22_06)
str(X22_07)
str(X22_08)

#Aqui convertirmos las columnas ride_id y direable_type a columna de caracteres para poder apilarlas correctamente

X22_06<-mutate(X22_06,ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))
X22_07<-mutate(X22_07,ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))
X22_08<-mutate(X22_08,ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))

#Aquí unimos los tres marcos de datos mensuales en un marco de datos global del trimestre complete

viajes_totales<- bind_rows(X22_06,X22_07,X22_08)

#Ahora eliminamos los marcos de datos individuales para conservar únicamente el marco integrado de viajes_totales

rm(X22_06)
rm(X22_07)
rm(X22_08)
ls()

#Aqui eliminales columnas que no nos sirven para nuestros propositos, así aprovechamos también a continuar liberando memoria

viajes_totales<-viajes_totales%>%select(-c(start_lat,start_lng,end_lat,end_lng))

#Hemos llegado al punto en el que podemos agregar datos y limpiarlos para comenzar con el análisis, seguimos el siguiente procedimiento:

#Primero inspeccionamos el marco de datos viajes_totales, después de haber reallizado todos los cambios anteriores.
colnames(viajes_totales)

#Averiguamos cuántas filas contiene el marco de datos
nrow(viajes_totales)

#averiguamos cuál es la dimensión del marco de datos
dim(viajes_totales)

#Para tener una pre-visualización del marco de datos y comprender el contexto, realizamos lo siguiente:
head(viajes_totales)

#Para ver la lista de columnas y los tipos de datos que contiene el marco
str(viajes_totales)

#para obtener el resumen estadístico de datos
summary(viajes_totales)

#Para averiguar cuántos miembros y cuántos usuarios casuales hay
table(viajes_totales$member_casual)

#El formato predeterminado es aaaa-mm-dd, entonces creamos una columna para la fecha, el mes, el día, el año y el dia de la semana(l,m,m,j,v,s,d)
viajes_totales$date<-as.Date(viajes_totales$started_at)
viajes_totales$month <- format(as.Date(viajes_totales$date), "%m")
viajes_totales$day <- format(as.Date(viajes_totales$date), "%d")
viajes_totales$year <- format(as.Date(viajes_totales$date), "%Y")
viajes_totales$day_of_week <- format(as.Date(viajes_totales$date), "%A")

#Ahora calculamos la duración de los viajes

viajes_totales$ride_length <- difftime(viajes_totales$ended_at,viajes_totales$started_at)

#Para poder realizar cálculos con la información de la columna ride_length, debemos convertirla a factor númerico

is.factor(viajes_totales$ride_length)
viajes_totales$ride_length <- as.numeric(as.character(viajes_totales$ride_length))
is.numeric(viajes_totales$ride_length)

# El marco contiene datos incorrectos cuando las bicicletas se sacaron de los muelles y Divvy verificó la calidad o la longitud del viaje fue negativa, para esto crearemos una nueva versión del marco de datos ya que se están eliminando los datos
viajes_totales_nueva_version <-viajes_totales[!(viajes_totales$start_station_name == "HQ QR" | viajes_totales$ride_length<0),]


#Ahora ya podemos realizar un análisis descriptivo del conjunto de datos
summary(viajes_totales_nueva_version$ride_length)

#Para comparar los usuarios casuales con los miembros

aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual, FUN = mean)
aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual, FUN = median)
aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual, FUN = max)
aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual, FUN = min)

# Para comparar el tiempo de viaje promedio diario de los miembros y los usuarios ocasionales
aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual +viajes_totales_nueva_version$day_of_week, FUN = mean)

# Para ordenar los dias de la semana
viajes_totales_nueva_version$day_of_week <- ordered(viajes_totales_nueva_version$day_of_week, levels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

# Para analizar el tiempo de viaje promedio por día de miembros y usuarios ocasionales
aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual + viajes_totales_nueva_version$day_of_week, FUN = mean)

#Ahora podemos realizar un análisis de las estadísticas del número de pasajeros por tipo y por cada dia de la semana

viajes_totales_nueva_version%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creamos un campo para dia de la semana
  group_by(member_casual, weekday) %>%  #agrupamos por usuario y por día
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% #calculamos el numero de viajes y su duración
  arrange(member_casual, weekday)%>%	#ordenamos los datos

viajes_totales_nueva_version%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual))+geom_col(position = "dodge")

counts <- aggregate(viajes_totales_nueva_version$ride_length ~ viajes_totales_nueva_version$member_casual + viajes_totales_nueva_version$day_of_week, FUN = mean)
write.csv(counts, file = 'resultados.csv')

