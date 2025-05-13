# Instalamos paquetes

library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(modeest)

# Cargamos los datasets y ponemos los nombres de las columnas en formato estandar

trips_2019 <- read_csv("Trips-2019-Q1.csv") %>% clean_names()
trips_2020 <- read_csv("Trips-2020-Q1.csv") %>% clean_names()

# Cambiamos nombres de columnas

trips_2020 <- trips_2020 %>%
  rename(
    trip_id = ride_id,
    start_time = started_at,
    end_time = ended_at,
    from_station_id = start_station_id,
    from_station_name = start_station_name,
    to_station_id = end_station_id,
    to_station_name = end_station_name,
    usertype = member_casual
  )

# Cambiamos las entradas member y casual del dataset 2020

trips_2020 <- trips_2020 %>%
  mutate(usertype = case_when(
    usertype == "member"  ~ "Subscriber",
    usertype == "casual" ~ "Customer",
    TRUE ~ tolower(usertype) 
  ))


# Unimos los 2 datasets

trips <- bind_rows(trips_2019, trips_2020)

# Cambiamos el tipo de dato

trips_2019 <- trips_2019 %>%
  mutate(trip_id = as.character(trip_id))

# Eliminamos columnas innecesarias

trips <- trips %>% select(-c(start_lat, start_lng, end_lat, end_lng, rideable_type))
trips <- trips %>% select(-c(rideable_type))

# Cambiamos el tipo de dato

trips <- trips %>%
  mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
  )

# Agregamos columna duracion del viaje

trips <- trips %>%
  mutate(
    tripduration = as.numeric(difftime(end_time, start_time, units = "mins"))
  )

# Filtramos para quitar entradas erroneas

trips <- trips %>%
filter(tripduration > 0 & tripduration < 1440)

# Creamos la columna dia de la semana

trips <- trips %>%
  mutate(
    week_day = wday(start_time, label = TRUE, abbr = FALSE, week_start = 1)
  )


# Calculamos maximo, minimo y media de duracion de los viajes

mean(trips$tripduration, na.rm = TRUE)        
max(trips$tripduration, na.rm = TRUE)         
min(trips$tripduration, na.rm = TRUE)  

# Calculamos moda

mfv(trips$week_day)

# Resumen por tipo de usuario y dia de la semana

trips %>%
  group_by(usertype, week_day) %>%
  summarise(
    total_trips = n(),
    average_duration = mean(tripduration, na.rm = TRUE)
  ) %>%
  arrange(usertype, week_day)

# Agregamos columna hora del dia

trips <- trips %>%
  mutate(hour_of_day = lubridate::hour(start_time)
  )


# Que dias y q horas usan mas las bicis cada tipo de usuario


trips %>%
  group_by(usertype, week_day, hour_of_day) %>%
  summarise(trips = n()) %>%
  arrange(usertype, week_day, hour_of_day)

# Calculo de promedio de viajes por hora en cada día de la semana, para cada tipo de usuario

average_summary <- trips %>%
  group_by(usertype, week_day, hour_of_day) %>%
  summarise(avg_trips = n() / n_distinct(date(start_time)), .groups = "drop")

# Creamos mapa de calor para ver en que dias y hrs se concentra mas el uso por cada tipo de usuario

ggplot(average_summary, aes(x = hour_of_day, y = week_day, fill = avg_trips)) +
  geom_tile(color = "black") +
  facet_wrap(~ usertype) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Number of trips per hour and day", x = "Time of day", y = "Day of the week") +
  theme_classic()

# Desde q estacion sale mas cada tipo de cliente

trips %>%
  group_by(usertype, from_station_name) %>%
  summarise(total_trips = n(), .groups = "drop") %>%
  arrange(desc(total_trips))

# Top 10 de estaciones con mas salidas para cada tipo de usuario


top_station_start <- trips %>%
  group_by(usertype, from_station_name) %>%
  summarise(total_trips = n(), .groups = "drop") %>%
  arrange(usertype, desc(total_trips)) %>%
  group_by(usertype) %>%
  slice_head(n = 10)

# Creamos un grafico para visualizar lo anterior

ggplot(top_station_start, aes(x = reorder(from_station_name, total_trips), y = total_trips, fill = usertype)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ usertype, scales = "free") +
  labs(title = "Top 10 stations by user type",
       x = "Home station", y = "Numbers of trips") +
  theme_classic()

# Duracion de viajes por tipo de usuario

trips %>%
  group_by(usertype) %>%
  summarise(mean_duration = mean(tripduration, na.rm = TRUE),
            median_duration = median(tripduration, na.rm = TRUE))

# Frecuencia de uso por mes de cada tipo de usuario

trips %>%
  mutate(month = lubridate::floor_date(start_time, "month")) %>%
  group_by(usertype, month) %>%
  summarise(trips = n())

# Guardamos el Dataset limpio y analizado como CSV

write.csv(trips, 'C:/Users/flbon/Desktop/Analisis de Datos/R/Cyclistic/trips_clean.csv', row.names = FALSE)