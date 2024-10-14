Lubridate Lab
================
Tepi
10/6/2024

## Laboratorio de la libreria Lubridate

``` r
##cargar librerias
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.3.3

``` r
library(lubridate)
library(dplyr)
```

## Resuelva las siguientes preguntas:

### Ejercicio 1: Convertir columnas de hora en fecha-hora

Problema: Convierte las columnas dep_time (hora de salida) y arr_time
(hora de llegada) en objetos de tipo datetime usando make_datetime() de
lubridate. Recuerda que estas columnas están en formato militar (HHMM).

Ayuda: Investiga la funcion matematica de modulo de r.

``` r
flights_datetime <- flights %>%
  mutate(
    # Extraer hora y minuto de dep_time
    dep_hour = dep_time %/% 100,
    dep_minute = dep_time %% 100,
    # Crear objeto datetime para dep_time
    dep_datetime = make_datetime(year, month, day, dep_hour, dep_minute),
    
    # Extraer hora y minuto de arr_time
    arr_hour = arr_time %/% 100,
    arr_minute = arr_time %% 100,
    # Crear objeto datetime para arr_time
    arr_datetime = make_datetime(year, month, day, arr_hour, arr_minute)
  )

flights_datetime %>%
  select(dep_time, dep_datetime, arr_time, arr_datetime) %>%
  head()
```

    ## # A tibble: 6 × 4
    ##   dep_time dep_datetime        arr_time arr_datetime       
    ##      <int> <dttm>                 <int> <dttm>             
    ## 1      517 2013-01-01 05:17:00      830 2013-01-01 08:30:00
    ## 2      533 2013-01-01 05:33:00      850 2013-01-01 08:50:00
    ## 3      542 2013-01-01 05:42:00      923 2013-01-01 09:23:00
    ## 4      544 2013-01-01 05:44:00     1004 2013-01-01 10:04:00
    ## 5      554 2013-01-01 05:54:00      812 2013-01-01 08:12:00
    ## 6      554 2013-01-01 05:54:00      740 2013-01-01 07:40:00

## Ejercicio 2: Duracion del vuelo

Calcula el tiempo de vuelo total en minutos entre las columnas dep_time
y arr_time que calculaste en el primer Ejercicio.

``` r
flights_datetime <- flights_datetime %>%
  mutate(
    # Ajustar arr_datetime si el vuelo llega al día siguiente
    arr_datetime = if_else(arr_datetime < dep_datetime, arr_datetime + days(1), arr_datetime),
    flight_duration = as.numeric(difftime(arr_datetime, dep_datetime, units = "mins"))
  )

flights_datetime %>%
  select(dep_datetime, arr_datetime, flight_duration) %>%
  head()
```

    ## # A tibble: 6 × 3
    ##   dep_datetime        arr_datetime        flight_duration
    ##   <dttm>              <dttm>                        <dbl>
    ## 1 2013-01-01 05:17:00 2013-01-01 08:30:00             193
    ## 2 2013-01-01 05:33:00 2013-01-01 08:50:00             197
    ## 3 2013-01-01 05:42:00 2013-01-01 09:23:00             221
    ## 4 2013-01-01 05:44:00 2013-01-01 10:04:00             260
    ## 5 2013-01-01 05:54:00 2013-01-01 08:12:00             138
    ## 6 2013-01-01 05:54:00 2013-01-01 07:40:00             106

## Ejercicio 3: Extraer componentes de fechas

Extrae el dia de la semana y la hora en que salieron los aviones y
guardalos en las variables `dep_day_of_week` y `dep_hour`.

``` r
dias_semana_es <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")

flights_datetime <- flights_datetime %>%
  mutate(
    dep_day_of_week = wday(dep_datetime, label = TRUE, abbr = FALSE),
    dep_hour = hour(dep_datetime)
  )

levels(flights_datetime$dep_day_of_week) <- dias_semana_es

flights_datetime %>%
  select(dep_datetime, dep_day_of_week, dep_hour) %>%
  head()
```

    ## # A tibble: 6 × 3
    ##   dep_datetime        dep_day_of_week dep_hour
    ##   <dttm>              <ord>              <int>
    ## 1 2013-01-01 05:17:00 martes                 5
    ## 2 2013-01-01 05:33:00 martes                 5
    ## 3 2013-01-01 05:42:00 martes                 5
    ## 4 2013-01-01 05:44:00 martes                 5
    ## 5 2013-01-01 05:54:00 martes                 5
    ## 6 2013-01-01 05:54:00 martes                 5

## Ejercicio 4: Crear nuevas columnas con el día de la semana y la semana del año

Problema: Usando la columna `time_hour`, crea una nueva columna que
indique el día de la semana y otra que indique la semana del año en la
que ocurrió el vuelo.

Ayuda: Invesitga la funcion wday de lubridate.

``` r
flights_datetime <- flights_datetime %>%
  mutate(
    time_day_of_week = wday(time_hour, label = TRUE, abbr = FALSE),
    week_of_year = week(time_hour)
  )

levels(flights_datetime$time_day_of_week) <- dias_semana_es

flights_datetime %>%
  select(time_hour, time_day_of_week, week_of_year) %>%
  head()
```

    ## # A tibble: 6 × 3
    ##   time_hour           time_day_of_week week_of_year
    ##   <dttm>              <ord>                   <dbl>
    ## 1 2013-01-01 05:00:00 martes                      1
    ## 2 2013-01-01 05:00:00 martes                      1
    ## 3 2013-01-01 05:00:00 martes                      1
    ## 4 2013-01-01 05:00:00 martes                      1
    ## 5 2013-01-01 06:00:00 martes                      1
    ## 6 2013-01-01 05:00:00 martes                      1

## Ejercicio 5: Encontrar los vuelos que salieron los fines de semana

Problema: Filtra los vuelos que despegaron un sábado o domingo y
devuelve el total de vuelos en fines de semana.

``` r
weekend_flights <- flights_datetime %>%
  filter(wday(dep_datetime) %in% c(1, 7))

total_weekend_flights <- nrow(weekend_flights)

total_weekend_flights
```

    ## [1] 83565
