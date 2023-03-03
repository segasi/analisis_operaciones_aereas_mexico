### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Importar bases de datos ----

## Operaciones diarias en el AIFA a partir del 21 de marzo de 2022 ----

# Fuente: elaboración propia a partir de información publicada por Flight Aware: https://flightaware.com/live/airport/MMSM

bd_aifa_fa <- 
  read_csv(file = "01_bd/a_partir_de_flight_aware/bd_operaciones_aifa_corte_20230224.csv")

## Estadística operacional regular por origen-destino ----

# Fuente: https://www.gob.mx/afac/acciones-y-programas/estadisticas-280404/

# Nota: Sólo considero los datos de operaciones nacionales e internacionales de servicio regular o programado

# Vuelos nacionales ----

# Número de vuelos nacionales
bd_orig_dest_nal_vuelos_w <- 
  read_excel("01_bd/afac/2022/sase-diciembre-2022-26012023.xlsx", 
             sheet = 2, 
             range = "a6:n513") %>% 
  clean_names() %>% 
  mutate(tipo = "Nacionales - Serv. regular") 


# Número de pasajeros nacionales
bd_orig_dest_nal_pasajeros_w <- 
  read_excel("01_bd/afac/2022/sase-diciembre-2022-26012023.xlsx", 
             sheet = 2, 
             range = "a6:aa513") %>% 
  clean_names() %>% 
  select(-c(ene_jan_3:total)) %>% 
  mutate(tipo = "Nacionales - Serv. regular") 

# Número de vuelos de carga
bd_orig_dest_nal_carga_w <- 
  read_excel("01_bd/afac/2022/sase-diciembre-2022-26012023.xlsx", 
             sheet = 2, 
             range = "a6:an513") %>% 
  clean_names() %>%
  select(-c(ene_jan_3:total_28)) %>%
  mutate(tipo = "Nacionales - Serv. regular") 

# Vuelos internacionales ----

# Número de vuelos internacionales
bd_orig_dest_intl_vuelos_w <- 
  read_excel("01_bd/afac/2022/sase-diciembre-2022-26012023.xlsx",
             sheet = 3, 
             range = "a6:p869") %>% 
  clean_names() %>% 
  mutate(tipo = "Internacionales - Serv. regular") 


# Número de pasajeros internacionales
bd_orig_dest_intl_pasajeros_w <- 
  read_excel("01_bd/afac/2022/sase-diciembre-2022-26012023.xlsx",
             sheet = 3, 
             range = "a6:ac869") %>% 
  clean_names() %>% 
  select(-c(ene_jan_5:total)) %>% 
  mutate(tipo = "Internacionales - Serv. regular") 

# Número de vuelos de carga internacionales
bd_orig_dest_intl_carga_w <- 
  read_excel("01_bd/afac/2022/sase-diciembre-2022-26012023.xlsx",
             sheet = 3, 
             range = "a6:ap869") %>% 
  clean_names() %>%
  select(-c(ene_jan_5:total_30)) %>%
  mutate(tipo = "Internacionales - Serv. regular") 

## Capacidad de aeronaves que operan desde/hacia el AIFA ----

# Fuentes: Los datos de las aeronaves utilzidas los obtuve de FlightAware y los de la capacidad de cada una de Seat Guru

bd_capacidad_aifa <- 
  read_csv(file = "01_bd/bd_capacidad_aviones_aifa.csv")


### Transformar de varias formas a tibbles ----

## Tibbles con información de Estadística operacional regular por origen-destino ----

# Vuelos nacionales ----

# Transformar estructura de tibbles de w a l

# Vuelos nacionales
bd_orig_dest_nal_vuelos_l <- 
  bd_orig_dest_nal_vuelos_w %>% 
  pivot_longer(-c(origen_from, destino_to, tipo), names_to = "mes", values_to = "n_vuelos")

# Pasajeros nacionales
bd_orig_dest_nal_pasajeros_l <- 
  bd_orig_dest_nal_pasajeros_w %>% 
  pivot_longer(-c(origen_from, destino_to, tipo), names_to = "mes", values_to = "n_pasajeros")

# Vuelos de carga nacionales
bd_orig_dest_nal_carga_l <- 
  bd_orig_dest_nal_carga_w %>% 
  pivot_longer(-c(origen_from, destino_to, tipo), names_to = "mes", values_to = "kg_carga")


# Construir función para realizar los mismos cambios a los tres tibbles con datos de operaciones nacionales
fun_cambios_varios_nal <- 
  function(nombre_tibble) {
    nombre_tibble %>% 
      # Renombrar variables
      rename(origen = origen_from,
             destino = destino_to) %>% 
      # Editar valores que corresponden a meses
      mutate(mes = case_when(str_detect(mes, "ene_") ~ "Enero",
                             str_detect(mes, "feb_") ~ "Febrero",
                             str_detect(mes, "mar_") ~ "Marzo",
                             str_detect(mes, "abr_") ~ "Abril",
                             str_detect(mes, "may_") ~ "Mayo",
                             str_detect(mes, "jun_") ~ "Junio",
                             str_detect(mes, "jul_") ~ "Julio",
                             str_detect(mes, "ago_") ~ "Agosto",
                             str_detect(mes, "sep_") ~ "Septiembre",
                             str_detect(mes, "oct_") ~ "Octubre",
                             str_detect(mes, "nov_") ~ "Noviembre",
                             str_detect(mes, "dic_") ~ "Diciembre"),
             # Reordenar niveles de variable mes
             mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
             # Generar versión numérica de mes
             mes_numero = case_when(mes == "Enero" ~ 1,
                                    mes == "Febrero" ~ 2,
                                    mes == "Marzo" ~ 3,
                                    mes == "Abril" ~ 4,
                                    mes == "Mayo" ~ 5,
                                    mes == "Junio" ~ 6,
                                    mes == "Julio" ~ 7,
                                    mes == "Agosto" ~ 8,
                                    mes == "Septiembre" ~ 9,
                                    mes == "Octubre" ~ 10,
                                    mes == "Noviembre" ~ 11,
                                    mes == "Diciembre" ~ 12),
             # Cambiar cadenas de texto de nombres aeropuertos a mayúsculas y minúsculas
             origen = str_to_title(origen),
             destino = str_to_title(destino),
             # Enchular nombres de aeropuertos
             origen = case_when(origen == "Cancun" ~ "Cancún",
                                origen == "Culiacan" ~ "Culiacán",
                                origen == "Del Bajio" ~ "Bajío",
                                origen == "Ixtapa Zihuatanejo" ~ "Zihuatanejo",
                                origen == "Ixtepec, Oaxaca" ~ "Ixtepec",
                                origen == "Mazatlan" ~ "Mazatlán",
                                origen == "Merida" ~ "Mérida",
                                origen == "Minatitlan" ~ "Minatitlán",
                                origen == "Queretaro" ~ "Querétaro",
                                origen == "San Luis Potosi" ~ "San Luis Potosí",
                                str_detect(origen, "San Jos") ~ "Los Cabos",
                                origen == "Torreon" ~ "Torreón",
                                origen == "Mexico" ~ "AICM",
                                origen == "Santa Lucía" ~ "AIFA",
                                T ~ origen),
             origen = str_replace(origen, "Ciudad", "Cd."),
             origen = str_replace(origen, " Del ", " del "),
             origen = str_replace(origen, "Puerto", "Pto."),
             destino = case_when(destino == "Cancun" ~ "Cancún",
                                 destino == "Culiacan" ~ "Culiacán",
                                 destino == "Del Bajio" ~ "Bajío",
                                 destino == "Ixtapa Zihuatanejo" ~ "Zihuatanejo",
                                 destino == "Ixtepec, Oaxaca" ~ "Ixtepec",
                                 destino == "Mazatlan" ~ "Mazatlán",
                                 destino == "Merida" ~ "Mérida",
                                 destino == "Minatitlan" ~ "Minatitlán",
                                 destino == "Queretaro" ~ "Querétaro",
                                 destino == "San Luis Potosi" ~ "San Luis Potosí",
                                 str_detect(destino, "San Jos") ~ "Los Cabos",
                                 destino == "Torreon" ~ "Torreón",
                                 destino == "Mexico" ~ "AICM",
                                 destino == "Santa Lucía" ~ "AIFA",
                                 T ~ destino),
             destino = str_replace(destino, "Ciudad", "Cd."),
             destino = str_replace(destino, " Del ", " del "),
             destino = str_replace(destino, "Puerto", "Pto.")) %>%
      relocate(mes_numero, .after = mes)
  }

# Utilizar fun_cambios_varios_nal
bd_orig_dest_nal_vuelos_l <- fun_cambios_varios_nal(nombre_tibble = bd_orig_dest_nal_vuelos_l)
bd_orig_dest_nal_pasajeros_l <- fun_cambios_varios_nal(nombre_tibble = bd_orig_dest_nal_pasajeros_l)
bd_orig_dest_nal_carga_l <- fun_cambios_varios_nal(nombre_tibble = bd_orig_dest_nal_carga_l)


# Unir tibbles
bd_orig_dest_nal <- 
  bd_orig_dest_nal_vuelos_l %>% 
  left_join(bd_orig_dest_nal_pasajeros_l, by = c("origen", "destino", "tipo", "mes", "mes_numero")) %>% 
  left_join(bd_orig_dest_nal_carga_l, by = c("origen", "destino", "tipo", "mes", "mes_numero"))


# Vuelos internacionales ----

# Transformar estructura de tibbles de w a l

# Vuelos internacionales
bd_orig_dest_intl_vuelos_l <- 
  bd_orig_dest_intl_vuelos_w %>%
  pivot_longer(-c(origen_from:pais_destino_country_to, tipo), 
               names_to = "mes", 
               values_to = "n_vuelos")

# Pasajeros internacionales
bd_orig_dest_intl_pasajeros_l <- 
  bd_orig_dest_intl_pasajeros_w %>% 
  pivot_longer(-c(origen_from:pais_destino_country_to, tipo), 
               names_to = "mes", 
               values_to = "n_pasajeros")

# Vuelos de carga internacionales
bd_orig_dest_intl_carga_l <- 
  bd_orig_dest_intl_carga_w %>% 
  pivot_longer(-c(origen_from:pais_destino_country_to, tipo),
               names_to = "mes", 
               values_to = "kg_carga")


# Renombrar y modificar valores de variables

# Construir función para realizar los mismos cambios a los tres tibbles con datos de operaciones internacionales
fun_cambios_varios_intl <- 
  function(nombre_tibble) {
    nombre_tibble %>% 
      # Renombrar variables
      rename(origen = origen_from,
             destino = destino_to,
             pais_origen = pais_origen_country_from,
             pais_destino = pais_destino_country_to) %>% 
      # Editar valores que corresponden a meses
      mutate(mes = case_when(str_detect(mes, "ene_") ~ "Enero",
                             str_detect(mes, "feb_") ~ "Febrero",
                             str_detect(mes, "mar_") ~ "Marzo",
                             str_detect(mes, "abr_") ~ "Abril",
                             str_detect(mes, "may_") ~ "Mayo",
                             str_detect(mes, "jun_") ~ "Junio",
                             str_detect(mes, "jul_") ~ "Julio",
                             str_detect(mes, "ago_") ~ "Agosto",
                             str_detect(mes, "sep_") ~ "Septiembre",
                             str_detect(mes, "oct_") ~ "Octubre",
                             str_detect(mes, "nov_") ~ "Noviembre",
                             str_detect(mes, "dic_") ~ "Diciembre"),
             # Reordenar niveles de variable mes
             mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
             # Generar versión numérica de mes
             mes_numero = case_when(mes == "Enero" ~ 1,
                                    mes == "Febrero" ~ 2,
                                    mes == "Marzo" ~ 3,
                                    mes == "Abril" ~ 4,
                                    mes == "Mayo" ~ 5,
                                    mes == "Junio" ~ 6,
                                    mes == "Julio" ~ 7,
                                    mes == "Agosto" ~ 8,
                                    mes == "Septiembre" ~ 9,
                                    mes == "Octubre" ~ 10,
                                    mes == "Noviembre" ~ 11,
                                    mes == "Diciembre" ~ 12),
             # Editar valores de México para agregar acentos
             pais_origen = if_else(pais_origen == "Mexico", "México", pais_origen),
             pais_destino = if_else(pais_destino == "Mexico", "México", pais_destino),
             # Cambiar cadenas de texto de nombres aeropuertos a mayúsculas y minúsculas
             origen = str_to_title(origen),
             destino = str_to_title(destino),
             # Enchular nombres de aeropuertos
             origen = case_when(origen == "Cancun" ~ "Cancún",
                                origen == "Culiacan" ~ "Culiacán",
                                origen == "Del Bajio" ~ "Bajío",
                                origen == "Ixtapa Zihuatanejo" ~ "Zihuatanejo",
                                origen == "Ixtepec, Oaxaca" ~ "Ixtepec",
                                origen == "Mazatlan" ~ "Mazatlán",
                                origen == "Merida" ~ "Mérida",
                                origen == "Minatitlan" ~ "Minatitlán",
                                origen == "Queretaro" ~ "Querétaro",
                                origen == "San Luis Potosi" ~ "San Luis Potosí",
                                str_detect(origen, "San Jos") ~ "Los Cabos",
                                origen == "Torreon" ~ "Torreón",
                                origen == "Mexico" ~ "AICM",
                                origen == "Santa Lucía" ~ "AIFA",
                                T ~ origen),
             origen = str_replace(origen, "Ciudad", "Cd."),
             origen = str_replace(origen, " Del ", " del "),
             origen = str_replace(origen, "Puerto", "Pto."),
             destino = case_when(destino == "Cancun" ~ "Cancún",
                                 destino == "Culiacan" ~ "Culiacán",
                                 destino == "Del Bajio" ~ "Bajío",
                                 destino == "Ixtapa Zihuatanejo" ~ "Zihuatanejo",
                                 destino == "Ixtepec, Oaxaca" ~ "Ixtepec",
                                 destino == "Mazatlan" ~ "Mazatlán",
                                 destino == "Merida" ~ "Mérida",
                                 destino == "Minatitlan" ~ "Minatitlán",
                                 destino == "Queretaro" ~ "Querétaro",
                                 destino == "San Luis Potosi" ~ "San Luis Potosí",
                                 str_detect(destino, "San Jos") ~ "Los Cabos",
                                 destino == "Torreon" ~ "Torreón",
                                 destino == "Mexico" ~ "AICM",
                                 destino == "Santa Lucía" ~ "AIFA",
                                 T ~ destino),
             destino = str_replace(destino, "Ciudad", "Cd."),
             destino = str_replace(destino, " Del ", " del "),
             destino = str_replace(destino, "Puerto", "Pto.")) %>%
      relocate(mes_numero, .after = mes)

  }

# Utilizar fun_cambios_varios_intl
bd_orig_dest_intl_vuelos_l <- fun_cambios_varios_intl(nombre_tibble = bd_orig_dest_intl_vuelos_l)
bd_orig_dest_intl_pasajeros_l <- fun_cambios_varios_intl(nombre_tibble = bd_orig_dest_intl_pasajeros_l)
bd_orig_dest_intl_carga_l <- fun_cambios_varios_intl(nombre_tibble = bd_orig_dest_intl_carga_l)


# Unir tibbles
bd_orig_dest_intl <- 
  bd_orig_dest_intl_vuelos_l %>% 
  left_join(bd_orig_dest_intl_pasajeros_l, by = c("origen", "pais_origen", "destino", "pais_destino", "tipo", "mes", "mes_numero")) %>% 
  left_join(bd_orig_dest_intl_carga_l, by = c("origen", "pais_origen", "destino", "pais_destino", "tipo", "mes", "mes_numero"))



## Tibbles con información de capacidad de aviones ----

# Calcular la capacidad promedio de los aviones que operan, tanto en general como en cada ruta desde/hacia el AIFA ---
bd_capacidad_aifa_media_ruta <- 
  bd_capacidad_aifa %>% 
  group_by(mes, mes_numero) %>% 
  mutate(capacidad_media_gral = mean(capacidad)) %>% 
  ungroup() %>% 
  group_by(mes, mes_numero, origen, destino) %>% 
  summarise(capacidad_media_gral = first(capacidad_media_gral), 
            capacidad_media_ruta = mean(capacidad)) %>% 
  ungroup()

### Unir tibbles con información de operaciones nacionales e internacionales, construidas a partir de la información de Estadística operacional regular por origen-destino ----
bd_orig_dest_2022 <- 
  bd_orig_dest_nal %>% 
  mutate(pais_origen = "México", 
         pais_destino = "México") %>% 
  bind_rows(bd_orig_dest_intl)


### Generar columnas que registre el año y la fecha del último día del mes correspondiente y reacomodar columnas ----
bd_orig_dest_2022 <- 
  bd_orig_dest_2022 %>% 
  mutate(año = 2022, 
         fecha = make_date(year =año, month = mes_numero, day = 1)) %>% 
  relocate(c(fecha, año), .before = mes) %>% 
  relocate(fecha:mes_numero, .before = origen)

### Guardar base de datos bd_orig_dest_2022 en formato .rds
bd_orig_dest_2022 %>% 
  saveRDS(file = "04_bd_generadas/bd_orig_dest_2022.rds")

 

### Genera tibble con información exclusiva del AIFA ----

# Salidas ----
bd_aifa_salidas <- 
  bd_orig_dest_2022 %>% 
  # Mantener solo observaciones de vuelos que salieorn del AIFA
  filter(origen == "AIFA") %>% 
  # Generar columna que identifique que estas operaciones son salidas
  mutate(tipo = "Salidas") %>% 
  # Unir datos de capacidad media de aeronaves usadas en operaciones desde/hacia este aeropuerto en cada ruta
  left_join(bd_capacidad_aifa_media_ruta, 
            by = c("mes", "mes_numero", "origen", "destino")) %>% 
  # Seleccionar, renombrar y reordenar columnas
  select(aeropuerto = destino, everything(), -c(origen, contains("pais_")))

#  Llegadas ----
bd_aifa_llegadas <- 
  bd_orig_dest_2022 %>% 
  # Mantener solo observaciones de vuelos que llegaron al AIFA
  filter(destino == "AIFA") %>% 
  # Generar columna que identifique que estas operaciones son llegadas
  mutate(tipo = "Llegadas") %>% 
  # Unir datos de capacidad media de aeronaves usadas en operaciones desde/hacia este aeropuerto en cada ruta
  left_join(bd_capacidad_aifa_media_ruta, 
            by = c("mes", "mes_numero", "origen", "destino")) %>% 
  # Seleccionar, renombrar y reordenar columnas
  select(aeropuerto = origen, everything(), -c(destino, contains("pais_")))

# Unir datos de salidas y llegadas del AIFA ----
bd_aifa_2022 <- 
  bd_aifa_salidas %>% 
  bind_rows(bd_aifa_llegadas)



### Calcular estadísticas descriptivas agregadas para el AIFA ----
resumen_mes_aifa_2022 <- 
  bd_aifa_2022 %>% 
  filter(!is.na(capacidad_media_gral)) %>% 
  group_by(mes, mes_numero) %>% 
  summarise(n_vuelos_mes = sum(n_vuelos),
            n_pasajeros_mes = sum(n_pasajeros),
            capacidad_media_gral = mean(capacidad_media_gral)) %>% 
  ungroup() %>% 
  # Corregir capacidad media para meses previos. Esto es necesario porque el número de rutas y compañías aéreas que han operado en cada una ha cambiado con el paso del tiempo
  mutate(capacidad_media_gral = case_when(mes == "Marzo" ~ 153,
                                          mes == "Abril" ~ 153,
                                          mes == "Mayo" ~ 145.3,
                                          mes == "Junio" ~ 145.3,
                                          mes == "Julio" ~ 149.6,
                                          mes == "Agosto" ~  142.6,
                                          T ~ capacidad_media_gral),
         # Construir variable que indica el número de días que operó el aeropuerto como terminal de vuelos comerciales en el respectivo mes 
         dias_operacion = case_when(mes == "Marzo" ~ 11,
                                    mes == "Abril" ~ 30,
                                    mes == "Mayo" ~ 31,
                                    mes == "Junio" ~ 30,
                                    mes == "Julio" ~ 31,
                                    mes == "Agosto" ~ 31,
                                    mes == "Septiembre" ~ 30,
                                    mes == "Octubre" ~ 31,
                                    mes == "Noviembre" ~ 30,
                                    mes == "Diciembre" ~ 31),
         # Promedio de vuelos operados cada día
         n_vuelos_x_dia = n_vuelos_mes/dias_operacion,
         # Promedio de pasajeros atendidos cada día
         n_pasajeros_x_dia = n_pasajeros_mes/dias_operacion,
         # Promedio de pasajeros que abordaron cada vuelo que salió o llegó de este aeropuerto
         media_pasajeros_x_vuelo_mes = n_pasajeros_mes/n_vuelos_mes,
         # Promedio de ocupacion 
         media_ocupacion_mes = media_pasajeros_x_vuelo_mes/capacidad_media_gral*100) %>% 
  filter(!is.na(dias_operacion)) %>% 
  arrange(mes_numero) %>% 
  as.data.frame()

## Generar columnas que registre el año y la fecha del último día del mes correspondiente y reacomodar columnas ----
resumen_mes_aifa_2022 <- 
  resumen_mes_aifa_2022 %>% 
  mutate(año = 2022, 
         fecha = make_date(year =año, month = mes_numero, day = 1)) %>% 
  relocate(c(fecha, año), .before = mes)

## Guardar base de datos resumen_mes_aifa_2022 en formato .rds
resumen_mes_aifa_2022 %>% 
  saveRDS(file = "04_bd_generadas/resumen_mes_aifa_2022.rds")



### Generar tibble para analizar el total de operaciones (salidas y llegadas) registradas en cada aeropuerto ---- 

# Cálculos a partir de información de Estadística operacional regular por origen-destino

lista_origen <- 
  bd_orig_dest_2022 %>% 
  filter(pais_origen == "México") %>% 
  distinct(origen) %>% 
  pull()

lista_destino <- 
  bd_orig_dest_2022 %>% 
  filter(pais_destino == "México") %>% 
  distinct(destino) %>% 
  pull()


bd_salidas_todos <- 
  bd_orig_dest_2022 %>% 
  filter(origen %in% lista_origen, 
         mes_numero < 13) %>% 
  mutate(tipo = "Salidas") %>% 
  rename(apto_interes = origen,
         apto_ruta = destino)

bd_llegadas_todos <- 
  bd_orig_dest_2022 %>% 
  filter(destino %in% lista_destino,
         mes_numero < 13) %>% 
  mutate(tipo = "Llegadas") %>% 
  rename(apto_ruta = origen,
         apto_interes = destino)

bd_operaciones_todos_2022 <- 
  bd_salidas_todos %>% 
  bind_rows(bd_llegadas_todos)

## Generar columnas que registre el año y la fecha del último día del mes correspondiente y reacomodar columnas ----
bd_operaciones_todos_2022 <- 
  bd_operaciones_todos_2022 %>% 
  mutate(año = 2022, 
         fecha = make_date(year =año, month = mes_numero, day = 1)) %>% 
  relocate(c(fecha, año), .before = mes) %>% 
  relocate(fecha:mes_numero, .before = apto_interes)

## Guardar base de datos bd_operaciones_todos_2022 en formato .rds
bd_operaciones_todos_2022 %>% 
  saveRDS(file = "04_bd_generadas/bd_operaciones_todos_2022.rds")

### Generar tibble con info exclusiva del AIFA, AICM y AIT, a partir de información de Estadística operacional regular por origen-destino ---- 

bd_salidas_sam <- 
  bd_orig_dest_2022 %>% 
  filter(origen %in% c("AIFA", "AICM", "Toluca"),
         # mes_numero < month(Sys.Date())
         ) %>% 
  mutate(tipo = "Salidas") %>% 
  rename(apto_interes = origen,
         apto_ruta = destino)

bd_llegadas_sam <- 
  bd_orig_dest_2022 %>% 
  filter(destino %in% c("AIFA", "AICM", "Toluca"),
         # mes_numero < month(Sys.Date())
         ) %>% 
  mutate(tipo = "Llegadas") %>% 
  rename(apto_ruta = origen,
         apto_interes = destino)

bd_sam_2022 <- 
  bd_salidas_sam %>% 
  bind_rows(bd_llegadas_sam)

