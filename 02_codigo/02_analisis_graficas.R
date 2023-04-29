### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Importar y procesar las bases de datos ----
source("02_codigo/01_b_importar_preparar_bd_acumulada.R") 

### Definir fecha de corte que irá en el nombre de las gráficas ----
fecha_corte_grafica <- str_replace_all(Sys.Date() - 3, "-", "")

fecha_corte_grafica

### Generar folder para guardar las gráficas ----
dir.create(file.path("03_vis/", str_c("graficas_", fecha_corte_grafica)))


### Gráfica 1: Número de operaciones diarias del AIFA ----
bd_g_1 <- 
  bd_aifa_fa %>%
  filter(categoria == "Comercial") %>% 
  group_by(fecha) %>% 
  summarise(semana = mean(semana),
            num_operaciones = sum(num_operaciones)) %>% 
  ungroup() %>% 
  mutate(total = sum(num_operaciones))

# Datos de los últimos 30 días
bd_g_1 %>% 
  tail(n = 60) %>% 
  print(n = Inf)

bd_g_1 %>% 
  tail(n = 58) %>% 
  summarise(minimo =  min(num_operaciones),
            maximo =  max(num_operaciones))

# Promedio mensual de operaciones diarias
bd_g_1  %>% 
  mutate(fecha_piso = floor_date(fecha, unit = "month")) %>%
  summarise(media_operaciones = mean(num_operaciones),
            .by = fecha_piso) %>%
  print(n = Inf)

# Promedio semanal de operaciones semanales
bd_g_1  %>% 
  summarise(media_operaciones = mean(num_operaciones),
            .by = semana) %>%
  print(n = Inf)

# Operaciones diarias
bd_g_1  %>% 
  filter(fecha >= "2023-03-01",
         fecha <= "2023-04-01") %>% 
  summarise(minimo =  min(num_operaciones),
            maximo =  max(num_operaciones))
  

# Gráfica
bd_g_1 %>%
  ggplot(aes(x = fecha,
             y = num_operaciones)) +
  annotate(geom = "rect", xmin = as_date("2022-04-01") - 0.5, xmax = as_date("2022-04-30") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-06-01") - 0.5, xmax = as_date("2022-06-30") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-08-01") - 0.5, xmax = as_date("2022-08-31") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-10-01") - 0.5, xmax = as_date("2022-10-31") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-12-01") - 0.5, xmax = as_date("2022-12-31") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2023-02-01") - 0.5, xmax = as_date("2023-02-26") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2023-04-01") - 0.5, xmax = as_date("2023-04-23") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_col(fill = "#b5261e") +
  # geom_text(aes(label = num_operaciones), color = "white", vjust = 1.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 60, color = "steelblue") +
  geom_hline(yintercept = 120, color = "salmon") +
  annotate(geom = "text", x = max(bd_g_1$fecha) - ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.445), y = 105, label = "De acuerdo con el General Isidoro Pastor, para que el AIFA\nllegue a su punto de equilibrio debe tener 120 operaciones\ndiarias. Estima que esto ocurra en 2025 o 2026.",  family = "Roboto", fontface = "bold", hjust = 0, size = 6, color = "grey30", lineheight = 1) +
  annotate(geom = "text", x = min(bd_g_1$fecha) + ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.485), y = 70, label = "El objetivo del primero año es atender 2.4 millones de pasajeros.\nSe deben realizar 60 operaciones diarias para lograrlo.",  family = "Roboto", fontface = "bold", hjust = 1, size = 6, color = "grey30", lineheight = 1) +
  # Etiqueta marzo
  annotate(geom = "text", x = as_date("2022-03-25") + 0.5, y = 35, label = "MAR.\n2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 4, color = "grey70", lineheight = 0.8) +
  # Etiqueta abril
  annotate(geom = "text", x = as_date("2022-04-15") + 0.5, y = 35, label = "ABRIL",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta mayo
  annotate(geom = "text", x = as_date("2022-05-15") + 0.5, y = 35, label = "MAYO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
  # Etiqueta junio
  annotate(geom = "text", x = as_date("2022-06-15") + 0.5, y = 35, label = "JUNIO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta julio
  annotate(geom = "text", x = as_date("2022-07-15") + 0.5, y = 35, label = "JULIO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
  # Etiqueta agosto
  annotate(geom = "text", x = as_date("2022-08-15") + 0.5, y = 35, label = "AGOSTO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta septiembre
  annotate(geom = "text", x = as_date("2022-09-15") + 0.8, y = 85, label = "SEPTIEMBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
  # Etiqueta diciembre
  annotate(geom = "text", x = as_date("2022-10-15") + 0.5, y = 85, label = "OCTUBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta diciembre
  annotate(geom = "text", x = as_date("2022-11-14") + 0.8, y = 85, label = "NOVIEMBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
  # Etiqueta diciembre
  annotate(geom = "text", x = as_date("2022-12-15") + 0.8, y = 85, label = "DICIEMBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta enero 2023
  annotate(geom = "text", x = as_date("2023-01-16") + 0.8, y = 85, label = "ENERO\n2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 0.8) +
  # Etiqueta febrero 2023
  annotate(geom = "text", x = as_date("2023-02-13") + 0.8, y = 85, label = "FEBRERO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta marzo 2023
  annotate(geom = "text", x = as_date("2023-03-13") + 0.8, y = 85, label = "MARZO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 0.8) +
  # Etiqueta abril 2023
  annotate(geom = "text", x = as_date("2023-04-11") + 0.8, y = 85, label = "ABRIL",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  annotate(geom = "curve", x = max(bd_g_1$fecha) - ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.03), y = 107, xend = max(bd_g_1$fecha) - ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.006), yend = 118, curvature = 0.5, arrow = arrow(length = unit(0.03, "npc")), linewidth = 1, color = "grey50") +
  annotate(geom = "curve", x = min(bd_g_1$fecha) + ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.03), y = 73, xend = min(bd_g_1$fecha) + ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.006), yend = 62, arrow = arrow(length = unit(0.03, "npc")), linewidth = 1, color = "grey50") +
  scale_x_date(expand = c(0, 0), 
               date_labels = "%b", 
               breaks = c(as_date("2022-03-21"), as_date("2022-04-01"), as_date("2022-04-01"), as_date("2022-05-01"), as_date("2022-06-01"), as_date("2022-07-01"), as_date("2022-08-01"), as_date("2022-09-01"), as_date("2022-10-01"), as_date("2022-11-01"), as_date("2022-12-01"), as_date("2023-01-01"), as_date("2023-02-01"), as_date("2023-03-01"), as_date("2023-04-01")
                          # max(bd_g_1$fecha)
                          ), 
               limits = c(as_date("2022-03-20"), max(bd_g_1$fecha) + 1)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 150, 10),
                     labels = comma,
                     limits = c(-1, 132),
                     # sec.axis = dup_axis()
  ) +
  labs(title = "Número de operaciones comerciales realizadas diariamente en el\nAeropuerto Internacional Felipe Ángeles",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: Elaboración propia a partir de datos publicados por flightaware.com<br>Nota: Calculé el número de operaciones diarias a partir del número diario de vuelos comerciales que <span style='color:#fa8072;'>salieron</span> y <span style='color:#fa8072;'>llegaron</span> al AIFA desde el 21 de marzo de 2022. Considero como vuelo comercial aquellos operados por<br>Aeroméxico, Conviasa, Viva Aerobus y Volaris.") +
  tema +
  theme(plot.title = element_text(size = 38, lineheight = 1),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 12, lineheight = 1),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5),
        # axis.text.y = element_blank(),
        legend.position = c(0.111, 0.97),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/01_numero_operaciones_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 16, height = 10)



### Gráfica 2: Porcentaje de ocupación mensual en las rutas operadas desde y hacia el AIFA ----

resumen_mes_aifa

bd_g_2 <- 
  bd_aifa %>% 
  mutate(ruta = str_c("AIFA-", aeropuerto),
         ruta = case_when(ruta == "AIFA-Santo Domingo,Rep Dom" ~ "AIFA-Sto. Domingo", 
                          ruta == "AIFA-Panama" ~ "AIFA-Panamá", 
                          T ~ ruta),
         ruta = fct_relevel(ruta, "AIFA-Tijuana", "AIFA-La Paz", 
                            "AIFA-Mérida", "AIFA-Los Cabos", 
                            "AIFA-Panamá", "AIFA-Cancún",
                            "AIFA-Mexicali", "AIFA-Pto. Escondido", 
                            "AIFA-Monterrey", "AIFA-Acapulco",  
                            "AIFA-Oaxaca", "AIFA-Sto. Domingo", 
                            "AIFA-Huatulco", "AIFA-Guadalajara",
                            "AIFA-Pto. Vallarta", "AIFA-Veracruz", 
                            "AIFA-La Habana", "AIFA-Villahermosa"),
         tipo = fct_relevel(tipo, "Salidas", "Llegadas")) 

bd_g_2 %>% 
  # Media de pasajeros por vuelo en cada ruta
  mutate(media_pasajeros_x_vuelo = n_pasajeros/n_vuelos,
         # Media del porcentaje de ocupación. Solo para el AIFA 
         media_por_ocupacion = media_pasajeros_x_vuelo/capacidad_media_ruta*100) %>%
  ggplot(aes(x = fecha,
             y = media_por_ocupacion/100,
             color = tipo,
             group = tipo)) +
  geom_vline(xintercept = as_date("2022-12-16"), color = "grey90") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-11-25"), y = 0.02, label = "\'22",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 3, color = "grey50", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2023-01-05"), y = 0.02, label = "\'23",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 3, color = "grey50", lineheight = 0.8) +
  geom_hline(yintercept = c(0.50, 0.8), color = "salmon", linetype = 3) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ ruta, ncol = 6) +
  scale_x_date(breaks = c(as_date("2022-03-01"), as_date("2022-05-01"), as_date("2022-07-01"), as_date("2022-09-01"), as_date("2022-11-01"), as_date("2023-01-01"), as_date("2023-03-01")), date_labels = "%b") +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %"),
                     limits = c(0, 1.01),
                     breaks = seq(0, 1, .10),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Porcentaje de ocupación mensual en las rutas operadas desde y hacia el AIFA",
       subtitle = "Información a marzo de 2023",
       x = NULL,
       y = NULL,
       caption = "\n\nElaborado por @segasi / Datos: Estadística operacional origen-destino, AFAC, bit.ly/3CXb7UG.\nNotas: Los resultados fueron calculados a partir de la información correspondiente a los vuelos clasificados como de \"servicio regular nacional\" que salieron o llegaron a cada\n aeropuerto. *Las cifras de marzo corresponden al período del 21 al 31 de dicho mes.",
       color = NULL) + 
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 26),
        panel.border = element_rect(color = "grey70", fill = "transparent"),
        axis.text.x = element_text(size = 12, hjust = 1, vjust = 0.5, angle = 90),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 20),
        legend.position = c(0.9, -0.07),
        legend.direction = "horizontal",
        strip.text = element_text(size = 18))

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/02_porcentaje_ocupacion_mensual_por_ruta_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 17, height = 14)


# Análisis diversos
bd_g_2_analisis <- 
  bd_g_2 %>% 
  arrange(aeropuerto, tipo, fecha) %>% 
  mutate(media_pasajeros_x_vuelo = n_pasajeros/n_vuelos,
         # Media del porcentaje de ocupación. Solo para el AIFA 
         media_por_ocupacion = media_pasajeros_x_vuelo/capacidad_media_ruta*100) 

# Cambio en puntos porcentuales
bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, fecha) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  filter(fecha == max(fecha)) %>% 
  # arrange(-cambio) %>% 
  print(n= Inf)

# Número de direcciones en las que el cambio fue positivo
bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, fecha) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  filter(fecha == max(fecha),
         cambio > 0) %>% 
  count(aeropuerto, sort = T)

bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, fecha) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  filter(fecha == max(fecha),
         cambio > 0) %>% 
  arrange(-cambio) %>%
  print(n= Inf)

# Número de direcciones en las que el cambio fue negativo
bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, fecha) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  filter(fecha == max(fecha),
         cambio < 0) %>% 
  count(aeropuerto, sort = T)

bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, fecha) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  filter(fecha == max(fecha),
         cambio < 0) %>% 
  arrange(-cambio) %>%
  print(n= Inf)

bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, fecha) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  filter(fecha == max(fecha),
         cambio < 0) %>% 
  arrange(aeropuerto) %>%
  print(n= Inf)

# Mantener sólo observaciones del último mes
bd_g_2_analisis %>%
  filter(fecha == max(fecha)) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo) %>% 
  print(n= Inf)

bd_g_2_analisis %>%
  filter(fecha == max(fecha)) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(-media_por_ocupacion) %>% 
  print(n= Inf)

# Contar en cuantas rutas media_por_ocupacion > 80, por sentido del vuelo
mas_de_80_por <- 
  bd_g_2_analisis %>%
  filter(fecha == max(fecha)) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo, mes_numero) %>% 
  filter(media_por_ocupacion > 80) %>% 
  count(aeropuerto, sort = T) %>% 
  pull(var = aeropuerto)

mas_de_80_por

bd_g_2_analisis %>%
  filter(fecha == max(fecha),
         aeropuerto %in% mas_de_80_por,
         media_por_ocupacion >= 80) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(-media_por_ocupacion) %>% 
  print(n= Inf)

# Contar en cuantas rutas media_por_ocupacion > 50 & media_por_ocupacion < 80, por sentido del vuelo
entre_50_y_80_por <- 
  bd_g_2_analisis %>%
  filter(fecha == max(fecha)) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo, mes_numero) %>% 
  filter(media_por_ocupacion > 50 & media_por_ocupacion < 80) %>% 
  count(aeropuerto, sort = T) %>% 
  pull(var = aeropuerto)

entre_50_y_80_por

bd_g_2_analisis %>%
  filter(fecha == max(fecha),
         aeropuerto %in% entre_50_y_80_por & !aeropuerto %in% mas_de_80_por,
         media_por_ocupacion >= 50 & media_por_ocupacion < 80) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(-media_por_ocupacion) %>% 
  print(n= Inf)

bd_g_2_analisis %>%
  filter(fecha == max(fecha),
         aeropuerto %in% entre_50_y_80_por & !aeropuerto %in% mas_de_80_por,
         media_por_ocupacion >= 50 & media_por_ocupacion < 80) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, -media_por_ocupacion) %>% 
  print(n= Inf)

# Contar en cuantas rutas media_por_ocupacion < 50, por sentido del vuelo
bd_g_2_analisis %>%
  filter(fecha == max(fecha)) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo, mes_numero) %>% 
  filter(media_por_ocupacion <= 50) %>% 
  count(aeropuerto, sort = T)


# Mantener observaciones del todos los meses
bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo, mes_numero) %>% 
  print(n= Inf)


bd_g_2_analisis %>%
  filter(fecha == "2023-03-01") %>% 
  group_by(aeropuerto) %>% 
  summarise(tot_pasajeros = sum(x = n_pasajeros)) %>% 
  ungroup() %>% 
  filter(tot_pasajeros > 0) %>% 
  arrange(-tot_pasajeros) %>% 
  mutate(por = tot_pasajeros/sum(x = tot_pasajeros)*100,
         por_acumulado = cumsum(x = por))


## Cálculos para descripción después de esta gráfica 

# Pasajeros atendidos en vuelos internacionales 
bd_aifa %>% 
  filter(fecha == max(x = fecha),
         aeropuerto %in% c("La Habana", "Panama", "Santo Domingo,Rep Dom")) %>% 
  summarise(tot_pasajeros = sum(n_pasajeros))


# Cambio mensual en el número de pasajeros atendidos en los últimos dos meses 
bd_operaciones_todos %>% 
  filter(fecha >= "2023-02-01") %>% 
  summarise(tot_pasajeros = sum(x = n_pasajeros, na.rm = T),
            tot_vuelos = sum(x = n_vuelos, na.rm = T),
            .by =  c(fecha, apto_interes)) %>% 
  mutate(cambio_p = (tot_pasajeros - lag(tot_pasajeros))/lag(tot_pasajeros)*100,
         cambio_v = (tot_vuelos - lag(tot_vuelos))/lag(tot_vuelos)*100,
         .by =  apto_interes) %>% 
  filter(!is.na(cambio_p)) %>% 
  arrange(-cambio_p) %>% 
  print(n = Inf)


bd_operaciones_todos %>% 
  filter(fecha >= "2023-02-01") %>% 
  summarise(tot_vuelos = sum(x = n_vuelos),
            .by =  c(fecha, apto_interes)) %>% 
  mutate(cambio = (tot_vuelos - lag(tot_vuelos))/lag(tot_vuelos)*100,
         .by =  apto_interes) %>% 
  filter(!is.na(cambio)) %>%
  arrange(cambio) %>%
  print(n = Inf)


### Gráfica 3: Promedio diario de operaciones aéreas realizadas y pasajeros atendidos en cada aeropuerto de México en ___ de 2022 ---- 

# Preparar tibble

dias_en_el_mes <- 31

bd_g_3 <- 
  bd_operaciones_todos %>% 
  filter(fecha == "2023-03-01") %>% 
  group_by(apto_interes) %>% 
  summarise(n_vuelos_mes = sum(n_vuelos, na.rm = T),
            n_pasajeros_mes = sum(n_pasajeros, na.rm = T),
            kg_carga_mes = sum(kg_carga, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(media_diaria_vuelos = n_vuelos_mes/dias_en_el_mes,
         media_diaria_pasajeros = n_pasajeros_mes/dias_en_el_mes,
         media_diaria_carga = kg_carga_mes/dias_en_el_mes,
         por_diaria_vuelos = n_vuelos_mes/sum(x = n_vuelos_mes, na.rm = TRUE),
         por_diaria_pasajeros = n_pasajeros_mes/sum(x = n_pasajeros_mes, na.rm = TRUE),
         por_diaria_carga = kg_carga_mes/sum(x = kg_carga_mes, na.rm = TRUE),
         color_geom = case_when(apto_interes == "AIFA" ~ "#b5261e", 
                                apto_interes == "Toluca" ~ "steelblue",
                                T ~ "grey70"),
         alfa_geom = case_when(apto_interes == "AIFA" ~ 1, 
                               apto_interes == "Toluca" ~ 1,
                               T ~ 0.5),
         tamaño_geom = case_when(apto_interes == "AIFA" ~ 1.5, 
                                 apto_interes == "Toluca" ~ 1.5,
                                 T ~ 0.7),
         etiqueta_aifa = ifelse(apto_interes == "AIFA", str_c(round(media_diaria_vuelos, 1), " operaciones"), ""),
         etiqueta_ait = ifelse(apto_interes == "Toluca", str_c(round(media_diaria_vuelos, 1), " operaciones"), ""),
         etiqueta_aicm = ifelse(apto_interes == "AICM", str_c(comma(round(media_diaria_vuelos, 1), accuracy = 1), " operaciones"), ""),
         
         etiqueta_aifa_pasajeros = ifelse(apto_interes == "AIFA", str_c(comma(round(media_diaria_pasajeros, 1)), " pasajeros"), ""),
         etiqueta_ait_pasajeros = ifelse(apto_interes == "Toluca", str_c(comma(round(media_diaria_pasajeros, 1)), " pasajeros"), ""),
         etiqueta_aicm_pasajeros = ifelse(apto_interes == "AICM", str_c(comma(round(media_diaria_pasajeros, 1), accuracy = 1), " pasajeros"), ""))


# Tibble ordenado de acuerdo con el número de vuelos operados en el último mes
bd_g_3 %>% 
  select(1:3, 5:6, 8:9) %>% 
  arrange(-n_vuelos_mes) %>% 
  print(n = Inf)

bd_g_3 %>% 
  select(1:3, 5:6, 8:9) %>% 
  arrange(-n_pasajeros_mes) %>% 
  print(n = Inf)

# Tibble ordenado de acuerdo con el número de pasajeros atendidos en el último mes
bd_g_3 %>% 
  select(1:3, 5:6, 8:9) %>% 
  arrange(-n_pasajeros_mes) %>% 
  print(n = Inf)

# Pasajeros que abordaron un vuelo internacional
bd_orig_dest %>% 
  filter(str_detect(tipo, "Inter"),
         origen == "AIFA" | destino == "AIFA",
         mes_numero == 12) %>% 
  summarise(tot_pasajeros_intl = sum(x = n_pasajeros))

# Tema de esta gráfica
tema_g_3 <- 
  tema +
  theme(plot.title = element_text(size = 30, lineheight = 1, color = "grey35"),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 15.5, lineheight = 1),
        axis.title.x = element_text(size = 20, margin = margin(15, 0, 0, 0, unit = "pt")),
        axis.text.x = element_text(size = 15),
        plot.background = element_rect(fill = "white", color = "white")) 


# Ver valores
bd_g_3 %>% 
  select(apto_interes, media_diaria_vuelos) %>% 
  arrange(-media_diaria_vuelos) %>% 
  print(n = Inf)

# Gráfica de operaciones aéreas
g_3_a <- 
  bd_g_3 %>% 
  ggplot(aes(x = media_diaria_vuelos,
             y = fct_reorder(apto_interes, media_diaria_vuelos),
             fill = color_geom)) +
  geom_col() +
  geom_text(aes(label = etiqueta_aifa), family = "Roboto", fontface = "bold", hjust = -0.1, size = 5) +
  geom_text(aes(label = etiqueta_ait), family = "Roboto", fontface = "bold", hjust = -0.1, size = 5) +
  geom_text(aes(label = etiqueta_aicm), family = "Roboto", fontface = "bold", hjust = 1.05, size = 5, color = "white") +
  # scale_x_log10(labels = comma) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 1200, 100),
                     limits = c(-1, max(bd_g_3$media_diaria_vuelos)*1.1),
                     labels = comma) +
  scale_fill_identity() +
  labs(title = "Operaciones aéreas",
       # x = "\nPromedio  (log)",
       x = "Promedio ",
       y = NULL) +
  tema_g_3

g_3_b <- 
  bd_g_3 %>% 
  ggplot(aes(x = media_diaria_pasajeros/1,
             y = fct_reorder(apto_interes, media_diaria_pasajeros),
             fill = color_geom)) +
  geom_col() +
  geom_text(aes(label = etiqueta_aifa_pasajeros), family = "Roboto", fontface = "bold", hjust = -0.1, size = 5) +
  geom_text(aes(label = etiqueta_ait_pasajeros), family = "Roboto", fontface = "bold", hjust = -0.1, size = 5) +
  geom_text(aes(label = etiqueta_aicm_pasajeros), family = "Roboto", fontface = "bold", hjust = 1.05, size = 5, color = "white") +
  # scale_x_log10(labels = comma) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(seq(0, 125e3, 25e3)),
                     limits = c(-1, max(bd_g_3$media_diaria_pasajeros/1)*1.05),
                     labels = comma) +
  scale_fill_identity() +
  labs(title = "Pasajeros atendidos",
       x = "Promedio",
       y = NULL) +
  tema_g_3 

# Texto título general
titulo_gral <- 
  ggdraw() + 
  draw_label(
    "Promedio diario de operaciones aéreas realizadas y pasajeros\natendidos en cada aeropuerto de México en marzo de 2023", fontfamily = "Roboto Black", color = "grey25", size = 40, fontface = 'bold', x = 0, hjust = 0) +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(0, 0, 0, 7))

nota_al_pie <- 
  ggdraw() + 
  draw_label("Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG\nNota: Los promedios fueron calculados a partir de la información correspondiente a los vuelos clasificados como de \"servicio regular\" nacional o\ninternacional que salieron o llegaron a cada aeropuerto.", fontfamily = "Roboto" , x = 0, hjust = 0, size = 18, color = "grey35") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(15, 0, 0, 15))

graficas <- 
  plot_grid(g_3_a, NULL, g_3_b, ncol = 3, align = "v", rel_widths = c(1, 0.06, 1)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

plot_grid(titulo_gral, graficas, nota_al_pie, ncol = 1, rel_heights = c(0.1, 1, 0.08))

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/03_promedio_diario_operaciones_y_pasajeros_x_aeropuerto_ultimo_mes_",fecha_corte_grafica, ".png"), dpi = 200, width = 16.5, height = 20)

ggsave("03_vis/promedio_diario_operaciones_y_pasajeros_x_aeropuerto_ultimo_mes_.png", dpi = 200, width = 16.5, height = 20)


## Gráfica 4: Número de operaciones aéreas realizadas mensualmente desde/hacia el AICM, AIFA y TLC ----

# Preparar datos para las siguientes tres gráficas
bd_g_4_5_6 <- 
  bd_operaciones_todos %>% 
  filter(apto_interes %in% c("AIFA", "AICM", "Toluca")) %>% 
  group_by(apto_interes, fecha) %>% 
  summarise(n_vuelos_mes = sum(n_vuelos, na.rm = T),
            n_pasajeros_mes = sum(n_pasajeros, na.rm = T),
            kg_carga_mes = sum(kg_carga, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fecha) %>% 
  mutate(por_vuelos = n_vuelos_mes/sum(n_vuelos_mes, na.rm = T),
         por_pasajeros = n_pasajeros_mes/sum(n_pasajeros_mes, na.rm = T),
         apto_interes = fct_rev(fct_relevel(apto_interes, "AICM", "Toluca", "AIFA"))) %>%
  ungroup()

# Analizar cifras de últimos tres meses ----
bd_g_4_5_6 %>% 
  filter(fecha >= as_date("2022-12-31")) %>% 
  arrange(apto_interes)  


bd_g_4_5_6 %>% 
  filter(fecha >= as_date("2022-11-01")) %>% 
  arrange(apto_interes) %>% 
  mutate(tot_vuelos_mes = sum(n_vuelos_mes),
         .by = fecha) %>%
  filter(apto_interes == "AICM")

bd_g_4_5_6 %>% 
  filter(fecha >= as_date("2022-11-01")) %>% 
  arrange(apto_interes) %>% 
  mutate(tot_vuelos_mes = sum(n_vuelos_mes),
         .by = fecha) %>% 
  filter(apto_interes == "AIFA")

bd_g_4_5_6 %>% 
  filter(fecha >= as_date("2022-11-01")) %>% 
  arrange(apto_interes) %>% 
  mutate(tot_vuelos_mes = sum(n_vuelos_mes),
         .by = fecha) %>%
  filter(apto_interes == "Toluca")


bd_g_4_5_6 %>%
  filter(fecha >= as_date("2022-07-01")) %>% 
  print(n = Inf)


bd_g_4_5_6 %>% 
  mutate(mes = month(fecha),
         año = year(fecha)) %>% 
  filter(apto_interes == "AICM",
         mes %in% c(1:3)) %>% 
  arrange(mes, año) %>% 
  mutate(cambio_por_v = (n_vuelos_mes - lag(n_vuelos_mes))/lag(n_vuelos_mes)*100,
         cambio_por_p = (n_pasajeros_mes - lag(n_pasajeros_mes))/lag(n_pasajeros_mes)*100,
         .by = mes)
  

bd_g_4_5_6 %>% 
  summarise(tot_vuelos = sum(n_vuelos_mes),
            tot_pasajeros = sum(n_pasajeros_mes),
            .by = fecha) %>% 
  arrange(-tot_vuelos)
  # arrange(tot_pasajeros)

## Cifra de pasajeros y operaciones agregadas por mes ----
bd_g_4_5_6 %>% 
  summarise(tot_vuelos_mes = sum(x = n_vuelos_mes),
            tot_pasajeros_mes = sum(x = n_pasajeros_mes),
            .by = fecha)



# Hacer gráfica 4 ----

bd_g_4_5_6 %>% 
  ggplot(aes(x = fecha,
             y = n_vuelos_mes,
             fill = apto_interes)) +
  geom_col() +
  geom_hline(yintercept = seq(0, 35000, 5000), linetype = 3, size = 0.2, color = "white") +
  geom_vline(xintercept = as_date("2022-12-16") + 0.5, color = "grey80") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-12-01"), y = 35000, label = "2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2023-01-01"), y = 35000, label = "2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  scale_x_date(breaks = seq.Date(from = as_date("2022-01-01"), to = max(bd_g_4_5_6$fecha), by = "1 month"), date_labels = "%b") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36000) , breaks = seq(0, 35000, 5000), sec.axis = dup_axis(), labels = comma_format(big.mark = " ")) +
  scale_fill_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Número de operaciones aéreas realizadas mensualmente en los aeropuertos\nde la Cd. de México, Toluca y Felipe Ángeles, respecto al total de operaciones\nefectuadas cada mes en el Sistema Aeroportuario Metropolitano (SAM)",
       subtitle = "",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los resultados fueron calculados a partir del número mensual de vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o <span style='color:#fa8072;'>llegaron</span> a cada aeropuerto.") +
  tema +
  theme(plot.title = element_text(size = 37, lineheight = 1),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18, lineheight = 1),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.position = c(0.148, 1),
        legend.direction = "horizontal",
        legend.text = element_text(size = 22)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/04_numero_operaciones_x_aeropuertos_sma_", fecha_corte_grafica, ".png"), dpi = 200, width = 19, height = 13)



## Gráfica 5: % de operaciones aéreas realizadas mensualmente desde/hacia el AICM, AIFA y TLC ----
bd_g_4_5_6 %>% 
  ggplot(aes(x = fecha,
             y = por_vuelos,
             fill = apto_interes)) +
  geom_col() +
  geom_hline(yintercept = seq(0.1, 0.90, 0.10), linetype = 3, size = 0.2, color = "white") +
  geom_vline(xintercept = as_date("2022-12-16") + 0.5, color = "grey80") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-12-01"), y = 1.04, label = "2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2023-01-01"), y = 1.04, label = "2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  scale_x_date(breaks = seq.Date(from = as_date("2022-01-01"), to = max(bd_g_4_5_6$fecha), by = "1 month"), date_labels = "%b") +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.01, 1.06), breaks = seq(0, 1, 0.1), sec.axis = dup_axis(), labels = percent_format(accuracy = 1, suffix = " %")) +
  scale_fill_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Porcentaje de operaciones aéreas realizadas mensualmente en los aeropuertos\nde la Cd. de México, Toluca y Felipe Ángeles, respecto al total de operaciones\nefectuadas cada mes en el Sistema Aeroportuario Metropolitano (SAM)",
       subtitle = "",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los porcentajes fueron calculados a partir del número mensual de vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o <span style='color:#fa8072;'>llegaron</span> a cada aeropuerto.") +
  tema +
  theme(plot.title = element_text(size = 38, lineheight = 1),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18, lineheight = 1),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 20),
        legend.position = c(0.145, 0.99),
        legend.direction = "horizontal",
        legend.text = element_text(size = 22)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/05_porcentaje_operaciones_x_aeropuertos_sma_", fecha_corte_grafica, ".png"), dpi = 200, width = 19, height = 13)


## Gráfica 6: % de pasajeros atendidos mensualmente por el AICM, AIFA y TLC ----
bd_g_4_5_6 %>% 
  ggplot(aes(x = fecha,
             y = por_pasajeros,
             fill = apto_interes)) +
  geom_col() +
  geom_hline(yintercept = seq(0.1, 0.90, 0.10), linetype = 3, size = 0.2, color = "white") +
  geom_vline(xintercept = as_date("2022-12-16") + 0.5, color = "grey80") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-12-01"), y = 1.04, label = "2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2023-01-01"), y = 1.04, label = "2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  scale_x_date(breaks = seq.Date(from = as_date("2022-01-01"), to = max(bd_g_4_5_6$fecha), by = "1 month"), date_labels = "%b") +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.01, 1.06), breaks = seq(0, 1, 0.1), sec.axis = dup_axis(), labels = percent_format(accuracy = 1, suffix = " %")) +
  scale_fill_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Porcentaje de pasajeros atendidos mensualmente en los aeropuertos de la\nCd. de México, Toluca y Felipe Ángeles, respecto al total de pasajeros\natendidos cada mes en el Sistema Aeroportuario Metropolitano (SAM)",
       subtitle = "",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los porcentajes fueron calculados a partir del número mensual de pasajeros que abordaron vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o<br><span style='color:#fa8072;'>llegaron</span> a cada aeropuerto.") +
  tema +
  theme(plot.title = element_text(size = 38, lineheight = 1),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18, lineheight = 1),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 20),
        legend.position = c(0.145, 0.99),
        legend.direction = "horizontal",
        legend.text = element_text(size = 22)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/06_porcentaje_pasajeros_x_aeropuertos_sma_", fecha_corte_grafica, ".png"), dpi = 200, width = 19, height = 13)



### Gráfica 7: Cambio porcentual mensual del número de operaciones y de pasajeros atendidos en las rutas operadas desde y hacia el AIFA ----

# Esta gráfica fue incluida en el texto a partir de la novena actualización del análisis
bd_g_7 <- 
  resumen_mes_aifa %>% 
  mutate(cambio_por_vuelos = (n_vuelos_mes - lag(n_vuelos_mes))/lag(n_vuelos_mes),
         cambio_por_pasajeros = (n_pasajeros_mes - lag(n_pasajeros_mes))/lag(n_pasajeros_mes)) %>% 
  select(fecha, mes, mes_numero, contains("cambio")) %>% 
  pivot_longer(-c(fecha, mes, mes_numero), names_to = "tipo", values_to = "cambio") %>% 
  mutate(tipo = ifelse(str_detect(tipo, "vuelos"), "Operaciones", "Pasajeros"),
         etiqueta = tipo) 

bd_g_7 %>% 
  filter(fecha > "2022-04-01") %>% 
  ggplot(aes(x = fecha,
             y = cambio,
             color = tipo,
             group = tipo)) +
  geom_vline(xintercept = as_date("2022-12-16"), color = "grey80") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-12-01"), y = 1.25, label = "2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2023-01-01"), y = 1.25, label = "2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 9, color = "grey70", lineheight = 0.8) +
  geom_hline(yintercept = 0, color = "tomato", linetype = 3) +
  geom_point(size = 6) +
  geom_textline(aes(label = etiqueta), linewidth = 2, size = 6, hjust = 0.54) +
  geom_point(data = bd_g_7 %>% filter(tipo == "Operaciones"),
             aes(x = fecha,
                 y = cambio,
                 color = tipo,
                 group = tipo),
             size = 6) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as_date("2022-04-29"),
                                                                       as_date("2023-03-01"))) +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %"),
                     limits = c(-0.2, 1.3),
                     breaks = seq(-0.3, 1.3, .10),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = c("#b5261e", "steelblue")) +
  labs(title = "Cambio porcentual mensual del número de operaciones y de pasajeros atendidos en\nlas rutas operadas desde y hacia el AIFA",
       subtitle = "Información a marzo de 2023",
       x = NULL,
       y = NULL,
       caption = "\n\nElaborado por @segasi / Datos: Estadística operacional origen-destino, AFAC, bit.ly/3CXb7UG.",
       color = NULL) + 
  tema +
  theme(plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 26),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 20),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 18))

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/07_cambio_porcentual_operaciones_pasajeros_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 16, height = 9)


### Gráfica 8: Pasajeros atendidos mensualmente en las rutas operadas desde y hacia el AIFA ----

# Preparar datos para las siguientes tres gráficas
bd_g_8_9 <- 
  bd_aifa %>% 
  summarise(tot_pasajeros = sum(x = n_pasajeros), 
            .by = c(fecha, aeropuerto)) %>% 
  complete(fecha, aeropuerto) %>% 
  arrange(aeropuerto, fecha) %>% 
  mutate(tot_pasajeros = if_else(is.na(tot_pasajeros), 0, tot_pasajeros),
         n_pas_ultimo_mes = if_else(fecha == max(x = fecha), tot_pasajeros, NA),
         n_pas_ultimo_mes = if_else(fecha == max(x = fecha) & aeropuerto == "Villahermosa", 220, n_pas_ultimo_mes)) %>% 
  fill(n_pas_ultimo_mes, .direction = "up") %>%
  mutate(aeropuerto = fct_reorder(aeropuerto, n_pas_ultimo_mes),
         colores_areas = case_when(aeropuerto == "Cancún" ~ "Cancún",
                                   aeropuerto == "Guadalajara" ~ "Guadalajara",
                                   aeropuerto == "Monterrey" ~ "Monterrey",
                                   aeropuerto == "Mérida" ~ "Mérida",
                                   aeropuerto == "Tijuana" ~ "Tijuana",
                                   aeropuerto == "Pto. Vallarta" ~ "Pto. Vallarta",
                                   aeropuerto == "Oaxaca" ~ "Oaxaca",
                                   .default = "Otros"),
         colores_areas = fct_reorder(colores_areas, n_pas_ultimo_mes)) %>% 
  mutate(por_pasajeros = tot_pasajeros/sum(x = tot_pasajeros),
         .by = fecha) 

# Revisar cifras del último mes
bd_g_8_9 %>% 
  filter(fecha == max(x = fecha)) %>% 
  arrange(-tot_pasajeros)

bd_g_8_9 %>% 
  filter(fecha == max(x = fecha)) %>% 
  arrange(-por_pasajeros)

## Hacer gráfica 8 ----
bd_g_8_9 %>% 
  ggplot(aes(x = fecha, 
             y = tot_pasajeros/1000,
             group = aeropuerto,
             fill = colores_areas)) +
  geom_area(color = "white") +
  # facet_wrap(~ aeropuerto) # Código para revisar orden de aeropuertos
  geom_vline(xintercept = as_date("2022-12-16") + 0.5, color = "white") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-12-07"), y = 5, label = "2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 6, color = "grey80", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2022-12-25"), y = 5, label = "2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 6, color = "grey80", lineheight = 0.8) +
  # Etiquetas de los aeropuertos
  annotate(geom = "text", x = as_date("2022-11-22"),
           y = c(28, 65, 92.5, 111, 126, 139, 152),
           label = c("Cancún", "Guadalajara", "Monterrey", "Mérida", "Tijuana", "Pto. Vallarta", "Oaxaca"),
           family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 0.8) +
  scale_x_date(breaks = seq.Date(from = as_date("2022-01-01"), to = max(bd_aifa$fecha), by = "1 month"),
               limits = c(min(bd_aifa$fecha) - 3, max(bd_aifa$fecha) + 3),
               date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 250, by = 25),
                     labels = comma,
                     expand = c(0, 0), sec.axis = dup_axis()) +
  scale_fill_manual(values = rev(c("#982019", "#d22c23", "#de4239", "#e35d56", "#e87973", "#ed9590", "#f1b1ae", "grey80"))) +
  labs(title = "Pasajeros atendidos mensualmente en las rutas operadas desde y hacia el AIFA",
       subtitle = "Cifras en miles, actualizadas a marzo de 2023",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los resultados fueron calculados a partir del número mensual de vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o <span style='color:#fa8072;'>llegaron</span> del AIFA.") +
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.caption = element_markdown(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.position = "none")

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/08_numero_pasajeros_por_ruta_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 16.5, height = 9)


### Gráfica 9: Porcentaje de pasajeros atendidos mensualmente en las rutas operadas desde y hacia el AIFA ----
bd_g_8_9 %>% 
  ggplot(aes(x = fecha, 
             y = por_pasajeros,
             group = aeropuerto,
             fill = colores_areas
  )) +
  geom_area(color = "white") +
  geom_hline(yintercept = seq(0, 1, 0.1), linetype = 3, size = 0.15, color = "white") +
  geom_vline(xintercept = as_date("2022-12-16") + 0.5, color = "white") +
  # Etiqueta 2022
  annotate(geom = "text", x = as_date("2022-12-07"), y = 0.05, label = "2022",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 6, color = "grey80", lineheight = 0.8) +
  # Etiqueta 2023
  annotate(geom = "text", x = as_date("2022-12-25"), y = 0.05, label = "2023",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 6, color = "grey80", lineheight = 0.8) +
  # Etiquetas de los aeropuertos
  annotate(geom = "text", x = as_date("2022-03-06"), 
           y = c(0.14, 0.33, 0.53, 0.65, 0.83), 
           label = c("Cancún", "Guadalajara", "Monterrey", "Mérida", "Tijuana"),
           family = "Roboto", fontface = "bold", hjust = 0, size = 7, color = "white", lineheight = 0.8) +
  annotate(geom = "text", x = as_date("2023-02-05"), 
           y = c(0.625, 0.742), 
           label = c("Pto. Vallarta", "Oaxaca"),
           family = "Roboto", fontface = "bold", hjust = 0.5, size = 6, color = "white", lineheight = 0.8) +
  scale_x_date(breaks = seq.Date(from = as_date("2022-01-01"), to = max(bd_aifa$fecha), by = "1 month"),
               limits = c(min(bd_aifa$fecha) - 3, max(bd_aifa$fecha) + 3),
               date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1),
                     labels = percent_format(),
                     expand = c(0, 0), sec.axis = dup_axis()) +
  scale_fill_manual(values = rev(c("#982019", "#d22c23", "#de4239", "#e35d56", "#e87973", "#ed9590", "#f1b1ae", "grey80"))) +
  labs(title = "Porcentaje de pasajeros atendidos mensualmente en las rutas operadas\ndesde y hacia el AIFA",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los resultados fueron calculados a partir del número mensual de vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o <span style='color:#fa8072;'>llegaron</span> del AIFA.") +
  tema +
  theme(plot.title = element_text(size = 34),
        plot.subtitle = element_text(size = 26),
        plot.caption = element_markdown(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.position = "none")

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/09_porcentaje_pasajeros_por_ruta_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 16, height = 9)


bd_g_8_9 %>% 
  print(n = Inf)

