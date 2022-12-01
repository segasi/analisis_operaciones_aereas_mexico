### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R") 

### Importar y procesar las bases de datos ----
source("02_codigo/01_importar_preparar_bd.R") 

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
  print(n = Inf)


# Promedio mensual de operaciones diarias
bd_g_1  %>% 
  mutate(mes = month(fecha)) %>% 
  group_by(mes) %>% 
  summarise(media_operaciones = mean(num_operaciones)) %>%
  ungroup() %>% 
  print(n = Inf)

# Promedio semanal de operaciones diarias
bd_g_1  %>% 
  group_by(semana) %>% 
  summarise(media_operaciones = mean(num_operaciones)) %>%
  ungroup() %>% 
  print(n = Inf)


# Gráfica
bd_g_1 %>%
  ggplot(aes(x = fecha,
             y = num_operaciones)) +
  annotate(geom = "rect", xmin = as_date("2022-04-01") - 0.5, xmax = as_date("2022-04-30") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-06-01") - 0.5, xmax = as_date("2022-06-30") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-08-01") - 0.5, xmax = as_date("2022-08-31") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate(geom = "rect", xmin = as_date("2022-10-01") - 0.5, xmax = as_date("2022-10-31") + 0.5, ymin = 0, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_col(fill = "#b5261e") +
  # geom_text(aes(label = num_operaciones), color = "white", vjust = 1.5, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = 60, color = "steelblue") +
  geom_hline(yintercept = 120, color = "salmon") +
  annotate(geom = "text", x = max(bd_g_1$fecha) - ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.455), y = 105, label = "De acuerdo con el General Isidoro Pastor, para que el AIFA\nllegue a su punto de equilibrio debe tener 120 operaciones\ndiarias. Estima que esto ocurra en 2025 o 2026.",  family = "Roboto", fontface = "bold", hjust = 0, size = 6, color = "grey30", lineheight = 1) +
  annotate(geom = "text", x = min(bd_g_1$fecha) + ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.5), y = 70, label = "El objetivo del primero año es atender 2.4 millones de pasajeros.\nSe deben realizar 60 operaciones diarias para lograrlo.",  family = "Roboto", fontface = "bold", hjust = 1, size = 6, color = "grey30", lineheight = 1) +
  # Etiqueta marzo
  annotate(geom = "text", x = as_date("2022-03-25") + 0.5, y = 35, label = "MARZO",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
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
  annotate(geom = "text", x = as_date("2022-09-15") + 0.8, y = 75, label = "SEPTIEMBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
  # Etiqueta octubre
  annotate(geom = "text", x = as_date("2022-10-15") + 0.5, y = 75, label = "OCTUBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "white", lineheight = 1) +
  # Etiqueta noviembre
  annotate(geom = "text", x = as_date("2022-11-14") + 0.8, y = 75, label = "NOVIEMBRE",  family = "Roboto", fontface = "bold", hjust = 0.5, size = 5, color = "grey70", lineheight = 1) +
  annotate(geom = "curve", x = max(bd_g_1$fecha) - ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.03), y = 107, xend = max(bd_g_1$fecha) - ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.006), yend = 118, curvature = 0.5, arrow = arrow(length = unit(0.03, "npc")), size = 1, color = "grey50") +
  annotate(geom = "curve", x = min(bd_g_1$fecha) + ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.03), y = 73, xend = min(bd_g_1$fecha) + ((max(bd_g_1$fecha) - min(bd_g_1$fecha)) * 0.006), yend = 62, arrow = arrow(length = unit(0.03, "npc")), size = 1, color = "grey50") +
  scale_x_date(expand = c(0, 0), 
               date_labels = "%b-%d", 
               breaks = c(as_date("2022-03-21"), as_date("2022-04-01"), as_date("2022-04-15"), as_date("2022-04-01"), as_date("2022-05-01"), as_date("2022-05-15"), as_date("2022-06-01"), as_date("2022-06-15"),  as_date("2022-07-01"), as_date("2022-07-15"), as_date("2022-08-01"), as_date("2022-08-15"), as_date("2022-09-01"), as_date("2022-09-15"), as_date("2022-10-01"), as_date("2022-10-15"), as_date("2022-11-01"), as_date("2022-11-15"),
                          max(bd_g_1$fecha)), 
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
  filter(mes_numero > 2 & mes_numero < 11) %>% 
  mutate(ruta = str_c("AIFA-", aeropuerto),
         ruta = case_when(ruta == "AIFA-Santo Domingo,Rep Dom" ~ "AIFA-Sto. Domingo (RD)", 
                          ruta == "AIFA-Panama" ~ "AIFA-Panamá (Panamá)", 
                          T ~ ruta),
         ruta = fct_relevel(ruta, "AIFA-Tijuana", "AIFA-Mexicali",  "AIFA-Panamá (Panamá)", "AIFA-La Paz", "AIFA-Cancún", "AIFA-Los Cabos", "AIFA-Sto. Domingo (RD)", "AIFA-Guadalajara", "AIFA-Mérida", "AIFA-Oaxaca", "AIFA-Huatulco", "AIFA-Pto. Escondido", "AIFA-Veracruz", "AIFA-Monterrey",  "AIFA-Pto. Vallarta", "AIFA-Acapulco", "AIFA-Villahermosa"),
         tipo = fct_relevel(tipo, "Salidas", "Llegadas")) 

# Análisis diversos
bd_g_2_analisis <- 
  bd_g_2 %>% 
  arrange(aeropuerto, tipo, mes) %>% 
  mutate(media_pasajeros_x_vuelo = n_pasajeros/n_vuelos,
         # Media del porcentaje de ocupación. Solo para el AIFA 
         media_por_ocupacion = media_pasajeros_x_vuelo/capacidad_media_ruta*100) 

# Cambio en puntos porcentuales
bd_g_2_analisis %>%
  filter(str_detect(mes, "Junio|Julio|Agosto")) %>% 
  select(-c(7, 8, 10)) %>%
  arrange(aeropuerto, tipo, mes_numero) %>% 
  group_by(aeropuerto, tipo) %>%
  mutate(cambio = media_por_ocupacion - lag(media_por_ocupacion)) %>% 
  ungroup() %>% 
  # arrange(-cambio) %>% 
  print(n= Inf)

# Mantener sólo observaciones del último mes
bd_g_2_analisis %>%
  filter(str_detect(mes, "Sept|Oct")) %>% 
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo, mes_numero) %>% 
  print(n= Inf)
s
# Mantener observaciones del todos los meses
bd_g_2_analisis %>%
  select(-c(7, 8, 10)) %>% 
  arrange(aeropuerto, tipo, mes_numero) %>% 
  print(n= Inf)


bd_g_2_analisis %>%
  filter(str_detect(mes, "Oct")) %>% 
  group_by(aeropuerto) %>% 
  summarise(tot_pasajeros = sum(x = n_pasajeros)) %>% 
  ungroup() %>% 
  filter(tot_pasajeros > 0) %>% 
  arrange(-tot_pasajeros) %>% 
  mutate(por = tot_pasajeros/sum(x = tot_pasajeros)*100,
         por_acumulado = cumsum(x = por))
  

bd_g_2 %>% 
  # Media de pasajeros por vuelo en cada ruta
  mutate(media_pasajeros_x_vuelo = n_pasajeros/n_vuelos,
         # Media del porcentaje de ocupación. Solo para el AIFA 
         media_por_ocupacion = media_pasajeros_x_vuelo/capacidad_media_ruta*100) %>%
  mutate(mes_breve = case_when(mes == "Enero" ~ "Ene.",
                               mes == "Febrero" ~ "Feb.",
                               mes == "Marzo" ~ "Mar.*",
                               mes == "Abril" ~ "Abr.",
                               mes == "Mayo" ~ "May.",
                               mes == "Junio" ~ "Jun.",
                               mes == "Julio" ~ "Jul.",
                               mes == "Agosto" ~ "Ago.",
                               mes == "Septiembre" ~ "Sep.",
                               mes == "Octubre" ~ "Oct.",
                               mes == "Noviembre" ~ "Nov.",
                               mes == "Diciembre" ~ "Dic.",
                               T ~ mes),
         # mes = ifelse(mes == "Marzo", str_c(mes, "*"), mes),
         mes_breve = fct_reorder(mes_breve, mes_numero)) %>% 
  ggplot(aes(x = mes_breve,
             y = media_por_ocupacion/100,
             color = tipo,
             group = tipo)) +
  geom_hline(yintercept = c(0.50, 0.8), color = "salmon", linetype = 3) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  facet_wrap(~ ruta, ncol = 5) +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %"),
                     limits = c(0, 1.01),
                     breaks = seq(0, 1, .10),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Porcentaje de ocupación mensual en las rutas operadas desde y hacia el AIFA",
       subtitle = "Información a octubre de 2022",
       x = NULL,
       y = NULL,
       caption = "\n\nElaborado por @segasi / Datos: Estadística operacional origen-destino, AFAC, bit.ly/3CXb7UG.\nNotas: Los resultados fueron calculados a partir de la información correspondiente a los vuelos clasificados como de \"servicio regular nacional\" que salieron o llegaron a cada\n aeropuerto. *Las cifras de marzo corresponden al período del 21 al 31 de dicho mes.",
       color = NULL) + 
  tema +
  theme(plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 26),
        panel.border = element_rect(color = "grey70", fill = "transparent"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 20),
        legend.position = c(0.9, -0.07),
        legend.direction = "horizontal",
        strip.text = element_text(size = 18))

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/02_porcentaje_ocupacion_mensual_por_ruta_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 17, height = 14)




### Gráfica 3: Promedio diario de operaciones aéreas realizadas y pasajeros atendidos en cada aeropuerto de México en ___ de 2022 ---- 

# Preparar tibble

dias_en_el_mes <- 31

bd_g_3 <- 
  bd_operaciones_todos %>% 
  filter(mes == "Octubre") %>% 
  group_by(apto_interes) %>% 
  summarise(n_vuelos_mes = sum(n_vuelos),
            n_pasajeros_mes = sum(n_pasajeros),
            kg_carga_mes = sum(kg_carga)) %>% 
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

# Tibble ordenado de acuerdo con el número de pasajeros atendidos en el último mes
bd_g_3 %>% 
  select(1:3, 5:6, 8:9) %>% 
  arrange(-n_pasajeros_mes) %>% 
  print(n = Inf)


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
                     limits = c(-1, max(bd_g_3$media_diaria_pasajeros/1)*1.01),
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
    "Promedio diario de operaciones aéreas realizadas y pasajeros\natendidos en cada aeropuerto de México en octubre de 2022", fontfamily = "Roboto Black", color = "grey25", size = 40, fontface = 'bold', x = 0, hjust = 0) +
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
  group_by(apto_interes, mes, mes_numero) %>% 
  summarise(n_vuelos_mes = sum(n_vuelos),
            n_pasajeros_mes = sum(n_pasajeros),
            kg_carga_mes = sum(kg_carga)) %>% 
  ungroup() %>% 
  group_by(mes) %>% 
  mutate(por_vuelos = n_vuelos_mes/sum(n_vuelos_mes),
         por_pasajeros = n_pasajeros_mes/sum(n_pasajeros_mes),
         apto_interes = fct_rev(fct_relevel(apto_interes, "AICM", "Toluca", "AIFA"))) %>%
  ungroup()

# Analizar cifras de últimos tres meses ----
bd_g_4_5_6 %>% 
  filter(between(x = mes_numero, left = 5, right = 10)) %>% 
  arrange(apto_interes) %>% 
  group_by(mes) %>% 
  mutate(tot_vuelos_mes = sum(n_vuelos_mes)) %>% 
  ungroup() %>% 
  filter(apto_interes == "Toluca")
  

bd_g_4_5_6 %>% 
  filter(between(x = mes_numero, left = 8, right = 10))

# Hacer gráfica 4 ----

bd_g_4_5_6 %>% 
  ggplot(aes(x = mes,
             y = n_vuelos_mes,
             fill = apto_interes)) +
  geom_col() +
  geom_hline(yintercept = seq(0, 35000, 5000), linetype = 3, size = 0.2, color = "white") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 35000) , breaks = seq(0, 35000, 5000), sec.axis = dup_axis(), labels = comma_format(big.mark = " ")) +
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
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 18),
        legend.position = c(0.88, 1.06),
        legend.direction = "horizontal",
        legend.text = element_text(size = 22)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/04_numero_operaciones_x_aeropuertos_sma_", fecha_corte_grafica, ".png"), dpi = 200, width = 19, height = 12)



## Gráfica 5: % de operaciones aéreas realizadas mensualmente desde/hacia el AICM, AIFA y TLC ----
bd_g_4_5_6 %>% 
  ggplot(aes(x = mes,
             y = por_vuelos,
             fill = apto_interes)) +
  geom_col() +
  geom_hline(yintercept = seq(0.1, 0.90, 0.10), linetype = 3, size = 0.2, color = "white") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.01, 1,01), breaks = seq(0, 1, 0.1), sec.axis = dup_axis(), labels = percent_format(accuracy = 1, suffix = " %")) +
  scale_fill_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Porcentaje de operaciones aéreas realizadas mensualmente en los aeropuertos\nde la Cd. de México, Toluca y Felipe Ángeles, respecto al total de operaciones\nefectuadas cada mes en el Sistema Aeroportuario Metropolitano (SAM)",
       subtitle = "",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los porcentajes fueron calculados a partir del número mensual de vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o <span style='color:#fa8072;'>llegaron</span> a cada aeropuerto.") +
  tema +
  theme(plot.title = element_text(size = 37, lineheight = 1),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18, lineheight = 1),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        legend.position = c(0.88, 1.06),
        legend.direction = "horizontal",
        legend.text = element_text(size = 22)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/05_porcentaje_operaciones_x_aeropuertos_sma_", fecha_corte_grafica, ".png"), dpi = 200, width = 19, height = 12)


## Gráfica 6: % de pasajeros atendidos mensualmente por el AICM, AIFA y TLC ----
bd_g_4_5_6 %>% 
  ggplot(aes(x = mes,
             y = por_pasajeros,
             fill = apto_interes)) +
  geom_col() +
  geom_hline(yintercept = seq(0.1, 0.90, 0.10), linetype = 3, size = 0.2, color = "white") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.01, 1,01), breaks = seq(0, 1, 0.1), sec.axis = dup_axis(), labels = percent_format(accuracy = 1, suffix = " %")) +
  scale_fill_manual(values = c("#b5261e", "steelblue", "grey70")) +
  labs(title = "Porcentaje de pasajeros atendidos mensualmente en los aeropuertos de la\nCd. de México, Toluca y Felipe Ángeles, respecto al total de pasajeros\natendidos cada mes en el Sistema Aeroportuario Metropolitano (SAM)",
       subtitle = "",
       x = NULL,
       y = NULL,
       fill = NULL,
       caption = "<br>Elaborado por @segasi / Datos: AFAC, bit.ly/3CXb7UG<br>Nota: Los porcentajes fueron calculados a partir del número mensual de pasajeros que abordaron vuelos clasificados como de \"servicio regular nacional\" que <span style='color:#fa8072;'>salieron</span> o<br><span style='color:#fa8072;'>llegaron</span> a cada aeropuerto.") +
  tema +
  theme(plot.title = element_text(size = 37, lineheight = 1),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_markdown(size = 18, lineheight = 1),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        legend.position = c(0.88, 1.06),
        legend.direction = "horizontal",
        legend.text = element_text(size = 22)) 

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/06_porcentaje_pasajeros_x_aeropuertos_sma_", fecha_corte_grafica, ".png"), dpi = 200, width = 19, height = 12)





### Gráfica 7: Cambio porcentual mensual del número de operaciones y de pasajeros atendidos en las rutas operadas desde y hacia el AIFA ----

# Esta gráfica fue incluida en el texto a partir de la novena actualización del análisis
resumen_mes_aifa %>% 
  mutate(mes = as.character(mes),
         mes = fct_relevel(mes, "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre"),
         cambio_por_vuelos = (n_vuelos_mes - lag(n_vuelos_mes))/lag(n_vuelos_mes),
         cambio_por_pasajeros = (n_pasajeros_mes - lag(n_pasajeros_mes))/lag(n_pasajeros_mes)) %>% 
  select(mes, mes_numero, contains("cambio")) %>% 
  filter(mes_numero > 4) %>% 
  pivot_longer(-c(mes, mes_numero), names_to = "tipo", values_to = "cambio") %>% 
  mutate(tipo = ifelse(str_detect(tipo, "vuelos"), "Operaciones", "Pasajeros"),
         etiqueta = ifelse(mes_numero == max(mes_numero), tipo, "")) %>% 
  ggplot(aes(x = mes,
             y = cambio,
             color = tipo,
             group = tipo)) +
  geom_hline(yintercept = 0, color = "tomato", linetype = 3) +
  geom_point(size = 6) +
  geom_line(size = 2) +
  geom_text(aes(label = etiqueta), hjust = 0, vjust = -0.8, color = "black", fontface = "bold", size = 6,  family = "Didact Gothic Regular") +
  scale_y_continuous(labels = percent_format(accuracy = 1, suffix = " %"),
                     limits = c(-0.2, 1.3),
                     breaks = seq(-0.3, 1.3, .10),
                     sec.axis = dup_axis()) +
  scale_color_manual(values = c("#b5261e", "steelblue")) +
  labs(title = "Cambio porcentual mensual del número de operaciones y de pasajeros atendidos en\nlas rutas operadas desde y hacia el AIFA",
       subtitle = "Información a octubre de 2022",
       x = NULL,
       y = NULL,
       caption = "\n\nElaborado por @segasi / Datos: Estadística operacional origen-destino, AFAC, bit.ly/3CXb7UG.",
       color = NULL) + 
  tema +
  theme(plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 26),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 20),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 18))

ggsave(str_c("03_vis/graficas_", fecha_corte_grafica, "/07_cambio_porcentual_operaciones_pasajeros_aifa_",fecha_corte_grafica, ".png"), dpi = 200, width = 16, height = 9)

