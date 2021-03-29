# Freelos
Analisis de Freelos de la Encuesta de Sueldos Kiwi2020

library(tidyverse)      # Transformar y limpiar datos
library(googlesheets4)  # Leer datos desde Google Sheets
library(gargle)         # Corregir lectura de símbolos especiales desde Google Sheets
library(gt)             # Dar formato a las tablas
library(extrafont)      # Permite utilizar otras fuentes en los gráficos y salidas
library(ggthemes)       # Amplía las posibilidades estéticas de ggplot2
library(scales)         # Permite cambiar los formatos de decimales, de porcentajes, etc.
library(ggalt)          # Nuevos tipos de geom para ggplot2. Para realizar el gráfico de gap salarial
library(funModeling)    # Para explorar datos y modelos
library(forcats)
library(DT)




options(scipen = 999)   # Modifica la visualización de los ejes numérico a valores nominales

loadfonts(quiet = TRUE) # Permite cargar en R otros tipos de fuentes.

# Estilo limpio sin líneas de fondo
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#FBFCFC"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia horizontales en gris claro
estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.y = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris
genero3 <- c("#8624F5","#FFD129", "#1FC3AA")

colores <-  c("#8624F5", "#1FC3AA")

azul <- "#344D7E"
verde <-  "#4A9FC7"
rosa1 <- "#B95192"
rosa2 <- "#EE5777"
naranja <- "#FF764C"
amarillo <- "#FFA600"
gris <- "#75838F"
lila <- "#755395"
rojo <- "#943126"

col4 <- c(azul, lila, rosa1, rosa2)
col5 <- c(azul, lila, rosa1, rosa2, naranja)
col6 <- c(azul, lila, rosa1, rosa2, naranja, amarillo)

# Creo un objeto con un texto que se va a repetir mucho a lo largo del análisis
fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam"

# Creo objetos para formatear las etiquetas numéricas de los ejes x e y
eje_x_n <- scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

eje_y_n <- scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


# Carga de datos

original <- sheets_read("1aeuu9dVfN42EjyvbmhEcsf0ilSz2DiXU-0MpnF896ss")

# Preprocesamiento 
kiwi <- original

limpios <- make.names(colnames(kiwi))
colnames(kiwi) <- limpios

rm(limpios)
kiwi <- kiwi %>% 
  select(-X.Querés.contestar.más.preguntas....31, 
         -X.Querés.contestar.más.preguntas....42) %>% 
  rename(genero = Género,
         genero_diverso = X.Te.identificás.como.LGBT...lesbiana..gay..bisexual..transexual..otra.minoría.sexual..,
         edad = Edad,
         discapacidad = X.Tenés.alguna.discapacidad.,
         nivel_formacion = `Máximo.nivel.de.formación`,
         carrera_grado = X.Qué.carrera.de.grado.estudiaste.,
         tipo_universidad = X.En.qué.tipo.de.universidad.estudiaste.tu.carrera.de.grado.,
         pais = País.en.el.que.trabajas,
         provincia = Provincia.donde.trabajas,
         trabajo = Trabajo,
         rubro = Rubro.de.la.empresa,
         dotacion = X.Cuántos.empleados.tiene.la.empresa.,
         origen_capital = Origen.del.capital,
         dotacion_rh = X.Cuántas.personas.integran.el.área.de.RRHH.,
         puesto = X.En.qué.puesto.trabajás.,
         tipo_contratacion = Tipo.de.contratación,
         funcion_rh = X.Cuál.es.tu.función.principal.en.RRHH.,
         personas_a_cargo = "X.Cuántas.personas.tenés.a.cargo...poné.0.si.no.tenés.gente.a.cargo.",
         anios_en_empresa = "X.Hace.cuántos.años.trabajas.en.la.empresa.donde.estás...0.para.menos.de.un.año.",
         anios_en_puesto = "X.Hace.cuántos.años.estás.en.tu.puesto.actual...0.para.menos.de.un.año.",
         anios_experiencia = X.Cuántos.años.de.experiencia.tenés.en.RRHH.,
         sueldo_bruto = X.Cuál.es.tu.remuneración.BRUTA.MENSUAL.en.tu.moneda.local...antes.de.impuestos.y.deducciones.,
         beneficios = X.Qué.beneficios.tenés.,
         bono = X.Recibís.bonos.,
         ajuste = X.Tuviste.ajustes.por.inflación.en.2020.,
         ajuste_porcentaje = X.Cuál.fue.el.porcentaje.de.aumento.acumulado.que.tuviste.en.2020.,
         ajuste_mes = Mes.del.último.ajuste,
         otros_proyectos = X.Trabajás.en.proyectos.independientes.además.de.tu.empleo.,
         erp = X.Qué.sistema.de.gestión.de.RRHH.usan.en.tu.empresa.,
         nombre_area = X.Cómo.se.llama.el.área.en.tu.empresa.,
         mate = X.Se.podía.tomar.mate.en.las.oficinas.de.tu.empresa...antes.del.COVID.19.,
         idioma_exigencia = X.Te.exigieron.saber.un.idioma.extranjero..inglés..portugués..etc...para.entrar.a.trabajar.en.tu.empresa.,
         idioma_porcentaje = X.Qué.porcentaje.del.tiempo.usas.el.idioma.extranjero.en.tu.puesto.actual.,
         contactos_linkedin = "X.Cuántos.contactos.tenés.en.LinkedIn...poné.0.si.no.tenés.cuenta.de.LinkedIn.",
         satisfaccion = X.Qué.tan.satisfecho.estás.con.tu.empresa.,
         busqueda = X.Estás.buscando.trabajo.,
         beneficios_expectativa = X.Qué.beneficios.te.gustaría.tener.,
         rh_una_palabra = Definí.a.RRHH.con.una.sola.palabra,
         pregunta_bizarra = X.Cuál.es.la.pregunta.más.bizarra.que.te.han.hecho.has.hecho.en.una.entrevista.,
         teletrabajo = X.Estás.trabajando.desde.tu.casa.,
         elementos = X.Qué.elementos.te.proveyó.la.empresa.para.que.puedas.trabajar.desde.tu.casa.,
         valoracion_gestion_empresa = X.Cómo.valorarías.la.gestión.de.tu.empresa.en.este.nuevo.contexto.,
         registro_fiscal = X.Cómo.estás.registrado.a.fiscalmente.,
         anios_freelance = X.Hace.cuántos.años.trabajás.como.freelance.,
         lugar_trabajo = X.Dónde.trabajás.habitualmente...sin.considerar.la.coyuntura.por.COVID.19.,
         exporta = X.Exportás.tus.servicios.,
         medio_pago_exterior = Si.exportás.servicios...a.través.de.qué.medios.de.pago.recibís.los.pagos.del.exterior.,
         cuotas = X.Aceptás.pagos.en.cuotas.,
         colaboracion_freelance = X.Trabajás.con.otros.freelancers.de.tu.mismo.rubro.,
         servicio_busqueda = X.Tu.servicio.principal.está.relacionado.con.búsqueda.y.selección.,
         busqueda_it = X.Te.dedicás.principalmente.a.realizar.búsquedas.de.IT.Tecnología.,
         trabajo_a_riesgo =X.Trabajás.a.riesgo.,
         coeficiente = X.Cuál.es.el.coeficiente.que.cobrás.por.tus.servicios.,
         base_coeficiente = El.coeficiente.lo.calculás.sobre.,
         garantia = X.Ofrecés.garantía.,
         servicio_principal = X.Cuál.es.el.servicio.principal.que.brindas...si.brindás.más.de.un.servicio..elegí.el.que.más.ingresos.genere.,
         valor_hora = X.Cuál.es.el.valor.hora.promedio.que.ofrecés...moneda.local.)

# Base de freelancers
freelo <- kiwi %>% 
  filter(trabajo == "Freelance")

#### freelance####

# Selecciono las variables a trabajar, y cambio el formato de "años de freelance"

freelo2<- freelo %>% 
  select(genero, genero_diverso, nivel_formacion, carrera_grado, tipo_universidad, pais, provincia, anios_freelance) %>% 
  mutate(anios_freelance = as.numeric(unlist(anios_freelance)))

#Trabajaremos solo con Argentina porque el resto no es representativo
# no se como filtrar solo fem/masc, para limpiarlo desde aca


freelo2<- freelo2 %>% 
  filter(pais=="Argentina")


#Distribucion en  el pais


freelo2 %>%    
  select(provincia) %>% 
  group_by(provincia) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)*100) %>% 
  filter(n > 3) %>% #para sacar lo no  representataivo
  arrange(-n) %>% 
  gt() %>% 
  tab_header(title = "Distribución por provincia") %>% 
  tab_source_note(source_note = fuente)%>% 
  cols_label(provincia = "Provincia", n="Cantidad", freq="Freq") # no se como sacarle decimales


#Recorte genero. La participación según el género entre freelance:

divF <- freelo2 %>% 
  select(genero, genero_diverso, nivel_formacion, carrera_grado, tipo_universidad, anios_freelance) %>% 
  mutate(genero = factor(genero, 
                         levels = c("Femenino", "Masculino", "Género Diverso",
                                    "Prefiero no responder"))) %>% 
  group_by(genero) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(n > 3) %>% #para sacar los NA y Prefiero no respo. que no es representataivo
  arrange(-n)


gt(divF) %>% 
  tab_header(title = "Distribución por Género") %>% 
  tab_source_note(source_note = fuente) %>% 
  cols_label(genero = "Genero", n="Cantidad", freq="Freq") # no se como sacarle decimales

# Compute the cumulative percentages (top of each rectangle)
divF$ymax <- cumsum(divF$freq)

# Compute the bottom of each rectangle
divF$ymin <- c(0, head(divF$ymax, n=-1))

# Compute label position
divF$labelPosition <- (divF$ymax + divF$ymin) / 2

# Compute a good label
divF$label <- paste0(divF$genero, "\n Cant: ", divF$n)

#Representacion grafica

ggplot(divF, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=genero)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +# Try to remove that to see how to make a pie chart
  scale_fill_manual(values = c("#8624F5",  "#1FC3AA", "#FFD129","#75838F")) +
  theme_void() +
  theme(legend.position = "right",
        panel.background = element_blank(),
        text = element_text(family = "Roboto")) +
  labs(title = "Cantidad de respuestas según género",
       fill = "Género", 
       caption = fuente)


#Educación

freelo2 <- freelo2 %>% 
  filter(nivel_formacion != "Secundario en curso") %>% 
  mutate(nivel_formacion = fct_collapse(nivel_formacion, "Universitario completo" = c("Maestría abandonada"),
                                        "Secundario completo" = c("Terciario abandonado", "Terciario en curso", 
                                                                  "Universitario abandonado"),
                                        "Maestría completa" = c("Doctorado en curso"))) %>% 
  
  
  # Respuestas por Nivel de Formación (falta ordenarlo)
  
  educF <- freelo2 %>% 
  select(genero, genero_diverso, nivel_formacion, carrera_grado, tipo_universidad,anios_freelance)


educF%>% 
  mutate(cuenta = 1) %>% 
  group_by(nivel_formacion) %>% 
  summarise(Cuenta = sum(cuenta)) %>% 
  arrange(-Cuenta) %>% 
  gt() %>% 
  tab_header(title = "Cantidad por Nivel de Formación") %>% 
  tab_source_note(source_note = fuente) %>% 
  cols_label(nivel_formacion = "Nivel de Formación")


# Respuesta por Tipo de Universidad

educ_tipoF<- educF %>% 
  select(tipo_universidad) %>%  
  group_by(tipo_universidad) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  arrange(-n)

# Compute the cumulative percentages (top of each rectangle)
educ_tipoF$ymax <- cumsum(educ_tipoF$freq)

# Compute the bottom of each rectangle
educ_tipoF$ymin <- c(0, head(educ_tipoF$ymax, n=-1))

# Compute label position
educ_tipoF$labelPosition <- (educ_tipoF$ymax + educ_tipoF$ymin) / 2

# Compute a good label
educ_tipoF$label <- paste0(educ_tipoF$tipo_universidad, "\n Cant: ", educ_tipoF$n)

# Make the plot
ggplot(educ_tipoF, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=tipo_universidad)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +# Try to remove that to see how to make a pie chart
  scale_fill_manual(values = c(gris, verde, azul)) +
  theme_void() +
  theme(legend.position = "right",
        panel.background = element_blank(),
        text = element_text(family = "Roboto")) +
  labs(title = "Tipo de Universidad",
       fill = "Tipo de Universidad", 
       caption = fuente) +
  theme(legend.position = "left")

#principales carreras estudiadas

carrerasF <- freelo2 %>% 
  select(nivel_formacion, carrera_grado, tipo_universidad, 
         genero) %>% 
  mutate(carrera_grado = factor(carrera_grado))


carrerasF <- carrerasF %>% 
  mutate(carrera_grado = fct_collapse(carrera_grado, 
                                      "Abogacía" = c("Abogacía")),
         carrera_grado = fct_collapse(carrera_grado, 
                                      "Administración de Empresas" = c("Lic en Administracion",
                                                                       "Administración y sistemas", "Lic Administracion - Contador Publico - Abogado en curso")),
         carrera_grado = fct_collapse(carrera_grado, 
                                      "Contador Público" = c("Economia", "Economía", "Contador Público/Lic. En Letras", "Economia y RR.HH")),
         carrera_grado = fct_collapse(carrera_grado, 
                                      "Comunicación Social" = c("Caomunicacion social me especialice en RRHH",  "Comunicación","Comunicación social", "Lic. en Ciencias de la Comunicación")), 
         carrera_grado = fct_collapse(carrera_grado, 
                                      "RRHH / RRLL / RRTT" = c("Estudié en la Universidad Analista en Recursos Humanos", "RRHH&Coaching ontologico profesional.", "Graduada en lic rrhh y abogacia")),
         carrera_grado = fct_collapse(carrera_grado, "Ingenierías" = c("Ing en sistemas", "Ing. Financiera", "Ingeniería Comercial", "ING.sistemas",  "Ingenieria Electrónica","Ingeniería Industrial")),
         carrera_grado = fct_collapse(carrera_grado, 
                                      "Psicología" = c("Psicologia","Psicología Industrial", "Psicología social",   "Psicología Social / Lic. en Dirección de las Organizaciones")),
         carrera_grado = fct_collapse(carrera_grado, "No estudié en la Universidad" = "Ninguna"),
         carrera_grado = fct_lump(carrera_grado, 
                                  prop = 0.02, 
                                  other_level = "Otros"),
         carrera_grado = factor(carrera_grado,
                                levels = c("RRHH / RRLL / RRTT", "Psicología", "Administración de Empresas", "Contador Público", "Otros")))


ggplot(carrerasF, aes(x = carrera_grado)) + 
  geom_bar(position = "dodge", fill = azul) +
  theme(axis.text.x = element_text(angle = 90)) + #en 90 se visualiza mejor
  labs(x="",y="") +
  estiloh +
  labs(title = "Principales carreras estudiadas",
       subtitle = "Freelance",
       caption = fuente)

#Educacion y genero

educ_genF <- freelo2 %>% 
  select(nivel_formacion, carrera_grado,
         tipo_universidad, genero,anios_freelance)



#representacion grafica (ver forma mas simple de sacar categorias no representativas)

educ_genF<-educ_genF %>% 
  filter(genero != "Género Diverso") %>%
  filter(genero != "No binario") %>%
  filter(genero != "Prefiero no responder")

ggplot(educ_genF, (aes(x = nivel_formacion, fill = genero))) + #Tipo de universidad y cargo
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="") +
  estilov +
  scale_fill_manual(values = c(gris, verde, azul,rojo)) +
  coord_flip() +
  labs(title = "Cantidad de respuestas según genero y universidad",
       x = "", fill = "Tipo de Universidad",
       caption = fuente)


educ_genF %>%
  mutate(nivel_formacion = factor(nivel_formacion,
                                  levels = c("Secundario completo", "Terciario completo",
                                             "Universitario en curso", "Universitario completo",
                                             "Maestría en curso","Maestría completa"))) %>% 
  group_by(nivel_formacion) %>% 
  ggplot(aes (y= nivel_formacion, fill = genero)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x="",y="") +
  scale_fill_manual(values = col5) +
  estilo +
  theme(legend.position = "top") +
  labs(title = "Máximo nivel educativo alcanzado por género",
       subtitle = "Distribución por frecuencias absolutas",
       caption = fuente, 
       fill = "Género")
