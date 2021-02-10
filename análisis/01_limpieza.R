#------------------------------------------------------------------------------#
# Proyecto:                   TRIBUNAL SUPERIOR DE JUSTICIA DE CIUDAD DE MÉXICO
# Objetivo:                   Procesar datos de la PJCDMX para generar bases 
#                             a nivel persona y a nivel delito 
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          14 de enero   de 2021
# Última actualización:       10 de febrero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, here, lubridate, zoo, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
dir <- here::here()
inp <- "datos_crudos/"
out <- "datos_limpios/"

setwd(dir)

# 1. Cargar datos --------------------------------------------------------------

# Sobre delitos cometidos en la Ciudad de México entre mayo-2011 y sep-2020
# Información de los asuntos ingresados por los diversos delitos
df_asuntos_crudo       <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                          skip = 8, sheet = "ingresados")   %>% slice(1:301833)

# Información de las personas involucradas como víctimas u ofendidas 
df_personas_crudo      <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                          skip = 8, sheet = "victOfend")    %>% slice(1:335316)

# Información de los asuntos con resolución del auto de plazo constitucional
df_sitjurid_crudo      <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                          skip = 8, sheet = "situaJurid")   %>% slice(1:203050) 

# Información de soluciones alternas o terminaciones anticipadas
df_alternas_crudo      <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                          skip = 8, sheet = "solAlternasTermAnticip")   %>% 
                          slice(1:28696)

# Información de la medida cautelar de prisión preventiva
df_cautelares_crudo    <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                          skip = 8, sheet = "medCautelares") %>% slice(1:176809)

# Información de las sentencias emitidas en primera instancia 
df_sentencias_crudo    <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                          skip = 7, sheet = "sentencias")   %>% slice(1:129493)

        
# 2. Limpiar datos -------------------------------------------------------------
# 2.1 Renombrar variables ------------------------------------------------------
df_asuntos <- df_asuntos_crudo                          %>% 
        rename(materia               = "Materia", 
                id_exp               = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_acusada       = "Indice para agrupación 2\r\n\r\n(Personas)", 
                year_ingreso         = "Año ingreso", 
                month_ingreso        = "Mes ingreso", 
                sexo_indiciada       = "Sexo de la persona indiciada", 
                edad_indiciada       = "Edad de la persona indiciada",
                delito               = "Tipo delictivo", 
                desag_estad          = "Desagregado estadístico", 
                consignacion         = "Consignacion", 
                comision             = "Comisión", 
                realizacion          = "Realización", 
                alcaldia             = "Alcaldía de ocurrencia") %>% 
        mutate(consignacion = case_when(consignacion == "con detenido" ~ "Con detenido", 
                                        consignacion == "sin detenido" ~ "Sin detenido", 
                                        consignacion ==   consignacion ~ consignacion)) 

df_personas <- df_personas_crudo        %>% 
        rename(id_exp                = "Indice para agrupación 1\r\n(Expediente / Carpeta)", 
                id_per_ofendida      = "Indice para agrupación 3\r\n(Persona)", 
                sexo_victima         = "Sexo de la persona involucrada como víctima u ofendida", 
                edad_victima         = "Edad de la persona involucrada como víctima u ofendida", 
                relacion             = "Relación entre la persona involucrada como víctima u ofendida y la persona probable responsable de la comisión del delito") %>% 
        mutate(base_ofendida = 1)


df_sitjurid <- df_sitjurid_crudo        %>%
        rename(materia               = "Materia", 
                id_exp               = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_acusada       = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_resolucion      = "Año resolución",  
                month_resolucion     = "Mes resolución", 
                sexo_procesada       = "Sexo de la persona procesada", 
                edad_procesada       = "Edad de la persona procesada",
                delito               = "Tipo delictivo", 
                desag_estad          = "Desagregado estadístico", 
                consignacion         = "Consignacion", 
                comision             = "Comisión", 
                realizacion          = "Realización", 
                alcaldia             = "Alcaldía de ocurrencia", 
                resolucion           = "Resolución a la situación jurídica") 

df_alternas <- df_alternas_crudo          %>% 
        rename(materia               = "Materia", 
                id_exp               = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_acusada       = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_audiencia_alt   = "Año audiencia", 
                month_audiencia_alt  = "Mes audiencia", 
                sexo_indiciada       = "Sexo de la persona indiciada", 
                edad_indiciada       = "Edad de la persona indiciada",
                delito               = "Tipo delictivo", 
                desag_estad          = "Desagregado estadístico", 
                consignacion         = "Consignacion", 
                comision             = "Comisión", 
                realizacion          = "Realización", 
                alcaldia             = "Alcaldía de ocurrencia", 
                solucion             = "Tipo de solución alterna o terminación anticipada")

df_cautelares <- df_cautelares_crudo %>% 
        rename(materia               = "Materia", 
                id_exp               = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_acusada       = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_audiencia_caut  = "Año audiencia",  
                month_audiencia_caut = "Mes audiencia", 
                sexo_vinculada       = "Sexo de la persona vinculada", 
                edad_vinculada       = "Edad de la persona vinculada",
                delito               = "Tipo delictivo", 
                desag_estad          = "Desagregado estadístico", 
                consignacion         = "Consignacion", 
                comision             = "Comisión", 
                realizacion          = "Realización", 
                alcaldia             = "Alcaldía de ocurrencia", 
                medida               = "Tipo de medida cautelar") 


df_sentencias <- df_sentencias_crudo %>% 
        rename(materia               = "Materia", 
                id_exp               = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_acusada       = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_sentencia       = "Año sentencia",  
                month_sentencia      = "Mes sentencia", 
                sexo_sentenciada     = "Sexo", 
                edad_sentenciada     = "Edad",
                delito               = "Tipo delictivo", 
                desag_estad          = "Desagregado estadístico", 
                consignacion         = "Consignación", 
                comision             = "Comisión", 
                realizacion          = "Realización", 
                alcaldia             = "Alcaldía de ocurrencia del delito", 
                forma_proceso        = "Forma del proceso", 
                sentencia            = "Tipo de sentencia", 
                years_sentencia      = "Años de sentencia", 
                months_sentencia     = "Meses de sentencia", 
                días_sentencia       = "Días de sentencia") 

# 2.2 Identificar número de observaciones, expedientes y personas únicos ------- 
# expediente > agresor o víctima > delitos > medida cautelar o solución alterna

# Asuntos - Mínimo grado de desagregación:  agresor (< expediente)
dim(df_asuntos)[1]                              # 301,833 observaciones (asuntos por expediente y agresor)
length(unique(df_asuntos$id_exp))               # 219,446 expedientes únicos
length(unique(df_asuntos$id_per_acusada))       # 255,792 agresores únicos

# Personas ofendidas - Mínimo grado de desagregación:  víctima (< expediente)
dim(df_personas)[1]                             # 335,316 observaciones (expediente por víctima)
length(unique(df_personas$id_exp))              # 231,183 expedientes únicos
length(unique(df_personas$id_per_ofendida))     # 252,737 víctimas únicas

# Situación jurídica - Mínimo grado de desagregación:  agresor(< expediente)
dim(df_sitjurid)[1]                             # 203,050 observaciones (asuntos con resolución de auto plazo por expediente y agresor)
length(unique(df_sitjurid$id_exp))              # 152,685 expedientes únicos
length(unique(df_sitjurid$id_per_acusada))      # 177,682 agresores únicos

# Soluciones alternas - Mínimo grado de desagregación: solución alterna (< delito)
dim(df_alternas)[1]                             #  28,696 observaciones (soluciones alternas por expediente y agresores)
length(unique(df_alternas$id_exp))              #  21,986 expedientes únicos
length(unique(df_alternas$id_per_acusada))      #  25,734 agresores únicos

# Medidas cautelares - Mínimo grado de desagregación: medida cautelar (< delito)
dim(df_cautelares)[1]                           # 176,809 observaciones (medidas cautelares)
length(unique(df_cautelares$id_exp))            #  48,285 expedientes únicos
length(unique(df_cautelares$id_per_acusada))    #  63,259 agresores únicos

# Sentencias - Mínimo grado de desagregación: delito (< agresor)
dim(df_sentencias)[1]                           # 129,493 observaciones (sentencias por delito)
length(unique(df_sentencias$id_exp))            #  97,176 expedientes únicos 
length(unique(df_sentencias$id_per_acusada))    # 116,377 agresores únicos


# 2.3 Identificar folios repetidos ---------------------------------------------
# Asuntos - Revisar folios repetidos 
df_freq  <- table(df_asuntos$id_per_acusada)
df_freq2 <- as.data.frame(table(df_freq))
print(df_freq2) # Frecuencia de repeticiones 
        #  Observaciones: el delito de robo suele tener muchas repeticiones.

# Personas ofendidas - Revisar folios repetidos
df_freq  <- table(df_personas$id_per_ofendida)
df_freq2 <- as.data.frame(table(df_freq)) 
print(df_freq2) # Frecuencia de repeticiones 
        # Obs: la observación con más repeticiones (340) es "No especificado"
        # Después sigue el folio 4408714 con 20 repeticiones aunque no hay 
        # ningún dato demográfico registrado. Lo que cambia es el número de 
        # expediente, pero no parece ser la misma víctima porque al buscar 
        # en df_asuntos, los delitos de cada expediente son muy distintos. 

# Situación jurídica - Revisar folios repetidos
df_freq  <- table(df_sitjurid$id_per_acusada)
df_freq2 <- as.data.frame(table(df_freq))
print(df_freq2) # Frecuencia de repeticiones 
        #  Observaciones: el delito de robo suele tener muchas repeticiones.

# Soluciones alternas - Revisar folios repetidos
df_freq  <- table(df_alternas$id_per_acusada)
df_freq2 <- as.data.frame(table(df_freq))
print(df_freq2) # Frecuencia de repeticiones 
        # Observaciones: aquí hay menos repeticiones, el que más se repite es 
        # una persona que recibió 3 medidas cautelares para 2 delitos, por lo 
        # que aparece 6 veces en total. 

# Medidas cautelares - Revisar folios repetidos
df_freq  <- table(df_cautelares$id_per_acusada)
df_freq2 <- as.data.frame(table(df_freq)) 
print(df_freq2) # Frecuencia de repeticiones 


# Sentencias - Revisar folios repetidos
df_freq  <- table(df_sentencias$id_per_acusada)
df_freq2 <- as.data.frame(table(df_freq)) 
print(df_freq2) # Frecuencia de repeticiones 

# Aunque tengan los mismos datos en cada variable, no hay observaciones repetidas.
# La documentación del TSJ-CDMX indica que cada renglón u observación es un 
# asunto, delito, medida cautelar o sentencia que provoca que se repitan tanto 
# los folios de expediente como los folios de personas. 

# 2.4 Crear bases agregadas en formato ancho (wide) ----------------------------

# 2.4.1 Asuntos ingresados -----------------------------------------------------

df_asuntos_renombrado1 <- df_asuntos %>% 
        # Unificar categorías de homicidio y feminicidio en 1 
        mutate(homicidio_1         = as.numeric(str_detect(delito, "Homicidio")), 
               homicidio_2         = as.numeric(str_detect(delito, "Feminicidio")), 
                d_homicidio_asunto = homicidio_1 + homicidio_2)                 %>% 
        # Unificar categorías para secuestros 
        mutate(secuestro_1         = as.numeric(str_detect(delito, "Secuestro")), 
               secuestro_2         = as.numeric(str_detect(delito, "Privación de la libertad")), 
               d_secuestro_asunto  = secuestro_1 + secuestro_2)                 %>% 
        # Unificar categorías para delitos sexuales
        mutate(sexuales_1          = as.numeric(str_detect(delito, "Abuso sexual")), 
               sexuales_2          = as.numeric(str_detect(delito, "Violacion")), 
               sexuales_3          = as.numeric(str_detect(delito, "Hostigamiento")), 
               d_sexuales_asunto   = sexuales_1 + sexuales_2 + sexuales_3)      %>% 
        # Crear variables dummies para otros delitos
        mutate(d_salud_asunto      = as.numeric(str_detect(delito, "salud")),
               d_robo_asunto       = as.numeric(str_detect(delito, "Robo")), 
               d_familiar_asunto   = as.numeric(str_detect(delito, "Violencia familiar")), 
               d_lesiones_asunto   = as.numeric(str_detect(delito, "Lesiones")),
               d_extorsion_asunto  = as.numeric(str_detect(delito, "Extorsion")), 
               d_objetos_asunto    = as.numeric(str_detect(delito, "objetos")), 
               d_encubrimiento_asunto = as.numeric(str_detect(delito, "Encubrimiento"))) %>% 
        # Crear categoría para el resto de los delitos 
        mutate(d_otros_asunto = ifelse(d_homicidio_asunto      != 1 & 
                                        d_salud_asunto         != 1 & 
                                        d_robo_asunto          != 1 & 
                                        d_familiar_asunto      != 1 & 
                                        d_lesiones_asunto      != 1 & 
                                        d_encubrimiento_asunto != 1 &
                                        d_extorsion_asunto     != 1 & 
                                        d_objetos_asunto       != 1 & 
                                        d_secuestro_asunto     != 1 & 
                                        d_sexuales_asunto      != 1, 1, 0)) %>% 
        # Retirar variables innecesarias 
        dplyr::select(-c(homicidio_1, homicidio_2, secuestro_1, secuestro_2,
                         sexuales_1,  sexuales_2,  sexuales_3)) 

# Crear nueva variable con nombres cortos de los delitos 
# Base final por delitos 
df_asuntos_delitos <- df_asuntos_renombrado1 %>% 
        mutate(delitos_cortos = case_when(d_homicidio_asunto == 1 ~ "Homicidio",
               d_salud_asunto     == 1 ~ "Delitos contra la salud",
               d_robo_asunto      == 1 ~ "Robo",
               d_familiar_asunto  == 1 ~ "Violencia familiar",
               d_lesiones_asunto  == 1 ~ "Lesiones",
               d_encubrimiento_asunto == 1 ~ "Encubrimiento",
               d_extorsion_asunto == 1 ~ "Extorsión",
               d_objetos_asunto   == 1 ~ "objetos aptos para agredir",
               d_secuestro_asunto == 1 ~ "Secuestro",
               d_sexuales_asunto  == 1 ~ "Delitos sexuales",
               d_otros_asunto     == 1 ~ "Otros delitos")) %>% 
        # Renombrar variable de sexo 
        mutate(sexo_indiciada = case_when(sexo_indiciada == "Femenino" ~ "Mujeres", 
               sexo_indiciada == "Masculino"       ~ "Hombres", 
               sexo_indiciada == "No especificado" ~ "No especificado" )) %>% 
        # Crear variable de fecha con objeto tipo date 
        mutate(date_ingreso = paste(year_ingreso, month_ingreso, sep = "-"), 
               date_ingreso = as.yearmon(date_ingreso))


# Base final agregada por acusadosa
df_asuntos_vars_wide <- df_asuntos_delitos %>% 
        # Crear variables para consignación 
        mutate(c_con_detenido = as.numeric(str_detect(consignacion, "Con detenido")), 
               c_sin_detenido = as.numeric(str_detect(consignacion, "Sin detenido"))) %>% 
                # Crear variables para comisión
        mutate(c_culposo      = as.numeric(str_detect(comision, "Culposo")), 
               c_doloso       = as.numeric(str_detect(comision, "Doloso"))) %>% 
        # Crear variables para realización 
        mutate(tr_consumado   = as.numeric(str_detect(realizacion, "Consumado")), 
               tr_tentativa   = as.numeric(str_detect(realizacion, "Tentativa")))        

# Dadas las irregularidades, se pierde la información de consignación, comisión y 
# realización, por ello, es necesario ponderar si también se tiene que hacer una 
# variable binaria sobre el tema. Por esto, en la limpieza siguiente no se puede 
# agrupar por estas variables, dado que generan más observaciones de las que 
# debería haber. 

df_asuntos_wide_summed <- df_asuntos_vars_wide %>% 
        group_by(id_exp, id_per_acusada, year_ingreso, month_ingreso, 
                date_ingreso, edad_indiciada, sexo_indiciada, materia) %>%
        # Contabilizar alcaldías donde se cometieron delitos 
        summarise(num_alcaldias             = length(unique(alcaldia)), 
        # Variables de conteo de consignación, comisión y realización 
                num_consignacion            = length(unique(consignacion)), 
                num_comision                = length(unique(comision)),
                num_realizacion             = length(unique(realizacion)),
        # Variables de conteo para consignación, comisión y realización 
                c_con_detenido              = sum(c_con_detenido), 
                c_sin_detenido              = sum(c_sin_detenido),
                c_culposo                   = sum(c_culposo), 
                c_doloso                    = sum(c_doloso), 
                tr_consumado                = sum(tr_consumado), 
                tr_tentativa                = sum(tr_tentativa),
        # Variables de conteo para delitos
                d_homicidio_asunto          = sum(d_homicidio_asunto), 
                d_secuestro_asunto          = sum(d_secuestro_asunto), 
                d_sexuales_asunto           = sum(d_sexuales_asunto), 
                d_salud_asunto              = sum(d_salud_asunto), 
                d_robo_asunto               = sum(d_robo_asunto), 
                d_familiar_asunto           = sum(d_familiar_asunto), 
                d_lesiones_asunto           = sum(d_lesiones_asunto), 
                d_extorsion_asunto          = sum(d_extorsion_asunto), 
                d_objetos_asunto            = sum(d_objetos_asunto), 
                d_encubrimiento_asunto      = sum(d_encubrimiento_asunto), 
                d_otros_asunto              = sum(d_otros_asunto)) %>% 
        ungroup() %>% 
        # Crar contador para el total de delitos 
        mutate(num_delitos = d_homicidio_asunto + d_secuestro_asunto + 
                        d_sexuales_asunto       + d_salud_asunto + d_robo_asunto +
                        d_familiar_asunto       + d_lesiones_asunto + 
                        d_extorsion_asunto      + d_objetos_asunto + 
                        d_encubrimiento_asunto  + d_otros_asunto) %>% 
        mutate(base_asuntos = 1) 

# Variables de conteo a indicadores binarios 
df_asuntos_acusados <- df_asuntos_wide_summed %>% 
        mutate(c_con_detenido          = if_else(c_con_detenido     == 0, 0, 1),
                c_sin_detenido         = if_else(c_sin_detenido     == 0, 0, 1),
                c_culposo              = if_else(c_culposo          == 0, 0, 1),
                c_doloso               = if_else(c_doloso           == 0, 0, 1),
                tr_consumado           = if_else(tr_consumado       == 0, 0, 1),
                tr_tentativa           = if_else(tr_tentativa       == 0, 0, 1),
        # Indicadores binarios para delitos 
                d_homicidio_asunto     = if_else(d_homicidio_asunto == 0, 0, 1), 
                d_secuestro_asunto     = if_else(d_secuestro_asunto == 0, 0, 1), 
                d_sexuales_asunto      = if_else(d_sexuales_asunto  == 0, 0, 1), 
                d_salud_asunto         = if_else(d_salud_asunto     == 0, 0, 1), 
                d_robo_asunto          = if_else(d_robo_asunto      == 0, 0, 1), 
                d_familiar_asunto      = if_else(d_familiar_asunto  == 0, 0, 1), 
                d_lesiones_asunto      = if_else(d_lesiones_asunto  == 0, 0, 1), 
                d_extorsion_asunto     = if_else(d_extorsion_asunto == 0, 0, 1), 
                d_objetos_asunto       = if_else(d_objetos_asunto   == 0, 0, 1), 
                d_encubrimiento_asunto = if_else(d_encubrimiento_asunto == 0, 0, 1), 
                d_otros_asunto         = if_else(d_otros_asunto     == 0, 0, 1)) 

# Cuando sólo agrupamos por id_exp y id_per_acusada: 265102
# Cuando agrupamos además con sexo, año y mes: 265134
        # Hay 32 observaciones adicionales, que se debe estar duplicando por una 
        # irregularidad en sexo, año o mes 


# 2.4.2 Personas agredidas -----------------------------------------------------
df_personas_vars_wide <- df_personas %>% 
        # Renombrar variable de sexo 
        mutate(sexo_victima = case_when(sexo_victima == "Femenino" ~ "Mujer", 
               sexo_victima == "Masculino" ~ "Hombre", 
               sexo_victima == "No especificado" ~ "No especificado")) %>% 
        # Variables dummies para víctimas por sexo 
        mutate(v_mujeres = as.numeric(str_detect(sexo_victima, "Mujer")), 
               v_hombres = as.numeric(str_detect(sexo_victima, "Hombre")), 
               v_no_esp  = as.numeric(str_detect(sexo_victima, "No especificado")))  %>% 
        # Variables dummies para tipo de relación 
        mutate(r_academica   = as.numeric(str_detect(relacion, "Académica")), 
               r_autoridad   = as.numeric(str_detect(relacion, "Autoridad")),
               r_concubinato = as.numeric(str_detect(relacion, "Concubinato")),
               r_empleo      = as.numeric(str_detect(relacion, "Empleo o profesión")), 
               r_ninguna     = as.numeric(str_detect(relacion, "Ninguna")),
               r_no_especif  = as.numeric(str_detect(relacion, "No especificado")),
               r_no_identif  = as.numeric(str_detect(relacion, "No identificada")),
               r_otro_tipo   = as.numeric(str_detect(relacion, "Otro tipo de relación")),
               r_parent_afin = as.numeric(str_detect(relacion, "Parentesco por afinidad")),
               r_parent_sang = as.numeric(str_detect(relacion, "Parentesco por consanguinidad")),
               r_tutor       = as.numeric(str_detect(relacion, "Tutor o curador")))

df_personas_expediente_summed <- df_personas_vars_wide %>% 
        group_by(id_exp) %>% 
        summarise(v_total     = n(), 
                # Número de víctimas desagregadas por sexo
                v_mujeres     = sum(v_mujeres), 
                v_hombres     = sum(v_hombres), 
                v_no_esp      = sum(v_no_esp), 
                # Tipo de relación 
                r_academica   = sum(r_academica), 
                r_autoridad   = sum(r_autoridad), 
                r_concubinato = sum(r_concubinato), 
                r_empleo      = sum(r_empleo), 
                r_ninguna     = sum(r_ninguna), 
                r_no_especif  = sum(r_no_especif), 
                r_no_identif  = sum(r_no_identif), 
                r_otro_tipo   = sum(r_otro_tipo), 
                r_parent_afin = sum(r_parent_afin), 
                r_parent_sang = sum(r_parent_sang), 
                r_tutor       = sum(r_tutor)) 
                
# Indicadores binarios para los tipos de relación 
df_personas_expediente <- df_personas_expediente_summed %>% 
        mutate(r_academica    = if_else(r_academica   == 0, 0, 1), 
                r_autoridad   = if_else(r_autoridad   == 0, 0, 1), 
                r_concubinato = if_else(r_concubinato == 0, 0, 1), 
                r_empleo      = if_else(r_empleo      == 0, 0, 1), 
                r_ninguna     = if_else(r_ninguna     == 0, 0, 1), 
                r_no_especif  = if_else(r_no_especif  == 0, 0, 1), 
                r_no_identif  = if_else(r_no_identif  == 0, 0, 1), 
                r_otro_tipo   = if_else(r_otro_tipo   == 0, 0, 1), 
                r_parent_afin = if_else(r_parent_afin == 0, 0, 1), 
                r_parent_sang = if_else(r_parent_sang == 0, 0, 1), 
                r_tutor       = if_else(r_tutor       == 0, 0, 1))




# El problema de que esté desagregada por víctima y luego por expediente es que 
# si hago la unión con las personas agresoras se va a repetir cada vez en todos
# los casos en los que haya más de una persona agresora. 


# 2.4.3 Situación jurídica -----------------------------------------------------
df_sitjurid_renombrado1 <- df_sitjurid %>% 
        # Unificar categorías de homicidio y feminicidio en 1 
        mutate(homicidio_1 = as.numeric(str_detect(delito, "Homicidio")), 
               homicidio_2 = as.numeric(str_detect(delito, "Feminicidio")), 
               d_homicidio_sitjurid   = homicidio_1 + homicidio_2)                 %>% 
        # Unificar categorías para secuestros 
        mutate(secuestro_1 = as.numeric(str_detect(delito, "Secuestro")), 
               secuestro_2 = as.numeric(str_detect(delito, "Privación de la libertad")), 
               d_secuestro_sitjurid   = secuestro_1 + secuestro_2)                 %>% 
        # Unificar categorías para delitos sexuales
        mutate(sexuales_1  = as.numeric(str_detect(delito, "Abuso sexual")), 
               sexuales_2  = as.numeric(str_detect(delito, "Violacion")), 
               sexuales_3  = as.numeric(str_detect(delito, "Hostigamiento")), 
               d_sexuales_sitjurid    = sexuales_1 + sexuales_2 + sexuales_3)      %>% 
        # Crear variables dummies para otros delitos
        mutate(d_salud_sitjurid       = as.numeric(str_detect(delito, "salud")),
               d_robo_sitjurid        = as.numeric(str_detect(delito, "Robo")), 
               d_familiar_sitjurid    = as.numeric(str_detect(delito, "Violencia familiar")), 
               d_lesiones_sitjurid    = as.numeric(str_detect(delito, "Lesiones")),
               d_extorsion_sitjurid   = as.numeric(str_detect(delito, "Extorsion")), 
               d_objetos_sitjurid     = as.numeric(str_detect(delito, "objetos")), 
               d_encubrimiento_sitjurid = as.numeric(str_detect(delito, "Encubrimiento"))) %>% 
        # Crear categoría para el resto de los delitos 
        mutate(d_otros_sitjurid = ifelse(d_homicidio_sitjurid != 1 & d_salud_sitjurid != 1    & 
                        d_robo_sitjurid      != 1 & d_familiar_sitjurid      != 1 & 
                        d_lesiones_sitjurid  != 1 & d_encubrimiento_sitjurid != 1 &
                        d_extorsion_sitjurid != 1 & d_objetos_sitjurid       != 1 & 
                        d_secuestro_sitjurid != 1 & d_sexuales_sitjurid      != 1, 1, 0)) %>% 
        # Retirar variables innecesarias 
        dplyr::select(-c(homicidio_1, homicidio_2, secuestro_1, secuestro_2,
                sexuales_1, sexuales_2, sexuales_3)) 

# Renombrar resolución (consultar cuáles son los mejores nombres para renombrar)
df_sitjurid_renombrado2 <- df_sitjurid_renombrado1 %>% 
        mutate(resolucion = case_when(resolucion == resolucion ~ resolucion))

# Crear nueva variable con nombres cortos de los delitos                        
df_sitjurid_delitos <- df_sitjurid_renombrado2 %>% 
        mutate(delitos_cortos = case_when(d_homicidio_sitjurid == 1 ~ "Homicidio",
                d_salud_sitjurid     == 1 ~ "Delitos contra la salud",
                d_robo_sitjurid      == 1 ~ "Robo",
                d_familiar_sitjurid  == 1 ~ "Violencia familiar",
                d_lesiones_sitjurid  == 1 ~ "Lesiones",
                d_encubrimiento_sitjurid == 1 ~ "Encubrimiento",
                d_extorsion_sitjurid == 1 ~ "Extorsión",
                d_objetos_sitjurid   == 1 ~ "objetos aptos para agredir",
                d_secuestro_sitjurid == 1 ~ "Secuestro",
                d_sexuales_sitjurid  == 1 ~ "Delitos sexuales",
                d_otros_sitjurid     == 1 ~ "Otros delitos")) %>% 
        # Renombrar variable de sexo 
        mutate(sexo_procesada = case_when(sexo_procesada == "Femenino" ~ "Mujeres", 
                sexo_procesada == "Masculino" ~ "Hombres", 
                sexo_procesada == "No especificado" ~ "No especificado" )) %>% 
        # Crear variable de fecha con objeto tipo date 
        mutate(date_resolucion = paste(year_resolucion, month_resolucion, sep = "-"), 
                date_resolucion = as.yearmon(date_resolucion))


# Crear variables binarias para situación jurídica ("s_" como sufijo de situación jurídica)
df_sitjurid_vars_wide <- df_sitjurid_delitos %>% 
        mutate(s_formal_prision            = as.numeric(str_detect(resolucion, "Formal prisión")), 
                s_libertad_falta_elementos = as.numeric(str_detect(resolucion, "Libertad por falta de elementos para procesar")), 
                s_no_especificado          = as.numeric(str_detect(resolucion, "No especificado")), 
                s_no_vinculado             = as.numeric(str_detect(resolucion, "No Vinculacion a Proceso")), 
                s_proceso_especial         = as.numeric(str_detect(resolucion, "Proceso especial para inimputable")), 
                s_sujecion_en_libertad     = as.numeric(str_detect(resolucion, "Sujeción a proceso sin restricción de la libertad")), 
                s_vinculado_proceso        = as.numeric(str_detect(resolucion, "Vinculacion a Proceso"))) %>% 
        # Corregir por los casos en los que se detectó "Vinculacion a Proceso" en "No Vinculacion a Proceso"
        mutate(s_vinculado_proceso = case_when(s_vinculado_proceso == 1 & resolucion == "No Vinculacion a Proceso" ~ 0, 
               s_vinculado_proceso == s_vinculado_proceso ~ s_vinculado_proceso)) %>% 
        # Crear variables para consignación 
        mutate(c_con_detenido = as.numeric(str_detect(consignacion, "Con detenido")), 
                c_sin_detenido = as.numeric(str_detect(consignacion, "Sin detenido"))) %>% 
        # Crear variables para comisión
        mutate(c_culposo = as.numeric(str_detect(comision, "Culposo")), 
                c_doloso = as.numeric(str_detect(comision, "Doloso"))) %>% 
        # Crear variables para realización 
        mutate(tr_consumado = as.numeric(str_detect(realizacion, "Consumado")), 
                tr_tentativa =  as.numeric(str_detect(realizacion, "Tentativa")))


# Base final desagregada por acusados 
df_sitjurid_wide_summed <- df_sitjurid_vars_wide %>% 
        group_by(id_exp, id_per_acusada, year_resolucion, month_resolucion, 
                date_resolucion, edad_procesada, sexo_procesada, materia) %>% 
        # Contabilizar alcaldías donde se cometieron delitos 
        summarise(num_alcaldias              = length(unique(alcaldia)), 
                  num_consignacion           = length(unique(consignacion)), 
                  num_comision               = length(unique(comision)),
                  num_realizacion            = length(unique(realizacion)),
       # Variables de conteo para consignación, comisión y realización 
                  c_con_detenido             = sum(c_con_detenido), 
                  c_sin_detenido             = sum(c_sin_detenido),
                  c_culposo                  = sum(c_culposo), 
                  c_doloso                   = sum(c_doloso), 
                  tr_consumado               = sum(tr_consumado), 
                  tr_tentativa               = sum(tr_tentativa),
        # Delitos 
                  d_homicidio_sitjurid       = sum(d_homicidio_sitjurid), 
                  d_secuestro_sitjurid       = sum(d_secuestro_sitjurid), 
                  d_sexuales_sitjurid        = sum(d_sexuales_sitjurid), 
                  d_salud_sitjurid           = sum(d_salud_sitjurid), 
                  d_robo_sitjurid            = sum(d_robo_sitjurid), 
                  d_familiar_sitjurid        = sum(d_familiar_sitjurid), 
                  d_lesiones_sitjurid        = sum(d_lesiones_sitjurid), 
                  d_extorsion_sitjurid       = sum(d_extorsion_sitjurid), 
                  d_objetos_sitjurid         = sum(d_objetos_sitjurid), 
                  d_encubrimiento_sitjurid   = sum(d_encubrimiento_sitjurid), 
                  d_otros_sitjurid           = sum(d_otros_sitjurid), 
        # Situación jurídica 
                  s_formal_prision           = sum(s_formal_prision), 
                  s_libertad_falta_elementos = sum(s_libertad_falta_elementos), 
                  s_no_especificado          = sum(s_no_especificado), 
                  s_no_vinculado             = sum(s_no_vinculado), 
                  s_proceso_especial         = sum(s_proceso_especial), 
                  s_sujecion_en_libertad     = sum(s_sujecion_en_libertad), 
                  s_vinculado_proceso        = sum(s_vinculado_proceso))  %>% 
        ungroup() %>%     
        mutate(num_delitos = d_homicidio_sitjurid + d_secuestro_sitjurid + 
                             d_sexuales_sitjurid  + d_salud_sitjurid + 
                             d_robo_sitjurid      + d_familiar_sitjurid  + 
                             d_lesiones_sitjurid  + d_extorsion_sitjurid + 
                             d_objetos_sitjurid   + d_encubrimiento_sitjurid + 
                             d_otros_sitjurid) %>% 
        mutate(base_sitjurid = 1)

# Variables de conteo a indicadores binarios 
df_sitjurid_acusados <- df_sitjurid_wide_summed %>% 
        mutate(c_con_detenido            = if_else(c_con_detenido         == 0, 0, 1),
                c_sin_detenido           = if_else(c_sin_detenido         == 0, 0, 1),
                c_culposo                = if_else(c_culposo              == 0, 0, 1),
                c_doloso                 = if_else(c_doloso               == 0, 0, 1),
                tr_consumado             = if_else(tr_consumado           == 0, 0, 1),
                tr_tentativa             = if_else(tr_tentativa           == 0, 0, 1),
                # Indicadores binarios para delitos 
                d_homicidio_sitjurid     = if_else(d_homicidio_sitjurid   == 0, 0, 1), 
                d_secuestro_sitjurid     = if_else(d_secuestro_sitjurid   == 0, 0, 1), 
                d_sexuales_sitjurid      = if_else(d_sexuales_sitjurid    == 0, 0, 1), 
                d_salud_sitjurid         = if_else(d_salud_sitjurid       == 0, 0, 1), 
                d_robo_sitjurid          = if_else(d_robo_sitjurid        == 0, 0, 1), 
                d_familiar_sitjurid      = if_else(d_familiar_sitjurid    == 0, 0, 1), 
                d_lesiones_sitjurid      = if_else(d_lesiones_sitjurid    == 0, 0, 1), 
                d_extorsion_sitjurid     = if_else(d_extorsion_sitjurid   == 0, 0, 1), 
                d_objetos_sitjurid       = if_else(d_objetos_sitjurid     == 0, 0, 1), 
                d_encubrimiento_sitjurid = if_else(d_encubrimiento_sitjurid == 0, 0, 1), 
                d_otros_sitjurid         = if_else(d_otros_sitjurid       == 0, 0, 1), 
                # Indicadores binarios para delitos 
                s_formal_prision         = if_else(s_formal_prision       == 0, 0, 1), 
                s_libertad_falta_elementos = if_else(s_libertad_falta_elementos == 0, 0, 1), 
                s_no_especificado        = if_else(s_no_especificado      == 0, 0, 1), 
                s_no_vinculado           = if_else(s_no_vinculado         == 0, 0, 1), 
                s_proceso_especial       = if_else(s_proceso_especial     == 0, 0, 1), 
                s_sujecion_en_libertad   = if_else(s_sujecion_en_libertad == 0, 0, 1), 
                s_vinculado_proceso      = if_else(s_vinculado_proceso    == 0, 0, 1))
        


# 2.4.4 Soluciones alternas -------------------------------------------------------
df_alternas_renombrado1 <- df_alternas %>% 
        # Unificar categorías de homicidio y feminicidio en 1 
        mutate(homicidio_1 = as.numeric(str_detect(delito, "Homicidio")), 
               homicidio_2 = as.numeric(str_detect(delito, "Feminicidio")), 
               d_homicidio_alternas   = homicidio_1 + homicidio_2)                 %>% 
        # Unificar categorías para secuestros 
        mutate(secuestro_1 = as.numeric(str_detect(delito, "Secuestro")), 
               secuestro_2 = as.numeric(str_detect(delito, "Privación de la libertad")), 
                d_secuestro_alternas   = secuestro_1 + secuestro_2)                 %>% 
        # Unificar categorías para delitos sexuales
        mutate(sexuales_1  = as.numeric(str_detect(delito, "Abuso sexual")), 
               sexuales_2  = as.numeric(str_detect(delito, "Violacion")), 
               sexuales_3  = as.numeric(str_detect(delito, "Hostigamiento")), 
                d_sexuales_alternas    = sexuales_1 + sexuales_2 + sexuales_3)      %>% 
        # Crear variables dummies para otros delitos
        mutate(d_salud_alternas       = as.numeric(str_detect(delito, "salud")),
               d_robo_alternas        = as.numeric(str_detect(delito, "Robo")), 
               d_familiar_alternas    = as.numeric(str_detect(delito, "Violencia familiar")), 
               d_lesiones_alternas    = as.numeric(str_detect(delito, "Lesiones")),
               d_extorsion_alternas   = as.numeric(str_detect(delito, "Extorsion")), 
               d_objetos_alternas     = as.numeric(str_detect(delito, "objetos")), 
                d_encubrimiento_alternas = as.numeric(str_detect(delito, "Encubrimiento"))) %>% 
        # Crear categoría para el resto de los delitos 
        mutate(d_otros_alternas = ifelse(d_homicidio_alternas != 1 & d_salud_alternas != 1    & 
                              d_robo_alternas      != 1 & d_familiar_alternas  != 1 & 
                              d_lesiones_alternas  != 1 & d_encubrimiento_alternas != 1 &
                              d_extorsion_alternas != 1 & d_objetos_alternas   != 1 & 
                              d_secuestro_alternas != 1 & d_sexuales_alternas  != 1, 1, 0)) %>% 
        # Retirar variables innecesarias 
        dplyr::select(-c(homicidio_1, homicidio_2, secuestro_1, secuestro_2,
                sexuales_1, sexuales_2, sexuales_3)) 

# Renombrar tipo de soluciones alternas (consultar cuáles son los nombres más adecuados)
df_alternas_renombrado2 <- df_alternas_renombrado1 %>% 
        mutate(solucion = case_when(solucion == solucion ~ solucion))


# Crear nueva variable con nombres cortos de los delitos                        
df_alternas_delitos <- df_alternas_renombrado2 %>% 
        mutate(delitos_cortos = case_when(d_homicidio_alternas == 1 ~ "Homicidio",
               d_salud_alternas          == 1 ~ "Delitos contra la salud",
               d_robo_alternas           == 1 ~ "Robo",
               d_familiar_alternas       == 1 ~ "Violencia familiar",
               d_lesiones_alternas       == 1 ~ "Lesiones",
               d_encubrimiento_alternas  == 1 ~ "Encubrimiento",
               d_extorsion_alternas      == 1 ~ "Extorsión",
               d_objetos_alternas        == 1 ~ "objetos aptos para agredir",
               d_secuestro_alternas      == 1 ~ "Secuestro",
               d_sexuales_alternas       == 1 ~ "Delitos sexuales",
               d_otros_alternas          == 1 ~ "Otros delitos")) %>% 
        # Renombrar variable de sexo 
        mutate(sexo_indiciada = case_when(sexo_indiciada == "Femenino" ~ "Mujeres", 
               sexo_indiciada == "Masculino" ~ "Hombres", 
               sexo_indiciada == "No especificado" ~ "No especificado" )) %>% 
        # Crear variable de fecha con objeto tipo date 
        mutate(date_audiencia_alt = paste(year_audiencia_alt, month_audiencia_alt, sep = "-"), 
                date_audiencia_alt = as.yearmon(date_audiencia_alt))



# Crear variables binarias para soluciones alternas ("a_" como sufijo de solución alterna)
df_alternas_vars_wide <- df_alternas_delitos %>% 
        mutate(a_reparatorio     = as.numeric(str_detect(solucion, "Acuerdo Reparatorio")), 
               a_perdon          = as.numeric(str_detect(solucion, "Perdon")), 
               a_suspension      = as.numeric(str_detect(solucion, "Suspensión Condicional del Proceso")), 
               a_criterio        = as.numeric(str_detect(solucion, "Criterio de Oportunidad")), 
               a_sobreseimiento  = as.numeric(str_detect(solucion, "Sobreseimiento"))) %>% 
        # Crear variables para consignación 
        mutate(c_con_detenido = as.numeric(str_detect(consignacion, "Con detenido")), 
                c_sin_detenido = as.numeric(str_detect(consignacion, "Sin detenido"))) %>% 
        # Crear variables para comisión
        mutate(c_culposo = as.numeric(str_detect(comision, "Culposo")), 
                c_doloso = as.numeric(str_detect(comision, "Doloso"))) %>% 
        # Crear variables para realización 
        mutate(tr_consumado = as.numeric(str_detect(realizacion, "Consumado")), 
                tr_tentativa =  as.numeric(str_detect(realizacion, "Tentativa")))

# Base final desagregada por personas 
df_alternas_wide_summed <- df_alternas_vars_wide %>% 
        group_by(id_exp, id_per_acusada, year_audiencia_alt, month_audiencia_alt, 
                date_audiencia_alt, sexo_indiciada, edad_indiciada, materia) %>% 
        # Contabilizar alcaldías donde se cometieron delitos 
        summarise(num_alcaldias             = length(unique(alcaldia)), 
                num_consignacion            = length(unique(consignacion)), 
                num_comision                = length(unique(comision)),
                num_realizacion             = length(unique(realizacion)),
        # Variables de conteo para consignación, comisión y realización 
                c_con_detenido              = sum(c_con_detenido), 
                c_sin_detenido              = sum(c_sin_detenido),
                c_culposo                   = sum(c_culposo), 
                c_doloso                    = sum(c_doloso), 
                tr_consumado                = sum(tr_consumado), 
                tr_tentativa                = sum(tr_tentativa),
        # Delitos 
                d_homicidio_alternas        = sum(d_homicidio_alternas), 
                d_secuestro_alternas        = sum(d_secuestro_alternas), 
                d_sexuales_alternas         = sum(d_sexuales_alternas), 
                d_salud_alternas            = sum(d_salud_alternas), 
                d_robo_alternas             = sum(d_robo_alternas), 
                d_familiar_alternas         = sum(d_familiar_alternas), 
                d_lesiones_alternas         = sum(d_lesiones_alternas), 
                d_extorsion_alternas        = sum(d_extorsion_alternas), 
                d_objetos_alternas          = sum(d_objetos_alternas), 
                d_encubrimiento_alternas    = sum(d_encubrimiento_alternas), 
                d_otros_alternas            = sum(d_otros_alternas),
        # Soluciones alternas 
                a_reparatorio               = sum(a_reparatorio), 
                a_perdon                    = sum(a_perdon), 
                a_suspension                = sum(a_suspension), 
                a_criterio                  = sum(a_criterio), 
                a_sobreseimiento            = sum(a_sobreseimiento)) %>% 
        ungroup() %>% 
        mutate(num_delitos = d_homicidio_alternas + d_secuestro_alternas + 
                             d_sexuales_alternas  + d_salud_alternas + 
                             d_robo_alternas      + d_familiar_alternas + 
                             d_lesiones_alternas  + d_extorsion_alternas + 
                             d_objetos_alternas   + d_encubrimiento_alternas + 
                             d_otros_alternas) %>% 
        mutate(base_sol_alternas = 1)

# Variables de conteo a indicadores binarios 
df_alternas_acusados <- df_alternas_wide_summed %>% 
        mutate(c_con_detenido            = if_else(c_con_detenido       == 0, 0, 1),
                c_sin_detenido           = if_else(c_sin_detenido       == 0, 0, 1),
                c_culposo                = if_else(c_culposo            == 0, 0, 1),
                c_doloso                 = if_else(c_doloso             == 0, 0, 1),
                tr_consumado             = if_else(tr_consumado         == 0, 0, 1),
                tr_tentativa             = if_else(tr_tentativa         == 0, 0, 1),
        # Indicadores binarios para delitos 
                d_homicidio_alternas     = if_else(d_homicidio_alternas == 0, 0, 1), 
                d_secuestro_alternas     = if_else(d_secuestro_alternas == 0, 0, 1), 
                d_sexuales_alternas      = if_else(d_sexuales_alternas  == 0, 0, 1), 
                d_salud_alternas         = if_else(d_salud_alternas     == 0, 0, 1), 
                d_robo_alternas          = if_else(d_robo_alternas      == 0, 0, 1), 
                d_familiar_alternas      = if_else(d_familiar_alternas  == 0, 0, 1), 
                d_lesiones_alternas      = if_else(d_lesiones_alternas  == 0, 0, 1), 
                d_extorsion_alternas     = if_else(d_extorsion_alternas == 0, 0, 1), 
                d_objetos_alternas       = if_else(d_objetos_alternas   == 0, 0, 1), 
                d_encubrimiento_alternas = if_else(d_encubrimiento_alternas == 0, 0, 1), 
                d_otros_alternas         = if_else(d_otros_alternas     == 0, 0, 1), 
        # Indicadores binarios para soluciones alternas 
                a_reparatorio            = if_else(a_reparatorio        == 0, 0, 1), 
                a_perdon                 = if_else(a_perdon             == 0, 0, 1), 
                a_suspension             = if_else(a_suspension         == 0, 0, 1), 
                a_criterio               = if_else(a_criterio           == 0, 0, 1), 
                a_sobreseimiento         = if_else(a_sobreseimiento     == 0, 0, 1))




# 2.4.5 Medidas cautelares -----------------------------------------------------
# Crear variables binarias para los delitos
df_cautelares_renombrado1 <- df_cautelares                              %>% 
        # unique() %>%  # Me parece que en medidas cautelares sí tiene sentido que se eliminen las observaciones repetidas. 
        # Unificar categorías de homicidio y feminicidio en 1 
        mutate(homicidio_1 = as.numeric(str_detect(delito, "Homicidio")), 
               homicidio_2 = as.numeric(str_detect(delito, "Feminicidio")), 
               d_homicidio_cautelares   = homicidio_1 + homicidio_2)                 %>% 
        # Unificar categorías para secuestros 
        mutate(secuestro_1 = as.numeric(str_detect(delito, "Secuestro")), 
               secuestro_2 = as.numeric(str_detect(delito, "Privación de la libertad")), 
                d_secuestro_cautelares   = secuestro_1 + secuestro_2)                 %>% 
        # Unificar categorías para delitos sexuales
        mutate(sexuales_1  = as.numeric(str_detect(delito, "Abuso sexual")), 
               sexuales_2  = as.numeric(str_detect(delito, "Violacion")), 
               sexuales_3  = as.numeric(str_detect(delito, "Hostigamiento")), 
               d_sexuales_cautelares    = sexuales_1 + sexuales_2 + sexuales_3)      %>% 
        # Crear variables dummies para otros delitos
        mutate(d_salud_cautelares       = as.numeric(str_detect(delito, "salud")),
               d_robo_cautelares        = as.numeric(str_detect(delito, "Robo")), 
               d_familiar_cautelares    = as.numeric(str_detect(delito, "Violencia familiar")), 
               d_lesiones_cautelares    = as.numeric(str_detect(delito, "Lesiones")),
               d_extorsion_cautelares   = as.numeric(str_detect(delito, "Extorsion")), 
               d_objetos_cautelares     = as.numeric(str_detect(delito, "objetos")), 
               d_encubrimiento_cautelares = as.numeric(str_detect(delito, "Encubrimiento"))) %>% 
        # Crear categoría para el resto de los delitos 
        mutate(d_otros_cautelares = ifelse(d_homicidio_cautelares     != 1 & 
                                           d_salud_cautelares         != 1 &  
                                           d_familiar_cautelares      != 1 & 
                                           d_lesiones_cautelares      != 1 & 
                                           d_encubrimiento_cautelares != 1 & 
                                           d_robo_cautelares          != 1 &
                                           d_extorsion_cautelares     != 1 & 
                                           d_objetos_cautelares       != 1 & 
                                           d_secuestro_cautelares     != 1 & 
                                           d_sexuales_cautelares      != 1, 1, 0)) %>% 
        # Retirar variables innecesarias 
        dplyr::select(-c(homicidio_1, homicidio_2, secuestro_1, secuestro_2,
                                        sexuales_1, sexuales_2, sexuales_3))         

# Renombrar medidas cautelares 
df_cautelares_renombrado2 <- df_cautelares_renombrado1 %>% 
        mutate(medida = case_when(
               medida == "El embargo de bienes;" ~ "Embargo", 
               medida == "El resguardo en su propio domicilio con las modalidades que el juez disponga" ~ "Resguardo en domicilio", 
               medida == "El sometimiento al cuidado o vigilancia de una persona o institución determinada o internamiento a institución determinada;" ~ "Vigilancia o internamiento", 
               medida == "La colocación de localizadores electrónicos" ~ "Localizadores electrónicos", 
               medida == "La exhibición de una garantía económica;" ~ "Garantía económica", 
               medida == "La inmovilización de cuentas y demás valores que se encuentren dentro del sistema financiero;" ~ "Inmovilización de cuentas", 
               medida == "La presentación periódica ante el juez o ante autoridad distinta que aquél designe;" ~ "Presentación periódica", 
               medida == "La prohibición de concurrir a determinadas reuniones o acercarse o ciertos lugares;" ~ "Prohibición de ir a lugares", 
               medida == "La prohibición de convivir, acercarse o comunicarse con determinadas personas, con las víctimas u ofendidos o testigos, siempre que no se afecte el derecho de defensa" ~ "Prohibición de comunicarse con personas", 
               medida == "La prohibición de salir sin autorización del país, de la localidad en la cual reside o del ámbito territorial que fije el juez;" ~ "Prohibición de salir de un lugar", 
               medida == "La separación inmediata del domicilio;" ~ "Separación del domicilio", 
               medida == "La suspensión temporal en el ejercicio de una determinada actividad profesional o laboral" ~ "Suspensión laboral",
               medida == "La suspensión temporal en el ejercicio del cargo cuando se le atribuye un delito cometido por servidores públicos" ~ "Suspensión laboral",
               medida == "Prisión Preventiva" ~ "Prisión preventiva"))

# Crear nueva variable con nombres cortos de los delitos                        
df_cautelares_delitos <- df_cautelares_renombrado2 %>% 
        mutate(delitos_cortos = case_when(d_homicidio_cautelares == 1 ~ "Homicidio",
               d_salud_cautelares         == 1 ~ "Delitos contra la salud",
               d_robo_cautelares          == 1 ~ "Robo",
               d_familiar_cautelares      == 1 ~ "Violencia familiar",
               d_lesiones_cautelares      == 1 ~ "Lesiones",
               d_encubrimiento_cautelares == 1 ~ "Encubrimiento",
               d_extorsion_cautelares     == 1 ~ "Extorsión",
               d_objetos_cautelares       == 1 ~ "objetos aptos para agredir",
               d_secuestro_cautelares     == 1 ~ "Secuestro",
               d_sexuales_cautelares      == 1 ~ "Delitos sexuales",
                d_otros_cautelares        == 1 ~ "Otros delitos"))  %>% 
        mutate(sexo_vinculada = case_when(sexo_vinculada == "Femenino" ~ "Mujeres", 
               sexo_vinculada == "Masculino" ~ "Hombres", 
               sexo_vinculada == "No especificado" ~ "No especificado")) %>% 
        # Crear variable de fecha con objeto tipo date 
        mutate(date_audiencia_caut  = paste(year_audiencia_caut, month_audiencia_caut, sep = "-"), 
                date_audiencia_caut = as.yearmon(date_audiencia_caut))


# Crear variables binarias para medidas cautelares ("m_" como sufijo de medida cautelar)
df_cautelares_vars_wide <- df_cautelares_delitos %>% 
        mutate(m_embargo             = as.numeric(str_detect(medida, "Embargo")), 
                m_resguardo          = as.numeric(str_detect(medida, "Resguardo en domicilio")), 
                m_vigilancia         = as.numeric(str_detect(medida, "Vigilancia o internamiento")), 
                m_localizador        = as.numeric(str_detect(medida, "Localizadores electrónicos")), 
                m_garantia_econ      = as.numeric(str_detect(medida, "Garantía económica")), 
                m_inmov_cuentas      = as.numeric(str_detect(medida, "Inmovilización de cuentas")), 
                m_presentacion       = as.numeric(str_detect(medida, "Presentación periódica")), 
                m_prohib_lugares     = as.numeric(str_detect(medida, "Prohibición de ir a lugares")), 
                m_prohib_comunica    = as.numeric(str_detect(medida, "Prohibición de comunicarse con personas")), 
                m_prohib_salir       = as.numeric(str_detect(medida, "Prohibición de salir de un lugar")), 
                m_separa_domicilio   = as.numeric(str_detect(medida, "Separación del domicilio")), 
                m_suspension_laboral = as.numeric(str_detect(medida, "Suspensión laboral")), 
                m_prision_preventiva = as.numeric(str_detect(medida, "Prisión preventiva")))   %>% 
        # Crear variables para consignación 
        mutate(c_con_detenido = as.numeric(str_detect(consignacion, "Con detenido")), 
                c_sin_detenido = as.numeric(str_detect(consignacion, "Sin detenido"))) %>% 
        # Crear variables para comisión
        mutate(c_culposo = as.numeric(str_detect(comision, "Culposo")), 
                c_doloso = as.numeric(str_detect(comision, "Doloso"))) %>% 
        # Crear variables para realización 
        mutate(tr_consumado = as.numeric(str_detect(realizacion, "Consumado")), 
                tr_tentativa =  as.numeric(str_detect(realizacion, "Tentativa")))


# Sintetizar información a nivel persona 
df_cautelares_wide_summed <- df_cautelares_vars_wide %>% 
        group_by(id_exp, id_per_acusada, year_audiencia_caut, month_audiencia_caut, 
                date_audiencia_caut, sexo_vinculada, edad_vinculada, materia) %>% 
        # Contabilizar alcaldías donde se cometieron delitos 
        summarise(num_alcaldias                = length(unique(alcaldia)), 
                num_consignacion               = length(unique(consignacion)), 
                num_comision                   = length(unique(comision)),
                num_realizacion                = length(unique(realizacion)),
        # Variables de conteo para consignación, comisión y realización 
                c_con_detenido                 = sum(c_con_detenido), 
                c_sin_detenido                 = sum(c_sin_detenido),
                c_culposo                      = sum(c_culposo), 
                c_doloso                       = sum(c_doloso), 
                tr_consumado                   = sum(tr_consumado), 
                tr_tentativa                   = sum(tr_tentativa),
        # Delitos  
                d_homicidio_cautelares         = sum(d_homicidio_cautelares), 
                d_secuestro_cautelares         = sum(d_secuestro_cautelares), 
                d_sexuales_cautelares          = sum(d_sexuales_cautelares), 
                d_salud_cautelares             = sum(d_salud_cautelares), 
                d_robo_cautelares              = sum(d_robo_cautelares), 
                d_familiar_cautelares          = sum(d_familiar_cautelares), 
                d_lesiones_cautelares          = sum(d_lesiones_cautelares), 
                d_extorsion_cautelares         = sum(d_extorsion_cautelares), 
                d_objetos_cautelares           = sum(d_objetos_cautelares), 
                d_encubrimiento_cautelares     = sum(d_encubrimiento_cautelares), 
                d_otros_cautelares             = sum(d_otros_cautelares),
        # Medidas cautelares
                m_embargo                      = sum(m_embargo), 
                m_resguardo                    = sum(m_resguardo), 
                m_vigilancia                   = sum(m_vigilancia), 
                m_localizador                  = sum(m_localizador), 
                m_garantia_econ                = sum(m_garantia_econ), 
                m_inmov_cuentas                = sum(m_inmov_cuentas), 
                m_presentacion                 = sum(m_presentacion), 
                m_prohib_lugares               = sum(m_prohib_lugares), 
                m_prohib_comunica              = sum(m_prohib_comunica), 
                m_prohib_salir                 = sum(m_prohib_salir), 
                m_separa_domicilio             = sum(m_separa_domicilio), 
                m_suspension_laboral           = sum(m_suspension_laboral), 
                m_prision_preventiva           = sum(m_prision_preventiva))   %>% 
        ungroup() %>% 
        mutate(num_delitos = d_homicidio_cautelares + d_secuestro_cautelares + 
                        d_sexuales_cautelares + d_salud_cautelares + 
                        d_robo_cautelares +
                        d_familiar_cautelares + d_lesiones_cautelares + 
                        d_extorsion_cautelares + d_objetos_cautelares + 
                        d_encubrimiento_cautelares + d_otros_cautelares) %>% 
        mutate(num_medidas_cautelares = m_embargo + m_resguardo + m_vigilancia +
                        m_localizador + m_garantia_econ + m_inmov_cuentas +
                        m_presentacion + m_prohib_lugares + m_prohib_comunica +
                        m_prohib_salir + m_separa_domicilio + m_suspension_laboral +
                        m_prision_preventiva) %>% 
        mutate(base_medida_cautelar = 1)

# Base final desagregada por personas 
# Variables de conteo a indicadores binarios 
df_cautelares_acusados <- df_cautelares_wide_summed %>% 
        mutate(c_con_detenido              = if_else(c_con_detenido         == 0, 0, 1),
                c_sin_detenido             = if_else(c_sin_detenido         == 0, 0, 1),
                c_culposo                  = if_else(c_culposo              == 0, 0, 1),
                c_doloso                   = if_else(c_doloso               == 0, 0, 1),
                tr_consumado               = if_else(tr_consumado           == 0, 0, 1),
                tr_tentativa               = if_else(tr_tentativa           == 0, 0, 1),
                # Indicadores binarios para delitos 
                d_homicidio_cautelares     = if_else(d_homicidio_cautelares == 0, 0, 1), 
                d_secuestro_cautelares     = if_else(d_secuestro_cautelares == 0, 0, 1), 
                d_sexuales_cautelares      = if_else(d_sexuales_cautelares  == 0, 0, 1), 
                d_salud_cautelares         = if_else(d_salud_cautelares     == 0, 0, 1), 
                d_robo_cautelares          = if_else(d_robo_cautelares      == 0, 0, 1), 
                d_familiar_cautelares      = if_else(d_familiar_cautelares  == 0, 0, 1), 
                d_lesiones_cautelares      = if_else(d_lesiones_cautelares  == 0, 0, 1), 
                d_extorsion_cautelares     = if_else(d_extorsion_cautelares == 0, 0, 1), 
                d_objetos_cautelares       = if_else(d_objetos_cautelares   == 0, 0, 1), 
                d_encubrimiento_cautelares = if_else(d_encubrimiento_cautelares == 0, 0, 1), 
                d_otros_cautelares         = if_else(d_otros_cautelares     == 0, 0, 1), 
                # Indicadores binarios para medidas cautelares 
                m_embargo                  = if_else(m_embargo              == 0, 0, 1), 
                m_resguardo                = if_else(m_resguardo            == 0, 0, 1), 
                m_vigilancia               = if_else(m_vigilancia           == 0, 0, 1), 
                m_localizador              = if_else(m_localizador          == 0, 0, 1), 
                m_garantia_econ            = if_else(m_garantia_econ        == 0, 0, 1), 
                m_inmov_cuentas            = if_else(m_inmov_cuentas        == 0, 0, 1), 
                m_presentacion             = if_else(m_presentacion         == 0, 0, 1), 
                m_prohib_lugares           = if_else(m_prohib_lugares       == 0, 0, 1), 
                m_prohib_comunica          = if_else(m_prohib_comunica      == 0, 0, 1), 
                m_prohib_salir             = if_else(m_prohib_salir         == 0, 0, 1), 
                m_separa_domicilio         = if_else(m_separa_domicilio     == 0, 0, 1), 
                m_suspension_laboral       = if_else(m_suspension_laboral   == 0, 0, 1), 
                m_prision_preventiva       = if_else(m_prision_preventiva   == 0, 0, 1)) 



# 2.4.6 Sentencias -------------------------------------------------------------
# Crear variables binarias para los delitos
df_sentencias_renombrado1 <- df_sentencias %>% 
        # Unificar categorías de homicidio y feminicidio en 1 
        mutate(homicidio_1 = as.numeric(str_detect(delito, "Homicidio")), 
               homicidio_2 = as.numeric(str_detect(delito, "Feminicidio")), 
               d_homicidio_sentencia   = homicidio_1 + homicidio_2)                 %>% 
        # Unificar categorías para secuestros 
        mutate(secuestro_1 = as.numeric(str_detect(delito, "Secuestro")), 
               secuestro_2 = as.numeric(str_detect(delito, "Privación de la libertad")), 
               d_secuestro_sentencia   = secuestro_1 + secuestro_2)                 %>% 
        # Unificar categorías para delitos sexuales
        mutate(sexuales_1  = as.numeric(str_detect(delito, "Abuso sexual")), 
               sexuales_2  = as.numeric(str_detect(delito, "Violacion")), 
               sexuales_3  = as.numeric(str_detect(delito, "Hostigamiento")), 
               d_sexuales_sentencia    = sexuales_1 + sexuales_2 + sexuales_3)      %>% 
        # Crear variables dummies para otros delitos
        mutate(d_salud_sentencia       = as.numeric(str_detect(delito, "salud")),
               d_robo_sentencia        = as.numeric(str_detect(delito, "Robo")), 
               d_familiar_sentencia    = as.numeric(str_detect(delito, "Violencia familiar")), 
               d_lesiones_sentencia    = as.numeric(str_detect(delito, "Lesiones")),
               d_extorsion_sentencia   = as.numeric(str_detect(delito, "Extorsion")), 
               d_objetos_sentencia     = as.numeric(str_detect(delito, "objetos")), 
               d_encubrimiento_sentencia = as.numeric(str_detect(delito, "Encubrimiento"))) %>% 
        # Crear categoría para el resto de los delitos 
        mutate(d_otros_sentencia = ifelse(d_homicidio_sentencia != 1 & 
                                      d_salud_sentencia         != 1 & 
                                      d_robo_sentencia          != 1 & 
                                      d_familiar_sentencia      != 1 & 
                                      d_lesiones_sentencia      != 1 & 
                                      d_encubrimiento_sentencia != 1 &
                                      d_extorsion_sentencia     != 1 & 
                                      d_objetos_sentencia       != 1 & 
                                      d_homicidio_sentencia     != 1 & 
                                      d_sexuales_sentencia      != 1, 1, 0)) %>% 
        # Retirar variables innecesarias 
        dplyr::select(-c(homicidio_1, homicidio_2, secuestro_1, secuestro_2,
                         sexuales_1, sexuales_2, sexuales_3)) 


# Renombrar tipo de soluciones alternas (consultar cuáles son los nombres más adecuados)
df_sentencias_renombrado2 <- df_sentencias_renombrado1 %>% 
        mutate(sentencia = case_when(sentencia == sentencia ~ sentencia))


# Crear nueva variable con nombres cortos de los delitos                        
df_sentencias_delitos <- df_sentencias_renombrado2 %>% 
        mutate(delitos_cortos = case_when(d_homicidio_sentencia == 1 ~ "Homicidio",
                d_salud_sentencia     == 1 ~ "Delitos contra la salud",
                d_robo_sentencia      == 1 ~ "Robo",
                d_familiar_sentencia  == 1 ~ "Violencia familiar",
                d_lesiones_sentencia  == 1 ~ "Lesiones",
                d_encubrimiento_sentencia == 1 ~ "Encubrimiento",
                d_extorsion_sentencia == 1 ~ "Extorsión",
                d_objetos_sentencia   == 1 ~ "objetos aptos para agredir",
                d_secuestro_sentencia == 1 ~ "Secuestro",
                d_sexuales_sentencia  == 1 ~ "Delitos sexuales",
                d_otros_sentencia     == 1 ~ "Otros delitos")) %>% 
        # Crear variable de fecha con objeto tipo date 
        mutate(date_sentencia = paste(year_sentencia, month_sentencia, sep = "-"), 
                date_sentencia = as.yearmon(date_sentencia))



# Crear variables binarias para sentencias ("sentencia_" como sufijo)
df_sentencias_vars_wide <- df_sentencias_delitos %>% 
        mutate(sentencia_absolutoria = as.numeric(str_detect(sentencia, "Absolutoria")), 
                sentencia_condenatoria = as.numeric(str_detect(sentencia, "Condenatoria")))  %>% 
                # Crear variables para consignación 
        mutate(c_con_detenido = as.numeric(str_detect(consignacion, "Con detenido")), 
                c_sin_detenido = as.numeric(str_detect(consignacion, "Sin detenido"))) %>% 
        # Crear variables para comisión
        mutate(c_culposo = as.numeric(str_detect(comision, "Culposo")), 
                c_doloso = as.numeric(str_detect(comision, "Doloso"))) %>% 
        # Crear variables para realización 
        mutate(tr_consumado = as.numeric(str_detect(realizacion, "Consumado")), 
                tr_tentativa =  as.numeric(str_detect(realizacion, "Tentativa")))


# Base final desagregada por personas 
df_sentencias_wide_summed <- df_sentencias_vars_wide %>% 
        group_by(id_exp, id_per_acusada, year_sentencia, month_sentencia, 
                date_sentencia, sexo_sentenciada, edad_sentenciada, 
                materia, years_sentencia, months_sentencia) %>%
        # Contabilizar alcaldías donde se cometieron delitos 
        summarise(num_alcaldias              = length(unique(alcaldia)), 
                num_consignacion             = length(unique(consignacion)), 
                num_comision                 = length(unique(comision)),
                num_realizacion              = length(unique(realizacion)),
        # Variables de conteo para consignación, comisión y realización 
                c_con_detenido               = sum(c_con_detenido), 
                c_sin_detenido               = sum(c_sin_detenido),
                c_culposo                    = sum(c_culposo), 
                c_doloso                     = sum(c_doloso), 
                tr_consumado                 = sum(tr_consumado), 
                tr_tentativa                 = sum(tr_tentativa),
        # Delitos 
                d_homicidio_sentencia        = sum(d_homicidio_sentencia), 
                d_secuestro_sentencia        = sum(d_secuestro_sentencia), 
                d_sexuales_sentencia         = sum(d_sexuales_sentencia), 
                d_salud_sentencia            = sum(d_salud_sentencia), 
                d_robo_sentencia             = sum(d_robo_sentencia), 
                d_familiar_sentencia         = sum(d_familiar_sentencia), 
                d_lesiones_sentencia         = sum(d_lesiones_sentencia), 
                d_extorsion_sentencia        = sum(d_extorsion_sentencia), 
                d_objetos_sentencia          = sum(d_objetos_sentencia), 
                d_encubrimiento_sentencia    = sum(d_encubrimiento_sentencia), 
                d_otros_sentencia            = sum(d_otros_sentencia),
        # Sentencias
                sentencia_absolutoria  = sum(sentencia_absolutoria), 
                sentencia_condenatoria = sum(sentencia_condenatoria)) %>% 
        ungroup() %>% 
        mutate(num_delitos = d_homicidio_sentencia + d_homicidio_sentencia + 
                        d_sexuales_sentencia + d_salud_sentencia + 
                        d_robo_sentencia + d_familiar_sentencia + 
                        d_lesiones_sentencia + d_extorsion_sentencia + 
                        d_objetos_sentencia + 
                        d_encubrimiento_sentencia + d_otros_sentencia) %>% 
        mutate(base_sentencias = 1)

# Base final desagregada por personas 
# Variables de conteo a indicadores binarios 
df_sentencias_acusados <- df_sentencias_wide_summed %>% 
        mutate(c_con_detenido             = if_else(c_con_detenido            == 0, 0, 1),
                c_sin_detenido            = if_else(c_sin_detenido            == 0, 0, 1),
                c_culposo                 = if_else(c_culposo                 == 0, 0, 1),
                c_doloso                  = if_else(c_doloso                  == 0, 0, 1),
                tr_consumado              = if_else(tr_consumado              == 0, 0, 1),
                tr_tentativa              = if_else(tr_tentativa              == 0, 0, 1),
                # Indicadores binarios para delitos     
                d_homicidio_sentencia     = if_else(d_homicidio_sentencia     == 0, 0, 1), 
                d_secuestro_sentencia     = if_else(d_secuestro_sentencia     == 0, 0, 1), 
                d_sexuales_sentencia      = if_else(d_sexuales_sentencia      == 0, 0, 1), 
                d_salud_sentencia         = if_else(d_salud_sentencia         == 0, 0, 1), 
                d_robo_sentencia          = if_else(d_robo_sentencia          == 0, 0, 1), 
                d_familiar_sentencia      = if_else(d_familiar_sentencia      == 0, 0, 1), 
                d_lesiones_sentencia      = if_else(d_lesiones_sentencia      == 0, 0, 1), 
                d_extorsion_sentencia     = if_else(d_extorsion_sentencia     == 0, 0, 1), 
                d_objetos_sentencia       = if_else(d_objetos_sentencia       == 0, 0, 1), 
                d_encubrimiento_sentencia = if_else(d_encubrimiento_sentencia == 0, 0, 1), 
                d_otros_sentencia         = if_else(d_otros_sentencia         == 0, 0, 1), 
                # Indicadores binarios para sentencia 
                sentencia_absolutoria     = if_else(sentencia_absolutoria     == 0, 0, 1), 
                sentencia_condenatoria    = if_else(sentencia_condenatoria    == 0, 0, 1))

beepr::beep(5)


# 3. Controles de calidad ------------------------------------------------------
# 3.1 Identificar folios repetidos ---------------------------------------------
# Asuntos ingresados 
df_freq_exp <- table(df_asuntos_acusados$id_exp)
df_freq_per <- as.data.frame(table(df_asuntos_acusados$id_per_acusada))
        table(df_freq_per$Freq) # Varias personas se repiten dos veces

# Personas agredidas 
df_freq_exp <- as.data.frame(table(df_personas_expediente$id_exp))
        table(df_freq_exp$Freq) # No hay ninguna persona repetida

# Situación jurídica 
df_freq_exp <- table(df_sitjurid_acusados$id_exp)
df_freq_per <- as.data.frame(table(df_sitjurid_acusados$id_per_acusada))
        table(df_freq_per$Freq)  # Varias personas se repiten dos veces 

# Soluciones alternas 
df_freq_exp <- table(df_alternas_acusados$id_exp)
df_freq_per <- as.data.frame(table(df_alternas_acusados$id_per_acusada)) 
        table(df_freq_per$Freq) # No hay ninguna persona repetida

# Medidas cautelares 
df_freq_exp <- table(df_cautelares_acusados$id_exp)
df_freq_per <- as.data.frame(table(df_cautelares_acusados$id_per_acusada)) 
        table(df_freq_per$Freq) # Sólo dos se repiten y tienen expedientes distinto
        
# Sentencias 
df_freq_exp <- table(df_sentencias_acusados$id_exp)
df_freq_per <- as.data.frame(table(df_sentencias_acusados$id_per_acusada)) 
        table(df_freq_per$Freq)

# Este folio único para persona está repetido en sentencias: 2558706
# Pero al buscarlo en las bases aparecen edades distintas, como si se hubiera
# utilizado para personas distintas 


# Para hacer revisión si las personas repetidas sí son por momentos distintos, 
# podría agrupar por identificador y crear variables contadoras para año, expediente
# y otras en donde se espere ver diferencias, como la materia 
        
# 3.2 Casos que en teoría deberían ser excluyentes -----------------------------
# En asuntos ingresados 
table(df_asuntos_acusados$c_doloso,          df_asuntos_acusados$c_culposo)
table(df_asuntos_acusados$c_con_detenido,    df_asuntos_acusados$c_sin_detenido)
table(df_asuntos_acusados$tr_tentativa,      df_asuntos_acusados$tr_consumado)
  
# En situación jurídica   
table(df_sitjurid_acusados$c_doloso,         df_sitjurid_acusados$c_culposo)
table(df_sitjurid_acusados$c_con_detenido,   df_sitjurid_acusados$c_sin_detenido)
table(df_sitjurid_acusados$tr_tentativa,     df_sitjurid_acusados$tr_consumado)
  
# En soluciones alternas  
table(df_alternas_acusados$c_doloso,         df_alternas_acusados$c_culposo)
table(df_alternas_acusados$c_con_detenido,   df_alternas_acusados$c_sin_detenido)
table(df_alternas_acusados$tr_tentativa,     df_alternas_acusados$tr_consumado)

# En medidas cautelares 
table(df_cautelares_acusados$c_doloso,       df_cautelares_acusados$c_culposo)
table(df_cautelares_acusados$c_con_detenido, df_cautelares_acusados$c_sin_detenido)
table(df_cautelares_acusados$tr_tentativa,   df_cautelares_acusados$tr_consumado)

# En sentencias 
table(df_sentencias_acusados$c_doloso,       df_sentencias_acusados$c_culposo)
table(df_sentencias_acusados$c_con_detenido, df_sentencias_acusados$c_sin_detenido)
table(df_sentencias_acusados$tr_tentativa,   df_sentencias_acusados$tr_consumado)


# 4. Guardar bases limpias -----------------------------------------------------

# Bases a nivel delito 
# Renombrar bases con nombres definitivos 
df_asuntos_ingresados_nivel_delito   <- df_asuntos_delitos
df_situacion_juridica_nivel_delito   <- df_sitjurid_delitos
df_soluciones_alternas_nivel_delito  <- df_alternas_delitos
df_medidas_cautelares_nivel_delito   <- df_cautelares_delitos
df_sentencias_nivel_delito           <- df_sentencias_delitos

# Guardar bases en formato .RData
save(df_asuntos_ingresados_nivel_delito,  file = paste0(out, "df_asuntos_ingresados_nivel_delito.RData"))
save(df_situacion_juridica_nivel_delito,  file = paste0(out, "df_situacion_juridica_nivel_delito.RData"))
save(df_soluciones_alternas_nivel_delito, file = paste0(out, "df_soluciones_alternas_nivel_delito.RData"))
save(df_medidas_cautelares_nivel_delito,  file = paste0(out, "df_medidas_cautelares_nivel_delito.RData"))
save(df_sentencias_nivel_delito,          file = paste0(out, "df_sentencias_nivel_delito.RData"))



# Bases a nivel persona
# Renombrar bases con nombres definitivos 
df_asuntos_ingresados_nivel_acusado  <- df_asuntos_acusados
df_situacion_juridica_nivel_acusado  <- df_sitjurid_acusados
df_soluciones_alternas_nivel_acusado <- df_alternas_acusados
df_medidas_cautelares_nivel_acusado  <- df_cautelares_acusados
df_sentencias_nivel_acusado          <- df_sentencias_acusados

# Guardar en formato .RDAta
save(df_asuntos_ingresados_nivel_acusado,  file = paste0(out, "df_asuntos_ingresados_nivel_acusado.RData"))
save(df_situacion_juridica_nivel_acusado,  file = paste0(out, "df_situacion_juridica_nivel_acusado.RData"))
save(df_soluciones_alternas_nivel_acusado, file = paste0(out, "df_soluciones_alternas_nivel_acusado.RData"))
save(df_medidas_cautelares_nivel_acusado,  file = paste0(out, "df_medidas_cautelares_nivel_acusado.RData"))
save(df_sentencias_nivel_acusado,          file = paste0(out, "df_sentencias_nivel_acusado.RData"))


# Bases de víctimas 
df_personas_agredidas_nivel_persona    <- df_personas
df_personas_agredidas_nivel_expediente <- df_personas_expediente

save(df_personas_agredidas_nivel_persona,    file = paste0(out, "df_personas_agredidas_nivel_persona.RData"))
save(df_personas_agredidas_nivel_expediente, file = paste0(out, "df_personas_agredidas_nivel_expediente.RData"))


beepr::beep(5)
  
  
# Fin del código #