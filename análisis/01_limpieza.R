#------------------------------------------------------------------------------#
# Objetivo:                     Procesar datos de la PJCDMX
# Encargada:                    Regina Isabel Medina Rosales
# Correo:                       rmedina@intersecta.org
# Fecha de creación:            14 de enero de 2021
# Última actualización:         23 de enero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, here)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
dir <- paste0(here::here(), "/GitHub/Intersecta-PJCDMX")
inp <- "datos_crudos/"
out <- "datos_limpios/"

setwd(dir)

# 1. Cargar datos --------------------------------------------------------------

# Sobre delitos cometidos en la Ciudad de México entre mayo-2011 y sep-2020
# Información de los asuntos ingresados por los diversos delitos
df_asuntos_crudo       <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                                                skip = 8, sheet = "ingresados")

# Información de las personas involucradas como víctimas u ofendidas 
df_personas_crudo      <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                                                 skip = 8, sheet = "victOfend")

# Información de los asuntos con resolución del auto de plazo constitucional
df_sitjurid_crudo      <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                                                skip = 8, sheet = "situaJurid")  

# Información de soluciones alternas o terminaciones anticipadas
df_alternas_crudo      <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                                    skip = 8, sheet = "solAlternasTermAnticip")

# Información de la medida cautelar de prisión preventiva
df_cautelares_crudo    <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                                             skip = 8, sheet = "medCautelares")

# Información de las sentencias emitidas en primera instancia 
df_sentencias_crudo    <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                                                skip = 7, sheet = "sentencias")

        
# 2. Limpiar datos -------------------------------------------------------------
# 2.1 Renombrar variables ------------------------------------------------------
df_asuntos <- df_asuntos_crudo          %>% 
        rename(materia           = "Materia", 
                id_exp           = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora  = "Indice para agrupación 2\r\n\r\n(Personas)", 
                year_ingreso     = "Año ingreso", 
                month_ingreso    = "Mes ingreso", 
                sexo_indiciada   = "Sexo de la persona indiciada", 
                edad_indiciada   = "Edad de la persona indiciada",
                delito   = "Tipo delictivo", 
                desag_estad      = "Desagregado estadístico", 
                consignacion     = "Consignacion", 
                comision         = "Comisión", 
                realizacion      = "Realización", 
                alcaldia_ocurr   = "Alcaldía de ocurrencia")

df_personas <- df_personas_crudo        %>% 
        rename(id_exp           = "Indice para agrupación 1\r\n(Expediente / Carpeta)", 
                id_per_ofendida = "Indice para agrupación 3\r\n(Persona)", 
                sexo_victima    = "Sexo de la persona involucrada como víctima u ofendida", 
                edad_victima    = "Edad de la persona involucrada como víctima u ofendida", 
                relacion        = "Relación entre la persona involucrada como víctima u ofendida y la persona probable responsable de la comisión del delito")


df_sitjurid <- df_sitjurid_crudo        %>%
        rename(materia           = "Materia", 
                id_exp           = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora  = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_resolucion  = "Año resolución",  
                month_resolucion = "Mes resolución", 
                sexo_procesada   = "Sexo de la persona procesada", 
                edad_procesada   = "Edad de la persona procesada",
                delito   = "Tipo delictivo", 
                desag_estad      = "Desagregado estadístico", 
                consignacion     = "Consignacion", 
                comision         = "Comisión", 
                realizacion      = "Realización", 
                alcaldia_ocurr   = "Alcaldía de ocurrencia", 
                resolución       = "Resolución a la situación jurídica") 

df_alternas <- df_alternas_crudo          %>% 
        rename(materia         = "Materia", 
                id_exp  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_audiencia  = "Año audiencia", 
                month_audiencia  = "Mes audiencia", 
                sexo_indiciada = "Sexo de la persona indiciada", 
                edad_indiciada = "Edad de la persona indiciada",
                delito = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignacion   = "Consignacion", 
                comision       = "Comisión", 
                realizacion    = "Realización", 
                alcaldia_ocurr = "Alcaldía de ocurrencia", 
                tipo_solución  = "Tipo de solución alterna o terminación anticipada")

df_cautelares <- df_cautelares_crudo %>% 
        rename(materia         = "Materia", 
                id_exp  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_audiencia  = "Año audiencia",  
                month_audiencia  = "Mes audiencia", 
                sexo_vinculada = "Sexo de la persona vinculada", 
                edad_vinculada = "Edad de la persona vinculada",
                delito = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignacion   = "Consignacion", 
                comision       = "Comisión", 
                realizacion    = "Realización", 
                alcaldia_ocurr = "Alcaldía de ocurrencia", 
                medida    = "Tipo de medida cautelar") 

df_sentencias <- df_sentencias_crudo %>% 
        rename(materia         = "Materia", 
                id_exp  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_sentencia  = "Año sentencia",  
                month_sentencia  = "Mes sentencia", 
                sexo_sentenciada = "Sexo", 
                edad_sentenciada = "Edad",
                delito = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignacion   = "Consignación", 
                comision       = "Comisión", 
                realizacion    = "Realización", 
                alcaldia_ocurr = "Alcaldía de ocurrencia del delito", 
                forma_proceso  = "Forma del proceso", 
                tipo_sentencia = "Tipo de sentencia", 
                years_sentencia = "Años de sentencia", 
                months_sentencia = "Meses de sentencia", 
                días_sentencia = "Días de sentencia") 

# 2.2 Identificar número de observaciones, expedientes y personas únicos ------- 
# expediente > agresor o víctima > delitos > medida cautelar o solución alterna

# Asuntos - Mínimo grado de desagregación:  agresor (< expediente)
dim(df_asuntos)[1]                              # 301,836 observaciones (asuntos por expediente y agresor)
length(unique(df_asuntos$id_exp))               # 219,447 expedientes únicos
length(unique(df_asuntos$id_per_agresora))      # 255,793 agresores únicos

# Personas ofendidas - Mínimo grado de desagregación:  víctima (< expediente)
dim(df_personas)[1]                             # 335,319 observaciones (expediente por víctima)
length(unique(df_personas$id_exp))              # 231,186 expedientes únicos
length(unique(df_personas$id_per_ofendida))     # 252,738 víctimas únicas

# Situación jurídica - Mínimo grado de desagregación:  agresor(< expediente)
dim(df_sitjurid)[1]                             # 203,053 observaciones (asuntos con resolución de auto plazo por expediente y agresor)
length(unique(df_sitjurid$id_exp))              # 152,686 expedientes únicos
length(unique(df_sitjurid$id_per_agresora))     # 177,683 agresores únicos

# Soluciones alternas - Mínimo grado de desagregación: solución alterna (< delito)
dim(df_alternas)[1]                             #  28,699 observaciones (soluciones alternas por expediente y agresores)
length(unique(df_alternas$id_exp))              #  21,987 expedientes únicos
length(unique(df_alternas$id_per_agresora))     #  25,735 agresores únicos

# Medidas cautelares - Mínimo grado de desagregación: medida cautelar (< delito)
dim(df_cautelares)[1]                           # 176,812 observaciones (medidas cautelares)
length(unique(df_cautelares$id_exp))            #  48,286 expedientes únicos
length(unique(df_cautelares$id_per_agresora))   #  63,260 agresores únicos

# Sentencias - Mínimo grado de desagregación: delito (< agresor)
dim(df_sentencias)[1]                           # 129,497 observaciones (sentencias por delito)
length(unique(df_sentencias$id_exp))            #  97,177 expedientes únicos 
length(unique(df_sentencias$id_per_agresora))   # 116,378 agresores únicos


# 2.3 Identificar folios repetidos ---------------------------------------------
# Asuntos - Revisar folios repetidos 
df_freq  <- table(df_asuntos$id_per_agresora)

df_freq2 <- as.data.frame(table(df_freq)) %>% 
                rename("Número de repeticiones" = df_freq, 
                        "Observaciones" = Freq)

# Personas ofendidas - Revisar folios repetidos
df_freq  <- table(df_personas$id_per_ofendida)

df_freq2 <- as.data.frame(table(df_freq)) %>% 
                rename("Número de repeticiones" = df_freq, 
                        "Observaciones" = Freq)

# Situación jurídica - Revisar folios repetidos
df_freq  <- table(df_sitjurid$id_per_agresora)

df_freq2 <- as.data.frame(table(df_freq)) %>% 
                rename("Número de repeticiones" = df_freq, 
                        "Observaciones" = Freq)

# Soluciones alternas - Revisar folios repetidos
df_freq  <- table(df_alternas$id_per_agresora)

df_freq2 <- as.data.frame(table(df_freq)) %>% 
                rename("Número de repeticiones" = df_freq, 
                        "Observaciones" = Freq)

# Medidas cautelares - Revisar folios repetidos
df_freq  <- table(df_cautelares$id_per_agresora)

df_freq2 <- as.data.frame(table(df_freq)) %>% 
                rename("Número de repeticiones" = df_freq, 
                        "Observaciones" = Freq)

# Sentencias - Revisar folios repetidos
df_freq  <- table(df_sentencias$id_per_agresora)

df_freq2 <- as.data.frame(table(df_freq)) %>% 
                rename("Número de repeticiones" = df_freq, 
                        "Observaciones" = Freq)

# Aunque tengan los mismos datos en cada variable, no hay observaciones repetidas.
# La documentación del TSJ-CDMX indica que cada renglón u observación es un 
# asunto, delito, medida cautelar o sentencia que provoca que se repitan tanto 
# los folios de expediente como los folios de personas. 

# 2.4 Crear nuevas variables ---------------------------------------------------

# Crear variables binarias para los delitos
df_cautelares_renombrado1 <- df_cautelares                              %>% 
        filter(year_audiencia != "2015")                                %>% # Filtrar 2015 
        # Unificar categorías de homicidio y feminicidio en 1 
        mutate(homicidio_1 = as.numeric(str_detect(delito, "Homicidio")), 
               homicidio_2 = as.numeric(str_detect(delito, "Feminicidio")), 
               homicidio   = homicidio_1 + homicidio_2)                 %>% 
        # Unificar categorías para secuestros 
        mutate(secuestro_1 = as.numeric(str_detect(delito, "Secuestro")), 
               secuestro_2 = as.numeric(str_detect(delito, "Privación de la libertad")), 
               secuestro   = secuestro_1 + secuestro_2)                 %>% 
        # Unificar categorías para delitos sexuales
        mutate(sexuales_1  = as.numeric(str_detect(delito, "Abuso sexual")), 
               sexuales_2  = as.numeric(str_detect(delito, "Violacion")), 
               sexuales_3  = as.numeric(str_detect(delito, "Hostigamiento")), 
               sexuales    = sexuales_1 + sexuales_2 + sexuales_3)      %>% 
        # Crear variables dummies para otros delitos
        mutate(salud       = as.numeric(str_detect(delito, "salud")),
               robo        = as.numeric(str_detect(delito, "Robo")), 
               familiar    = as.numeric(str_detect(delito, "Violencia familiar")), 
               lesiones    = as.numeric(str_detect(delito, "Lesiones")),
               extorsion   = as.numeric(str_detect(delito, "Extorsion")), 
               objetos     = as.numeric(str_detect(delito, "Portación de objetos")), 
               encubrimiento = as.numeric(str_detect(delito, "Encubrimiento"))) %>% 
        # Crear categoría para el resto de los delitos 
        mutate(otros = ifelse(homicidio != 1 & salud != 1    & robo != 1 & 
                              familiar  != 1 & lesiones != 1 & encubrimiento != 1 &
                              extorsion != 1 & objetos  != 1 & secuestro != 1 & 
                              sexuales  != 1, 1, 0)) %>% 
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
        mutate(delitos_cortos = case_when(homicidio == 1 ~ "Homicidio",
                                          salud     == 1 ~ "Delitos contra la salud",
                                          robo      == 1 ~ "Robo",
                                          familiar  == 1 ~ "Violencia familiar",
                                          lesiones  == 1 ~ "Lesiones",
                                          encubrimiento == 1 ~ "Encubrimiento",
                                          extorsion == 1 ~ "Extorsión",
                                          objetos   == 1 ~ "Portación de objetos aptos para agredir",
                                          secuestro == 1 ~ "Secuestro",
                                          sexuales  == 1 ~ "Delitos sexuales",
                                          otros     == 1 ~ "Otros delitos"))


# 3. Unificar bases ------------------------------------------------------------
# Unir base de asuntos ingresados con datos de personas ofendidas
df_unida1 <- df_asuntos %>% 
        full_join(df_personas,   by = c("id_exp")) %>% 
        dplyr::select(materia, id_exp, id_per_agresora, year_ingreso, 
                month_ingreso, sexo_indiciada, edad_indiciada, id_per_ofendida,
                sexo_victima, edad_victima, relacion, delito, 
                desag_estad, consignacion, comision, realizacion, alcaldia_ocurr)


# Unir con base de situación jurídica 
df_unida2 <- df_unida1 %>% 
        full_join(df_sitjurid,   by = c("id_exp", "id_per_agresora"))

        full_join(df_sitjurid,   by = c("id_exp", "id_per_agresora")) %>%
        full_join(df_alternas,   by = c("id_exp", "id_per_agresora")) %>% 
        full_join(df_cautelares, by = c("id_exp", "id_per_agresora")) %>% 
        full_join(df_sentencias, by = c("id_exp", "id_per_agresora")) 

sum(is.na(df_completa$id_exp))
sum(is.na(df_completa$id_per_ofendida))
sum(is.na(df_completa$id_per_agresora))



# 4. Guardar bases limpias -----------------------------------------------------
# Renombrar bases con nombres definitivos 
df_cautelares <- df_cautelares_delitos

save(df_cautelares, file = paste0(out, "df_cautelares.RData"))




# Fin del código #