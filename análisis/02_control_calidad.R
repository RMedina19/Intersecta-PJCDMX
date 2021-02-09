#------------------------------------------------------------------------------#
# Proyecto:                   TRIBUNAL SUPERIOR DE JUSTICIA DE CIUDAD DE MÉXICO
# Objetivo:                   Encontrar inconsistencias en los datos
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          27 de enero de 2021
# Última actualización:       02 de febrero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, magrittr, dplyr, here, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
dir <- here::here()
inp <- "datos_limpios/"
out <- "datos_limpios/"

setwd(dir)


# 1. Cargar datos --------------------------------------------------------------

load(paste0(inp, "df_TJCDMX_full.RData"))

# 2. Crear bases con subconjuntos ----------------------------------------------
# Desagregar según las mismas variables pero de diferentes bases (pestañas)
dim(df_TJCDMX_full)
names(df_TJCDMX_full)

df_edad <- df_TJCDMX_full %>% 
        dplyr::select("id_exp", "id_per_acusada", "aparece_en_bases", 
                contains("edad")) %>% 
        arrange(desc(aparece_en_bases))


df_sexo <- df_TJCDMX_full %>% 
        dplyr::select("id_exp", "id_per_acusada", "aparece_en_bases", 
                contains("sexo")) %>% 
        arrange(desc(aparece_en_bases))

df_fechas <- df_TJCDMX_full %>% 
        dplyr::select("id_exp", "id_per_acusada", "aparece_en_bases", 
                contains("date_"), contains("year_"), contains("month_")) %>% 
        arrange(desc(aparece_en_bases))

df_juridico <- df_TJCDMX_full %>% 
        dplyr::select("id_exp", "id_per_acusada", "aparece_en_bases", 
                contains("num_"), contains("materia_"), contains("c_"), 
                contains("tr_")) %>% 
        arrange(desc(aparece_en_bases))

df_delitos <- df_TJCDMX_full %>% 
        dplyr::select("id_exp", "id_per_acusada", "aparece_en_bases", 
                contains("num_delitos_"), contains("d_")) %>% 
        arrange(desc(aparece_en_bases))


# 3. Buscar inconsistencias ----------------------------------------------------
# Distribución de los datos según el número de bases en los que aparecen 
table(df_edad$aparece_en_bases)
table(df_sexo$aparece_en_bases)
table(df_fechas$aparece_en_bases)
table(df_juridico$aparece_en_bases)
table(df_delitos$aparece_en_bases)


# Crar variables indicadoras para inconsistencias 
df_edad_error <- df_edad %>% 
        # replace_na(list(edad_acusada_asunto = 0,
        #         edad_acusada_sitjurid = 0,
        #         edad_acusada_alternas = 0,
        #         edad_acusada_cautelares = 0,
        #         edad_sentenciada = 0)) %>%
        mutate(edad_igual = ((edad_acusada_asunto == edad_acusada_sitjurid) & 
        (edad_acusada_asunto == edad_acusada_alternas) &
        (edad_acusada_asunto == edad_acusada_cautelares) & 
        (edad_acusada_asunto == edad_sentenciada))) 


df_sexo_error <- df_sexo %>% 
        mutate(sexo_igual = ((sexo_acusada_asunto == sexo_acusada_sitjurid)))

