#------------------------------------------------------------------------------#
# Objetivo:                     Unir las bases de datos de la PJCDMX
# Encargada:                    Regina Isabel Medina Rosales
# Correo:                       rmedina@intersecta.org
# Fecha de creación:            27 de enero de 2021
# Última actualización:         02 de febrero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, here, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
dir <- here::here()
inp <- "datos_limpios/"
out <- "datos_limpios/"

setwd(dir)


# 1. Cargar datos --------------------------------------------------------------

load(paste0(inp, "df_asuntos_ingresados_nivel_acusado.Rdata"))
load(paste0(inp, "df_personas_agredidas_nivel_expediente.Rdata"))
load(paste0(inp, "df_situacion_juridica_nivel_acusado.Rdata"))
load(paste0(inp, "df_soluciones_alternas_nivel_acusado.Rdata"))
load(paste0(inp, "df_medidas_cautelares_nivel_acusado.Rdata"))
load(paste0(inp, "df_sentencias_nivel_acusado.Rdata"))


# 2. Unir bases ----------------------------------------------------------------
# 2.1 Juntar asuntos ingresados y personas agredidas ---------------------------
df_unida1 <- df_asuntos_ingresados_nivel_acusado %>% 
        left_join(df_personas_agredidas_nivel_expediente, by = c("id_exp"))

# Revisar frecuencia de expedientes
df_freq_exp <- as.data.frame(table(df_unida1$id_exp))
#table(df_freq_exp$Freq)

# Revisar frecuencia de personas acusadas
df_freq_per <- as.data.frame(table(df_unida1$id_per_acusada)) 
table(df_freq_per$Freq)



# 2.2 Añadir situación jurídica ------------------------------------------------
df_unida2 <- df_unida1 %>% 
        full_join(df_situacion_juridica_nivel_acusado, 
                by = c("id_exp", "id_per_acusada", "materia", "num_alcaldias", 
                        "num_consignacion", "num_comision", "num_realizacion", 
                        "homicidio", "secuestro", "sexuales", "salud", "robo", 
                        "familiar", "lesiones", "extorsion", "objetos", 
                        "encubrimiento", "otros", "num_delitos"))

# Revisar frecuencia de expedientes
df_freq_exp <- as.data.frame(table(df_unida2$id_exp))
#table(df_freq_exp$Freq)

# Revisar frecuencia de personas acusadas
df_freq_per <- as.data.frame(table(df_unida2$id_per_acusada)) 
table(df_freq_per$Freq)

                # Folio de persona que es utilizado en más de una: 10806638

# 2.3 Añadir soluciones alternas -----------------------------------------------
df_unida3 <- df_unida2 %>% 
        full_join(df_soluciones_alternas_nivel_acusado, 
                by = c("id_exp", "id_per_acusada", "edad_indiciada", 
                        "sexo_indiciada", "materia", "num_alcaldias", 
                        "num_consignacion", "num_comision", "num_realizacion", 
                        "homicidio", "secuestro", "sexuales", "salud", "robo", 
                        "familiar", "lesiones", "extorsion", "objetos", 
                        "encubrimiento", "otros", "num_delitos"))

# Revisar frecuencia de expedientes
df_freq_exp <- as.data.frame(table(df_unida3$id_exp))
#table(df_freq_exp$Freq)

# Revisar frecuencia de personas acusadas
df_freq_per <- as.data.frame(table(df_unida3$id_per_acusada)) 
table(df_freq_per$Freq)

                # Folio de persona que es utilizado en más de una: 29426034


# 2.4 Añadir medidas cautelares ------------------------------------------------
df_unida4 <- df_unida3 %>% 
        full_join(df_medidas_cautelares_nivel_acusado, 
                by = c("id_exp", "id_per_acusada", "materia", "num_alcaldias", 
                        "num_consignacion", "num_comision", "num_realizacion", 
                        "homicidio", "secuestro", "sexuales", "salud", "robo", 
                        "familiar", "lesiones", "extorsion", "objetos", 
                        "encubrimiento", "otros", "num_delitos"))

# Revisar frecuencia de expedientes
df_freq_exp <- as.data.frame(table(df_unida4$id_exp))
#table(df_freq_exp$Freq)

# Revisar frecuencia de personas acusadas
df_freq_per <- as.data.frame(table(df_unida4$id_per_acusada)) 
table(df_freq_per$Freq)

# 2.5 Añadir sentencias --------------------------------------------------------
df_unida5 <- df_unida4 %>% 
        full_join(df_sentencias_nivel_acusado, 
                by = c("id_exp", "id_per_acusada", "materia", "num_alcaldias", 
                        "num_consignacion", "num_comision", "num_realizacion", 
                        "homicidio", "secuestro", "sexuales", "salud", "robo", 
                        "familiar", "lesiones", "extorsion", "objetos", 
                        "encubrimiento", "otros", "num_delitos")) 


# Revisar frecuencia de expedientes
df_freq_exp <- as.data.frame(table(df_unida5$id_exp))
#table(df_freq_exp$Freq)

# Revisar frecuencia de personas acusadas
df_freq_per <- as.data.frame(table(df_unida5$id_per_acusada)) 
table(df_freq_per$Freq)



# 5. Añadir variables de análisis ----------------------------------------------
df_unida_extra <- df_unida5 %>% 
        # Convertir NAs a 0 en variables binarias 
        replace_na(list(base_asuntos = 0, 
                        base_sitjurid = 0, 
                        base_sol_alternas = 0, 
                        base_medida_cautelar = 0, 
                        base_sentencias = 0)) %>% 
        # Crear variable de registro en todas las bases 
        mutate(aparece_en_bases = base_asuntos      + base_sitjurid + 
                                  base_sol_alternas + base_medida_cautelar + 
                                  base_sentencias) %>% 
        # Convertir en binarias las variables de conteo
        #mutate() %>% 
        # Crear variable binaria de terminación 
        mutate(terminacion = base_sentencias +
                        a_reparatorio + a_perdon + a_suspension + a_criterio +
                        a_sobreseimiento + s_libertad_falta_elementos +
                        s_no_vinculado) 
        # Crear variable rápida de tipo de terminación 
        #mutate(tipo_terminacion)



        
        

# 4. Seleccionar variables finales ---------------------------------------------

v_delitos <- c("homicidio", "secuestro", "sexuales", "salud", "robo", "familiar", 
                "lesiones", "extorsion", "objetos", "encubrimiento", "otros")

df_unida <- df_unida_extra %>% 
        select(materia,  # Materia penal 
                starts_with("id_"), # Identificadores de expediente y personas
                "aparece_en_bases", 
                starts_with("base_"), # Bases en las que se encuentra
                starts_with("num_"), # Número de variables agregadas
                # Temporalidad
                starts_with("year_"), 
                starts_with("month_"), 
                # Demográficos 
                starts_with("sexo_"), 
                starts_with("edad_"),
                starts_with("v_"),
                starts_with("r_"), 
                # Delitos
                all_of(v_delitos), 
                starts_with("s_"), # Situación jurídica
                starts_with("a_"), # Soluciones alternas
                starts_with("m_"), # Medidas cautelares
                starts_with("sentencia_")) # Sentencias 




# 4. Controles de revisión -----------------------------------------------------
# Folios que se repiten 
# Ver frecuencia de expedientes y personas 
df_freq_exp <- table(df_unida$id_exp)
df_freq_per <- as.data.frame(table(df_unida$id_per_acusada))
table(df_freq_per$Freq) # Frecuencia de folios únicos por persona


# Número total de observaciones por base
dim(df_asuntos_ingresados_nivel_acusado)
dim(df_personas_agredidas_nivel_expediente)
dim(df_situacion_juridica_nivel_acusado)
dim(df_soluciones_alternas_nivel_acusado)
dim(df_medidas_cautelares_nivel_acusado)
dim(df_sentencias_nivel_acusado)
dim(df_unida5)
dim(df_unida)


# 5. Guardar base final --------------------------------------------------------
# Renombrar con base definitiva 
df_TJCDMX <- df_unida 

# Guardar formato .RData
save(df_TJCDMX, file = paste0(out, "df_TJCDMX.RData"))

# Guardar formato csv
write.csv(df_TJCDMX, file = paste0(out, "df_TJCDMX.csv"))



beepr::beep(5)

# Fin del código #
