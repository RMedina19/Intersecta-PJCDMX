#------------------------------------------------------------------------------#
# Proyecto:                   TRIBUNAL SUPERIOR DE JUSTICIA DE CIUDAD DE MÉXICO
# Objetivo:                   Unir las bases de datos de la PJCDMX
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          27 de enero de 2021
# Última actualización:       10 de febrero de 2021
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


# 2. Procesamiento de las bases previo a unión ---------------------------------
# 2.1 Renombrar variables para sean únicas por base ----------------------------
# Asuntos 
df_asuntos <- df_asuntos_ingresados_nivel_acusado %>% 
        rename(year_asunto                  = year_ingreso, 
                month_asunto                = month_ingreso, 
                date_asunto                 = date_ingreso,
                edad_acusada_asunto         = edad_indiciada, 
                sexo_acusada_asunto         = sexo_indiciada, 
                materia_asunto              = materia, 
                num_delitos_asuntos         = num_delitos, 
                num_alcaldias_asunto        = num_alcaldias, 
                num_consignacion_asunto     = num_consignacion, 
                num_comision_asunto         = num_comision, 
                num_realizacion_asunto      = num_realizacion, 
                c_con_detenido_asunto       = c_con_detenido, 
                c_sin_detenido_asunto       = c_sin_detenido, 
                c_culposo_asunto            = c_culposo, 
                c_doloso_asunto             = c_doloso, 
                tr_consumado_asunto         = tr_consumado, 
                tr_tentativa_asunto         = tr_tentativa)

# Personas agredidas 
df_agredidas <- df_personas_agredidas_nivel_expediente
        
# Situación jurídica 
df_sitjurid <- df_situacion_juridica_nivel_acusado %>% 
        rename(year_sitjurid                = year_resolucion, 
                month_sitjurid              = month_resolucion, 
                date_sijurid                = date_resolucion, 
                edad_acusada_sitjurid       = edad_procesada, 
                sexo_acusada_sitjurid       = sexo_procesada, 
                materia_sitjurid            = materia, 
                num_delitos_sitjurid        = num_delitos, 
                num_alcaldias_sitjurid      = num_alcaldias, 
                num_consignacion_sitjurid   = num_consignacion, 
                num_comision_sitjurid       = num_comision, 
                num_realizacion_sitjurid    = num_realizacion, 
                c_con_detenido_sitjurid     = c_con_detenido, 
                c_sin_detenido_sitjurid     = c_sin_detenido, 
                c_culposo_sitjurid          = c_culposo, 
                c_doloso_sitjurid           = c_doloso, 
                tr_consumado_sitjurid       = tr_consumado, 
                tr_tentativa_sitjurid       = tr_tentativa)


# Soluciones alternas
df_alternas <- df_soluciones_alternas_nivel_acusado %>% 
        rename(year_alternas                = year_audiencia_alt, 
                month_alternas              = month_audiencia_alt, 
                date_alternas               = date_audiencia_alt, 
                edad_acusada_alternas       = edad_indiciada, 
                sexo_acusada_alternas       = sexo_indiciada, 
                materia_alternas            = materia, 
                num_delitos_alternas        = num_delitos, 
                num_alcaldias_alternas      = num_alcaldias, 
                num_consignacion_alternas   = num_consignacion, 
                num_comision_alternas       = num_comision, 
                num_realizacion_alternas    = num_realizacion, 
                c_con_detenido_alternas     = c_con_detenido, 
                c_sin_detenido_alternas     = c_sin_detenido, 
                c_culposo_alternas          = c_culposo, 
                c_doloso_alternas           = c_doloso, 
                tr_consumado_alternas       = tr_consumado, 
                tr_tentativa_alternas       = tr_tentativa)

# Medidas cautelares
df_cautelares <- df_medidas_cautelares_nivel_acusado %>% 
        rename(year_cautelares              = year_audiencia_caut, 
                month_cautelares            = month_audiencia_caut, 
                date_cautelares             = date_audiencia_caut, 
                edad_acusada_cautelares     = edad_vinculada, 
                sexo_acusada_cautelares     = sexo_vinculada, 
                materia_cautelares          = materia, 
                num_delitos_cautelares      = num_delitos, 
                num_alcaldias_cautelares    = num_alcaldias, 
                num_consignacion_cautelares = num_consignacion, 
                num_comision_cautelares     = num_comision, 
                num_realizacion_cautelares  = num_realizacion, 
                c_con_detenido_cautelares   = c_con_detenido, 
                c_sin_detenido_cautelares   = c_sin_detenido, 
                c_culposo_cautelares        = c_culposo, 
                c_doloso_cautelares         = c_doloso, 
                tr_consumado_cautelares     = tr_consumado, 
                tr_tentativa_cautelares     = tr_tentativa)
        
# Sentencias
df_sentencias <- df_sentencias_nivel_acusado %>% 
        rename(materia_sentencia            = materia, 
                num_delitos_sentencia       = num_delitos, 
                num_alcaldias_sentencia     = num_alcaldias, 
                num_consignacion_sentencia  = num_consignacion, 
                num_comision_sentencia      = num_comision, 
                num_realizacion_sentencia   = num_realizacion, 
                c_con_detenido_sentencia    = c_con_detenido, 
                c_sin_detenido_sentencia    = c_sin_detenido, 
                c_culposo_sentencia         = c_culposo, 
                c_doloso_sentencia          = c_doloso, 
                tr_consumado_sentencia      = tr_consumado, 
                tr_tentativa_sentencia      = tr_tentativa) %>% 
        mutate(sexo_sentenciada = case_when(sexo_sentenciada == "Femenino" ~ "Mujeres",
                sexo_sentenciada == "Masculino" ~ "Hombres",
                sexo_sentenciada ==  sexo_sentenciada ~ sexo_sentenciada))



# 3. Unir bases ----------------------------------------------------------------
# Juntar asuntos ingresados y personas agredidas 
df_unida1 <- df_asuntos %>% 
        left_join(df_agredidas, by = c("id_exp"))

# Añadir situación jurídica 
df_unida2 <- df_unida1 %>% 
        full_join(df_sitjurid, by = c("id_exp", "id_per_acusada"))

# Añadir soluciones alternas 
df_unida3 <- df_unida2 %>% 
        full_join(df_alternas, by = c("id_exp", "id_per_acusada"))

# Añadir medidas cautelares
df_unida4 <- df_unida3 %>% 
        full_join(df_cautelares, by = c("id_exp", "id_per_acusada"))

# Añadir sentencias 
df_unida5 <- df_unida4 %>% 
        full_join(df_sentencias, by = c("id_exp", "id_per_acusada")) 



# Revisar frecuencia de expedientes
# Asuntos ingresados y personas
df_freq_exp <- as.data.frame(table(df_unida1$id_exp))
df_freq_per <- as.data.frame(table(df_unida1$id_per_acusada)) 
        table(df_freq_per$Freq)


# Asuntos ingresados, personas y situación jurídica 
df_freq_exp <- as.data.frame(table(df_unida2$id_exp))
df_freq_per <- as.data.frame(table(df_unida2$id_per_acusada)) 
        table(df_freq_per$Freq) # Folio de persona que es utilizado en más de una: 10806638

# Asuntos ingresados, personas, situación jurídica y soluciones alternas
df_freq_exp <- as.data.frame(table(df_unida3$id_exp))
df_freq_per <- as.data.frame(table(df_unida3$id_per_acusada)) 
        table(df_freq_per$Freq) # Folio de persona que es utilizado en más de una: 29426034


# Asuntos ingresados, personas, sit. jurid., sol. alternas y medidas cautelares
df_freq_exp <- as.data.frame(table(df_unida4$id_exp))
df_freq_per <- as.data.frame(table(df_unida4$id_per_acusada)) 
        table(df_freq_per$Freq)


# Asuntos ingresados, personas, sit. jurid., sol. alternas, cautelares y sentencias
df_freq_exp <- as.data.frame(table(df_unida5$id_exp))
df_freq_per <- as.data.frame(table(df_unida5$id_per_acusada)) 
        table(df_freq_per$Freq)


# 4. Procesamiento posterior a la unión de las bases ---------------------------

# 4.2 Construcción de variables de análisis ------------------------------------
df_unida_extra <- df_unida5 %>% 
        # Convertir NAs a 0 en variables binarias 
        replace_na(list(base_asuntos         = 0, 
                        base_sitjurid        = 0, 
                        base_sol_alternas    = 0, 
                        base_medida_cautelar = 0, 
                        base_sentencias      = 0))      %>% 
        # Crear variable de registro en todas las bases 
        mutate(aparece_en_bases = base_asuntos      + base_sitjurid + 
                                  base_sol_alternas + base_medida_cautelar + 
                                  base_sentencias) %>% 
        # Crear variable que indique el número de terminaciones 
        mutate(num_terminacion = rowSums(cbind(base_sentencias, a_perdon, 
                                                a_suspension, a_criterio, 
                                                a_sobreseimiento, 
                                                s_libertad_falta_elementos, 
                                                s_no_vinculado), na.rm = T)) %>% 
        # Variable binaria para terminación 
        mutate(con_terminacion = if_else(num_terminacion == 0, 0, 1))        %>% 
        # Variable que indique el tipo de terminación 
        mutate(tipo_terminacion = case_when(num_terminacion   == 0 ~ "Sin terminación", 
                                   a_perdon                   == 1 ~ "Solución alterna: Perdón", 
                                   a_suspension               == 1 ~ "Solución alterna: Suspensión condicional del proceso", 
                                   a_criterio                 == 1 ~ "Solución alterna: Criterio de oportunidad", 
                                   a_sobreseimiento           == 1 ~ "Solución alterna: Sobreseimiento", 
                                   s_libertad_falta_elementos == 1 ~ "Libertad por falta de elementos", 
                                   s_no_vinculado             == 1 ~ "Sin vinculación a proceso", 
                                   sentencia_absolutoria      == 1 ~ "Sentencia absolutoria",  
                                   sentencia_condenatoria     == 1 ~ "Sentencia condenatoria"))  %>% 
        # Variable para indicar prisión preventiva 
        mutate(num_ppo   = rowSums(cbind(s_formal_prision, m_prision_preventiva), na.rm = T),
               con_ppo  = if_else(num_ppo == 0, 0, 1), 
               tipo_ppo = case_when(con_ppo              == 0 ~ "Sin prisión preventiva", 
                                    s_formal_prision     == 1 ~ "PPO como situación jurídica", 
                                    m_prision_preventiva == 1 ~ "PPO como medida cautelar")) 


        
# 4.2 Seleccionar variables finales para base completa -------------------------

df_unida_completa <- df_unida_extra %>% 
        select(starts_with("id_"), # Identificadores de expediente y personas
                "aparece_en_bases", 
                starts_with("base_"), # Bases en las que se encuentra
                starts_with("num_alcaldias"),
                starts_with("num_consigna"),
                starts_with("num_comision"),
                starts_with("num_realiza"),
                starts_with("num_delitos"),
                # Temporalidad
                starts_with("year_"), 
                starts_with("month_"), 
                starts_with("date_"),
                # Demográficos 
                starts_with("sexo_"), 
                starts_with("edad_"),
                # Situación y prisión preventiva
                ends_with("_terminacion"), 
                ends_with("_ppo"),
                # Características jurídicas 
                starts_with("materia_"), 
                starts_with("c_con"),
                starts_with("c_sin"),
                starts_with("c_cul"),
                starts_with("c_dol"),
                starts_with("tr_"),
                # Número de víctimas y tipo de relación
                starts_with("v_"),
                starts_with("r_"), 
                # Resto de las variables
                starts_with("d_"), # Delitos
                starts_with("s_"), # Situación jurídica
                starts_with("a_"), # Soluciones alternas
                "num_medidas_cautelares",
                starts_with("m_"), # Medidas cautelares
                starts_with("sentencia_"), 
                "years_sentencia", "months_sentencia") # Sentencias 

# Verificar que no se haya perdido ninguna variable ni observación 
dim(df_unida_extra)
dim(df_unida_completa)

# 4.3 Seleccionar variables finales para base sintética ------------------------
# Indicador único por sexo y edad de la persona acusada, así como materia, 
# consignación, comisión, realización y número de alcaldías. 


df_completa <- df_unida_completa %>% 
        mutate(materia = case_when(is.na(materia_asunto)    == F ~ materia_asunto, 
                        is.na(materia_sitjurid)             == F ~ materia_sitjurid,
                        is.na(materia_alternas)             == F ~ materia_alternas,
                        is.na(materia_cautelares)           == F ~ materia_cautelares,
                        is.na(materia_sentencia)            == F ~ materia_sentencia), 
                num_alcaldias = case_when(is.na(num_alcaldias_asunto) == F ~ num_alcaldias_asunto, 
                        is.na(num_alcaldias_sitjurid)       == F ~ num_alcaldias_sitjurid, 
                        is.na(num_alcaldias_alternas)       == F ~ num_alcaldias_alternas, 
                        is.na(num_alcaldias_cautelares)     == F ~ num_alcaldias_cautelares, 
                        is.na(num_alcaldias_sentencia)      == F ~ num_alcaldias_sentencia), 
                num_consignacion = case_when(is.na(num_consignacion_asunto) == F ~ num_consignacion_asunto, 
                        is.na(num_consignacion_sitjurid)    == F ~ num_consignacion_sitjurid, 
                        is.na(num_consignacion_alternas)    == F ~ num_consignacion_alternas, 
                        is.na(num_consignacion_cautelares)  == F ~ num_consignacion_cautelares, 
                        is.na(num_consignacion_sentencia)   == F ~ num_consignacion_sentencia), 
                c_con_detenido = case_when(is.na(c_con_detenido_asunto) == F ~ c_con_detenido_asunto, 
                        is.na(c_con_detenido_sitjurid)      == F ~ c_con_detenido_sitjurid, 
                        is.na(c_con_detenido_alternas)      == F ~ c_con_detenido_alternas, 
                        is.na(c_con_detenido_cautelares)    == F ~ c_con_detenido_cautelares, 
                        is.na(c_con_detenido_sentencia)     == F ~ c_con_detenido_sentencia), 
                s_sin_detenido = case_when(is.na(c_sin_detenido_asunto) == F ~ c_sin_detenido_asunto, 
                        is.na(c_sin_detenido_sitjurid)      == F ~ c_sin_detenido_sitjurid, 
                        is.na(c_sin_detenido_alternas)      == F ~ c_sin_detenido_alternas, 
                        is.na(c_sin_detenido_cautelares)    == F ~ c_sin_detenido_cautelares, 
                        is.na(c_sin_detenido_sentencia)     == F ~ c_sin_detenido_sentencia), 
                num_comision = case_when(is.na(num_comision_asunto) == F ~ num_comision_asunto, 
                        is.na(num_comision_sitjurid)        == F ~ num_comision_sitjurid, 
                        is.na(num_comision_alternas)        == F ~ num_comision_alternas, 
                        is.na(num_comision_cautelares)      == F ~ num_comision_cautelares, 
                        is.na(num_comision_sentencia)       == F ~ num_comision_sentencia), 
                c_culposo = case_when(is.na(c_culposo_asunto) == F ~ c_culposo_asunto, 
                        is.na(c_culposo_sitjurid)           == F ~ c_culposo_sitjurid, 
                        is.na(c_culposo_alternas)           == F ~ c_culposo_alternas, 
                        is.na(c_culposo_cautelares)         == F ~ c_culposo_cautelares, 
                        is.na(c_culposo_sentencia)          == F ~ c_culposo_sentencia), 
                c_doloso = case_when(is.na(c_doloso_asunto) == F ~ c_doloso_asunto, 
                        is.na(c_doloso_sitjurid)            == F ~ c_doloso_sitjurid, 
                        is.na(c_doloso_alternas)            == F ~ c_doloso_alternas, 
                        is.na(c_doloso_cautelares)          == F ~ c_doloso_cautelares, 
                        is.na(c_doloso_sentencia)           == F ~ c_doloso_sentencia), 
                num_realizacion = case_when(is.na(num_realizacion_asunto) == F ~ num_realizacion_asunto, 
                        is.na(num_realizacion_sitjurid)     == F ~ num_realizacion_sitjurid, 
                        is.na(num_realizacion_alternas)     == F ~ num_realizacion_alternas, 
                        is.na(num_realizacion_cautelares)   == F ~ num_realizacion_cautelares, 
                        is.na(num_realizacion_sentencia)    == F ~ num_realizacion_sentencia), 
                tr_consumado = case_when(is.na(tr_consumado_asunto) == F ~ tr_consumado_asunto, 
                        is.na(tr_consumado_sitjurid)        == F ~ tr_consumado_sitjurid, 
                        is.na(tr_consumado_alternas)        == F ~ tr_consumado_alternas, 
                        is.na(tr_consumado_cautelares)      == F ~ tr_consumado_cautelares, 
                        is.na(tr_consumado_sentencia)       == F ~ tr_consumado_sentencia), 
                tr_tentativa = case_when(is.na(tr_tentativa_asunto) == F ~ tr_tentativa_asunto, 
                        is.na(tr_tentativa_sitjurid)        == F ~ tr_tentativa_sitjurid, 
                        is.na(tr_tentativa_alternas)        == F ~ tr_tentativa_alternas, 
                        is.na(tr_tentativa_cautelares)      == F ~ tr_tentativa_cautelares, 
                        is.na(tr_tentativa_sentencia)       == F ~ tr_tentativa_sentencia), 
                sexo = case_when(is.na(sexo_acusada_asunto) == F ~ sexo_acusada_asunto, 
                        is.na(sexo_acusada_sitjurid)        == F ~ sexo_acusada_sitjurid, 
                        is.na(sexo_acusada_alternas)        == F ~ sexo_acusada_alternas, 
                        is.na(sexo_acusada_cautelares)      == F ~ sexo_acusada_cautelares, 
                        is.na(sexo_sentenciada)             == F ~ sexo_sentenciada), 
                edad = case_when(is.na(edad_acusada_asunto) == F ~ edad_acusada_asunto, 
                        is.na(edad_acusada_sitjurid)        == F ~ edad_acusada_sitjurid, 
                        is.na(edad_acusada_alternas)        == F ~ edad_acusada_alternas, 
                        is.na(edad_acusada_cautelares)      == F ~ edad_acusada_cautelares, 
                        is.na(edad_sentenciada)             == F ~ edad_sentenciada), 
                num_delitos = case_when(is.na(num_delitos_asuntos) == F ~ num_delitos_asuntos, 
                        is.na(num_delitos_sitjurid)         == F ~ num_delitos_sitjurid, 
                        is.na(num_delitos_alternas)         == F ~ num_delitos_alternas, 
                        is.na(num_delitos_cautelares)       == F ~ num_delitos_cautelares, 
                        is.na(num_delitos_sentencia)        == F ~ num_delitos_sentencia), 
                d_homicidio = case_when(is.na(d_homicidio_asunto) == F ~ d_homicidio_asunto, 
                        is.na(d_homicidio_sitjurid)         == F ~ d_homicidio_sitjurid, 
                        is.na(d_homicidio_alternas)         == F ~ d_homicidio_alternas, 
                        is.na(d_homicidio_cautelares)       == F ~ d_homicidio_cautelares, 
                        is.na(d_homicidio_sentencia)        == F ~ d_homicidio_sentencia), 
                d_secuestro = case_when(is.na(d_secuestro_asunto) == F ~ d_secuestro_asunto, 
                        is.na(d_secuestro_sitjurid)         == F ~ d_secuestro_sitjurid, 
                        is.na(d_secuestro_alternas)         == F ~ d_secuestro_alternas, 
                        is.na(d_secuestro_cautelares)       == F ~ d_secuestro_cautelares, 
                        is.na(d_secuestro_sentencia)        == F ~ d_secuestro_sentencia), 
                d_sexuales = case_when(is.na(d_sexuales_asunto) == F ~ d_sexuales_asunto, 
                        is.na(d_sexuales_sitjurid)          == F ~ d_sexuales_sitjurid, 
                        is.na(d_sexuales_alternas)          == F ~ d_sexuales_alternas, 
                        is.na(d_sexuales_cautelares)        == F ~ d_sexuales_cautelares, 
                        is.na(d_sexuales_sentencia)         == F ~ d_sexuales_sentencia), 
                d_salud = case_when(is.na(d_salud_asunto)   == F ~ d_salud_asunto, 
                        is.na(d_salud_sitjurid)             == F ~ d_salud_sitjurid, 
                        is.na(d_salud_alternas)             == F ~ d_salud_alternas, 
                        is.na(d_salud_cautelares)           == F ~ d_salud_cautelares, 
                        is.na(d_salud_sentencia)            == F ~ d_salud_sentencia), 
                d_robo = case_when(is.na(d_robo_asunto)     == F ~ d_robo_asunto, 
                        is.na(d_robo_sitjurid)              == F ~ d_robo_sitjurid, 
                        is.na(d_robo_alternas)              == F ~ d_robo_alternas, 
                        is.na(d_robo_cautelares)            == F ~ d_robo_cautelares, 
                        is.na(d_robo_sentencia)             == F ~ d_robo_sentencia), 
                d_familiar = case_when(is.na(d_familiar_asunto) == F ~ d_familiar_asunto, 
                        is.na(d_familiar_sitjurid)          == F ~ d_familiar_sitjurid, 
                        is.na(d_familiar_alternas)          == F ~ d_familiar_alternas, 
                        is.na(d_familiar_cautelares)        == F ~ d_familiar_cautelares, 
                        is.na(d_familiar_sentencia)         == F ~ d_familiar_sentencia), 
                d_lesiones = case_when(is.na(d_lesiones_asunto) == F ~ d_lesiones_asunto, 
                        is.na(d_lesiones_sitjurid)          == F ~ d_lesiones_sitjurid, 
                        is.na(d_lesiones_alternas)          == F ~ d_lesiones_alternas, 
                        is.na(d_lesiones_cautelares)        == F ~ d_lesiones_cautelares, 
                        is.na(d_lesiones_sentencia)         == F ~ d_lesiones_sentencia), 
                d_extorsion = case_when(is.na(d_extorsion_asunto) == F ~ d_extorsion_asunto, 
                        is.na(d_extorsion_sitjurid)         == F ~ d_extorsion_sitjurid, 
                        is.na(d_extorsion_alternas)         == F ~ d_extorsion_alternas, 
                        is.na(d_extorsion_cautelares)       == F ~ d_extorsion_cautelares, 
                        is.na(d_extorsion_sentencia)        == F ~ d_extorsion_sentencia), 
                d_objetos = case_when(is.na(d_objetos_asunto) == F ~ d_objetos_asunto, 
                        is.na(d_objetos_sitjurid)           == F ~ d_objetos_sitjurid, 
                        is.na(d_objetos_alternas)           == F ~ d_objetos_alternas, 
                        is.na(d_objetos_cautelares)         == F ~ d_objetos_cautelares, 
                        is.na(d_objetos_sentencia)          == F ~ d_objetos_sentencia), 
                d_encubrimiento = case_when(is.na(d_encubrimiento_asunto) == F ~ d_encubrimiento_asunto, 
                        is.na(d_encubrimiento_sitjurid)     == F ~ d_encubrimiento_sitjurid, 
                        is.na(d_encubrimiento_alternas)     == F ~ d_encubrimiento_alternas, 
                        is.na(d_encubrimiento_cautelares)   == F ~ d_encubrimiento_cautelares, 
                        is.na(d_encubrimiento_sentencia)    == F ~ d_encubrimiento_sentencia), 
                d_otros = case_when(is.na(d_otros_asunto)   == F ~ d_otros_asunto, 
                        is.na(d_otros_sitjurid)             == F ~ d_otros_sitjurid, 
                        is.na(d_otros_alternas)             == F ~ d_otros_alternas, 
                        is.na(d_otros_cautelares)           == F ~ d_otros_cautelares, 
                        is.na(d_otros_sentencia)            == F ~ d_otros_sentencia)) %>% 
        mutate(consignacion = case_when(c_con_detenido        == 1 ~ "Con detenido", 
                                        c_con_detenido        == 0 ~ "Sin detenido"), 
                comision    = case_when(c_doloso              == 1 ~ "Doloso", 
                                        c_doloso              == 0 ~ "Culposo"), 
                realizacion = case_when(tr_consumado          == 1 ~ "Consumado", 
                                        tr_consumado          == 0 ~ "Tentativa"), 
                sentencia   = case_when(sentencia_absolutoria == 1 ~ "Absolutoria", 
                                        sentencia_absolutoria == 0 ~ "Condenatoria"))  %>% 
        # Delitos cortos 
        mutate(delitos_cortos = case_when(d_homicidio == 1 ~ "Homicidio",
                d_salud         == 1 ~ "Delitos contra la salud",
                d_robo          == 1 ~ "Robo",
                d_familiar      == 1 ~ "Violencia familiar",
                d_lesiones      == 1 ~ "Lesiones",
                d_encubrimiento == 1 ~ "Encubrimiento",
                d_extorsion     == 1 ~ "Extorsión",
                d_objetos       == 1 ~ "Objetos aptos para agredir",
                d_secuestro     == 1 ~ "Secuestro",
                d_sexuales      == 1 ~ "Delitos sexuales",
                d_otros         == 1 ~ "Otros delitos"))
    

v_delitos <- c("d_homicidio", "d_secuestro", "d_sexuales", "d_salud", "d_robo", 
                "d_familiar", "d_lesiones", "d_extorsion", "d_objetos", 
                "d_encubrimiento", "d_otros")

v_juridico <- c("materia", "consignacion", "comision", "realizacion")


df_selecta <- df_completa %>% 
        dplyr::select(starts_with("id_"), 
                # Registro en bases de datos
                aparece_en_bases, 
                starts_with("base_"), 
                # Temporalidad
                starts_with("year_"), 
                starts_with("month_"), 
                starts_with("date_"), 
                # Demográfico
                sexo, edad, 
                num_alcaldias,
                # Situación y prisión preventiva
                ends_with("_terminacion"), 
                ends_with("_ppo"),
                # Características jurídicos 
                all_of(v_juridico), 
                # Número de víctimas y tipo de relación 
                starts_with("v_"), 
                starts_with("r_"), 
                # Delitos
                num_delitos,
                "delitos_cortos",
                all_of(v_delitos),
                # Situación jurídica 
                starts_with("s_"), 
                # Soluciones alternas
                starts_with("a_"), 
                # Medidas cautelares
                "num_medidas_cautelares", 
                starts_with("m_"), 
                # Sentencia 
                "sentencia", "years_sentencia", "months_sentencia") %>% 
        select(-c(num_ppo))



# 5. Controles de revisión -----------------------------------------------------
# Folios que se repiten 
# Ver frecuencia de expedientes y personas 
df_freq_exp <- table(df_unida_completa$id_exp)
df_freq_per <- as.data.frame(table(df_unida_completa$id_per_acusada))
        table(df_freq_per$Freq) # Frecuencia de folios únicos por persona

# Número total de observaciones por base
dim(df_asuntos_ingresados_nivel_acusado)
dim(df_personas_agredidas_nivel_expediente)
dim(df_situacion_juridica_nivel_acusado)
dim(df_soluciones_alternas_nivel_acusado)
dim(df_medidas_cautelares_nivel_acusado)
dim(df_sentencias_nivel_acusado)
dim(df_unida5)
dim(df_unida_extra)
dim(df_completa)
dim(df_selecta)

# Asegurarse de que cada variable tenga los valores adecuados
for (i in 3:199){
        print(names(df_completa[i])) # Nombre de la variable
        print(table(df_completa[i])) # Tabla de valores 
}


for (i in 3:92){
        print(names(df_selecta[i])) # Nombre de la variable
        print(table(df_selecta[i])) # Tabla de valores 
}



# 6. Guardar base final --------------------------------------------------------
# Renombrar con base definitiva 
df_TJCDMX_completa      <- df_completa
df_TJCDMX_selecta       <- df_selecta

# Guardar formato .RData
save(df_TJCDMX_completa, file = paste0(out, "df_TJCDMX_completa.RData"))
save(df_TJCDMX_selecta,  file = paste0(out, "df_TJCDMX_selecta.RData"))

# Guardar formato csv
write.csv(df_TJCDMX_completa, file = paste0(out, "df_TJCDMX_completa.csv"))
write.csv(df_TJCDMX_selecta,  file = paste0(out, "df_TJCDMX_selecta.csv"))


beepr::beep(5)

# Fin del código #

