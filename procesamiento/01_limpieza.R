#------------------------------------------------------------------------------#
# Objetivo:                     Procesar datos de la PJCDMX
# Encargada:                    Regina Isabel Medina Rosales
# Correo:                       rmedina@intersecta.org
# Fecha de creación:            14 de enero de 2021
# Última actualización:         14 de enero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
library(readxl)         # Para lectura de archivos xlsx
library(dplyr)          # Para limpieza de datos 

# Limpiar espacio de trabajo 
rm(list=ls())


# 1. Cargar datos --------------------------------------------------------------

# Sobre delitos cometidos en la Ciudad de México entre mayo-2011 y sep-2020
# Información de los asuntos ingresados por los diversos delitos
df_asuntos_crudo        <- read_excel("datos_crudos/F227220_081220 INFOMEX.xlsx", 
                               skip = 8, sheet = "ingresados")

# Información de las personas involucradas como víctimas u ofendidas 
df_personas_crudo       <- read_excel("datos_crudos/F227220_081220 INFOMEX.xlsx", 
                               skip = 8, sheet = "victOfend")

# Información de los asuntos con resolución del auto de plazo constitucional
df_sitjurid_crudo       <- read_excel("datos_crudos/F227220_081220 INFOMEX.xlsx", 
                               skip = 8, sheet = "situaJurid")  

# Información de soluciones alternas o terminaciones anticipadas
df_alternas_crudo       <- read_excel("datos_crudos/F227220_081220 INFOMEX.xlsx", 
                               skip = 8, sheet = "solAlternasTermAnticip")

# Información de la medida cautelar de prisión preventiva
df_cautelares_crudo     <- read_excel("datos_crudos/F227220_081220 INFOMEX.xlsx", 
                               skip = 8, sheet = "medCautelares")

# Información de las sentencias emitidas en primera instancia 
df_sentencias_crudo     <- read_excel("datos_crudos/F227220_081220 INFOMEX.xlsx", 
                               skip = 7, sheet = "sentencias")

        
# 2. Limpiar datos -------------------------------------------------------------
# Renombrar variables 
df_asuntos <- df_asuntos_crudo          %>% 
        rename(materia         = "Materia", 
                id_expediente  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_persona     = "Indice para agrupación 2\r\n\r\n(Personas)", 
                año_ingreso    = "Año ingreso", 
                mes_ingreso    = "Mes ingreso", 
                sexo_indiciada = "Sexo de la persona indiciada", 
                edad_indiciada = "Edad de la persona indiciada",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignacion", 
                comisión       = "Comisión", 
                realización    = "Realización", 
                alcaldía       = "Alcaldía de ocurrencia")

df_personas <- df_personas_crudo        %>% 
        rename(id_expediente   = "Indice para agrupación 1\r\n(Expediente / Carpeta)", 
                id_persona     = "Indice para agrupación 3\r\n(Persona)", 
                sexo_victima   = "Sexo de la persona involucrada como víctima u ofendida", 
                edad_victima   = "Edad de la persona involucrada como víctima u ofendida", 
                relación       = "Relación entre la persona involucrada como víctima u ofendida y la persona probable responsable de la comisión del delito")        


df_sitjurid <- df_sitjurid_crudo        %>%
        rename(materia         = "Materia", 
                id_expediente  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_persona     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                año_resolucion = "Año resolución",  
                mes_resolucion = "Mes resolución", 
                sexo_procesada = "Sexo de la persona procesada", 
                edad_procesada = "Edad de la persona procesada",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignacion", 
                comisión       = "Comisión", 
                realización    = "Realización", 
                alcaldía       = "Alcaldía de ocurrencia", 
                resolución     = "Resolución a la situación jurídica") 

df_alternas <- df_alternas_crudo          %>% 
        rename(materia         = "Materia", 
                id_expediente  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_persona     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                año_audiencia  = "Año audiencia", 
                mes_audiencia  = "Mes audiencia", 
                sexo_indiciada = "Sexo de la persona indiciada", 
                edad_indiciada = "Edad de la persona indiciada",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignacion", 
                comisión       = "Comisión", 
                realización    = "Realización", 
                alcaldía       = "Alcaldía de ocurrencia", 
                tipo_solución  = "Tipo de solución alterna o terminación anticipada")

df_cautelares <- df_cautelares_crudo %>% 
        rename(materia         = "Materia", 
                id_expediente  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_persona     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                año_audiancia  = "Año audiencia",  
                mes_audiancia  = "Mes audiencia", 
                sexo_vinculada = "Sexo de la persona vinculada", 
                edad_vinculada = "Edad de la persona vinculada",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignacion", 
                comisión       = "Comisión", 
                realización    = "Realización", 
                alcaldía       = "Alcaldía de ocurrencia", 
                tipo_medida    = "Tipo de medida cautelar") 

df_sentencias <- df_sentencias_crudo %>% 
        rename(materia         = "Materia", 
                id_expediente  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_persona     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                año_sentencia  = "Año sentencia",  
                mes_sentencia  = "Mes sentencia", 
                sexo_sentenciada = "Sexo", 
                edad_sentenciada = "Edad",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignación", 
                comisión       = "Comisión", 
                realización    = "Realización", 
                alcaldía       = "Alcaldía de ocurrencia del delito", 
                forma_proceso  = "Forma del proceso", 
                tipo_sentencia = "Tipo de sentencia", 
                años_sentencia = "Años de sentencia", 
                meses_sentencia = "Meses de sentencia", 
                días_sentencia = "Días de sentencia") 
        


# Fin del código #

# Preguntas: 
# 1. ¿A qué se refiere persona indiciada? 
# 2. ¿Son excluyentes los casos de medidas cautelares y de soluciones alternas
# 3. ¿El identificador de persona es para quien denuncia? 
# 4. En el registro de sentencia, ¿el sexo se refiere a la persona sentenciada?
        
