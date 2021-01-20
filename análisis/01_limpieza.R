#------------------------------------------------------------------------------#
# Objetivo:                     Procesar datos de la PJCDMX
# Encargada:                    Regina Isabel Medina Rosales
# Correo:                       rmedina@intersecta.org
# Fecha de creación:            14 de enero de 2021
# Última actualización:         20 de enero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, dplyr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp <- "datos_crudos/"
out <- "datos_limpios/"


# 1. Cargar datos --------------------------------------------------------------

# Sobre delitos cometidos en la Ciudad de México entre mayo-2011 y sep-2020
# Información de los asuntos ingresados por los diversos delitos
df_asuntos_crudo        <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                               skip = 8, sheet = "ingresados")

# Información de las personas involucradas como víctimas u ofendidas 
df_personas_crudo       <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                               skip = 8, sheet = "victOfend")

# Información de los asuntos con resolución del auto de plazo constitucional
df_sitjurid_crudo       <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                               skip = 8, sheet = "situaJurid")  

# Información de soluciones alternas o terminaciones anticipadas
df_alternas_crudo       <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                               skip = 8, sheet = "solAlternasTermAnticip")

# Información de la medida cautelar de prisión preventiva
df_cautelares_crudo     <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                               skip = 8, sheet = "medCautelares")

# Información de las sentencias emitidas en primera instancia 
df_sentencias_crudo     <- read_excel(paste0(inp, "F227220_081220 INFOMEX.xlsx"), 
                               skip = 7, sheet = "sentencias")

        
# 2. Limpiar datos -------------------------------------------------------------
# Renombrar variables 
df_asuntos <- df_asuntos_crudo          %>% 
        rename(materia           = "Materia", 
                id_exp           = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora  = "Indice para agrupación 2\r\n\r\n(Personas)", 
                year_ingreso     = "Año ingreso", 
                month_ingreso    = "Mes ingreso", 
                sexo_indiciada   = "Sexo de la persona indiciada", 
                edad_indiciada   = "Edad de la persona indiciada",
                tipo_delictivo   = "Tipo delictivo", 
                desag_estad      = "Desagregado estadístico", 
                consignación     = "Consignacion", 
                comision         = "Comisión", 
                realizacion      = "Realización", 
                alcaldia_ocurr   = "Alcaldía de ocurrencia")

df_personas <- df_personas_crudo        %>% 
        rename(id_exp           = "Indice para agrupación 1\r\n(Expediente / Carpeta)", 
                id_per_ofendida = "Indice para agrupación 3\r\n(Persona)", 
                sexo_victima    = "Sexo de la persona involucrada como víctima u ofendida", 
                edad_victima    = "Edad de la persona involucrada como víctima u ofendida", 
                relación        = "Relación entre la persona involucrada como víctima u ofendida y la persona probable responsable de la comisión del delito")


df_sitjurid <- df_sitjurid_crudo        %>%
        rename(materia           = "Materia", 
                id_exp           = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora  = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_resolucion  = "Año resolución",  
                month_resolucion = "Mes resolución", 
                sexo_procesada   = "Sexo de la persona procesada", 
                edad_procesada   = "Edad de la persona procesada",
                tipo_delictivo   = "Tipo delictivo", 
                desag_estad      = "Desagregado estadístico", 
                consignación     = "Consignacion", 
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
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignacion", 
                comision       = "Comisión", 
                realizacion    = "Realización", 
                alcaldia_ocurr = "Alcaldía de ocurrencia", 
                tipo_solución  = "Tipo de solución alterna o terminación anticipada")

df_cautelares <- df_cautelares_crudo %>% 
        rename(materia         = "Materia", 
                id_exp  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_audiancia  = "Año audiencia",  
                month_audiencia  = "Mes audiencia", 
                sexo_vinculada = "Sexo de la persona vinculada", 
                edad_vinculada = "Edad de la persona vinculada",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignacion", 
                comision       = "Comisión", 
                realizacion    = "Realización", 
                alcaldia_ocurr = "Alcaldía de ocurrencia", 
                tipo_medida    = "Tipo de medida cautelar") 

df_sentencias <- df_sentencias_crudo %>% 
        rename(materia         = "Materia", 
                id_exp  = "Indice para agrupación 1\r\n\r\n(Expediente / Carpeta)", 
                id_per_agresora     = "Indice para agrupación 2\r\n\r\n(Persona)", 
                year_sentencia  = "Año sentencia",  
                month_sentencia  = "Mes sentencia", 
                sexo_sentenciada = "Sexo", 
                edad_sentenciada = "Edad",
                tipo_delictivo = "Tipo delictivo", 
                desag_estad    = "Desagregado estadístico", 
                consignación   = "Consignación", 
                comision       = "Comisión", 
                realizacion    = "Realización", 
                alcaldia_ocurr = "Alcaldía de ocurrencia del delito", 
                forma_proceso  = "Forma del proceso", 
                tipo_sentencia = "Tipo de sentencia", 
                years_sentencia = "Años de sentencia", 
                months_sentencia = "Meses de sentencia", 
                días_sentencia = "Días de sentencia") 
        

# 3. Explorar datos -------------------------------------------------------------
# Identificar número de observaciones, expedientes únicos y personas únicas

dim(df_asuntos)[1]                              # 301,836 observaciones (asuntos)
length(unique(df_asuntos$id_exp))               # 219,447 expedientes únicos
length(unique(df_asuntos$id_per_agresora))      # 255,793 agresores únicos

dim(df_personas)[1]                             # 335,319 observaciones (víctimas)
length(unique(df_personas$id_exp))              # 231,186 expedientes únicos
length(unique(df_personas$id_per_ofendida))     # 252,738 víctimas únicas

dim(df_sitjurid)[1]                             # 203,053 observaciones (asuntos con resolución de auto plazo)
length(unique(df_sitjurid$id_exp))              # 152,686 expedientes únicos
length(unique(df_sitjurid$id_per_agresora))     # 177,683 agresores únicos

dim(df_alternas)[1]                             # 28,699 observaciones (soluciones alternas)
length(unique(df_alternas$id_exp))              # 21,987 expedientes únicos
length(unique(df_alternas$id_per_agresora))     # 25,735 agresores únicos

dim(df_cautelares)[1]                           # 176,812 observaciones (medidas cautelares)
length(unique(df_cautelares$id_exp))            # 48,286 expedientes únicos
length(unique(df_cautelares$id_per_agresora))   # 63,260 agresores únicos

dim(df_sentencias)[1]                           # 129,497 observaciones (sentencias por delito)
length(unique(df_sentencias$id_exp))            # 97,177 expedientes únicos 
length(unique(df_sentencias$id_per_agresora))   # 116,378 agresores únicos



# Fin del código #

        
