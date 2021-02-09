#------------------------------------------------------------------------------#
# Proyecto:                   TRIBUNAL SUPERIOR DE JUSTICIA DE CIUDAD DE MÉXICO
# Objetivo:                   Procesar datos de la PJCDMX
# Encargadas:                 Estefanía Vela y Regina I. Medina
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          10 de enero de 2021
# Última actualización:       24 de enero de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)

p_load(scales, tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor,
        extrafont, beepr, extrafont, treemapify, ggmosaic, srvyr, ggrepel, 
        lubridate, cowplot)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios 
inp             <- "datos_limpios/"
out             <- "figuras/"
asuntos         <- "asuntos_ingresados/"
personas        <- "personas_agredidas/"
sitjur          <- "situacion_juridica/"
alternas        <- "soluciones_alternas/"
medidas         <- "medidas_cautelares/"
sentencias      <- "sentencias/"


# 1. Cargar datos --------------------------------------------------------------
load(paste0(inp, "df_asuntos_ingresados.RData"))
load(paste0(inp, "df_personas_agredidas.RData"))
load(paste0(inp, "df_situacion_juridica.RData"))
load(paste0(inp, "df_soluciones_alternas.RData"))
load(paste0(inp, "df_medidas_cautelares.RData"))
load(paste0(inp, "df_sentencias.RData"))

# 2. Configuración del tema para visualización ---------------------------------

tema <- theme_linedraw() +
        theme(text = element_text(family = "Helvetica", color = "grey35"),
                plot.title = element_text(size = 20, 
                                        face = "bold", 
                                        margin = margin(10,4,5,4), 
                                        family="Helvetica", 
                                        color = "black"),
                plot.subtitle = element_text(size = 18, 
                                        color = "#666666", 
                                        margin = margin(5, 5, 5, 5), 
                                        family="Helvetica"),
                plot.caption = element_text(hjust = 1, 
                                        size = 14, 
                                        family = "Helvetica"),
                panel.grid = element_line(linetype = 2), 
                legend.position = "none",
                panel.grid.minor = element_blank(),
                legend.title = element_text(size = 16, 
                                        family="Helvetica"),
                legend.text = element_text(size = 16, 
                                        family="Helvetica"),
                legend.title.align = 1,
                axis.title = element_text(size = 16, 
                                        hjust = .5, 
                                        margin = margin(1,1,1,1), 
                                        family="Helvetica"),
                axis.text = element_text(size = 16, 
                                        face = "bold", 
                                        family="Helvetica", 
                                        angle=0, 
                                        hjust=.5),
                strip.background = element_rect(fill="#525252"),
                strip.text.x = element_text(size=16, 
                                        family = "Helvetica"),
                strip.text.y = element_text(size=16, 
                                        family = "Helvetica")) +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", 
                "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", 
                "#e5989b"))

fill_base        <-  c("#F178B1","#998FC7", "#FF8C00", "#663399", "#C2F970", 
                        "#00979C", "#B1EDE8", "#FE5F55")
fill_autoridades <-  c("#F178B1","#998FC7", "#FF8C00", "#663399", "#C2F970", 
                        "#00979C", "#B1EDE8", "#FE5F55", "#C52233")
fill_dos         <-  c("#F178B1","#998FC7")

fill_default <-  c("#EDF7FC", "#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C",
                   "#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", 
                   "#e5989b", "#9b5de5", "#0466c8", "#ffee32")

# Establecer vectores de texto 
leyenda <- "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. "


# 3. Visualizaciones de asuntos ingresados -------------------------------------
# Delitos
df_delitos <- df_asuntos_ingresados %>% 
        group_by(delitos_cortos) %>% 
        summarize(total = n()) %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Visualización 
ggplot(df_delitos, 
        aes(x = delitos_cortos, y = porcent, fill = fill_default[10])) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(porcent, "%")),
                position = position_stack(vjust = 0.5), 
                size = 4, color = "black", family = "Helvetica") +
        labs(title = "Distribución de delitos cometidos en CDMX", 
                subtitle = "(2011-2020)", 
                x = "", 
                y = "Porcentaje") +
        coord_flip(ylim=c(0,100)) +
        tema

# Guardar visualización 
ggsave(paste0(out, asuntos, "g_delitos.png"), width = 18, height = 10)


# Delitos por comisión 
df_delitos_comision <- df_asuntos_ingresados %>% 
        group_by(delitos_cortos, comision) %>% 
        summarize(total = n()) %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))


# Visualización 
ggplot(df_delitos_comision, 
        aes(x = delitos_cortos, y = porcent, fill = comision)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(porcent, "%")),
                position = position_stack(vjust = 0.5), 
                size = 4, color = "black", family = "Helvetica") +
        labs(title = "Distribución de delitos cometidos en CDMX", 
                subtitle = "Según comisión (2011-2020)", 
                x = "", 
                y = "Porcentaje") +
        coord_flip(ylim=c(0,100)) +
        tema +
        scale_fill_manual(values= fill_default)
        
# Guardar visualización 
ggsave(paste0(out, asuntos, "g_delitos_comision.png"), width = 18, height = 10)


# Delitos por año 
df_delitos_year <- df_asuntos_ingresados %>% 
        group_by(year_ingreso, delitos_cortos)        %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(year_ingreso)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# View(df_year)

# Visualización 
ggplot(df_delitos_year) +
        geom_area(aes(x = as.integer(year_ingreso), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos de los asuntos ingresados al TSJ-CDMX", 
                subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "top") 

# Guardar visualización
ggsave(paste0(out, asuntos, "g_delitos_año.png"), width = 20, height = 16)
        
        
# Delitos por sexo         
df_delitos_sexo <- df_asuntos_ingresados %>% 

# Delitos por año y por sexo         
df_delitos_year_sexo <- df_asuntos_ingresados %>% 



# 4. Visualizaciones de personas agredidas -------------------------------------
# Desagregación por sexo 
df_sexo <- df_personas_agredidas %>% 
        rename(sexo = sexo_victima) %>% 
        group_by(sexo) %>% 
        summarize(total = n()) %>% 
        ungroup()  %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1)) %>% 
        filter(is.na(sexo) == FALSE)

# Visualización 
ggplot(df_sexo, 
        aes(x = sexo, y = porcent, fill = sexo)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(porcent,"%")),
                position = position_stack(vjust = 0.5), 
                size = 4, color = "black", family = "Helvetica") +
        labs(title    = "Proporción de víctimas",
                subtitle = "Por sexo", 
                caption  = leyenda, 
                x = "", y = "", fill = "") +
        tema +
        scale_fill_manual(values= fill_default) +
        coord_flip(ylim=c(0,100))   +
        theme(legend.position = "top")

# Guardar visualización
ggsave(paste0(out, personas, "g_víctimas_genero.png"), width = 18, height = 10)


# Desagregación por edad
df_edad <- df_personas_agredidas %>% 
        rename(edad = edad_victima) %>% 
        group_by(edad) %>% 
        summarize(total = n()) %>% 
        ungroup()  %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1)) %>% 
        filter(is.na(edad) == FALSE)


# Visualización 
ggplot(df_edad, 
        aes(x = edad, y = porcent, fill = edad)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(porcent,"%")),
                position = position_stack(vjust = 0.5), 
                size = 4, color = "black", family = "Helvetica") +
        labs(title    = "Proporción de víctimas",
                subtitle = "Por edad", 
                caption  = leyenda, 
                x = "", 
                y = "",
                fill = "") +
        tema +
        #scale_fill_manual(values=c("#F178B1","#998FC7", "#04C0E4")) +
        coord_flip(ylim=c(0,100))   +
        theme(legend.position = "top")

# Guardar visualización
ggsave(paste0(out, personas, "g_víctimas_edad.png"), width = 18, height = 10)



# 5. Visualizaciones de situación jurídica -------------------------------------
# Por género
df_genero <- df_situacion_juridica %>% 
        rename(sexo = sexo_procesada) %>% 
        group_by(sexo) %>% 
        summarize(total = n()) %>% 
        ungroup()  %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1)) %>% 
        filter(is.na(sexo) == FALSE)

# Por delito 


# 6. Visualizaciones de soluciones alternas ------------------------------------
# 7. Visualizaciones de medidas cautelares -------------------------------------
# 7.1 Prisión preventiva por delito y comisión ---------------------------------
# Limpieza de datos
df_prisprev <- df_medidas_cautelares                            %>% 
        group_by(medida, delitos_cortos, comision)      %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(comision, delitos_cortos)              %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))%>% 
        filter(medida == "Prisión preventiva")

                # View(df_prisprev)

# Visualización 
ggplot(df_prisprev, 
        aes(x = comision, y = porcent, fill = medida))  +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = paste0(porcent,"%")),
                      position = position_stack(vjust = 0.5), 
                      size = 4, color = "black", family = "Helvetica") +
        labs(title    = "Proporción que representa la prisión preventiva del total de medidas cautelares",
             subtitle = "Por delito y forma de comisión \n", 
             caption  = leyenda, 
             x = "", 
             y = "",
             fill = "") +
        tema +
        scale_fill_manual(values=fill_default) +
        facet_wrap(~delitos_cortos) +
        coord_flip(ylim=c(0,100))   +
        theme(legend.position = "top")

# Guardar visualización
ggsave(paste0(out, medidas, "g_delitos_medidas_culposos.png"), width = 18, height = 16)


# 7.2 Delitos por comisión -----------------------------------------------------

# Limpieza de datos
df_delito <- df_medidas_cautelares                              %>% 
        group_by(delitos_cortos, comision)              %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(delitos_cortos)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))
# View(df_delito)

# Visualización 
ggplot(df_delito, aes(x = delitos_cortos, y=porcent, fill=comision)) +
        geom_bar(stat="identity", position="stack") +
        scale_fill_manual(values=fill_default)+
        guides(fill = guide_legend(reverse=TRUE))+
        geom_text(aes(label=paste0(porcent,"%")),
                position = position_stack(vjust = 0.5), 
                size=4, color="black", family = "Helvetica")+
        labs(title="Delitos por forma de comisión",
                caption=leyenda, 
                x="", y="",
                subtitle = "Por delito y forma de comisión \n", fill="") +
        tema +
        coord_flip(ylim=c(0,100))+
        theme(legend.position = "top")

# Guardar visualización
ggsave(paste0(out, medidas, "g_delitos_forma_comisión.png"), width = 18, height = 16)


# 7.3 Delitos por año ----------------------------------------------------------
# Limpieza de datos 
df_year_delito <- df_medidas_cautelares                         %>% 
        group_by(year_audiencia, delitos_cortos)        %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(year_audiencia)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

        # View(df_year)

# Visualización 
ggplot(df_year_delito) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos de las personas sentenciadas en la CDMX", 
                subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "top") 

# Guardar visualización
ggsave(paste0(out, medidas, "g_delitos_año.png"), width = 20, height = 16)

# 7.4 Medidas cautelares por delito --------------------------------------------
# Limpieza de datos 
df_medidas_delito <- df_medidas_cautelares                      %>% 
        group_by(delitos_cortos, year_audiencia, medida) %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(delitos_cortos, year_audiencia)        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

        # View(df_medidas_delito)

# Visualización 
ggplot(df_medidas_delito) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=medida), size=2.5) +
        labs(title = "Delitos de las personas sentenciadas en la CDMX", 
                subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~delitos_cortos)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 

# Guardar visualización
ggsave(paste0(out, medidas, "g_delitos_medidas.png"), width = 20, height = 16)


# 7.5 Medida cautelar por sexo -------------------------------------------------
# Limpieza de datos 
df_medida_sexo <- df_medidas_cautelares                         %>% 
        rename(sexo = sexo_vinculada) %>% 
        group_by(sexo, year_audiencia, medida)          %>% 
        filter(sexo != "No especificado")               %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(sexo, year_audiencia)                  %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Visualización 
ggplot(df_medida_sexo) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=medida), size=2.5) +
        labs(title = "Medidas cautelares dictadas en la CDMX", 
                subtitle = "Por año, por sexo \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 

# Guardar visualización
ggsave(paste0(out, medidas, "g_medida_sexo.png"), width = 20, height = 16)


# 7.6 Prisión preventiva por delito y por sexo ---------------------------------
# Limpieza de datos 
df_medida_delito_sexo <- df_medidas_cautelares                                 %>% 
        rename(sexo = sexo_vinculada) %>% 
        group_by(delitos_cortos, year_audiencia, medida, sexo) %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(medida, year_audiencia, sexo)          %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1)) %>% 
        filter(medida == "Prisión preventiva", 
                sexo != "No especificado")

# Visualización 
ggplot(df_medida_delito_sexo) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos que tuvieron prisión preventiva en la CDMX", 
                subtitle = "Por año, por sexo \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 

# Guardar visualización
ggsave(paste0(out, medidas, "g_medidas_delito_sexo.png"), width = 20, height = 16)

# 7.7 Medidas cautelares por año -----------------------------------------------
# Limpieza de datos 
df_year_medidas <- df_medidas_cautelares                        %>% 
        group_by(year_audiencia, medida)                %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(year_audiencia)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Visualización 
ggplot(df_year_medidas) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=medida), size=2.5) +
        labs(title = "Medidas cautelares dictadas por el Tribunal Superior de Justicia de la CDMX", 
                subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Medidas cautelares:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 

# Guardar visualización
ggsave(paste0(out, medidas, "g_medidas_año.png"), width = 20, height = 16)




# 8. Visualizaciones de sentencias ---------------------------------------------

# 8.1 Sentido de la sentencia por sexo -----------------------------------------
# Limpieza de datos 
df_sentencia_sexo <- df_medidas_cautelares                      %>% 
        group_by(anio_ing, sentencia, sexo)             %>% 
        filter(sexo != "No especificado")               %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(anio_ing, sexo)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))


# Visualización 
ggplot(df_sentencia_sexo) +
        geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=sentencia), size=2.5) +
        labs(title = "Sentido de la sentencia", 
                subtitle = "Por año y sexo de la persona sentenciada \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right") 

# Guardar visualización
ggsave(paste(out, "1 sentido sexo.png", sep = "/"), width = 20, height = 16)

# 8.2 Delitos de las personas condenadas ---------------------------------------
# Limpieza de datos 
df_delitos_condenadas <- df_medidas_cautelares                  %>% 
        group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
        filter(sexo != "No especificado")               %>% 
        filter(sentencia == "Condenatoria")             %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(anio_ing, sexo)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

        # Visualización 
ggplot(df_delitos_condenadas) +
        geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos de las personas condenadas", 
                subtitle = "Por año y sexo de la persona condenada \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Delitos:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right") 

# Guardar visualización
ggsave(paste(out, "1 condena sexo.png", sep = "/"), width = 20, height = 16)

# 8.3 Personas condenadas por sexo ---------------------------------------------
# Limpieza de datos 
df_condenadas_sexo  <- sentenciados                     %>% 
        group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
        filter(sexo != "No especificado")               %>% 
        filter(sentencia == "Condenatoria")             %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(anio_ing, delitos_cortos)              %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Visualización 
ggplot(porano) +
        geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=sexo), size=2.5) +
        labs(title = "Sexo de las personas condenadas en la CDMX", 
                subtitle = "Por año y por delito \n", y = "\n Porcentaje \n", x="",
                caption = leyenda, 
                fill ="Sexo de la persona condenada:") +
        scale_fill_manual(values = fill_default) +
        scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~delitos_cortos)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right") 

# Guardar visualización
ggsave(paste(out, "condena sexo por año por delito.png", sep = "/"), width = 20, height = 16)


# Fin del código #