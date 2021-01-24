#------------------------------------------------------------------------------#
# Objetivo:                     Procesar datos de la PJCDMX
# Encargadas:                   Estefanía Vela y Regina I. Medina
# Correo:                       rmedina@intersecta.org
# Fecha de creación:            10 de enero de 2021
# Última actualización:         23 de enero de 2021
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
inp <- "datos_limpios/"
out <- "figuras/"

# 1. Cargar datos --------------------------------------------------------------
load(paste0(inp, "df_cautelares.RData"))

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
                                        family = "Helvetica"))

fill_base        <-  c("#F178B1","#998FC7", "#FF8C00", "#663399", "#C2F970", 
                        "#00979C", "#B1EDE8", "#FE5F55")
fill_autoridades <-  c("#F178B1","#998FC7", "#FF8C00", "#663399", "#C2F970", 
                        "#00979C", "#B1EDE8", "#FE5F55", "#C52233")
fill_dos         <-  c("#F178B1","#998FC7")



# 3. Preparación de datos para visualización -----------------------------------
# Uso de prisión preventiva desagregado por delito y comisión 
df_prisprev <- df_cautelares                            %>% 
        group_by(medida, delitos_cortos, comision)      %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(comision, delitos_cortos)              %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))%>% 
        filter(medida == "Prisión preventiva")

# View(df_prisprev)

# Delitos por forma de comisión 
df_delito <- df_cautelares                              %>% 
        group_by(delitos_cortos, comision)              %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(delitos_cortos)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))
# View(df_delito)

# Delitos por año 
df_year_delito <- df_cautelares                         %>% 
        group_by(year_audiencia, delitos_cortos)        %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(year_audiencia)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# View(df_year)

# Distribución de medidas cautelares según delito 
df_medidas_delito <- df_cautelares                      %>% 
        group_by(delitos_cortos, year_audiencia, medida) %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(delitos_cortos, year_audiencia)        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# View(df_medidas_delito)


#### FALTA AGREGAR POR SEXO ####
df_medida_sexo <- df_cautelares                         %>% 
        group_by(sexo, year_audiencia, medida)          %>% 
        filter(sexo != "No especificado")               %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(sexo, year_audiencia)                  %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Prisión preventiva por delito y por sexo 
porano <- df_cautelares                                 %>% 
        group_by(delitos_cortos, year_audiencia, medida, sexo) %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(medida, year_audiencia, sexo)          %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1)) %>% 
        filter(medida == "Prisión preventiva", 
                sexo != "No especificado")

# Medidas cautelares por año
df_year_medidas <- df_cautelares                        %>% 
        group_by(year_audiencia, medida)                %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(year_audiencia)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Sentido de la sentencia por sexo
df_sentencia_sexo <- df_cautelares                      %>% 
        group_by(anio_ing, sentencia, sexo)             %>% 
        filter(sexo != "No especificado")               %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(anio_ing, sexo)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Delitos de las personas condenadas
df_delitos_condenadas <- df_cautelares                  %>% 
        group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
        filter(sexo != "No especificado")               %>% 
        filter(sentencia == "Condenatoria")             %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(anio_ing, sexo)                        %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))

# Personas condenadas por sexo
df_condenadas_sexo  <- sentenciados                     %>% 
        group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
        filter(sexo != "No especificado")               %>% 
        filter(sentencia == "Condenatoria")             %>% 
        summarize(total = n())                          %>% 
        ungroup()                                       %>% 
        group_by(anio_ing, delitos_cortos)              %>% 
        mutate(denomin = sum(total, na.rm = T),
                porcent = round(total / denomin * 100, 1))


# 4. Visualización de datos ----------------------------------------------------
# Establecer vectores de texto 
leyenda <- "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. "

# 4.1 Prisión preventiva por delito y comisión ---------------------------------
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
        scale_fill_manual(values=c("#F178B1","#998FC7")) +
        facet_wrap(~delitos_cortos) +
        coord_flip(ylim=c(0,100))   +
        theme(legend.position = "top")

ggsave(paste0(out, "g_delitos_medidas_culposos.png"), width = 18, height = 16)


# 4.2 Delitos por comisión -----------------------------------------------------
ggplot(df_delito, aes(x = delitos_cortos, y=porcent, fill=comision)) +
        geom_bar(stat="identity", position="stack") +
        scale_fill_manual(values=c("#F178B1","#998FC7"))+
        guides(fill = guide_legend(reverse=TRUE))+
        geom_text(aes(label=paste0(porcent,"%")),
                position = position_stack(vjust = 0.5), size=4, color="black", family = "Helvetica")+
        labs(title="Delitos por forma de comisión",
                caption="\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. ", x="", y="",
                subtitle = "Por delito y forma de comisión \n", fill="") +
        tema +
        coord_flip(ylim=c(0,100))+
        theme(legend.position = "top")
ggsave(paste(out, "g_delitos_forma_comisión.png", sep = "/"), width = 18, height = 16)


# 4.3 Delitos por año ----------------------------------------------------------
ggplot(df_year_delito) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos de las personas sentenciadas en la CDMX", subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Delitos:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b")) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "top") 
ggsave(paste(out, "g_delitos_año.png", sep = "/"), width = 20, height = 16)

# 4.4 Medidas cautelares por delito --------------------------------------------
ggplot(df_medidas_delito) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=medida), size=2.5) +
        labs(title = "Delitos de las personas sentenciadas en la CDMX", subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Delitos:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                "#9b5de5", "#0466c8", "#ffee32")) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~delitos_cortos)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 
ggsave(paste(out, "g_delitos_medidas.png", sep = "/"), width = 20, height = 16)


# 4.5 Medida cautelar por sexo -------------------------------------------------
ggplot(df_medida_sexo) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=medida), size=2.5) +
        labs(title = "Medidas cautelares dictadas en la CDMX", subtitle = "Por año, por sexo \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Delitos:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                "#9b5de5", "#0466c8", "#ffee32")) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 
ggsave(paste(out, "1 MEDIDAS X sexo.png", sep = "/"), width = 20, height = 16)


# 4.6 Prisión preventiva por delito y por sexo ---------------------------------
ggplot(porano) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos que tuvieron prisión preventiva en la CDMX", subtitle = "Por año, por sexo \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Delitos:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                "#9b5de5", "#0466c8", "#ffee32")) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 
ggsave(paste(out, "1 PP X DELITOS x sesxo.png", sep = "/"), width = 20, height = 16)

# 4.7 Medidas cautelares por año -----------------------------------------------
ggplot(df_year_medidas) +
        geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=medida), size=2.5) +
        labs(title = "Medidas cautelares dictadas por el Tribunal Superior de Justicia de la CDMX", subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Medidas cautelares:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                "#9b5de5", "#0466c8", "#ffee32")) +
        scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right", 
                legend.key.size = unit(.5, "cm"),
                legend.key.width = unit(.5,"cm")) 
ggsave(paste(out, "1 MEDIDAS X AÑO.png", sep = "/"), width = 20, height = 16)


# 4.9 Sentido de la sentencia por sexo -----------------------------------------
ggplot(df_sentencia_sexo) +
        geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=sentencia), size=2.5) +
        labs(title = "Sentido de la sentencia", subtitle = "Por año y sexo de la persona sentenciada \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Delitos:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b")) +
        scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right") 
ggsave(paste(out, "1 sentido sexo.png", sep = "/"), width = 20, height = 16)

# 4.10 Delitos de las personas condenadas --------------------------------------
ggplot(df_delitos_condenadas) +
        geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=delitos_cortos), size=2.5) +
        labs(title = "Delitos de las personas condenadas", subtitle = "Por año y sexo de la persona condenada \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Delitos:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b")) +
        scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~sexo)  +
        theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right") 
ggsave(paste(out, "1 condena sexo.png", sep = "/"), width = 20, height = 16)

# 4.11 Personas condenadas por sexo --------------------------------------------
ggplot(porano) +
        geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=sexo), size=2.5) +
        labs(title = "Sexo de las personas condenadas en la CDMX", subtitle = "Por año y por delito \n", y = "\n Porcentaje \n", x="",
                caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
                fill ="Sexo de la persona condenada:") +
        scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b")) +
        scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
        scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
        tema +
        facet_wrap(~delitos_cortos)  +
        theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
        coord_cartesian(ylim = c(0, 100))+
        theme(legend.position = "right") 
ggsave(paste(out, "condena sexo por año por delito.png", sep = "/"), width = 20, height = 16)


# Fin del código #