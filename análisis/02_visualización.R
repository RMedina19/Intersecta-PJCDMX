#------------------------------------------------------------------------------#
# Objetivo:                     Procesar datos de la PJCDMX
# Encargadas:                   Estefanía Vela y Regina I. Medina
# Correo:                       rmedina@intersecta.org
# Fecha de creación:            10 de enero de 2021
# Última actualización:         20 de enero de 2021
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
