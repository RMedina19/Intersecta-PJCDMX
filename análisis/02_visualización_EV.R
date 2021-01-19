########## Medidas cautelares TSJ CDMX 

rm(list=ls())
setwd("~")

require(pacman)
p_load(scales, tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor,extrafont,
       beepr, extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, cowplot)

# ------------ Directorios ---------------- #

inp <- "/Users/samnbk/Dropbox/SaM/By Date/marzo 2018/R_Genero/TSJDF/inp"
out <- "/Users/samnbk/Dropbox/SaM/By Date/marzo 2018/R_Genero/TSJDF/out"

############## Tema para exportar en .png

tema <-  theme_linedraw() +
  theme(text = element_text(family = "Helvetica", color = "grey35"),
        plot.title = element_text(size = 20, face = "bold", margin = margin(10,4,5,4), family="Helvetica", color = "black"),
        plot.subtitle = element_text(size = 18, color = "#666666", margin = margin(5, 5, 5, 5), family="Helvetica"),
        plot.caption = element_text(hjust = 1, size = 14, family = "Helvetica"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 16, family="Helvetica"),
        legend.text = element_text(size = 16, family="Helvetica"),
        legend.title.align = 1,
        axis.title = element_text(size = 16, hjust = .5, margin = margin(1,1,1,1), family="Helvetica"),
        axis.text = element_text(size = 16, face = "bold", family="Helvetica", angle=0, hjust=.5),
        strip.background = element_rect(fill="#525252"),
        strip.text.x = element_text(size=16, family = "Helvetica"),
        strip.text.y = element_text(size=16, family = "Helvetica"))

fill_base <-  c("#F178B1","#998FC7", "#FF8C00", "#663399", "#C2F970", "#00979C", "#B1EDE8", "#FE5F55")
fill_autoridades <-  c("#F178B1","#998FC7", "#FF8C00", "#663399", "#C2F970", "#00979C", "#B1EDE8", "#FE5F55", "#C52233")
fill_dos <-  c("#F178B1","#998FC7")

############# base

sentenciados = read.csv(paste(inp, "cautelares.csv", sep="/"))
sentenciados <- read.csv("datos_crudos/cautelares.csv")
sentenciados$tot = 1

View(sentenciados)

sentenciados = filter(sentenciados, year_audiencia != "2015")

sentenciados$homicidio_1 <- as.numeric(str_detect(sentenciados$delito, "Homicidio"))
sentenciados$homicidio_2 <- as.numeric(str_detect(sentenciados$delito, "Feminicidio"))
sentenciados$homicidio   <- rowSums(sentenciados[c("homicidio_1", "homicidio_2")], na.rm = T)
sentenciados$homicidio_2 <- sentenciados$homicidio_1  <- NULL

sentenciados$salud <- as.numeric(str_detect(sentenciados$delito, "salud"))

sentenciados$robo <- as.numeric(str_detect(sentenciados$delito, "Robo"))

sentenciados$familiar <- as.numeric(str_detect(sentenciados$delito, "Violencia familiar"))

sentenciados$lesiones <- as.numeric(str_detect(sentenciados$delito, "Lesiones"))

sentenciados$encubrimiento <- as.numeric(str_detect(sentenciados$delito, "Encubrimiento"))

sentenciados$extorsion <- as.numeric(str_detect(sentenciados$delito, "Extorsion"))

sentenciados$objetos <- as.numeric(str_detect(sentenciados$delito, "Portacion de objetos"))

sentenciados$secuestro_1 <- as.numeric(str_detect(sentenciados$delito, "Secuestro"))
sentenciados$secuestro_2 <- as.numeric(str_detect(sentenciados$delito, "Privación de la libertad"))
sentenciados$secuestro   <- rowSums(sentenciados[c("secuestro_1", "secuestro_2")], na.rm = T)
sentenciados$secuestro_2 <- sentenciados$secuestro_1  <- NULL

sentenciados$sexuales_1 <- as.numeric(str_detect(sentenciados$delito, "Abuso sexual"))
sentenciados$sexuales_2 <- as.numeric(str_detect(sentenciados$delito, "Violacion"))
sentenciados$sexuales_3 <- as.numeric(str_detect(sentenciados$delito, "Hostigamiento"))
sentenciados$sexuales   <- rowSums(sentenciados[c("sexuales_1", "sexuales_2", "sexuales_3")], na.rm = T)
sentenciados$sexuales_3 <- sentenciados$sexuales_2 <- sentenciados$sexuales_1 <- NULL

sentenciados = mutate(sentenciados, otros = ifelse(homicidio != 1 & salud != 1 & robo != 1 & familiar != 1 & lesiones != 1 & encubrimiento != 1 &
                                                     extorsion != 1 & objetos != 1 & secuestro != 1 & sexuales !=1, 1, 0))
# Línea añadida por Regina
sentenciados <- rename(sentenciados, numero = "ï..numero")

autofinal = select(sentenciados, numero, homicidio, salud, robo, familiar, lesiones, encubrimiento, extorsion, objetos, secuestro, sexuales, otros)

autofinal = ddply(autofinal,"numero",numcolwise(sum))

autoridad = select(autofinal, numero, homicidio, salud, robo, familiar, lesiones, encubrimiento, extorsion, objetos, secuestro, sexuales, otros)

autoridad$homicidio  = gsub("1", "Homicidio", autoridad$homicidio)
autoridad$salud  = gsub("1", "Delitos contra la salud", autoridad$salud)
autoridad$robo  = gsub("1", "Robo", autoridad$robo)
autoridad$familiar  = gsub("1", "Violencia familiar", autoridad$familiar)
autoridad$lesiones  = gsub("1", "Lesiones", autoridad$lesiones)
autoridad$encubrimiento  = gsub("1", "Encubrimiento", autoridad$encubrimiento)
autoridad$extorsion  = gsub("1", "Extorsión", autoridad$extorsion)
autoridad$objetos  = gsub("1", "Portación de objetos aptos para agredir", autoridad$objetos)
autoridad$secuestro  = gsub("1", "Secuestro", autoridad$secuestro)
autoridad$sexuales  = gsub("1", "Delitos sexuales", autoridad$sexuales)
autoridad$otros  = gsub("1", "Otros delitos", autoridad$otros)

autoridad <- autoridad %>% 
  mutate(delitos_cortos = paste(homicidio, salud, robo, familiar, lesiones, encubrimiento, extorsion, objetos, secuestro, sexuales, otros))%>% 
  mutate(delitos_cortos = str_remove_all(delitos_cortos, "0 "))%>% 
  mutate(delitos_cortos = str_remove_all(delitos_cortos, " 0"))

autoridades = select(autoridad, numero, delitos_cortos)

sentenciados = full_join(sentenciados, autoridades, by=c("numero"))

sentenciados$sexo  = gsub("Femenino", "Mujeres", sentenciados$sexo)
sentenciados$sexo  = gsub("Masculino", "Hombres", sentenciados$sexo)

sentenciados$delitos_cortos  = gsub("Portación de objetos aptos para agredir", "Portación de objetos \n aptos para agredir", sentenciados$delitos_cortos)

table(sentenciados$medida)
sentenciados$medida  = gsub("El embargo de bienes;", "Embargo", sentenciados$medida)
sentenciados$medida  = gsub("El resguardo en su propio domicilio con las modalidades que el juez disponga", "Resguardo en domicilio", sentenciados$medida)
sentenciados$medida  = gsub("El sometimiento al cuidado o vigilancia de una persona o institución determinada o internamiento a institución determinada;", "Vigilancia o internamiento", sentenciados$medida)
sentenciados$medida  = gsub("La colocación de localizadores electrónicos", "Localizadores electrónicos", sentenciados$medida)
sentenciados$medida  = gsub("La exhibición de una garantía económica;", "Garantía económica", sentenciados$medida)
sentenciados$medida  = gsub("La inmovilización de cuentas y demás valores que se encuentren dentro del sistema financiero;", "Inmovilización de cuentas", sentenciados$medida)
sentenciados$medida  = gsub("La presentación periódica ante el juez o ante autoridad distinta que aquél designe;", "Presentación periódica", sentenciados$medida)
sentenciados$medida  = gsub("La prohibición de concurrir a determinadas reuniones o acercarse o ciertos lugares;", "Prohibición de ir a lugares", sentenciados$medida)
sentenciados$medida  = gsub("La prohibición de convivir, acercarse o comunicarse con determinadas personas, con las víctimas u ofendidos o testigos, siempre que no se afecte el derecho de defensa", "Prohibición de comunicarse con personas", sentenciados$medida)
sentenciados$medida  = gsub("La prohibición de salir sin autorización del país, de la localidad en la cual reside o del ámbito territorial que fije el juez;", "Prohibición de salir de un lugar", sentenciados$medida)
sentenciados$medida  = gsub("La separación inmediata del domicilio;", "Separación del domicilio", sentenciados$medida)
sentenciados$medida  = gsub("La suspensión temporal en el ejercicio de una determinada actividad profesional o laboral", "Suspensión laboral", sentenciados$medida)
sentenciados$medida  = gsub("La suspensión temporal en el ejercicio del cargo cuando se le atribuye un delito cometido por servidores públicos", "Suspensión laboral", sentenciados$medida)
sentenciados$medida  = gsub("Prisión Preventiva", "Prisión preventiva", sentenciados$medida)


View(sentenciados)

########################

########################

porano <- sentenciados %>% 
  group_by(medida, delitos_cortos, comision) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(comision, delitos_cortos) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))%>% 
  filter(medida == "Prisión preventiva")
View(porano)

ggplot(porano, aes(x = comision, y=porcent, fill=medida)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("#F178B1","#998FC7"))+
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label=paste0(porcent,"%")),
            position = position_stack(vjust = 0.5), size=4, color="black", family = "Helvetica")+
  labs(title="Proporción que representa la prisión preventiva del total de medidas cautelares",
       caption="\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. ", x="", y="",
       subtitle = "Por delito y forma de comisión \n", fill="") +
  tema +
  facet_wrap(~delitos_cortos)  +
  coord_flip(ylim=c(0,100))+
  theme(legend.position = "top")
ggsave(paste(out, "delitos medidas culposos.png", sep = "/"), width = 18, height = 16)

########################

porano <- sentenciados %>% 
  group_by(delitos_cortos, comision) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(delitos_cortos) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano, aes(x = delitos_cortos, y=porcent, fill=comision)) +
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
ggsave(paste(out, "delitos forma comisión.png", sep = "/"), width = 18, height = 16)

########################
        
porano <- sentenciados %>% 
  group_by(year_audiencia, delitos_cortos) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_audiencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano) +
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
ggsave(paste(out, "1 sentenciados.png", sep = "/"), width = 20, height = 16)

########################

porano <- sentenciados %>% 
  group_by(delitos_cortos, year_audiencia, medida) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(delitos_cortos, year_audiencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

ggplot(porano) +
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
ggsave(paste(out, "1 DELITOS X MEDIDAS.png", sep = "/"), width = 20, height = 16)

######################## delitos por medida

porano <- sentenciados %>% 
  group_by(delitos_cortos, year_audiencia, medida) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(medida, year_audiencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

ggplot(porano) +
  geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=delitos_cortos), size=2.5) +
  labs(title = "Delitos que se dictaron por medida en la CDMX", subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
       caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
       fill ="Delitos:") +
  scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                               "#9b5de5", "#0466c8", "#ffee32")) +
  scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
  scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
  tema +
  facet_wrap(~medida)  +
  theme(axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5)) +
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position = "right", 
        legend.key.size = unit(.5, "cm"),
        legend.key.width = unit(.5,"cm")) 
ggsave(paste(out, "1 MEDIDAS X DELITOS.png", sep = "/"), width = 20, height = 16)

######################## delitos por medida

porano <- sentenciados %>% 
  group_by(sexo, year_audiencia, medida) %>% 
  mutate(tot = 1) %>% 
  filter(sexo != "No especificado")%>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sexo, year_audiencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

ggplot(porano) +
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

######################## delitos por medida

porano <- sentenciados %>% 
  group_by(delitos_cortos, year_audiencia, medida) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(medida, year_audiencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

porano = filter(porano, medida == "Prisión preventiva")

ggplot(porano) +
  geom_area(aes(x = as.integer(year_audiencia), y = porcent, fill=delitos_cortos), size=2.5) +
  labs(title = "Delitos que tuvieron prisión preventiva en la CDMX", subtitle = "Por año \n", y = "\n Porcentaje \n", x="",
       caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
       fill ="Delitos:") +
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
ggsave(paste(out, "1 PP X DELITOS.png", sep = "/"), width = 20, height = 16)

######################## delitos por medida

porano <- sentenciados %>% 
  group_by(delitos_cortos, year_audiencia, medida, sexo) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(medida, year_audiencia, sexo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

porano = filter(porano, medida == "Prisión preventiva")
porano = filter(porano, sexo != "No especificado")


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


########################

porano <- sentenciados %>% 
  group_by(year_audiencia, medida) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_audiencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

ggplot(porano) +
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


########################

porano <- sentenciados %>% 
  group_by(anio_ing, sentencia, sexo) %>% 
  filter(sexo != "No especificado") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio_ing, sexo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano) +
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

########################

porano <- sentenciados %>% 
  group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
  filter(sexo != "No especificado") %>% 
  filter(sentencia == "Condenatoria") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio_ing, sexo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano) +
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

########################

porano <- sentenciados %>% 
  group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
  filter(sexo != "No especificado") %>% 
  filter(sentencia == "Condenatoria") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio_ing, delitos_cortos) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

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

########################

porano <- sentenciados %>% 
  group_by(anio_ing, sentencia, delitos_cortos, sexo) %>% 
  filter(sexo != "No especificado") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio_ing, sexo, sentencia) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano) +
  geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=delitos_cortos), size=2.5) +
  labs(title = "Delitos de las personas condenadas", subtitle = "Por año y sexo de la persona condenada \n", y = "\n Porcentaje \n", x="",
       caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
       fill ="Delitos:") +
  scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b")) +
  scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
  scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
  tema +
  facet_grid(sexo~sentencia, labeller = label_wrap_gen(width=12)) +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position = "right") 
ggsave(paste(out, "1 condena abso sexo.png", sep = "/"), width = 20, height = 16)

########################

porano <- sentenciados %>% 
  group_by(sentencia, delitos_cortos, sexo) %>% 
  filter(sexo != "No especificado") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sexo, delitos_cortos) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano, aes(x = reorder(delitos_cortos, -porcent), y=porcent, fill=sentencia)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("#F178B1","#998FC7"))+
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label=paste0(porcent,"%")),
            position = position_stack(vjust = 0.5), size=4, color="black", family = "Helvetica")+
  labs(title="El sentido de las sentencias por delito",
       caption="\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. ", x="", y="",
       subtitle = "Según el sexo de la persona sentenciada \n", fill="Sentido de la sentencia:") +
  tema +
  facet_wrap(~sexo)  +
  coord_flip(ylim=c(0,100))+
  theme(legend.position = "top")
ggsave(paste(out, "delitos por sexos cdmx.png", sep = "/"), width = 18, height = 16)

########################

porano <- sentenciados %>% 
  group_by(delitos_cortos, sexo) %>% 
  filter(sexo != "No especificado") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(delitos_cortos) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)


ggplot(porano, aes(x = reorder(delitos_cortos, -porcent), y=porcent, fill=sexo)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("#F178B1","#998FC7"))+
  guides(fill = guide_legend(reverse=TRUE))+
  geom_text(aes(label=paste0(porcent,"%")),
            position = position_stack(vjust = 0.5), size=4, color="black", family = "Helvetica")+
  labs(title="El sexo de las personas condenadas en la CDMX",
       caption="\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. ", x="", y="",
       subtitle = "Según el delito (para las sentencias de 2011-2019) \n", fill="Sexo de persona condenada:") +
  tema +
  coord_flip(ylim=c(0,100))+
  theme(legend.position = "top")
ggsave(paste(out, "condenas por sexo.png", sep = "/"), width = 18, height = 16)

########################

porano <- sentenciados %>% 
  group_by(anio_ing, sentencia, delito, robo, sexo) %>% 
  filter(robo == "1")%>%
  filter(sexo != "No especificado") %>% 
  filter(sentencia == "Condenatoria") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio_ing, sexo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(porano) +
  geom_area(aes(x = as.integer(anio_ing), y = porcent, fill=delito), size=2.5) +
  labs(title = "Delitos de robo por los que las personas fueron condenadas", subtitle = "Por año y sexo de la persona condenada \n", y = "\n Porcentaje \n", x="",
       caption = "\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
       fill ="Delitos:") +
  scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                               "#6d6875", "#9d0208", "#6930c3", "#a2d2ff", "#52796f", "#ff006e", "#3e1f47", "#ffcad4", "#ffcb77", "#40916c")) +
  scale_x_continuous(breaks=seq(from=2011, to=2019, by=1)) +
  scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
  tema +
  facet_wrap(~sexo)  +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5)) +
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position = "right") 
ggsave(paste(out, "1 condena robo sexo.png", sep = "/"), width = 20, height = 16)

########################

porano <- sentenciados %>% 
  group_by(sentencia, delito, robo, sexo) %>% 
  filter(robo == "1")%>%
  filter(sexo != "No especificado") %>% 
  filter(sentencia == "Condenatoria") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sexo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(subset(porano),
       aes(area=porcent, fill=delito, label=paste(paste0(porcent,"%")))) +
  geom_treemap() + 
  geom_treemap_text(place = "centre", grow = F, color ="black", family = "Helvetica") +
  scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                               "#6d6875", "#9d0208", "#6930c3", "#a2d2ff", "#52796f", "#ff006e", "#3e1f47", "#ffcad4", "#ffcb77", "#40916c")) +
  labs(title="Los delitos de robo por los que las personas fueron condenadas por el TSJ CDMX", 
       subtitle = "Según el sexo de la persona condenada \n", 
       caption="\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
       fill="Delito de robo:") +
  facet_wrap(~sexo) +
  tema + 
  theme(strip.text.x = element_text(size=20, face="bold", angle=0, color = "black"),
        strip.text.y = element_text(size=20, face="bold", color = "black", angle = 0),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(legend.position = "right") +
  theme(legend.text = element_text(color = "black", size = 8))+
  coord_fixed()
ggsave(paste(out, "treemap viofami.png", sep = "/"), width =20, height = 16) 

########################

porano <- sentenciados %>% 
  group_by(sentencia, delito, homicidio, sexo) %>% 
  filter(homicidio == "1")%>%
  filter(sexo != "No especificado") %>% 
  filter(sentencia == "Condenatoria") %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sexo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))
View(porano)

ggplot(subset(porano),
       aes(area=porcent, fill=delito, label=paste(paste0(porcent,"%")))) +
  geom_treemap() + 
  geom_treemap_text(place = "centre", grow = F, color ="black", family = "Helvetica") +
  scale_fill_manual(values = c("#EDF7FC","#F6CCEE", "#04C0E4", "#016FB9", "#3AB79C","#A3FEFC", "#FF82A9", "#e63946", "#457b9d", "#2a9d8f", "#e5989b",
                               "#6d6875", "#9d0208", "#6930c3", "#a2d2ff", "#52796f", "#ff006e", "#3e1f47", "#ffcad4", "#ffcb77", "#40916c")) +
  labs(title="Los delitos de homicidio por los que las personas fueron condenadas por el TSJ CDMX", 
       subtitle = "Según el sexo de la persona condenada \n", 
       caption="\n Fuente: Respuesta del TSJCDMX a solicitud de acceso a la información pública. \n", 
       fill="Delito de homicidio:") +
  facet_wrap(~sexo) +
  tema + 
  theme(strip.text.x = element_text(size=20, face="bold", angle=0, color = "black"),
        strip.text.y = element_text(size=20, face="bold", color = "black", angle = 0),
        strip.background = element_rect(colour="white", fill="white")) +
  theme(legend.position = "right") +
  theme(legend.text = element_text(color = "black", size = 8))+
  coord_fixed()
ggsave(paste(out, "treemap homicidio iofami.png", sep = "/"), width =20, height = 16) 

