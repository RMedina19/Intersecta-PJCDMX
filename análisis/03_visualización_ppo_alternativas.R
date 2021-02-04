########## Base del TSJ-CDMX hasta septiembre 2020 - versión de Regina Medina

rm(list=ls())
        
#setwd("~")

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

# Limpieza de datos ------------------------------------------------------------

base = read.csv(paste(inp, "df_TJCDMX.csv", sep="/"))

# Cargar datos desde carpeta de GitHub
load("~/GitHub/Intersecta-PJCDMX/datos_limpios/df_TJCDMX.RData")
base <- read.csv("~/GitHub/Intersecta-PJCDMX/datos_limpios/df_TJCDMX.csv")

base$tot = 1

base$sexo_sentenciada  = gsub("Masculino", "Hombres", base$sexo_sentenciada)
base$sexo_sentenciada  = gsub("Femenino", "Mujeres", base$sexo_sentenciada)

base <- base %>% 
  mutate(sexoppo = paste(sexo_procesada, sexo_vinculada)) 

base$sexoppo  = gsub("Hombres Hombres", "Hombres", base$sexoppo)
base$sexoppo  = gsub("NA Hombres", "Hombres", base$sexoppo)
base$sexoppo  = gsub("No especificado NA", "No especificado", base$sexoppo)
base$sexoppo  = gsub("Hombres NA", "Hombres", base$sexoppo)
base$sexoppo  = gsub("NA Mujeres", "Mujeres", base$sexoppo)
base$sexoppo  = gsub("No especificado No especificado", "No especificado", base$sexoppo)
base$sexoppo  = gsub("Mujeres Mujeres", "Mujeres", base$sexoppo)
base$sexoppo  = gsub("Mujeres NA", "Mujeres", base$sexoppo)
base$sexoppo  = gsub("NA No especificado", "No especificado", base$sexoppo)
base$sexoppo  = gsub("NA NA", "NA", base$sexoppo)

############ pp o alternativas

base$pp1 <- ifelse(base$s_formal_prision > 0, c("Con_pp"), c("Sin_pp"))
base$pp2 <- ifelse(base$m_prision_preventiva > 0, c("Con_pp"), c("Sin_pp"))
base$pp3 <- ifelse(base$s_sujecion_en_libertad > 0, c("Con_alt"), c("Sin_alt"))
base$pp4 <- ifelse(base$m_embargo > 0, c("Con_alt"), c("Sin_alt"))
base$pp5 <- ifelse(base$m_resguardo > 0, c("Con_alt"), c("Sin_alt"))
base$pp6 <- ifelse(base$m_vigilancia > 0, c("Con_alt"), c("Sin_alt"))
base$pp7 <- ifelse(base$m_localizador > 0, c("Con_alt"), c("Sin_alt"))
base$pp8 <- ifelse(base$m_garantia_econ > 0, c("Con_alt"), c("Sin_alt"))
base$pp9 <- ifelse(base$m_inmov_cuentas > 0, c("Con_alt"), c("Sin_alt"))
base$pp10 <- ifelse(base$m_presentacion > 0, c("Con_alt"), c("Sin_alt"))
base$pp11 <- ifelse(base$m_prohib_lugares > 0, c("Con_alt"), c("Sin_alt"))
base$pp12 <- ifelse(base$m_prohib_comunica > 0, c("Con_alt"), c("Sin_alt"))
base$pp13 <- ifelse(base$m_prohib_salir > 0, c("Con_alt"), c("Sin_alt"))
base$pp14 <- ifelse(base$m_separa_domicilio > 0, c("Con_alt"), c("Sin_alt"))
base$pp15 <- ifelse(base$m_suspension_laboral > 0, c("Con_alt"), c("Sin_alt"))

base <- base %>% 
  mutate(ppalt = paste(pp1, pp2, pp3, pp4, pp5, pp6, pp7, pp8, pp9, pp10, pp11, pp12, pp13, pp14, pp15))%>% 
  mutate(ppalt = str_remove_all(ppalt, "Sin_alt"))%>% 
  mutate(ppalt = str_remove_all(ppalt, "Sin_pp"))

base$ppalt_1 <- as.numeric(str_detect(base$ppalt, "Con_pp"))
base$ppalt_2 <- as.numeric(str_detect(base$ppalt, "Con_alt"))


base_ppo = select(base, X, ppalt_1, ppalt_2, year_resolucion, year_audiencia_caut)
base_ppo$year_ppo <- rowSums(base_ppo[c("year_resolucion", "year_audiencia_caut")], na.rm = T)

table(base_ppo$year_ppo)

base_ppo$year_ppo  = gsub("4031", "2015", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4032", "2016", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4033", "2016", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4034", "2017", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4035", "2017", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4036", "2018", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4037", "2018", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4038", "2019", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4039", "2019", base_ppo$year_ppo)
base_ppo$year_ppo  = gsub("4040", "2020", base_ppo$year_ppo)

base_ppo = select(base_ppo, X, year_ppo)

base = full_join(base, base_ppo, by=c("X"))

# Revisar variables del df_TJCDMX y las generadas para base_ppo 
df_ppo <- base %>% 
        select("tot", starts_with("id"), contains("year"), starts_with("s_"), 
                sarts_with("m_"), contains("pp")) %>% 
        mutate()
        
# Visualización de datos -------------------------------------------------------

porano <- base %>% 
  mutate(tot = 1) %>% 
  select(year_ppo, ppalt_1, ppalt_2)%>%
  filter(year_ppo != 0)%>%
  filter(year_ppo != 4031)%>%
  gather(medida, tot, ppalt_1:ppalt_2)%>%
  mutate(medida = str_replace(medida, "ppalt_1", "Con prisión preventiva"),
         medida = str_replace(medida, "ppalt_2", "Alternativa a prisión preventiva"))%>% 
  ungroup() %>% 
  group_by(year_ppo, medida) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_ppo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

View(porano)

ggplot(porano, aes(x = as.integer(year_ppo), y=porcent, fill = medida)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values = c("red", "black")) +
labs(title="Distribución de las medidas cautelares",
     caption="\n Fuente: Respuesta del TSJ-CDMX a una solicitud de acceso a la información pública\n", 
     x="\n", y="\n \n",
     subtitle = "Por año \n", fill = "La medida cautelar fue:") +
  tema +
  theme(legend.position = "top") +
  scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
  theme(axis.text.x = element_text(angle = 0, size = 12))

# ggsave(paste(out, "2011-2020 proporción PP.png", sep = "/"), width = 16, height = 16)

porano <- base %>% 
  mutate(tot = 1) %>% 
  select(year_ppo, sexoppo, ppalt_1, ppalt_2)%>%
  filter(year_ppo != 0)%>%
  filter(sexoppo != "No especificado")%>%
  gather(medida, tot, ppalt_1:ppalt_2)%>%
  mutate(medida = str_replace(medida, "ppalt_1", "Con prisión preventiva"),
         medida = str_replace(medida, "ppalt_2", "Alternativa a prisión preventiva"))%>% 
  ungroup() %>% 
  group_by(year_ppo, sexoppo, medida) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_ppo, sexoppo) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

View(porano)

ggplot(porano, aes(x = as.integer(year_ppo), y=porcent, fill = medida)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values = c("red", "black"))+
  labs(title="Distribución de las medidas cautelares",
     caption="\n Fuente: Respuesta del TSJ-CDMX a una solicitud de acceso a la información pública\n", 
     x="\n", y="\n \n",
     subtitle = "Por año, por sexo \n", fill = "La medida cautelar fue:") +
  tema +
  facet_wrap(~sexoppo) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
  theme(axis.text.x = element_text(angle = 0, size = 12))

# ggsave(paste(out, "2011-2020 proporción PP sexo.png", sep = "/"), width = 16, height = 16)


ggplot(data=subset(porano, medida == "Con prisión preventiva")) +
  geom_line(aes(x = as.integer(year_ppo),  y = porcent, color=sexoppo), size=1.5) +
  geom_point(aes(x = as.integer(year_ppo), y = porcent), size=2, color = "#4d4d4d") +
  geom_text_repel(aes(x = as.integer(year_ppo), y = porcent, label=paste0(porcent, "%")), 
                  color = "black", vjust =-.5, hjust=.5,  family="Georgia", size=4.5, angle =0) +
  scale_color_manual(values = c("#46bfdf", "#ff1654")) +
  labs(title = "Hombres versus mujeres a los que se les dictó prisión preventiva", subtitle = "Por año \n", 
       y = "\n Porcentaje con prisión preventiva \n", x="",
       caption = "\n Fuente: TSJ-CDMX \n", 
       color ="Sexo de la persona con prisión preventiva dictada:") +
  scale_x_continuous(breaks=seq(from=2011, to=2020, by=1)) +
  scale_y_continuous(breaks=seq(from=0, to=100, by=10)) +
  tema +
  theme(legend.position = "top") +
  coord_cartesian(ylim = c(0, 100))

# ggsave(paste(out, "05 hom v mujer común.png", sep = "/"), width = 16, height = 16)
