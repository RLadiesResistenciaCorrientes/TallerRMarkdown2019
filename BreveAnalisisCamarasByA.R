install.packages("easypackages")  #para llamar a varias librerias al mismo tiempo
library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont","plotly", "treemapify") 

# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")
# Se realiza por única vez
font_import()
fonts()
loadfonts(device = "win")
#------------------------------------------------------------------------------------------
#Lectura y limpieza
#------------------------------------------------------------------------------------------
datosParlamento <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")
View(datosParlamento)
write.csv(datos_uip,"datosParlamento.csv")
#datosParlamento <-readr::read_csv("datosParlamento.csv")
dim(datosParlamento)
head(datosParlamento)
tail(datosParlamento)

datosParlamento <- clean_names(datosParlamento)
#names(datosParlamento)[6] = "masJoven"
#names(datosParlamento)[7] = "nroIntegrantes"
names(datosParlamento)[8] = "codPais"
# valores mundiales sin año
mundo<- datosParlamento%>% group_by(codPais, pais)%>% summarise(totalporcenM=(sum(porcentaje_mujeres,na.rm = TRUE)),totalMujeres=round((totalporcenM*10), 0), totalIntegrantes=(sum(numero_integrantes, na.rm = TRUE)),totalporcenH=(100- totalporcenM))%>%arrange(totalporcenM)
View(mundo)
#--------------------------------------------------------------------------------------------------
#CÁMARA BAJA O UNICAMERAL
#--------------------------------------------------------------------------------------------------
#Agrupo por cámara baja o única sin año

camaraBaja<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="baja" | camara =="única")%>%
  summarise(totalPorcenCBaja=(sum(porcentaje_mujeres,na.rm = TRUE)), totalIntegrantesCBaja=(sum(numero_integrantes, na.rm = TRUE)))%>%arrange(totalPorcenCBaja)
View(camaraBaja)

#Panamá aparece sin datos pero en la fuente original si los tienen

# selecciono sólo los que tienen representacion en cámara baja o única
camaraBajaSi <-camaraBaja %>%filter(totalPorcenCBaja>0)
View(camaraBajaSi)

camaraBajaLatam <- filter(camaraBajaSi, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
View(camaraBajaLatam)

#sólo para mejorar la visualización
camaraBajaLatam[camaraBajaLatam$pais == "República Dominicana",2]<-"Rep.Dominicana"

#países sin representacion en camara baja: Afghanistán, Benin, España,	Micronesia,Indonesia,India, Nigeria,Panamá, Papúa Nueva Guinea,Vanuatu
paisesSinMujeresCBaja <- camaraBaja%>% filter (totalPorcenCBaja ==0)
View(paisesSinMujeresCBaja)
#--------------------------------------------------------------------------------------------------
#CÁMARA ALTA O SENADO
#--------------------------------------------------------------------------------------------------
#Agrupo por cámara alta o senado
camaraAlta<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="alta")%>%
  summarise(totalPorcenCAlta=(sum(porcentaje_mujeres,na.rm = TRUE)), totalIntegrantesCAlta=(sum(numero_integrantes, na.rm = TRUE)))%>%arrange(totalPorcenCAlta)
View(camaraAlta)

# selecciono sólo los que tienen representacion en cámara baja o única
camaraAltaSi <-camaraAlta %>%filter(totalPorcenCAlta>0)
View(camaraAltaSi)

camaraAltaLatam <- filter(camaraAltaSi, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))%>% arrange(desc(totalPorcenCAlta))
View(camaraAltaLatam)


#países sin representacion en camara alta: 
paisesSinMujeresCAlta <- camaraAlta%>% filter (totalPorcenCAlta ==0)
View(paisesSinMujeresCAlta)

#Paleta colores
pL<-lacroix_palette("PassionFruit", n = 19, type = "continuous")
p<-lacroix_palette("PassionFruit", n = 580, type = "continuous")
pM<-lacroix_palette("PassionFruit", n = 189, type = "continuous")
p1<-lacroix_palette("Pamplemousse",n=580, type = "continuous")
pL1<-lacroix_palette("Pamplemousse",n = 19, type = "continuous")
pM1<-lacroix_palette("Pamplemousse",n=189, type = "continuous")

p2<-lacroix_palette("PeachPear", n = 580, type = "continuous")
pL2<-lacroix_palette("PeachPear", n = 19, type = "continuous")
pL22<-lacroix_palette("PeachPear", n = 12, type = "continuous")
pM2<-lacroix_palette("PeachPear", n = 189, type = "continuous")

p3<-lacroix_palette("PeachPear",n = 19, type = "paired")

#------------------------------------------------------------------------------------------------------------------------------
# Gráficos Cámara baja - # primero los publicados
#------------------------------------------------------------------------------------------------------------------------------
#GGanimate en black PUBLICADO
#---------------------------------------
mujeresLATAMOrdenada <- ggplot(data =camaraBajaLatam, aes((reorder(pais, totalPorcenCBaja)), y=totalPorcenCBaja, fill=pais, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  geom_bar(stat="identity", position=position_dodge()) +              #aes(reorder(pais, totalPorcenCBaja)
  scale_colour_manual(values =pL) +  
  scale_fill_manual(values =pL) +
  labs (x = "", y = "Porcentaje", 
        title= ("Mujeres en los parlamentos \n de Países de Latinoamérica" ),
        subtitle= ("Cámara baja o única"),
        caption = " Fuente: #DatosdeMiercoles por Patricia Loto", legend=" ") +
  geom_text(aes(y = totalPorcenCBaja,label = totalPorcenCBaja),
            position = position_stack(), size=2.5, vjust=2, hjust=0.5 ,col="white")+ # agrego título al gráfico
  theme (axis.text.x =element_text(angle=90, vjust = 1, hjust=0.8, color="white", size=11),
         axis.text.y= element_text(color="white", size=11),
         plot.title = element_text(family="Palatino",
                                   # size=rel(1), 
                                   size = 14,
                                   vjust=1.4, 
                                   hjust=0.5,                        
                                   # position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="white", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         plot.subtitle = element_text(hjust = 0.5, color="white"),
         plot.caption = element_text(color = "green", face = "bold", size = 9),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "black", color = "black"))+
  ylim(0,60)
mujeresLATAMOrdenada
ggsave("mujeresLATAMOrdenada.png",width = 10, height = 5, dpi = "retina")

#plotly Puede publicarse es hermoso
ggplotly(mujeresLATAMOrdenada, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)
mujeresLATAMOrdenada + transition_states(totalPorcenCBaja, wrap = FALSE) +
  shadow_mark()+
  enter_grow() +
  enter_fade()

#------------------------------------------------------------------------------------------------------------------------------
# con GGANIMATE con porcentaje de mujeres en camara Baja o única latinoamerica fondo beige paleta:PeachPear
mujeresCBajaLATAM2 <- ggplot(data =camaraBajaLatam, aes(reorder(pais, totalPorcenCBaja), y=totalPorcenCBaja, fill=pais,label="TRUE", text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  geom_bar(size=2, stat="identity", position=position_dodge()) +             #aes(reorder(pais, totalPorcenCBaja),
  #scale_colour_manual(values =pL2) +  
  scale_fill_manual(values =pL2) +
  labs (x = "", y = "Porcentaje", 
        title= ("Mujeres en los parlamentos \n de Países de Latinoamérica" ),
        subtitle= ("Cámara baja o única"),
        caption = " Fuente: #DatosdeMiercoles", legend=" ") + # agrego título al gráfico
  theme (axis.text.x =element_text(angle=90, vjust = 1, hjust=0.8, color="black", size=11),
         plot.title = element_text(family="Georgia",
                                   # size=rel(1), 
                                   size = 14,
                                   vjust=1.4, 
                                   hjust=0.5,                        
                                   # position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="black", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(color = "darkblue", face = "bold", size = 9),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "beige", color = "beige"))+
  ylim(0,60)#+
#text = element_text(family = "Palatino", colour = "black", size = 14))+

mujeresCBajaLATAM2
ggsave("mujeresCBajaLATAM2.png",width = 10, height = 5, dpi = "retina")
#para publicar
mujeresCBajaLATAM2 + transition_states(totalPorcenCBaja, wrap = FALSE) +
  shadow_mark()+
  enter_grow() +
  enter_fade()

#------------------------------------------------------------------------------------------------------------------------------
# con PLOTLY con porcentaje de mujeres en camara Baja o única - colores por defecto

mujeresCBajalat <- ggplot(data =camaraBajaLatam, aes(x=(pais), y=totalPorcenCBaja, colour=pais, fill=totalPorcenCBaja, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  geom_bar(size=2, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =pL) +  
  #scale_fill_manual(values =pL) +
  labs (x = "País", y = "Porcentaje", 
        title= ("Mujeres en los parlamentos \n de Países de Latinoamérica" ),
        caption = " Fuente: #DatosdeMiercoles", legend=" ") + # agrego título al gráfico
  theme (axis.text.x =element_blank(),
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(#family="Comic Sans MS",
           # size=rel(1), 
           size = 14,
           vjust=1, 
           hjust=0.5,                        #Para separarlo del gráfico
           #position_identity(center),   
           face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
           color="white", #Color del texto  color=maroon, lightblue
           lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "pink", color = "pink"),
         text = element_text(family = "Palatino", colour = "white", size = 14))
mujeresCBajalat
#plotly
ggplotly(mujeresCBajalat, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)
#--------------------------------------------------------
#formato Ejes
xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = '')

#--------------------------------------------------------------------------------------------------------------------------------
#PLOTLY - ok para publicar
p<- plot_ly (x = camaraBajaLatam$pais, y = camaraBajaLatam$totalPorcenCBaja, color = camaraBajaLatam$pais, text = paste('<b>País:</b>', camaraBajaLatam$pais,'\n <b> Mujeres:</b>', camaraBajaLatam$totalPorcenCBaja, '%' ), 
             hoverinfo = "text", type = "bar") %>% layout(title= 'Mujeres en el Parlamento', legend= '',
                                                          xaxis = list(showline = F, 
                                                                       showticklabels = F, 
                                                                       fixedrange = T,
                                                                       showlegend =TRUE,
                                                                       title = "País"),
                                                          yaxis = list(fixedrange = T, 
                                                                       title = "Porcentaje"))

p

#------------------------------------------------------------------------------------------------------------------------------
# con GGANIMATE con porcentaje de mujeres en camara Baja o única latinoamerica 
#fondo blanco PUBLICAR y paleta passionfruit
#------------------------------------------------------------------------------------------------------------------------------
mujeresCBajaLATAM <- ggplot(data =camaraBajaLatam, aes((pais), y=totalPorcenCBaja, fill=pais, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  geom_bar(size=2, stat="identity", position=position_dodge()) +              #aes(reorder(pais, totalPorcenCBaja)
  #scale_colour_manual(values =pL) +  
  scale_fill_manual(values =pL) +
  labs (x = "", y = "Porcentaje", 
        title= ("Mujeres en los parlamentos \n de Países de Latinoamérica"  ),
        subtitle= ("Cámara baja o única"),
        caption = " Fuente: #DatosdeMiercoles por Patricia Loto", legend=" ") + # agrego título al gráfico
  theme (axis.text.x =element_text(angle=90, vjust = 1, hjust=0.8, color="black", size=10),
         plot.title = element_text(family="Palatino",
                                   # size=rel(1), 
                                   size = 14,
                                   vjust=1.4, 
                                   hjust=0.5,                        
                                   # position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="black", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(color = "green", face = "bold", size = 9),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "white", color = "white"))+
  ylim(0,60)
mujeresCBajaLATAM

mujeresCBajaLATAM + transition_states(totalPorcenCBaja, wrap = FALSE) +
  shadow_mark()+
  enter_grow() +
  enter_fade()


#------------------------------------------------------------------------------------------------------------------------------
#plotly con coord_flip passsionfruit
#-----------------------------------------------------------------------------------------
# camara baja LATAM
Latam <- ggplot(camaraBajaLatam, aes(reorder(pais, totalPorcenCBaja), totalPorcenCBaja, size =(totalPorcenCBaja), text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  #geom_point(show.legend = F) + 
  geom_col(aes(fill=pais)) +
  scale_fill_manual(values =pL22) +  #pL2
  coord_flip()+ 
  theme_classic()+   #theme_wsj()theme_classic() +theme_economist()
  labs(title = "Mujeres en los parlamentos \n de Países de Latinoamérica" ,
       subtitle = "Período:2019",
       x = "País",
       y = "Porcentaje",
       caption="#DatosDeMiercoles por Patricia Loto", legend=" ") +
  theme(axis.text.x =element_text(color="black", size=9),  #
        axis.text.y =element_text(color="black", size=9),
        plot.caption = element_text(color = "lightblue", face ="bold", size = 10, vjust=1),  ##562457
        plot.title = element_text(size=10,
                                  family = "Palatino",
                                  #size=rel(0.4),
                                  vjust=2,
                                  hjust=0.5,
                                  #position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.position= " ", legend.text= element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 11))
Latam
ggplotly(Latam, hoverformat='2.F', tooltip = "text")

#theme(aspect.ratio=1)
Latam + transition_time(importaXP) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)
# ------------------------------------------------------------------------------- 
#gganimate

ggplot(camaraAlta, aes(totalPorcenCAlta, totalIntegrantesCAlta, size = totalPorcenCAlta, colour = pais)) +
  geom_point(alpha = 1, show.legend = TRUE) +
  scale_colour_manual(values = p2) +
  #scale_size(range = c(2, 12)) +
  scale_x_log10() +
  theme(legend.position = " " , legend.text= element_blank())

# facet_grid(~pais) +
# Here comes the gganimate specific bits
labs(title = '"Mujeres en los parlamentos \n de Países de Latinoamérica" : round({frame_time},2)', x = 'Porcentaje Participación', y = 'Total Integrantes') +
  transition_time(totalPorcenCBaja) +
  shadow_mark(alpha = 1, size = 2)
# ease_aes('linear')


#------------------------------------------------------------------------------------------------------------------------------
# Gráficos Cámara alta - Publicado
#------------------------------------------------------------------------------------------------------------------------------
# camara alta LATAM
LatamAlta <- ggplot(camaraAltaLatam, aes(reorder(pais, totalPorcenCAlta), totalPorcenCAlta, size =(totalPorcenCAlta))) + 
  #geom_point(show.legend = F) + 
  geom_col(aes(fill=pais)) +
  scale_fill_manual(values =pL2) +  #pL2
  coord_flip()+ 
  theme_classic()+   #theme_wsj()theme_classic() +theme_economist()
  labs(title = "Mujeres en los parlamentos de Países de Latinoamérica\n Cámara Alta o Senado",
       subtitle = "Al 1ro de enero de 2019",
       x = "",
       y = "% Mujeres",
       caption="#DatosDeMiercoles por Patricia Loto", legend=" ") +
  geom_text(aes(label = totalPorcenCAlta),
            position = position_stack(), size=2.5, vjust=1.5, hjust=1.5 ,col="black")+
  theme(axis.text.x =element_text(color="black", size=9),  #
        axis.text.y =element_text(color="black", size=9),
        plot.caption = element_text(color = "brown", face ="bold", size = 10, vjust=1),  ##562457
        plot.title = element_text(size=10,
                                  family = "Palatino",
                                  vjust=2,
                                  hjust=0.5,
                                  #position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.position= " ", legend.text= element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 11))
#+  xlim(0,60)

LatamAlta

ggplotly(LatamAlta, hoverformat='2.F', tooltip = "text")

#theme(aspect.ratio=1)
Latam + transition_time(importaXP) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)
#--------------------------------------------------------------------------------------
# camara baja LATAM
LatamA <- ggplot(camaraAltaLatam, aes(reorder(pais, totalPorcenCAlta), totalPorcenCAlta, size =(totalPorcenCAlta), text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCAlta, '%' ))) + 
  #geom_point(show.legend = F) + 
  geom_col(aes(fill=pais)) +
  scale_fill_manual(values =pL22) +  #pL2
  coord_flip()+ 
  theme_classic()+ 
  facet_grid(~ pais)+
  #theme_wsj()theme_classic() +theme_economist()
  labs(title = "Mujeres en los parlamentos \n de Países de Latinoamérica" ,
       subtitle = "Período: Al 1ro de enero de 2019",
       x = "",
       y = "Porcentaje",
       legend=" ",
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(color="black", size=9),  #
        axis.text.y =element_text(color="black", size=9),
        plot.caption = element_text(color = "lightblue", face ="bold", size = 10, vjust=1),  ##562457
        plot.title = element_text(size=10,
                                  family = "Palatino",
                                  #size=rel(0.4),
                                  vjust=2,
                                  hjust=0.5,
                                  #position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.title = "", legend.position= " ", legend.text= element_blank())

LatamA
ggplotly(LatamA, hoverformat='2.F', tooltip = "text", showlegend =FALSE)

#theme(aspect.ratio=1)
Latam + transition_time(importaXP) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)

#plotly
p<- plot_ly (reorder(camaraAltaLatam$pais, camaraAltaLatam$totalPorcenCAlta), y = camaraAltaLatam$totalPorcenCAlta, color = camaraAltaLatam$pais,  text = paste('Lealtad',camaraAltaLatam$pais, ":",camaraAltaLatam$totalPorcenCAlta), 
             hoverinfo = "text", type = "bar") %>% layout(title= 'Temporada de la 1 a la 6', legend= 'Personaje',
                                                          xaxis = list(showline = F, 
                                                                       showticklabels = F, 
                                                                       fixedrange = T,
                                                                       showlegend =TRUE,
                                                                       title = "Lealtad"),
                                                          yaxis = list(fixedrange = T, 
                                                                       title = ""))


p







