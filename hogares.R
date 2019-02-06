#ENCUESTA HOGARES
#PREPARACIÓN
library(ggplot2)
library(tidyverse)
library(sjlabelled)
library(sjmisc)
library(naniar)
library(visdat)
library(corrplot)
library(reshape2)
library(Hmisc)
library(questionr)
library(extrafont)
library(stringr)
library(scales)
library(haven)
library(googlesheets)

#IMPORTAR  FUENTES
font_import()
font_import(c('./fuentes/MuseoSans_500.ttf'))

#LEER DATOS
setwd('~/Dropbox/Corpovisionarios/UAESP')
hogares<-to_label(read_spss('./UAESP_HOGARES.sav'),add.non.labelled = TRUE)
Numhogares<-read_spss('./UAESP_HOGARES.sav')

#CREAR VARIABLES
#Crear las variables que siempre se crean (sociodemográficas recodificadas)

hogares$JOVEN_ADULTO<-as.factor(cut(hogares$EDAD,breaks=c(-Inf, 25,Inf)))
levels(hogares$JOVEN_ADULTO)<-c("Jovenes (14-25)", "Adultos (26 o más)")
set_label(hogares$JOVEN_ADULTO)<-'Edad'
hogares$EDAD_CAT<-as.factor(cut(hogares$EDAD,breaks=c(-Inf, 17,25,35,45,55,Inf)))
levels(hogares$EDAD_CAT)<-c("Menor de 18 años", "18-25","26-35","36-45","46-55","Mayor de 55 años")  
set_label(hogares$EDAD_CAT)<-'Edad'
hogares$ESTRA_CAT<-as.factor(cut(hogares$ESTRATO,breaks=c(-Inf, 2,4,Inf)))
levels(hogares$ESTRA_CAT)<-c("Bajo", "Medio", "Alto")
set_label(hogares$ESTRA_CAT)<-'Nivel_socioeconómico'

hogares$RS_PRAC6_CAT<-as.factor(cut(hogares$RS_PRAC6,breaks=c(-Inf, 17,25,35,45,55,Inf)))
levels(hogares$RS_PRAC6_CAT)<-c("Menor de 18 años", "18-25","26-35","36-45","46-55","Mayor de 55 años")  
set_label(hogares$RS_PRAC6_CAT)<-'Edad de la persona que más se preocupa por la separación de residuos en el hogar'

hogares$RS_PRAC9_CAT<-as.factor(cut(hogares$RS_PRAC9,breaks=c(-Inf, 17,25,35,45,55,Inf)))
levels(hogares$RS_PRAC9_CAT)<-c("Menor de 18 años", "18-25","26-35","36-45","46-55","Mayor de 55 años")  
set_label(hogares$RS_PRAC9_CAT)<-'Edad de la persona que saca la basura del hogar'

#Arreglar  etiquetas equivocadas
malasEtiquetas<-c()

for (i in 1:ncol(hogares)){
  name=paste('hogares',names(hogares)[i],sep = "$")
  
  if('Completament e en desacuerdo' %in% levels(eval(parse(text=name)))){
    malasEtiquetas<-c(malasEtiquetas,name)
  } 
} 

malasEtiquetas
levels(hogares$IA_CREE6)[4]<-'Completamente en desacuerdo'

str(hogares$RS_CONO33)
hogares$RS_CONO33<-set_label(hogares$RS_CONO33,label = 'El camión de basura se lleva todas las bolsas sin distinguir el color')
str(hogares$RS_CONO34)
hogares$RS_CONO34<-set_label(hogares$RS_CONO34,label = 'Los recicladores revisan todas las bolsas de basura sin importar el color')
hogares$T_VIVIENDA<-set_label(hogares$T_VIVIENDA,label = 'Vivienda')

hogares$RS_PRAC12[hogares$RS_PRAC12=='NA']<-NA
hogares$RS_PRAC12<- droplevels(hogares$RS_PRAC12)
hogares$RS_PRAC13[hogares$RS_PRAC13=='NA']<-NA
hogares$RS_PRAC13<- droplevels(hogares$RS_PRAC13)
table(hogares$RS_PRAC12)

#REVISAR NAS
vis_dat(hogares)
vis_miss(hogares)
gg_miss_var(hogares)
missvar<-miss_var_summary(hogares)
miss_case_table(hogares)
remove<-c()
for (i in 1:ncol(hogares)){
  if(sum(is.na(hogares[,i]))>=nrow(hogares)){
    remove<-paste(remove,i,sep = ',-')
    }
}

hogares<-hogares[,c(-27,-41,-52,-60,-70,-89,-94,-101,-111,-116,-121,-126,-148,-159,-173,-187)]
Numhogares<-Numhogares[,c(-27,-41,-52,-60,-70,-89,-94,-101,-111,-116,-121,-126,-148,-159,-173,-187)]
Numhogares1<-Numhogares

#REVISAR DUPLICADOS
duplicados<-duplicated(hogares) | duplicated(hogares[nrow(hogares):1, ])[nrow(hogares):1]
VERDADEROS<-duplicados[duplicados==TRUE]

#Guardar Base
file<-"./UAESP_hogares_corregida.Rda"
save(hogares, file=file)
#file<-"./UAESP_hogares_corregida.sav"
#write_spss(hogares,file)

#RECODIFICAR VARIABLES PARA AGRUPAR NIVELES
datos1<-hogares
nInicio<-8
nFinal<-166
excluir<- nombres[c(168:177)]
#correr recodificacionVariables.R

file<-"./UAESP_hogares_recodificada.Rda"
save(datos1, file=file)
hogaresR<-datos1

#CREAR MATRICES
#correlaciones
tipos<-sapply(Numhogares, class) 
which(tipos!='numeric')
Numhogares<-Numhogares[,c(-1:-7,-14,-17,-26,-39,-78,-167:-177)]

Numhogares<-Numhogares[ , order(names(Numhogares))]
namesNumhogares<-names(Numhogares)

#names(Numhogares)<- c(1:154)
correlaciones<-round(cor(Numhogares),4)
melted_correlaciones <- filter(melt(correlaciones))

ggplot(data = melted_correlaciones, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
res2 <- rcorr(as.matrix(Numhogares))
res2$P
##REVISIÓN CORRELACIONES PREGUNTAS CONFUSAS

#eDAD REPETIDA
edadrepetida<-length(intersect(datos$RS_PRAC6,datos$EDAD))
edadrepetida2<-length(intersect(datos$RS_PRAC9,datos$EDAD))
edadrepetida3<-length(intersect(datos$RS_PRAC9, datos$RS_PRAC6))
#correlaciones individuos NO VALE LA PENA


#hogaresTrans <- as.data.frame(t(Numhogares1[,-1]))
#hogaresTrans <- data.frame(lapply(hogaresTrans, function(x) as.numeric(as.character(x))))
# tipos<-sapply(hogaresTrans, class) 
# which(tipos!='numeric')
# correlacionesPersonas<-round(cor(hogaresTrans,use = 'pairwise.complete.obs'),4)
# melted_correlacionesPersonas <- filter(melt(correlacionesPersonas),value!=1)

#RECODIFICAR CATEGORÍAS PEQUEÑAS
levels(hogaresR$T_VIVIENDA)<-c(levels(hogaresR$T_VIVIENDA),'Otro tipo de vivienda')
hogaresR$T_VIVIENDA[hogaresR$T_VIVIENDA=='Cuarto/pieza/habitación con renta diaria']<-'Otro tipo de vivienda'
hogaresR$T_VIVIENDA[hogaresR$T_VIVIENDA=='Cuarto/pieza/ habitación con renta mensual']<-'Otro tipo de vivienda'
hogaresR$T_VIVIENDA[hogaresR$T_VIVIENDA=='Otro tipo de vivienda (carpa, tienda, vagón, refugio natural, puente, etc.).']<-'Otro tipo de vivienda'

hogaresR$T_VIVIENDA<- droplevels(hogaresR$T_VIVIENDA)
hogaresR$T_VIVIENDA = factor(hogaresR$T_VIVIENDA,levels(hogaresR$T_VIVIENDA)[c(3,4,2,1,5)])
table(hogaresR$T_VIVIENDA)

levels(hogares$T_VIVIENDA)<-c(levels(hogares$T_VIVIENDA),'Otro tipo de vivienda')
hogares$T_VIVIENDA[hogares$T_VIVIENDA=='Cuarto/pieza/habitación con renta diaria']<-'Otro tipo de vivienda'
hogares$T_VIVIENDA[hogares$T_VIVIENDA=='Cuarto/pieza/ habitación con renta mensual']<-'Otro tipo de vivienda'
hogares$T_VIVIENDA[hogares$T_VIVIENDA=='Otro tipo de vivienda (carpa, tienda, vagón, refugio natural, puente, etc.).']<-'Otro tipo de vivienda'
hogares$T_VIVIENDA<- droplevels(hogares$T_VIVIENDA)
hogares$T_VIVIENDA = factor(hogares$T_VIVIENDA,levels(hogares$T_VIVIENDA)[c(3,4,2,1,5)])
table(hogares$T_VIVIENDA)


hogaresR$T_VIVIENDA<- droplevels(hogaresR$T_VIVIENDA)
hogaresR$T_VIVIENDA = factor(hogaresR$T_VIVIENDA,levels(hogaresR$T_VIVIENDA)[c(3,4,2,1,5)])
table(hogaresR$T_VIVIENDA)

levels(hogaresR$RS_PRAC1)<-c(levels(hogaresR$RS_PRAC1),'2 o más recipientes')
#levels(hogares$RS_PRAC1)<-c(levels(hogares$RS_PRAC1),'2 o más recipientes')
hogaresR$RS_PRAC1[hogares$RS_PRAC1>=2]<-'2 o más recipientes'
hogares$RS_PRAC1[hogares$RS_PRAC1>=2]<-'2 o más recipientes'

table(hogaresR$RS_PRAC1)
hogaresR$RS_PRAC1<-droplevels(hogaresR$RS_PRAC1)
#CREAR GRÁFICAS FRECUENCIAS SIMPLES
datos<-hogaresR

# PREPARACIÓN DATOS
nombres<-names(datos)
#datos<-set_label(datos,etiquetas)
etiquetasVariable<-get_label(datos)

#GRÁFICAS
which(names(datos)=='RELA_ACTOR7')
nInicio<-211
nFinal<-211
excluir<- nombres[c(168:177)]
#tabla<-read_clip_tbl()
for(i in nInicio:nFinal){
    # i<-158
    if(nombres[i] %in% excluir){
      next
    }else if(sum(is.na(datos[,i]))>=nrow(datos)){
      next
    }else {
      name=paste('datos',names(datos)[i],sep = "$")  
      tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),weights = datos$PONDERADOR)))
      names(tabla)[names(tabla)=='Var1']<-etiquetasVariable[i]
      names(tabla)[names(tabla)=='Freq']<-'Total'
      #tabla<-filter(tabla,Total>=0.0001)
      p<-ggplot(tabla,aes(x=tabla[,1], y=Total))
      p+geom_bar( fill = "#00A2BF",stat='identity')+ labs(x= str_wrap('En general, ¿Cómo es su relación con otros recicladores?',width = 50))+
        theme(panel.background = element_rect(fill = "white", colour = "white"),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text=element_text(size=12),
              axis.title=element_text(size=12,face="bold", family = 'Museo Sans 700',colour = '#3C3C3B'),
              axis.text.x = element_text( hjust = 0.5, vjust=0, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8,face="bold"  ),
              legend.text = element_text(family = 'Museo Sans 300',colour = '#3C3C3B'))+
        geom_text(aes(label=percent(round(Total,3))),vjust=-0.1,hjust=0.4,size=3,family = 'Museo Sans 300',colour = '#3C3C3B',face="bold")+
        scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
      ggsave(paste("./graficas/recicladores/recodificadas/",colnames(datos)[i], ".png",sep =""), width = 9, height = 8, units = "cm")
    }}

#Con etiquetas invertidas

variables_invertidas<- c(which(nombres=='N_EDUCATIVO'),which(nombres=='OCUPACION'),which(nombres=='T_VIVIENDA'),which(nombres=='EDAD_CAT'))
variables_invertidas<- c(which(nombres=='RELA_EXPEC5'),which(nombres=='RELA_EXPEC6'),which(nombres=='RS_CONO30'))
variables_invertidas<- c(which(nombres=='RELA_NORM1'),which(nombres=='RELA_NORM2'),which(nombres=='RELA_NORM3'),which(nombres=='RELA_NORM4'))
variables_invertidas<- c(which(nombres=='RS_MOTIV1'),which(nombres=='RS_MOTIV2'))
variables_invertidas<- c(which(nombres=='RS_PRAC10'))
for(i in variables_invertidas){
  # i<-13
  if(nombres[i] %in% excluir){
    next
  }else if(sum(is.na(datos[,i]))>=nrow(datos)){
    next
  }else {
    name=paste('datos',names(datos)[i],sep = "$")  
    tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),weights = datos$PONDERADOR)))
    names(tabla)[names(tabla)=='Var1']<-etiquetasVariable[i]
    names(tabla)[names(tabla)=='Freq']<-'Total'
    #tabla<-filter(tabla,Total>=0.0001)
    #PARA ORDENAR
    tabla<- tabla[order(-tabla$Total),] 
    p<-ggplot(tabla,aes(x=reorder(tabla[,1],-Total), y=Total))
    #p<-ggplot(tabla,aes(x=tabla[,1], y=Total))
    p+geom_bar( fill = "#00A2BF",stat='identity')+ labs(x= str_wrap(etiquetasVariable[i],width = 50))+
      theme(panel.background = element_rect(fill = "white", colour = "white"),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold", family = 'Museo Sans 700',colour = '#3C3C3B'),
            axis.text.x = element_text( angle=90,hjust = 0.5, vjust=0.5, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8,face="bold" ),
            legend.text = element_text(family = 'Museo Sans 300',colour = '#3C3C3B'))+
      geom_text(aes(label=percent(round(Total,3))),vjust=-0.1,hjust=0.4,size=3,family = 'Museo Sans 300',colour = '#3C3C3B',face="bold")+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
    ggsave(paste("./graficas/hogares/recodificadas/",colnames(datos)[i], ".png",sep =""), width = 11, height = 10, units = "cm")
  }}

#VARIAS VARIABLES EN UNA SÓLA GRÁFICA

variablesSimilares<- grep('IA_CONRES',nombres)
variablesSimilares
#variablesSimilares<-variablesSimilares[c(2:4,13,14)]
variablesSimilares<-c(213,138)
#variablesSimilares<-c(variablesSimilares,166)

tablaMultivariada<-data.frame()
#datos<-hogaresR
datos<-recicladoresR
for(i in variablesSimilares){
  # i<-136
  if(nombres[i] %in% excluir){
    next
  }else if(sum(is.na(datos[,i]))>=nrow(datos)){
    next
  }else {
    name=paste('datos',names(datos)[i],sep = "$")  
    tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),weights = datos$PONDERADOR)))
    #CAMBIAR
    #tabla2<-filter(tabla,Var1!='No')
    tabla2<-filter(tabla,Var1=='Muchísimo o mucho')
    #tabla2$nombre<-etiquetasVariable[i]
    #tabla2<-tabla2[,-1]
    tablaMultivariada<-rbind(tablaMultivariada,tabla2)
  }
}

tablaMultivariada
names(tablaMultivariada)[names(tablaMultivariada)=='nombre']<-'% que respondió afirmativamente'
names(tablaMultivariada)[names(tablaMultivariada)=='Freq']<-'Total'
####ORDENAR  
tablaMultivariada<- tablaMultivariada[order(-tablaMultivariada$Total),] 
  #tabla<-filter(tabla,Total>=0.0001)
  p<-ggplot(tablaMultivariada,aes(x= reorder(tablaMultivariada[,2],-Total), y=Total))
    p+geom_bar( fill = "#00A2BF",stat='identity')+ labs(x= str_wrap(names(tablaMultivariada)[2],width = 70))+
      theme(panel.background = element_rect(fill = "white", colour = "white"),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold", family = 'Museo Sans 700',colour = '#3C3C3B'),
            #axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8, face = 'bold' ),
            axis.text.x = element_text(hjust = 0.5, vjust=0, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8, face = 'bold' ))+
            #legEend.text = element_text(family = 'Museo Sans 300',colour = '#3C3C3B'))+
      geom_text(aes(label=percent(round(Total,2))),vjust=-0.1,hjust=0.4,family = 'Museo Sans 300',colour = '#3C3C3B')+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
    #p+coord_flip()
    nombreGrafica<-'expectativas'
    ggsave(paste("./graficas/hogares/recodificadas/",nombreGrafica, ".png",sep =""), width = 8, height = 9, units = "cm")

#IDENTIFICAR DIFERENCIAS SIGNIFICATIVAS
#USAR LA BASE REOCODIFICADA

datos<- hogaresR
#Fisher test para cada desagregador
j=12
desagregador=paste('datos',names(datos)[j],sep = "$")
which(nombres=='RS_PRAC1')
nInicio=18
nFinal=18
difSignificativas<-c()
for(i in nInicio:nFinal){
  # i<-158
  if(nombres[i] %in% excluir){
    next
  }else if(sum(is.na(datos[,i]))>=nrow(datos)){
    next
  }else if (i==j){
    next
    }else {
    name=paste('datos',names(datos)[i],sep = "$")
tabla<-((wtd.table(eval(parse(text=name)),eval(parse(text=desagregador)) ,weights = datos$PONDERADOR)))
chisqTest<-chisq.test(tabla)

if( !is.na(chisqTest['p.value'])&&chisqTest['p.value']<0.05){
  tabla<-(prop.table(wtd.table(eval(parse(text=name)),eval(parse(text=desagregador)) ,weights = datos$PONDERADOR),2))
  if ( (max(tabla[1,])- min(tabla[1,]))>=0.03){
    difSignificativas<-c(difSignificativas,names(datos)[i])
    #colnames(tabla) <- dimnames(tabla)[[2]]
    #colnames(tabla)[3]<-'Total'
    #tabla<-tabla[-nrow(tabla),]
    tabla<-as.data.frame(tabla)
    tabla<-filter(tabla,Var1=='2 o más recipientes')
    names(tabla)[names(tabla)=='Var1']<-etiquetasVariable[i]
    names(tabla)[names(tabla)=='Var2']<-etiquetasVariable[j]
    names(tabla)[names(tabla)=='Freq']<-'Prop'
    #CAMBIAR A MANO
    #p<-ggplot(tabla,aes(x=reorder(tabla[,1],-Prop), y=Prop, fill= Vivienda))
    tabla<-arrange(tabla,Vivienda)
    #p<-ggplot(tabla,aes(x= tabla[,1], y=Prop, fill=str_wrap(Vivienda,width = 20)))
    
    p<-ggplot(tabla,aes(x=tabla[,1], y=Prop, fill=Vivienda))
    p+geom_bar(stat='identity', position=position_dodge())+ labs(x= str_wrap(etiquetasVariable[i],width = 50))+
      theme(panel.background = element_rect(fill = "white", colour = "white"),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold", family = 'Museo Sans 700',colour = '#3C3C3B'),
            axis.text.x = element_text( hjust = 0.5, vjust=0, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8),
            #axis.text.x = element_text( angle=90,hjust = 0, vjust=0.5, family = 'Museo Sans 300',colour = '#3C3C3B' ),
            legend.text = element_text(family = 'Museo Sans 300',colour = '#3C3C3B'),
            legend.position = 'bottom',
            legend.title = element_blank())+
      guides(fill=guide_legend(nrow = 3,byrow = TRUE))+
      geom_text(aes(label=percent(round(Prop,2))),vjust=-0.1,hjust=0.4,position = position_dodge(0.9), size=3)+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 50))+
      #scale_fill_manual(values=c('#00A2BF','#E84E0F','#FDC944','#878787','#009640','#3C3C3B'))
      #scale_fill_manual(values=c('#caa036','#FDC944','#fdd97c'))
      #scale_fill_manual(values=c('#003039','#00515f','#008198','#19abc5','#66c7d8','#b2e3eb'))
      scale_fill_manual(values=c('#5c1f06','#b93e0c','#e84e0f','#ee8357','#f5b89f'))
    ggsave(paste("./graficas/hogares/desagregadas/",colnames(datos)[i],desagregador, ".png",sep =""), width = 14, height = 10, units = "cm")
    
  }
  }
  }}

etiquetasVariable[74]<-'¿Por su casa pasa algún reciclador?'
etiquetasVariable[12]<-'Vivienda'
#VARIAS VARIABLES EN UNA SÓLA GRÁFICA DESAGREGADA
datos<-hogaresR
nombres<-names(datos)
etiquetasVariable<-get_label(hogaresR)

consumoResponsable<-grep('IA_CONRES',nombres)
creenciasAmbientales<-grep('IA_CREE',nombres)
confianzas<-grep('RELA_CONF',nombres)
expectativas<-grep('RELA_EXPEC',nombres)[1:2]
regAutoridad<-grep('RELA_NORM',nombres)[5:8]
regCiudadanos<-grep('RELA_NORM',nombres)[9:12]
jdl<-grep('RELA_NORM',nombres)[13:23]
representacionesRecicladores1<-grep('RELA_REPRE',nombres)[1:5]
representacionesRecicladores2<-grep('RELA_REPRE',nombres)[6:9]
tolerancias1<-grep('RELA_REPRE',nombres)[10:20]
tolerancias2<-grep('RELA_REPRE',nombres)[21:30]
percepcionOtro<-grep('RELA_REPRE',nombres)[31:33]
percepcionOtro<-c(percepcionOtro,166)
conocimientos1<-grep('RS_CONO',nombres)[1:9]
conocimiento2<-grep('RS_CONO',nombres)[10:18]
conocimientos3<-grep('RS_CONO',nombres)[19:26]
conocimientos4<-grep('RS_CONO',nombres)[32:35]
repre<-grep('RELA_REPRE',nombres)[32:33]
pracSeparacion<-grep('RS_PRAC',nombres)[c(2:3,13:14)]
which(names(recicladores)=='RELA_ACTOR7')
variablesSimilares<-c(208,210,211)

#variablesSimilares<-consumoResponsable
#variablesSimilares<-creenciasAmbientales
#variablesSimilares<-confianzas
#variablesSimilares<-expectativas
#variablesSimilares<-regAutoridad
#variablesSimilares<-regCiudadanos
#variablesSimilares<-jdl
#variablesSimilares<-tolerancias1
variablesSimilares<-tolerancias2
#variablesSimilares<-representacionesRecicladores1
#variablesSimilares<-representacionesRecicladores2
#variablesSimilares<-percepcionOtro
#variablesSimilares<-conocimientos1
#variablesSimilares<-conocimiento2
# variablesSimilares<-conocimientos3
#variablesSimilares<-conocimientos4
#variablesSimilares<-repre
#variablesSimilares<-pracSeparacion


variablesSimilares


#variablesSimilares<-variablesSimilares[c(1:8)]
#variablesSimilares<-c(variablesSimilares,166)

j=180
desagregador=paste('datos',names(datos)[j],sep = "$")

tablaMultivariada<-data.frame()

for(i in variablesSimilares){
  # i<-40
  if(nombres[i] %in% excluir){
    next
  }else if(sum(is.na(datos[,i]))>=nrow(datos)){
    next
  }else {
    name=paste('datos',names(datos)[i],sep = "$")  
    tabla<-(as.data.frame(prop.table(wtd.table(eval(parse(text=name)),eval(parse(text=desagregador)) ,weights = datos$PONDERADOR),2)))
    #CAMBIAR
    #tabla<-filter(tabla,Var1=='Bolsa blanca')
    #tabla<-filter(tabla,Var1=='No sé')
    #tabla<-filter(tabla,Var1=='Sí')
    tabla<-filter(tabla,Var1!='No')
    #tabla<-filter(tabla,Var1=='Completamente de acuerdo o de acuerdo')
    #tabla<-filter(tabla,Var1=='Muchísimo o mucho')
    #tabla<-filter(tabla,Var1=='Siempre o casi siempre')
    tabla$Var1<-etiquetasVariable[i]
    names(tabla)[names(tabla)=='Var2']<-etiquetasVariable[j]
    names(tabla)[names(tabla)=='Freq']<-'Prop'
    tablaMultivariada<-rbind(tablaMultivariada,tabla)
  }
}

tablaMultivariada
#tablaMultivariada<-tablaMultivariada[c(-13,-15),]
#tablaMultivariada[11:15,1]<-"¿En su trabajo usted separa los residuos reciclables de los no reciclables?"
#tablaMultivariada[16:20,1]<-"¿En centros comerciales usted separa los residuos reciclables de los no reciclables?"

#tablaMultivariada<-tablaMultivariada[c(4:6,10:12,16:18),]
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Prácticas de consumo responsable (% Sí)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Creencias y actitudes medioambientales (% Completamente de acuerdo o de acuerdo)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Confianza institucional (% que confía muchísimo o mucho)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'% que contestó afirmativamente'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-' En su ciudad, los ciudadanos que incurren en los siguientes comportamientos, ¿son corregidos por las autoridades? (% Siempre o casi siempre)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-' En su ciudad, los ciudadanos que incurren en los siguientes comportamientos, ¿son corregidos por los ciudadanos? (% Siempre o casi siempre)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Justificaciones para desobedecer la ley (% que justifica)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Representaciones de recicladores (% Sí)'
names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'% que no le gustaría tener como vecino a: '
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'¿Cuáles de los siguientes elementos arroja en la bolsa de residuos reciclables?'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'¿Cuáles de los siguientes elementos cree usted que son reciclables?'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Por favor indique en qué bolsa debería ir cada uno de los siguientes materiales (% Bolsa blanca)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Por favor indique en qué bolsa debería ir cada uno de los siguientes materiales (% No sé)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Conocimientos sobre el sistema de reciclaje en Bogotá (% Completamente de acuerdo o de acuerdo)'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'% que contestó afirmativamente'
#names(tablaMultivariada)[names(tablaMultivariada)=='Var1']<-'Prácticas de separación en la fuente (% Sí)'

names(tablaMultivariada)[names(tablaMultivariada)=='Prop']<-'Total'
names(tablaMultivariada)[2]<-c('Encuesta')
####ORDENAR  
#tablaMultivariada<- tablaMultivariada[order(-tablaMultivariada$Total),] 
#tablaMultivariada<-filter(tablaMultivariada, Total >=0.0001)
#tablaT<-tablaMultivariada
#tablaMultivariada<-tablaT[c(-13,-15),]
#tablaMultivariada<-arrange(tablaMultivariada,Encuesta)
p<-ggplot(tablaMultivariada,aes(x= reorder(tablaMultivariada[,1],-Total), y=Total, fill=Encuesta))
#p<-ggplot(tablaMultivariada,aes(x= tablaMultivariada[,1], y=Total, fill= str_wrap(Encuesta,width = 30)))
#p<-ggplot(tablaMultivariada,aes(x= reorder(tablaMultivariada[,1],c(1:20)), y=Total, fill= str_wrap(Encuesta,width = 20)))
#p<-ggplot(tablaMultivariada,aes(x=ordered(tablaMultivariada[,1], levels=, y=Total, fill= Encuesta))
p+geom_bar(stat='identity', position=position_dodge())+ labs(x= str_wrap(names(tablaMultivariada)[1],width = 50))+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold", family = 'Museo Sans 700',colour = '#3C3C3B'),
        axis.text.x = element_text(hjust = 0.5, vjust=0, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8 ),
        #axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5, family = 'Museo Sans 300',colour = '#3C3C3B' ),
        legend.text = element_text(family = 'Museo Sans 300',colour = '#3C3C3B',size = 8),
        legend.title = element_blank(),
        #legend.title = element_text(family = 'Museo Sans 300',colour ='#3C3C3B', face = "bold"),
        legend.position = 'bottom')+
  #guides(fill=guide_legend(title="Nivel socioeconómico"))+
  geom_text(aes(label=percent(round(Total,2))),vjust=-0.1,hjust=0.4,position = position_dodge(0.9), size=2.7)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18))+
  #scale_fill_manual(values=c('#00A2BF','#E84E0F','#FDC944','#878787','#009640','#3C3C3B'))
  #NSE
  scale_fill_manual(values=c('#caa036','#FDC944','#fdd97c'))
  #edad
  #scale_fill_manual(values=c('#003039','#00515f','#008198','#19abc5','#66c7d8','#b2e3eb'))
  #vivienda
  #scale_fill_manual(values=c('#5c1f06','#b93e0c','#e84e0f','#ee8357','#f5b89f'))
  #Encuesta
  #scale_fill_manual(values=c('#00A2BF','#E84E0F'))
  #SEXO
  #scale_fill_manual('Sexo encuestado',values=c('#FDC944', '#990099'))

#nombreGrafica<-'Prácticas de consumo responsable1'
#nombreGrafica<-'Creencias ambientales'
#nombreGrafica<-'Confianza institucional'
#nombreGrafica<-'Expectativas'
#nombreGrafica<-'Regulación autoridades'
#nombreGrafica<-'Regulación ciudadanos'
#nombreGrafica<-'Justificaciones para desobedecer la ley'
#nombreGrafica<-'Representaciones recicladores1'
#nombreGrafica<-'Representaciones recicladores2'
#nombreGrafica<-'Tolercancias1'
nombreGrafica<-'Tolercancias2'
#nombreGrafica<-'Representación del otro'
#nombreGrafica<-'Conocimienos1'
#nombreGrafica<-'Conocimienos2'
#nombreGrafica<-'Conocimienos3'
#nombreGrafica<-'Conocimienos4'
#nombreGrafica<-'Conocimienos5'
#nombreGrafica<-'Representaciones'
#nombreGrafica<-'Prácticas separación'
ggsave(paste("./graficas/hogares/",nombreGrafica,desagregador, ".png",sep =""), width = 21, height = 9, units = "cm")



#tabla<-filter(tabla,Total>=0.0001)

