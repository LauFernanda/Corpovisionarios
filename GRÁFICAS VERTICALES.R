setwd('./Dropbox/Corpovisionarios/UAESP/')
##GRÁFICAS UNIVARIADAS
datos<-recicladoresR
datos<-hogares
etiquetasVariable<-get_label(datos)
nombres<-names(datos)
i= which(names(datos)=='RS_MOTIV1')
name=paste('datos',names(datos)[i],sep = "$")

tabla<-(prop.table(wtd.table(eval(parse(text=name)),weights = datos$PONDERADOR)))
tabla<-as.data.frame(tabla)
names(tabla)[names(tabla)=='Var1']<-etiquetasVariable[i]
names(tabla)[names(tabla)=='Freq']<-'Prop'
tabla
write_clip(tabla)
p<-ggplot(tabla,aes(x= reorder(tabla[,1],Prop), y=Prop))

p+geom_bar( fill = "#00A2BF",stat='identity')+ labs(x= str_wrap(names(tabla)[1],width = 40))+coord_flip()+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold", family = 'Museo Sans 700',colour = '#3C3C3B'),
        #axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5, family = 'Museo Sans 300',colour = '#3C3C3B',size = 8, face = 'bold' ),
        axis.text.y = element_text(hjust = 0.5, vjust=0.5, family = 'Museo Sans 300',colour = '#3C3C3B',size = 10, face = 'bold' ),
        legend.text = element_text(family = 'Museo Sans 300',colour = '#3C3C3B'))+
  geom_text(aes(label=percent(round(Prop,2))),vjust=-0.5, hjust=-0.01, size=4,family = 'Museo Sans 500',colour = '#3C3C3B')+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
nombreGrafica<-nombres[i]
ggsave(paste("./graficas/hogares/recodificadas/",nombreGrafica, ".png",sep =""), width = 20, height = 10, units = "cm")
