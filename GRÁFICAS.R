##GRÁFICAS DESAGREGADAS
datos<-hogaresYrecicladores
datos<-hogares
datos<-recicladores
etiquetasVariable<-get_label(hogares)
etiquetasVariable<-get_label(recicladores)
i= which(names(datos)=='RELA_EXPEC4')
j=which(names(datos)=='encuesta')
desagregador=paste('datos',names(datos)[j],sep = "$")
name=paste('datos',names(datos)[i],sep = "$")
tabla<-((wtd.table(eval(parse(text=name)),eval(parse(text=desagregador)) ,weights = datos$PONDERADOR)))
chisqTest<-chisq.test(tabla)
tabla
chisqTest['p.value']

  tabla<-(prop.table(wtd.table(eval(parse(text=name)),eval(parse(text=desagregador)) ,weights = datos$PONDERADOR),2))
  tabla<-as.data.frame(tabla)
  tabla  
  #tabla<-filter(tabla,Var1=='Sí')
    names(tabla)[names(tabla)=='Var1']<-etiquetasVariable[i]
    names(tabla)[names(tabla)=='Var2']<-'desagregacion'
    names(tabla)[names(tabla)=='Freq']<-'Prop'
    tabla
    #CAMBIAR A MANO
    #p<-ggplot(tabla,aes(x=reorder(tabla[,1],-Prop), y=Prop, fill= Vivienda))
    #tabla<-arrange(tabla,Vivienda)
    #p<-ggplot(tabla,aes(x= tabla[,1], y=Prop, fill=str_wrap(Vivienda,width = 20)))
    p<-ggplot(tabla,aes(x=tabla[,1], y=Prop, fill=desagregacion))
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
      guides(fill=guide_legend(nrow = 1,byrow = TRUE))+
      geom_text(aes(label=percent(round(Prop,2))),vjust=-0.1,hjust=0.4, size =3,position = position_dodge(0.9))+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 50))+
      #HOGARES VS RECICLADORES
      scale_fill_manual(values=c('#00A2BF','#E84E0F','#FDC944','#878787','#009640','#3C3C3B'))
      #NSE
      #scale_fill_manual(values=c('#caa036','#FDC944','#fdd97c'))
      #SEXO
    #scale_fill_manual('Sexo encuestado',values=c('#FDC944', '#990099'))
      #scale_fill_manual(values=c('#003039','#00515f','#008198','#19abc5','#66c7d8','#b2e3eb'))
      #scale_fill_manual(values=c('#5c1f06','#b93e0c','#e84e0f','#ee8357','#f5b89f'))
    #chiquita
    w=8
    h=10
    #ANCHA
    w=12
    h=9
    ggsave(paste("./graficas/recicladores y hogares/",colnames(datos)[i],desagregador, ".png",sep =""), width = w, height = h, units = "cm")
    
