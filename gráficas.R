library(questionr)
##GRÁFICAS SIMPLES
i= 18
name=paste('datos',names(datos)[i],sep = "$")  
tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),weights = datos$PONDERADOR)))
names(tabla)[names(tabla)=='Var1']<-etiquetasVariable[i]
names(tabla)[names(tabla)=='Freq']<-'Total'

tabla<-filter(tabla,Total>=0.0001)

p<-ggplot(tabla,aes(x=tabla[,1], y=Total))
p+geom_bar( fill = "#EDA81E",stat='identity')+ labs(x= etiquetasVariable[i])+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(label=percent(round(Total,3))),vjust=-0.1,hjust=0.4)


#Invertidas

p+geom_bar( fill = "#EDA81E",stat='identity')+ labs(x= etiquetasVariable[i])+
  coord_flip()+theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  geom_text(aes(label=percent(round(Total,3))),vjust=-0.1,hjust=0.4)




p<-ggplot(tabla,aes(x=tabla[,1], y=Total, group=1))

p+geom_path(color = "#EDA81E",stat='identity')+ labs(x= etiquetasVariable[i])+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        axis.title.y=element_blank(),
        )

for(i in nInicio:nFinal){
  # i<-23
  if(nombres[i] %in% excluir1){
    next
  }else if(sum(is.na(datos[,i]))>=nrow(datos)){
    next
  }else {
    #matriz1<-datos[!is.na(datos[i]),]
    #PARA INCLUIR NAS
    matriz1<-datos
    nombres2<-nombres1[c(i)]
    grupos<-group_by_(matriz1,.dots=nombres2)
    ##cambiar primer par?metro SUM por nombre variable ponderador
    tabla<-summarise(grupos,sumpon=sum(datos$PONDERADOR, na.rm=TRUE))
    tabla1<-as.data.frame(mutate(tabla,PORCENTAJE=sumpon/sum(sumpon, na.rm=TRUE)))
    ##Quitar la columna correspondiente a la suma de ponderadores, que en adelante es irrelevante
    tabla2<-tabla1[,-2] 
    tabla4<-mutate(tabla3,PREGUNTA=etiquetasVariable[i])
    tabla4<-mutate(tabla4,NOMBRE_VARIABLE=nombres[i])
    ##Renombrar y ordenar columnas
    names(tabla4)[2]<-"OPCIONES_RESPUESTA"
    tabla4<-tabla4[c("NOMBRE_VARIABLE","PREGUNTA",desagregador,"OPCIONES_RESPUESTA","PORCENTAJE")]  
    formula<-as.formula(paste("NOMBRE_VARIABLE+PREGUNTA+","OPCIONES_RESPUESTA~",desagregador,sep = ""))
    tabla5<-dcast(tabla4, formula,fill="",drop=FALSE)
    ##Añadir columna total
    nombres3<-nombres1[i]
    matriz2<-datos[!is.na(datos[i]),]
    grupos2<-group_by_(matriz2,.dots=nombres3)
    ##cambiar primer par?metro SUM por nombre variable ponderador
    #tabla6<-summarise(grupos2,sumpon=sum(PONDERADOR, na.rm=TRUE))
    #tabla7<-mutate(tabla6,Total=sumpon/sum(sumpon, na.rm=TRUE))
    tabla6<-summarise(grupos2,sumpon=sum(PONDERADOR, na.rm=FALSE))
    tabla7<-mutate(tabla6,Total=sumpon/sum(sumpon, na.rm=FALSE))
    tabla8<-as.data.frame(tabla7)
    tabla8<-tabla8[-2]
    names(tabla8)[1]<-"OPCIONES_RESPUESTA"
    tablaFinal<-full_join(tabla5,tabla8)
    p<-ggplot(datos,  aes_string(colnames(datos)[i]))
    p+geom_bar( fill = "#00BFFF")+coord_flip()+ labs(x= etiquetasVariable[i],y='Número de registros')+ geom_text(stat='count',aes(label=scales::percent((..count..)/sum(..count..))),vjust=-0.1,hjust=0.4)
    ggsave(paste("./graficas/hogares/",colnames(datos)[i], ".png",sep =""))
    
    ##Creaci?n hoja de excel particular para cada variablem
    #nameSheet <- as.character(etiquetasVariable[i])
    nameSheet <- as.character(nombres[i])
    #hoja     <- createSheet(wb, sheetName = nameSheet)
    #addDataFrame(tablaFinal, sheet = hoja,
    #            startRow = 1, startColumn = 1,
    #           row.names = FALSE, col.names = TRUE,
    #          colnamesStyle =  esBorde,
    #         colStyle = list('4'=esPorc,'5'=esPorc,'6'=esPorc,'7'=esPorc,'8'=esPorc,'9'=esPorc,'10'=esPorc,'11'=esPorc,'12'=esPorc,'13'=esPorc,'14'=esPorc,'15'=esPorc,'16'=esPorc,'17'=esPorc,'18'=esPorc))
    ##Agregar resultado a la tabla completa
    tablaCompleta<-rbind(tablaCompleta,tablaFinal)
  }
}
nameSheet <- "Matriz"
hoja1     <- createSheet(wb, sheetName = nameSheet)
addDataFrame(tablaCompleta, sheet = hoja1,
             startRow = 1, startColumn = 1,
             row.names = FALSE, col.names = TRUE,
             colnamesStyle =  esBorde,
             colStyle = list('4'=esPorc,'5'=esPorc,'6'=esPorc,'7'=esPorc,'8'=esPorc,'9'=esPorc,'10'=esPorc,'11'=esPorc,'12'=esPorc,'13'=esPorc,'14'=esPorc,'15'=esPorc,'16'=esPorc,'17'=esPorc,'18'=esPorc))

file<-paste("./",directorio,fecha,"matriz_", organizacion,"_", desagregador,".xlsx",sep="")
saveWorkbook(wb, file = file)
}
}