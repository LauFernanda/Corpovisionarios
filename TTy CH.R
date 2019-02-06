options(OutDec= ".") 
datos<-to_label(read_spss( file = './BASE_FINAL_CHEMONIC_TT_30122018.sav'),add.non.labelled = TRUE)
head(datos$PONDERADOR)
str(datos$PONDERADOR)

#REVISAR NAS
vis_dat(datos,warn_large_data = FALSE)
vis_miss(datos,warn_large_data = FALSE)
gg_miss_var(datos)
write_clip(miss_var_summary(datos))
miss_case_table(datos)
write_clip(miss_case_table(datos))
datos$ID<- 1:nrow(datos) 
datos1<-add_n_miss(datos)
datos1<-select(datos1,ID, n_miss_all)
write_clip(datos1)

write_clip(filter(datos,ID==4213))
##REVISAR DICCIONARIO
##etiquetas de victimización no corresponden
##etiqueta de formulario no existe, no encuentro ID Id_reg no corresponde
etiquetasVariable<-get_label(datos)
str(datos$d_9_mp_11_g_2)

##REVISAR TAMAÑOS EXPANDIDOS
options(OutDec= ".") 
tamaños<-aggregate(datos$PONDERADOR, by=list(Category=datos$COD_munimunicipio), FUN=sum)
write_clip(tamaños)

##Nuevas  variables
table(datos$COD_munimunicipio)
datos$COD_munimunicipio<-to_factor(datos$COD_munimunicipio)
table(datos$COD_munimunicipio)
levels(datos$COD_munimunicipio)<-c("Apartadó", "Santander de Quilichao", "Montelíbano","Puerto Libertador", "Istmina", "Francisco Pizarro", "Tumaco", "Orito","Puerto Asís") 

table (datos$d_0_municentro_poblado)
datos$d_0_municentro_poblado<-fct_recode(datos$d_0_municentro_poblado, Tumaco= "TUMACO")

datos$EDAD_CAT<-as.factor(cut(datos$ANNOS,breaks=c(-Inf, 17,25,35,45,55,Inf)))
levels(datos$EDAD_CAT)<-c("Menores de 18 años", "18 a 25 años","26 a 35 años","36 a 45 años","46 a 55 años","Mayores de 55 años")  
set_label(datos$EDAD_CAT)<-'Edad'

datos$ESTRA_CAT<-as.factor(cut(datos$ESTRA,breaks=c(-Inf, 2,4,Inf)))
levels(datos$ESTRA_CAT)<-c("Bajo", "Medio", "Alto")
set_label(datos$ESTRA_CAT)<-'Nivel_socioeconómico'

datos$Raza2<-datos$RAZA
levels(datos$Raza2)
datos$Raza2[datos$Raza2== "Raizal del archipiélago de San Andrés, Providencia y Santa Catalina"]<-"Negro (a), Mulato (a), Afrodescendiente"
datos$Raza2[datos$Raza2== "Palenquero de San Basilio"]<-"Negro (a), Mulato (a), Afrodescendiente"

levels(datos$Raza2)[3]<-"Negro (a), Mulato (a), Afrodescendiente, Raizal o palenquero"

table(datos$Raza2)
datos$Raza2<-droplevels(datos$Raza2)

datos$PROYECTO<-datos$COD_munimunicipio
datos$PROYECTO<-fct_collapse(datos$PROYECTO, Chemonics= municipiosCH)
datos$PROYECTO<-fct_collapse(datos$PROYECTO, Tetratech= municipiosTT)
table(datos$PROYECTO,datos$COD_munimunicipio)

levels(datos$CONV_VEC_INS)[levels(datos$CONV_VEC_INS)=='NA'] <- NA
levels(datos$CONV_NO_PAGO)[levels(datos$CONV_NO_PAGO)=='NA'] <- NA
datosCHr$MA_ESTADO_2<-datosCH$MA_ESTADO_2
levels(datosCHr$MA_ESTADO_2)<-c(levels(datosCHr$MA_ESTADO_2),"Muy bien  o bien","Mal o muy mal")
datosCHr$MA_ESTADO_2[datosCHr$MA_ESTADO_2 == "Muy bien"]<- "Muy bien  o bien"
datosCHr$MA_ESTADO_2[datosCHr$MA_ESTADO_2 == "Bien"]<- "Muy bien  o bien"
datosCHr$MA_ESTADO_2[datosCHr$MA_ESTADO_2 == "Mal"]<-"Mal o muy mal" 
datosCHr$MA_ESTADO_2[datosCHr$MA_ESTADO_2 == "Muy mal"]<-"Mal o muy mal"
table(datosCHr$MA_ESTADO_2)
datosCHr$MA_ESTADO_2<-droplevels(datosCHr$MA_ESTADO_2)

datosCHr$VICTIMA<-NA
victimizaciones<-which(names(datosCHr) %in% names(datosCHr)[startsWith(names(datosCHr),'VIC_')])
for (i in 1:nrow(datosCHr)){
  victimizacion_fila<-as_vector(datosCHr[i,victimizaciones])
  if("Sí" %in% victimizacion_fila){
    datosCHr$VICTIMA[i]<-"Sí"
  }
  else{
    datosCHr$VICTIMA[i]<-"No"
  }
}

prop.table(table(datosCHr$VICTIMA))
datosCH$VICTIMA<-datosCHr$VICTIMA

datosCH$ESTADO_PROVEE<-NA
PROVISION_ESTADO<-which(names(datosCH) %in% names(datosCH)[startsWith(names(datosCH),'MA_ESTADO_')])
PROVISION_ESTADO<-PROVISION_ESTADO[c(-1,-2)]
for (i in 1:nrow(datosCHr)){
  provision_fila<-as_vector(datosCH[i,PROVISION_ESTADO])
  datosCH$ESTADO_PROVEE[i]<-7-sum(provision_fila=="No")
  }

prop.table(table(datosCH$ESTADO_PROVEE))
datosCH$VICTIMA<-datosCHr$VICTIMA

###PARA INDICADORES SE  USA COMO NA PARA TABLA COMO NIVEL

#datos$CONV_VEC_INS<-fct_explicit_na(datos$CONV_VEC_INS, na_level = "No aplica")
#datos$CONV_NO_PAGO<-fct_explicit_na(datos$CONV_NO_PAGO, na_level = "No aplica")

datos$CONV_VEC_INS<-droplevels(datos$CONV_VEC_INS)
datos$CONV_NO_PAGO<-droplevels(datos$CONV_NO_PAGO)

datos$SEXO<-to_factor(datos$SEXO)
levels(datos$SEXO)[1]<-"Hombre"
levels(datos$SEXO)[2]<-"Mujer"

datos$TOL_ALC<-to_factor(datos$TOL_ALC)
datos$TOL_ALC<-fct_explicit_na(datos$TOL_ALC, na_level = "No seleccionado")
levels(datos$TOL_ALC)[1]<-"No le gustaría tener como vecino"

datos$TOL_DROG<-to_factor(datos$TOL_DROG)
datos$TOL_DROG<-fct_explicit_na(datos$TOL_DROG, na_level = "No seleccionado")
levels(datos$TOL_DROG)[1]<-"No le gustaría tener como vecino"

datos$TOL_HOM<-to_factor(datos$TOL_HOM)
datos$TOL_HOM<-fct_explicit_na(datos$TOL_HOM, na_level = "No seleccionado")
levels(datos$TOL_HOM)[1]<-"No le gustaría tener como vecino"

datos$TOL_PROS<-to_factor(datos$TOL_PROS)
datos$TOL_PROS<-fct_explicit_na(datos$TOL_PROS, na_level = "No seleccionado")
levels(datos$TOL_PROS)[1]<-"No le gustaría tener como vecino"

datos$TOL_SID<-to_factor(datos$TOL_SID)
datos$TOL_SID<-fct_explicit_na(datos$TOL_SID, na_level = "No seleccionado")
levels(datos$TOL_SID)[1]<-"No le gustaría tener como vecino"

datos$TOL_RELDIF<-to_factor(datos$TOL_RELDIF)
datos$TOL_RELDIF<-fct_explicit_na(datos$TOL_RELDIF, na_level = "No seleccionado")
levels(datos$TOL_RELDIF)[1]<-"No le gustaría tener como vecino"

datos$TOL_NACDIF<-to_factor(datos$TOL_NACDIF)
datos$TOL_NACDIF<-fct_explicit_na(datos$TOL_NACDIF, na_level = "No seleccionado")
levels(datos$TOL_NACDIF)[1]<-"No le gustaría tener como vecino"

datos$TOL_NAR<-to_factor(datos$TOL_NAR)
datos$TOL_NAR<-fct_explicit_na(datos$TOL_NAR, na_level = "No seleccionado")
levels(datos$TOL_NAR)[1]<-"No le gustaría tener como vecino"

datos$TOL_POLI<-to_factor(datos$TOL_POLI)
datos$TOL_POLI<-fct_explicit_na(datos$TOL_POLI, na_level = "No seleccionado")
levels(datos$TOL_POLI)[1]<-"No le gustaría tener como vecino"

datos$TOL_REGDIF<-to_factor(datos$TOL_REGDIF)
datos$TOL_REGDIF<-fct_explicit_na(datos$TOL_REGDIF, na_level = "No seleccionado")
levels(datos$TOL_REGDIF)[1]<-"No le gustaría tener como vecino"

datos$TOL_CORR<-to_factor(datos$TOL_CORR)
datos$TOL_CORR<-fct_explicit_na(datos$TOL_CORR, na_level = "No seleccionado")
levels(datos$TOL_CORR)[1]<-"No le gustaría tener como vecino"

datos$TOL_COLDIF<-to_factor(datos$TOL_COLDIF)
datos$TOL_COLDIF<-fct_explicit_na(datos$TOL_COLDIF, na_level = "No seleccionado")
levels(datos$TOL_COLDIF)[1]<-"No le gustaría tener como vecino"

datos$TOL_POLT<-to_factor(datos$TOL_POLT)
datos$TOL_POLT<-fct_explicit_na(datos$TOL_POLT, na_level = "No seleccionado")
levels(datos$TOL_POLT)[1]<-"No le gustaría tener como vecino"

datos$TOL_PARA<-to_factor(datos$TOL_PARA)
datos$TOL_PARA<-fct_explicit_na(datos$TOL_PARA, na_level = "No seleccionado")
levels(datos$TOL_PARA)[1]<-"No le gustaría tener como vecino"

datos$TOL_GUE<-to_factor(datos$TOL_GUE)
datos$TOL_GUE<-fct_explicit_na(datos$TOL_GUE, na_level = "No seleccionado")
levels(datos$TOL_GUE)[1]<-"No le gustaría tener como vecino"

datos$TOL_DES<-to_factor(datos$TOL_DES)
datos$TOL_DES<-fct_explicit_na(datos$TOL_DES, na_level = "No seleccionado")
levels(datos$TOL_DES)[1]<-"No le gustaría tener como vecino"

datos$TOL_REIN<-to_factor(datos$TOL_REIN)
datos$TOL_REIN<-fct_explicit_na(datos$TOL_REIN, na_level = "No seleccionado")
levels(datos$TOL_REIN)[1]<-"No le gustaría tener como vecino"

datos$TOL_IND<-to_factor(datos$TOL_IND)
datos$TOL_IND<-fct_explicit_na(datos$TOL_IND, na_level = "No seleccionado")
levels(datos$TOL_IND)[1]<-"No le gustaría tener como vecino"

datos$MA_BIENE_1<-to_factor(datos$MA_BIENE_1)
datos$MA_BIENE_1<-fct_explicit_na(datos$MA_BIENE_1, na_level = "No seleccionado")
levels(datos$MA_BIENE_1)[1]<-"% que lo seleccionó" 

datos$MA_BIENE_2<-to_factor(datos$MA_BIENE_2)
datos$MA_BIENE_2<-fct_explicit_na(datos$MA_BIENE_2, na_level = "No seleccionado")
levels(datos$MA_BIENE_2)[1]<-"% que lo seleccionó" 

datos$MA_BIENE_3<-to_factor(datos$MA_BIENE_3)
datos$MA_BIENE_3<-fct_explicit_na(datos$MA_BIENE_3, na_level = "No seleccionado")
levels(datos$MA_BIENE_3)[1]<-"% que lo seleccionó" 

datos$MA_BIENE_4<-to_factor(datos$MA_BIENE_4)
datos$MA_BIENE_4<-fct_explicit_na(datos$MA_BIENE_4, na_level = "No seleccionado")
levels(datos$MA_BIENE_4)[1]<-"% que lo seleccionó" 

datos$MA_BIENE_5<-to_factor(datos$MA_BIENE_5)
datos$MA_BIENE_5<-fct_explicit_na(datos$MA_BIENE_5, na_level = "No seleccionado")
levels(datos$MA_BIENE_5)[1]<-"% que lo seleccionó" 

datos$MA_BIENE_6<-to_factor(datos$MA_BIENE_6)
datos$MA_BIENE_6<-fct_explicit_na(datos$MA_BIENE_6, na_level = "No seleccionado")
levels(datos$MA_BIENE_6)[1]<-"% que lo seleccionó" 

datos$MA_BIENE_7<-to_factor(datos$MA_BIENE_7)
datos$MA_BIENE_7<-fct_explicit_na(datos$MA_BIENE_7, na_level = "No seleccionado")
levels(datos$MA_BIENE_7)[1]<-"% que lo seleccionó" 

datos$COLOR_PALETA<-to_factor(datos$COLOR_PALETA)
datos$COLOR_PALETA<-fct_explicit_na(datos$COLOR_PALETA, na_level = "No seleccionado")


etiquetasVariable<-get_label(datos)
etiquetasVariable[35]<-"Los hijos de un vecino insultaron o agredieron a sus hijos"
etiquetasVariable[39]<-"No le han pagado lo que le corresponde por su trabajo"


### Datos recodificados
datos1<-datos

original<-datos

recodificada<-datos1

datos<-recodificada
datos<-original

###TABLA
write_clip(names(datos))
write_clip(etiquetasVariable)

tablaCompleta<-data.frame()

#Varias variables  no  tienen las etiquetas correspondientes
j=1
#i=353

for(i in 11:(ncol(datos)-1)){
  name=paste('datos',names(datos)[i],sep = "$")
  nameCH=paste('datosCH',names(datos)[i],sep = "$")
  nameTT=paste('datosTT',names(datos)[i],sep = "$")
  
  if(i %in% c(12:16,18,26:28,114,116,144,255,260,262,263,265,288,290,292,294,296,298,300,302,304,306,308,310,312,314,
              316,318,320,322,324,326,354:372)){
    next
  }else{
    tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),datos$COD_munimunicipio,weights = datos$PONDERADOR,na.show = FALSE),2))
    tabla2<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),weights = datos$PONDERADOR,na.show = FALSE)))
    tabla3<-as.data.frame(prop.table(wtd.table(eval(parse(text=nameCH)), weights = datosCH$PONDERADOR,na.show = FALSE)))
    tabla4<-as.data.frame(prop.table(wtd.table(eval(parse(text=nameTT)), weights = datosTT$PONDERADOR,na.show = FALSE)))
    
    tabla$Pregunta<-paste(j," ",etiquetasVariable[i])
    tabla2$Pregunta<-paste(j," ",etiquetasVariable[i])
    tabla3$Pregunta<-paste(j," ",etiquetasVariable[i])
    tabla4$Pregunta<-paste(j," ",etiquetasVariable[i])
    
    tabla2$Var2<- "Promedio ponderado 9 municipios"
    tabla3$Var2<- "Promedio ponderado Chemonics"
    tabla4$Var2<- "Promedio ponderado Tetratech"
    
    tablaCompleta<-rbind(tablaCompleta,tabla)
    tablaCompleta<-rbind(tablaCompleta,tabla2)
    tablaCompleta<-rbind(tablaCompleta,tabla3)
    tablaCompleta<-rbind(tablaCompleta,tabla4)
    
  }
  j=j+1
}

names(tablaCompleta)
table(tablaCompleta$Var2)
qpt<-(filter(tablaCompleta, Var2 == "Promedio ponderado 9 municipios")[,4]) 
qpTT<-as.character(filter(tablaCompleta, Var2 == "Promedio ponderado Tetratech")[,4])
qpCH<-as.character(filter(tablaCompleta, Var2 == "Promedio ponderado Chemonics")[,4])
setdiff(qpTT,qpt)
dif<-cbind(qpt,qpCH)
head(tablaCompleta)

names(tablaCompleta)<-c('Respuesta','Municipio','Proporción','Pregunta')
tablaCompleta <- tablaCompleta[c(2,4,1,3)]

tablafiltrada<-subset(tablaCompleta, !is.nan(tablaCompleta[,4]))
tablafiltrada<-subset(tablafiltrada, !is.na(tablaCompleta[,4]))
#tablafiltrada<-subset(tablafiltrada, !is.na(tablaCompleta[,3]))
table(tablafiltrada$Municipio)
write_clip(tablafiltrada)
write_clip(tablaCompleta)
##Con desagregadores

municipios<-levels(datos$COD_munimunicipio)

tablaCompleta<-data.frame()
j=1

for(i in 11:(ncol(datos)-1)){
  if(i %in% c(12:16,18,26:28,114,116,144,255,260,262,263,265,288,290,292,294,296,298,300,302,304,306,308,310,312,314,
              316,318,320,322,324,326,354:372)){
    next
  }  else if(i %in% c(346:352)){
    for (m in 1:length(municipios)){
      
      datosMunicipio<-filter(datos, COD_munimunicipio == municipios[m])
      name=paste('datosMunicipio',names(datos)[i],sep = "$")  
      #cambio
      tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),datosMunicipio$ZONAS, weights = datosMunicipio$PONDERADOR,na.show = TRUE),2))
      tabla2<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)), weights = datosMunicipio$PONDERADOR,na.show = TRUE)))
      
      tabla$Pregunta<-paste(j," ",etiquetasVariable[i])
      tabla2$Pregunta<-paste(j," ",etiquetasVariable[i])
      
      tabla$Municipio<-municipios[m]
      tabla2$Municipio<-municipios[m]
      
      tabla2$Var2<- "Promedio municipio"
      
      tablaCompleta<-rbind(tablaCompleta,tabla)
      tablaCompleta<-rbind(tablaCompleta,tabla2)  
    }
    
  }  else{
    for (m in 1:length(municipios)){
      
      datosMunicipio<-filter(datos, COD_munimunicipio == municipios[m])
      name=paste('datosMunicipio',names(datos)[i],sep = "$")  
      #cambio
      tabla<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)),datosMunicipio$ZONAS, weights = datosMunicipio$PONDERADOR),2))
      tabla2<-as.data.frame(prop.table(wtd.table(eval(parse(text=name)), weights = datosMunicipio$PONDERADOR)))
      
      tabla$Pregunta<-paste(j," ",etiquetasVariable[i])
      tabla2$Pregunta<-paste(j," ",etiquetasVariable[i])
      
      tabla$Municipio<-municipios[m]
      tabla2$Municipio<-municipios[m]
      
      tabla2$Var2<- "Promedio municipio"
      if(ncol(tabla)>=3){
        tablaCompleta<-rbind(tablaCompleta,tabla)
        tablaCompleta<-rbind(tablaCompleta,tabla2)    
      }
      
    }
    
  }
  j=j+1
}


names(tablaCompleta)
table(tablaCompleta$Var2)
head(tablaCompleta)
tail(tablaCompleta)

names(tablaCompleta)<-c('Respuesta','Desagregador','Proporción','Pregunta','Municipio')

tablaCompleta_o <- tablaCompleta[c(5,2,4,1,3)]

tablafiltrada<-subset(tablaCompleta_o, !is.nan(tablaCompleta[,4]))
tablafiltrada<-subset(tablafiltrada, !is.na(tablaCompleta[,4]))
#tablafiltrada<-subset(tablafiltrada, !is.na(tablaCompleta[,3]))

write_clip(tablafiltrada)


##INDICADORES##
datos1<-original

completa<-datos1

## Bases proyecto ##
municipiosCH<-c("Apartadó","Santander de Quilichao","Montelíbano","Istmina","Francisco Pizarro", "Orito")
municipiosTT<- c("Puerto Libertador","Tumaco","Puerto Asís")

datosCH<-filter(completa, COD_munimunicipio %in% municipiosCH)
datosTT<-filter(completa, COD_munimunicipio %in% municipiosTT)
datosCHr<-filter(recodificada, COD_munimunicipio %in% municipiosCH)
datosTTr<-filter(recodificada, COD_munimunicipio %in% municipiosTT)

recodificada$IDNV_VIF_STDR<-completa$IDNV_VIF_STDR
## INDICADORES POR MUNICIPIO@@
tablaIndicadores<-data.frame()

municipios_z<-(levels(as.data.frame(table(datos$d_0_municentro_poblado))[,1]))
variablesIndicador<-names(indicadores)[endsWith(names(indicadores),'STDR')]

    for (m in 1:length(municipios_z)){
      datosMunicipio<-filter(indicadores, d_0_municentro_poblado == municipios_z[m])
      fila<- vector()
      for(i in 1:length(variablesIndicador)){  
      name=paste('datosMunicipio',variablesIndicador[i],sep = "$")  
      indicador<-weighted.mean(eval(parse(text=name)), datosMunicipio$PONDERADOR,na.rm=T)
      fila<-c(fila,indicador) 
      }
      tablaIndicadores<-rbind(tablaIndicadores,fila)
    }

fila<-vector()
for(i in 1:length(variablesIndicador)){  
  name=paste('datos1',variablesIndicador[i],sep = "$")  
  indicador<-weighted.mean(eval(parse(text=name)), datos1$PONDERADOR,na.rm=T)
  fila<-c(fila,indicador) 
}
fila
tablaIndicadores<-rbind(tablaIndicadores,fila)
#Tabla indicadores Tableau
tablaIndicadores<-data.frame(stringsAsFactors = FALSE)

variablesIndicador<-names(indicadores)[endsWith(names(indicadores),'STDR')]

municipios_z<-(levels(as.data.frame(table(datos$d_0_municentro_poblado))[,1]))
for (m in 1:length(municipios_z)){
  datosMunicipio<-filter(indicadores, d_0_municentro_poblado == municipios_z[m])
    for(i in 1:length(variablesIndicador)){  
    name=paste('datosMunicipio',variablesIndicador[i],sep = "$")  
    indicador<-weighted.mean(eval(parse(text=name)), datosMunicipio$PONDERADOR,na.rm=T)
    fila<-cbind(municipios_z[m],variablesIndicador[i],indicador)
    tablaIndicadores<-rbind(tablaIndicadores,fila)
  }
  
}

for(i in 1:length(variablesIndicador)){  
  name=paste('datosCH',variablesIndicador[i],sep = "$")
  name2=paste('datosTT',variablesIndicador[i],sep = "$") 
  indicador<-weighted.mean(eval(parse(text=name)), datosCH$PONDERADOR,na.rm=T)
  fila<-cbind("Promedio ponderado Chemonics",variablesIndicador[i],indicador)
  tablaIndicadores<-rbind(tablaIndicadores,fila)
  indicador<-weighted.mean(eval(parse(text=name2)), datosTT$PONDERADOR,na.rm=T)
  fila<-cbind("Promedio ponderado Tetratech",variablesIndicador[i],indicador)
  tablaIndicadores<-rbind(tablaIndicadores,fila)
}

zona<-(levels(as.data.frame(table(datos$Zona))[,1]))

for (m in 1:length(zona)){
  datoszona<-filter(completa, Zona == zona[m])
  for(i in 1:length(variablesIndicador)){  
    name=paste('datoszona',variablesIndicador[i],sep = "$")  
    indicador<-weighted.mean(eval(parse(text=name)), datoszona$PONDERADOR,na.rm=T)
    fila<-cbind(zona[m],variablesIndicador[i],indicador)
    tablaIndicadores<-rbind(tablaIndicadores,fila)
  }
  
}


names(tablaIndicadores)<-c('Municipio','Indicador','Valor')
write_clip(tablaIndicadores)
