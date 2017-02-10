base<-"2016-12-28ECC_nacional_desde_2008.Rda"
matrizECC(base="./2017-01-25ECC_nacional_desde_2008.Rda", diccionario = "./documentacion/diccionarioNacional.csv",
          muestras = "./documentacion/20160616 Muestras ECC para promedio nacional.csv",
          desagregar = c("Sexo","Edad_cat","Estra_cat"),excluir=c("Annos","Educa_a"))
matrizECC<-function (base, diccionario, muestras, desagregar,excluir){
  #Carga paquetes necesarios
  library(tidyr)
  library(dplyr)
  library(sjmisc)
  library(reshape2)
  library(xlsx)

# Carga la base de datos
#Lee la base dependiento del formato 
  if(grep(".Rda",base)==1){
    x<-(load(base))
    datos<-to_label(get(x))
    rm(x)
    } else if(grep(".csv",base)==1){
    datos<-read.csv2(base)
  } else if (grep(".sav",base)==1){
    datos<-to_label(read_spss(base,enc="UTF-8",attach.var.labels=TRUE))
  } else{print("La base de datos está en un formato no permitido")}

#Lee los archivos externos (diccionario y información sobre las muestras)
  
  etiquetasVariable<-read.csv(diccionario)
  etiquetasVariable<-etiquetasVariable[,2]
  datos<-set_label(datos,etiquetasVariable)
  
  muestras<-read.csv(muestras)
  i <- sapply(muestras, is.factor)
  muestras[i] <- lapply(muestras[i], as.character)
  incluidas<-(select(filter(muestras,incluida=="Incluída"),medicion))
  incluidas<-incluidas$medicion
  tamanoExpandido<-select(muestras,medicion,tamanno_expandido)
  
  #Lee los nombres y etiquetas de las variables
  etiquetasVariable<-lapply(etiquetasVariable, as.character)
  nombres <- names(datos)
  nombres1<- lapply(nombres, as.symbol)
  
  #Crea tabla de salida
  tablaFinal<-data.frame()
  fecha<-Sys.Date()
  #Bucle para frecuencias sin desagregación
  for(i in 10:227){
    #i<-10
    if(nombres[i] %in% excluir){
      next
    } else if(i>198&&i<225&&i!=211){
      matriz1<-datos
    } else{matriz1<-datos[!is.na(datos[i]),]  }
    
    #Añadir al vector nombres2 los números de las columnas que se quieran usar para agrupar la base de datos   
    nombres2<-nombres1[c(3,i)]
    grupos<-group_by_(matriz1,.dots=nombres2)
    ##cambiar primer par?metro SUM por nombre variable ponderador
    tabla<-summarise(grupos,sumpon=sum(Ponderador, na.rm=TRUE))
    tabla1<-mutate(tabla,Porcentaje=sumpon/sum(sumpon, na.rm=TRUE))
    tabla2<-as.data.frame(tabla1)
    ##Quitar la columna correspondiente a la suma de ponderadores, que en adelante es irrelevante
    tabla3<-tabla2[,-3] 
    tabla4<-mutate(tabla3,Pregunta=etiquetasVariable[[i]])
    tabla4<-mutate(tabla4,NombreVariable=nombres[i])
    ##Renombrar y ordenar columnas
    names(tabla4)[2]<-"OpcionesRespuesta"
    ##Cambiar "P_1" por variable categ?rica que define los grupos
    tabla4<-tabla4[c("NombreVariable","Pregunta","medicion","OpcionesRespuesta","Porcentaje")]  
    #tabla5<-spread(tabla4,medicion,Porcentaje,fill=0,drop = FALSE,convert = TRUE)
    tabla5<-dcast(tabla4, NombreVariable+Pregunta+OpcionesRespuesta~ medicion,fill=0,drop=FALSE)
    #Cómo incluir porcentajes de todas pero sólo calcular con algunas el nacional???
    
    tablafiltrada<-filter(tabla4,medicion %in% incluidas)
    if(class(tablafiltrada$medicion)=="numeric"){tablafiltrada$medicion<-as.integer(tablafiltrada$medicion)}
    tabla6<-left_join(tablafiltrada,tamanoExpandido, by="medicion")
    tabla7<-mutate(tabla6,PorcentajeXExpandido=Porcentaje*tamanno_expandido)
    tabla8<-mutate(tabla7, ExpandidoParaDenominador= ifelse(Porcentaje== 0,0,tamanno_expandido))
    grupos2<-group_by(tabla8,OpcionesRespuesta)
    tabla9<-summarise(grupos2,numerador=sum(PorcentajeXExpandido, na.rm=TRUE))
    tabla10<-summarise(grupos2,denominador=sum(ExpandidoParaDenominador, na.rm=TRUE))
    promedioNacional<-merge(tabla9,tabla10)
    promedioNacional<-mutate(promedioNacional,promedioCiudadesMedidas=numerador/denominador)
    promedioNacional2<-select(promedioNacional,OpcionesRespuesta,promedioCiudadesMedidas)
    tablaCiudadesyNacional<-left_join(tabla5,promedioNacional2,by="OpcionesRespuesta")
    tablaFinal<-rbind(tablaFinal,tablaCiudadesyNacional)
  }
  #Bucle para frecuencia con desagregadores
  for(j in 1:length(desagregar)){
    #j<-2
    desagregador<-desagregar[j]
    excluir1<-c(excluir,desagregador)
    nDes<-as.numeric(which(nombres==desagregador))
    tablaDesagregacion<-data.frame()
    files<-vector()
    #Cálculo de la matriz de frecuencias para el desagregador
    for(i in 10:227){
     # i<-20
      ##A?adir al vector nombres2 los n?meros de las columnas que se quieran usar para agrupar la base de datos   
      if(nombres[i] %in% excluir1){
        next
      } else if(i>198&&i<225&&i!=211){
        matriz1<-datos
      } else{matriz1<-datos[!is.na(datos[i]),]  }
      
      nombres2<-nombres1[c(3,nDes,i)]
      matriz1<-datos[!is.na(datos[i]),]
      grupos<-group_by_(matriz1,.dots=nombres2)
      ##cambiar primer par?metro SUM por nombre variable ponderador
      tabla<-summarise(grupos,sumpon=sum(Ponderador, na.rm=TRUE))
      tabla1<-mutate(tabla,Porcentaje=sumpon/sum(sumpon, na.rm=TRUE))
      tabla2<-as.data.frame(tabla1)
      ##Quitar la columna correspondiente a la suma de ponderadores, que en adelante es irrelevante
      tabla3<-select(tabla2, -sumpon) 
      tabla4<-mutate(tabla3,Pregunta=etiquetasVariable[[i]])
      tabla4<-mutate(tabla4,NombreVariable=nombres[i])
      ##Renombrar y ordenar columnas
      names(tabla4)[3]<-"OpcionesRespuesta"
      tabla4<-tabla4[c("NombreVariable","Pregunta","medicion",desagregador,"OpcionesRespuesta","Porcentaje")]  
      formula<-as.formula(paste("NombreVariable+Pregunta+",desagregador,"+OpcionesRespuesta~ medicion",sep = ""))
      tabla5<-dcast(tabla4, formula,fill=0,drop=FALSE)
      
      #Cómo incluir porcentajes de todas pero sólo calcular con algunas el nacional???
      tablafiltrada<-filter(tabla4,medicion %in% incluidas)
      if(class(tablafiltrada$medicion)=="numeric"){tablafiltrada$medicion<-as.integer(tablafiltrada$medicion)}
      tabla6<-left_join(tablafiltrada,tamanoExpandido, by="medicion")
      tabla7<-mutate(tabla6,PorcentajeXExpandido=Porcentaje*tamanno_expandido)
      tabla8<-mutate(tabla7, ExpandidoParaDenominador= ifelse(Porcentaje== 0,0,tamanno_expandido))
      nombres3<-(c(desagregador,"OpcionesRespuesta"))
      grupos2<-group_by_(tabla8,.dots=nombres3)
      tabla9<-summarise(grupos2,numerador=sum(PorcentajeXExpandido, na.rm=TRUE))
      tabla10<-summarise(grupos2,denominador=sum(ExpandidoParaDenominador, na.rm=TRUE))
      promedioNacional<-merge(tabla9,tabla10)
      promedioNacional<-mutate(promedioNacional,promedioCiudadesMedidas=numerador/denominador)
      promedioNacional2<-select(promedioNacional,-numerador,-denominador)
      tablaCiudadesyNacional<-left_join(tabla5,promedioNacional2)
      tablaDesagregacion<-rbind(tablaDesagregacion,tablaCiudadesyNacional)    
      #primerasColumnas<-tablaDesagregacion[1:4]
    }
    #eval(parse(text = paste("categorias<-levels(tablaDesagregacion$",desagregador,")",sep = "")))
    #for(k in 1:length(categorias)){
      #k<-3
     # eval(parse(text=paste("tablaDesagregacion1<-filter(tablaDesagregacion,",desagregador,"=='",categorias[k],"')",sep="")))  
      #tablaDesagregacion1<-tablaDesagregacion1[-1:-3]
      #namesCat<-paste(k,names(tablaDesagregacion1),categorias[k],sep = "_")
      #names(tablaDesagregacion1)<-namesCat
      #file<-gsub(" a ","_de_",paste(desagregador,categorias[k],sep=""))
      #file<-gsub("-","a",file)
      #files<-c(files,file)
      #assign(file,tablaDesagregacion1, envir= .GlobalEnv)
    #}
    #eval(parse(text = paste("comparativa<-bind_cols(",paste(files,collapse = ","),")",sep="")))
    #comparativa<-comparativa[sort(names(comparativa))]
    #primerasColumnas<-mutate(primerasColumnas, n=1)
    #eval(parse(text = paste("primerasColumnas<-spread(primerasColumnas,",desagregador,",n)",sep = "")))
    #primerasColumnas<-primerasColumnas[,1:3]
    #comparativa<-cbind(primerasColumnas,comparativa)
    file<-paste("./",fecha,"matriz_moduloComun", desagregador,".csv",sep="")
    #write.csv2(comparativa,file)
    write.csv2(tablaDesagregacion,file)
  }
  fecha<-Sys.Date()
  file<-paste("./",fecha,"matriz_moduloComun",".csv",sep="")
  write.csv2(tablaFinal,file)
  }