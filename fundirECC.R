fundirECC<-function(nacional,paraFundir,inconsistencias, ciudad){
  #Carga de paquetes necesarios 
  library(sjmisc)
  library(xlsx)
  library(dplyr)
  
  #Lee los archivos necesarios  
  if(grep(".Rda",nacional)==1){
    x<-(load(nacional))
    nacional<-get(x)
    rm(x)
  } else if(grep(".csv",nacional)==1){
    nacional<-read.csv2(nacional)
  } else if (grep(".sav",nacional)==1){
    nacional<-to_label(read_spss(nacional,enc="UTF-8",attach.var.labels=TRUE))
  } else{print("La base de datos está en un formato no permitido")}
  
  paraFundir<-read_spss(paraFundir,enc = "UTF-8",attach.var.labels=TRUE)
  inconsistencias<-read.xlsx(inconsistencias,3)
  
  #Eliminar módulo adicional, identificación y preguntas otro, cuál identificadas en el documento de inconsistencias
  eliminar<-select(filter(inconsistencias,acciones=="eliminar"),variable)
  eliminar<-as.character(eliminar[,1])
  diferencia<-setdiff(eliminar,names(paraFundir))
  
  for (i in seq(diferencia)){
    variable<-which(eliminar==diferencia[i])
    #variable<-as.symbol(quitar[i])
    eliminar<-eliminar[-variable]
  }
  
  for (i in seq(eliminar)){
    variable<-which(names(paraFundir)==eliminar[i])
    #variable<-as.symbol(quitar[i])
    paraFundir<-paraFundir[,-variable]
  }
  
  variablesParaFundirNoNacional<-sort(setdiff(names(paraFundir),names(nacional)))
  variablesParaCrear<-sort(setdiff(names(nacional),names(paraFundir)))
  
  #Crear las variables que siempre se crean (sociodemográficas recodificadas)
  
  paraFundir$Joven_Adulto<-as.numeric(cut(paraFundir$Annos,breaks=c(-Inf, 25,Inf)))
  paraFundir$Joven_Adulto<-set_labels(paraFundir$Joven_Adulto,c("Jovenes (14-25)", "Adultos (26 o más)"))  
  
  paraFundir$Edad_cat<-as.numeric(cut(paraFundir$Annos,breaks=c(-Inf, 17,25,35,45,55,Inf)))
  paraFundir$Edad_cat<-set_labels(paraFundir$Edad_cat,c("Menor a 18", "18-25","26-35","36-45","46-55","Mayor a 55"))  
  
  paraFundir$Estra_cat<-as.numeric(cut(paraFundir$Estra,breaks=c(-Inf, 2,4,Inf)))
  paraFundir$Estra_cat<-set_labels(paraFundir$Estra_cat,c("Bajo", "Medio", "Alto")) 
  if("Manzana" %in% names(paraFundir)){
    paraFundir$Manzana<-as.character(paraFundir$Manzana)                              
  }
  
  #Labels completos a las numéricas en paraFundir (Mientras se modifica el diccionario)
  attributes(paraFundir$satf_vida)<-NULL
  attr(paraFundir$satf_vida,"label")<-"Qué tan satisfecho se siente usted con su vida considerada en conjunto"
  etiquetas<-c(1,2,3,4,5,6,7,8,9,10)
  names(etiquetas)<-c("Muy insatisfecho","2","3","4","5","6","7","8","9","Muy satisfecho")
  attr(paraFundir$satf_vida,"labels")<-etiquetas
  
  attributes(paraFundir$P_seg_cal)<-NULL
  attr(paraFundir$P_seg_cal,"label")<-"¿De 1 a 5, qué calificación le da a esta ciudad en seguridad?"
  etiquetas<-c(1,2,3,4,5)
  names(etiquetas)<-c("Muy insegura","2","3","4","Muy segura")
  attr(paraFundir$P_seg_cal,"labels")<-etiquetas
  
  attributes(paraFundir$Imp_apub)<-NULL
  attr(paraFundir$Imp_apub,"label")<-"Qué tan importantes son los asuntos públicos en su vida"
  etiquetas<-c(1,2,3,4,5,6,7,8,9,10)
  names(etiquetas)<-c("Poco importantes","2","3","4","5","6","7","8","9","Muy importantes")
  attr(paraFundir$Imp_apub,"labels")<-etiquetas
  
  ##Crea la variable de ciudad
  cuantasCiudades<-length(attr(nacional$medicion,"labels"))
  paraFundir$medicion<-cuantasCiudades+1
  etiquetas<-cuantasCiudades+1
  names(etiquetas)<-ciudad
  attr(paraFundir$medicion,"labels")<-etiquetas
  
  #Modifica la variables Ciudad en la base consolidada para incluir la ciudad nueva
  ciudadesViejas<-names(attr(nacional$medicion,"labels"))
  attr(nacional$medicion,"labels")<-c(attr(nacional$medicion,"labels"),cuantasCiudades+1)
  names(attr(nacional$medicion,"labels")) = c(ciudadesViejas,ciudad)
  
  #Imprime las bases en el global Environment
  assign('paraFundir',paraFundir,envir=.GlobalEnv)
  assign('nacional', nacional,envir=.GlobalEnv)
}