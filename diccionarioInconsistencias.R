diccionarioInconsistencias<-function(nacional,paraFundir, nombreCiudad){
  #Carga paquetes necesarios
  library(sjmisc)
  library(tidyr)
  library(xlsx)
  
  #lee base nacional y construye su diccionario
  nacional<-read_spss(nacional,enc = "UTF-8",attach.var.labels=TRUE)
  nombresNacional<-names(nacional)
  labelsNacional<-get_label(nacional)
  diccionarioNacional<-as.data.frame(cbind(nombresNacional,labelsNacional))
  
  #lee base para fundir  y construye su diccionario
  paraFundir<-read_spss(paraFundir,enc = "UTF-8",attach.var.labels=TRUE)
  nombresParaFundir<-names(paraFundir)
  labelsParaFundir<-get_label(paraFundir)
  diccionarioParaFundir<-as.data.frame(cbind(nombresParaFundir,labelsParaFundir))
  
  #Identifica inconsistencias en los nombres de variables de las dos bases
  eliminar<-setdiff(names(paraFundir),names(nacional))
  crear<-setdiff(names(nacional),names(paraFundir))
  inconsistencias<-as.data.frame(cbind(eliminar,crear))
  inconsistencias<-gather(inconsistencias,"acciones","variable",1:2)
  inconsistencias<-unique(inconsistencias)
  ##diccionario completo
  ##Guardar en excel
  wb <- createWorkbook()
  nameSheet <- paste ("diccionario", nombreCiudad)
  hoja     <- createSheet(wb, sheetName = nameSheet)
  addDataFrame(diccionarioParaFundir, sheet = hoja,
               startRow = 1, startColumn = 1,
               row.names = FALSE, col.names = TRUE)
  
  nameSheet <- "diccionarioNacional"
  hoja     <- createSheet(wb, sheetName = nameSheet)
  addDataFrame(diccionarioNacional, sheet = hoja,
               startRow = 1, startColumn = 1,
               row.names = FALSE, col.names = TRUE)
  
  nameSheet <- "inconsistencias"
  hoja     <- createSheet(wb, sheetName = nameSheet)
  addDataFrame(inconsistencias, sheet = hoja,
               startRow = 1, startColumn = 1,
               row.names = FALSE, col.names = TRUE)
  
  file<- paste ("diccionario", nombreCiudad,".xlsx",sep="")
  saveWorkbook(wb, file = file)
}