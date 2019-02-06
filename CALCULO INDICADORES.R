##INDICADOR
#Leer Base y diccionarios.
load('./UAESP_hogares_corregida.Rda')

archivos<-(gs_ls()[,1])
diccionario<-gs_title(as.character(archivos[4,1]))
diccionario<-gs_read(diccionario,ws=1)
datos1<-hogares
datos1<-droplevels(datos1)
datos1<-recicladores
#datos1<-recicladores
#datos1<-droplevels(datos1)



### ARREGLAR VARIABLES INUSUALES

datos1$RS_PRAC1[datos1$RS_PRAC1>=2]<-'2 o más recipientes'
datos1$RS_PRAC1<-to_factor(datos1$RS_PRAC1)
table(datos1$RS_PRAC1)

datos1$RS_CONO19[datos1$RS_CONO19=='No sé']<-'Bolsa negra'
datos1$RS_CONO20[datos1$RS_CONO20=='No sé']<-'Bolsa negra'
datos1$RS_CONO21[datos1$RS_CONO21=='No sé']<-'Bolsa negra'
datos1$RS_CONO22[datos1$RS_CONO22=='No sé']<-'Bolsa negra'
datos1$RS_CONO23[datos1$RS_CONO23=='No sé']<-'Bolsa negra'
datos1$RS_CONO24[datos1$RS_CONO24=='No sé']<-'Bolsa negra'
datos1$RS_CONO25[datos1$RS_CONO25=='No sé']<-'Bolsa blanca'
datos1$RS_CONO26[datos1$RS_CONO26=='No sé']<-'Bolsa negra'
names(datos1)[names(datos1)=='RELA_EXPEC10']<-'RELA_REPRE34'
datos1<-droplevels(datos1)

#### VARIABLES CON VALORES PARA INDICADOR

directa<-(filter(diccionario,DESEABILIDAD=='DIRECTA')[,3]) 
indirecta<-(filter(diccionario,DESEABILIDAD=='INDIRECTA')[,3]) 

#intersect(names(datos1),(directa[,1]))
datos1<-hogares
### RECODIFICAR DE O A 1
for(i in 1:nrow(directa)){
  variable<-as.character(directa[i,1])
  if(variable %in% names(datos1)){
    eval(parse(text = paste("noCategorias<-length(levels(datos1$",
                            variable,"))",sep="")))
    eval(parse(text = paste("datos1$",variable,"<-as.integer(datos1$",
                            variable,")",sep="")))
    eval(parse(text = paste("datos1<-mutate(datos1,",variable,"_RI=ifelse(",variable,"==",noCategorias,
                            ",1,(1-((",noCategorias,"-",variable,")/(",noCategorias,"-1)))))",sep = ""))) 
  }
  }


for(i in 1:nrow(indirecta)){
  variable<-as.character(indirecta[i,1])
  if(variable %in% names(datos1)){
  eval(parse(text = paste("noCategorias<-length(levels(datos1$",
                          variable,"))",sep="")))
  eval(parse(text = paste("datos1$",variable,"<-as.integer(datos1$",
                          variable,")",sep="")))
  eval(parse(text = paste("datos1<-mutate(datos1,",variable,"_RI=ifelse(",variable,"==1,1,((",variable,"-1)*1/(",noCategorias,"-1)-1)*-1))",sep = "")))
}
}

#####PRACTICAS

variablesIndicador<-names(datos1)[startsWith(names(datos1),'RS_PRAC')]

variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[-c(4,5)]
practicas<-select(datos1,variablesIndicador)
vis_miss(practicas)
table(hogares$RS_PRAC13,practicas$RS_PRAC13_RI)
alpha(practicas)
## Alpha 0.68 Quitar 13? (separación en centros comerciales)

hogares$IND_PRACTICAS<-rowSums(practicas,na.rm = T)*10/length(variablesIndicador)
promPracticas<-wtd.mean(hogares$IND_PRACTICAS, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)


DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_PRACTICAS, PONDERADOR))


write_clip(DESAGREGADO_IND)

####CONOCIMIENTOS
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RS_CONO')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[-c(1:4,8:12,27)]
conocimientos<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(conocimientos)
table(hogares$RS_CONO31, conocimientos$RS_CONO31_RI)
alpha(conocimientos)
#Alpha 0.75
hogares$IND_CONOCIMIENTOS<-rowSums(conocimientos,na.rm = T)*10/length(variablesIndicador)
promConocimientos<-wtd.mean(hogares$IND_CONOCIMIENTOS, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_CONOCIMIENTOS, PONDERADOR))


write_clip(DESAGREGADO_IND)

####MOTIVACIONES
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RS_MOTIV')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]

####REPRESENTACIÓN DEL OTRO
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_REPRE')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
#variablesIndicador<-variablesIndicador[-c(1:4,7:11)]
representacion_otro<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_otro)
table(hogares$RELA_REPRE1, representacion_otro$RELA_REPRE1_RI)
alpha(representacion_otro)
#Alpha 0.77 REPRE 1 se comporta opuesto

hogares$IND_REPRESENTACION<-rowSums(representacion_otro,na.rm = T)*10/length(variablesIndicador)
promRepresentacion<-wtd.mean(hogares$IND_REPRESENTACION, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_REPRESENTACION, PONDERADOR))


write_clip(DESAGREGADO_IND)


### REPRESENTACION RECICLADORES
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_REPRE')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[-c(5,6,12:25,29)]
representacion_reci<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_reci)
table(FENICIA_FULL$RELA_REPRE1,representacion_reci$RELA_REPRE1_RI)
alpha(representacion_reci)
#Alpha 0.59 REPRE 1 se comporta opuesto. Si se excluye, alpha= 0.63
#Alpha fenicia 0.51
FENICIA_FULL$IND_REPRE_RECICLADOR<-rowSums(representacion_reci,na.rm = T)*10/length(variablesIndicador)
promRepresentacionRecicla<-wtd.mean(hogares$IND_REPRE_RECICLADOR, normwt = "ignored", na.rm = TRUE)

datos<-filter(FENICIA_FULL,medicion=='POS')
DESAGREGADO_IND <- 
  datos %>% 
  group_by(TRATAMIENTO) %>% 
  summarise(prom= mean(IND_REPRE_RECICLADOR))


write_clip(DESAGREGADO_IND)

### REPRESENTACION CIUDADANOS
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_REPRE')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
representacion_otro<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_otro)
table(hogares$RELA_REPRE11, representacion_otro$RELA_REPRE11_RI)
alpha(representacion_otro)
#Alpha 0.75 
hogares$IND_REPRE_GENERAL<-rowSums(representacion_otro,na.rm = T)*10/length(variablesIndicador)
promRepresentacionGeneral<-wtd.mean(hogares$IND_REPRE_GENERAL, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_REPRE_GENERAL, PONDERADOR))


write_clip(DESAGREGADO_IND)
### EXPECTATIVAS (REALMENTE ES DE ACUERDOS)

variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_EXPEC')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
#variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
expectativas<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(expectativas)
table(hogares$RELA_EXPEC7, expectativas$RELA_EXPEC7_RI)
alpha(expectativas)
#Alpha 0.75 
hogares$IND_EXPECTATIVAS<-rowSums(expectativas,na.rm = T)*10/length(variablesIndicador)
promExpectativas<-wtd.mean(hogares$IND_EXPECTATIVAS, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_EXPECTATIVAS, PONDERADOR))


write_clip(DESAGREGADO_IND)

### REPRESENTACION CIUDADANOS + EXPECTATIVAS (ACUERDOS)
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_REPRE')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
variablesIndicador2<-names(datos1)[startsWith(names(datos1),'RELA_EXPEC')]
variablesIndicador2<-variablesIndicador2[endsWith(variablesIndicador2, 'RI')]
variablesIndicador<-c(variablesIndicador,variablesIndicador2)
representacion_otro2<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_otro2)
table(hogares$RELA_REPRE11, representacion_otro2$RELA_REPRE11_RI)
alpha(representacion_otro2)
#Alpha 0.76 
hogares$IND_REPRE_CIUDADANOS<-rowSums(representacion_otro2,na.rm = T)*10/length(variablesIndicador)
promRepresentacionCiudadanos<-wtd.mean(hogares$IND_REPRE_CIUDADANOS, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(EDAD_CAT) %>% 
  summarise(prom= weighted.mean(IND_REPRE_CIUDADANOS, PONDERADOR))


write_clip(DESAGREGADO_IND)

### RELACIÓN CON LAS NORMAS

variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_NORM')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
#variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
normas<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(normas)
table(hogares$RELA_NORM21, normas$RELA_NORM21_RI)
alpha(normas)
#Alpha 0.79  PERCEPCIÓN REGULACIONES  VAN EN DIRECCIÓN OPUESTA A JDL. CALCULAR POR APARTE
hogares$IND_RELANORM<-rowSums(normas,na.rm = T)*10/length(variablesIndicador)
promNormas<-wtd.mean(hogares$IND_RELANORM, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_RELANORM, PONDERADOR))


write_clip(DESAGREGADO_IND)


#PERCEPCIÓN DE REGULACIÓN
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_NORM')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(12:19)]
regulacion<-select(datos1,variablesIndicador)


#REVISAR NAS
vis_miss(regulacion)
table(hogares$RELA_NORM5, regulacion$RELA_NORM5_RI)
alpha(regulacion)
#Alpha 0.87
hogares$IND_PERCREGULACION<-rowSums(regulacion,na.rm = T)*10/length(variablesIndicador)
promPercRegulacion<-wtd.mean(hogares$IND_PERCREGULACION, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_PERCREGULACION, PONDERADOR))


write_clip(DESAGREGADO_IND)

#####JDL
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_NORM')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(1:11)]
jdl<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(jdl)
table(hogares$RELA_NORM23, jdl$RELA_NORM23_RI)
alpha(jdl)
#Alpha 0.86
hogares$IND_JDL<-rowSums(jdl,na.rm = T)*10/length(variablesIndicador)
promJDL<-wtd.mean(hogares$IND_JDL, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_JDL, PONDERADOR))


write_clip(DESAGREGADO_IND)

####CONFIANZA

variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_CONF')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
#variablesIndicador<-variablesIndicador[c(1:11)]
confianza<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(confianza)
table(hogares$RELA_CONF3, confianza$RELA_CONF3_RI)
alpha(confianza)
#Alpha 0.86
hogares$IND_CONF<-rowSums(confianza,na.rm = T)*10/length(variablesIndicador)
promConfianza<-wtd.mean(hogares$IND_CONF, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_CONF, PONDERADOR))


write_clip(DESAGREGADO_IND)

###RELACION ENTRE ACTORES
variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_CONF')]

#CONSUMO RESPONSABLE
variablesIndicador<-names(datos1)[startsWith(names(datos1),'IA_CONRES')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
#variablesIndicador<-variablesIndicador[c(1:11)]
conRes<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(conRes)
table(hogares$IA_CONRES2, conRes$IA_CONRES2_RI)
alpha(conRes)
#Alpha 0.49 Sacar Conres2 Consumo de  icopor lo deja 0.53
hogares$IND_CONRES<-rowSums(conRes,na.rm = T)*10/length(variablesIndicador)
promConRes<-wtd.mean(hogares$IND_CONRES, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_CONRES, PONDERADOR))


write_clip(DESAGREGADO_IND)

#CREENCIAS AMBIENTALES
variablesIndicador<-names(datos1)[startsWith(names(datos1),'IA_CREE')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
#variablesIndicador<-variablesIndicador[c(1:11)]
cree<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(cree)
table(hogares$IA_CREE2, cree$IA_CREE2_RI)
alpha(cree)
#Alpha 0.37
hogares$IND_CREE<-rowSums(cree,na.rm = T)*10/length(variablesIndicador)
promCree<-wtd.mean(hogares$IND_CREE, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_CREE, PONDERADOR))


write_clip(DESAGREGADO_IND)

####CONFIANZA ENTIDADES RELACIONADAS CON ASEO

variablesIndicador<-names(datos1)[startsWith(names(datos1),'RELA_CONF')]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(2,6:8)]
confianza<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(confianza)
table(hogares$RELA_CONF2, confianza$RELA_CONF2_RI)
alpha(confianza)
#Alpha 0.77 PUEDE SUBIR A 079 SI SALE ALCALDÍA
hogares$IND_CONF_ASEO<-rowSums(confianza,na.rm = T)*10/length(variablesIndicador)
promConfianzaAseo<-wtd.mean(hogares$IND_CONF_ASEO, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_CONF, PONDERADOR))


write_clip(DESAGREGADO_IND)


### REPRESENTACION CIUDADANOS Y ACUERDOS
variablesIndicador<-names(datos1)[c(startsWith(names(datos1),'RELA_REPRE'),startsWith(names(datos1),'RELA_EXPEC'))]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
representacion_otro<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_otro)
table(hogares$RELA_REPRE11, representacion_otro$RELA_REPRE11_RI)
alpha(representacion_otro)
#Alpha 0.75 
hogares$IND_REPRE_GENERAL<-rowSums(representacion_otro,na.rm = T)*10/length(variablesIndicador)
promRepresentacionGeneral<-wtd.mean(hogares$IND_REPRE_GENERAL, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_REPRE_GENERAL, PONDERADOR))


write_clip(DESAGREGADO_IND)

file<-"./UAESP_hogares_corregida.Rda"
save(hogares, file=file)

####TODOS LOS INDICADORES EN UNO
datos1<-hogares
variablesIndicador<-names(datos1)[c(startsWith(names(datos1),'IND'))]
variablesIndicador<-variablesIndicador[c(-3,-5,-6,-7,-13)]
indicador<-select(datos1,variablesIndicador)

#REVISAR NAS

vis_miss(indicador)
#table(hogares$RELA_REPRE11, representacion_otro$RELA_REPRE11_RI)
alpha(indicador)
#Alpha 0.44 
hogares$IND_REPRE_GENERAL<-rowSums(representacion_otro,na.rm = T)*10/length(variablesIndicador)
promRepresentacionGeneral<-wtd.mean(hogares$IND_REPRE_GENERAL, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_REPRE_GENERAL, PONDERADOR))


write_clip(DESAGREGADO_IND)

variablesIndicador<-names(datos1)[c(endsWith(names(datos1),'RELA_REPRE'),startsWith(names(datos1),'RELA_EXPEC'))]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
representacion_otro<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_otro)
table(hogares$RELA_REPRE11, representacion_otro$RELA_REPRE11_RI)
alpha(representacion_otro)
#Alpha 0.75 
hogares$IND_REPRE_GENERAL<-rowSums(representacion_otro,na.rm = T)*10/length(variablesIndicador)
promRepresentacionGeneral<-wtd.mean(hogares$IND_REPRE_GENERAL, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)

DESAGREGADO_IND <- 
  hogares %>% 
  group_by(T_VIVIENDA) %>% 
  summarise(prom= weighted.mean(IND_REPRE_GENERAL, PONDERADOR))


write_clip(DESAGREGADO_IND)


### Matriz de correlaciones 
variablesIndicador<-names(hogares)[startsWith(names(hogares),'IND')]
variablesIndicador<-variablesIndicador[c(-3,-5,-6,-7,-13)]
variablesIndicador<-variablesIndicador[c(1,2,9,3,5,4,6,7,8)]
datos<-select(hogares, variablesIndicador )

correlaciones<-round(cor(datos),4)
write_clip(correlaciones)
melted_correlaciones <- filter(melt(correlaciones))

ggplot(data = melted_correlaciones, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

### TODAS LAS VARIABLES CON DESEABILIDAD
variablesIndicador<-names(datos1)[c(startsWith(names(datos1),'RELA_REPRE'),startsWith(names(datos1),'RELA_EXPEC'))]
variablesIndicador<-variablesIndicador[endsWith(variablesIndicador, 'RI')]
variablesIndicador<-variablesIndicador[c(5,6,12:25,29)]
representacion_otro<-select(datos1,variablesIndicador)

#REVISAR NAS
vis_miss(representacion_otro)
table(hogares$RELA_REPRE11, representacion_otro$RELA_REPRE11_RI)
alpha(representacion_otro)
#Alpha 0.75 
hogares$IND_REPRE_GENERAL<-rowSums(representacion_otro,na.rm = T)*10/length(variablesIndicador)
promRepresentacionGeneral<-wtd.mean(hogares$IND_REPRE_GENERAL, weights = hogares$PONDERADOR, normwt = "ignored", na.rm = TRUE)
