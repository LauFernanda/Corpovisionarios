tabla<-as.data.frame(prop.table(wtd.table(recodificada$LEG_SENT, recodificada$MA_LEGSENT2, weights = recodificada$PONDERADOR)))
tabla<-as.data.frame(prop.table(wtd.table(recodificada$MA_MASLEY, recodificada$MA_LEGSENT2, weights = recodificada$PONDERADOR)))

#names(tabla)<-c("Sentimiento hacia norma o regla","Sentimiento hacia ley" )
ggplot(data = tabla, aes(x=Var1, y=Var2, fill=Freq)) + 
  geom_tile()
tabla<-(prop.table(wtd.table(recodificada$JDL_FAMILIA, recodificada$VIF_GH, weights = recodificada$PONDERADOR)))

write_clip(original$MA_JUS_ACT_OT)

table(datos$PRO_CC, datos$PRO_CC_RI)

table(original$PRO_CC)
santander<-filter(original, COD_munimunicipio == "Santander de Quilichao")
istmina<-filter(original, COD_munimunicipio == "Istmina")
orito<-filter(original, COD_munimunicipio == "Orito")
write_clip(orito$MA_JUS_ACT_OT)
table(istmina$SP_ACUEDUCTO)

table(santander$MA_BIENE_4)

tabla<-as.data.frame(prop.table(wtd.table(recodificada$LEG_SENT, recodificada$MA_LEGSENT2, weights = recodificada$PONDERADOR)))
#####################SOLICITUDES###################

#CHEMONICS
##SIMPLES
#RURAL Y URBANO
tabla<-as.data.frame(prop.table(wtd.table(datosCH$MA_MASC_CON_ARBI,datosCH$EDAD_CAT, weights = datosCH$PONDERADOR,na.show = FALSE),2))
tabla
write_clip(tabla)
tabla<-as.data.frame(prop.table(wtd.table(datosCHr$CONF_I_OJU,weights = datosCHr$PONDERADOR,na.show = FALSE)))


options(OutDec = ",")

tabla<-as.data.frame(prop.table(wtd.table(datosCH$MA_ESTADO_2, datosCH$PRO_FPC, weights = datosCH$PONDERADOR),2))
tabla
write_clip(tabla)
table(original$PRO_FPC)
tabla<-as.data.frame(prop.table(wtd.table(datosCHr$MA_ESTADO_2, weights = datosCH$PONDERADOR)))
write_clip(tabla)
write_clip(get_label(datosCH$MA_ESTADO_2))

## FRANCO

#Cruces con  indicadores
datosCH$P_SEG_CAL<-datosCH$P_SEG_CAL
table(datosCH$P_SEG_CAL)
datosCH$P_SEG_CAL<-as.numeric((datosCH$P_SEG_CAL))[datosCH$P_SEG_CAL]

tabla<-as.data.frame(datosCH %>%group_by(COD_munimunicipio) %>%summarise(prom= weighted.mean(IRCP_CONFI_INT_STDR, PONDERADOR)))

tabla

write_clip(tabla)
write_clip(levels(datosCHr$MA_JUS_EQU1))

P1<-filter(datosCH,SEXO=="Hombre")
P2<-filter(datosCH,SEXO=="Mujer")
wtd.t.test(x = P1$IDNV_VIP_STDR,y= P2$IDNV_VIP_STDR,weight = P1$PONDERADOR, weighty = P2$PONDERADOR,samedata = FALSE)
t.test(IRL_disposicion_STDR ~ MA_JUS_EQU1, data = datosCHr, var.equal = TRUE)

wtd.mean(datosCH$IRCP_CONFI_INT_STDR,weights = datosCH$PONDERADOR)


prop.table(wtd.table(datosCHr$PRO_FPC, weights = datosCH$PONDERADOR))

levels(datosCH$CONV_VEC_INS)[levels(datosCH$CONV_VEC_INS)=='No aplica'] <- NA

levels(datosCH$CONV_NO_PAGO)[levels(datosCH$CONV_NO_PAGO)=='No aplica'] <- NA

summary(datosCH$IRCP_CONFI_INT_STDR)

tabla<-as.data.frame(prop.table(wtd.table(datosCHr$Raza2,datosCHr$COLOR_PALETA, weights = datosCHr$PONDERADOR,na.show = FALSE)))
tabla
write_clip(tabla)

ggplot(datosCH, aes(x=IPO_CONFI_STDR, y=IDNV_VIP_STDR)) + geom_point()
write_clip(datosCH$MA_EMO_17_OT) 
###PRESENTACIÓN
sum(datosCH$PONDERADOR,na.rm=T)

table(datosTT$Raza2,datosTT$d_0_municentro_poblado)
write_clip(orito$MA_JUS_ACT_OT)
## TETRATECH
tabla<-as.data.frame(datosTTr %>%group_by(PRO_CC) %>%summarise(prom= weighted.mean(IPO_ACUER_STDR, PONDERADOR)))
tabla
write_clip(tabla)
write_clip(wtd.mean(datosTT$IPO_ACUER_STDR, weights = datosTT$PONDERADOR))

levels()

write_clip(levels(datosCHr$MA_JUS_EQU1))



P1<-filter(datosCH,SEXO=="Hombre")
P2<-filter(datosCH,SEXO=="Mujer")
wtd.t.test(x = P1$IDNV_VIP_STDR,y= P2$IDNV_VIP_STDR,weight = P1$PONDERADOR, weighty = P2$PONDERADOR,samedata = FALSE)

##Tablas  dos categóricas
ruralTTr<-filter(datosTTr,Zona=='Rural')
tabla<-as.data.frame(prop.table(wtd.table(datosTTr$PART_AC_MS,datosTTr$CONF_I_EJ, weights = datosTTr$PONDERADOR,na.show = FALSE),2))

tabla
write_clip(tabla)
write_clip(levels(datosTTr$MA_EMO_8))
write_clip(prop.table(wtd.table(datosTTr$CONF_I_PERSONERIA, weights = datosTTr$PONDERADOR)))
tabla<-as.data.frame(prop.table(wtd.table(datosCHr$CONF_I_OJU,weights = datosCHr$PONDERADOR,na.show = FALSE)))
wtd.chi.sq(ruralTTr$PART_AC_MS,ruralTTr$MA_ESTADO_6, weight = ruralTTr$PONDERADOR)

