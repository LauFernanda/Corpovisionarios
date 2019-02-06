##UNIR VARIABLES INDICADORES

datosTTr<-cbind(datosTTr,datosTT[,c(469:486)])

##Regresión
##CONFIANZA INTERPERSONAL
names(datosTTr)<-toupper(names(datosTTr))
model <- lm(IPO_CONFI_STDR  ~ COD_MUNIMUNICIPIO+ZONA+ SEXO +RAZA2+ ESTRA_CAT + ANNOS + EDUCA  + RELIGION+ LEG_SENT + ACU_PER_PREF + ACU_VEC + ACU_PAR + ACU_COM + ACU_DES + ACU_EST +
              IDNV_VIP_STDR + IDNV_VIF_STDR + IRL_DISPOSICION_STDR+ IRL_ARMONIZACION_STDR +
              IIC_SITUACIONES_STDR + IIC_TOLERANCIA_STDR + 
              IRCP_PARTIC_STDR + IPO_ACUER_STDR + IPO_PROBI_C_STDR + IPO_SIM_STDR + 
              IRL_ENTUSIASMO_STDR + IRCP_REGULA_STDR + IRCP_PROBI_FP_STDR + 
              IRCP_CONFI_INT_STDR + IRCP_IMPORT_STDR + IS_PERCEP_SEG_STDR + 
              VICTIMA+MA_LEGSENT2+MA_EMO_6+VICTIMA_DISCRIMINACION ,
            data = datosTTr, weights = datosTTr$PONDERADOR)
summary(model)
#covariate.labels= c("Tumaco", "Puerto Asís","Rural","Mujer", "Mestizo",,"Nivel socioeconómico medio","Nivel socioeconómico alto","Edad","Nivel educativo Primaria","Nivel educativo Secundaria",
                    "Nivel educativo Técnico/Tecnólogo","Nivel educativo Universitario","Nivel educativo Posgrado","Sentimiento hacia las normas o reglas positivo", "Sentimiento hacia las normas o reglas indiferente", "Sentimiento hacia las normas o reglas negativo", "Sentimiento hacia las normas o reglas muy negativo",
                    "Preferencia hacer acuerdos casi nunca o nunca","No preferencia  a hacer acuerdos con Vecinos", "No preferencia  a hacer acuerdos con parientes",
                    "No preferencia a hacer acuerdos con compañeros de trabajo", "No preferencia  a hacer acuerdos con desconocidos", "No preferencia  a hacer acuerdos con el Estado",
                    "No violencia interpersonal","No violencia intrafamiliar", "Disposición a obedecer la ley",  "Armonización de la ley con otros sistemas reguladores",
                    "No ocurrencia de situaciones conflictivas", "Coexistencia y diversidad",
                    "Participación efectiva", "Confianza en cumplimiento de acuerdos", "Percepción de probidad en otros ciudadanos", "Asimetría en la percepción de motivaciones de los demás",
                    "Opinión positiva frente a la ley", "Percepción de regulación por parte de las autoridades", "Probidad de funcionarios públicos", 
                    "Confianza en instituciones públicas", "Importancia de los asuntos públicos", "Percepción de seguridad",
                    "Satisfacción con la vida","Victima (directa o indirecta")
stargazer(model, type="text", dep.var.labels =c("Confianza interpersonal"),
                    out="Confianza interpersonal.txt")

datosTTr$CONF_PER_GRAL_D<-to_dummy(datosTTr$CONF_PER_GRAL)
table(datosTTr$CONF_PER_GRAL_D$CONF_PER_GRAL_1,datosTTr$CONF_PER_GRAL)
datosTTr$CONF_PER_GRAL_D<-datosTTr$CONF_PER_GRAL_D$CONF_PER_GRAL_1
table(datosTTr$CONF_PER_GRAL_D,datosTTr$CONF_PER_GRAL)

model <- glm( CONF_PER_GRAL_D  ~ COD_MUNIMUNICIPIO+ZONA+ SEXO +RAZA2+ ESTRA_CAT + ANNOS + EDUCA  + RELIGION+
                LEG_SENT + ACU_PER_PREF + ACU_VEC + ACU_PAR + ACU_COM + ACU_DES + ACU_EST +
                IDNV_VIP_STDR + IDNV_VIF_STDR + IRL_DISPOSICION_STDR+ IRL_ARMONIZACION_STDR +
                IIC_SITUACIONES_STDR + IIC_TOLERANCIA_STDR + 
                IRCP_PARTIC_STDR + IPO_ACUER_STDR + IPO_PROBI_C_STDR + IPO_SIM_STDR + 
                IRL_ENTUSIASMO_STDR + IRCP_REGULA_STDR + IRCP_PROBI_FP_STDR + 
                IRCP_CONFI_INT_STDR + IRCP_IMPORT_STDR + IS_PERCEP_SEG_STDR + 
                VICTIMA+MA_LEGSENT2+MA_EMO_6+VICTIMA_DISCRIMINACION+MA_PRODUCT_1+MA_PRODUCT_2+
                LMC_FAC_LEY+LMC_LEYYCOST+LMC_LEYYCONS+PART_ORG_REL+PART_ORG_AMBIENTAL+
                PART_SERVPUB+PART_ASO_PRODU+MA_OPI_POSACUERDO+LMC_COMP+LMC_COMD+ 
                CONF_IP_AM+ CONF_IP_VEC+ CONF_IP_PR+CONF_IP_COM ,
              data = datosTTr, family = binomial,weights = datosTTr$PONDERADOR)
summary(model)

coeficientesCONF_PER_GRAL<-as.data.frame(summary(model)$coef)
write.csv2(coeficientesCONF_PER_GRAL, file="regresión logística conf_per_gral.csv")

stargazer(model, type="text", dep.var.labels =c("Confianza interpersonal GENERAL"),
          out="Confianza interpersonal GENERAL sin expandir.txt")



str(datosTTr$CONF_PER_GRAL)

##SIMETRÍA
model <- lm(IPO_SIM_STDR  ~ ZONA+ SEXO +RAZA2+ ESTRA_CAT + ANNOS + EDUCA  + LEG_SENT + LEG_SENT2+ ACU_PER_PREF + ACU_VEC + ACU_PAR + ACU_COM + ACU_DES + ACU_EST +
              IDNV_VIP_STDR + IDNV_VIF_STDR + IRL_DISPOSICION_STDR+ IRL_ARMONIZACION_STDR +
              IIC_SITUACIONES_STDR + IIC_TOLERANCIA_STDR + 
              IRCP_PARTIC_STDR + IPO_ACUER_STDR + IPO_PROBI_C_STDR +IPO_CONFI_STDR  + 
              IRL_ENTUSIASMO_STDR + IRCP_REGULA_STDR + IRCP_PROBI_FP_STDR + 
              IRCP_CONFI_INT_STDR + IRCP_IMPORT_STDR + IS_PERCEP_SEG_STDR + 
              VICTIMA +MA_EMO_6+VICTIMA_DISCRIMINACION,
            data = datosTTr, weights = datosTTr$PONDERADOR)
summary(model)

stargazer(model, type="text", dep.var.labels =c("Simetría en la percepción de motivaciones de los demás"),
          covariate.labels= c("Rural","Mujer", "Nivel socioeconómico medio","Nivel socioeconómico alto","Edad","Nivel educativo Primaria","Nivel educativo Secundaria",
                              "Nivel educativo Técnico/Tecnólogo","Nivel educativo Universitario","Nivel educativo Posgrado","Sentimiento hacia las normas o reglas positivo", "Sentimiento hacia las normas o reglas indiferente", "Sentimiento hacia las normas o reglas negativo", "Sentimiento hacia las normas o reglas muy negativo",
                              "Preferencia hacer acuerdos casi nunca o nunca","No preferencia  a hacer acuerdos con Vecinos", "No preferencia  a hacer acuerdos con parientes",
                              "No preferencia a hacer acuerdos con compañeros de trabajo", "No preferencia  a hacer acuerdos con desconocidos", "No preferencia  a hacer acuerdos con el Estado",
                              "No violencia interpersonal","No violencia intrafamiliar", "Disposición a obedecer la ley",  "Armonización de la ley con otros sistemas reguladores",
                              "No ocurrencia de situaciones conflictivas", "Coexistencia y diversidad",
                              "Participación efectiva", "Confianza en cumplimiento de acuerdos", "Percepción de probidad en otros ciudadanos", "Confianza interpersonal",
                              "Opinión positiva frente a la ley", "Percepción de regulación por parte de las autoridades", "Probidad de funcionarios públicos", 
                              "Confianza en instituciones públicas", "Importancia de los asuntos públicos", "Percepción de seguridad",
                              "Satisfacción con la vida","Victima (directa o indirecta"),
          out="Simetría.html")
