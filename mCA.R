names(recodificada)
datosTT$observacionesobserv<-as.character(datosTT$observacionesobserv)
str(datosTT$observacionesobserv)
datosTT1<-datosTT[,-360]
names(datosTT)
write_spss(datosTT1,"./datosTT.sav")

categoricas<-recodificada[,c(-1:-9,-14:-16,-18,-27,seq(-288,-326, by=-2),-354:-364,-365:-372)]
categoricas$SEXO<-to_factor(categoricas$SEXO)

levels(categoricas$SEXO)[1]<-"Hombre"
levels(categoricas$SEXO)[2]<-"Mujer"
categoricas$MA_BIENE_1<-to_factor(categoricas$MA_BIENE_1)
categoricas$MA_BIENE_2<-to_factor(categoricas$MA_BIENE_2)
categoricas$MA_BIENE_3<-to_factor(categoricas$MA_BIENE_3)
categoricas$MA_BIENE_4<-to_factor(categoricas$MA_BIENE_4)
categoricas$MA_BIENE_5<-to_factor(categoricas$MA_BIENE_5)
categoricas$MA_BIENE_6<-to_factor(categoricas$MA_BIENE_6)
categoricas$MA_BIENE_7<-to_factor(categoricas$MA_BIENE_7)
categoricas$COLOR_PALETA<-to_factor(categoricas$COLOR_PALETA)

#seq(-288,-326, by=-2)
f <- sapply(categoricas, is.factor)
categoricas<-categoricas[,sapply(categoricas,is.factor)]
f <- sapply(categoricas, is.factor)

which(f == FALSE)
vis_miss(categoricas, warn_large_data = FALSE)

MCA<-MCA(categoricas,row.w = recodificada$PONDERADOR)
fviz_screeplot(MCA, addlabels = TRUE, ylim = c(0,2 ))
var <- get_mca_var(MCA)
fviz_mca_var(MCA, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

####PCA
c<-KMO(indicadores[,1:16])

res.pca <- prcomp(indicadores, scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
            col.var = "contrib", # Color by contributions to the PC
           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE     # Avoid text overlapping,
)

### HCPC
res.hcp2_cl<-HCPC(MCA, nb.clust = 2, graph = TRUE)
res.hcp<-HCPC(MCA, nb.clust = 3, graph = TRUE)
variables_signi<-as.data.frame(res.hcp$desc.var$test.chi2)
write_clip(variables_signi)
write_clip(rownames(variables_signi))

var_cluster<-as.data.frame(res.hcp$desc.var$category$`3`)

write_clip(var_cluster)
write_clip(rownames(var_cluster))

res.hcp$desc.ind$para

tablaHCPC<-res.hcp$data.clust
tablaHCPC$PONDERADOR<-recodificada$PONDERADOR
tablaHCPC$ID<-recodificada$ID
write_clip(as.data.frame(prop.table(table(tablaHCPC$clust))))

fviz_dend(res.hcp, rect = TRUE)

prop.table(wtd.table(tablaHCPC$ZONAS,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),1)
prop.table(wtd.table(tablaHCPC$SEXO,weights = tablaHCPC$PONDERADOR))
prop.table(wtd.table(tablaHCPC$SEXO,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),2)

prop.table(wtd.table(tablaHCPC$EDAD_CAT,weights = tablaHCPC$PONDERADOR))
prop.table(wtd.table(tablaHCPC$EDAD_CAT,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),2)

prop.table(wtd.table(tablaHCPC$Raza2,weights = tablaHCPC$PONDERADOR))
prop.table(wtd.table(tablaHCPC$Raza2,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),2)

prop.table(wtd.table(tablaHCPC$COD_munimunicipio,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),2)

prop.table(wtd.table(tablaHCPC$d_0_municentro_poblado,weights = tablaHCPC$PONDERADOR))
write_clip(as.data.frame(prop.table(wtd.table(tablaHCPC$d_0_municentro_poblado,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),2)))
write_clip(levels(original$d_0_municentro_poblado))
prop.table(wtd.table(tablaHCPC$MA_DISCRI_8,tablaHCPC$clust,weights = tablaHCPC$PONDERADOR),1)


write_clip(variablesHCPC)
indicadores$clust<-tablaHCPC$clust

indicadores$PONDERADOR<-tablaHCPC$PONDERADOR
indicadores_clust<-data.frame(Cluster=c(1,2,3))
for(i in 1:length(variablesIndicador)){  
  indicador<-indicadores %>%group_by(clust) %>% 
        summarise(prom= weighted.mean(eval(parse(text=variablesIndicador[i])),w = PONDERADOR))
  names(indicador)[2]<-variablesIndicador[i]
  indicadores_clust<-cbind(indicadores_clust,indicador[,2])
}
indicadores_clust<-t(indicadores_clust)
write_clip(indicadores_clust)
write_clip(rownames(indicadores_clust))
fviz_cluster(res.hcp2_cl,labelsize = 0,show.clust.cent = TRUE)
indiviuos_clust<-(res.hcp$desc.ind$dist$`1`)

#fviz_mca_var(MCA, col.var = "contrib",
 #            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
  #           repel = TRUE, # avoid text overlapping (slow)
   #          ggtheme = theme_minimal()
#)
plot()