Etapas<- c('EI', 'EII', 'EIV', 'EVD')
Tratamientos<- c('T1', 'T1 y T2', 'T1 y T3', 'Control y fotos')

x1<- sample(0:1000000000,1)
x2<- sample(0:1000000000,1)

set.seed(x1)
set.seed(x2)
sample(Etapas)
sample(Tratamientos)

s1<-x1
s2<-x2
