nubePalabras<-function(file,fileStopWords)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

fileStopWords<-"./sw.csv"
stopwords1<-as.character(read.csv(fileStopWors)[,1])
file<-"./Escritorio/actividades_med.csv"
#datos<-google
datos<-as.data.frame(read.csv(file, as.is = c(1,2)))
datos[1]<-lapply(datos[1],as.character)
datos[2]<-as.numeric((datos$freq))

datos<-select(datos,Si.pudieras.cambiar.algunos.elementos.de.tu.animal.de.poder...cuáles.serían.)
datos<-filter(datos,razon_escogio!=""& razon_escogio!="NA")

for (j in seq(datos))
{
datos[,j] <- gsub("no toma", "no_toma", datos[,j],ignore.case = TRUE)
#datos[,j] <- gsub("cada día", "cada_día", datos[,j],ignore.case = TRUE)
#datos[,j] <- gsub("dar lo mejor", "dar_lo_mejor", datos[,j],ignore.case = TRUE)
#datos[,j] <- gsub("primer día", "primer_día", datos[,j],ignore.case = TRUE)
  }

corpus <- VCorpus(DataframeSource(datos),readerControl=list(language="spa"))


# Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove numbers
corpus<- tm_map(corpus, removeNumbers)

# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)
# Remove spanish common stopwords
corpus<- tm_map(corpus, removeWords, stopwords("spanish"))

# specify your stopwords as a character vector
corpus <- tm_map(corpus, removeWords, stopwords1) 

#stemming document
for(x in 1:length(corpus)){
  corpus1[[x]]<- stemDocument(corpus[[x]],language="spa") 
}

#change plurals
(f <- content_transformer(function(x, pattern,new) gsub(pattern, new, x)))
 # corpus<-tm_map(corpus, f,pattern="s\\s",new=" ")
 # corpus<-tm_map(corpus, f,pattern="disfraces",new="disfraz")
 #  corpus<-tm_map(corpus, f,pattern="celebracione",new="celebración")
# corpus<-tm_map(corpus, f,pattern="capacitacione",new="capacitaciones")
# corpus<-tm_map(corpus, f,pattern="reunione",new="reuniones")
# corpus<-tm_map(corpus, f,pattern="tallere",new="talleres")
# corpus<-tm_map(corpus, f,pattern="cumpleaño",new="cumpleaños")
# corpus<-tm_map(corpus, f,pattern="actuamo",new="actuamos")
# corpus<-tm_map(corpus, f,pattern="especiale",new="especiales")
# corpus<-tm_map(corpus, f,pattern="oportunidade",new="oportunidades")
##Tabla resumen
dtm <- TermDocumentMatrix(corpus)
matrizFrecuencias <- as.data.frame(as.matrix(dtm))
v <- sort(rowSums(matrizFrecuencias),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
write.csv(d,"./r_escogio.csv")

head(d, 70)

##Nube de frecuencias
wordcloud(words = datos$actividad, freq = datos$freq, scale=c(2.8,.7),min.freq = 0,
          random.order=FALSE, rot.per=0, 
          colors= c("firebrick1","navy","aquamarine4"))

wordcloud(words = datos$emoción, freq = datos$freq, scale=c(8,.9),min.freq = 0,
          random.order=FALSE, rot.per=0,ordered.colors=TRUE,
          colors= c("firebrick1", "aquamarine4")[factor(datos$color)])


wordcloud(words = df$name, freq = df$freq, min.freq = 1,scale = c(2, 0.4),
          max.words=200, random.order=FALSE, rot.per=0.1, 
          ordered.colors=TRUE,
          colors=brewer.pal(8, "Dark2")[factor(df$year)])



wordcloud(words = d$word, freq = d$freq, scale=c(3,.3),min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.4, 
          colors= c("gray47","yellowgreen","navy"))


