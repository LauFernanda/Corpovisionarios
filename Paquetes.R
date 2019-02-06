# LIBRERIAS
if(!("foreign" %in% rownames(installed.packages()))){ install.packages("foreign",dependencies=c("Depends","Imports","Suggests")) }
if(!("reshape2" %in% rownames(installed.packages()))){ install.packages("reshape2",dependencies=c("Depends","Imports")) }
if(!("BH" %in% rownames(installed.packages()))){ install.packages("BH",dependencies=c("Depends","Imports")) }
if(!("dplyr" %in% rownames(installed.packages()))){ install.packages("dplyr",dependencies=c("Depends","Imports")) }
if(!("haven" %in% rownames(installed.packages()))){ install.packages("haven",dependencies=c("Depends","Imports")) }
if(!("RcppEigen" %in% rownames(installed.packages()))){ install.packages("RcppEigen",dependencies=c("Depends","Imports")) }
if(!("lme4" %in% rownames(installed.packages()))){ install.packages("lme4",dependencies=c("Depends","Imports")) }
if(!("sjmisc" %in% rownames(installed.packages()))){ install.packages("sjmisc",dependencies=c("Depends","Imports")) }
if(!("psych" %in% rownames(installed.packages()))){ install.packages("psych",dependencies=c("Depends","Imports","Suggests")) }
if(!("psy" %in% rownames(installed.packages()))){ install.packages("psy",dependencies=c("Depends","Imports","Suggests")) }
if(!("printr" %in% rownames(installed.packages()))){ install.packages("printr",dependencies=c("Depends","Imports","Suggests")) }
if(!("htmlTable" %in% rownames(installed.packages()))){ install.packages("htmlTable",dependencies=c("Depends","Imports","Suggests")) }

if(!("tidyr" %in% rownames(installed.packages()))){ install.packages("tidyr",dependencies=c("Depends","Imports")) }

if(!("rJava" %in% rownames(installed.packages()))){ install.packages("rJava",dependencies=c("Depends","Imports")) }
if(!("libcurl" %in% rownames(installed.packages()))){ install.packages("liburl",dependencies=c("Depends","Imports")) }
if(!("curl" %in% rownames(installed.packages()))){ install.packages("curl",dependencies=c("Depends","Imports")) }
if(!("openssl-devel" %in% rownames(installed.packages()))){ install.packages("openssl-devel",dependencies=c("Depends","Imports")) }
if(!("openssl" %in% rownames(installed.packages()))){ install.packages("openssl",dependencies=c("Depends","Imports")) }
if(!("RMySQL" %in% rownames(installed.packages()))){ install.packages("RMySQL",dependencies=c("Depends","Imports")) }

if(!("xlsx" %in% rownames(installed.packages()))){ install.packages("xlsx",dependencies=c("Depends","Imports")) }
if(!("qdapTools" %in% rownames(installed.packages()))){ install.packages("qdapTools",dependencies=c("Depends","Imports")) }
if(!("gender" %in% rownames(installed.packages()))){ install.packages("gender",dependencies=c("Depends","Imports")) }
if(!("RCurl" %in% rownames(installed.packages()))){ install.packages("RCurl",dependencies=c("Depends","Imports")) }
if(!("tidyr" %in% rownames(installed.packages()))){ install.packages("tidyr",dependencies=c("Depends","Imports")) }
if(!("XML" %in% rownames(installed.packages()))){ install.packages("XML",dependencies=c("Depends","Imports")) }
if(!("party" %in% rownames(installed.packages()))){ install.packages("party",dependencies=c("Depends","Imports")) }


if(!("ggplot2" %in% rownames(installed.packages()))){ install.packages("ggplot2",dependencies=c("Depends","Imports")) }

if(!("qdap" %in% rownames(installed.packages()))){ install.packages("qdap",dependencies=c("Depends","Imports")) }
if(!("Hmisc" %in% rownames(installed.packages()))){ install.packages("Hmisc",dependencies=c("Depends","Imports")) }
if(!("gdata" %in% rownames(installed.packages()))){ install.packages("gdata",dependencies=c("Depends","Imports")) }
if(!("weights" %in% rownames(installed.packages()))){ install.packages("weights",dependencies=c("Depends","Imports")) }
if(!("tm" %in% rownames(installed.packages()))){ install.packages("tm",dependencies=c("Depends","Imports")) }
if(!("SnowballC" %in% rownames(installed.packages()))){ install.packages("SnowballC",dependencies=c("Depends","Imports")) }
if(!("wordcloud" %in% rownames(installed.packages()))){ install.packages("wordcloud",dependencies=c("Depends","Imports")) }
if(!("RColorBrewer" %in% rownames(installed.packages()))){ install.packages("RColorBrewer",dependencies=c("Depends","Imports")) }
if(!("ggplot2t" %in% rownames(installed.packages()))){ install.packages("ggplot2",dependencies=c("Depends","Imports")) }
if(!("XML" %in% rownames(installed.packages()))){ install.packages("XML",dependencies=c("Depends","Imports")) }
if(!("alluvial" %in% rownames(installed.packages()))){ install.packages("alluvial",dependencies=c("Depends","Imports","Suggests")) }

if(!("RQDA" %in% rownames(installed.packages()))){ install.packages("RQDA",dependencies=c("Depends","Imports")) }
if(!("circlize" %in% rownames(installed.packages()))){ install.packages("circlize",dependencies=c("Depends","Imports")) }
if(!("stringr" %in% rownames(installed.packages()))){ install.packages("stringr",dependencies=c("Depends","Imports")) }
if(!("rpart" %in% rownames(installed.packages()))){ install.packages("rpart",dependencies=c("Depends","Imports","Suggests")) }
if(!("rpart.plot" %in% rownames(installed.packages()))){ install.packages("rpart.plot",dependencies=c("Depends","Imports","Suggests")) }

library(rpart.plot)
library(rpart)
library(stringr)
library(foreign)
library(Hmisc)
library(gdata)
library(weights)
library(haven)
library(sjmisc)
library(xlsx)
library(dplyr)
library(tidyr)
library(reshape2)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(qdap)
library(RQDA)

library(haven)
library(sjmisc)
library(circlize)
library(alluvial)
#NO CRAN
if(!require("ghit")){
  install.packages("ghit")
}

ghit::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"))

install.packages("tesseract")

if (!require(devtools)) {
  install.packages("devtools")
}
devtools::install_github("corybrunson/ggalluvial")
library(ggalluvial)