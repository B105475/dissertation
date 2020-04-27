
####################
#clean script- set up
library(readr)
WS <- read_csv("Edinburgh Yr 4/Diss/all.max0.03.csv", 
               +     col_names = FALSE)
WS1 <-data.frame(sapply(WS,function(x)as.character(gsub("^R_","",x))))
colnames(WS1)[1] <-"iso1"
colnames(WS1)[2] <-"iso2"
colnames(WS1)[5] <-"Genome"
colnames(WS1)[6] <-"host1"
colnames(WS1)[7] <-"hos2"

#rename repeated
WS2$Genome<-as.character(WS2$Genome)
WS2$Genome[WS2$Genome == "aac6Ibcr-aac6Ibcr-blaOXA1-catB3"] <- "aac6Ibcr-blaOXA1-catB3"
WS2$Genome<-as.factor(WS2$Genome)


#merge with source
library(readxl)Source <- read_excel("Edinburgh Yr 4/Diss/Source final.xlsx")
colnames(Source)[1] <-"Genome"
colnames(Source)[2] <-"Source"

WS2 <- merge(WS1,Source,by.x = 'iso1',by.y='Genome',all.x = TRUE)
WS2 <- merge(WS2,Source,by.x = 'iso2',by.y='Genome',all.x = TRUE)
####################
#Basic characteristics
character1 <- ddply(WS2, .variables = c('iso1','Source.x'), nrow)
character2 <- ddply(WS2, .variables = c('iso2','Source.y'), nrow)
colnames(character1)<-c("Isolate","Source","freq")
colnames(character2)<-c("Isolate","Source","freq")
character <- rbind(character1,character2)
count(character$Source)

####################
#Basic characteriscs of the metadataset

count(Source$Source)


