#Figure 3a, 4a
# dutch % and raw frequency of isolates
library(plyr)
library(tidyverse)
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

#merge with source
library(readxl)Source <- read_excel("Edinburgh Yr 4/Diss/Source final.xlsx")
colnames(Source)[1] <-"Genome"
colnames(Source)[2] <-"Source"

WS2 <- merge(WS1,Source,by.x = 'iso1',by.y='Genome',all.x = TRUE)
WS2 <- merge(WS2,Source,by.x = 'iso2',by.y='Genome',all.x = TRUE)

#rename repeated
WS2$Genome<-as.character(WS2$Genome)
WS2$Genome[WS2$Genome == "aac6Ibcr-aac6Ibcr-blaOXA1-catB3"] <- "aac6Ibcr-blaOXA1-catB3"
WS2$Genome<-as.factor(WS2$Genome)

####################
#____= initial filtered groups
#____1= count+group from iso1
#____2= count+group from iso2
#rename columns in these
#____3= combine 1+2
#____4= regroup
#____5= recount and % freq
#____f= merge with allgene, convert to 0 (final), allgene is a list of all gene combo that appeared in this dataset

allgene <- read_excel("Edinburgh Yr 4/Diss/list of gene combos.xlsx")
View(allgene)
allgene$`Contig gene`<-as.character(allgene$`Contig gene`)
allgene$`Contig gene`[allgene$`Contig gene` == "aac6Ibcr-aac6Ibcr-blaOXA1-catB3"] <- "aac6Ibcr-blaOXA1-catB3"
allgene$`Contig gene`<-as.factor(allgene$`Contig gene`)

dutchhh <- WS2 %>% filter(Source.x=="human"&Source.y=="human")
dutchcc <- WS2 %>% filter(Source.x=="cattle"&Source.y=="cattle")
dutchpp <- WS2 %>% filter(Source.x=="poultry"&Source.y=="poultry")
dutchch <- WS2 %>% filter(Source.x=="cattle"&Source.y=="human"|Source.x=="human"&Source.y=="cattle")
dutchhp <- WS2 %>% filter(Source.x=="human"&Source.y=="poultry"|Source.x=="poultry"&Source.y=="human")
dutchhp <- WS2 %>% filter(Source.x=="human"&Source.y=="poultry"|Source.x=="poultry"&Source.y=="human")


dutchcc1 <- ddply(dutchcc, .variables = c('Genome','iso1'), nrow)
dutchcc2 <- ddply(dutchcc, .variables = c('Genome','iso2'), nrow)
dutchch1 <- ddply(dutchch, .variables = c('Genome','iso1'), nrow)
dutchch2 <- ddply(dutchch, .variables = c('Genome','iso2'), nrow)
dutchhh1 <- ddply(dutchhh, .variables = c('Genome','iso1'), nrow)
dutchhh2 <- ddply(dutchhh, .variables = c('Genome','iso2'), nrow)
dutchhp1 <- ddply(dutchhp, .variables = c('Genome','iso1'), nrow)
dutchhh2 <- ddply(dutchhp, .variables = c('Genome','iso2'), nrow)
dutchpp1 <- ddply(dutchpp, .variables = c('Genome','iso1'), nrow)
dutchpp2 <- ddply(dutchpp, .variables = c('Genome','iso2'), nrow)

colnames(dutchcc1) <- c("Genome", "Isolate", "Sharing")
colnames(dutchcc2) <- c("Genome", "Isolate", "Sharing")
colnames(dutchch1) <- c("Genome", "Isolate", "Sharing")
colnames(dutchch2) <- c("Genome", "Isolate", "Sharing")
colnames(dutchhh1) <- c("Genome", "Isolate", "Sharing")
colnames(dutchhh2) <- c("Genome", "Isolate", "Sharing")
colnames(dutchhp1) <- c("Genome", "Isolate", "Sharing")
colnames(dutchhh2) <- c("Genome", "Isolate", "Sharing")
colnames(dutchpp1) <- c("Genome", "Isolate", "Sharing")
colnames(dutchpp2) <- c("Genome", "Isolate", "Sharing")

dutchcc3 <- rbind(dutchcc1,dutchcc2)
dutchch3 <- rbind(dutchch1,dutchch2)
dutchhh3 <- rbind(dutchhh1,dutchhh2)
dutchhp3 <- rbind(dutchhp1,dutchhh2)
dutchpp3 <- rbind(dutchpp1,dutchpp2)

dutchcc4 <- dutchcc3 %>%group_by(Genome,Isolate) %>% summarise("freq"=sum(Sharing))
dutchch4 <- dutchch3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))
dutchhh4 <- dutchhh3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))
dutchhp4 <- dutchhp3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))
dutchpp4 <- dutchpp3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))

dutchcc5 <- count(dutchcc4)
dutchch5 <- count(dutchch4)
dutchhh5 <- count(dutchhh4)
dutchhp5 <- count(dutchhp4)
dutchpp5 <- count(dutchpp4)

dutchcc5 <- dutchcc5 %>% mutate("%"= n/sum(dutchcc5$n)*100)
dutchch5 <- dutchch5 %>% mutate("%"= n/sum(dutchch5$n)*100)
dutchhh5 <- dutchhh5 %>% mutate("%"= n/sum(dutchhh5$n)*100)
dutchhp5 <- dutchhp5 %>% mutate("%"= n/sum(dutchhp5$n)*100)
dutchpp5 <- dutchpp5 %>% mutate("%"= n/sum(dutchpp5$n)*100)

dutchccf <- merge(allgene,dutchcc5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
dutchchf <- merge(allgene,dutchch5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
dutchhhf <- merge(allgene,dutchhh5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
dutchhpf <- merge(allgene,dutchhp5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
dutchppf <- merge(allgene,dutchpp5, by.x='Contig gene',by.y='Genome',all.x=TRUE)

dutchccf[is.na(dutchccf)] <- 0
dutchchf[is.na(dutchchf)] <- 0
dutchhhf[is.na(dutchhhf)] <- 0
dutchhpf[is.na(dutchhpf)] <- 0
dutchppf[is.na(dutchppf)] <- 0


#GENERATED GRAPHS for dutch

ggplot(data=dutchccf, aes(x=dutchccf$`Contig gene`,y=dutchccf$`%`))+geom_bar(stat="identity",fill="red")+coord_flip(ylim=c(0,45))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("cattle-cattle (n=34)")
ggplot(data=dutchchf, aes(x=dutchchf$`Contig gene`,y=dutchchf$`%`))+geom_bar(stat="identity",fill="blue")+coord_flip(ylim=c(0,45))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("cattle-human (n=128)")
ggplot(data=dutchhhf, aes(x=dutchhhf$`Contig gene`,y=dutchhhf$`%`))+geom_bar(stat="identity",fill="green")+coord_flip(ylim=c(0,45))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("human-human (n=173)")
ggplot(data=dutchhpf, aes(x=dutchhpf$`Contig gene`,y=dutchhpf$`%`))+geom_bar(stat="identity",fill="purple")+coord_flip(ylim=c(0,45))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("human-poultry (n=35)")
ggplot(data=dutchppf, aes(x=dutchppf$`Contig gene`,y=dutchppf$`%`))+geom_bar(stat="identity",fill="orange")+coord_flip(ylim=c(0,45))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("poultry-poultry (n=53)")


##############################
#Raw frequency graphs

ggplot(data=dutchccf, aes(x=dutchccf$`Contig gene`,y=dutchccf$`n`))+geom_bar(stat="identity",fill="red")+coord_flip(ylim=c(0,65))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("cattle-cattle (n=34)")
ggplot(data=dutchchf, aes(x=dutchchf$`Contig gene`,y=dutchchf$`n`))+geom_bar(stat="identity",fill="blue")+coord_flip(ylim=c(0,65))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("cattle-human (n=128)")
ggplot(data=dutchhhf, aes(x=dutchhhf$`Contig gene`,y=dutchhhf$`n`))+geom_bar(stat="identity",fill="green")+coord_flip(ylim=c(0,65))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("human-human (n=173)")
ggplot(data=dutchhpf, aes(x=dutchhpf$`Contig gene`,y=dutchhpf$`n`))+geom_bar(stat="identity",fill="purple")+coord_flip(ylim=c(0,65))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("human-poultry (n=35)")
ggplot(data=dutchppf, aes(x=dutchppf$`Contig gene`,y=dutchppf$`n`))+geom_bar(stat="identity",fill="orange")+coord_flip(ylim=c(0,65))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("poultry-poultry(n=53)")














