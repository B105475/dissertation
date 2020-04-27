#Figure 3b, 4b
#east england % and raw frequency of isolates
library(plyr)
library(tidyverse)
####################
#clean script- set up
library(readxl)
Ludden <- read_excel("Edinburgh Yr 4/Diss/NEWall_0.03.xlsx")
   

Ludden1 <-data.frame(sapply(Ludden,function(x)as.character(gsub("^R_","",x))))
colnames(Ludden1)[1] <-"iso1"
colnames(Ludden1)[2] <-"iso2"
colnames(Ludden1)[5] <-"Genome"
colnames(Ludden1)[6] <-"host1"
colnames(Ludden1)[7] <-"host2"

#rename repeated
Ludden1$Genome<-as.character(Ludden1$Genome)
Ludden1$Genome[Ludden1$Genome == "aac6Ibcr-aac6Ibcr-blaOXA1-catB3"] <- "aac6Ibcr-blaOXA1-catB3"
Ludden1$Genome<-as.factor(Ludden1$Genome)
####################
#____= initial filtered groups
#____1= count+group from iso1
#____2= count+group from iso2
#rename columns in these
#____3= combine 1+2
#____4= regroup
#____5= recount and % freq
#____f= merge with allgene, convert to 0 (final), allgene is a list of all gene combo that appeared in this dataset

luddenhh <- Ludden1 %>% filter(host1=="human"&host2=="human")
luddencc <- Ludden1 %>% filter(host1=="bovine"&host2=="bovine")
luddenpp <- Ludden1 %>% filter(host1=="chicken"&host2=="chicken")
luddench <- Ludden1 %>% filter(host1=="bovine"&host2=="human"|host1=="human"&host2=="bovine")
luddenhp <- Ludden1 %>% filter(host1=="human"&host2=="chicken"|host1=="chicken"&host2=="human")

allgene <- read_excel("Edinburgh Yr 4/Diss/list of gene combos.xlsx")
View(allgene)

luddencc1 <- ddply(luddencc, .variables = c('Genome','iso1'), nrow)
luddencc2 <- ddply(luddencc, .variables = c('Genome','iso2'), nrow)
luddench1 <- ddply(luddench, .variables = c('Genome','iso1'), nrow)
luddench2 <- ddply(luddench, .variables = c('Genome','iso2'), nrow)
luddenhh1 <- ddply(luddenhh, .variables = c('Genome','iso1'), nrow)
luddenhh2 <- ddply(luddenhh, .variables = c('Genome','iso2'), nrow)
luddenhp1 <- ddply(luddenhp, .variables = c('Genome','iso1'), nrow)
luddenhp2 <- ddply(luddenhp, .variables = c('Genome','iso2'), nrow)
luddenpp1 <- ddply(luddenpp, .variables = c('Genome','iso1'), nrow)
luddenpp2 <- ddply(luddenpp, .variables = c('Genome','iso2'), nrow)

colnames(luddencc1) <- c("Genome", "Isolate", "Sharing")
colnames(luddencc2) <- c("Genome", "Isolate", "Sharing")
colnames(luddench1) <- c("Genome", "Isolate", "Sharing")
colnames(luddench2) <- c("Genome", "Isolate", "Sharing")
colnames(luddenhh1) <- c("Genome", "Isolate", "Sharing")
colnames(luddenhh2) <- c("Genome", "Isolate", "Sharing")
colnames(luddenhp1) <- c("Genome", "Isolate", "Sharing")
colnames(luddenhp2) <- c("Genome", "Isolate", "Sharing")
colnames(luddenpp1) <- c("Genome", "Isolate", "Sharing")
colnames(luddenpp2) <- c("Genome", "Isolate", "Sharing")

luddencc3 <- rbind(luddencc1,luddencc2)
luddench3 <- rbind(luddench1,luddench2)
luddenhh3 <- rbind(luddenhh1,luddenhh2)
luddenhp3 <- rbind(luddenhp1,luddenhp2)
luddenpp3 <- rbind(luddenpp1,luddenpp2)

luddencc4 <- luddencc3 %>%group_by(Genome,Isolate) %>% summarise("freq"=sum(Sharing))
luddench4 <- luddench3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))
luddenhh4 <- luddenhh3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))
luddenhp4 <- luddenhp3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))
luddenpp4 <- luddenpp3 %>%group_by(Genome,Isolate) %>% summarise('freq'=sum(Sharing))

luddencc5 <- count(luddencc4)
luddench5 <- count(luddench4)
luddenhh5 <- count(luddenhh4)
luddenhp5 <- count(luddenhp4)
luddenpp5 <- count(luddenpp4)

luddencc5 <- luddencc5 %>% mutate("%"= n/sum(luddencc5$n)*100)
luddench5 <- luddench5 %>% mutate("%"= n/sum(luddench5$n)*100)
luddenhh5 <- luddenhh5 %>% mutate("%"= n/sum(luddenhh5$n)*100)
luddenhp5 <- luddenhp5 %>% mutate("%"= n/sum(luddenhp5$n)*100)
luddenpp5 <- luddenpp5 %>% mutate("%"= n/sum(luddenpp5$n)*100)

luddenccf <- merge(allgene,luddencc5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
luddenchf <- merge(allgene,luddench5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
luddenhhf <- merge(allgene,luddenhh5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
luddenhpf <- merge(allgene,luddenhp5, by.x='Contig gene',by.y='Genome',all.x=TRUE)
luddenppf <- merge(allgene,luddenpp5, by.x='Contig gene',by.y='Genome',all.x=TRUE)

luddenccf[is.na(luddenccf)] <- 0
luddenchf[is.na(luddenchf)] <- 0
luddenhhf[is.na(luddenhhf)] <- 0
luddenhpf[is.na(luddenhpf)] <- 0
luddenppf[is.na(luddenppf)] <- 0

luddenhpf <- allgene
luddenhpf$n <- 0
luddenhpf$"%" <- 0

ggplot(data=luddenccf, aes(x=luddenccf$`Contig gene`,y=luddenccf$`%`))+geom_bar(stat="identity",fill="red")+coord_flip(ylim=c(0,100))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("cattle-cattle (n=21)")
ggplot(data=luddenchf, aes(x=luddenchf$`Contig gene`,y=luddenchf$`%`))+geom_bar(stat="identity",fill="blue")+coord_flip(ylim=c(0,100))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("cattle-human (n=78)")
ggplot(data=luddenhhf, aes(x=luddenhhf$`Contig gene`,y=luddenhhf$`%`))+geom_bar(stat="identity",fill="green")+coord_flip(ylim=c(0,100))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("human-human (n=105)")
ggplot(data=luddenhpf, aes(x=luddenhpf$`Contig gene`,y=luddenhpf$`%`))+geom_bar(stat="identity",fill="purple")+coord_flip(ylim=c(0,100))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("human-poultry (n=0)")
ggplot(data=luddenppf, aes(x=luddenppf$`Contig gene`,y=luddenppf$`%`))+geom_bar(stat="identity",fill="orange")+coord_flip(ylim=c(0,100))+xlab("Gene Combinations")+ylab("Relative Frequency of Isolates (%)")+ggtitle("poultry-poultry (n=28)")


##############################
#Raw frequency graphs

ggplot(data=luddenccf, aes(x=luddenccf$`Contig gene`,y=luddenccf$`n`))+geom_bar(stat="identity",fill="red")+coord_flip(ylim=c(0,80))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("cattle-cattle (n=21)")
ggplot(data=luddenchf, aes(x=luddenchf$`Contig gene`,y=luddenchf$`n`))+geom_bar(stat="identity",fill="blue")+coord_flip(ylim=c(0,80))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("cattle-human (n=78)")
ggplot(data=luddenhhf, aes(x=luddenhhf$`Contig gene`,y=luddenhhf$`n`))+geom_bar(stat="identity",fill="green")+coord_flip(ylim=c(0,80))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("human-human (n=105)")
ggplot(data=luddenhpf, aes(x=luddenhpf$`Contig gene`,y=luddenhpf$`n`))+geom_bar(stat="identity",fill="purple")+coord_flip(ylim=c(0,80))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("human-poultry (n=0)")
ggplot(data=luddenppf, aes(x=luddenppf$`Contig gene`,y=luddenppf$`n`))+geom_bar(stat="identity",fill="orange")+coord_flip(ylim=c(0,80))+xlab("Gene Combinations")+ylab("Basic Frequency of Isolates")+ggtitle("poultry-poultry(n=28)")















