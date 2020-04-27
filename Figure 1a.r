#investigating impact of host type in interspecies AMR contig sharing(transfer)
#compare inter and intraspecies sharing of each host type, do something similar to table 5
table(X2kbmergenew1$Sample_species.x,X2kbmergenew1$Sample_species.y)
#this showed us the tally for pair frequencies, can find % off of this

#make source material- all possible host pairs
mcattlecattle <- X2kbmergenew1 %>% filter(Sample_species.x=="cattle"&Sample_species.y=="cattle")
mcattlehuman <- X2kbmergenew1 %>% filter(Sample_species.x=="cattle"&Sample_species.y=="human"|Sample_species.x=="human"&Sample_species.y=="cattle")
mcattlemeat <- X2kbmergenew1 %>% filter(Sample_species.x=="cattle"&Sample_species.y=="meat"|Sample_species.x=="meat"&Sample_species.y=="cattle")
mcattlepoultry <- X2kbmergenew1 %>% filter(Sample_species.x=="cattle"&Sample_species.y=="poultry"|Sample_species.x=="poultry"&Sample_species.y=="cattle")
mcattlepig <- X2kbmergenew1 %>% filter(Sample_species.x=="cattle"&Sample_species.y=="pig"|Sample_species.x=="pig"&Sample_species.y=="cattle")
mcattlecomp <- X2kbmergenew1 %>% filter(Sample_species.x=="cattle"&Sample_species.y=="Dog/cat"|Sample_species.x=="Dog/cat"&Sample_species.y=="cattle")

mhumanhuman <- X2kbmergenew1 %>% filter(Sample_species.x=="human"&Sample_species.y=="human")
mhumanmeat <- X2kbmergenew1 %>% filter(Sample_species.x=="human"&Sample_species.y=="meat"|Sample_species.x=="meat"&Sample_species.y=="human")
mhumanpoultry <- X2kbmergenew1 %>% filter(Sample_species.x=="human"&Sample_species.y=="poultry"|Sample_species.x=="poultry"&Sample_species.y=="human")
mhumanpig <- X2kbmergenew1 %>% filter(Sample_species.x=="human"&Sample_species.y=="pig"|Sample_species.x=="pig"&Sample_species.y=="human")
mhumancomp <- X2kbmergenew1 %>% filter(Sample_species.x=="human"&Sample_species.y=="Dog/cat"|Sample_species.x=="Dog/cat"&Sample_species.y=="human")

mmeatmeat <- X2kbmergenew1 %>% filter(Sample_species.x=="meat"&Sample_species.y=="meat")
mmeatpoultry <- X2kbmergenew1 %>% filter(Sample_species.x=="meat"&Sample_species.y=="poultry"|Sample_species.x=="poultry"&Sample_species.y=="meat")
mmeatpig <- X2kbmergenew1 %>% filter(Sample_species.x=="meat"&Sample_species.y=="pig"|Sample_species.x=="pig"&Sample_species.y=="meat")
mmeatcomp <- X2kbmergenew1 %>% filter(Sample_species.x=="meat"&Sample_species.y=="Dog/cat"|Sample_species.x=="Dog/cat"&Sample_species.y=="meat")

mpoultrypoultry <- X2kbmergenew1 %>% filter(Sample_species.x=="poultry"&Sample_species.y=="poultry")
mpoultrypig <- X2kbmergenew1 %>% filter(Sample_species.x=="poultry"&Sample_species.y=="pig"|Sample_species.x=="pig"&Sample_species.y=="poultry")
mpoultrycomp <- X2kbmergenew1 %>% filter(Sample_species.x=="poultry"&Sample_species.y=="Dog/cat"|Sample_species.x=="Dog/cat"&Sample_species.y=="poultry")

mpigpig <- X2kbmergenew1 %>% filter(Sample_species.x=="pig"&Sample_species.y=="pig")
mpigcomp <- X2kbmergenew1 %>% filter(Sample_species.x=="pig"&Sample_species.y=="Dog/cat"|Sample_species.x=="Dog/cat"&Sample_species.y=="pig")

mcompcomp <- X2kbmergenew1 %>% filter(Sample_species.x=="Dog/cat"&Sample_species.y=="Dog/cat")


#finding isolates- trial with cc
isoinvolvedcc1 <- data.frame(mcattlecattle$X2)
isoinvolvedcc2 <- data.frame(mcattlecattle$X1)
colnames(isoinvolvedcc1) <-"Isolate"
colnames(isoinvolvedcc2) <-"Isolate"
isoinvolvedcc <- rbind(isoinvolvedcc1,isoinvolvedcc2)
isoinvolvedcc <- isoinvolvedcc %>%group_by(Isolate) %>% summarise()


#apply to the rest
isoinvolvedch1 <- data.frame(mcattlehuman$X2)
isoinvolvedch2 <- data.frame(mcattlehuman$X1)
colnames(isoinvolvedch1) <-"Isolate"
colnames(isoinvolvedch2) <-"Isolate"
isoinvolvedch <- rbind(isoinvolvedch1,isoinvolvedch2)
isoinvolvedch <- isoinvolvedch %>%group_by(Isolate) %>% summarise()

isoinvolvedcm1 <-data.frame(mcattlemeat$X2)
isoinvolvedcm2 <-data.frame(mcattlemeat$X1)
colnames(isoinvolvedcm1) <-"Isolate"
colnames(isoinvolvedcm2) <-"Isolate"
isoinvolvedcm <- rbind(isoinvolvedcm1,isoinvolvedcm2)
isoinvolvedcm <- isoinvolvedcm %>%group_by(Isolate) %>% summarise()

isoinvolvedcpi1 <-data.frame(mcattlepig$X2)
isoinvolvedcpi2 <-data.frame(mcattlepig$X1)
colnames(isoinvolvedcpi1) <-"Isolate"
colnames(isoinvolvedcpi2) <-"Isolate"
isoinvolvedcpi <- rbind(isoinvolvedcpi1,isoinvolvedcpi2)
isoinvolvedcpi <- isoinvolvedcpi %>%group_by(Isolate) %>% summarise()


isoinvolvedcpo1 <- data.frame(mcattlepoultry$X2)
isoinvolvedcpo2 <- data.frame(mcattlepoultry$X1)
colnames(isoinvolvedcpo1) <-"Isolate"
colnames(isoinvolvedcpo2) <-"Isolate"
isoinvolvedcpo <- rbind(isoinvolvedcpo1,isoinvolvedcpo2)
isoinvolvedcpo <- isoinvolvedcpo %>%group_by(Isolate) %>% summarise()

isoinvolvedccomp1 <-data.frame(mcattlecomp$X2)
isoinvolvedccomp2 <-data.frame(mcattlecomp$X1)
colnames(isoinvolvedccomp1) <-"Isolate"
colnames(isoinvolvedccomp2) <-"Isolate"
isoinvolvedccomp <- rbind(isoinvolvedccomp1,isoinvolvedccomp2)
isoinvolvedccomp <- isoinvolvedccomp %>%group_by(Isolate) %>% summarise()

isoinvolvedhh1 <- data.frame(mhumanhuman$X2)
isoinvolvedhh2 <- data.frame(mhumanhuman$X1)
colnames(isoinvolvedhh1) <-"Isolate"
colnames(isoinvolvedhh2) <-"Isolate"
isoinvolvedhh <- rbind(isoinvolvedhh1,isoinvolvedhh2)
isoinvolvedhh <- isoinvolvedhh %>%group_by(Isolate) %>% summarise()

isoinvolvedhm1 <- data.frame(mhumanmeat$X2)
isoinvolvedhm2 <- data.frame(mhumanmeat$X1) 
colnames(isoinvolvedhm1) <-"Isolate"
colnames(isoinvolvedhm2) <-"Isolate"
isoinvolvedhm <- rbind(isoinvolvedhm1,isoinvolvedhm2)
isoinvolvedhm <- isoinvolvedhm %>%group_by(Isolate) %>% summarise()

isoinvolvedhpo1 <- data.frame(mhumanpoultry$X2)
isoinvolvedhpo2 <- data.frame(mhumanpoultry$X1) 
colnames(isoinvolvedhpo1) <-"Isolate"
colnames(isoinvolvedhpo2) <-"Isolate"
isoinvolvedhpo <- rbind(isoinvolvedhpo1,isoinvolvedhpo2)
isoinvolvedhpo <- isoinvolvedhpo %>%group_by(Isolate) %>% summarise()

isoinvolvedhpi1 <- data.frame(mhumanpig$X2)
isoinvolvedhpi2 <- data.frame(mhumanpig$X1) 
colnames(isoinvolvedhpi1) <-"Isolate"
colnames(isoinvolvedhpi2) <-"Isolate"
isoinvolvedhpi <- rbind(isoinvolvedhpi1,isoinvolvedhpi2)
isoinvolvedhpi <- isoinvolvedhpi %>%group_by(Isolate) %>% summarise()

isoinvolvedhcomp1 <- data.frame(mhumancomp$X2)
isoinvolvedhcomp2 <- data.frame(mhumancomp$X1) 
colnames(isoinvolvedhcomp1) <-"Isolate"
colnames(isoinvolvedhcomp2) <-"Isolate"
isoinvolvedhcomp <- rbind(isoinvolvedhcomp1,isoinvolvedhcomp2)
isoinvolvedhcomp <- isoinvolvedhcomp %>%group_by(Isolate) %>% summarise()

isoinvolvedmm1 <- data.frame(mmeatmeat$X2)
isoinvolvedmm2 <- data.frame(mmeatmeat$X1) 
colnames(isoinvolvedmm1) <-"Isolate"
colnames(isoinvolvedmm2) <-"Isolate"
isoinvolvedmm <- rbind(isoinvolvedmm1,isoinvolvedmm2)
isoinvolvedmm <- isoinvolvedmm %>%group_by(Isolate) %>% summarise()

isoinvolvedmpo1 <- data.frame(mmeatpoultry$X2)
isoinvolvedmpo2 <- data.frame(mmeatpoultry$X1)
colnames(isoinvolvedmpo1) <-"Isolate"
colnames(isoinvolvedmpo2) <-"Isolate"
isoinvolvedmpo <- rbind(isoinvolvedmpo1,isoinvolvedmpo2)
isoinvolvedmpo <- isoinvolvedmpo %>%group_by(Isolate) %>% summarise()

isoinvolvedmpi1 <- data.frame(mmeatpig$X2)
isoinvolvedmpi2 <- data.frame(mmeatpig$X1)
colnames(isoinvolvedmpi1) <-"Isolate"
colnames(isoinvolvedmpi2) <-"Isolate"
isoinvolvedmpi <- rbind(isoinvolvedmpi1,isoinvolvedmpi2)
isoinvolvedmpi <- isoinvolvedmpi %>%group_by(Isolate) %>% summarise()

isoinvolvedmcomp1 <- data.frame(mmeatcomp$X2)
isoinvolvedmcomp2 <- data.frame(mmeatcomp$X1)
colnames(isoinvolvedmcomp1) <-"Isolate"
colnames(isoinvolvedmcomp2) <-"Isolate"
isoinvolvedmcomp <- rbind(isoinvolvedmcomp1,isoinvolvedmcomp2)
isoinvolvedmcomp <- isoinvolvedmcomp %>%group_by(Isolate) %>% summarise()

isoinvolvedpopo1 <- data.frame(mpoultrypoultry$X2)
isoinvolvedpopo2 <- data.frame(mpoultrypoultry$X1)
colnames(isoinvolvedpopo1) <-"Isolate"
colnames(isoinvolvedpopo2) <-"Isolate"
isoinvolvedpopo <- rbind(isoinvolvedpopo1,isoinvolvedpopo2)
isoinvolvedpopo <- isoinvolvedpopo %>%group_by(Isolate) %>% summarise()

isoinvolvedpopi1 <- data.frame(mpoultrypig$X2)
isoinvolvedpopi2 <- data.frame(mpoultrypig$X1)
colnames(isoinvolvedpopi1) <-"Isolate"
colnames(isoinvolvedpopi2) <-"Isolate"
isoinvolvedpopi <- rbind(isoinvolvedpopi1,isoinvolvedpopi2)
isoinvolvedpopi <- isoinvolvedpopi %>%group_by(Isolate) %>% summarise()

isoinvolvedpocomp1 <- data.frame(mpoultrycomp$X2)
isoinvolvedpocomp2 <- data.frame(mpoultrycomp$X1)
colnames(isoinvolvedpocomp1) <-"Isolate"
colnames(isoinvolvedpocomp2) <-"Isolate"
isoinvolvedpocomp <- rbind(isoinvolvedpocomp1,isoinvolvedpocomp2)
isoinvolvedpocomp <- isoinvolvedpocomp %>%group_by(Isolate) %>% summarise()

isoinvolvedpipi1 <- data.frame(mpigpig$X2)
isoinvolvedpipi2 <- data.frame(mpigpig$X1)
colnames(isoinvolvedpipi1) <-"Isolate"
colnames(isoinvolvedpipi2) <-"Isolate"
isoinvolvedpipi <- rbind(isoinvolvedpipi1,isoinvolvedpipi2)
isoinvolvedpipi <- isoinvolvedpipi %>%group_by(Isolate) %>% summarise()

isoinvolvedpicomp1 <- data.frame(mpigcomp$X2)
isoinvolvedpicomp2 <- data.frame(mpigcomp$X1)
colnames(isoinvolvedpicomp1) <-"Isolate"
colnames(isoinvolvedpicomp2) <-"Isolate"
isoinvolvedpicomp <- rbind(isoinvolvedpicomp1,isoinvolvedpicomp2)
isoinvolvedpicomp <- isoinvolvedpicomp %>%group_by(Isolate) %>% summarise()

isoinvolvedcompcomp1 <- data.frame(mcompcomp$X2)
isoinvolvedcompcomp2 <- data.frame(mcompcomp$X1)
colnames(isoinvolvedcompcomp1) <-"Isolate"
colnames(isoinvolvedcompcomp2) <-"Isolate"
isoinvolvedcompcomp <- rbind(isoinvolvedcompcomp1,isoinvolvedcompcomp2)
isoinvolvedcompcomp <- isoinvolvedcompcomp %>%group_by(Isolate) %>% summarise()



