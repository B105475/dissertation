#Figure 1b
################# Characteristic table
crosseng <-table(eng2kb$Host1,eng2kb$Host2)

ecattlecattle <- eng2kb %>% filter(Host1=="bovine"&Host2=="bovine")
ecattlehuman <- eng2kb %>% filter(Host1=="bovine"&Host2=="human"|Host1=="human"&Host2=="bovine")
ecattlemeat <- eng2kb %>% filter(Host1=="bovine"&Host2=="meat"|Host1=="meat"&Host2=="bovine")
ecattlepoultry <- eng2kb %>% filter(Host1=="bovine"&Host2=="chicken"|Host1=="chicken"&Host2=="bovine")
ecattlepig <- eng2kb %>% filter(Host1=="bovine"&Host2=="pig"|Host1=="pig"&Host2=="bovine")
ecattleturk <- eng2kb %>% filter(Host1=="bovine"&Host2=="turkeyt"|Host1=="turkey"&Host2=="bovine")

ehumanhuman <- eng2kb %>% filter(Host1=="human"&Host2=="human")
ehumanmeat <- eng2kb %>% filter(Host1=="human"&Host2=="meat"|Host1=="meat"&Host2=="human")
ehumanpoultry <- eng2kb %>% filter(Host1=="human"&Host2=="chicken"|Host1=="chicken"&Host2=="human")
ehumanpig <- eng2kb %>% filter(Host1=="human"&Host2=="pig"|Host1=="pig"&Host2=="human")
ehumanturk <- eng2kb %>% filter(Host1=="human"&Host2=="turkey"|Host1=="turkey"&Host2=="human")

emeatmeat <- eng2kb %>% filter(Host1=="meat"&Host2=="meat")
emeatpoultry <- eng2kb %>% filter(Host1=="meat"&Host2=="chicken"|Host1=="chicken"&Host2=="meat")
emeatpig <- eng2kb %>% filter(Host1=="meat"&Host2=="pig"|Host1=="pig"&Host2=="meat")
emeatturk <- eng2kb %>% filter(Host1=="meat"&Host2=="turkey"|Host1=="turkey"&Host2=="meat")

epoultrypoultry <- eng2kb %>% filter(Host1=="chicken"&Host2=="chicken")
epoultrypig <- eng2kb %>% filter(Host1=="chicken"&Host2=="pig"|Host1=="pig"&Host2=="chicken")
epoultryturk <- eng2kb %>% filter(Host1=="chicken"&Host2=="turkey"|Host1=="turkey"&Host2=="chicken")

epigpig <- eng2kb %>% filter(Host1=="pig"&Host2=="pig")
epigturk <- eng2kb %>% filter(Host1=="pig"&Host2=="turkey"|Host1=="turkey"&Host2=="pig")

eturkturk <- eng2kb %>% filter(Host1=="turkey"&Host2=="turkey")

#Finding isolate
eisoinvolvedcc1 <- data.frame(ecattlecattle$Isolate.1)
eisoinvolvedcc2 <- data.frame(ecattlecattle$Isolate.2)
colnames(eisoinvolvedcc1) <-"Isolate"
colnames(eisoinvolvedcc2) <-"Isolate"
eisoinvolvedcc <- rbind(eisoinvolvedcc1,eisoinvolvedcc2)
eisoinvolvedcc <- eisoinvolvedcc %>%group_by(Isolate) %>% summarise()

eisoinvolvedch1 <- data.frame(ecattlehuman$Isolate.1)
eisoinvolvedch2 <- data.frame(ecattlehuman$Isolate.2)
colnames(eisoinvolvedch1) <-"Isolate"
colnames(eisoinvolvedch2) <-"Isolate"
eisoinvolvedch <- rbind(eisoinvolvedch1,eisoinvolvedch2)
eisoinvolvedch <- eisoinvolvedch %>%group_by(Isolate) %>% summarise()

eisoinvolvedcm1 <-data.frame(ecattlemeat$Isolate.1)
eisoinvolvedcm2 <-data.frame(ecattlemeat$Isolate.2)
colnames(eisoinvolvedcm1) <-"Isolate"
colnames(eisoinvolvedcm2) <-"Isolate"
eisoinvolvedcm <- rbind(eisoinvolvedcm1,eisoinvolvedcm2)
eisoinvolvedcm <- eisoinvolvedcm %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedcpi1 <-data.frame(ecattlepig$Isolate.1)
eisoinvolvedcpi2 <-data.frame(ecattlepig$X1)
colnames(eisoinvolvedcpi1) <-"Isolate"
colnames(eisoinvolvedcpi2) <-"Isolate"
eisoinvolvedcpi <- rbind(eisoinvolvedcpi1,eisoinvolvedcpi2)
eisoinvolvedcpi <- eisoinvolvedcpi %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedcpo1 <- data.frame(ecattlepoultry$Isolate.1)
eisoinvolvedcpo2 <- data.frame(ecattlepoultry$Isolate.2)
colnames(eisoinvolvedcpo1) <-"Isolate"
colnames(eisoinvolvedcpo2) <-"Isolate"
eisoinvolvedcpo <- rbind(eisoinvolvedcpo1,eisoinvolvedcpo2)
eisoinvolvedcpo <- eisoinvolvedcpo %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedct1 <-data.frame(ecattleturk$Isolate.1)
eisoinvolvedct2 <-data.frame(ecattleturk$Isolate.2)
colnames(eisoinvolvedct1) <-"Isolate"
colnames(eisoinvolvedct2) <-"Isolate"
eisoinvolvedct <- rbind(eisoinvolvedct1,eisoinvolvedct2)
eisoinvolvedct <- eisoinvolvedct %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedhh1 <- data.frame(ehumanhuman$Isolate.1)
eisoinvolvedhh2 <- data.frame(ehumanhuman$Isolate.2)
colnames(eisoinvolvedhh1) <-"Isolate"
colnames(eisoinvolvedhh2) <-"Isolate"
eisoinvolvedhh <- rbind(eisoinvolvedhh1,eisoinvolvedhh2)
eisoinvolvedhh <- eisoinvolvedhh %>%group_by(Isolate) %>% summarise()

eisoinvolvedhm1 <- data.frame(ehumanmeat$Isolate.1)
eisoinvolvedhm2 <- data.frame(ehumanmeat$Isolate.2) 
colnames(eisoinvolvedhm1) <-"Isolate"
colnames(eisoinvolvedhm2) <-"Isolate"
eisoinvolvedhm <- rbind(eisoinvolvedhm1,eisoinvolvedhm2)
eisoinvolvedhm <- eisoinvolvedhm %>%group_by(Isolate) %>% summarise()

eisoinvolvedhpo1 <- data.frame(ehumanpoultry$Isolate.1)
eisoinvolvedhpo2 <- data.frame(ehumanpoultry$Isolate.2) 
colnames(eisoinvolvedhpo1) <-"Isolate"
colnames(eisoinvolvedhpo2) <-"Isolate"
eisoinvolvedhpo <- rbind(eisoinvolvedhpo1,eisoinvolvedhpo2)
eisoinvolvedhpo <- eisoinvolvedhpo %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedhpi1 <- data.frame(ehumanpig$Isolate.1)
eisoinvolvedhpi2 <- data.frame(ehumanpig$Isolate.2) 
colnames(eisoinvolvedhpi1) <-"Isolate"
colnames(eisoinvolvedhpi2) <-"Isolate"
eisoinvolvedhpi <- rbind(eisoinvolvedhpi1,eisoinvolvedhpi2)
eisoinvolvedhpi <- eisoinvolvedhpi %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedht1 <- data.frame(ehumanturk$Isolate.1)
eisoinvolvedht2 <- data.frame(ehumanturk$Isolate.2) 
colnames(eisoinvolvedht1) <-"Isolate"
colnames(eisoinvolvedht2) <-"Isolate"
eisoinvolvedht <- rbind(eisoinvolvedht1,eisoinvolvedht2)
eisoinvolvedht <- eisoinvolvedht %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedmm1 <- data.frame(emeatmeat$Isolate.1)
eisoinvolvedmm2 <- data.frame(emeatmeat$Isolate.2) 
colnames(eisoinvolvedmm1) <-"Isolate"
colnames(eisoinvolvedmm2) <-"Isolate"
eisoinvolvedmm <- rbind(eisoinvolvedmm1,eisoinvolvedmm2)
eisoinvolvedmm <- eisoinvolvedmm %>%group_by(Isolate) %>% summarise()

eisoinvolvedmpo1 <- data.frame(emeatpoultry$Isolate.1)
eisoinvolvedmpo2 <- data.frame(emeatpoultry$Isolate.2)
colnames(eisoinvolvedmpo1) <-"Isolate"
colnames(eisoinvolvedmpo2) <-"Isolate"
eisoinvolvedmpo <- rbind(eisoinvolvedmpo1,eisoinvolvedmpo2)
eisoinvolvedmpo <- eisoinvolvedmpo %>%group_by(Isolate) %>% summarise()

eisoinvolvedmpi1 <- data.frame(emeatpig$Isolate.1)
eisoinvolvedmpi2 <- data.frame(emeatpig$Isolate.2)
colnames(eisoinvolvedmpi1) <-"Isolate"
colnames(eisoinvolvedmpi2) <-"Isolate"
eisoinvolvedmpi <- rbind(eisoinvolvedmpi1,eisoinvolvedmpi2)
eisoinvolvedmpi <- eisoinvolvedmpi %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedmt1 <- data.frame(emeatturk$Isolate.1)
eisoinvolvedmt2 <- data.frame(emeatturk$Isolate.2)
colnames(eisoinvolvedmt1) <-"Isolate"
colnames(eisoinvolvedmt2) <-"Isolate"
eisoinvolvedmt <- rbind(eisoinvolvedmt1,eisoinvolvedmt2)
eisoinvolvedmt <- eisoinvolvedmt %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedpopo1 <- data.frame(epoultrypoultry$Isolate.1)
eisoinvolvedpopo2 <- data.frame(epoultrypoultry$Isolate.2)
colnames(eisoinvolvedpopo1) <-"Isolate"
colnames(eisoinvolvedpopo2) <-"Isolate"
eisoinvolvedpopo <- rbind(eisoinvolvedpopo1,eisoinvolvedpopo2)
eisoinvolvedpopo <- eisoinvolvedpopo %>%group_by(Isolate) %>% summarise()

eisoinvolvedpopi1 <- data.frame(epoultrypig$Isolate.1)
eisoinvolvedpopi2 <- data.frame(epoultrypig$Isolate.2)
colnames(eisoinvolvedpopi1) <-"Isolate"
colnames(eisoinvolvedpopi2) <-"Isolate"
eisoinvolvedpopi <- rbind(eisoinvolvedpopi1,eisoinvolvedpopi2)
eisoinvolvedpopi <- eisoinvolvedpopi %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedpot1 <- data.frame(epoultryturk$Isolate.1)
eisoinvolvedpot2 <- data.frame(epoultryturk$Isolate.2)
colnames(eisoinvolvedpot1) <-"Isolate"
colnames(eisoinvolvedpocomp2) <-"Isolate"
eisoinvolvedpot <- rbind(eisoinvolvedpot1,eisoinvolvedpt2)
eisoinvolvedpot <- eisoinvolvedpot %>%group_by(Isolate) %>% summarise()
# 0 cases#

eisoinvolvedpipi1 <- data.frame(epigpig$Isolate.1)
eisoinvolvedpipi2 <- data.frame(epigpig$Isolate.2)
colnames(eisoinvolvedpipi1) <-"Isolate"
colnames(eisoinvolvedpipi2) <-"Isolate"
eisoinvolvedpipi <- rbind(eisoinvolvedpipi1,eisoinvolvedpipi2)
eisoinvolvedpipi <- eisoinvolvedpipi %>%group_by(Isolate) %>% summarise()

eisoinvolvedpit1 <- data.frame(epigturk$Isolate.1)
eisoinvolvedpit2 <- data.frame(epigturk$Isolate.2)
colnames(eisoinvolvedpit1) <-"Isolate"
colnames(eisoinvolvedpit2) <-"Isolate"
eisoinvolvedpit <- rbind(eisoinvolvedpit1,eisoinvolvedpit2)
eisoinvolvedpit <- eisoinvolvedpit %>%group_by(Isolate) %>% summarise()

eisoinvolvedtt1 <- data.frame(eturkturk$Isolate.1)
eisoinvolvedtt2 <- data.frame(eturkturk$Isolate.2)
colnames(eisoinvolvedtt1) <-"Isolate"
colnames(eisoinvolvedtt2) <-"Isolate"
eisoinvolvedtt <- rbind(eisoinvolvedtt1,eisoinvolvedtt2)
eisoinvolvedtt <- eisoinvolvedtt %>%group_by(Isolate) %>% summarise()