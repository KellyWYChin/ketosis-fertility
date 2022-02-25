df.preg <- all_farm %>%
  select(FarmNo, CowId, BirthDate, CalvingDate, Lactation, CullDate, InsDate, InsNumber, NextIns, PregDate.x, PregDate.y, DeterminationType, AbortionDate, DaysToAbort)
summary(df.preg)
#Select the pregnancy determined by HN system (DeterminationType = 2)
df.preg1 <- df.preg[(df.preg$DeterminationType == 2 | is.na(df.preg$DeterminationType)), ]
summary(df.preg1)
#Select the pregnancy determined by HN system, Physical, and Visual abort (DeterminationType = 2, 3, and 4)
df.preg2 <- df.preg[(df.preg$DeterminationType == 2 | df.preg$DeterminationType == 3 | df.preg$DeterminationType == 4), ]
 #Find out that Determination Type 4 is confusing
#Remove rows with FarmNo = NA
df.preg3 <- df.preg1[!is.na(df.preg1$FarmNo), ]
summary(df.preg3)
df.preg1 <- df.preg3
rm(df.preg3)
rm(df.preg2)
summary(df.preg1)

#Remove column PregDate.x
df.preg1$PregDate.x <- NULL
summary(df.preg1)
################################################################################
### Check data consistency (PregDate - Calving Date) & (CullDate - Calving Date) ##################
################################################################################
df.preg1$CalvingDate <- as.Date(df.preg1$CalvingDate)
df.preg1$PregDate.y <- as.Date(df.preg1$PregDate.y)
df.preg1$CullDate <- as.Date(df.preg1$CullDate)
#date that CullDate is before 2018-06-01 should be nonsense in this dataset
df.preg1$CullDate[df.preg1$CullDate <= "2018-05-31"] <- NA
df.preg1$preg.calving <- difftime(df.preg1$PregDate.y, df.preg1$CalvingDate, units="days")
df.preg1$cull.calving <- difftime(df.preg1$CullDate, df.preg1$CalvingDate, units="days")
df.preg1$Culled <- ifelse(!is.na(df.preg1$cull.calving), 1, 0)
summary(df.preg1)
#exclude rows that have negative values of time between pregnancy and calving
df.preg1 <- df.preg1[(df.preg1$preg.calving >=0 | is.na(df.preg1$preg.calving)), ]
#exclude rows that have been culled within 150 DIM
df.preg1 <- df.preg1[-which(df.preg1$cull.calving < 150), ]

############ Check data consistency (PregDate - Insemination Date) #############
df.preg1$InsDate <- as.Date(df.preg1$InsDate)
df.preg1$preg.ins <- difftime(df.preg1$PregDate.y, df.preg1$InsDate, units="days")
df.preg1$preg.ins <- as.numeric(df.preg1$preg.ins)
df.preg1 <- df.preg1[(df.preg1$preg.ins >=0 | is.na(df.preg1$preg.ins)), ]
summary(df.preg1)
df.data3 <- df.preg1 %>%
  distinct(FarmNo, CowId)
############ Check (AbortionDate - PregDate) ###################################
df.preg1$AbortionDate <- as.Date(df.preg1$AbortionDate)
df.preg1$abort.preg <- difftime(df.preg1$AbortionDate, df.preg1$PregDate.y, units = "days")
df.preg1$abort.preg <- as.numeric(df.preg1$abort.preg)
summary(df.preg1)
str(df.preg1$DaysToAbort)
#exclude rows with negative DaysToAbort
df.preg1 <- df.preg1[(df.preg1$DaysToAbort >=0 | is.na(df.preg1$DaysToAbort)), ]

################################################################################
########### list the first insemination, one cow in only one row ###############
df.ins1 <- df.preg1 %>%
  group_by(FarmNo, CowId, Lactation, CalvingDate) %>%
  summarize(InsNumber_1 = min(InsNumber), PregDate_1 = min(PregDate.y), InsDate_1=min(InsDate), AbortionDate_1 = min(AbortionDate))

#list the last insemination, one cow in only one row
df.insN <- df.preg1 %>%
  group_by(FarmNo, CowId, Lactation, CalvingDate) %>%
  summarize(InsNumber_N = max(InsNumber), PregDate_N = max(PregDate.y), InsDate_N = max(InsDate), AbortionDate_N = max(AbortionDate))
summary(df.insN)
table(df.insN$InsNumber_N) #How many times does a cow be inseminated?
#number of cows in Number of AI
df.insN$InsNumber_N <- as.factor(df.insN$InsNumber_N)
ggplot(data=df.insN, aes(x=factor(InsNumber_N))) +
  geom_bar(stat="count", fill="lightblue") +
  stat_count(geom="text", size=3, aes(label=..count..)) +
  ggtitle("Number of Cows and Number of AI") + xlab("Number of AI")
table(df.ins1$InsNumber_1)
str(df.insN)
df.data <- merge(df.ins1, df.insN, by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.y=TRUE, sort=F)
df.table <- df.preg1 %>%
  distinct(FarmNo, CowId, Lactation, .keep_all=TRUE)

## TODO
#merge pregnancy data into one data.frame
preg <- data.frame()
preg <- merge(df.data, df.table, by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate'), all.y=TRUE, sort=F)
summary(preg)
table(preg$FarmNo)
preg$InsDate <- NULL
preg$InsNumber <- NULL
preg$PregDate.y <- NULL
preg$NextIns <- NULL
preg$AbortionDate <- NULL
rm(list=ls(pattern="df"))

#exclude duplicated cows
preg$farmcow <- paste(preg$FarmNo, preg$CowId, sep="")
preg <- preg %>%
  mutate(farmcow.dup = ifelse(duplicated(farmcow)|
                                duplicated(farmcow, fromLast=TRUE), 1, 0))
sum(preg$farmcow.dup)
preg <- preg[(preg$farmcow.dup == 0), ]
preg$farmcow.dup <- NULL
preg$farmcow <- NULL
summary(preg)

##################### Define pregnancy status ##################################
preg$PregStatus <- ifelse((preg$PregDate_N==TRUE), NA, "1")
preg$PregStatus <- ifelse((is.na(preg$PregDate_N) & !is.na(preg$InsDate_N)), "0", preg$PregStatus)
table(preg$PregStatus)

#################### Define endtime ############################################
#calculate the time difference between insemination date and pregnancy date
preg$PregDate_N <- as.Date(preg$PregDate_N)
preg$InsDate_N <- as.Date(preg$InsDate_N)
preg$ins.preg <- as.numeric(difftime(preg$PregDate_N, preg$InsDate_N, units="days"))
summary(preg$ins.preg1)
preg$ins.preg <- ifelse((preg$InsNumber_N==1), difftime(preg$PregDate_1, preg$InsDate_1, units="days"),
                        difftime(preg$PregDate_N, preg$InsDate_N, units="days"))
preg$ins.preg <- as.numeric(preg$ins.preg)
summary(preg$ins.preg)
preg$ins.preg1 <- ifelse((preg$ins.preg > 300), difftime(preg$PregDate_1, preg$InsDate_N, units="days"),
                         preg$ins.preg)

#calculate the time difference between last insemination date and calving date
preg$Endtime <- as.numeric(difftime(preg$InsDate_N, preg$CalvingDate, units="days"))
summary(preg$Endtime)
hist(preg$Endtime)

preg$Endtime1 <- ifelse((preg$InsNumber_N==1), difftime(preg$InsDate_1, preg$CalvingDate, units="days"),
                        difftime(preg$InsDate_N, preg$CalvingDate, units="days"))
preg$diffEndtime <- NULL
preg$Endtime1 <- NULL

################# considering abortion date ####################################
#cows that abort after 1st insemination
preg$ins.abo1 <- as.numeric(difftime(preg$AbortionDate_1, preg$InsDate_1, units="days"))
#cows that abort after last insemination
preg$ins.aboN <- as.numeric(difftime(preg$AbortionDate_N, preg$InsDate_N, units="days"))

################ whether the cow gets pregnant in the first AI?#################
preg$FirstAI_DFC <- as.numeric(difftime(preg$InsDate_1, preg$CalvingDate, units="days"))
preg$PregStatus_1 <- ifelse(!(preg$InsNumber_N==1), 0, 1)

################ Reproduction events ###########################################
repro <- all_farm %>%
  select(FarmNo, CowId, BirthDate, CalvingDate, Lactation, DFC_CLA, DFCFirstHeat, DaysToFirstIns, DaysToLastIns, DaysBtwFirstAndLastIns) %>%
  distinct(FarmNo, CowId, Lactation, .keep_all=TRUE)

#exclude duplicated cows
repro$farmcow <- paste(repro$FarmNo, repro$CowId, sep="")
repro <- repro %>%
  mutate(farmcow.dup = ifelse(duplicated(farmcow)|
                                duplicated(farmcow, fromLast=TRUE), 1, 0))
repro <- repro[(repro$farmcow.dup == 0), ]
repro$farmcow.dup <- NULL
repro$farmcow <- NULL
summary(repro)
#exclude cows without FarmNo
repro <- repro[!is.na(repro$FarmNo), ]
table(repro$FarmNo)
table(preg$FarmNo)

reproevents <- merge(preg, repro, by=c('FarmNo', 'CowId', 'Lactation', 'CalvingDate', 'BirthDate'), all.x=TRUE, sort=F)
summary(reproevents)

####### Data exclusion #########################################################

# Exclude cows without DFC_CLA
reproevents <- reproevents[!is.na(reproevents$DFC_CLA), ]
# Exclude cows with DFC_CLA less than 20 and more than 200
reproevents <- reproevents[-which(reproevents$DFC_CLA < 20 | reproevents$DFC_CLA > 200), ]
summary(reproevents)

# Exclude cows DaysToFirstIns <20 DIM and >250 DIM
reproevents <- reproevents[-which(reproevents$DaysToFirstIns < 20 | reproevents$DaysToFirstIns >250), ]

# Exclude cows DFCFirstHeat <20 DIM and >200 DIM
reproevents <- reproevents[-which(reproevents$DFCFirstHeat < 20 | reproevents$DFCFirstHeat > 200), ]

# time from first AI to first detected cyclic
reproevents$Ins.Calving <- as.numeric(difftime(reproevents$InsDate_1, reproevents$CalvingDate, units="days"))
# time between first AI to DFC_CLA
reproevents$FirstAItoFirstCLA <- as.numeric(reproevents$Ins.Calving-reproevents$DFC_CLA)
summary(reproevents)
# time between first AI to first detected heat (DFCFirstHeat)
reproevents$FirstAItoFirstHeat <- as.numeric(reproevents$Ins.Calving-reproevents$DFCFirstHeat)
summary(reproevents)
hist(reproevents$FirstAItoFirstCLA)

# Check data consistency
summary(reproevents$Ins.Calving)
summary(reproevents$FirstAItoFirstCLA)
summary(reproevents$FirstAItoFirstHeat)

# Exclude inconsistent cows
df.repro1 <- reproevents[-which(reproevents$FirstAItoFirstCLA < 0), ]
summary(df.repro1$FirstAItoFirstCLA)
summary(df.repro1$FirstAItoFirstHeat)

df.repro1 <- df.repro1[-which(df.repro1$FirstAItoFirstHeat < -1), ]
summary(df.repro1$FirstAItoFirstHeat)

repro <- df.repro1

#################### Define seasons ############################################
getSeason <- function(DATES){
  WS <- as.Date("2012-1-1", format="%Y-%m-%d")
  SE <- as.Date("2012-4-1", format="%Y-%m-%d")
  SS <- as.Date("2012-7-1", format="%Y-%m-%d")
  FE <- as.Date("2012-10-1", format="%Y-%m-%d")

  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  ifelse(d >= WS & d < SE, "Winter",
         ifelse(d >= SE & d < SS, "Spring",
                ifelse(d >= SS & d < FE, "Summer", "Fall")))
}
reproevents <- reproevents %>%
  add_column(Season =
               getSeason(reproevents$CalvingDate), .after="CalvingDate")
reproevents$`Season <- getSeason(reproevents$CalvingDate)` <- NULL
reproevents$Season.1 <- NULL
################################################################################
#########create a new variable ECC (early/late conceived cows)##################
str(reproevents)
reproevents$ECC <- ifelse((reproevents$Ins.Calving <= 80 & reproevents$PregStatus_1 == "1"), 1, 0)
table(reproevents$FarmNo)
df.reproevents <- reproevents %>%
  group_by(FarmNo) %>%
  summarize(CowNo = n())
summary(df.reproevents)
sum(df.reproevents$CowNo)

################################################################################
########### time between culling and last&first AI #############################
reproevents$CullDate <- as.Date(reproevents$CullDate)
reproevents <- reproevents %>%
  add_column(cull.lastAI = difftime(reproevents$CullDate, reproevents$InsDate_N, units="days"), .after="cull.calving") %>%
  add_column(cull.firstAI = difftime(reproevents$CullDate, reproevents$InsDate_1, units="days"), .after="cull.calving")

summary(reproevents)

#################################################################################
####### Descriptive Statistics ##################################################
#################################################################################
ggplot(data=reproevents, aes(x=factor(CowId))) +
  geom_bar(stat="count", fill="orange") +
  stat_count(geom="text", size = 4, aes(label=..count..)) +
  ggtitle("No. of cows in 38 dairy farms") +
  xlab("Farm ID") +
  ylab("No. of Cows")

#Lactation distribution
ggplot(data=reproevents, aes(x=factor(Lactation))) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 4, aes(label=..count..)) +
  ggtitle("Parity distribution") + xlab("Parity")

#Season distribution
ggplot(data=reproevents, aes(x=factor(Season, level=c('Spring', 'Summer', 'Fall', 'Winter')))) +
  geom_bar(stat = "count", fill="orange")+
  stat_count(geom = "text", size= 4, aes(label=..count..)) +
  ggtitle("Number of cows in each calving season") +
  xlab("Calving Season")

#DFC_CLA
summary(reproevents$DFC_CLA)
ggplot(data=reproevents, aes(x=DFC_CLA)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(20, 200, by=30)) +
  ggtitle("Time from calving to resumption of cyclicity") +
  xlab("DIM")

#DFCFirstHeat
summary(reproevents$DFCFirstHeat)
ggplot(data=reproevents, aes(x = DFCFirstHeat)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(0, 320, by=50)) +
  ggtitle("Time from calving to first detected heat") +
  xlab("DIM")

#DaysToFirstIns
summary(reproevents$DaysToFirstIns)
ggplot(data=reproevents, aes(x = DaysToFirstIns)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(0, 400, by=50)) +
  ggtitle("Time from calving to first AI") +
  xlab("DIM")

#DaysToLastIns
summary(reproevents$DaysToLastIns)
ggplot(data=reproevents, aes(x = DaysToLastIns)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(0, 650, by=50)) +
  ggtitle("Time from calving to last AI") +
  xlab("DIM")


#DaysBtwFirstAndLastIns
summary(reproevents$DaysBtwFirstAndLastIns)
ggplot(data=reproevents, aes(x = DaysBtwFirstAndLastIns)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(0, 600, by=50)) +
  ggtitle("Time between the first and the last AI") +
  xlab("Days")

#FirstAItoFirstCLA
summary(reproevents$FirstAItoFirstCLA)
ggplot(data=reproevents, aes(x = FirstAItoFirstCLA)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(-250, 400, by=50)) +
  ggtitle("Time between the first resumption of cyclicity and the first AI") +
  xlab("Days")
#know how many FirstAItoFirstCLA are minus values
df.table <- reproevents %>%
  select(FarmNo, CowId, FirstAItoFirstCLA) %>%
  filter(FirstAItoFirstCLA >= 0) #4224 cows

#FirstAItoFirstHeat
summary(reproevents$FirstAItoFirstHeat)
ggplot(data=reproevents, aes(x=FirstAItoFirstHeat)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(-250, 350, by=50)) +
  ggtitle("Time betweeh the first heat and first AI") +
  xlab("Days")
df.table1 <- reproevents %>%
  select(FarmNo, CowId, FirstAItoFirstHeat) %>%
  filter(FirstAItoFirstHeat >= 0)

#pregnancy status after 1st AI
summary(reproevents$PregStatus_1)
sum(reproevents$PregStatus_1, na.rm=T)

#pregnancy status after last AI
reproevents$PregStatus <- as.numeric(reproevents$PregStatus)
summary(reproevents$PregStatus)
table(reproevents$PregStatus)
sum(reproevents$PregStatus, na.rm=T)

#ECC
table(reproevents$ECC)
summary(reproevents$ECC)

#endtime
summary(reproevents$Endtime)
ggplot(data=reproevents, aes(x=Endtime)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(0, 600, by=50)) +
  ggtitle("Time betweeh last AI and calving") +
  xlab("Days")

#culled cows
summary(reproevents$Culled)
reproevents$Culled <- as.numeric(reproevents$Culled)
table(reproevents$Culled)

#cull.calving
reproevents$cull.calving <- as.numeric(reproevents$cull.calving)
summary(reproevents$cull.calving)
ggplot(data=reproevents, aes(x=cull.calving)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(-5, 700, by=50)) +
  ggtitle("Time betweeh culling and calving") +
  xlab("Days")

#cull.firstAI
reproevents$cull.firstAI <- as.numeric(reproevents$cull.firstAI)
summary(reproevents$cull.firstAI)
ggplot(data=reproevents, aes(x=cull.firstAI)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(-220, 700, by=50)) +
  ggtitle("Time betweeh culling and the first AI") +
  xlab("Days")

#cull.lastAI
reproevents$cull.lastAI <- as.numeric(reproevents$cull.lastAI)
summary(reproevents$cull.lastAI)
ggplot(data=reproevents, aes(x=cull.lastAI)) +
  geom_histogram(col="red", fill="green", alpha=0.2) +
  scale_x_continuous(breaks = seq(-270, 650, by=50)) +
  ggtitle("Time betweeh culling and the last AI") +
  xlab("Days")


