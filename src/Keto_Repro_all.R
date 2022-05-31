###### farm 1001-merged file ######
df.calving <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.cowinfo <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "CowInfo")
df.culldata <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "CullData")
df.calving <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.insemination <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Insemination")
df.pregnancy <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Pregnancy")
df.milk <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "MilkData")
df.repro <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "ReproData")
df.keto <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Ketosis")
colnames(df.cowinfo)[colnames(df.cowinfo)=="CowID"] <- "CowId"
farm1001 <- data.frame()
farm1001 <- merge(df.cowinfo, df.calving, by = "CowId", all.y=TRUE) %>%
  merge(df.culldata, by = "CowId", all.x = TRUE) %>%
  merge(df.insemination, by = "CowId", all.x=TRUE, all.y=TRUE) %>%
  merge(df.pregnancy, by = "CowId", all.x=TRUE, all.y=TRUE)

###### farm 1038-merged file ######
df.calving38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.cowinfo38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "CowInfo")
#Change BirthDate into character
df.cowinfo38$BirthDate <- as.character(df.cowinfo38$BirthDate)
df.culldata38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "CullData")
df.calving38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Calving")
df.insemination38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Insemination")
df.pregnancy38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "Pregnancy")
#change the CowID in sheet cowinfo into CowId
colnames(df.cowinfo38)[colnames(df.cowinfo38)=="CowID"] <- "CowId"
df.master38 <- data.frame()
df.master38 <- merge(df.cowinfo38, df.calving38, by = "CowId") %>%
  merge(df.culldata38, by = "CowId", all.x = TRUE) %>%
  merge(df.insemination38, by = "CowId") %>%
  merge(df.pregnancy38, by = "CowId")

rm(list=ls(pattern="df"))
rm(df.repro)
rm(all_farm_daily)
rm(farm1001)
rm(df.repro01)
rm(df.calving38)
rm(df.cowinfo38)
rm(df.culldata38)
rm(df.insemination38)
rm(n_insem38)
rm(n_insem)
rm(NumInsemination)
rm(ncow.all)
rm(farm38_repro)

df.repro38 <- read_excel("1038, All Data, Proc, June 2020.xlsx", sheet = "ReproData")
#delete rows that the insemination = FALSE
df.repro38 = df.repro38[!df.repro38$Inseminate == FALSE, ]
#times of insemination per cow
df.n_insem38 <- df.repro38 %>%
  group_by(CowId, FarmNo) %>%
  summarise(Inseminate=n())
### merge farm 1038 reproduction events and ReproData ###
farm38_repro <- data.frame()
farm38_repro <- merge(df.master38, df.n_insem38, by=c('CowId', 'FarmNo'), all.y=TRUE)

##### find out the number of insemination for each cow #####
df.repro$Ins <- ifelse(df.repro$Inseminate == "TRUE", 1, 0)
#times of insemination per cow
df.n_insem <- df.repro %>%
  group_by(CowId, FarmNo) %>%
  summarise(n.Ins=sum(Ins))

##### find out the DFC when the cows have the last insemination date #####
df.d_insem <- df.repro %>%
  group_by(CowId, FarmNo) %>%
  select(CowId, FarmNo, Inseminate, SamplingTime, DFC) %>%
  filter(Inseminate==TRUE) %>%
  slice_max(SamplingTime)

# delete rows that the insemination = FALSE
df.repro$Ins <- df.repro[!df.repro$Inseminate == FALSE, ]
#create a new code for cows that BHBraw over 0.08 as 1
df.keto$ketoCow <- ifelse(df.keto$BHBraw >= 0.08, 1, 0)

all_farm_raw = all_farm_raw[!is.na(all_farm_raw$FarmNo), ]
##### number of inseminations for each cow in every farm #####
N.D.Insemination <- data.frame()

for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  #open only ReproData sheet from each farm
  df.repro <- read_excel(filename, sheet = "ReproData")

  #create a new column which code the cows which were inseminated as 1, not-inseminated as 0
  df.repro$Ins <- ifelse(df.repro$Inseminate == "TRUE", 1, 0)
  #times of insemination per cow
  df.n_insem <- df.repro %>%
    group_by(CowId, FarmNo) %>%
    summarise(n.Ins=sum(Ins))

  df.fd_insem <- df.repro %>%
    group_by(CowId, FarmNo) %>%
    select(CowId, FarmNo, Inseminate, SamplingTime, DFC) %>%
    filter(Inseminate==TRUE) %>%
    slice_max(SamplingTime)

  df.n_insem <- merge(df.n_insem, df.fd_insem, by=c('FarmNo', 'CowId'), all.x=TRUE, all.y=TRUE)
  N.D.Insemination <- bind_rows(N.D.Insemination, df.n_insem)
}
# change the inseminate=NA to 0 #
N.D.Insemination$Inseminate <- as.character(N.D.Insemination$Inseminate)
N.D.Insemination$Inseminate <- ifelse(is.na(N.D.Insemination$Inseminate), "0", N.D.Insemination$Inseminate)

# change the name of the columns #
names(N.D.Insemination)[names(N.D.Insemination) == "DFC"] <- "DFC_last_Ins"
names(N.D.Insemination)[names(N.D.Insemination) == "Inseminate"] <- "Ins"

##### merge all farms with reproduction events and number of inseminations #####
all_farm_repro <- data.frame()
all_farm_repro <- merge(all_farm_raw, N.D.Insemination, by=c('FarmNo', 'CowId'), all.y=TRUE, all.x=TRUE)

##### milk BHB average#####
df.keto01 <- read_excel("1001, All Data, Proc, June 2020.xlsx", sheet = "Ketosis")
df.BHB.avg01 <- df.keto01 %>%
  group_by(CowId, FarmNo) %>%
  summarise(BHB.avg=mean(BHBraw))

df.all.BHBavg <- data.frame()
for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  df.keto <- read_excel(filename, sheet = "Ketosis")
  df.BHB.avg<- df.keto %>%
    group_by(CowId, FarmNo) %>%
    summarise(mean_BHB=mean(BHBraw))
  df.all.BHBavg <- bind_rows(df.all.BHBavg, df.BHB.avg)
}
summary(df.all.BHBavg)
table(df.all.BHBavg$FarmNo)
all_farm_keto_repro <- merge(all_farm_repro, df.all.BHBavg, by=c('CowId', 'FarmNo'), all.y=TRUE, all.x=TRUE)

##### merge the ketotic Cows and the earliest DFC #####
all_farm_keto <- data.frame()
for(i in 1001:1038){
  filename <- paste("C:/Data/",i,", All Data, Proc, June 2020.xlsx", sep = "")
  df.keto <- read_excel(filename, sheet = "Ketosis")
  ##### how many days that BHB>0.08 mmol/L ######
  df.keto$ketoCow <- ifelse(df.keto$BHBraw >= 0.08, 1, 0)
  df.n.ketoCow <- df.keto %>%
    group_by(CowId, FarmNo) %>%
    summarise(n.ketoCow=sum(ketoCow))
  ##### the earliest DFC when BHB>0.08 mmol/L together with the BHB value #####
  df.table <- df.keto %>%
    group_by(CowId) %>%
    select(CowId, FarmNo, ketoCow, BHBraw, DFC) %>%
    filter(ketoCow==1) %>%
    slice_min(DFC)
df.n.ketoCow <- merge(df.n.ketoCow, df.table, by=c('CowId', 'FarmNo'), all.x=TRUE)
all_farm_keto <- bind_rows(all_farm_keto, df.n.ketoCow)
}
all_farm_keto$ketoCow <- as.character(all_farm_keto$ketoCow)
all_farm_keto$ketoCow <- ifelse(is.na(all_farm_keto$ketoCow), "0", all_farm_keto$ketoCow)

# rename the BHBraw to "the first BHB>0.08" and DFC to "first DFC when BHB>0.08"
names(all_farm_keto)[names(all_farm_keto) == "BHBraw"] <- "first_ketoBHB"
names(all_farm_keto)[names(all_farm_keto) == "DFC"] <- "DFC_first_ketoBHB"

all_farm <- data.frame()
all_farm <- merge(all_farm_keto_repro, all_farm_keto, by=c('FarmNo', 'CowId'), all.y=TRUE, all.x=TRUE)
