library(readxl)
library(ggplot2)
library(dplyr)
str(Three)
install.packages("xlsx")

# plot number of cows in 38 farms in this study
ggplot(data = Three, aes(x = factor(FarmNo))) +
  geom_bar(stat = "count", fill = "orange")+
  stat_count(geom = "text", size = 3, aes(label = ..count..))+
  theme_classic() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("Number of cows in each farm") +
  xlab("Farm ID") +
  ylab("Number of cows")

# plot days from calving to reproduction events
ggplot(data = Three, aes(x=DaysToCLA)) +
  geom_histogram(breaks = seq(0, 200, by = 5),
                 fill = "green") +
  ggtitle("Days from calving to first HN detected cycle") +
  xlab("DIM") +
  ylab("Number of cows") +
  theme_classic()

# Parity distribution
ggplot(data=Three, aes(x=factor(Lactation)))+
  geom_bar(stat="count", fill="skyblue") +
  stat_count(geom="text", size=3, aes(label=..count..)) +
  ggtitle("Parity Distribution") +
  theme_classic() +
  xlab("Parity") +
  ylab("#cows")

# create a dataframe with ketosis parameters within 20 DIM
df.20keto <- Three %>%
  select(FarmNo, CowId, ad_Lactation, CalvingSeason, KetoDur20,
         AvgKeto20_100, MaxKetoDIM20, MaxKeto20, nHighKeto20, onset20) %>%
  mutate(KetoCow = ifelse(AvgKeto20_100 >= 8, "Yes", "No")) %>%
  filter(onset20 >= 3 & onset20 <= 20)

# plot onset of ketosis within 20 DIM
ggplot(data = df.20keto, aes(x=factor(onset20))) +
  geom_bar(stat = "count", fill = "gray") +
  stat_count(geom = "text", size = 3, aes(label = ..count..))+
  theme_classic() +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Number of cows")

summary(df.20keto$onset20[df.20keto$onset20 <= 20])
summary(survival)

# plot when the MAX BHB happened within 20 DIM
ggplot(data = df.20keto, aes(x=factor(MaxKetoDIM20))) +
  geom_bar(stat = "count", fill = "gray") +
  stat_count(geom = "text", size = 3, aes(label = ..count..))+
  theme_classic() +
  xlab("DIM when Max BHB happened within 20 DIM (DIM)") +
  ylab("Number of cows")

summary(df.20keto$MaxKetoDIM20[df.20keto$MaxKetoDIM20 <= 20])

# plot when the MAX BHB happened within 20 DIM grouped by avg ketosis levels
ggplot(data = df.20keto, aes(x=factor(MaxKetoDIM20), fill = KetoCow)) +
  geom_bar(stat = "count", position = "stack") +
  stat_count(geom = "text", size = 3, aes(label = ..count..))+
  theme_classic() +
  xlab("DIM when Max BHB happened within 20 DIM (DIM)") +
  ylab("Number of cows")

# plot onset of ketosis within 20 d grouped by avg ketosis levels
ggplot(data = df.20keto, aes(x=factor(onset20), fill = KetoCow))+
  geom_bar(stat = "count", position = "stack") +
  stat_count(geom = "text", size = 3, aes(label = ..count..))+
  theme_classic() +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Number of cows")

df.20ketocow <- df.20keto %>%
  filter(KetoCow == "Yes")
summary(df.20ketocow)

which(Three$onset20 == 0)

# plot days to resumption of cyclicity and onset of ketosis
boxplot(Three$DaysToCLA~factor(Three$onset20))

# boxplot of onset of ketosis and average ketosis
boxplot(Three$AvgKeto20_100 ~ factor(Three$onset20),
        ylab = "Average milk BHB within 20 DIM",
        xlab = "Onset of ketosis (DIM)")
abline(h= 8, col = "red")

# plot parity and oneset
boxplot(Three$onset20 ~ Three$ad_Lactation,
        ylab = "Onset of ketosis (DIM)",
        xlab = "Parity")

ggplot(data = df.20keto, aes(x = onset20, y = factor(ad_Lactation))) +
  geom_boxplot() +
  #facet_wrap(~FarmNo) +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Parity (1, 2, 3, and 4+)") +
  theme_classic()

ggplot(data = df.20keto, aes(x = onset20, y = factor(ad_Lactation), fill = KetoCow)) +
  geom_boxplot() +
  #facet_wrap(~FarmNo) +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Parity (1, 2, 3, and 4+)")

# plot calving season and oneset
ggplot(data = df.20keto, aes(x = onset20, y = factor(CalvingSeason))) +
  geom_boxplot() +
  facet_wrap(~ad_Lactation) +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Calving seasons")

ggplot(data = df.20keto, aes(x = onset20, y = factor(CalvingSeason), fill = KetoCow)) +
  geom_boxplot() +
  #facet_wrap(~FarmNo) +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Calving seasons")

# parity and average ketosis
plot(Three$AvgKeto20_atleast10, Three$ad_Lactation,
     main = "Scatterplot of AvgKeto20 and lactation groups",
     ylab = "Parity groups",
     xlab = "Average milk BHB in 20 DIM (mmol/L)", pch = 19)
abline(v = 0.08, col = "red")
describe.by(Three$ad_Lactation, Three$AvgKeto20_atleast10)

# average ketosis 20d
ggplot(data = Three, aes(x = AvgKeto20_atleast10)) +
  geom_histogram(breaks = seq(0, 0.45, by =0.01),
                 fill = "darkgreen", alpha = 0.8) +
  ggtitle("Average milk BHB within 20 DIM") +
  xlab("Average milk BHB (mmol/L)") +
  ylab("#cows") +
  theme_classic()+
  geom_vline(xintercept=0.08, color="red", linetype = "dashed")

# average ketosis 20 vs ketosis duration 20
plot(Three$AvgKeto20_atleast10, Three$KetoDur20,
     main = "Scatterplot of Avg milk BHB and ketosis duration within 20 DIM",
     xlab = "Average milk BHB (mmol/L)",
     ylab = "Duraiton (days)", pch = 19)
abline(v = 0.08, col = "red")
abline(lm(Three$KetoDur20~Three$AvgKeto20_atleast10), col = "blue")

plot(Three$AvgKeto20_atleast10, Three$MaxKetoDIM20,
     main = "Scatterplot of Avg milk BHB and max milk BHB within 20 DIM",
     ylab = "DIM of max milk BHB (mmol/L)",
     xlab = "Average milk BHB (mmol/L)", pch = 19)
abline(v = 0.08, col = "red")
abline(lm(Three$MaxKetoDIM20~Three$AvgKeto20_atleast10), col = "blue")

boxplot(Three$DaysToFirstIns~Three$ad_Lactation, ylab = "Days from calving to first insemination", xlab = "Parity")

############################################################
# create a dataframe with ketosis parameters within 60 DIM #
############################################################
df.60keto <- Three %>%
  select(FarmNo, CowId, ad_Lactation, CalvingSeason, KetoDur60,
         AvgKeto60_atleast10, MaxKetoDIM60, MaxKeto60, nHighKeto20, onset60) %>%
  mutate(KetoCow = ifelse(AvgKeto60_atleast10 >= 0.08, "Yes", "No"))

summary(df.60keto$onset60[df.60keto$onset60 <= 60])
ggplot(data = df.60keto, aes(x=factor(onset60)))+
  geom_bar(stat = "count", fill = "gray") +
  stat_count(geom = "text", size = 3, aes(label = ..count..))+
  theme_classic() +
  xlab("Onset of ketosis within 60 DIM (DIM)") +
  ylab("Number of cows")

####################################################
##  Correlation of milk BHB levels within 20 DIM  ##
####################################################
str(df.20keto)
df.20keto.cor <- df.20keto %>%
  select(CowId, ad_Lactation, KetoDur20, AvgKeto20_100, MaxKetoDIM20, MaxKeto20, nHighKeto20, onset20)
cor.keto20 <- cor(na.omit(df.20keto.cor))
cor.keto20
# correlation plot of ketosis parameters in 20 d
corrplot(cor.keto20,
         tl.col = "brown",
         tl.srt = 90, bg = "white",
         title = "\n\n Correlation plot of ketosis parameters in 20 d",
         addCoef.col = "black",
         type = "lower")

# plot days to CLA with onset of ketosis within 20 d
plot(df1$DaysToCLA~df1$onset20)
ggplot(data = df.20keto.repro, aes(x = factor(onset20), y = DaysToCLA)) +
  geom_boxplot() +
  xlab("Onset of ketosis within 20 DIM (DIM)") +
  ylab("Days from calving to resumption of cyclicity") +
  theme_classic()
