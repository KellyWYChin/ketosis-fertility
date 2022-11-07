# insert milk and ketosis data from Farm 1001
milkone <- read_excel("../data/raw/1001, All Data, Proc, June 2020.xlsx", sheet = "MilkData")
ketoneone <- read_excel("../data/raw/1001, All Data, Proc, June 2020.xlsx", sheet = "Ketosis")
summary(milkone)
summary(ketoneone)

# select useful parameters from milk and ketosis data
milk <- milkone %>%
  filter(DFC <= 60) %>%
  select(CowId, DayYield, DFC, Roll7Day)

ketone <- ketoneone %>%
  filter(DFC < 61) %>%
  mutate(rDFC = floor(DFC)) %>%
  select(CowId, Parity, rDFC, BHBraw)


# change the name of rDFC to DFC
colnames(ketone)[colnames(ketone) == "rDFC"] <- "DFC"

# merge 2 files
mix <- merge(ketone, milk, by = c('CowId', 'DFC'), all.x = TRUE, all.y = TRUE)

# cow number 62
cow1 <- mix %>%
  filter(DFC > 3) %>%
  filter(CowId == 62)

library(reshape2)
long1 <- melt(cow1, measure.vars = c("DayYield", "BHBraw"))

ggplot(long1, aes(x = DFC, y = value, col = variable, group = variable)) +
  geom_point()

# adjust BHB values by *1000
cow1 <- cow1 %>%
  mutate(BHB = 1000*BHBraw)
cow1$BHBraw <- NULL
long1 <- melt(cow1, measure.vars = c("DayYield", "BHB"))

ggplot(long1, aes(x = DFC, y = value, col = variable, group = variable)) +
  geom_point() + geom_line()
