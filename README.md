# BML
##Figure1
library(tidyverse)
setwd("C:/Users/Angel/Downloads/qiime16S_2022")
setwd("C:/Users/Angel/Downloads/qiime18S_2022")
#Read alpha diversity data:
alphadiv<-read.csv("core-metrics/alphadiversity.csv")
shannon_bml<-alphadiv %>% select(source2, platform, year, season, surfacevsbottom, description, shannon_entropy) %>% 
  filter(source2 == "BML", surfacevsbottom %in% c("surface", "watercolumn", "bottom"),
         year %in% c("2015", "2016", "2017", "2018", "2019", "2021")) %>% 
  group_by(year)
shannon_bml$year<- as.factor(shannon_bml$year)
ggplot(shannon_bml, aes(x = year, y = shannon_entropy)) +
  geom_boxplot(position=position_dodge(1), size=1.5) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  xlab("Year") +
  ylab("Shannon") +
  theme_bw(base_size = 24, base_family = "serif") +
  theme(axis.text.x = element_text(face="bold", color="black", size=24), axis.text.y = element_text(face="bold", color="black", size=24)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=36), axis.title.y  = element_text(face="bold", colour="black", size=36)) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 2.0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.length.y = unit(.4, "cm"), axis.ticks.length.x = unit(.4, "cm"))
  
#Shannon BML, BCR, Tailings graph
actail<-alphadiv %>% select(source2, platform, year, season, surfacevsbottom, description, shannon_entropy) %>%
  filter(source2 == "Tailings", year %in% c("2017","2018", "2019", "2021")) %>%
  group_by(year)
actail$year<- as.factor(actail$year)
bcr<-alphadiv %>% select(source2, platform, year, season, surfacevsbottom, description, shannon_entropy) %>%
  filter(source2 == "BCR", year %in% c("2016", "2017","2018", "2019", "2021")) %>%
  group_by(year)
bcr$year<- as.factor(bcr$year)
m <- rbind(shannon_bml, bcr, actail)
m$year<- as.factor(m$year)
ggplot(m, aes(x = year, y = shannon_entropy, colour = source2)) +
  geom_boxplot(position = position_dodge2(preserve = "single"), size=1.5) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  xlab("Year") +
  ylab("Shannon") +
  theme_bw(base_size = 24, base_family = "serif") +
  theme(axis.text.x = element_text(face="bold", color="black", size=24), axis.text.y = element_text(face="bold", color="black", size=24)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=36), axis.title.y  = element_text(face="bold", colour="black", size=36)) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 2.0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.length.y = unit(.4, "cm"), axis.ticks.length.x = unit(.4, "cm")) +
  theme(legend.position = c(0.15, 0.85)) +
  labs(colour="Sampling site")

#Summary graph:
ggplot(m, aes(x = source2, y = shannon_entropy, fill=source2)) +
  geom_boxplot(position = position_dodge2(preserve = "single"), size=1, alpha=0.75) +
  stat_summary(fun = "mean", geom = "point", shape = 16,
               size = 5, color = "black") +
  xlab("Sampling site") +
  ylab("Shannon") +
  theme_bw(base_size = 24, base_family = "serif") +
  theme(axis.text.x = element_text(face="bold", color="black", size=24), axis.text.y = element_text(face="bold", color="black", size=24)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=36), axis.title.y  = element_text(face="bold", colour="black", size=36)) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 2.0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.ticks.length.y = unit(.4, "cm"), axis.ticks.length.x = unit(.4, "cm")) +
  theme(legend.position="none")

#mean values:
mean_all <- rbind(bml_mean, tailings_mean, bcr_mean)
write.csv(mean_all, file = "core-metrics/16S-Chao1-mean.csv")
