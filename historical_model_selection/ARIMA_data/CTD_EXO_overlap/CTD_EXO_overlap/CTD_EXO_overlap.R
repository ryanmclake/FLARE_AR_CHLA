##Flora/EXO overlap
##Author: Mary Lofton
##Date: 08NOV18

#load packages
pacman::p_load(tidyverse, lubridate)

#source function for ggplot equations
source("./CTD_EXO_overlap/CTD_EXO_overlap/ggplot_smooth_func.R")

#read in EXO data
exo <- read_csv("./CTD_EXO_overlap/CTD_EXO_overlap/Catwalk.csv", skip = 1)%>%
  select(TIMESTAMP, Chla_1, BGAPC_1) %>%
  filter(Chla_1 != "NAN")
exo <- exo[-c(1,2),]
exo <- exo %>%
  mutate(Day = date(TIMESTAMP),
         Hour = hour(TIMESTAMP)) %>%
  select(-TIMESTAMP)
exo$Chla_1 <- as.numeric(exo$Chla_1)
exo$BGAPC_1 <- as.numeric(exo$BGAPC_1)
exo <- aggregate(cbind(Chla_1, BGAPC_1) ~ Day + Hour, data = exo, mean) %>%
  mutate(Depth_m = 1)

#read in CTD data
# Load .txt files for appropriate reservoir 
ctd <- read_csv("./CTD_EXO_overlap/CTD_EXO_overlap/CTD.csv") %>%
  select(Reservoir, Site, Date, Depth_m, Chla_ugL) %>%
  mutate(Year = year(Date),
         Day = date(Date),
         Hour = hour(Date)) %>%
  filter(Reservoir == "FCR", Site == 50, Year == 2018) %>%
  mutate(Depth_m = ifelse(Depth_m <= 1.2 & Depth_m >= 0.8, 1, Depth_m)) 

ctd1 <- aggregate(Chla_ugL ~ Day + Hour + Depth_m, data = ctd, mean) 


#combine EXO and FP data
overlap <- left_join(ctd1, exo, by = c("Day","Hour","Depth_m")) %>%
  filter(!is.na(Chla_1))

#plotting theme
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=16), axis.text = element_text(size = 18))

#run overlap between logical combinations of vars
plot1 <- ggplot(data = overlap, aes(x = BGAPC_1*10, y = Chla_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 6)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "cyan4")+
  xlab("Phycocyanin (EXO sonde) - ug/L")+
  ylab("Chl-a (CTD) - ug/L")+
  mytheme
plot1
#ggsave(plot1, filename = "C:/Users/Mary Lofton/Desktop/CTD_EXO_overlap/phycocyanin_CTDchla.png",
#       device = "png")


plot2 <- ggplot(data = overlap, aes(x = Chla_1, y = Chla_ugL))+
  geom_point(size = 3)+
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 6)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "springgreen3")+
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Chl-a (CTD) - ug/L")+
  mytheme
plot2
ggsave(plot2, filename = "C:/Users/Mary Lofton/Desktop/CTD_EXO_overlap/EXOchla_CTDchla.png",
       device = "png")
