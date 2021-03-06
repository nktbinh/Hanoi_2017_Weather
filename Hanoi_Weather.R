library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(lubridate)

"http://academic.udayton.edu/kissock/http/Weather/gsod95-current/VSHANOI.txt"%>%
  read.table() %>% data.frame %>% tbl_df ->data
names(data)<-c("month","day","year","tempF")

data <- data%>%
  mutate(tempC=(tempF-32)*5/9)

data2 <- data %>%
  select(year,month,day) %>% 
  mutate(date = make_date(year,month,day)) %>%
  mutate(yday = yday(date))
  
data3<-merge(data,data2, by = c("year","month","day"))

# Past Years
past <- data3 %>%
  group_by(year) %>%
  arrange(yday) %>%
  rename(newday = yday) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  filter(tempF!=-99 & year!=2017)%>%
  group_by(newday)%>%
  mutate(upper=max(tempC),
         lower=min(tempC),
         avg=mean(tempC),
         se=sd(tempC)/sqrt(length(tempC)))%>%
  mutate(avg_upper=avg+(2.101*se),  #Calculate 95% CI for mean
         avg_lower=avg-(2.101*se))%>%
  ungroup()

#Current Year
present <- data3 %>%
  group_by(year) %>%
  arrange(yday) %>%
  rename(newday = yday) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  filter(tempF != -99 & year == 2017) # filter out missing data & select current year data  

# Create data frame that represents the lowest temp for each day of the historical data:
pastlows <- past %>%
  group_by(newday) %>%
  summarise(Pastlow=min(tempC))

# Create a data frame that identifies the day in 2017 in which the temps were lower than all previous 22 years:
presentlows<-present %>%
  left_join(pastlows) %>%
  mutate(record = ifelse(tempC<Pastlow,"Y","N")) %>%
  filter(record == "Y")

# Create a data frame that represents the highest temp for each day for the historical data:
pasthighs <- past %>%
  group_by(newday) %>%
  summarise(Pasthigh = max(tempC))

# Create a data frame that identifies the day in 2017 in which the temps were higher than all previous 22 years:
presenthighs<- present %>%
  left_join(pasthighs) %>%
  mutate(record=ifelse(tempC>Pasthigh,"Y","N")) %>%
  filter(record=="Y")

# Utility function to format y-axis labels with degree notation
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# Create y-axis variable:
a <- dgr_fmt(seq(5,40, by=5))

legend_data <- data.frame(x=seq(168,177),y=rnorm(10,12.5,1))

# ------------------------------------------------------------

p <- ggplot(past,aes(newday,tempC))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "seashell2"),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  geom_linerange(past, 
                 mapping=aes(x=newday,ymin=lower,ymax=upper),
                 size = 0.9, color = "wheat2", alpha=.1)

ggsave("Step1", p, device = "png", width = 16, height = 12)

p1 <- p + geom_linerange(past, mapping=aes(x=newday, ymin=avg_lower, ymax=avg_upper), colour = "#A57E69")

ggsave("Step2", p1, device = "png", width = 16, height = 12)

p2 <- p1 +
  geom_line(present, mapping=aes(x=newday, y=tempC, group=1), color = "#4A2123", size = 1) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)

ggsave("Step3", p2, device = "png", width = 16, height = 12)

p3 <- p2 + geom_hline(yintercept = 5, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 10, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 15, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 20, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 25, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 30, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 35, colour = "ivory2", linetype=1, size = .1) +
  geom_hline(yintercept = 40, colour = "ivory2", linetype=1, size = .1)

ggsave("Step4", p3, device = "png", width = 16, height = 12)

p4 <- p3 +
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5)

ggsave("Step5", p4, device = "png", width = 16, height = 12)

p5 <- p4 +
  coord_cartesian(ylim = c(5,40)) +
  scale_y_continuous(breaks = seq(5,40, by=5), labels = a) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("Jan", "Feb", "March", "April",
                                "May", "June", "July", "Aug", "Sep",
                                "Oct", "Nov", "Dec"))

ggsave("Step6", p5, device = "png", width = 16, height = 12)

p6 <- p5 +
  geom_point(data=presentlows, aes(x=newday, y=tempC), colour="blue3") +
  geom_point(data=presenthighs, aes(x=newday, y=tempC), colour="firebrick3")

ggsave("Step7", p6, device = "png", width = 16, height = 12)

p7 <- p6 +
  ggtitle("Hanoi's weather in 2017") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="gray30",size=18))
  
grob1 = grobTree(textGrob("Temperature in Celcius\n",
                            x=0.01, y=0.95, hjust=0,
                            gp=gpar(col="gray30", fontsize = 12, fontface="bold")))

grob2 = grobTree(textGrob(paste("Data represents average daily temperatures. Accessible data\n",
                                "dates back to January 1st, 1995. Data for 2017\n",
                                "is only available through May 17th.", sep=""),
                          x=0.01, y=0.89, hjust=0,
                          gp=gpar(col="gray30", fontsize = 9)))

p7 <- p7 + annotation_custom(grob1) + annotation_custom(grob2)

ggsave("Step8", p7, device = "png", width = 16, height = 12)

grob3 = grobTree(textGrob(paste("In 2017 there have been 8 days\n",
                                "that were hottest since 1995\n",sep=""),
                          x=0.12, y=0.68, hjust=0,
                          gp=gpar(col="firebrick3", fontsize=9)))

grob4 = grobTree(textGrob(paste("Intense heatwave sets unprecedented record\n",
                                "temp in early June\n",sep=""),
                          x=0.45, y=0.83, hjust=0,
                          gp=gpar(col="firebrick3", fontsize=9)))

p8 <- p7 + annotation_custom(grob3) + annotation_custom(grob4)
p8 <- p8 +
  annotate("segment", x = 156, xend = 165, y = 35, yend = 36, colour = "firebrick3") +
  annotate("segment", x = 53, xend = 53, y = 26, yend = 28, colour = "firebrick3")

ggsave("Step9", p8, device = "png", width = 16, height = 12)

p9 <- p8 +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 20, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 9, yend = 16, colour = "#A57E69", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y)) +
  annotate("segment", x = 183, xend = 185, y = 15.7, yend = 15.7, colour = "#A57E69", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 9.2, yend = 9.2, colour = "#A57E69", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 9.2, yend = 15.7, colour = "#A57E69", size=.5) +
  annotate("text", x = 212, y = 12.5, label = "NORMAL RANGE", size=4, colour="gray30") +
  annotate("text", x = 148, y = 12.5, label = "2017 Temperature", size=4, colour="gray30") +
  annotate("text", x = 193, y = 20, label = "RECORD HIGH", size=4, colour="gray30") +
  annotate("text", x = 193, y = 5, label = "RECORD LOW", size=4, colour="gray30")

ggsave("Step10", p9, device = "png", width = 16, height = 12)