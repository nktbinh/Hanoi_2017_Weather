library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
"http://academic.udayton.edu/kissock/http/Weather/gsod95-current/VSHANOI.txt"%>%
  read.table() %>% data.frame %>% tbl_df ->data
names(data)<-c("month","day","year","tempF")

#create additional column to represent temperature in Celsius
data%>%
  mutate(tempC=(tempF-32)*5/9) -> data
#Past Years
data%>%
  group_by(year,month)%>%
  arrange(day)%>%
  ungroup()%>%
  group_by(year)%>%
  mutate(newday=seq(1,length(day)))%>%
  ungroup%>%
  filter(tempF!=-99 & year!=2017)%>%
  group_by(newday)%>%
  mutate(upper=max(tempC),lower=min(tempC),avg=mean(tempC),se=sd(tempC)/sqrt(length(tempC)))%>%
  mutate(avg_upper=avg+(2.101*se),  #Calculate 95% CI for mean
         avg_lower=avg-(2.101*se))%>%
  ungroup() -> past

#Current Year
data %>%
  group_by(year, month) %>%
  arrange(day) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(newday = seq(1, length(day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(tempF != -99 & year == 2017) -> present # filter out missing data & select current year data  

#Create data frame that represents the lowest temp for each day of the historical data:
pastlows <- past %>%
  group_by(newday) %>%
  summarise(Pastlow=min(tempC))

#Create a data frame that identifies the day in 2017 in which the temps were lower than all previous 22 years:
presentlows<-present %>%
  left_join(pastlows) %>%
  mutate(record=ifelse(tempC<Pastlow,"Y","N")) %>%
  filter(record=="Y")

#Create a data frame that represents the highest temp for each day for the historical data:
pasthighs <- past %>%
  group_by(newday) %>%
  summarise(Pasthigh=max(tempC))

#Create a data frame that identifies the day in 2017 in which the temps were higher than all previous 22 years:
presenthighs<- present %>%
  left_join(pasthighs) %>%
  mutate(record=ifelse(tempC>Pasthigh,"Y","N")) %>%
  filter(record=="Y")

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

#Create y-axis variable:
a <- dgr_fmt(seq(0,110, by=10))
  
p <- ggplot(past,aes(newday,tempC))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  geom_linerange(past,mapping=aes(x=newday,ymin=lower,ymax=upper),color="wheat2",alpha=.1)

#Next we add the data that represents the 95% CI around the daily mean temperatures for 1995 - 2016
p <- p + geom_linerange(past, mapping=aes(x=newday, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")

p <- p +
  geom_line(present, mapping=aes(x=newday, y=tempC, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat3", linetype=1, size=1)

p <- p + geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1) +
  geom_hline(yintercept = 110, colour = "white", linetype=1)

p <- p +
  geom_vline(xintercept = 31, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat3", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat3", linetype=3, size=.5)

p <- p +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(0,110, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("Jan", "Feb", "March", "April",
                                "May", "June", "July", "Aug", "Sep",
                                "Oct", "Nov", "Dec"))


#
p <- p +
  geom_point(data=presentlows, aes(x=newday, y=tempC), colour="blue3") +
  geom_point(data=presenthighs, aes(x=newday, y=tempC), colour="firebrick3")

p <- p +
  ggtitle("Hanoi's weather in 2017") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=18)) +
  annotate("text", x = 55, y = 115, label = "temperature in Celsius", size=3, fontface="bold")


