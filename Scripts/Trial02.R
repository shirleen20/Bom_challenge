#---Data Viz Prac Challenge and Questions: Pulling It All Together using BOM Meterological data----

library(tidyverse)

#Question 1
#For the Perth station (ID 9225), produce three scatter plots showing the relationship between 
#the maximum temperature and each other measurement recorded (minimum temperature, rainfall and 
#solar exposure).

Bom_data <- read_csv("Data/BOM_data.csv")
view(Bom_data)
Bom_data_Perth <- Bom_data %>%
  filter(Station_number == 9225) %>% 
  separate(Temp_min_max, into = c("t_min", "t_max"), sep = "/") %>% 
  filter(t_min != "-", t_max != "-", Rainfall != "-", Solar_exposure != "-") %>% 
  mutate(t_min=as.numeric(t_min), t_max=as.numeric(t_max), Rainfall=as.numeric(Rainfall), Solar_exposure=as.numeric(Solar_exposure))

q1_plot1 <- Bom_data_Perth %>% 
  ggplot(mapping = aes( x= t_max, y= t_min))+
  geom_point(alpha=0.2, colour = "Magenta")+
  xlab("Maximum temperature") +
  ylab("Minimun temperature") 

q1_plot2 <- Bom_data_Perth %>% 
  ggplot(mapping = aes( x= t_max, y= Rainfall))+
  geom_point(alpha=0.2, colour = "darkgreen")+
  xlab("Maximum temperature")+
  ylab ("Rainfall")

q1_plot3 <- Bom_data_Perth %>% 
  ggplot(mapping = aes( x= t_max, y= Solar_exposure))+
  geom_point(alpha=0.2, colour = "chocolate1")+
  xlab("Maximum temperature")+
  ylab("Solar exposure")

#Question 2
#Display these four measurements for the Perth station in a single scatter plot by using additional aesthetic mappings.
q2_plot4 <- Bom_data_Perth %>% 
  ggplot(mapping = aes( x= t_max, y= t_min,size= Rainfall, colour= Solar_exposure))+
  geom_point(alpha=0.2)+
  labs(xlab= "Maximum temperature",
       ylab= "Minimum temperature")+
  theme(legend.text = element_text(size=8), legend.title=element_text(size=8))
 
#Question 3
#Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.

library(cowplot)

combined_plot <- plot_grid(q1_plot1,q1_plot2,q1_plot3,q2_plot4, labels = "AUTO")

ggsave("results/final_plot.png", plot = combined_plot, width = 20, height = 15, dpi = 300, units = "cm")

#Question 4
#Using the entire BOM dataset, calculate the average monthly rainfall for each station. 
#Produce a lineplot to visualise this data and the state each station is in.
Bom_stations <- read_csv ("Data/BOM_stations.csv")
view(Bom_stations)

Tidy_bom_stations <- Bom_stations %>% 
  gather (key = Station_number, value=amount, -info) %>% 
  spread(key = info, value= amount) %>% 
  mutate (Station_number=as.numeric(Station_number))

joined_data <- Tidy_bom_stations %>% 
  full_join(Bom_data, by= c("Station_number"= "Station_number"))

#q4 <- select(joined_data, state, Station_number, Month, Rainfall)# reductant line of codes

q4_data <- joined_data %>% 
  group_by(state, Station_number, Month) %>%
  mutate(Rainfall=as.numeric(Rainfall)) %>% 
  filter(Rainfall != "-") %>% 
  summarise(mean_rainfall = mean(Rainfall))%>%
  arrange(Month) 

#All plots is one graph
q4_plot1 <- q4_data %>% 
  ggplot(aes(x= as.character(Month), y= mean_rainfall, colour=state, group=Station_number))+
  geom_line(size=1)+
  scale_colour_brewer(palette="Set3")


#Plots seperated by state using facet wrap
q4_plot2 <- q4_data %>% 
  ggplot(aes(x= as.factor(Month), y=mean_rainfall, group=as.factor(Station_number), colour=state))+
  geom_line(size=1)+
  facet_wrap(~state)+
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.direction="horizontal")+
  labs(x="Month",
       y="Mean rainfall",
       colour="State")

ggsave(filename="results/q4_plot2.png", plot=q4_plot2, width=15, height=12, dpi=300, units="cm")


