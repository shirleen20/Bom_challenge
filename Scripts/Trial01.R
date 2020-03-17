library(tidyverse)
Bom_data <- read_csv("Data/BOM_data.csv")
view(Bom_data)
Bom_stations <- read_csv ("Data/BOM_stations.csv")
view(Bom_stations)

question_1 <- Bom_data %>% 
separate(Temp_min_max, into = c("min_Temp", "max_Temp")) %>% 
filter (min_Temp >=0, max_Temp >= 0, Rainfall >= 0) %>% 
  group_by(Station_number) %>% 
  summarise(num_row=n())
view (question_1)

#question_2 
lowest_ave_temp <- Bom_data %>%
  separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep= "/") %>% 
  filter (min_Temp >=0, max_Temp >= 0) %>% 
  mutate (min_Temp = as.numeric (min_Temp)) %>%   
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) %>% 
  group_by(Month) %>%
  summarise(average = mean(Temp_diff))  
  view desc(lowest_ave_temp)
  
  



         