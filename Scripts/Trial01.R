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

#question_2: Which month saw the lowest average daily temperature difference?
lowest_ave_temp <- Bom_data %>%
  separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep= "/") %>% 
  filter (min_Temp >=0, max_Temp >= 0) %>% 
  mutate (min_Temp = as.numeric (min_Temp)) %>%   
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) %>% 
  group_by(Month) %>%
  summarise(average = mean(Temp_diff)) 
 
  
#question 3: Which state saw the lowest average daily temperature difference?

Tidy_bom_stations <- Bom_stations %>% 
  gather (key = Station_number, value=amount, -info) %>% 
  spread(key = info, value= amount) %>% 
  mutate (Station_number=as.numeric(Station_number))

state_ave_temp <- Bom_data %>%
  separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep= "/") %>% 
  mutate (min_Temp = as.numeric (min_Temp)) %>%   
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) 
 

combined_data <- full_join (Tidy_bom_stations, state_ave_temp, by= c("Station_number"= "Station_number"))

state_lowest_ave_temp <- combined_data %>%
filter(min_Temp != "NA", max_Temp != "NA") %>% 
  mutate (min_Temp = as.numeric (min_Temp)) %>%   
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) %>% 
  group_by(state) %>%
  summarise(average = mean(Temp_diff)) %>% 
  arrange(average) %>% #sorts from lowest to highest
  slice(1) #takes the smallest one and gives answer for state with lowest temp diff


         