#Day 5
library(tidyverse)
Bom_data <- read_csv("Data/BOM_data.csv")
view(Bom_data)
Bom_stations <- read_csv ("Data/BOM_stations.csv")
view(Bom_stations)
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?
question_1 <- Bom_data %>% 
separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep = "/") %>% 
filter (min_Temp != "-", max_Temp != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% 
  summarise(num_row=n())
view (question_1)
# Answer question1: 20 stations


#question_2: Which month saw the lowest average daily temperature difference?
lowest_ave_temp <- Bom_data %>%
  separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep= "/") %>% 
  filter (min_Temp != "-", max_Temp != "-") %>% 
  mutate (min_Temp = as.numeric (min_Temp)) %>%   
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) %>% 
  group_by(Month) %>%
  summarise(average = mean(Temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)
 
#question 3: Which state saw the lowest average daily temperature difference?

Tidy_bom_stations <- Bom_stations %>% 
  gather (key = Station_number, value=amount, -info) %>% 
  spread(key = info, value= amount) %>% 
  mutate (Station_number=as.numeric(Station_number))

state_ave_temp <- Bom_data %>%
  separate(Temp_min_max, into = c("min_Temp", "max_Temp"), sep= "/") 
 

combined_data <- Tidy_bom_stations %>% 
  full_join(state_ave_temp, by= c("Station_number"= "Station_number"))

#Answer for Q3 after combining can be done in 2 ways
#1st method
state_lowest_ave_temp <- combined_data %>%
  filter(min_Temp != "-", max_Temp != "-") %>%
  mutate (min_Temp = as.numeric (min_Temp)) %>% #lines 44-46 could be done in one line:  mutate(Temp_diff = as.numeric(max_Temp) - as.numeric(min_Temp)) %>% 
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) %>% 
  group_by(state) %>%
  summarise(average = mean(Temp_diff)) %>% #you can also do filtering here using summarise(average = mean(Temp_diff, na.rm = TRUE) %>% 
  arrange(average) %>% #sorts from lowest to highest
  slice(1) #gives answer for state with lowest temp diff
#Answer Q3: QLD      7.36

#Second Method to question 3 by removing 2nd line that says filter and use na.rm = TRUE
state_lowest_ave_temp <- combined_data %>%
  mutate (min_Temp = as.numeric (min_Temp)) %>%  
  mutate (max_Temp = as.numeric (max_Temp)) %>% 
  mutate (Temp_diff = max_Temp - min_Temp) %>% 
  group_by(state) %>%
  summarise(average = mean(Temp_diff, na.rm = TRUE)) %>% # filtering here using na.rm = TRUE) 
  arrange(average) %>% #sorts from lowest to highest
  slice(1) #gives answer for state with lowest temp diff

#Day 6 
#Question 4:
#Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure
question_4 <- combined_data %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(lon = as.numeric(lon)) %>% 
  group_by(Station_number, lon) %>%
  summarise(average_solar_exp = mean(Solar_exposure, na.rm = TRUE)) %>% # filtering here using na.rm = TRUE) 
  arrange(lon) %>% 
  filter (lon== min(lon)| lon== max(lon)) %>% 
  ungroup() %>% 
  filter (lon==min(lon) | lon==max(lon))
#Answer question 4: No, the westmost (lowest longitude) or eastmost (highest longitude) 
#does not have a higher average solar exposure
# The westmost (lowest longitude) has average solar exp = 19.2)
# The eastmost (highest longitude) has average solar exp = 19.5




        