# load packages
library(tidyverse)
library(lubridate)

# data import (the dataset was download from https://ourworldindata.org/covid-deaths )
data <- read_csv("owid-covid-data.csv")

# selection of useful variables

colnames(data)

selected_data <- data %>% 
  select(iso_code,continent,location,date,population,new_cases,new_deaths,
         total_vaccinations,people_vaccinated,people_fully_vaccinated,new_vaccinations)


# see summary of data 

colnames(selected_data)
glimpse(selected_data)
summary(selected_data)
skimr::skim(selected_data)

# filter data (in order to eliminate duplicated data)
unique(selected_data$location)

# filter some records of locations that are not countries

filtered_data <-  selected_data %>% 
  filter(!(location %in% c("Africa", "Asia","Europe","European Union","International","Low income",
                           "Lower middle income","North America","Oceania","South America",
                           "Upper middle income","World","High income","North Korea")))

unique(filtered_data$location)

# ANALISIS
# summary of data
skimr::skim(filtered_data)

summary_data <- filtered_data %>% 
  group_by(location) %>% 
  summarise(t_population = mean(population), t_cases = sum(new_cases, na.rm=T), t_deaths = sum(new_deaths,na.rm=T),
            t_fully_vaccinated = max(if_else(is.na(people_fully_vaccinated), 0, people_fully_vaccinated), na.rm = T)) %>% 
  mutate(fatality_rate = t_deaths/t_cases, vaccination_rate = t_fully_vaccinated/t_population)

# countries with more cases

summary_data %>% 
  arrange(desc(t_cases)) %>% 
  mutate(location = reorder(location, t_cases)) %>% 
  slice(1:10) %>% 
  ggplot(aes(location,t_cases))+
  geom_bar(stat = "identity", width = 0.8)+
  geom_text(aes(label = paste(round(t_cases/1e6,1), "M")), hjust = 1.2, color = "white")+
  coord_flip()+
  labs(title = "Top 10 Countries by Cases", x = "", y = "Total Cases")+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))


# Countries with more deaths
summary_data %>% 
  arrange(desc(t_deaths)) %>% 
  mutate(location = reorder(location, t_deaths)) %>% 
  slice(1:10) %>% 
  ggplot(aes(location,t_deaths))+
  geom_bar(stat = "identity", width = 0.8)+
  geom_text(aes(label = paste(round(t_deaths/1e3,0), "K")), hjust = 1.2, color = "white")+
  coord_flip()+
  labs(title = "Top 10 Countries by Number of Deaths", x = "", y = "Total Deaths")+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))

# countries with the highest fatality rate
summary_data %>% 
  arrange(desc(fatality_rate)) %>% 
  mutate(location = reorder(location, fatality_rate)) %>% 
  slice(1:10) %>% 
  ggplot(aes(location, fatality_rate))+
  geom_bar(stat = "identity", width = 0.8)+
  geom_text(aes(label = paste(round(fatality_rate*100,2),"%")), hjust = 1.2, color = "white")+
  coord_flip()+
  labs(title = "Top 10 Countries by Fatality Rate", x = "", y = "Fataliry rate")+
  scale_y_continuous(labels = scales::unit_format(unit = "%", scale = 100))

# countries with the highest vaccination rate
summary_data %>% 
  arrange(desc(vaccination_rate)) %>% 
  mutate(location = reorder(location, vaccination_rate)) %>% 
  slice(1:10) %>% 
  ggplot(aes(location, vaccination_rate))+
  geom_bar(stat = "identity", width = 0.8)+
  geom_text(aes(label = paste(round(vaccination_rate*100,2),"%")), hjust = 1.2, color = "white")+
  coord_flip()+
  labs(title = "Top 10 Countries by Vaccination Rate", x = "", y = "Vaccination rate")+
  scale_y_continuous(labels = scales::unit_format(unit = "%", scale = 100))

# evolution of cases

selected_data %>% 
  group_by(month = floor_date(date, unit = "month")) %>% 
  summarise(t_cases = sum(new_cases, na.rm = T)) %>% 
  ggplot(aes(month, t_cases))+
  geom_bar(stat = "identity")+
  labs(title = "Evolution of Cases", x = "", y = "Total cases")+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))

# evolution of deaths
selected_data %>% 
  group_by(month = floor_date(date, unit = "month")) %>% 
  summarise(t_deaths = sum(new_deaths, na.rm = T)) %>% 
  ggplot(aes(month, t_deaths))+
  geom_bar(stat = "identity")+
  labs(title = "Evolution of Deaths", x = "", y = "Total deaths")+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))

# evolution of mortality rate
selected_data %>% 
  group_by(month = floor_date(date, unit = "month")) %>% 
  summarise(t_cases = sum(new_cases, na.rm = T),t_deaths = sum(new_deaths, na.rm = T)) %>%
  mutate(fatal = t_deaths/t_cases) %>% 
  ggplot(aes(month, fatal))+
  geom_line()+
  labs(title = "Evolution of Fatality Rate", x = "", y = "% fatality")+
  scale_y_continuous(labels = scales::unit_format(unit = "%", scale = 100))

